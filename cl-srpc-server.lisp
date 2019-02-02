
(in-package cl-srpc)

(defparameter *protocol-version* "CL-SRPC 1.0")

(defclass server (%cl-srpc-connection)
  ((name ;; some name for the server, not needed
    :initarg :name :initform "un-named" :accessor server-name)
   (protocol-version
    :initarg :protocol-version :initform *protocol-version*
    :accessor protocol-version)
   (running ;; is this server runnning?  
    :initarg :running :initform nil :accessor server-running)
   (threaded ;; launch requests in new thread?
    :initarg :threaded :initform t :accessor server-threaded)
   (timeout ;; timeout for io operations
    :initarg :timeout :initform 10 :accessor server-timeout)
   (address ;; IP address to listen to
    :initarg :address :initform "127.0.0.1" :accessor server-address)
   (port ;; port number
    :initarg :port :initform 50000 :accessor server-port)
   (listener-socket
    :initform nil :accessor server-listener-socket)))

;; make a long random request ID 
(defun generate-req-id ()
  (ironclad:byte-array-to-hex-string (get-random-bytes 32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS AND STRUCTURES FOR CACHING RESULTS THAT AREN'T RETURNED
;; IMMEDIATELY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *default-result-hash-lifespan* +one-day+)

(defstruct eval-result
  (request-id nil)
  ;; :new, :pending (waiting for computation to finish), :done
  (status nil) 
  (ut-create (get-universal-time))
  ;; how long to keep around before discarding
  (lifespan  *default-result-hash-lifespan*)
  (result   nil)) ;; NIL if no result, else (value1 value2)
                  ;; or if error (:cl-srpc-remote-error error-type "desc")
  
;; result hash has
;; KEY=REQUEST-ID
;; VALUE=EVAL-RESULT struct
(defvar *result-hash* (make-hash-table :test 'equalp))
(defvar *result-hash-lock* (bordeaux-threads:make-lock "result-hash-lock"))
;;
;; add a request and return its new ID; otherwise NIL until this request 
(defun add-new-req-id-to-result-hash (&key (lifespan  *default-result-hash-lifespan*))
  (bordeaux-threads:with-lock-held (*result-hash-lock*)
    (loop for req-id = (generate-req-id)
	  for i from 0
	  when (> i 1000) ;; somehow our random key generator isn't working
	    do (error "Can't generate fresh REQ-ID not in hash!")
	  do  (when (not (gethash req-id *result-hash*))
		 (setf (gethash req-id *result-hash* )
		       (make-eval-result
			:request-id req-id
			:status :new
			:ut-create (get-universal-time)
			:lifespan lifespan
			:result nil))
		 (return req-id)))))
;;
(defun retrieve-req-from-result-hash (req-id &key (remove nil) (remove-if-done nil))
  (bordeaux-threads:with-lock-held (*result-hash-lock*)
    (let ((eresult (gethash req-id *result-hash*)))
      (when (and eresult
		 (or remove
		     (and remove-if-done
			  (eq (eval-result-status eresult) :done))))
	(remhash req-id *result-hash*))
      eresult)))
;;
(defun set-req-lifespan (req-id lifespan)
  (when (< 5 lifespan (* 100 +one-day+)) ;; sanity check
    (bordeaux-threads:with-lock-held (*result-hash-lock*)
      (let ((eresult (gethash req-id *result-hash*)))
	(setf (eval-result-lifespan eresult) lifespan)))))
;;
(defun mark-result-pending (req-id)
  (bordeaux-threads:with-lock-held (*result-hash-lock*)
    (let ((eresult (gethash req-id *result-hash*)))
      (when eresult
	(setf (eval-result-status eresult) :pending)))))
;;
(defun mark-result-done (req-id value) ;; mark as done, and retrieve value
  (bordeaux-threads:with-lock-held (*result-hash-lock*)
    (let ((eresult (gethash req-id *result-hash*)))
      (when eresult
	(setf (eval-result-status eresult) :done)
	(setf (eval-result-result eresult) value)))))
;;
;; delete eval-results in *result-hash* that have been around for more than
;; (eval-result-lifespan eval-result)
(defun clean-result-hash () 
  (bordeaux-threads:with-lock-held (*result-hash-lock*)
    (loop with ut-now = (get-universal-time)
	  for req-id being the hash-key of *result-hash*
	  for eresult being the hash-value of *result-hash*
	  for ut-create = (eval-result-ut-create eresult)
	  for lifespan = (eval-result-lifespan eresult)
	  when (> (- ut-now ut-create) lifespan)
	    do  (remhash req-id *result-hash*))))

;; functions to run a thread to clean the result-hash of expired items
(let ((cleaner-lock (bordeaux-threads:make-lock "cl-srpc-cleaner-lock"))
      (cleaner-thread nil)
      (stop-cleaner nil))
  ;;
  ;; function to loop forever in its thread, sleeping, and cleaning
  ;; the *result-hash*
  (defun cleaner-function ()
    (loop
      do (sleep 60)
	 (clean-result-hash)
	 (when stop-cleaner
	   (bordeaux-threads:with-lock-held (cleaner-lock)
	     (setf cleaner-thread nil))
	   (return))))
  ;; 
  (defun maybe-start-cleaner-thread ()
    (bordeaux-threads:with-lock-held (cleaner-lock)
      (when (not cleaner-thread)
	(setf stop-cleaner nil)
	(setf cleaner-thread
	      (bordeaux-threads:make-thread
	       'cleaner-function
	       :name "cl-srpc-result-hash-cleaner-thread")))))
  ;;
  (defun stop-cleaner-thread ()
    (bordeaux-threads:with-lock-held (cleaner-lock)
      (setf stop-cleaner t))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

(defmethod print-object ((server server) stream)
  (format stream "#<cl-srpc:server ~S ~A:~A>"
	  (server-name server) (server-address server) (server-port server)))
  
(defmethod stop-server ((server server))
  "Stop a running server."
  (when (server-running server)
    (usocket:socket-close (server-listener-socket server)))
  (setf (server-running server) nil))



(defmethod start-server ((server server)
			 &key
			   (new-thread bordeaux-threads:*supports-threads-p*))
  "Start a CL-JRPC server.  If NEW-THREAD is true, then the listener
runs inside a new thread, and this function returns immediately."
  ;; open socket outside of any thread, so that errors are more evident
  (when (and new-thread
	     (not bordeaux-threads:*supports-threads-p*))
    "Error - running threaded server in a non-threaded lisp.")
  (when bordeaux-threads:*supports-threads-p*
    (maybe-start-cleaner-thread))
  (let ((listener-socket (usocket:socket-listen (server-address server) 
						(server-port server)
						:reuse-address t)))
    (flet ((run-server (lsocket)  ;; the internal function that runs the server
	     (setf (server-listener-socket server) lsocket)
	     (setf (server-running server) t)
	     (ignore-errors ;; a shutdown will close the socket, and we ignore the error
	      (loop
		do
		   (when (not (server-running server)) ;; will never be called?
		     (return))
		   (let ((connection (usocket:socket-accept
				      lsocket
				      :element-type '(unsigned-byte 8))))
		     (debug-format server "Accepted connection on listener socket")
		     ;; could shut down in SOCKET-ACCEPT
		     (when (not (server-running server)) 
		       (return))
		     ;; if server is threaded, run the call in a new thread
		     (if (server-threaded server)
			 (bordeaux-threads:make-thread
			  (lambda () (server-connection-function server connection))
			  :name "cl-srpc-server worker")
			 (server-connection-function server connection)))))))
      
      
      (if new-thread
	  (bordeaux-threads:make-thread
	   (lambda () (run-server listener-socket))
	   :name "cl-srpc-server main listener")
	  (run-server listener-socket)))))




(defun server-connection-function (server connection)
  (declare (type server server)
	   (type usocket:stream-usocket connection))
  (ignore-errors ;; exit threads cleanly rather than leaving them hanging
   (let ((stream (usocket:socket-stream connection))
	 ;; a 'unique' ID for this request to prevent replay attacks
	 ;; among other things
	 (request-id (add-new-req-id-to-result-hash))
	 (no-errors nil)) ;; flag that we finished OK; otherwise delete req-id from hash
     (debug-format server "Entered SERVER-CONNECTION-FUNCTION")
     (unwind-protect
	  (progn
	    (setf (usocket:socket-option connection :send-timeout)
		  (server-timeout server))
	    (setf (usocket:socket-option connection :receive-timeout)
		  (server-timeout server))

	    (write-utf-8-line *protocol-version* stream)
	    (debug-format server "Wrote protocol-version ~A" *protocol-version*)

	    (send-encrypted-expression 
	     server stream
	     nil `(("REQUEST-ID" ,request-id))
	     nil ;; no expression
	     :where "(send REQUEST-ID)")
	     

	    (multiple-value-bind (external-request-headers
				  internal-request-headers
				  client-request-expression)
		(receive-encrypted-expression
		 server stream
		 :where "(receiving expression from client)")
	      (declare (ignore external-request-headers))
	      (multiple-value-bind (internal-response-headers computed-expression)
		  (compute-server-response server request-id
					   internal-request-headers
					   client-request-expression)

		(send-encrypted-expression server stream
					   nil internal-response-headers 
					   computed-expression
					   :where "(response to client)")
		(setf no-errors t))))
		       
     (progn
       (when (not no-errors) ;; an error happened so kill this request id
	 (retrieve-req-from-result-hash request-id :remove t))
       (ignore-errors (usocket:socket-close connection)))))))





 

;; return (values headers expression) to return to client
(defun compute-server-response (server request-id
				client-request-headers
				client-request-expression)
  (declare (type server server)
	   (type string request-id))
  (let* ((command (or (get-header "COMMAND" client-request-headers)
		      (error "No COMMAND found in client request.")))
	 (req-id-ok (when (not (equalp (get-header "REQUEST-ID"
						   client-request-headers)
				       request-id))
		      (debug-format
		       server
		       "REQUEST-ID in client response did not match.")
		      (error "Request id return from client does not match original ID")))
	 (cached-request-id ;; if retrieving from cache
	   (get-header "CACHED-REQUEST-ID" client-request-headers))
	 (lifespan (or (ignore-errors
			(parse-integer
			 (get-header "LIFESPAN" client-request-headers)))
		       *default-result-hash-lifespan*))
	 (result
	   (cond ((equalp command "EVAL")
		  (debug-format server "Performing simple EVAL")
		  (prog1
		      (eval-remote-expression client-request-expression)
		    (retrieve-req-from-result-hash ;; not caching
		     request-id :remove t)))
		 ;;
		 ((equalp command "EVAL-AND-CACHE")
		  (debug-format server "Performing EVAL-AND-CACHE")
		  (eval-and-cache-remote-expression client-request-expression
						    request-id lifespan))
		 ;;
		 ((equalp command "RETRIEVE-CACHED-VALUE")
		  (debug-format server "Performing RETRIEVE-CACHED-VALUE")
		  ;; delete this request (because RETRIEVE-TRANSIENT-VALUE
		  ;; requests should not be cached)
		  (retrieve-req-from-result-hash ;; not caching
		     request-id :remove t)
		  (retrieve-cached-eval cached-request-id)))))
    (declare (ignore req-id-ok))
    (values
     `(("REQUEST-ID" ,request-id)
       ("COMMAND-RUN" , command))
     result)))



(defun eval-remote-expression (expression)
  (ignore-errors ;; paranoid outer safety
   (let ((results
	   (handler-case
	       (multiple-value-list ;; outer list is values
		(eval expression))
	     (error (condition)
	       (list 'cl-srpc-remote-error
		     (type-of condition)
		     (format nil "~A" condition))))))
     results)))
	 

(defun eval-and-cache-remote-expression (expression request-id lifespan)
  (mark-result-pending request-id) ;; this result is now in progress
  (if (not bordeaux-threads:*supports-threads-p*)
      '(cl-srpc-remote-error 'lisp-does-not-support-threads
	"Cannot cache CL-SRPC evaluations when lisp does not support threads.")
      ;;
      (progn ;; if threads supported, launch a thread, and return T
	(set-req-lifespan request-id lifespan)
	(bordeaux-threads:make-thread 
	 (lambda ()
	   (mark-result-done
	    request-id
	    ;; this has an ignore-errors inside
	    (eval-remote-expression  expression)))
	 :name "cl-srpc-delayed-thread")
	T)))
    
(defun retrieve-cached-eval (request-id)
  (let ((eresult
	  ;; the value NIL means no result, but (NIL) means result was NIL
	  (retrieve-req-from-result-hash request-id :remove-if-done t)))
    (if (not eresult)
	`(cl-srpc-remote-error no-cached-result-found
	  ,(format nil "no cached result for RESULT-ID: ~A" request-id))
	(eval-result-result eresult))))
				 
						   
    
			  
	   



