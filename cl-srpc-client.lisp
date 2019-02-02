
(in-package cl-srpc)


(defclass client (%cl-srpc-connection)
  ((name ;; some name for the server, not needed
    :initarg :name :initform "un-named" :accessor client-name)
   (protocol-version
    :initarg :protocol-version :initform *protocol-version*
    :accessor protocol-version)
   (timeout ;; timeout for io operations
    :initarg :timeout :initform 10 :accessor client-timeout)
   (remote-address ;; IP address to connect to
    :initarg :remote-address :initform "127.0.0.1" :accessor remote-address)
   (remote-port ;; port number
    :initarg :remote-port :initform 50000 :accessor remote-port)))

(defclass remote-promise ()
  ((client :initarg :client :initform nil :accessor remote-promise-client)
   (request-id :initarg :request-id :accessor remote-promise-request-id)
   (evaluated :initform nil :accessor remote-promise-evaluated)))

(defmethod print-object ((client client) stream)
  (format stream "#<cl-srpc:client to ~S ~A:~A>"
	  (client-name client)
	  (remote-address client)
	  (remote-port client)))


(defmethod print-object ((remote-promise remote-promise) stream)
  (format stream "#<cl-srpc:remote-promise (~A)>"
	  (if (remote-promise-evaluated remote-promise)
	      "evaluated"
	      "un-evaluated")))

(defconstant +one-day+ (* 24 3600))

(defun execute-remote-call (client &key
				     (expression nil)
				     (return-promise nil)
				     (promise-lifespan +one-day+)
				     (promise nil))
  "Eval a Lisp EXPRESSION on a remote side using host:port
configuration of CLIENT. 

Returns list of values 
  (VAL1 VAL2 VAL3 ...) 
from remote evaluation, or in case of error returns 
  (CL-SRPC:CL-SRPC-REMOTE-ERROR ERROR-TYPE-OF ERROR-DESCRIPTION)
where CL-SRPC:CL-SRPC-REMOTE-ERROR is a symbol in the CL-SRPC package.
  
Keywords are:

  EXPRESSION - A Lisp expression like (+ 1 2 3) to evaluate
     at remote end, and list of values (VAL1 VAL2 ...) is returned,
     in this example the list (5)
  RETURN-PROMISE - The EXPRESSION (required) is not evaluated during 
     the call, which returns immediately, but is cached 
     on server side upon completion, and a REMOTE-PROMISE is returned.
  PROMISE-LIFESPAN - how long in seconds result is cached on the server.
     One day by default
  PROMISE  - A REMOTE-PROMISE object is provided here, and NIL is 
     returned if the computation is incomplete; otherwise a list
     of values (VAL1 VAL2 ...) is returned.
"
  (when (and promise expression)
    (error "Only one of EXPRESSION and PROMISE maybe be given."))
  (when (and return-promise (not expression))
    (error "If RETURN-PROMISE is given, then EXPRESSION must also be given."))
  (when (not (or expression promise))
    (error "At least one of EXPRESSION or PROMISE must be given."))
  (when (and promise (remote-promise-evaluated promise))
    (error "Remote promise ~A has been evaluated already." promise))
  (when (and promise-lifespan
	     (not (integerp promise-lifespan))
	     (not (< 5 promise-lifespan (* 100 +one-day+))))
    (error "PROMISE-LIFESPAN = ~A is not a a time (in sec) between 10 seconds and 100 days."
	   promise-lifespan))
  (let* ((connection (usocket:socket-connect
		     (remote-address client)
		     (remote-port client)
		     :element-type '(unsigned-byte 8)))
	 (stream (usocket:socket-stream connection)))
    ;;

    ;; FIXME - replace with receive-encrypted-expression
    ;; and send-encrypted-expression and add the initial
    ;; transfer of request-id

    ;;
    (unwind-protect
	  (progn
	    (setf (usocket:socket-option connection :send-timeout)
		  (client-timeout client))
	    (setf (usocket:socket-option connection :receive-timeout)
		  (client-timeout client))

	    (let ((server-protocol-version (read-utf-8-line stream)))
	      (debug-format client "Read proto version ~A"
		      server-protocol-version)
	      (when (not (equalp server-protocol-version
				 (protocol-version client)))
		(error "Wrong protocol version: server ~A; client ~A"
		       server-protocol-version (protocol-version client))))

	    ;; get the REQUEST-ID header from the first encrypted
	    ;; block from server
	    (multiple-value-bind (external-headers internal-headers)
		(receive-encrypted-expression
		 client stream :where "(getting REQUEST-ID block)")
	      (declare (ignore external-headers))
	      ;;
	      (let ((request-id (get-header "REQUEST-ID" internal-headers)))
		;; send the RPC command in an encrypted block
		(let* ((command
			 (cond ((and expression (not return-promise))
				"EVAL")
			       ((and expression return-promise)
				"EVAL-AND-CACHE")
			       (promise
				"RETRIEVE-CACHED-VALUE")))
		       (internal-headers `(("COMMAND" ,command)
					   ("REQUEST-ID" ,request-id)
					   ,@(if return-promise
						 `(("LIFESPAN"
						    ,promise-lifespan)))
					   ,@(if promise
						 `(("CACHED-REQUEST-ID"
						    ,(remote-promise-request-id promise)))))))
					 
		       
		  (send-encrypted-expression
		   client stream
		   nil internal-headers 
		   expression :where "(sending request)")
		  
		  (debug-format client "Done sending client encrypted data")
		  
		  (multiple-value-bind (external-server-headers
					internal-server-headers
					server-expression)
		      (receive-encrypted-expression
		       client stream :where "(receiving response)")
		    ;;
		    (declare (ignorable external-server-headers))
		    
		    ;; this last test is probably not needed
		    (when (not (equalp
				request-id
				(get-header "REQUEST-ID" internal-server-headers)))
		      (debug-format
		       client
		       "FATAL ERROR: REQUEST-ID in server response did not match.")
		      (error "REQUEST-ID in server response did not match."))
		    
		    ;; FIXME - what about caching cases?
		    (cond ((equalp command "EVAL")
			   server-expression)
			  ((equalp command "RETRIEVE-CACHED-VALUE")
			   (when server-expression
			     ;; note that we've gotten an answer, if the answer
			     ;; wasn't NIL
			     (setf (remote-promise-evaluated promise) t))
			     server-expression)
			  ((equalp command "EVAL-AND-CACHE")
			   (make-instance 'remote-promise
					  :client client
					  :request-id request-id))))))))
	    
      (progn
	(usocket:socket-close connection)))))



(defun evaluate-promise (promise)
  "Evaluate a REMOTE-PROMISE, returning NIL or list (VALUE1 VALUE2 ...)
of the remote evaluation.

In case of error, returns
 (CL-SRPC:CL-SRPC-REMOTE-ERROR ERROR-TYPE-OF ERROR-DESCRIPTION)

This function is shorthand for
  
   (EXECUTE-REMOTE-CALL (REMOTE-PROMISE-CLIENT PROMISE) :PROMISE PROMISE)
"
  (declare (type remote-promise promise))
  (execute-remote-call
   (remote-promise-client promise)
   :promise promise))
