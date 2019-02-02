
(in-package cl-srpc)

(defconstant +hash-algo+ :sha256)

(defconstant +one-day+ (* 24 3600))

;; cipeher specified as arguments to (ironclad:make-cipher) but
;; NIL  means 'no cipher'
(defparameter *default-cipher*
  `(:blowfish :key ,(string-to-utf-8-bytes "Don't You Dare Use This Key") :mode :cbc))

(defclass %cl-srpc-connection ()
  ((cipher-args ;; arguments to give to (ironclad:make-cipher ...), or NIL
    :initarg :cipher-args
    :initform *default-cipher*
    :accessor cipher-args)
   (debug-stream
    :initarg :debug-stream
    :initform nil
    :accessor debug-stream)))


;; if there is a debug stream, write to it
;; a function, not method, to avoid CLOS overhead
(defun debug-format (cl-srpc-connection &rest format-args)
  (declare (type %cl-srpc-connection cl-srpc-connection)) 
  (when (debug-stream cl-srpc-connection)
    (apply #'format (debug-stream cl-srpc-connection) format-args)
    (terpri (debug-stream cl-srpc-connection)))) 
    
   



;; not sure if we trust ironclad to be threadsafe
(let ((random-byte-lock
	(bordeaux-threads:make-lock "random-byte-lock")))
  (defun get-random-bytes (nbytes) ;; get a random array of (unsigned-byte 8)
    (bordeaux-threads:with-lock-held (random-byte-lock)
      (ironclad:random-data nbytes))))
  

	       


;; read a line from byte-8 stream, as UTF8, of form "KEYWORD: value"
;; and return the value, of type, throwing an error on failure
(defun read-named-line (stream keyword type) 
  (declare (type stream stream)
	   (type simple-string keyword)
	   (type (member :string :integer) type))
  (let ((line (read-utf-8-line stream)))
    (when (not line) (error "Premature end of input"))
    (when (not
	   (and
	    (not (> (length keyword) (length line)))
	    (char= (aref line (length keyword)) #\:) ;; trailing colon
	    (string-equal keyword line :end2 (length keyword))))
      (error "Invalid line \"~A\" - expected \"~A: <~A>\"" line keyword type))
    (cond ((eq type :string)
	   (string-trim " " (subseq line (1+ (length keyword)))))
	  ((eq type :integer)
	   (let ((n (ignore-errors (parse-integer
				    line :start
				    (1+ (length keyword))))))
	     (when (not n)
	       (error "Invalid line \"~A\" - expected \"~A: NNN\"" line keyword))
	     n)))))




(defun write-named-line (stream keyword value)
  (declare (type stream stream)
	   (type string keyword)
	   (type (or string integer) value))
  (write-string-utf-8 keyword stream)
  (write-char-utf-8  #\: stream)
  (write-string-utf-8
   (cond ((integerp value)
	  (format nil  "~D" value))
	 ((stringp value)
	  value))
   stream)
  (terpri-utf-8 stream))


(defun read-line-from-char-or-byte-stream (stream)
  (cond ((eq (stream-element-type stream) 'character)
	 ;; read-line doesn't have a char limit, unfortunately, but
	 ;; presumably it is used with string streams, which are limited
	 (read-line stream nil nil)) 
	(t ;; unsigned-byte 8
	 (read-utf-8-line stream))))


;; read a list of headers of the form "Foo: bar" from a stream, ending when
;; a blank line or EOF are reached.
(defun read-headers-from-stream (char-or-byte-stream)
  (declare (optimize speed))
  (loop with header-list = nil
	for line of-type string = (read-line-from-char-or-byte-stream
				   char-or-byte-stream)
	until (or (not line) (= (length line) 0))
	do
	   (let ((ncolon
		   (or (position #\: line)
		       (error "No colon found in header line ~A" line))))
	     (push (list (subseq line 0 ncolon)
			 (string-trim " " (subseq line (+ 1 ncolon))))
		   header-list))
	finally  (return (nreverse header-list))))

;; get header from a list like '(("foo" "val-of-foo") ("bar" "99"))
;; optionally converting to TYPE (just :integer so far)
(defun get-header (keyword header-list &key (type :string))
  (declare (type string keyword)
	   (type list header-list)
	   (type (member :string :integer) type))
  (loop for pair in header-list
	when (string-equal keyword (first pair))
	  do  (return
		(cond ((eq type :string)
		       (second pair))
		      ((eq type :integer)
		       (parse-integer (second pair)))))))
		     
			 
(defun write-headers-to-stream (headers char-or-byte-stream)
  (let ((char-stream-p (eq (stream-element-type char-or-byte-stream)
			   'character)))
    (flet ((writestring (string)
	     (if char-stream-p
		 (write-string string char-or-byte-stream)
		 (write-string-utf-8 string char-or-byte-stream)))
	   (%terpri ()
	     (if char-stream-p
		 (terpri char-or-byte-stream)
		 (terpri-utf-8 char-or-byte-stream))))
      (dolist (header headers)
	(writestring (first header))
	(writestring ": ")
	(writestring  (if (stringp (second header)) ;; avoid format 
			  (second header)
			  (write-to-string (second header))))
	(%terpri)) ;; blank line
      (%terpri)
      (force-output char-or-byte-stream))))


    

(defun read-nbytes (stream nchars)
  (let ((vec (make-array nchars :element-type '(unsigned-byte 8))))
    (read-sequence vec stream)
    vec))


(defun cipher-requires-init-vector-p (cipher mode)
  (declare (type symbol cipher mode))
  (and (not (eq cipher nil))
       (not (member mode '(:ecb)))))

;; return the cipher and the initialization-vector
(defun build-cipher-for-connection  (cl-srpc-connection &optional init-vector)
  (declare (type %cl-srpc-connection cl-srpc-connection))  ;; client or server
  (destructuring-bind (cipher-name &key mode &allow-other-keys)
      (cipher-args cl-srpc-connection)
    (let* ((iv-required (cipher-requires-init-vector-p cipher-name mode))
	   (iv-size (when iv-required (ironclad:block-length cipher-name)))
	   (iv (when iv-required
		 (or init-vector
		     (get-random-bytes iv-size)))))
      (when (not (= (length iv) iv-size))
	(error "Provided init-vector is the wrong length for the cipher ~A" cipher-name))
      (values
       (apply 'ironclad:make-cipher
	      (append (cipher-args cl-srpc-connection)
		      (list :initialization-vector iv)))
       iv))))


;; given a BUF with digest-length bytes empty at the start, put the
;; hash of the remainder into these empty bytes
(defun %prepend-hash-to-bytes (buf &key
				     (digest-start 0) ;; where to start putting data
				     (data-end nil))  ;; last element of data (1+ index)
  (declare (type (array (unsigned-byte 8) (*)) buf))
  (ironclad:digest-sequence +hash-algo+ buf
			    :digest-start digest-start
			    :digest buf
			    :start (+ digest-start (ironclad:digest-length +hash-algo+))
			    :end data-end))

;; check if the hash at the start of BUF is valid
(defun %verify-prepended-hash (buf &key (digest-start 0) end)
  (declare (type (array (unsigned-byte 8) (*)) buf))

  (let ((hashvec (ironclad:digest-sequence  
		  +hash-algo+ buf 
		  :start (+ digest-start (ironclad:digest-length +hash-algo+))
		  :end end)))
    (loop for i below (length hashvec)
	  when (not (= (aref hashvec i) (aref buf (+ digest-start i))))
	    do (return nil) ;; not matching
	  finally (return t))))

(defun decrypt-byte8-vec-to-string (cl-srpc-connection ciphertext-buf)
  "Perform the inverse operation of encrypt-string-to-byte8-vec, returning
the string that was originally encrypted, stripping it of padding and leading
IV. See encrypt-string-to-byte8-vec for details of the format of ciphertext-buf."
  (declare (type %cl-srpc-connection cl-srpc-connection) ;; client or server
	   (type (simple-array (unsigned-byte 8) (*)) ciphertext-buf))
  (destructuring-bind (cipher-name &key mode &allow-other-keys)
      (or (cipher-args cl-srpc-connection) (list nil)) ;; CIPHER-NAME=NIL if no cipher
    ;; extract the initialization vector, and build the cipher
    (let* ((iv-required (cipher-requires-init-vector-p cipher-name mode))
	   (nblock (ironclad:block-length cipher-name))
	   (nhash (ironclad:digest-length +hash-algo+))
	   (iv-size (or (when iv-required nblock)
			0))
	   (iv (if (>= (length ciphertext-buf) iv-size)
		   (subseq ciphertext-buf 0 iv-size)
		   (error "Ciphertext is shorter than cipher IV.")))
	   (cipher (when cipher-name
		     (build-cipher-for-connection cl-srpc-connection iv))))
	       

      (if (not cipher-name)
	  ;;
	  (progn ;; no encryption used
	    (if (%verify-prepended-hash ciphertext-buf)
		(utf-8-bytes-to-string ciphertext-buf :start nhash) ;; no encryption used
		(error "Unencrypted data did not validate against hash.")))
	  ;;
	  (let* ((nout ;; safely large output buffer
		   (+ (length ciphertext-buf)
		      (ash (length ciphertext-buf) -1) ;; 1.5x length of plaintext
		      128)) ;; some extra constant room, including for IV
		 (plaintext-buf (make-array nout :element-type '(unsigned-byte 8))))
	    (multiple-value-bind (n-bytes-consumed n-bytes-produced)
		(ironclad:decrypt cipher ciphertext-buf plaintext-buf 
				  :plaintext-start 0
				  :ciphertext-start iv-size)
	      (declare (ignorable n-bytes-consumed))
	      (let ((npad (aref plaintext-buf 0)))
		(when (< n-bytes-produced (1+ nhash))
		  (error "Ciphertext is shorter than cipher IV plus NPAD."))
		
		(when (not (%verify-prepended-hash
			    plaintext-buf
			    :digest-start 1  ;; after NPAD
			    :end (+ 0 n-bytes-produced)))
		  (error "Decrypted data did not validate against hash."))
		(utf-8-bytes-to-string plaintext-buf
				       :start (+ 1 nhash) ;; skip npad byte, and hash
				       :end  (+ 0 (- n-bytes-produced npad)))))))))) 




(defun encrypt-string-to-byte8-vec (cl-srpc-connection plaintext-string)
  "Encryption function - for a PLAINTEXT-STRING, it returns a byte
buffer.  The first BLOCK-SIZE elements of this buffer consist of a
random cipher IV (initialization vector), and the remaining elements
consist of ciphertext.

This ciphertext, when decrypted, consists of a byte vector

  #(NPAD  
    HASHBYTE1 HASHBYTE2 ... HASHBYTEn  
    DATA1     DATA2     ... DATAn
    PAD1      PAD2      ... PADn)

where, in in detail, it has
  -  one NPAD byte giving the length of the padding at the end; padding
     is both to make the input a multiple of block size, and to randomize
     length
  -  the +HASH-ALGO+ hash of everything that follows
  -  the plaintext string, after it was converted to utf-8 byte array
  -  some padding at end, so that the total length is a multiple the block size,
     and so the length of the transmission is randomized."
  (declare (type %cl-srpc-connection cl-srpc-connection) ;; client or server
	   (type string plaintext-string))
  (destructuring-bind (cipher-name &key mode &allow-other-keys)
      (or (cipher-args cl-srpc-connection) (list nil)) ;; CIPHER-NAME=NIL if no cipher
    (let* ((iv-required (cipher-requires-init-vector-p cipher-name mode))
	   (nblock (ironclad:block-length cipher-name))
	   (nhash (ironclad:digest-length +hash-algo+))
	   (iv-size (or (when iv-required nblock)
			0)))
      (multiple-value-bind (cipher iv)
	  (when cipher-name
	    (build-cipher-for-connection cl-srpc-connection))
	;;	
	(if (not cipher-name)
	    ;;
	    (let ((buf (string-to-utf-8-bytes plaintext-string
					      :byte-pad-start nhash)))
	      (%prepend-hash-to-bytes buf)
	      buf)
	    ;;
	    (let* ((npt-raw (utf-8-byte-length plaintext-string))
		   ;; random padding to change message length
		   (npad-rnd (random 128)) 
		   ;; length including randomizing padding and the NPAD byte at start
		   ;; and the hash
		   (npt (+ 1 npt-raw npad-rnd nhash)) 
		   ;; more padding to make the length equal to multiple of block
		   (npad-block (let ((n (- nblock (rem npt nblock))))
				 (if (= n nblock) 0 n)))
		   ;; the total padding to put at end of plaintext
		   (npad (+ npad-block npad-rnd))
		   (plaintext-buf (string-to-utf-8-bytes
				   plaintext-string
				   ;; blank byte for the npad byte, and hash
				   :byte-pad-start (+ 1 nhash)
				   :byte-pad-end  npad))
		   (nout (+ npt (ash npt -1) 128)) ;; 1.5x length plus a bit extra
		   (ciphertext-buf (make-array nout :element-type '(unsigned-byte 8))))
	      (assert (<= 0 npad 255))
	      ;; put the IV into the first elements of ciphertext, unencrypted
	      (dotimes (i iv-size)
		(setf (aref ciphertext-buf i) (aref iv i)))
	      ;; put the padding size in the first byte of the plaintext
	      (setf (aref plaintext-buf 0) npad)
	      ;; put the hash into the next bytes of the plaintext
	      ;; the hash is for the entire data, including padding
	      (%prepend-hash-to-bytes plaintext-buf
				      :digest-start 1)
	      ;; and perform encryption
	      (multiple-value-bind (n-bytes-consumed n-bytes-produced)
		  (ironclad:encrypt cipher plaintext-buf ciphertext-buf
				    :ciphertext-start iv-size)
		(declare (ignorable n-bytes-consumed))
		(subseq ciphertext-buf 0  (+ iv-size  n-bytes-produced)))))))))



;; send and receive encrypted expressions, prepended with external
;; headers (just CONTENT-LENGTH), and containing internal headers
(defun send-encrypted-expression (cl-srpc-connection
				  stream
				  external-headers ;; headers before encr. block
				  internal-headers ;; headers inside encr. block
				  expression
				  &key
				    (marshal t)
				    (where "")) ;; annotation for DEBUG-FORMAT
  (let ((plaintext (with-output-to-string (s)
		     (write-headers-to-stream internal-headers s)
		     (write
		      (if marshal
			  (marshal:marshal expression)
			  expression)
		      :stream s))))
    (debug-format cl-srpc-connection "Plaintext ~A~Ais: ~%=======~%~A~%=======~%"
		  (or where "") (if where " " "")
		  plaintext)
    (let ((encrypted-octets
	    (encrypt-string-to-byte8-vec cl-srpc-connection plaintext)))
      (debug-format
       cl-srpc-connection
       "Encrypted octets~A~A: ~A"
       (if where " " "") (or where "")  
       encrypted-octets)
      (write-headers-to-stream
       (append external-headers `(("CONTENT-LENGTH" ,(length encrypted-octets))))
       stream)
      (write-sequence encrypted-octets stream)
      (force-output stream))))  ;; this seems necessary


;; get an encrypted expression, starting with unencrypted
;; CONTENT-LENGTH and other headers
;; return (values external-headers internal-headers value)
(defun receive-encrypted-expression (cl-srpc-connection
				     stream
				     &key
				       (unmarshal t)
				       (where nil)) ;; annotation for DEBUG-FORMAT
  (let* ((external-headers (read-headers-from-stream stream)))
    (debug-format
     cl-srpc-connection
     "Received external (un-encrypted) headers ~A~A: ~A"
     (or where "") (if where " " "")
     external-headers)
    (let ((content-length (or (ignore-errors
			       (parse-integer
				(get-header "CONTENT-LENGTH" external-headers)))
			      (error "Failed to get CONTENT-LENGTH"))))
       (debug-format
	cl-srpc-connection
	"Reading encrypted data ~A~A  of length ~A"
	(if where "at " " ") (or where "") 
	content-length)
      (let ((encrypted-octets
	      (read-nbytes stream content-length)))
	(debug-format
	 cl-srpc-connection
	 "Encrypted octets ~A~A: ~A"
	 (or where "")  (if where " " "")
	 encrypted-octets)
	(let ((plaintext
		(decrypt-byte8-vec-to-string
		 cl-srpc-connection encrypted-octets)))
	  (debug-format cl-srpc-connection "Plaintext ~A~Ais: ~%=======~%~A~%=======~%"
			(or where "") (if where " " "")
			plaintext)
	   (let (internal-headers value)
	     (with-input-from-string (s plaintext)
	       (setf internal-headers (read-headers-from-stream s))
	       (setf value  (subseq plaintext (file-position s)))
	       ;;
	       (values
		external-headers
		internal-headers
		(if unmarshal
		    (marshal:unmarshal (progn (read-from-string value)))
		    plaintext)))))))))
	    
	
    
  
							









(defparameter *test-case-string* nil) ;; so we recover what string led to a bad test
(defun test-cipher (cl-srpc-connection &key (min-len 0) (max-len 1000))
  "Run an encrypt/decrypt test on a SERVER or CLIENT object for random strings
from length=0 to MAX-LEN, to verify that the crypto works for parameters chosen."
  (flet ((make-random-string (n)
	   (let ((string (make-string n)))
	     (loop for i below n
		   do (setf (aref string i) (code-char (random 255))))
	     string)))
    (loop for j from min-len to max-len
	  for rstring = (make-random-string j)
	  do
	     (setf *test-case-string* rstring)
	     (when (not (equalp rstring
				(decrypt-byte8-vec-to-string
				 cl-srpc-connection
				 (encrypt-string-to-byte8-vec
				  cl-srpc-connection
				  rstring))))
	       (error "Failed encrypt/decrypt comparison for length =~A 
     see variable *test-case-string*" j)))
    "Test Passed OK"))
		      

						 
			    
