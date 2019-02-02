

(asdf:defsystem cl-srpc
  :depends-on (marshal    ;; cl-marshal, for packaging lisp structs
	       usocket    ;; for sockets
	       bordeaux-threads  ;; for spawning threads
	       ;; babel      ;; for UTF-8 encoding/decoding to octets
	       ironclad)  ;; for crypto
  :components
  ((:file "cl-srpc-package" :depends-on nil)
   (:file "trivial-utf-8-srpc"
    :depends-on ("cl-srpc-package")) ;; borrowed from trivial-utf-8
   (:file "cl-srpc-utils"
    :depends-on ("cl-srpc-package" "trivial-utf-8-srpc"))
   (:file "cl-srpc-server"
    :depends-on ("cl-srpc-package" "cl-srpc-utils"))
   (:file "cl-srpc-client"
    :depends-on ("cl-srpc-package" "cl-srpc-utils"))

   ))
