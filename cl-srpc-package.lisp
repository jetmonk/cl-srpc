

(defpackage cl-srpc
  (:use #:cl)
  (:export
   ;; trivial-utf8-srpc.lisp
   #:string-to-utf-8-bytes
   ;; cl-srpc-utils.lisp
   #:test-cipher
   ;; cl-srpc-server.lisp
   #:server #:server-running
   #:start-server #:stop-server
   ;; cl-jrcp-client.lisp
   #:client
   #:remote-promise #:remote-promise-evaluated
   #:execute-remote-call
   #:evaluate-promise
   #:cl-srpc-remote-error ;; special symbol to signal an error condition

   )) 
