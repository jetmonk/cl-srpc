# cl-srpc - Common Lisp Secure RPC 

An encrypted RPC (Remote Procedure Call) package that sends lisp expressions like
`(+ 1 1)` from a client to a server for evaluation, either immediate or deferred.

## Dependencies

* usocket  - for networking
* ironclad  - for encryption
* cl-marshal - for serialization of lisp objects


## Usage

### Define a symmetric key to use on both server and client

````
;; the key is the arguments given to (ironclad:make-cipher) and NIL
;; means not to use any cipher, but send in cleartext

(defparameter *my-cipher*
  `(:blowfish :key ,(string-to-utf-8-bytes "Don't You Dare Use This Key") :mode :cbc))
````

### On server side

```
(asdf:load-system "cl-srpc")

(defparameter *server*
  (make-instance 'cl-srpc:server
    :cipher-args *my-cipher* 
    :address "127.0.0.1"  
    :port 50000))

(cl-sprc:start-server *server*)
````

###  On client side
````
(asdf:load-system "cl-srpc")
````

#### On client side, evaluate remotely and return immediately
````
(defparameter *client*
  (make-instance 'cl-srpc:client
    :cipher-args *my-cipher*
    :remote-address "127.0.0.1"  ;; same as server
    :remote-port 50000))         ;; same as server


;; then call

(cl-srpc:execute-remote-call *client* :expression '(+ 1 2))
  ==> (3)  ;; values returned inside list


(Cl-srpc:execute-remote-call *client* :expression '(values 1 2 3))
  ==> (1 2 3) ;; multiple values

;; or in the case of a remote error
(cl-srpc:execute-remote-call *client* :expression '(/ 1 "two"))
  ==> (cl-srpc:cl-srpc-remote-error ;; special symbol indicating error
       type-error ;; the type of the error
       "The value  \"two\" is not of type number")
````

#### On client side, evaluate remotely but return a promise of a result
````
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One can also use :RETURN-PROMISE to return immediately, but
;; create a REMOTE-PROMISE object that can be used to retrieve
;; the result of the computation later
;;
(defparameter *promise*
 (cl-srpc:execute-remote-call
	*client*
        :expression '(progn (sleep 10) (+ 1 2))
	:promise-lifespan 1000 ;; optional longevity on server
	:return-promise t)) ;; return immediatly
*promise*			      
  ==> #<cl-srpc:remote-promise (un-evaluated)>

 (cl-srpc:evaluate-promise *promise*)
   ==> NIL ;; result is not ready yet
 (cl-srpc:evaluate-promise *promise*)
   ==> (3) ;; now we got a result, and the *promise* is used up
 *promise*			      
   ==> #<cl-srpc:remote-promise (evaluated)>

;; now try forcing re-evaluation of an evaluated promise
(setf (cl-srpc:remote-promise-evaluated *promise*) nil)
(cl-srpc:evaluate-promise *promise*)
 ==> (cl-srpc:cl-srpc-remote-error   ;; error: promise not found on server
      cl-srpc::no-cached-result-found
      "no cached result for RESULT-ID: 078e2716335269....")

````
The PROMISE-LIFESPAN is how many seconds the cached result is kept
on the server before being deleted by a cleanup thread. By
default it is 1 day.



## Internals

Internals are documented, but are not of concern to most users.

### PROTOCOL COMMUNICATION SEQUENCE 

1.  CLIENT connects and waits
2.  SERVER sends *protocol-version*

    >CL-SRPC 1.0

3. CLIENT reads this, and exits on incompatible version.

4.  SERVER sends encrypted block (see below for definition) with nothing but header

    > REQUEST-ID: xxxxxxxxx
    
    This request id will be returned by the client in its request.

5. CLIENT reads this REQUEST-ID encrypted block.
   
6. CLIENT sends request in an encrypted block.   The encrypted block contains the
    (encrypted) REQUEST-ID header, to prevent replay attacks.
   
7. SERVER reads NN bytes as an encrypted block (see below)
    and decrypts encrypted block into a string, which is eval'ed
    (see below for encrypted blocks)
   
8. SERVER writes an encrypted block containing the evaluated expression
   
9. CLIENT readst the encrypted block, and closes connection.



### CONTENT OF ENCRYPTED BLOCKS 

An encrypted block, upon decryption, consists of internal headers of the form
"HEADER:  <value>\n" followed by a blank line, followed by <content-text>.

Each encrypted block is preceded by plaintext external headers:

  >CONTENT-LENGTH: nn\n
  
  >\n

The encrypted block sent by the client has the header

  >COMMAND: [command]
  
where [command] is one of

  1) EVAL - evaluate the cl-marshal encoded lisp in <content-text),
     and return  it as cl-marshal text
  2) EVAL-AND-CACHE - evaluate, but do so in another thread, but return
     a REQUEST-ID header that can be used to retrieve this cached value later
  3) RETRIEVE-CACHED-VALUE - retrieve the value passed in the
     REQUEST-ID header and return it as cl-marshal encoded text

The encrypted block returned by the server has the header REQUEST-ID
followed by the cl-marshal encoded content being returned.



### INTERNAL STRUCTURE OF ENCRYPTED BLOCKS

The structure of encrypted blocks is described in the documentation
inside cl-srpc-utils.lisp.

The functions  encrypt-string-to-byte8-vec and decrypt-byte8-vec-to-string
handle block encryption/decryption transparently.   The function
test-cipher verifies that encryption/decryption works for a given cipher works
inside a CL-SRPC-CONNECTION object

For a given ironclad cipher with a block size, the first BLOCK-SIZE
elements are the IV (initialiation vector), and the remaining elements
are ciphertext.

The ciphertext, when decrypted by ironclad, has
  *  one NPAD byte giving the length of the padding at the end; padding
     is both to make the input a multiple of block size, and to randomize
     length
  *  the +HASH-ALGO+ hash of everything that follows
  *  the plaintext string, after it was converted to utf-8 byte array
  *  some padding at end, so that the total length is a multiple the block
     size, and so the length of the transmission is randomized.  This
     padding is stripped out during decryption.



   
