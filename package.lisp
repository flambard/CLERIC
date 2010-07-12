(defpackage :common-lisp-erlang-interface
  (:documentation "CLERIC (Common Lisp Erlang Interface) - An implementation of the Erlang distribution protocol.")
  (:nicknames :cleric)
  (:use :cl)
  (:export

   ;; Type
   erlang-translatable
   erlang-translatable-p

   ;; Classes
   erlang-binary
   erlang-external-fun
   erlang-internal-fun
   erlang-new-internal-fun
   erlang-pid
   erlang-port
   erlang-reference
   erlang-tuple
   remote-node
   link
   send
   exit
   unlink
   node-link
   reg-send
   group-leader
   exit2

   ;; Class methods and functions
   encode
   decode
   bytes
   bits-in-last-byte
   module
   arity
   size
   elements
   tuple
   binary
   string-to-binary
   bytes-to-binary
   binary-to-string
   remote-node-socket
   remote-node-port
   remote-node-name
   remote-node-host
   remote-node-connect
   encode-control-message
   decode-control-message
   node
   make-pid
   trace-token
   from-pid
   to-pid
   cookie
   message
   reason
   to-name

   ;; Conditions
   not-implemented-error
   try-again
   handshake-failed-error
   connection-closed-error
   node-unreachable-error
   epmd-unreachable-error
   epmd-host-unknown-error
   epmd-response-error
   malformed-message-error
   malformed-external-erlang-term-error
   untranslatable-lisp-object-error
   unexpected-message-length-error
   unexpected-message-tag-error

   ;; Constants
   +lowest-version-supported+
   +highest-version-supported+

   ;; Special variables
   *this-node*
   *remote-nodes*
   *lisp-t-symbol-is-erlang-true*
   *lisp-nil-is-erlang-empty-list*
   *lisp-nil-symbol-is-erlang-false*
   *lisp-nil-at-tail-is-erlang-empty-list*
   *lisp-string-is-erlang-binary*
   *erlang-true-is-lisp-t-symbol*
   *erlang-false-is-lisp-nil-symbol*
   *erlang-string-is-lisp-string*

   ;; EPMD Functions
   epmd-lookup-node
   
   ;; Connection functions
   reg-send
   send
   link
   unlink
   receive-node-messages

   ))


(defpackage :bert
  (:documentation "BERT (Binary ERlang Term).")
  (:use :cl :cleric)
  (:shadow encode decode)
  (:export

   ;; Classes
   erlang-binary
   erlang-tuple

   ;; Functions and methods
   encode
   decode
   binary
   bytes
   bits-in-last-byte
   size
   string-to-binary
   bytes-to-binary
   binary-to-string
   tuple
   elements
   arity
   
   ;; Special variables
   *lisp-t-symbol-is-erlang-true*
   *lisp-nil-is-erlang-empty-list*
   *lisp-nil-symbol-is-erlang-false*
   *lisp-nil-at-tail-is-erlang-empty-list*
   *lisp-string-is-erlang-binary*
   *erlang-true-is-lisp-t-symbol*
   *erlang-false-is-lisp-nil-symbol*
   *erlang-string-is-lisp-string*

   ;; Conditions
   untranslatable-lisp-object-error
   unexpected-message-length-error
   unexpected-message-tag-error
   
   ))
