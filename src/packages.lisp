(defpackage #:cleric-bops
  (:documentation "Common byte operations used internally by CLERIC and BERT.")
  (:use #:cl)
  (:export

   #:bytes-to-signed-int32
   #:bytes-to-string
   #:bytes-to-uint16
   #:bytes-to-uint32
   #:bytes-to-unsigned-integer

   #:read-bytes
   #:read-bytes-as-string
   #:read-signed-int32
   #:read-string
   #:read-uint16
   #:read-uint32

   #:uint16-to-bytes
   #:uint32-to-bytes
   #:unsigned-integer-to-bytes

   #:string-to-bytes

   ))

(defpackage #:common-lisp-erlang-interface
  (:documentation "CLERIC (Common Lisp Erlang Interface) - An implementation of the Erlang distribution protocol.")
  (:nicknames #:cleric)
  (:use #:cl #:cleric-bops)
  (:export

   ;; Type
   #:erlang-translatable
   #:erlang-translatable-p

   ;; Classes
   #:erlang-binary
   #:erlang-external-fun
   #:erlang-old-internal-fun
   #:erlang-new-internal-fun
   #:erlang-pid
   #:erlang-port
   #:erlang-reference
   #:erlang-tuple
   #:remote-node
   #:link
   #:send
   #:exit
   #:unlink
   #:node-link
   #:reg-send
   #:group-leader
   #:exit2

   ;; Class methods and functions
   #:encode
   #:decode
   #:make-atom
   #:match-p
   #:bytes
   #:bits-in-last-byte
   #:module
   #:arity
   #:size
   #:elements
   #:tuple
   #:binary
   #:string-to-binary
   #:bytes-to-binary
   #:binary-to-string
   #:remote-node-socket
   #:remote-node-port
   #:remote-node-name
   #:remote-node-host
   #:remote-node-connect
   #:remote-node-accept-connect
   #:connected-remote-nodes
   #:node
   #:make-pid
   #:trace-token
   #:from-pid
   #:to-pid
   #:cookie
   #:message
   #:reason
   #:to-name
   #:listening-p
   #:listening-port
   #:start-listening
   #:stop-listening
   #:this-node
   #:node-name
   #:node-host

   ;; Conditions
   #:not-implemented-error
   #:already-listening-on-socket
   #:not-listening-on-socket
   #:try-again
   #:handshake-failed-error
   #:connection-closed-error
   #:node-unreachable-error
   #:malformed-message-error
   #:malformed-external-erlang-term-error
   #:untranslatable-lisp-object-error
   #:unexpected-message-length-error
   #:unexpected-message-tag-error

   ;; Condition restarts
   #:try-again-condition-p
   #:try-connect-again-restart
   #:start-listening-on-socket-restart

   ;; Constants
   #:+lowest-version-supported+
   #:+highest-version-supported+

   ;; Special variables
   #:*atom-symbol-package*
   #:*lisp-t-is-erlang-true*
   #:*lisp-nil-is-erlang-empty-list*
   #:*lisp-nil-is-erlang-false*
   #:*lisp-nil-at-tail-is-erlang-empty-list*
   #:*lisp-string-is-erlang-binary*
   #:*erlang-true-is-lisp-t*
   #:*erlang-false-is-lisp-nil*
   #:*erlang-string-is-lisp-string*

   ;; Connection functions
   #:reg-send
   #:send
   #:link
   #:unlink
   #:receive-node-messages

   ))

(defpackage #:cleric-epmd
  (:documentation
   "Functions for querying the EPMD (Erlang Port Mapper Daemon).")
  (:use #:cl #:cleric #:cleric-bops #:flexi-streams)
  (:export

   #:publish
   #:published-p
   #:unpublish
   #:lookup-node
   #:print-all-registered-nodes

   #:already-registered
   #:unreachable-error
   #:host-unknown-error
   #:response-error

   ))
