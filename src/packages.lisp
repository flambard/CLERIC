(defpackage #:common-lisp-erlang-interface
  (:documentation "CLERIC (Common Lisp Erlang Interface) - An implementation of the Erlang distribution protocol.")
  (:nicknames #:cleric)
  (:use #:cl #:erlang-term #:etf-bops #:flexi-streams)
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
   #:make-reference
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

(defpackage #:cleric-epmd-client
  (:documentation "An EPMD (Erlang Port Mapper Daemon) client.")
  (:nicknames :cleric-epmd) ;; Old name
  (:use #:cl #:epmd-client)
  (:export

   #:epmd-connection
   #:publish
   #:published-node-name
   #:published-node-port
   #:published-p
   #:unpublish

   #:lookup-node
   #:print-all-registered-nodes

   #:node-info
   #:node-name
   #:node-host
   #:node-port
   #:node-type
   #:node-protocol
   #:node-lowest-version
   #:node-highest-version
   #:node-extra-field

   #:already-registered
   #:unreachable-error
   #:host-unknown-error

   ))
