(defpackage :common-lisp-erlang-interface-system
  (:nicknames :cleric-system)
  (:use :cl))

(in-package :cleric-system)

(asdf:defsystem :cleric
  :description "Common Lisp Erlang Interface - An implementation of the Erlang distribution protocol."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.1.2"
  :license "MIT License"
  :depends-on (:usocket :flexi-streams :md5 :ieee-floats :alexandria :nibbles)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "listen"
                    :depends-on ("packages"))
             (:file "local-node"
                    :depends-on ("packages"))
             (:file "macros"
                    :depends-on ("packages"))
             (:file "generic-functions"
                    :depends-on ("packages"
                                 "constants"))
             (:file "remote-node"
                    :depends-on ("packages"
                                 "handshake"
                                 "listen"
                                 "local-node"
                                 "atom-cache"))
             (:file "conditions"
                    :depends-on ("packages"))
             (:file "constants"
                    :depends-on ("packages"))
             (:file "atom-cache"
                    :depends-on ("packages"))
             (:file "atom-cache-entries"
                    :depends-on ("packages"
                                 "atom-cache"
                                 "special-variables"))
             (:file "special-variables"
                    :depends-on ("packages"))
             (:file "bops"
                    :depends-on ("packages"))
             (:file "classes"
                    :depends-on ("packages"))
             (:file "type-erlang-translatable"
                    :depends-on ("packages"
                                 "classes"))
             (:file "distribution-header"
                    :depends-on ("packages"
                                 "atom-cache"
                                 "bops"))
             (:file "ieee-floats" ;; Needs IEEE-Floats library
                    :depends-on ("packages"
                                 "bops"))
             (:file "epmd"
                    :depends-on ("packages"
                                 "listen"
                                 "local-node"
                                 "remote-node"
                                 "bops"))
             (:file "decode"
                    :depends-on ("packages"
                                 "conditions"
                                 "bops"
                                 "generic-functions"
                                 "constants"
                                 "special-variables"
                                 "classes"
                                 "atom-cache"
                                 "erlang-atom"
                                 "erlang-binary"
                                 "erlang-float"
                                 "erlang-fun"
                                 "erlang-integer"
                                 "erlang-list"
                                 "erlang-pid"
                                 "erlang-port"
                                 "erlang-reference"
                                 "erlang-tuple"))
             (:file "handshake"
                    :depends-on ("packages"
                                 "local-node"
                                 "bops"
                                 "md5"))
             (:file "md5" ;; Needs MD5 library
                    :depends-on ("packages"))
             (:file "control-message"
                    :depends-on ("packages"
                                 "classes"
                                 "decode"))
             (:file "node-protocol"
                    :depends-on ("packages"
                                 "macros"
                                 "distribution-header"
                                 "classes"
                                 "bops"
                                 "control-message"
                                 "remote-node"))
             (:file "erlang-atom"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-binary"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-float"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "ieee-floats"
                                 "bops"))
             (:file "erlang-fun"
                    :depends-on ("packages"
                                 "macros"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-integer"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "bops"))
             (:file "erlang-list"
                    :depends-on ("packages"
                                 "macros"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-pid"
                    :depends-on ("packages"
                                 "local-node"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-port"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "classes"
                                 "bops"))
             (:file "erlang-reference"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "classes"
                                 "bops"))
             (:file "erlang-string"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-tuple"
                    :depends-on ("packages"
                                 "constants"
                                 "conditions"
                                 "classes"
                                 "bops"))
             ))))

(asdf:defsystem :cleric-test
  :description "Unit tests for CLERIC."
  :depends-on (:cleric :eos)
  :components
  ((:module :test
            :components
            ((:file "tests")
             (:file "cleric" :depends-on ("tests"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :cleric))))
  (asdf:load-system :cleric-test)
  (asdf:test-system :cleric-test))
