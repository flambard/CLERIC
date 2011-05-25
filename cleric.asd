(defpackage :common-lisp-erlang-interface-system
  (:nicknames :cleric-system)
  (:use :cl))

(in-package :cleric-system)

(asdf:defsystem :cleric
  :description "Common Lisp Erlang Interface - An implementation of the Erlang distribution protocol."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.0.8"
  :license "MIT License"
  :depends-on (:usocket :md5 :ieee-floats :alexandria)
  :components
  ((:module :src
            :components
            ((:file "package")
             (:file "listen"
                    :depends-on ("package"))
             (:file "macros"
                    :depends-on ("package"))
             (:file "generic-functions"
                    :depends-on ("package"
                                 "constants"))
             (:file "remote-node"
                    :depends-on ("package"
                                 "listen"
                                 "atom-cache"
                                 "special-variables"))
             (:file "conditions"
                    :depends-on ("package"))
             (:file "constants"
                    :depends-on ("package"))
             (:file "atom-cache"
                    :depends-on ("package"))
             (:file "atom-cache-entries"
                    :depends-on ("package"
                                 "atom-cache"
                                 "special-variables"))
             (:file "special-variables"
                    :depends-on ("package"))
             (:file "bops"
                    :depends-on ("package"))
             (:file "classes"
                    :depends-on ("package"))
             (:file "type-erlang-translatable"
                    :depends-on ("package"
                                 "classes"))
             (:file "distribution-header"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "atom-cache"
                                 "special-variables"
                                 "bops"))
             (:file "ieee-floats" ;; Needs IEEE-Floats library
                    :depends-on ("package"
                                 "bops"))
             (:file "epmd"
                    :depends-on ("package"
                                 "conditions"
                                 "remote-node"
                                 "bops"
                                 "constants"))
             (:file "decode"
                    :depends-on ("package"
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
                    :depends-on ("package"
                                 "conditions"
                                 "bops"
                                 "constants"
                                 "md5"))
             (:file "md5" ;; Needs MD5 library
                    :depends-on ("package"))
             (:file "control-message"
                    :depends-on ("package"
                                 "generic-functions"
                                 "constants"
                                 "classes"
                                 "decode"))
             (:file "node-protocol"
                    :depends-on ("package"
                                 "macros"
                                 "conditions"
                                 "distribution-header"
                                 "classes"
                                 "bops"
                                 "control-message"
                                 "remote-node"))
             (:file "erlang-atom"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-binary"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-float"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "ieee-floats"
                                 "bops"))
             (:file "erlang-fun"
                    :depends-on ("package"
                                 "macros"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-integer"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "bops"))
             (:file "erlang-list"
                    :depends-on ("package"
                                 "macros"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-pid"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "classes"
                                 "bops"))
             (:file "erlang-port"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "classes"
                                 "bops"))
             (:file "erlang-reference"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "classes"
                                 "bops"))
             (:file "erlang-string"
                    :depends-on ("package"
                                 "constants"
                                 "conditions"
                                 "special-variables"
                                 "bops"))
             (:file "erlang-tuple"
                    :depends-on ("package"
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
