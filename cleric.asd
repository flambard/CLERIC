(defpackage :common-lisp-erlang-interface-system
  (:nicknames :cleric-system)
  (:use :cl))

(in-package :cleric-system)

(asdf:defsystem :cleric
  :description "Common Lisp Erlang Interface - An implementation of the Erlang distribution protocol."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.2.1"
  :license "MIT License"
  :depends-on (:erlang-term :usocket :flexi-streams :md5 :alexandria)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "listen"
                    :depends-on ("packages"))
             (:file "local-node"
                    :depends-on ("packages"))
             (:file "remote-node"
                    :depends-on ("packages"
                                 "handshake"
                                 "listen"
                                 "local-node"
                                 "atom-cache"))
             (:file "atom-cache"
                    :depends-on ("packages"))
             (:file "atom-cache-entries"
                    :depends-on ("packages"
                                 "atom-cache"))
             (:file "distribution-header"
                    :depends-on ("packages"
                                 "atom-cache"))
             (:file "epmd-protocol"
                    :depends-on ("packages"))
             (:file "epmd-client"
                    :depends-on ("packages"
                                 "epmd-protocol"))
             (:file "handshake"
                    :depends-on ("packages"
                                 "local-node"
                                 "md5"))
             (:file "md5" ;; Needs MD5 library
                    :depends-on ("packages"))
             (:file "control-message"
                    :depends-on ("packages"))
             (:file "node-protocol"
                    :depends-on ("packages"
                                 "distribution-header"
                                 "control-message"
                                 "remote-node"))
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
