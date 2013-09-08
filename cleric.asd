(defpackage :common-lisp-erlang-interface-system
  (:nicknames :cleric-system)
  (:use :cl))

(in-package :cleric-system)

(asdf:defsystem :cleric
  :description "Common Lisp Erlang Interface - An implementation of the Erlang distribution protocol."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.2.2"
  :license "MIT License"
  :depends-on (:epmd
               :erlang-term
               :usocket
               :flexi-streams
               :md5
               :alexandria
               :com.gigamonkeys.binary-data)
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
             (:file "handshake-protocol"
                    :depends-on ("packages"))
             (:file "handshake"
                    :depends-on ("packages"
                                 "handshake-protocol"
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
