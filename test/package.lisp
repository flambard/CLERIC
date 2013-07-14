(defpackage #:cleric-test
  (:documentation "Unit tests for CLERIC.")
  (:use #:cl #:cleric #:cleric-handshake-protocol #:eos #:flexi-streams)
  (:shadow #:run-all-tests)
  (:export

   #:run-all-tests

   ))

(in-package :cleric-test)

(def-suite cleric)
(def-suite handshake)

(defun run-all-tests ()
  (run! 'cleric)
  (run! 'handshake))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cleric-test))))
  (run-all-tests))
