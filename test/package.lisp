(defpackage #:cleric-test
  (:documentation "Unit tests for CLERIC.")
  (:use #:cl #:cleric #:cleric-handshake-protocol #:flexi-streams #:fiveam)
  (:shadow #:run-all-tests)
  (:export

   #:all-tests
   #:run-all-tests

   ))

(in-package :cleric-test)

(def-suite all-tests)

(def-suite cleric    :in all-tests)
(def-suite handshake :in all-tests)

(defun run-all-tests ()
  (run! 'all-tests))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cleric-test))))
  (run-all-tests))
