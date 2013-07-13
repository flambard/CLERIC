(defpackage #:cleric-test
  (:documentation "Unit tests for CLERIC.")
  (:use #:cl #:cleric #:eos)
  (:shadow #:run-all-tests)
  (:export

   #:run-all-tests

   ))

(in-package :cleric-test)

(def-suite cleric)

(defun run-all-tests ()
  (run! 'cleric))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cleric-test))))
  (run-all-tests))
