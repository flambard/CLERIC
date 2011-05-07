(in-package #:cl-user)

(defpackage #:cleric-test
  (:use #:cl #:cleric))

(in-package #:cleric-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(eos:def-suite eos:in-suite eos:run! eos:signals
            eos:is eos:is-false eos:is-true eos:is-every))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(eos:test ,name ,@body))

(def-suite cleric)

(defun run-all-tests ()
  (run! 'cleric))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :cleric-test))))
  (run-all-tests))
