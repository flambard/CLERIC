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


(test sbcl-challenge-md5-digest-bug
    ;; Bug in CLERIC::CALCULATE-DIGEST, it returned same MD5sum for different
    ;; strings! Turns out that MD5:MD5SUM-SEQUENCE is deprecated for strings.
    ;; Use MD5:MD5SUM-STRING instead.
    (is (not (every #'=
                    (cleric::calculate-digest 1234567890 "kaka")
                    (cleric::calculate-digest 1234567891 "kaka"))))
  )
