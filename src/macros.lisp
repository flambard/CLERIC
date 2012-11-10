(in-package :cleric-etf)

(defmacro multiple-value-bind* (bind-clauses &body body)
  (if (null bind-clauses) `(progn ,@body)
      (destructuring-bind ((varlist values-form) &rest rest) bind-clauses
        (if rest
            `(multiple-value-bind ,varlist ,values-form
               (multiple-value-bind* ,rest ,@body))
            `(multiple-value-bind ,varlist ,values-form
               ,@body) ))))
