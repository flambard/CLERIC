(in-package :cleric)

(deftype erlang-translatable ()
  "A type that encompasses all types of Lisp objects that can be translated to Erlang objects."
  '(satisfies erlang-translatable-p))

(defun erlang-translatable-p (object)
  "Returns true if OBJECT is translatable to an Erlang object."
  (typecase object
    ((or erlang-object integer float symbol string)
     t)
    (list
     (every #'erlang-translatable-p object))
    (t
     nil)))
