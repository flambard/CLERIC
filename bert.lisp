;; BERT (Binary ERlang Term)
;;
;; See http://bert-rpc.org/
;;

(in-package :bert)


(deftype bert-translatable ()
  "A type that encompasses all types of Lisp objects that can be translated to BERT objects."
  '(satisfies bert-translatable-p))

(defun bert-translatable-p (object)
  "Returns true if OBJECT is translatable to an Erlang object."
  (typecase object
    ((or integer float symbol string hash-table erlang-tuple erlang-binary time regex)
     t)
    (list
     (every #'bert-translatable-p object))
    (t
     nil)))


(defmethod encode (object)
  (cleric:encode object :version-tag t))

(defun decode (bytes)
  (cleric:decode bytes :version-tag t))

