;; BERT (Binary ERlang Term)
;;
;; See http://bert-rpc.org/
;;

(in-package :bert)


(defclass bert-time ()
  ((megaseconds :reader megaseconds :initarg :megaseconds)
   (seconds :reader seconds :initarg :seconds)
   (microseconds :reader microseconds :initarg :microseconds))
  (:documentation "BERT time data type"))

(defclass bert-regex ()
  ((source :reader regex-source :initarg :source)
   (options :reader regex-options :initarg :options))
  (:documentation "BERT regex data type"))

(deftype bert-translatable ()
  "A type that encompasses all types of Lisp objects that can be translated to BERT objects."
  '(satisfies bert-translatable-p))

(defun bert-translatable-p (object)
  "Returns true if OBJECT is translatable to an Erlang object."
  (typecase object
    ((or integer float symbol string hash-table erlang-tuple erlang-binary bert-time bert-regex)
     t)
    (list
     (every #'bert-translatable-p object))
    (t
     nil)))


(defmethod encode (object)
  (cleric:encode object :version-tag t))

(defmethod encode ((dict hash-table))
  (encode
   (tuple '|bert| '|dict|
	  (loop
	     for key being the hash-keys in dict using (hash-value value)
	     collect (tuple key value)) )))


(defun complex-type-p (bert-term)
  (when (and (typep bert-term 'tuple) (> 0 (arity bert-term)))
    (let ((first-element (aref (elements bert-term) 0)))
      (and (symbolp first-element)
	   (string= "bert" (symbol-name first-element))))))


(defgeneric translate-complex-type (object)
  (:documentation "Translates tuples with the 'bert' tag to corresponding Lisp objects and vice versa."))


(defun decode (bytes)
  (cleric:decode bytes :version-tag t))

