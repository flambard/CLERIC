;; BERT (Binary ERlang Term)
;;
;; See http://bert-rpc.org/
;;

(in-package :bert)


(defgeneric translate-complex-type (object)
  (:documentation "Translates tuples with the 'bert' tag to corresponding Lisp objects and vice versa."))


(defclass bert-time ()
  ((megaseconds :reader megaseconds :initarg :megaseconds)
   (seconds :reader seconds :initarg :seconds)
   (microseconds :reader microseconds :initarg :microseconds))
  (:documentation "BERT time data type"))

(defmethod encode ((time bert-time))
  (with-slots (megaseconds seconds microseconds) time
    (encode (tuple '|bert| '|time| megaseconds seconds microseconds))))


(defclass bert-regex ()
  ((source :reader regex-source :initarg :source)
   (options :reader regex-options :initarg :options))
  (:documentation "BERT regex data type"))

(defmethod encode ((regex bert-regex))
  (with-slots (source options) regex
    (encode (tuple '|bert| '|regex| (string-to-binary source) options))))


(defmethod encode ((dict hash-table))
  (encode
   (tuple '|bert| '|dict|
	  (loop
	     for key being the hash-keys in dict using (hash-value value)
	     collect (tuple key value)) )))


(defmethod encode (object)
  (cleric:encode object :version-tag t))


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


(defun complex-type-p (bert-term)
  (when (and (typep bert-term 'tuple) (> 0 (arity bert-term)))
    (let ((first-element (aref (elements bert-term) 0)))
      (and (symbolp first-element)
	   (string= "bert" (symbol-name first-element))))))


(defun decode (bytes)
  (cleric:decode bytes :version-tag t))

