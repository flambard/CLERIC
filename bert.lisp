;; BERT (Binary ERlang Term)
;;
;; See http://bert-rpc.org/
;;

(in-package :bert)


(defgeneric translate-complex-type (object)
  (:documentation "Translates tuples with the 'bert' tag to corresponding Lisp objects."))

(defgeneric encode (object &key berp-header)
  (:documentation "Encodes the BERT-translatable object to a vector of bytes."))


(defvar true (tuple '|bert| '|true|)
  "BERT boolean true term.")

(defvar false (tuple '|bert| '|false|)
  "BERT boolean false term.")

(defun boolean (value)
  (if value true false))


(defclass bert-time ()
  ((megaseconds :reader megaseconds :initarg :megaseconds)
   (seconds :reader seconds :initarg :seconds)
   (microseconds :reader microseconds :initarg :microseconds))
  (:documentation "BERT time data type"))

(defmethod translate-complex-type ((time bert-time))
  (with-slots (megaseconds seconds microseconds) time
    (tuple '|bert| '|time| megaseconds seconds microseconds)))


(defclass bert-regex ()
  ((source :reader regex-source :initarg :source)
   (options :reader regex-options :initarg :options))
  (:documentation "BERT regex data type"))

(defmethod translate-complex-type ((regex bert-regex))
  (with-slots (source options) regex
    (tuple '|bert| '|regex| (string-to-binary source) options)))


(defmethod translate-complex-type ((dict hash-table))
  (tuple '|bert| '|dict|
	 (loop
	    for key being the hash-keys in dict using (hash-value value)
	    collect (tuple (translate-complex-type key)
			   (translate-complex-type value))) ))


(defmethod translate-complex-type ((nil-symbol (eql nil)))
  (declare (ignore nil-symbol))
  (tuple '|bert| '|nil|))


(defmethod translate-complex-type ((tuple erlang-tuple))
  (with-slots (elements) tuple
    (make-instance 'erlang-tuple
		   :elements (map 'vector #'translate-complex-type elements))))


(defmethod translate-complex-type ((lst list))
  (mapcar #'translate-complex-type lst))


(defmethod translate-complex-type (object)
  object)


(defmethod encode (object &key berp-header)
  (let ((bytes (cleric:encode (translate-complex-type object) :version-tag t)))
    (if berp-header
	(concatenate 'vector (cleric::uint32-to-bytes (length bytes)) bytes)
	bytes)))


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


(defun simple-bert-term-p (bert-term)
  (typep bert-term '(or integer float symbol string erlang-binary)))

(defun compound-bert-term-p (bert-term)
  (typep bert-term '(or list erlang-tuple)))

(defun complex-bert-term-p (bert-term)
  (when (and (typep bert-term 'erlang-tuple) (< 0 (arity bert-term)))
    (let ((first-element (aref (elements bert-term) 0)))
      (and (symbolp first-element)
	   (string= "bert" (symbol-name first-element))))))


(defun translate-complex-terms (term)
  (cond
    ((simple-bert-term-p term)
     term)
    ((listp term)
     (mapcar #'translate-complex-terms term))
    ((complex-bert-term-p term)
     (translate-complex-term term))
    ((typep term 'erlang-tuple)
     (with-slots (elements) term
       (make-instance
	'erlang-tuple
	:elements (map 'vector #'translate-complex-terms elements))))
    (t
     (error "~a is not a BERT term." term)) ))

(defun translate-complex-term (term)
  (assert (typep term 'erlang-tuple))
  (with-slots (elements) term
    (assert (string= "bert" (symbol-name (aref elements 0))))
    (ecase (intern (symbol-name (aref elements 1)) :keyword)
      (:|nil| nil)
      (:|true| t)
      (:|false| nil)
      (:|dict|
       (translate-dict-term (aref elements 2)))
      (:|time|
       (translate-time-term (aref elements 2)
			    (aref elements 3)
			    (aref elements 4)))
      (:|regex|
       (translate-regex-term (aref elements 2)
			     (aref elements 3))) )))

(defun translate-dict-term (dict)
  (loop
     with hash = (make-hash-table)
     for tuple in dict
     do (let* ((elements (elements tuple))
	       (key (aref elements 0))
	       (value (aref elements 1)))
	  (setf (gethash key hash) value))
     finally (return hash)))

(defun translate-time-term (megaseconds seconds microseconds)
  (make-instance 'bert-time
		 :megaseconds megaseconds
		 :seconds seconds
		 :microseconds microseconds))

(defun translate-regex-term (source options)
  (make-instance 'bert-regex
		 :source source
		 :options options))


(defun decode (bytes)
  (translate-complex-terms (cleric:decode bytes :version-tag t)))

