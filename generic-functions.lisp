(in-package :cleric)

;;;;
;;;; ENCODE - For encoding Erlang terms
;;;;

(defgeneric encode (erlang-translatable-object &key version-tag)
  (:documentation "Encodes the Erlang translatable object to a vector of bytes."))

(defmethod encode :around (x &key version-tag)
  (if version-tag
      (concatenate 'vector (vector +protocol-version+) (call-next-method x))
      (call-next-method x)))


;;;;
;;;; ENCODE-CONTROL-MESSAGE - For encoding Control Messages
;;;;

(defgeneric encode-control-message (control-message &key version-tag)
  (:documentation "Encodes the Control Message to a vector of bytes."))


;;;;
;;;; SIZE
;;;;

(defgeneric size (tuple-or-binary)
  (:documentation "Returns the size of an Erlang tuple or binary."))
