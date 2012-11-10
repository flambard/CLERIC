(in-package :cleric)

;;;;
;;;; ENCODE - For encoding Erlang terms
;;;;

(defgeneric encode (erlang-translatable-object &key version-tag atom-cache-entries)
  (:documentation "Encodes the Erlang translatable object to a vector of bytes."))

(defmethod encode :around (x &key version-tag atom-cache-entries)
  (if version-tag
      (concatenate '(vector octet)
                   (vector +protocol-version+)
                   (call-next-method x :atom-cache-entries atom-cache-entries))
      (call-next-method x :atom-cache-entries atom-cache-entries)))


;;;;
;;;; MATCH-P - Predicate for comparing Erlang objects
;;;;

(defgeneric match-p (object-a object-b)
  (:documentation "Predicate for testing if two Erlang objects match."))

(defmethod match-p (a b)
  nil)


;;;;
;;;; ARITY
;;;;

(defgeneric arity (tuple-or-fun)
  (:documentation "Returns the arity of an Erlang tuple or fun."))


;;;;
;;;; SIZE
;;;;

(defgeneric size (tuple-or-binary)
  (:documentation "Returns the size of an Erlang tuple or binary."))
