;;;; Atom cache entries

(in-package :cleric)

(defclass atom-cache-collector ()
  ((atom-cache
    :initarg :atom-cache
    :documentation "The atom cache used for a node connection.")
   (cache-references
    :reader cache-references
    :initarg :cache-references
    :initform (make-cache-reference-collection)
    :documentation "The collection of cache references."))
  (:documentation "The atom cache collector is used to retrieve already cached
atoms and to collect atom references to be used in a protocol message."))


(defun make-atom-cache-collector (atom-cache &optional cache-references)
  (if (null cache-references)
      (make-instance 'atom-cache-collector :atom-cache atom-cache)
      (make-instance 'atom-cache-collector
                     :atom-cache atom-cache
                     :cache-references cache-references)))


;;;
;;; Implementation of the ERLANG-TERM atom cache interface
;;;

(defmethod etf-aci:get-atom ((ref integer) (collector atom-cache-collector))
  (let ((cached-atom (get-cache-reference ref (cache-references collector))))
    (if (consp cached-atom)
        (values (car cached-atom) t)
        (values nil nil))))

(defmethod etf-aci:put-atom ((atom symbol) (collector atom-cache-collector))
  (with-slots (atom-cache cache-references) collector
    (let ((ref (find-cache-reference atom cache-references)))
      (if (integerp ref)
          ref ;; Atom used earlier in message. Use the same reference.
          (let ((existing-pos (atom-cache-location-of atom atom-cache)))
            (if (consp existing-pos)
                (add-cache-reference atom ;; Make a new cache reference
                                     nil
                                     (car existing-pos)
                                     (cdr existing-pos)
                                     cache-references)
                (let ((free-pos (atom-cache-find-free atom-cache)))
                  (if (null free-pos)
                      nil ;; There was no space left in the atom cache.
                      (add-cache-reference atom ;; Make a new cache reference
                                           t
                                           (car free-pos)
                                           (cdr free-pos)
                                           cache-references))) )) ))))


;;;
;;; Cache reference collection
;;;

(defun make-cache-reference-collection ()
  (make-array 256
              :fill-pointer 0
              :adjustable nil
              :initial-element t))

(defun get-cache-reference (ref collection)
  (aref collection ref))

(defun find-cache-reference (atom collection)
  (position atom collection :key #'car :test #'eq))

(defun add-cache-reference (atom new-p s-index internal-s-index collection)
  (when (vector-push (list atom new-p s-index internal-s-index) collection)
    (1- (fill-pointer collection))))
