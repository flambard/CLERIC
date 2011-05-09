(in-package :cleric)

;;;;
;;;; Erlang tuple
;;;;

(defclass erlang-tuple (erlang-object)
  ((elements :reader elements :initarg :elements))
  (:documentation "Erlang tuple."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "{~{~s~^ ~}}" (coerce (elements object) 'list))))

(defun tuple (&rest erlang-translatable-objects)
  "Create an Erlang tuple"
  (make-instance 'erlang-tuple
                 :elements (coerce erlang-translatable-objects 'vector)))

(defmethod arity ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (length (elements x)))

(defmethod size ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (arity x))

(defun erlang-tuple-ref (tuple pos)
  (svref (elements tuple) pos))

(defun erlang-tuple-arity (tuple)
  (length (elements tuple)))


;;;
;;; Encode/Decode
;;;

(defun read-erlang-tuple (stream &optional use-version-tag) ;; OBSOLETE?
  (when use-version-tag
    (let ((version (read-byte stream)))
      (unless (= version +protocol-version+)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list +protocol-version+)))))
  (let ((tag (read-byte stream)))
    (case tag
      (#.+small-tuple-ext+ (read-external-small-tuple stream))
      (#.+large-tuple-ext+ (read-external-large-tuple stream))
      (#.+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-message-tag-error
              :received-tag tag
              :expected-tags (list +small-tuple-ext+
                                   +large-tuple-ext+
                                   +compressed-term+))) )))

(defun decode-erlang-tuple (bytes &key (start 0) (version-tag nil))
  (when version-tag
    (let ((version (aref bytes start)))
      (unless (= version +protocol-version+)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list +protocol-version+))))
    (incf start))
  (let ((tag (aref bytes start)))
    (case tag
      (#.+small-tuple-ext+ (decode-external-small-tuple bytes (1+ start)))
      (#.+large-tuple-ext+ (decode-external-large-tuple bytes (1+ start)))
      (#.+compressed-term+
       (decode-compressed-erlang-term bytes (1+ start)))
      (otherwise
       (error 'unexpected-message-tag-error
              :received-tag tag
              :expected-tags (list +small-tuple-ext+
                                   +large-tuple-ext+
                                   +compressed-term+))) )))


;; SMALL_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   1   |     N    |
;; +-----+-------+----------+
;; | 104 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-small-tuple (tuple atom-cache-entries)
  (concatenate 'vector
               (vector +small-tuple-ext+ (erlang-tuple-arity tuple))
               (mapconc-vector
                #'(lambda (element)
                    (encode element :atom-cache-entries atom-cache-entries))
                (elements tuple))))

(defun read-external-small-tuple (stream) ;; OBSOLETE?
  ;; Assume tag +small-tuple-ext+ is read
  (make-instance 'erlang-tuple
                 :elements (read-tuple-contents stream (read-byte stream))))

(defun decode-external-small-tuple (bytes &optional (pos 0))
  (let ((arity (aref bytes pos)))
    (multiple-value-bind (elements new-pos)
        (decode-tuple-contents bytes arity (1+ pos))
      (values (make-instance 'erlang-tuple :elements elements)
              new-pos))))


;; LARGE_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   4   |     N    |
;; +-----+-------+----------+
;; | 105 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-large-tuple (tuple atom-cache-entries)
  (concatenate 'vector
               (vector +large-tuple-ext+)
               (uint32-to-bytes (erlang-tuple-arity tuple))
               (mapconc-vector
                #'(lambda (element)
                    (encode element :atom-cache-entries atom-cache-entries))
                (elements tuple))))

(defun read-external-large-tuple (stream)
  ;; Assume tag +large-tuple-ext+ is read
  (make-instance 'erlang-tuple
                 :elements (read-tuple-contents stream (read-uint32 stream))))

(defun decode-external-large-tuple (bytes &optional (pos 0))
  (let ((arity (bytes-to-uint32 bytes pos)))
    (multiple-value-bind (elements new-pos)
        (decode-tuple-contents bytes arity (+ 4 pos))
      (values (make-instance 'erlang-tuple :elements elements)
              new-pos))))


;;; Helper functions

(defun mapconc-vector (fn vector)
  (loop
     with bytes = #()
     for element across vector
     do (setf bytes (concatenate 'vector bytes (funcall fn element)))
     finally (return bytes)))

(defun read-tuple-contents (stream arity)
  (coerce (loop repeat arity collect (read-erlang-term stream)) 'vector))

(defun decode-tuple-contents (bytes arity pos)
  (loop
     repeat arity
     for (element pos1) = (multiple-value-list (decode bytes :start pos))
     do (setf pos pos1)
     collect element into elements
     finally (return (values (coerce elements 'vector) pos))))
