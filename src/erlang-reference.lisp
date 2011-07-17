(in-package :cleric)

;;;;
;;;; Erlang reference
;;;;

(defclass erlang-reference (erlang-identifier)
  ()
  (:documentation "Erlang ref."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-reference) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id) object
      (format stream "~a <~{~a~^.~}>" node
              (nreverse (mapcar #'bytes-to-uint32 (four-byte-blocks id)))))))

(defun four-byte-blocks (bytes)
  (loop
     repeat (/ (length bytes) 4)
     for pos from 0 by 4
     collect (subseq bytes pos (+ 4 pos))))


;;;
;;; Encode/Decode
;;;

(defmethod encode ((x erlang-reference) &key &allow-other-keys)
  (if (alexandria:length= 4 (slot-value x 'id))
      (encode-external-reference x) ;; Perhaps always use new reference?
      (encode-external-new-reference x)))


;; REFERENCE_EXT
;; +-----+------+----+----------+
;; |  1  |   N  |  4 |     1    |
;; +-----+------+----+----------+
;; | 101 | Node | ID | Creation |
;; +-----+------+----+----------+
;;

(defun encode-external-reference (ref)
  (with-slots (node id creation) ref
    (concatenate '(vector octet)
                 (vector +reference-ext+)
                 (encode node)
                 id
                 (vector creation))))

(defun read-external-reference (stream) ;; OBSOLETE?
  ;; Assume tag +reference-ext+ is read
  (make-instance 'erlang-reference
                 :node (read-erlang-atom stream)
                 :id (read-bytes 4 stream)
                 :creation (read-byte stream)))

(defun decode-external-reference (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-reference
                           :node node
                           :id (subseq bytes pos1 (+ pos1 4))
                           :creation (aref bytes (+ pos1 4)))
            (+ pos1 5))))


;; NEW_REFERENCE_EXT
;; +-----+-----+------+----------+--------+
;; |  1  |  2  |   N  |     1    |    N'  |
;; +-----+-----+------+----------+--------+
;; | 114 | Len | Node | Creation | ID ... |
;; +-----+-----+------+----------+--------+
;;

(defun encode-external-new-reference (ref)
  (with-slots (node creation id) ref
    (concatenate '(vector octet)
                 (vector +new-reference-ext+)
                 (uint16-to-bytes (/ (length id) 4))
                 (encode node)
                 (vector creation)
                 id))) ;; Several 4-byte IDs..

(defun read-external-new-reference (stream) ;; OBSOLETE?
  ;; Assume tag +new-reference-ext+ is read
  (let ((length (read-uint16 stream)))
    (make-instance 'erlang-reference
                   :node (read-erlang-atom stream)
                   :creation (read-byte stream)
                   :id (read-bytes (* 4 length) stream))))

(defun decode-external-new-reference (bytes &optional (pos 0))
  (let ((length (bytes-to-uint16 bytes pos)))
    (multiple-value-bind (node pos1) (decode-erlang-atom bytes (+ 2 pos))
      (values (make-instance 'erlang-reference
                             :node node
                             :creation (aref bytes pos1)
                             :id (subseq bytes
                                         (1+ pos1)
                                         (+ 1 pos1 (* 4 length))))
              (+ 1 pos1 (* 4 length))))))
