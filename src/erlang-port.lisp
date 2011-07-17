(in-package :cleric)

;;;;
;;;; Erlang port
;;;;

(defclass erlang-port (erlang-identifier)
  ()
  (:documentation "Erlang port."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-port) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id) object
      (format stream "~a <~a>" node (bytes-to-uint32 id)))))


;;;
;;; Encode/Decode
;;;

(defmethod encode ((x erlang-port) &key &allow-other-keys)
  (encode-external-port x))


;; PORT_EXT
;; +-----+------+----+----------+
;; |  1  |   N  |  4 |     1    |
;; +-----+------+----+----------+
;; | 102 | Node | ID | Creation |
;; +-----+------+----+----------+
;;

(defun encode-external-port (port)
  (with-slots (node id creation) port
    (concatenate '(vector octet)
                 (vector +port-ext+)
                 (encode node)
                 id
                 (vector creation))))

(defun decode-external-port (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-port
                           :node node
                           :id (subseq bytes pos1 (+ pos1 4))
                           :creation (aref bytes (+ pos1 4)))
            (+ pos1 5))))
