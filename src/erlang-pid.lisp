(in-package :cleric)

;;;;
;;;; Erlang pid
;;;;

(defclass erlang-pid (erlang-identifier)
  ((serial :initarg :serial))
  (:documentation "Erlang PID."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-pid) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id serial) object
      (format stream "~a <~a.~a>"
              node (bytes-to-uint32 id) (bytes-to-uint32 serial)))))

;; Pids in Erlang are printed like this <X.id.serial>
;; where X = some number representing the node.

(defun make-pid ()
  "Create a new Erlang PID."
  (make-instance 'erlang-pid
                 :node (make-symbol *this-node*)
                 :id (generate-new-pid-id)
                 :serial #(0 0 0 0) ;; What to set here?
                 :creation 1)) ;; What to set here?

(defun generate-new-pid-id ()
  (uint32-to-bytes (incf *pid-id-counter*)))

;; Not mentioned in the documentation: Serial only uses the least significant 13 bits!

(defmethod match-p ((a erlang-pid) (b erlang-pid))
  (and (call-next-method)
       (every #'= (slot-value a 'serial) (slot-value b 'serial))))


;;;
;;; Encode/Decode
;;;

(defun read-erlang-pid (stream) ;; OBSOLETE?
  (let ((tag (read-byte stream)))
    (case tag
      (#.+pid-ext+
       (read-external-pid stream))
      (#.+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-erlang-term
              :received-tag tag
              :expected-tags (list +pid-ext+
                                   +compressed-term+))) )))

(defun decode-erlang-pid (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (case tag
      (#.+pid-ext+
       (decode-external-pid bytes (1+ pos)))
      (#.+compressed-term+
       (decode-compressed-erlang-term bytes (1+ pos)))
      (otherwise
       (error 'unexpected-erlang-term
              :received-tag tag
              :expected-tags (list +pid-ext+
                                   +compressed-term+))) )))


;; PID_EXT
;; +-----+------+----+--------+----------+
;; |  1  |   N  |  4 |    4   |     1    |
;; +-----+------+----+--------+----------+
;; | 103 | Node | ID | Serial | Creation |
;; +-----+------+----+--------+----------+
;;

(defun encode-external-pid (pid)
  (with-slots (node id serial creation) pid
    (concatenate 'vector
                 (vector +pid-ext+)
                 (encode node)
                 id
                 serial
                 (vector creation))))

(defun read-external-pid (stream) ;; OBSOLETE?
  ;; Assume tag +pid-ext+ is read
  (make-instance 'erlang-pid
                 :node (read-erlang-atom stream)
                 :id (read-bytes 4 stream)
                 :serial (read-bytes 4 stream)
                 :creation (read-byte stream)))

(defun decode-external-pid (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-pid
                           :node node
                           :id (subseq bytes pos1 (+ pos1 4))
                           :serial (subseq bytes (+ pos1 4) (+ pos1 8))
                           :creation (aref bytes (+ pos1 8)))
            (+ pos1 9))))
