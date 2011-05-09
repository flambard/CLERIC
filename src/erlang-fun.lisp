(in-package :cleric)

;;;;
;;;; Erlang function ("Fun")
;;;;

;;; fun M:F/A
(defclass erlang-external-fun (erlang-fun)
  ((function :initarg :function))
  (:documentation "Erlang fun in external format (module:function/arity)."))

;;; fun F/A or fun (...) -> ...
(defclass erlang-internal-fun (erlang-fun)
  ((pid :initarg :pid)
   (index :initarg :index)
   (uniq :initarg :uniq)
   (free-vars :reader free-vars :initarg :free-vars))
  (:documentation "Erlang fun in internal format."))

(defclass erlang-new-internal-fun (erlang-internal-fun)
  ((index :initarg :old-index)
   (uniq :initarg :old-uniq)
   (new-uniq :initarg :uniq)
   (new-index :initarg :index))
  (:documentation "Erlang fun in new internal format."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-external-fun) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (module arity function) object
      (format stream "~a:~a/~a" module function arity))))


;;;
;;; Encode/Decode
;;;

;; FUN_EXT
;; +-----+---------+-----+--------+-------+------+-----------+
;; |  1  |     4   |     |        |       |      |           |
;; +-----+---------+-----+--------+-------+------+-----------+
;; | 117 | NumFree | Pid | Module | Index | Uniq | Free vars |
;; +-----+---------+-----+--------+-------+------+-----------+
;;

(defun encode-external-fun (fun)
  (with-slots (module pid index uniq free-vars) fun
    (concatenate 'vector
                 (vector +fun-ext+)
                 (uint32-to-bytes (length free-vars))
                 (encode pid)
                 (encode module)
                 (encode index)
                 (encode uniq)
                 (list-contents-to-bytes free-vars))))

(defun read-external-fun (stream) ;; OBSOLETE?
  ;; Assume tag +fun-ext+ is read
  (let ((free-vars-length (read-uint32 stream)))
    (make-instance 'erlang-internal-fun
                   :pid (read-erlang-pid stream)
                   :module (read-erlang-atom stream)
                   :index (read-erlang-integer stream)
                   :uniq (read-erlang-integer stream)
                   :free-vars (loop
                                 repeat free-vars-length
                                 collect (read-erlang-term stream)))))

(defun decode-external-fun (bytes &optional (pos 0))
  (let ((free-vars-length (bytes-to-uint32 bytes pos)))
    (multiple-value-bind* (((pid pos1) (decode-erlang-pid bytes (+ pos 4)))
                           ((module pos2) (decode-erlang-atom bytes pos1))
                           ((index pos3) (decode-erlang-integer bytes pos2))
                           ((uniq pos4) (decode-erlang-integer bytes pos3)))
      (loop
         repeat free-vars-length
         for (free-var p) = (multiple-value-list
                             (decode bytes :start pos4))
         do (setf pos4 p)
         collect free-var into free-vars
         finally (return (values (make-instance 'erlang-internal-fun
                                                :pid pid
                                                :module module
                                                :index index
                                                :uniq uniq
                                                :free-vars free-vars)
                                 pos)) ))))


;; NEW_FUN_EXT
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;; |  1  |   4  |   1   |  16  |    4  |     4   |        |          |         |     |           |
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;; | 112 | Size | Arity | Uniq | Index | NumFree | Module | OldIndex | OldUniq | Pid | Free vars |
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;;

(defun encode-external-new-fun (fun)
  (with-slots (module arity pid index uniq free-vars new-uniq new-index) fun
    (let ((bytes (concatenate 'vector
                              (vector arity)
                              new-uniq
                              (uint32-to-bytes new-index)
                              (uint32-to-bytes (length free-vars))
                              (encode module)
                              (encode index)
                              (encode uniq)
                              (encode pid)
                              (list-contents-to-bytes free-vars))))
      (concatenate 'vector
                   (vector +new-fun-ext+)
                   (uint32-to-bytes (+ 4 (length bytes)))
                   bytes))))

(defun read-external-new-fun (stream) ;; OBSOLETE?
  ;; Assume tag +new-fun-ext+ is read
  (let ((size-bytes (read-bytes 4 stream)))
    (decode-external-new-fun
     (concatenate 'vector
                  size-bytes
                  (read-bytes (- (bytes-to-uint32 size-bytes) 4) stream)))))

(defun decode-external-new-fun (bytes &optional (pos 0))
  (let ((size (bytes-to-uint32 bytes pos))
        (arity (aref bytes (+ 4 pos)))
        (uniq (subseq bytes (+ 5 pos) (+ 21 pos)))
        (index (bytes-to-uint32 (+ 21 pos)))
        (free-vars-length (bytes-to-uint32 bytes (+ 25 pos))))
    (multiple-value-bind* (((module pos1) (decode-erlang-atom bytes (+ 29 pos)))
                           ((old-index pos2) (decode-erlang-integer bytes pos1))
                           ((old-uniq pos3) (decode-erlang-integer bytes pos2))
                           ((pid pos4) (decode-erlang-pid bytes pos3)))
      (loop
         repeat free-vars-length
         for (free-var p) = (multiple-value-list
                             (decode bytes :start pos4))
         do (setf pos4 p)
         collect free-var into free-vars
         finally (return (progn
                           (assert (= p (+ size pos)))
                           (values (make-instance 'erlang-new-internal-fun
                                                  :arity arity
                                                  :uniq uniq
                                                  :index index
                                                  :module module
                                                  :old-index old-index
                                                  :old-uniq old-uniq
                                                  :pid pid
                                                  :free-vars free-vars)
                                   p)) )))))



;; EXPORT_EXT
;; +-----+--------+----------+-------+
;; |  1  |        |          |       |
;; +-----+--------+----------+-------+
;; | 113 | Module | Function | Arity |
;; +-----+--------+----------+-------+
;;

(defun encode-external-export (fun)
  (with-slots (module function arity) fun
    (concatenate 'vector
                 (vector +export-ext+)
                 (encode module)
                 (encode function)
                 (encode arity))))

(defun read-external-export (stream) ;; OBSOLETE?
  ;; Assume tag +export-ext+ is read
  (make-instance 'erlang-external-fun
                 :module (read-erlang-atom stream)
                 :function (read-erlang-atom stream)
                 :arity (read-erlang-integer stream)))

(defun decode-external-export (bytes &optional (pos 0))
  (multiple-value-bind* (((module pos1) (decode-erlang-atom bytes pos))
                         ((function pos2) (decode-erlang-atom bytes pos1))
                         ((arity pos3) (decode-erlang-integer bytes pos2)))
    (values (make-instance 'erlang-external-fun
                           :module module
                           :function function
                           :arity arity)
            pos3)))
