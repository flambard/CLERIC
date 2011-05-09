(in-package :cleric)

;;;;
;;;; Erlang float
;;;;

;;;
;;; Encode/Decode
;;;

;; NEW_FLOAT_EXT
;; +----+------------+
;; |  1 |      8     |
;; +----+------------+
;; | 70 | IEEE float |
;; +----+------------+
;;

(defun encode-external-new-float (double-float)
  (concatenate 'vector
               (vector +new-float-ext+)
               (double-float-to-bytes double-float)))

(defun read-external-new-float (stream) ;; OBSOLETE?
  ;; Assume tag +new-float-ext+ is read
  (decode-external-new-float (read-bytes 8 stream)))

(defun decode-external-new-float (bytes &optional (pos 0))
  (values (bytes-to-double-float (subseq bytes pos))
          (+ 8 pos)))


;; FLOAT_EXT (superseded by NEW_FLOAT_EXT)
;; +----+--------------+
;; |  1 |      31      |
;; +----+--------------+
;; | 99 | Float String |
;; +----+--------------+
;;
;; The float is stored in string format, created with sprintf with format "%.20e".
;; To unpack the float use sscanf with format "%lf".

(defun encode-external-float (float)
  (concatenate 'vector
               (vector +float-ext+)
               (string-to-bytes (format nil "~(~,20E~)" float))))

(defun read-external-float (stream) ;; OBSOLETE?
  ;; Assume tag +float-ext+ is read
  (decode-external-float (read-bytes 31 stream)))

(defun decode-external-float (bytes &optional (pos 0))
  (let ((string (bytes-to-string bytes 31 pos)))
    (values (read-from-string string)
            (+ 31 pos))))
