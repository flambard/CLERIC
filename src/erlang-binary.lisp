(in-package :cleric)

;;;;
;;;; Erlang binary
;;;;

(defclass erlang-binary (erlang-object)
  ((bytes :reader bytes
          :initarg :bytes
          :documentation "Returns a vector of bytes from an Erlang binary.")
   (bits :reader bits-in-last-byte
         :initarg :bits
         :initform 8
         :documentation
         "The number of bits in the last byte of an Erlang binary."))
  (:documentation "Erlang binary."))


;;;
;;; Methods
;;;

(defmethod print-object ((object erlang-binary) stream)
  (print-unreadable-object (object stream :type t)
    (if (= 8 (bits-in-last-byte object))
        (format stream "<~{~s~^ ~}>" (coerce (bytes object) 'list))
        (format stream "<~{~s~^ ~}:~a>" (coerce (bytes object) 'list)
                (bits-in-last-byte object)))))

(defun binary (&rest bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(defun string-to-binary (string)
  "Creates an Erlang binary from the characters in STRING."
  (make-instance 'erlang-binary :bytes (string-to-bytes string)))

(defun bytes-to-binary (bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(defun binary-to-string (binary)
  "Translates the bytes in BINARY to an ASCII string."
  (bytes-to-string (bytes binary)))

(defmethod size ((x erlang-binary))
  "The byte-size of Erlang binary X."
  (length (bytes x)))

(defmethod match-p ((a erlang-binary) (b erlang-binary))
  (every #'= (bytes a) (bytes b)))


;;;
;;; Encode/Decode
;;;

;; BINARY_EXT
;; +-----+-----+------+
;; |  1  |  4  |  Len |
;; +-----+-----+------+
;; | 109 | Len | Data |
;; +-----+-----+------+
;;

(defun encode-external-binary (erlang-binary)
  (with-slots (bytes) erlang-binary
    (concatenate 'vector
                 (vector +binary-ext+)
                 (uint32-to-bytes (length bytes))
                 bytes)))

(defun read-external-binary (stream) ;; OBSOLETE?
  ;; Assume tag +binary-ext+ is read
  (let* ((length (read-uint32 stream))
         (bytes (read-bytes length stream)))
    (make-instance 'erlang-binary :bytes bytes)))

(defun decode-external-binary (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
        (pos4 (+ 4 pos)))
    (values (make-instance 'erlang-binary
                           :bytes (subseq bytes pos4 (+ pos4 length)))
            (+ pos4 length))))


;; BIT_BINARY_EXT
;; +----+-----+------+------+
;; |  1 |  4  |   1  |  Len |
;; +----+-----+------+------+
;; | 77 | Len | Bits | Data |
;; +----+-----+------+------+
;;

(defun encode-external-bit-binary (erlang-binary)
  (with-slots (bytes bits) erlang-binary
    (concatenate 'vector
                 (vector +bit-binary-ext+)
                 (uint32-to-bytes (length bytes))
                 (vector bits)
                 bytes)))

(defun read-external-bit-binary (stream) ;; OBSOLETE?
  ;; Assume tag +bit-binary-ext+ is read
  (let* ((length (read-uint32 stream)))
    (make-instance 'erlang-binary
                   :bits (read-byte stream)
                   :bytes (read-bytes length stream))))

(defun decode-external-bit-binary (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
        (bits (aref bytes (+ 4 pos)))
        (pos5 (+ pos 5)))
    (values (make-instance 'erlang-binary
                           :bits bits
                           :bytes (subseq bytes pos5 (+ pos5 length)))
            (+ pos5 length))))
