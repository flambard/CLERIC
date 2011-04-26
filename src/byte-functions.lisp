(in-package :cleric)

(defun bytes-to-unsigned-integer (bytes &optional (number-of-bytes nil) (pos 0))
  (let ((n (if number-of-bytes number-of-bytes (length bytes))))
    (usocket:octet-buffer-to-integer bytes n :start pos)))

(defun bytes-to-uint16 (bytes &optional (pos 0))
  (bytes-to-unsigned-integer bytes 2 pos))

(defun bytes-to-uint32 (bytes &optional (pos 0))
  (bytes-to-unsigned-integer bytes 4 pos))


(defun read-uint16 (stream)
  (bytes-to-unsigned-integer (read-bytes 2 stream) 2))

(defun read-uint32 (stream)
  (bytes-to-unsigned-integer (read-bytes 4 stream) 4))


(defun bytes-to-signed-int32 (bytes &optional (pos 0))
  (let ((int 0))
    (setf (ldb (byte 7 24) int) (aref bytes (+ 0 pos))) ;; All bits except the sign
    (setf (ldb (byte 8 16) int) (aref bytes (+ 1 pos)))
    (setf (ldb (byte 8 8) int) (aref bytes (+ 2 pos)))
    (setf (ldb (byte 8 0) int) (aref bytes (+ 3 pos)))
    (if (= 1 (ldb (byte 1 7) (aref bytes (+ 0 pos)))) ;; The sign bit
	(- (1+ (logxor int #x7FFFFFFF))) ;; Two's complement
	int)))

(defun read-signed-int32 (stream)
  (bytes-to-signed-int32 (read-bytes 4 stream)))


(defun unsigned-integer-to-bytes (uint number-of-bytes)
  (let ((bytes (make-array number-of-bytes :element-type '(unsigned-byte 8))))
    (usocket:integer-to-octet-buffer uint bytes number-of-bytes)))

(defun uint16-to-bytes (int)
  (unsigned-integer-to-bytes int 2))

(defun uint32-to-bytes (int)
  (unsigned-integer-to-bytes int 4))


(defun string-to-bytes (string)
  (map 'simple-vector #'char-code string))

(defun bytes-to-string (bytes &optional length (pos 0))
  (map 'string #'code-char (subseq bytes pos (when length (+ pos length)))))


(defun read-bytes (n stream)
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    ;; Does it block until the whole sequence is filled when reading from a socket?
    bytes))

(defun read-bytes-as-string (n stream)
  (bytes-to-string (read-bytes n stream)))
