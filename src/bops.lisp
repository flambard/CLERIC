(in-package :cleric-bops)

(defun bytes-to-unsigned-integer (bytes &optional (number-of-bytes nil) (pos 0))
  (let ((n (if number-of-bytes number-of-bytes (length bytes))))
    (usocket:octet-buffer-to-integer bytes n :start pos)))

(defun bytes-to-uint16 (bytes &optional (pos 0))
  (nibbles:ub16ref/be bytes pos))

(defun bytes-to-uint32 (bytes &optional (pos 0))
  (nibbles:ub32ref/be bytes pos))


(defun read-uint16 (stream)
  (nibbles:read-ub16/be stream))

(defun read-uint32 (stream)
  (nibbles:read-ub32/be stream))


(defun write-uint16 (int stream)
  (nibbles:write-ub16/be int stream)
  t)

(defun write-uint32 (int stream)
  (nibbles:write-ub32/be int stream)
  t)


(defun bytes-to-signed-int32 (bytes &optional (pos 0))
  (nibbles:sb32ref/be bytes pos))

(defun read-signed-int32 (stream)
  (nibbles:read-sb32/be stream))


(defun unsigned-integer-to-bytes (uint number-of-bytes)
  (let ((bytes (nibbles:make-octet-vector number-of-bytes)))
    (usocket:integer-to-octet-buffer uint bytes number-of-bytes)))

(defun uint16-to-bytes (int)
  (let ((bytes (nibbles:make-octet-vector 2)))
    (setf (nibbles:ub16ref/be bytes 0) int)
    bytes))

(defun uint32-to-bytes (int)
  (let ((bytes (nibbles:make-octet-vector 4)))
    (setf (nibbles:ub32ref/be bytes 0) int)
    bytes))


(defun string-to-bytes (string)
  (map 'simple-vector #'char-code string))

(defun bytes-to-string (bytes &optional length (pos 0))
  (map 'string #'code-char (subseq bytes pos (when length (+ pos length)))))


(defun read-bytes (n stream)
  (let ((bytes (nibbles:make-octet-vector n)))
    (read-sequence bytes stream)
    ;; Does it block until the whole sequence is filled when reading from a socket?
    bytes))

(defun read-string (n stream)
  (let ((str (make-string n)))
    (read-sequence str stream)
    str))
