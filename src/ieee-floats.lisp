;;;; Depends on the IEEE-Floats library

(in-package :cleric-etf)

(defun double-float-to-bytes (f)
  (let ((bits (ieee-floats:encode-float64 f)))
    (unsigned-integer-to-bytes bits 8)))

(defun bytes-to-double-float (bytes)
  (let ((bits (bytes-to-unsigned-integer bytes 8)))
    (ieee-floats:decode-float64 bits)))
