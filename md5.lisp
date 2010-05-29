;;;; Depends on the MD5 library

(in-package :cleric)

(defun calculate-digest (challenge cookie)
  (md5:md5sum-sequence (format nil "~a~a" cookie challenge)))
