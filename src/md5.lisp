;;;; Depends on the MD5 library

(in-package :cleric)

(defun calculate-digest (challenge cookie)
  (let ((input (format nil "~a~a" cookie challenge)))
    #+(or :cmu :sbcl (and :lispworks (not :lispworks4)) :ccl :allegro)
    (md5:md5sum-string input)
    #-(or :cmu :sbcl (and :lispworks (not :lispworks4)) :ccl :allegro)
    (md5:md5sum-sequence (map '(vector (unsigned-byte 8)) #'char-code input))))
