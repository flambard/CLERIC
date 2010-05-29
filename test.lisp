(in-package :cleric)

(defun send-object (stream from-pid to-name object)
  (write-sequence (message-reg-send from-pid '|| to-name object) stream)
  (finish-output stream))
