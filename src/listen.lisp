(in-package :cleric)

(defun listening-p ()
  (not (null *listening-socket*)))

(defun listening-port ()
  (when *listening-socket*
    (usocket:get-local-port *listening-socket*)))

(defun stop-listening ()
  (when *listening-socket*
    (usocket:socket-close *listening-socket*)
    (setf *listening-socket* nil)))

(defun start-listening ()
  (if *listening-socket*
      (error 'already-listening-on-socket)
      (progn
        (setf *listening-socket*
              (usocket:socket-listen usocket:*wildcard-host*
                                     usocket:*auto-port*
                                     :element-type '(unsigned-byte 8)))
        t)))

(defun accept-connect ()
  (if *listening-socket*
      (let ((socket (usocket:socket-accept *listening-socket*
                                           :element-type '(unsigned-byte 8))))
        (setf (usocket:socket-stream socket)
              (make-flexi-stream (usocket:socket-stream socket)))
        socket)
      (error 'not-listening-on-socket)))
