(in-package :cleric)

(defvar *listening-socket* nil
  "The listening socket. NIL if not listening.")


;;;
;;; Conditions
;;;

(define-condition already-listening-on-socket (error)
  ((port :reader port :initarg :port))
  (:documentation "This error is signaled when trying to listen on a socket when already listening on an existing socket."))

(define-condition not-listening-on-socket (error)
  ()
  (:documentation "This condition is signaled when trying to accept connections with a listening socket."))

(defun start-listening-on-socket-restart (condition)
  (declare (ignore condition))
  (invoke-restart 'start-listening-on-socket))


;;;
;;; Socket listening functions
;;;

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
