;;;; Functions for querying EPMD (Erlang Port Mapped Daemon)

(in-package :cleric-epmd-client)

;;; EPMD port
(defconstant +epmd-port+ 4369
  "The default TCP port the EPMD listens on.")

(defvar *epmd-socket* nil
  "The EPMD socket. NIL if not registered in EPMD.")



(defun connect-to-epmd (&optional (host "localhost"))
  (let ((socket (handler-case (usocket:socket-connect host
                                                      +epmd-port+
                                                      :element-type 'octet)
                  (usocket:connection-refused-error ()
                    (error 'unreachable-error))
                  (usocket:unknown-error ()
                    (error 'host-unknown-error)))))
    (setf (usocket:socket-stream socket)
          (make-flexi-stream (usocket:socket-stream socket)))
    socket))


;;;
;;; WITH-EPMD-CONNECTION-STREAM macro
;;;

(defmacro with-epmd-connection-stream
    ((stream-var &optional (host "localhost")) &body body)
  "Create a local scope where STREAM-VAR is a socket stream connected to the EPMD."
  (let ((socket-var (gensym)))
    `(let* ((,socket-var (connect-to-epmd ,host))
            (,stream-var (usocket:socket-stream ,socket-var)))
       (unwind-protect (progn ,@body)
         (usocket:socket-close ,socket-var))) ))


;;;
;;; EPMD API
;;;

(defun publish (node-name listening-port)
  (if *epmd-socket*
      (error 'already-registered)
      (let* ((socket (connect-to-epmd))
             (epmd (usocket:socket-stream socket)))
        (write-alive2-request epmd node-name listening-port)
        (finish-output epmd)
        (let ((creation (read-alive2-response epmd)))
          (declare (ignore creation))
          (setf *epmd-socket* socket)
          t))))

(defun published-p ()
  (not (null *epmd-socket*)))

(defun unpublish ()
  (when *epmd-socket*
    (usocket:socket-close *epmd-socket*)
    (setf *epmd-socket* nil)
    t))

(defun lookup-node (node-name &optional (host "localhost"))
  "Query the EPMD about a node. Returns a REMOTE-NODE object that represents the node."
  (with-epmd-connection-stream (epmd host)
    (write-port-please2-request epmd node-name)
    (finish-output epmd)
    (read-port-please2-response epmd host)))

(defun print-all-registered-nodes (&optional (host "localhost") (stream t))
  "Query the EPMD about all registered nodes and print the information."
  (with-epmd-connection-stream (epmd host)
    (write-names-request epmd)
    (finish-output epmd)
    (multiple-value-bind (epmd-port node-info)
        (read-names-response epmd)
      (declare (ignore epmd-port))
      (format stream "~{~a~%~}" node-info)
      t)))


;;;
;;; Conditions
;;;

(define-condition already-registered (error)
  ()
  (:documentation "This error is signaled when trying to register on the EPMD when already registered."))

(define-condition host-unknown-error (error)
  ;; USOCKET:UNKNOWN-ERROR
  ()
  (:documentation "This error is signaled if the hostname for EPMD is unresolvable."))

(define-condition unreachable-error (error)
  ;; USOCKET:CONNECTION-REFUSED-ERROR
  ()
  (:documentation "This error is signaled when the EPMD is unreachable."))

(define-condition response-error (error)
  ;; Useful?
  ()
  (:documentation "This error is signaled when the EPMD sends an error response."))
