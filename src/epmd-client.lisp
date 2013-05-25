;;;; Functions for querying EPMD (Erlang Port Mapped Daemon)

(in-package :cleric-epmd-client)

;;; EPMD port
(defconstant +epmd-port+ 4369
  "The default TCP port the EPMD listens on.")


(defun connect-to-epmd (&optional (host "localhost"))
  (handler-case (usocket:socket-connect host +epmd-port+ :element-type 'octet)
    (usocket:connection-refused-error ()
      (error 'unreachable-error))
    (usocket:unknown-error ()
      (error 'host-unknown-error))))


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

(defclass epmd-connection ()
  ((socket :initarg :socket :reader epmd-connection-socket)
   (creation :initarg :creation)
   (node-name :initarg :node-name :reader published-node-name)
   (node-port :initarg :node-port :reader published-node-port))
  (:documentation "An open connection to an EPMD server held as long as the
node is published on that EPMD."))


(defun publish (node-name listening-port)
  (let* ((socket (connect-to-epmd))
         (stream (usocket:socket-stream socket)))
    (write-alive2-request stream node-name listening-port)
    (finish-output stream)
    (let ((creation (read-alive2-response stream)))
      (make-instance 'epmd-connection
                     :socket socket
                     :creation creation
                     :node-name node-name
                     :node-port node-port))))

(defun published-p (epmd-connection)
  ;; We try to read from the socket and if we get the END-OF-FILE condition,
  ;; it means that the connection has been closed and we are not published.
  ;; XXX: Not sure if this actually works.
  (handler-case
      (progn
        (usocket:socket-receive (epmd-connection-socket epmd-connection) nil 0)
        t)
    (end-of-file () nil)))

(defun unpublish (epmd-connection)
  (usocket:socket-close (epmd-connection-socket epmd-connection)))


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

(define-condition host-unknown-error (error)
  ;; USOCKET:UNKNOWN-ERROR
  ()
  (:documentation "This error is signaled if the hostname for EPMD is unresolvable."))

(define-condition unreachable-error (error)
  ;; USOCKET:CONNECTION-REFUSED-ERROR
  ()
  (:documentation "This error is signaled when the EPMD is unreachable."))
