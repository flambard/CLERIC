(in-package :cleric)


(defclass remote-node ()
  ((socket :reader remote-node-socket :initarg :socket)
   (atom-cache :reader remote-node-atom-cache :initform (make-atom-cache))
   (port :reader remote-node-port :initarg :port)
   (node-type :initarg :node-type) ;; 'ERLANG or 'HIDDEN
   (protocol :initarg :protocol) ;; 0 (TCP/IP v4)
   (lowest-version :initarg :lowest-version)
   (highest-version :initarg :highest-version)
   (name :reader remote-node-name :initarg :name :documentation "The name of the remote node.")
   (host :reader remote-node-host :initarg :host)
   (full-name :initarg :full-name :initform nil)
   (extra-field :initarg :extra-field :initform #())
   (group-leader :initarg :group-leader :initform '|init|))
  (:documentation "A representation of a remote node."))

(defmethod print-object ((object remote-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (port node-type name host) object
      (format stream "(~a) ~a@~a [~a]" node-type name host port))))

(defmethod socket-stream ((node remote-node))
  (usocket:socket-stream (remote-node-socket node)))

(defun remote-node-connect (remote-node cookie)
  "Connect and perform handshake with a remote node."
  (let ((socket
         (handler-case
             (usocket:socket-connect host port :element-type '(unsigned-byte 8))
           (usocket:connection-refused-error ()
             (error 'node-unreachable-error)) )))
    (restart-case
        (handler-bind ((condition #'(lambda (c)
                                      (declare (ignore c))
                                      (usocket:socket-close socket) )))
            (when (perform-client-handshake (usocket:socket-stream socket)
                                            cookie)
              (setf (slot-value remote-node 'socket) socket)
              (push remote-node *remote-nodes*)
              t))
      (try-connect-again ()
        (remote-node-connect remote-node cookie))) ))

(defun listen-for-remote-node ()
  (usocket:socket-listen usocket:*wildcard-host*
                         usocket:*auto-port*
                         :element-type '(unsigned-byte 8)))

(defun remote-node-accept-connect (listening-socket)
  (node-accept-connect listening-socket))

(defun find-connected-remote-node (node-name) ;; Make NODE-NAME a node designator
  (find node-name *remote-nodes* :key #'remote-node-name :test #'string=)) ;; Perhaps also check full name?
