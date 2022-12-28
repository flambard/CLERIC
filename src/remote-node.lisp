(in-package :cleric)

(defvar *remote-nodes* (list)
  "Remote nodes connected to.")


(defclass remote-node ()
  ((socket :reader remote-node-socket :initarg :socket)
   (atom-cache :reader remote-node-atom-cache :initform (make-atom-cache))
   (port :reader remote-node-port :initarg :port)
   (node-type :initarg :node-type) ;; 'ERLANG or 'HIDDEN
   (protocol :initarg :protocol :initform 0) ;; 0 (TCP/IP v4)
   (highest-version :initarg :highest-version)
   (lowest-version :initarg :lowest-version)
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


(define-condition node-unreachable-error (error)
  ;; USOCKET:CONNECTION-REFUSED-ERROR
  ()
  (:documentation "This error is signaled when trying to connect to a node that is unreachable."))

(defun try-connect-again-restart (condition)
  (declare (ignore condition))
  (invoke-restart 'try-connect-again))

(defun make-remote-node (node-info)
  (make-instance 'remote-node
                 :port (cleric-epmd:node-port node-info)
                 :node-type (cleric-epmd:node-type node-info)
                 :protocol (cleric-epmd:node-protocol node-info)
                 :highest-version (cleric-epmd:node-highest-version node-info)
                 :lowest-version (cleric-epmd:node-lowest-version node-info)
                 :name (cleric-epmd:node-name node-info)
                 :host (cleric-epmd:node-host node-info)
                 :extra-field (cleric-epmd:node-extra-field node-info)))

(defun remote-node-connect (node-info cookie)
  "Connect and perform handshake with a remote node."
  (let* ((remote-node (make-remote-node node-info))
         (socket
          (handler-case
              (usocket:socket-connect (remote-node-host remote-node)
                                      (remote-node-port remote-node)
                                      :element-type '(unsigned-byte 8))
            (usocket:connection-refused-error ()
              (error 'node-unreachable-error)) )))
    (restart-case
        (handler-bind ((condition #'(lambda (condition)
                                      (declare (ignore condition))
                                      (usocket:socket-close socket))))
          (multiple-value-bind (full-node-name flags)
              (perform-client-handshake (usocket:socket-stream socket) cookie)
            (declare (ignore full-node-name flags))
            (setf (slot-value remote-node 'socket) socket)
            (register-connected-remote-node remote-node)))
      (try-connect-again ()
        :test try-again-condition-p
        (remote-node-connect node-info cookie))) ))

(defun remote-node-accept-connect (cookie)
  (let ((socket (restart-case (accept-connect)
                  (start-listening-on-socket ()
                    :report "Start listening on a socket."
                    (start-listening)
                    (accept-connect)) )))
    (handler-bind ((condition #'(lambda (condition)
                                  (declare (ignore condition))
                                  (usocket:socket-close socket))))
      (multiple-value-bind (full-node-name flags version)
          (perform-server-handshake (usocket:socket-stream socket) cookie)
        (declare (ignore flags))
        (register-connected-remote-node
         (make-instance 'remote-node
                        :socket socket
                        :node-type 'erlang ;; Can we get this information from flags?
                        :lowest-version version
                        :highest-version version
                        :name (node-name full-node-name)
                        :host (node-name full-node-name)
                        :full-name full-node-name))
        full-node-name))))


(defun register-connected-remote-node (remote-node)
  (push remote-node *remote-nodes*)
  t)

(defun find-connected-remote-node (node-name) ;; Make NODE-NAME a node designator
  (find node-name *remote-nodes* :key #'remote-node-name :test #'string=)) ;; Perhaps also check full name?

(defun remote-node-sockets ()
  (mapcar #'remote-node-socket *remote-nodes*))

(defun connected-remote-nodes ()
  (mapcar #'remote-node-name *remote-nodes*))
