;;;; Functions for querying EPMD (Erlang Port Mapped Daemon)

(in-package :cleric-epmd)

;;; EPMD port
(defconstant +epmd-port+ 4369
  "The default TCP port the EPMD listens on.")

;;; EPMD message tags
(defconstant +port2-resp+       (char-code #\w))
(defconstant +alive2-req+       (char-code #\x))
(defconstant +alive2-resp+      (char-code #\y))
(defconstant +port-please2-req+ (char-code #\z))
(defconstant +names-req+        (char-code #\n))

;; Node type tags
(defconstant +node-type-hidden+ 72)
(defconstant +node-type-erlang+ 77)

(defconstant +protocol-tcpip4+ 0)

(defvar *epmd-socket* nil
  "The EPMD socket. NIL if not registered in EPMD.")


;;;
;;; ALIVE2_REQ
;;
;; 2 bytes: Total length of following message in bytes
;; 1 byte:  'x'               [ALIVE2_REQ message]
;; 2 bytes: Listening port
;; 1 byte:  72                [hidden node (not Erlang node)]
;; 1 byte:  0                 [protocol: tcp/ip v4]
;; 2 bytes: 5                 [lowest version supported]
;; 2 bytes: 5                 [highest version supported]
;; 2 bytes: Length of node name
;; N bytes: Node name
;; 2 bytes: Length of the Extra field
;; M bytes: Extra             [???]
;;

(defun write-alive2-request (stream node-name port &optional (extra #()))
  (let* ((node-name-length (length node-name))
         (extra-field-length (length extra))
         (message-length (+ 13 node-name-length extra-field-length)))
    (write-uint16 message-length stream)
    (write-byte +alive2-req+ stream)
    (write-uint16 port stream)
    (write-byte +node-type-hidden+ stream)
    (write-byte +protocol-tcpip4+ stream)
    (write-uint16 +lowest-version-supported+ stream)
    (write-uint16 +highest-version-supported+ stream)
    (write-uint16 node-name-length stream)
    (write-string node-name stream)
    (write-uint16 extra-field-length stream)
    (write-sequence (coerce '(vector octet) extra) stream))
  t)


;;;
;;; ALIVE2_RESP
;;
;; 1 byte:  'y'               [ALIVE2_RESP message]
;; 1 byte:  Result            [0 means OK, >0 means ERROR]
;; 2 bytes: Creation          [?]
;;

(defun read-alive2-response (stream)
  (handler-case
      (let* ((tag (read-byte stream))
             (result (read-byte stream))
             (creation (read-uint16 stream)))
        (cond
          ((/= tag +alive2-resp+)
           (error 'unexpected-message-tag-error
                  :tag tag
                  :expected-tags (list +alive2-resp+)))
          ((/= 0 result)
           (error 'response-error))
          (t
           creation)))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; PORT_PLEASE2_REQ
;;
;; 2 bytes: Total length of following message
;; 1 byte:  'z'            [PORT_PLEASE2_REQ message]
;; N bytes: Node name
;;

(defun write-port-please2-request (stream node-name)
  (write-sequence (uint16-to-bytes (1+ (length node-name))) stream)
  (write-byte +port-please2-req+ stream)
  (write-string node-name stream)
  t)


;;;
;;; PORT_PLEASE2_RESP
;;
;; 1 byte:  'w'            [PORT2_RESP message]
;; 1 byte:  Result         [0 means OK, >0 means ERROR]
;;; Continued only if result = 0
;; 2 bytes: Port
;; 1 byte:  Node type      [77 means Erlang node, 72 means hidden node]
;; 1 byte:  Protocol       [0 means TCP/IP v4]
;; 2 bytes: Lowest version supported
;; 2 bytes: Highest version supported
;; 2 bytes: Node name length
;; N bytes: Node name
;; 2 bytes: Extra field length
;; M bytes: Extra field
;;

(defun read-port-please2-response (stream host)
  (handler-case
      (let ((tag (read-byte stream))
            (result (read-byte stream)))
        (cond
          ((/= tag +port2-resp+)
           (error 'unexpected-message-tag-error
                  :tag tag
                  :expected-tags (list +port2-resp+)))
          ((/= 0 result)
           nil) ;; No nodes with that name.
          (t
           (let* ((port (read-uint16 stream))
                  (node-type (read-byte stream))
                  (protocol (read-byte stream))
                  (lowest-version-supported (read-uint16 stream))
                  (highest-version-supported (read-uint16 stream))
                  (node-name-length (read-uint16 stream))
                  (node-name (read-string node-name-length stream))
                  (extra-field-length (read-uint16 stream))
                  (extra-field (read-bytes extra-field-length stream)))
             (make-instance 'remote-node
                            :port port
                            :node-type (case node-type
                                         (#.+node-type-hidden+ 'hidden)
                                         (#.+node-type-erlang+ 'erlang)
                                         (otherwise
                                          (error 'malformed-message-error)))
                            :protocol protocol
                            :lowest-version lowest-version-supported
                            :highest-version highest-version-supported
                            :name node-name
                            :host host
                            :extra-field extra-field) ))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; NAMES_REQ
;;
;; 2 bytes: Total length of following message
;; 1 byte:  'n'            [NAMES_REQ message]
;;

(defun make-names-request ()
  (concatenate '(vector octet)
               (uint16-to-bytes 1)
               (vector +names-req+)))

(defun write-names-request (stream)
  (write-sequence (make-names-request) stream)
  t)


;;;
;;; NAMES_RESP
;;
;; 4 bytes: EPMDPortNo     Why do we get this?
;; N bytes: NodeInfo
;;

(defun read-names-response (stream)
  (values (handler-case (read-uint32 stream)
            (end-of-file () (error 'connection-closed-error)))
          (loop
             for line = (read-line stream nil)
             while line collect line)))


;;;
;;; EPMD API
;;;

(defun publish ()
  (if *epmd-socket*
      (error 'already-registered)
      (restart-case
          (if (not (listening-p))
              (error 'not-listening-on-socket)
              (let* ((socket (handler-case (connect-to-epmd)
                               (usocket:connection-refused-error ()
                                 (error 'unreachable-error))))
                     (epmd (usocket:socket-stream socket)))
                (write-alive2-request
                 epmd (node-name (this-node)) (listening-port))
                (finish-output epmd)
                (let ((creation (read-alive2-response epmd)))
                  (declare (ignore creation))
                  (setf *epmd-socket* socket)
                  t)))
        (start-listening-on-socket ()
          :report "Start listening on a socket."
          :test (lambda (c)
                  (declare (ignore c))
                  (not (listening-p)))
          (start-listening)
          (publish)))))

(defun published-p ()
  (not (null *epmd-socket*)))

(defun unpublish ()
  (when *epmd-socket*
    (usocket:socket-close *epmd-socket*)
    (setf *epmd-socket* nil)
    t))

(defmacro with-epmd-connection-stream
    ((stream-var &optional (host "localhost")) &body body)
  "Create a local scope where STREAM-VAR is a socket stream connected to the EPMD."
  (let ((socket-var (gensym)))
    `(let* ((,socket-var (handler-case (connect-to-epmd ,host)
                           (usocket:connection-refused-error ()
                             (error 'unreachable-error))
                           (usocket:unknown-error ()
                             (error 'host-unknown-error))))
            (,stream-var (usocket:socket-stream ,socket-var)))
       (unwind-protect (progn ,@body)
         (usocket:socket-close ,socket-var))) ))

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
;;; Helper functions
;;;

(defun connect-to-epmd (&optional (host "localhost"))
  (let ((socket (usocket:socket-connect host +epmd-port+ :element-type 'octet)))
    (setf (usocket:socket-stream socket)
          (make-flexi-stream (usocket:socket-stream socket) :element-type 'octet))
    socket))


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
