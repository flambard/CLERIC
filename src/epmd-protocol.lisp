;;;; The EPMD protocol

(in-package :cleric-epmd-protocol)

;;; EPMD message tags
(defconstant +port2-resp+       #\w)
(defconstant +alive2-req+       #\x)
(defconstant +alive2-resp+      #\y)
(defconstant +port-please2-req+ #\z)
(defconstant +names-req+        #\n)

;; Node type tags
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +node-type-hidden+ 72)
  (defconstant +node-type-erlang+ 77)
  )

(defconstant +protocol-tcpip4+ 0)


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
    (write-char +alive2-req+ stream)
    (write-uint16 port stream)
    (write-byte +node-type-hidden+ stream)
    (write-byte +protocol-tcpip4+ stream)
    (write-uint16 +lowest-version-supported+ stream)
    (write-uint16 +highest-version-supported+ stream)
    (write-uint16 node-name-length stream)
    (write-string node-name stream)
    (write-uint16 extra-field-length stream)
    (write-sequence (coerce extra '(vector octet)) stream))
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
      (let* ((tag (read-char stream))
             (result (read-byte stream))
             (creation (read-uint16 stream)))
        (cond
          ((char/= tag +alive2-resp+)
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
  (write-uint16 (1+ (length node-name)) stream)
  (write-char +port-please2-req+ stream)
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
      (let ((tag (read-char stream))
            (result (read-byte stream)))
        (cond
          ((char/= tag +port2-resp+)
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
                  (node-name (make-string node-name-length)))
             (read-sequence node-name stream)
             (let ((extra-field-length (read-uint16 stream))
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
                              :extra-field extra-field) )))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; NAMES_REQ
;;
;; 2 bytes: Total length of following message
;; 1 byte:  'n'            [NAMES_REQ message]
;;

(defun write-names-request (stream)
  (write-uint16 1 stream)
  (write-char +names-req+ stream)
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

