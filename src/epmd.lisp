;;;; Functions for querying EPMD (Erlang Port Mapped Daemon)

(in-package :cleric)

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

(defun make-alive2-request (node-name port &optional (extra #()))
  (let* ((node-name-length (length node-name))
         (extra-field-length (length extra))
         (message-length (+ 13 node-name-length extra-field-length)))
    (concatenate 'vector
                 (uint16-to-bytes message-length)
                 (vector +alive2-req+)
                 (uint16-to-bytes port)
                 (vector +node-type-hidden+
                         +protocol-tcpip4+
                         +lowest-version-supported+
                         +highest-version-supported+)
                 (uint16-to-bytes node-name-length)
                 (string-to-bytes node-name)
                 (uint16-to-bytes extra-field-length)
                 extra)))


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
           (error 'epmd-response-error))
          (t
           creation)))
    (end-of-file () (error 'connection-closed-error))))


(defun epmd-publish (port &optional (node-name "lispnode"))
  ;; TODO: Check if we are already registered
  (let* ((socket (handler-case
                     (usocket:socket-connect "localhost" +epmd-port+
                                             :element-type '(unsigned-byte 8))
                   (usocket:connection-refused-error ()
                     (error 'epmd-unreachable-error))))
         (epmd (usocket:socket-stream socket)))
    (write-sequence (make-alive2-request node-name port) epmd)
    (finish-output epmd)
    (let ((creation (read-alive2-response epmd)))
      (declare (ignore creation))
      (setf *epmd-socket* socket)
      t)))

(defun epmd-unpublish ()
  (when *epmd-socket*
    (usocket:socket-close *epmd-socket*)
    t))

(defmacro with-epmd-connection-stream ((stream-var &optional (host "localhost") (port +epmd-port+)) &body body)
  "Create a local scope where STREAM-VAR is a socket stream connected to the EPMD."
  (let ((socket-var (gensym)))
    `(let* ((,socket-var (handler-case (usocket:socket-connect
                                        ,host
                                        ,port
                                        :element-type '(unsigned-byte 8))
                           (usocket:connection-refused-error ()
                             (error 'epmd-unreachable-error))
                           (usocket:unknown-error ()
                             (error 'epmd-host-unknown-error))))
            (,stream-var (usocket:socket-stream ,socket-var)))
       (unwind-protect (progn ,@body)
         (usocket:socket-close ,socket-var))) ))

(defun node-name (node-string)
  "Return the name part of a node identifier"
  ;; All characters up to a #\@ is the name
  (let ((pos (position #\@ node-string)))
    (if pos
        (subseq node-string 0 pos)
        node-string)))

(defun node-host (node-string)
  "Return the host part of a node identifier"
  ;; All characters after a #\@ is the host
  (let ((pos (position #\@ node-string)))
    (if pos
        (subseq node-string (1+ pos))
        "localhost"))) ;; OK with localhost??

(defun epmd-lookup-node (node-name &optional (host "localhost"))
  "Query the EPMD about a node. Returns a REMOTE-NODE object that represents the node."
  (with-epmd-connection-stream (epmd host)
    ;;;; PORT_PLEASE2_REQ ;;;;
    ;; 2 bytes: Total length of following message
    ;; 1 byte:  'z'            [PORT_PLEASE2_REQ message]
    ;; N bytes: Node name
    (let* ((message-length (1+ (length node-name)))
           (port-please2-req (concatenate 'vector
                                          (uint16-to-bytes message-length)
                                          (vector +port-please2-req+)
                                          (string-to-bytes node-name))))
      (write-sequence port-please2-req epmd)
      (finish-output epmd))
    ;;;; PORT2_RESP ;;;;
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
    (handler-case
        (let ((tag (read-byte epmd))
              (result (read-byte epmd)))
          (cond
            ((/= tag +port2-resp+)
             (error 'unexpected-message-tag-error
                    :tag tag
                    :expected-tags (list +port2-resp+)))
            ((/= 0 result)
             nil) ;; No nodes with that name.
            (t
             (let* ((port2-response (read-bytes 10 epmd))
                    (node-name-length (bytes-to-uint16 (subseq port2-response 8)))
                    (node-name (bytes-to-string (read-bytes node-name-length epmd)))
                    (extra-field-length (bytes-to-uint16 (read-bytes 2 epmd)))
                    (extra-field (read-bytes extra-field-length epmd)))
               (make-instance 'remote-node
                              :port (bytes-to-uint16 port2-response)
                              :node-type (case (aref port2-response 2)
                                           (#.+node-type-hidden+ 'hidden)
                                           (#.+node-type-erlang+ 'erlang)
                                           (otherwise
                                            (error 'malformed-message-error
                                                   :bytes port2-response)))
                              :protocol (aref port2-response 3)
                              :lowest-version (bytes-to-uint16 (subseq port2-response 4 6))
                              :highest-version (bytes-to-uint16 (subseq port2-response 6 8))
                              :name node-name
                              :host host
                              :extra-field extra-field) ))))
      (end-of-file () (error 'connection-closed-error))) ))
