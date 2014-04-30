(in-package :cleric)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pass-through+ 112)
  )

(alexandria:define-constant +tock+
    (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
  :test #'equalp)


;;;
;;; Sending Control Messages
;;;

(defun reg-send (from-pid to-name node message) ;; Merge with SEND in the future
  "Send a message to a registered Pid."
  (let* ((remote-node (find-connected-remote-node node))
         (stream (socket-stream remote-node)))
    (write-node-message (make-instance 'reg-send
                                       :from-pid from-pid
                                       :to-name (make-symbol to-name)
                                       :message message)
                        stream
                        :distribution-header t
                        :cache-atoms t)
    (finish-output stream)))

(defun send (to-pid message)
  "Send a message to Pid."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
         (stream (socket-stream remote-node)))
    (write-node-message (make-instance 'send
                                       :to-pid to-pid
                                       :message message)
                        stream
                        :distribution-header t
                        :cache-atoms t)
    (finish-output stream)))

(defun link (from-pid to-pid)
  "Create a link between two Pids."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
         (stream (socket-stream remote-node)))
    (write-node-message (make-instance 'link
                                       :from-pid from-pid
                                       :to-pid to-pid)
                        stream
                        :distribution-header t
                        :cache-atoms t)
    (finish-output stream)))

(defun unlink (from-pid to-pid)
  "Remove a link between two Pids."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
         (stream (socket-stream remote-node)))
    (write-node-message (make-instance 'unlink
                                       :from-pid from-pid
                                       :to-pid to-pid)
                        stream
                        :distribution-header t
                        :cache-atoms t)
    (finish-output stream)))


;;;
;;; Receiving Control Messages
;;;

(defun receive-node-messages (&key timeout)
  "Waits for and receives messages from connected nodes."
  (let ((sockets (remote-node-sockets)))
    (when (null sockets)
      (warn "Not connected to any nodes.")
      (return-from receive-node-messages nil))
    (multiple-value-bind (ready-sockets time-left)
        (usocket:wait-for-input sockets :timeout timeout :ready-only t)
      (declare (ignore time-left))
      (loop
         for socket in ready-sockets
         ;; Find the REMOTE-NODE object that corresponds to the socket object
         ;; and bind *ATOM-CACHE* to the node's atom cache.
         ;; OR, pass the whole REMOTE-NODE object to READ-NODE-MESSAGE.
         for message = (read-node-message (usocket:socket-stream socket))
         unless (eq message 'tick) collect message))))


;;; FullMessage
;; +--------+--------------------+----------------+---------+
;; |    4   |          D         |        N       |     M   |
;; +--------+--------------------+----------------+---------+
;; | Length | DistributionHeader | ControlMessage | Message |
;; +--------+--------------------+----------------+---------+
;;
;; Where Length = D + N + M
;;

(defun write-node-message (control-message stream
                           &key (distribution-header nil) (cache-atoms nil))
  (if distribution-header
      (let ((cache-collector (when cache-atoms
                               (make-atom-cache-collector *atom-cache*))))
        (let ((etf-aci:*atom-cache* cache-collector))
          (let ((cm (encode-control-message control-message))
                (dh (make-distribution-header
                     (cache-references cache-collector))))
            (write-uint32 (+ (length dh) (length cm)) stream)
            (write-sequence dh stream)
            (write-sequence cm stream))))
      (let ((cm (encode-control-message control-message
                                        :version-tag +protocol-version+)))
        (write-uint32 (1+ (length cm)) stream)
        (write-byte +pass-through+ stream)
        (write-sequence cm stream)))
  t)

(defun read-node-message (stream)
  (let ((length (handler-case (read-uint32 stream)
                  (end-of-file ()
                    (error 'connection-closed-error)))))
    (when (= 0 length) ;; Received TICK. Send TOCK.
      (write-sequence +tock+ stream)
      (finish-output stream)
      (return-from read-node-message
        (if (listen stream)
            (read-node-message stream)
            'tick)))
    (let ((bytes (handler-case (read-bytes length stream)
                   (end-of-file ()
                     (error 'connection-closed-error)))))
      (case (aref bytes 0)
        (#.+pass-through+
         (decode-control-message bytes
                                 :start 1
                                 :version-tag +protocol-version+))
        (#.+protocol-version+
         (multiple-value-bind (cache pos) (decode-distribution-header bytes 1)
           (let ((etf-aci:*atom-cache*
                  (make-atom-cache-collector *atom-cache* cache)))
             (decode-control-message bytes :start pos :version-tag nil))))
        (otherwise
         (error 'unexpected-message-tag-error
                :received-tag (aref bytes 0)
                :expected-tags (list +pass-through+ +protocol-version+)))) )))
