(in-package :cleric)


(defun reg-send (from-pid to-name node message) ;; Merge with SEND in the future
  "Send a message to a registered Pid."
  (let* ((remote-node (find-connected-remote-node node))
	 (stream (usocket:socket-stream (remote-node-socket remote-node))))
    (write-sequence (make-node-message (make-instance 'reg-send
						      :from-pid from-pid
						      :to-name (make-symbol to-name)
						      :message message)
				       :distribution-header t
				       :cache-atoms t)
		    stream)
    (finish-output stream)))

(defun send (to-pid message)
  "Send a message to Pid."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
	 (stream (usocket:socket-stream (remote-node-socket remote-node))))
    (write-sequence (make-node-message (make-instance 'send
						      :to-pid to-pid
						      :message message)
				       :distribution-header t
				       :cache-atoms t)
		    stream)
    (finish-output stream)))

(defun link (from-pid to-pid)
  "Create a link between two Pids."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
	 (stream (usocket:socket-stream (remote-node-socket remote-node))))
    (write-sequence (make-node-message (make-instance 'link
						      :from-pid from-pid
						      :to-pid to-pid)
				       :distribution-header t
				       :cache-atoms t)
		    stream)
    (finish-output stream)))

(defun unlink (from-pid to-pid)
  "Remove a link between two Pids."
  (let* ((remote-node (find-connected-remote-node (node to-pid)))
	 (stream (usocket:socket-stream (remote-node-socket remote-node))))
    (write-sequence (make-node-message (make-instance 'unlink
						      :from-pid from-pid
						      :to-pid to-pid)
				       :distribution-header t
				       :cache-atoms t)
		    stream)
    (finish-output stream)))



;;; FullMessage
;; +--------+--------------------+----------------+---------+
;; |    4   |          D         |        N       |     M   |
;; +--------+--------------------+----------------+---------+
;; | Length | DistributionHeader | ControlMessage | Message |
;; +--------+--------------------+----------------+---------+
;;
;; Where Length = D + N + M
;;

(defun make-node-message (control-message &key (distribution-header nil) (cache-atoms nil))
  (if distribution-header
      (let ((cached-atoms (when cache-atoms (make-atom-cache-entries))))
	(let ((cm (encode-control-message control-message
					  :atom-cache-entries cached-atoms))
	      (dh (make-distribution-header cached-atoms)))
	  (concatenate 'vector
		       (uint32-to-bytes (+ (length dh) (length cm)))
		       dh
		       cm)))
      (let ((cm (encode-control-message control-message :version-tag t)))
	(concatenate 'vector
		     (uint32-to-bytes (1+ (length cm)))
		     (vector +pass-through+)
		     cm))))


(defun receive-node-messages (&key timeout)
  "Waits for and receives messages from connected nodes."
  (when (null *remote-nodes*)
    (warn "Not connected to any nodes.")
    (return-from receive-node-messages nil))
  (multiple-value-bind (ready-sockets time-left)
      (usocket:wait-for-input
       (mapcar #'remote-node-socket *remote-nodes*)
       :timeout timeout
       :ready-only t)
    (declare (ignore time-left))
    (loop
       for socket in ready-sockets
       ;; Find the REMOTE-NODE object that corresponds to the socket object and
       ;; bind *ATOM-CACHE* to the node's atom cache.
       ;; OR, pass the whole REMOTE-NODE object to READ-NODE-MESSAGE.
       for message = (read-node-message (usocket:socket-stream socket))
       unless (eq message 'tick) collect message)))


(defconstant +tock+ #(0 0 0 0))

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
		     (error 'connection-closed-error))))
	  (use-version-tags t)
	  (*cached-atoms* #())
	  (pos 1))
      (case (aref bytes 0)
	(#.+pass-through+)
	(#.+protocol-version+
	 (multiple-value-bind (cached-atoms pos1)
	     (decode-distribution-header bytes pos)
	   (setf pos pos1)
	   (setf use-version-tags nil)
	   (setf *cached-atoms* cached-atoms)))
	(otherwise
	 (error 'unexpected-message-tag-error
		:received-tag (aref bytes 0)
		:expected-tags (list +pass-through+ +protocol-version+))))
      (decode-control-message bytes
			      :start pos
			      :version-tag use-version-tags) )))
