;;;; Handshake between nodes

(in-package :cleric)

;; Erlang node capabilities: #x7FFD (all except DFLAG_ATOM_CACHE)

(defun capability-flags ()
  ;; According to distribution_handshake.txt, hidden nodes should
  ;; only set the flag EXTENDED_REFERENCES.
  ;; That does not work for me.
  (logior +dflag-extended-references+
	  +dflag-extended-pids-ports+
	  +dflag-new-floats+
	  +dflag-dist-hdr-atom-cache+
	  +dflag-small-atom-tags+))

(defun generate-challenge ()
  "Generate a random 32-bit unsigned integer."
  (random (expt 2 32)))

(defun make-name-message (version full-node-name)
  ;;;; Name message
  ;; 2 bytes: Length of message
  ;; 1 byte:  'n'
  ;; 2 bytes: Version
  ;; 4 bytes: Flags
  ;; N bytes: Full node name
  (concatenate 'vector
	       (uint16-to-bytes (+ 7 (length full-node-name)))
	       (vector (char-code #\n))
	       (uint16-to-bytes version)
	       (uint32-to-bytes (capability-flags))
	       (string-to-bytes full-node-name)))

(defun read-status-message (stream)
  ;;;; Status message
  ;; 2 bytes: Length of message
  ;; 1 byte:  's'
  ;; N bytes: Status (string)
  ;;          "ok"
  ;;          "ok_simultaneous"
  ;;          "nok"
  ;;          "not_allowed"
  ;;          "alive"
  (handler-case
      (let ((message-length (read-uint16 stream))
	    (tag (read-byte stream)))
	(if (= tag (char-code #\s))
	    (read-bytes-as-string (1- message-length) stream)
	    (error 'unexpected-message-tag-error
		   :received-tag tag
		   :expected-tags (list (char-code #\s)))))
    (end-of-file () (error 'connection-closed-error))))

(defun read-challenge-message (stream)
  ;;;; Challenge message
  ;; 2 bytes: Length of message
  ;; 1 byte:  'n'
  ;; 2 bytes: Version
  ;; 4 bytes: Flags
  ;; 4 bytes: Challenge
  ;; N bytes: Full node name
  (handler-case
      (let ((message-length (read-uint16 stream))
	    (tag (read-byte stream)))
	(if (= tag (char-code #\n))
	    (values (read-uint16 stream)
		    (read-uint32 stream)
		    (read-uint32 stream)
		    (read-bytes-as-string (- message-length 11) stream))
	    (error 'unexpected-message-tag-error
		   :received-tag tag
		   :expected-tags (list (char-code #\n)))))
    (end-of-file () (error 'connection-closed-error))))
    

(defun make-challenge-reply-message (challenge digest)
  ;;;; Challenge reply message
  ;; 2 bytes:  Length of message
  ;; 1 byte:   'r'
  ;; 4 bytes:  Challenge
  ;; 16 bytes: Digest
  (concatenate 'vector
	       (uint16-to-bytes 21)
	       (vector (char-code #\r))
	       (uint32-to-bytes challenge)
	       digest))

(defun read-challenge-ack-message (stream)
  ;;;; Challenge ack message
  ;; 2 bytes:  Length of message
  ;; 1 byte:   'a'
  ;; 16 bytes: Digest
  (handler-case
      (let ((message-length (read-uint16 stream))
	    (tag (read-byte stream))
	    (digest (read-bytes 16 stream)))
	(cond
	  ((/= 17 message-length)
	   (error 'unexpected-message-length-error
		  :received-length message-length
		  :expected-length 17))
	  ((/= tag (char-code #\a))
	   (error 'unexpected-message-tag-error
		  :received-tag tag
		  :expected-tags (list (char-code #\a))))
	  (t
	   digest)))
    (end-of-file () (error 'connection-closed-error))))


(defun node-connect (host port cookie)
  (let* ((socket (handler-case (usocket:socket-connect
				host port :element-type '(unsigned-byte 8))
		   (usocket:connection-refused-error ()
		     (error 'node-unreachable-error))))
	 (stream (usocket:socket-stream socket)))
    (write-sequence (make-name-message +highest-version-supported+ *this-node*)
		    stream)
    (finish-output stream)
    (let ((status (read-status-message stream)))
      (cond
	((or (string= status "ok")
	     (string= status "ok_simultaneous"))
	 (multiple-value-bind (version flags challenge full-node-name)
	     (read-challenge-message stream)
	   ;; (declare (ignore version flags full-node-name))
	   (format t "FULL-NODE-NAME: ~a, VERSION: ~a, FLAGS: ~b~&"
		   full-node-name version flags)
	   (let ((new-challenge (generate-challenge))
		 (digest (calculate-digest challenge cookie)))
	     (write-sequence (make-challenge-reply-message new-challenge digest)
			     stream)
	     (finish-output stream)
	     (unless (equal-digests (calculate-digest new-challenge cookie)
				    (read-challenge-ack-message stream))
	       (usocket:socket-close socket)
	       ;; Error and close socket!
	       (error 'handshake-failed-error
		      :reason "Received incorrect digest"))
	     socket))) ;; Connect successful
	((string= status "nok")
	 (usocket:socket-close socket)
	 (signal 'try-again
		 :reason "Busy with other ongoing handshake"))
	((string= status "not_allowed")
	 (usocket:socket-close socket)
	 (error 'handshake-failed-error
		:reason "Connection not allowed"))
	((string= status "alive")
	 ;; Already connected! (confused)
	 ;; What to do?
	 ;; Close socket?
	 (error 'not-implemented-error
		:comment "Already connected. Not sure what to do in this situation."))
	(t
	 (usocket:socket-close socket)
	 (error "This should not happen (as usual).")) ) )))


(defun equal-digests (d1 d2)
  (every #'= d1 d2))
