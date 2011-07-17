;;;; Handshake between nodes

(in-package :cleric)

;;;
;;; Name message
;;
;; 2 bytes: Length of message
;; 1 byte:  'n'
;; 2 bytes: Version
;; 4 bytes: Flags
;; N bytes: Full node name
;;

(defun write-name-message (stream version flags full-node-name)
  (write-uint16 (+ 7 (length full-node-name)) stream)
  (write-char #\n stream)
  (write-uint16 version stream)
  (write-uint32 flags stream)
  (write-string full-node-name stream)
  t)

(defun read-name-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-char stream)))
        (if (char= tag #\n)
            (values (read-uint16 stream)
                    (read-uint32 stream)
                    (read-string (- message-length 7) stream))
            (error 'unexpected-message-tag-error
                   :received-tag tag
                   :expected-tags (list #\n))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Status message
;;
;; 2 bytes: Length of message
;; 1 byte:  's'
;; N bytes: Status (string)
;;          "ok"
;;          "ok_simultaneous"
;;          "nok"
;;          "not_allowed"
;;          "alive"
;;

(defun write-status-message (stream status-string)
  (write-uint16 (1+ (length status-string)) stream)
  (write-char #\s stream)
  (write-string status-string stream)
  t)

(defun read-status-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-char stream)))
        (if (char= tag #\s)
            (read-string (1- message-length) stream)
            (error 'unexpected-message-tag-error
                   :received-tag tag
                   :expected-tags (list #\s))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Challenge message
;;
;; 2 bytes: Length of message
;; 1 byte:  'n'
;; 2 bytes: Version
;; 4 bytes: Flags
;; 4 bytes: Challenge
;; N bytes: Full node name
;;

(defun write-challenge-message (stream version flags challenge full-node-name)
  (write-uint16 (+ 11 (length full-node-name)) stream)
  (write-char #\n stream)
  (write-uint16 version stream)
  (write-uint32 flags stream)
  (write-uint32 challenge stream)
  (write-string full-node-name)
  t)

(defun read-challenge-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-char stream)))
        (if (char= tag #\n)
            (values (read-uint16 stream)
                    (read-uint32 stream)
                    (read-uint32 stream)
                    (read-string (- message-length 11) stream))
            (error 'unexpected-message-tag-error
                   :received-tag tag
                   :expected-tags (list #\n))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Challenge reply message
;;
;; 2 bytes:  Length of message
;; 1 byte:   'r'
;; 4 bytes:  Challenge
;; 16 bytes: Digest
;;

(defun write-challenge-reply-message (stream challenge digest)
  (write-uint16 21 stream)
  (write-char #\r stream)
  (write-uint32 challenge stream)
  (write-sequence digest stream)
  t)

(defun read-challenge-reply-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-char stream)))
        (cond
          ((/= 21 message-length)
           (error 'unexpected-message-length-error
                  :received-length message-length
                  :expected-length 21))
          ((char/= tag #\r)
           (error 'unexpected-message-tag-error
                  :received-tag tag
                  :expected-tags (list #\r)))
          (t
           (values (read-uint32 stream)
                   (read-bytes 16 stream)))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Challenge ack message
;;
;; 2 bytes:  Length of message
;; 1 byte:   'a'
;; 16 bytes: Digest
;;

(defun write-challenge-ack-message (stream digest)
  (write-uint16 (1+ (length digest)) stream)
  (write-char #\a stream)
  (write-sequence digest stream)
  t)

(defun read-challenge-ack-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-char stream)))
        (cond
          ((/= 17 message-length)
           (error 'unexpected-message-length-error
                  :received-length message-length
                  :expected-length 17))
          ((char/= tag #\a)
           (error 'unexpected-message-tag-error
                  :received-tag tag
                  :expected-tags (list #\a)))
          (t
           (read-bytes 16 stream))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Client handshake
;;;

(defun perform-client-handshake (stream cookie)
  (write-name-message
   stream +highest-version-supported+ (capability-flags) (this-node))
  (finish-output stream)
  (let ((status (read-status-message stream)))
    (cond
      ((or (string= status "ok")
           (string= status "ok_simultaneous"))
       (multiple-value-bind (version flags challenge full-node-name)
           (read-challenge-message stream)
         (let ((new-challenge (generate-challenge))
               (digest (calculate-digest challenge cookie)))
           (write-challenge-reply-message stream new-challenge digest)
           (finish-output stream)
           (unless (equal-digests (calculate-digest new-challenge cookie)
                                  (read-challenge-ack-message stream))
             (error 'handshake-failed-error
                    :reason "Received incorrect digest"))
           (values full-node-name flags version) ))) ;; Connect successful
      ((string= status "nok")
       (signal 'try-again
               :reason "Busy with other ongoing handshake"))
      ((string= status "not_allowed")
       (error 'handshake-failed-error
              :reason "Connection not allowed"))
      ((string= status "alive")
       (error 'handshake-failed-error
              :reason "Already connected."))
      (t
       (error "This should not happen (as usual).")) ) ))


;;;
;;; Server handshake
;;;

(defun perform-server-handshake (stream cookie)
  (multiple-value-bind (version flags full-node-name) (read-name-message stream)
    ;; TODO: Check if node is allowed to connect to us
    (write-status-message stream "ok")
    (let ((challenge (generate-challenge)))
      (write-challenge-message stream
                               +highest-version-supported+
                               (capability-flags)
                               challenge
                               (this-node))
      (finish-output stream)
      (multiple-value-bind (new-challenge digest)
          (read-challenge-reply-message stream)
        (unless (equal-digests (calculate-digest challenge cookie) digest)
          (error 'handshake-failed-error
                 :reason "Received incorrect digest"))
        (write-challenge-ack-message
         stream (calculate-digest new-challenge cookie))
        (finish-output stream)
        (values full-node-name flags version) )))) ;; Connect successful


;;;
;;; Helper functions
;;;

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

(defun equal-digests (d1 d2)
  (and (alexandria:length= d1 d2)
       (every #'= d1 d2)))
