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

(defun make-name-message (version flags full-node-name)
  (concatenate 'vector
               (uint16-to-bytes (+ 7 (length full-node-name)))
               (vector (char-code #\n))
               (uint16-to-bytes version)
               (uint32-to-bytes flags)
               (string-to-bytes full-node-name)))

(defun read-name-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-byte stream)))
        (if (= tag (char-code #\n))
            (values (read-uint16 stream)
                    (read-uint32 stream)
                    (read-bytes-as-string (- message-length 7) stream))
            (error 'unexpected-message-tag-error
                   :received-tag tag
                   :expected-tags (list (char-code #\n)))))
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

(defun make-status-message (status-string)
  (concatenate 'vector
               (uint16-to-bytes (1+ (length status-string)))
               (vector (char-code #\s))
               (string-to-bytes status-string)))

(defun read-status-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-byte stream)))
        (if (= tag (char-code #\s))
            (read-bytes-as-string (1- message-length) stream)
            (error 'unexpected-message-tag-error
                   :received-tag tag
                   :expected-tags (list (char-code #\s)))))
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

(defun make-challenge-message (version flags challenge full-node-name)
  (concatenate 'vector
               (uint16-to-bytes (+ 11 (length full-node-name)))
               (vector (char-code #\n))
               (uint16-to-bytes version)
               (uint32-to-bytes flags)
               (uint32-to-bytes challenge)
               (string-to-bytes full-node-name)))

(defun read-challenge-message (stream)
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


;;;
;;; Challenge reply message
;;
;; 2 bytes:  Length of message
;; 1 byte:   'r'
;; 4 bytes:  Challenge
;; 16 bytes: Digest
;;

(defun make-challenge-reply-message (challenge digest)
  (concatenate 'vector
               (uint16-to-bytes 21)
               (vector (char-code #\r))
               (uint32-to-bytes challenge)
               digest))

(defun read-challenge-reply-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-byte stream)))
        (cond
          ((/= 21 message-length)
           (error 'unexpected-message-length-error
                  :received-length message-length
                  :expected-length 21))
          ((/= tag (char-code #\r))
           (error 'unexpected-message-tag-error
                  :received-tag tag
                  :expected-tags (list (char-code #\r))))
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

(defun make-challenge-ack-message (digest)
  (concatenate 'vector
               (uint16-to-bytes (1+ (length digest)))
               (vector (char-code #\a))
               digest))

(defun read-challenge-ack-message (stream)
  (handler-case
      (let ((message-length (read-uint16 stream))
            (tag (read-byte stream)))
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
           (read-bytes 16 stream))))
    (end-of-file () (error 'connection-closed-error))))


;;;
;;; Client handshake
;;;

(defun perform-client-handshake (stream cookie)
  (write-sequence (make-name-message +highest-version-supported+
                                     (capability-flags)
                                     *this-node*)
                  stream)
  (finish-output stream)
  (let ((status (read-status-message stream)))
    (cond
      ((or (string= status "ok")
           (string= status "ok_simultaneous"))
       (multiple-value-bind (version flags challenge full-node-name)
           (read-challenge-message stream)
         (let ((new-challenge (generate-challenge))
               (digest (calculate-digest challenge cookie)))
           (write-sequence (make-challenge-reply-message new-challenge digest)
                           stream)
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
       ;; Already connected! (confused)
       ;; What to do?
       (error 'not-implemented-error
              :comment "Already connected. Not sure what to do in this situation."))
      (t
       (error "This should not happen (as usual).")) ) ))


;;;
;;; Server handshake
;;;

(defun perform-server-handshake (stream cookie)
  (multiple-value-bind (version flags full-node-name) (read-name-message stream)
    ;; TODO: Check if node is allowed to connect to us
    (write-sequence (make-status-message "ok") stream)
    (let ((challenge (generate-challenge)))
      (write-sequence (make-challenge-message +highest-version-supported+
                                              (capability-flags)
                                              challenge
                                              *this-node*)
                      stream)
      (finish-output stream)
      (multiple-value-bind (new-challenge digest)
          (read-challenge-reply-message stream)
        (unless (equal-digests (calculate-digest challenge cookie) digest)
          (error 'handshake-failed-error
                 :reason "Received incorrect digest"))
        (write-sequence
         (make-challenge-ack-message (calculate-digest new-challenge cookie))
         stream)
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
