(in-package :cleric-handshake-protocol)

(defun write-message (stream message)
  (write-value (type-of message) stream message))

(define-binary-type byte-vector (bytes)
  (:reader (in)
    (let ((vector (make-array bytes :element-type '(unsigned-byte 8))))
      (read-sequence vector in)
      vector))
  (:writer (out value)
    (write-sequence value out)))

;; Described in https://www.erlang.org/doc/apps/erts/erl_dist_protocol.html#distribution-handshake

(define-binary-class handshake-message ()
  ((message-length u2)
   (tag            iso-8859-1-char)))


;;;
;;; Name message
;;
;; 2 bytes: Length of message
;; 1 byte:  'N'
;; 8 bytes: Flags
;; 4 bytes: Creation
;; 2 bytes: Name length
;; N bytes: Full node name
;;

(define-binary-class name (handshake-message)
  ((flags          (unsigned-integer :bytes 8 :bits-per-byte 8))
   (creation       u4)
   (name-length    u2)
   (full-node-name (iso-8859-1-string :length name-length))))

(defun make-name-message (flags creation full-node-name)
  (let ((name-length (length full-node-name)))
    (make-instance 'name
                  :message-length (+ 15 name-length)
                  :tag #\N
                  :flags flags
                  :creation creation
                  :name-length name-length
                  :full-node-name full-node-name)))

(defun read-name-message (stream)
  (read-value 'name stream))


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

(define-binary-class status (handshake-message)
  ((status (iso-8859-1-string :length (- message-length 1)))))

(defun make-status-message (status)
  (make-instance 'status
                 :message-length (1+ (length status))
                 :tag #\s
                 :status status))

(defun read-status-message (stream)
  (read-value 'status stream))


;;;
;;; Challenge message
;;
;; 2 bytes: Length of message
;; 1 byte:  'N'
;; 8 bytes: Flags
;; 4 bytes: Challenge
;; N bytes: Full node name
;;

(define-binary-class challenge (handshake-message)
  ((flags          (unsigned-integer :bytes 8 :bits-per-byte 8))
   (challenge      u4)
   (creation       u4)
   (name-length    u2)
   (full-node-name (iso-8859-1-string :length name-length))))

(defun make-challenge-message (flags challenge creation full-node-name)
  (let ((name-length (length full-node-name)))
    (make-instance 'challenge
                  :message-length (+ 19 name-length)
                  :tag #\N
                  :flags flags
                  :challenge challenge
                  :creation creation
                  :name-length name-length
                  :full-node-name full-node-name)))

(defun read-challenge-message (stream)
  (read-value 'challenge stream))


;;;
;;; Challenge reply message
;;
;; 2 bytes:  Length of message
;; 1 byte:   'r'
;; 4 bytes:  Challenge
;; 16 bytes: Digest
;;

(define-binary-class challenge-reply (handshake-message)
  ((challenge u4)
   (digest    (byte-vector :bytes 16))))

(defun make-challenge-reply-message (challenge digest)
  (make-instance 'challenge-reply
                 :message-length 21
                 :tag #\r
                 :challenge challenge
                 :digest digest))

(defun read-challenge-reply-message (stream)
  (read-value 'challenge-reply stream))


;;;
;;; Challenge ack message
;;
;; 2 bytes:  Length of message
;; 1 byte:   'a'
;; 16 bytes: Digest
;;

(define-binary-class challenge-ack (handshake-message)
  ((digest (byte-vector :bytes 16))))

(defun make-challenge-ack-message (digest)
  (make-instance 'challenge-ack
                 :message-length 17
                 :tag #\a
                 :digest digest))

(defun read-challenge-ack-message (stream)
  (read-value 'challenge-ack stream))
