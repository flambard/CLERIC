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

(define-binary-class handshake-message ()
  ((message-length u2)
   (tag            iso-8859-1-char)))


;;;
;;; Name message
;;
;; 2 bytes: Length of message
;; 1 byte:  'n'
;; 2 bytes: Version
;; 4 bytes: Flags
;; N bytes: Full node name
;;

(define-binary-class name (handshake-message)
  ((version        u2)
   (flags          u4)
   (full-node-name (iso-8859-1-string :length (- message-length 7)))))

(defun make-name-message (version flags full-node-name)
  (make-instance 'name
                 :message-length (+ 7 (length full-node-name))
                 :tag #\n
                 :version version
                 :flags flags
                 :full-node-name full-node-name))

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
;; 1 byte:  'n'
;; 2 bytes: Version
;; 4 bytes: Flags
;; 4 bytes: Challenge
;; N bytes: Full node name
;;

(define-binary-class challenge (handshake-message)
  ((version        u2)
   (flags          u4)
   (challenge      u4)
   (full-node-name (iso-8859-1-string :length (- message-length 11)))))

(defun make-challenge-message (version flags challenge full-node-name)
  (make-instance 'challenge
                 :message-length (+ 11 (length full-node-name))
                 :tag #\n
                 :version version
                 :flags flags
                 :challenge challenge
                 :full-node-name full-node-name))

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
