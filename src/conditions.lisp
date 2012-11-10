(in-package :cleric)

(define-condition not-implemented-error (program-error)
  ((comment :reader comment :initarg :comment))
  (:documentation "The signaling function is not implemented yet."))

(define-condition try-again ()
  ((reason :reader reason :initarg :reason))
  (:documentation "This condition is signaled when trying to connect to a remote node that is busy."))

(defun try-again-condition-p (condition)
  (typep condition 'try-again))

(defun try-connect-again-restart (condition)
  (declare (ignore condition))
  (invoke-restart 'try-connect-again))

(define-condition handshake-failed-error (error)
  ((reason :reader reason :initarg :reason))
  (:documentation "This error is signaled if the handshake during connection to a remote node fails."))

(define-condition connection-closed-error (error)
  ;; END-OF-FILE
  ()
  (:documentation "This error is signaled when trying to read from a socket stream that has been closed."))

(define-condition node-unreachable-error (error)
  ;; USOCKET:CONNECTION-REFUSED-ERROR
  ()
  (:documentation "This error is signaled when trying to connect to a node that is unreachable."))

(define-condition malformed-message-error (error)
  ((bytes :reader bytes :initarg :bytes))
  (:documentation "This error is signaled when a protocol message is malformed."))

(define-condition malformed-external-erlang-term-error (error)
  ((bytes :reader bytes :initarg :bytes))
  (:documentation "This error is signaled when an encoded Erlang term is malformed."))

(define-condition untranslatable-lisp-object-error (error)
  ((object :reader object :initarg :object))
  (:documentation "This error is signaled when trying to encode an unencodable object."))

(define-condition unexpected-message-length-error (malformed-message-error)
  ((received-length :reader received-length :initarg :received-length)
   (expected-length :reader expected-length :initarg :expected-length))
  (:documentation "This error is signaled when the specified length of a message is not the expected length."))

(define-condition unexpected-message-tag-error (malformed-message-error)
  ((received-tag :reader received-tag :initarg :received-tag)
   (expected-tags :reader expected-tags :initarg :expected-tags))
  (:documentation "This error is signaled when an unexpected message tag is read."))
