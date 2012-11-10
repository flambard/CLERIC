(in-package :cleric-etf)

(define-condition not-implemented-error (program-error)
  ((comment :reader comment :initarg :comment))
  (:documentation "The signaling function is not implemented yet."))

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
