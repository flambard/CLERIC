(in-package :cleric)

(defclass erlang-object ()
  ())

(defmethod make-instance :around ((class (eql 'erlang-object)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))


(defclass erlang-binary (erlang-object)
  ((bytes :reader bytes
	  :initarg :bytes
	  :documentation "Returns a vector of bytes from an Erlang binary.")
   (bits :reader bits-in-last-byte
	 :initarg :bits
	 :initform 8
	 :documentation
	 "The number of bits in the last byte of an Erlang binary."))
  (:documentation "Erlang binary."))

(defmethod print-object ((object erlang-binary) stream)
  (print-unreadable-object (object stream :type t)
    (if (= 8 (bits-in-last-byte object))
	(format stream "<~{~s~^ ~}>" (coerce (bytes object) 'list))
	(format stream "<~{~s~^ ~}:~a>" (coerce (bytes object) 'list)
		(bits-in-last-byte object)))))

(defun binary (&rest bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(defun string-to-binary (string)
  "Creates an Erlang binary from the characters in STRING."
  (make-instance 'erlang-binary :bytes (string-to-bytes string)))

(defun bytes-to-binary (bytes)
  "Creates an Erlang binary from BYTES."
  (assert (every #'(lambda (b) (typep b '(unsigned-byte 8))) bytes))
  (make-instance 'erlang-binary :bytes (coerce bytes 'vector)))

(defun binary-to-string (binary)
  "Translates the bytes in BINARY to an ASCII string."
  (bytes-to-string (bytes binary)))

(defmethod size ((x erlang-binary))
  "The byte-size of Erlang binary X."
  (length (bytes x)))


(defclass erlang-fun (erlang-object)
  ((module :reader module :initarg :module)
   (arity :reader arity :initarg :arity :documentation "The arity of an Erlang Fun."))
  (:documentation "Erlang fun."))

(defmethod make-instance :around ((class (eql 'erlang-fun)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))

;;; fun M:F/A
(defclass erlang-external-fun (erlang-fun)
  ((function :initarg :function))
  (:documentation "Erlang fun in external format (module:function/arity)."))

(defmethod print-object ((object erlang-external-fun) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (module arity function) object
      (format stream "~a:~a/~a" module function arity))))

;;; fun F/A or fun (...) -> ...
(defclass erlang-internal-fun (erlang-fun)
  ((pid :initarg :pid)
   (index :initarg :index)
   (uniq :initarg :uniq)
   (free-vars :reader free-vars :initarg :free-vars))
  (:documentation "Erlang fun in internal format."))

(defclass erlang-new-internal-fun (erlang-internal-fun)
  ((index :initarg :old-index)
   (uniq :initarg :old-uniq)
   (new-uniq :initarg :uniq)
   (new-index :initarg :index))
  (:documentation "Erlang fun in new internal format."))


(defclass erlang-identifier (erlang-object)
  ((node :reader node :initarg :node)
   (id :initarg :id)
   (creation :initarg :creation)))

(defmethod make-instance :around ((class (eql 'erlang-identifier)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))

(defclass erlang-pid (erlang-identifier)
  ((serial :initarg :serial))
  (:documentation "Erlang PID."))

(defmethod print-object ((object erlang-pid) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id serial) object
      (format stream "~a <~a.~a>"
	      node (bytes-to-uint32 id) (bytes-to-uint32 serial)))))

;; Pids in Erlang are printed like this <X.id.serial>
;; where X = some number representing the node.

(defun make-pid ()
  "Create a new Erlang PID."
  (make-instance 'erlang-pid
		 :node (make-symbol *this-node*)
		 :id (generate-new-pid-id)
		 :serial #(0 0 0 0) ;; What to set here?
		 :creation 1)) ;; What to set here?

(defun generate-new-pid-id ()
  (uint32-to-bytes (incf *pid-id-counter*)))

;; Not mentioned in the documentation: Serial only uses the least significant 13 bits!

(defclass erlang-port (erlang-identifier)
  ()
  (:documentation "Erlang port."))

(defmethod print-object ((object erlang-port) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id) object
      (format stream "~a <~a>" node (bytes-to-uint32 id)))))


(defclass erlang-reference (erlang-identifier)
  ()
  (:documentation "Erlang ref."))

(defmethod print-object ((object erlang-reference) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (node id) object
      (format stream "~a <~{~a~^.~}>" node
	      (nreverse (mapcar #'bytes-to-uint32 (four-byte-blocks id)))))))

(defun four-byte-blocks (bytes)
  (loop
     repeat (/ (length bytes) 4)
     for pos from 0 by 4
     collect (subseq bytes pos (+ 4 pos))))


(defclass erlang-tuple (erlang-object)
  ((elements :reader elements :initarg :elements))
  (:documentation "Erlang tuple."))

(defmethod print-object ((object erlang-tuple) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "{~{~s~^ ~}}" (coerce (elements object) 'list))))

(defun tuple (&rest erlang-translatable-objects)
  "Create an Erlang tuple"
  (make-instance 'erlang-tuple
		 :elements (coerce erlang-translatable-objects 'vector)))

(defmethod arity ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (length (elements x)))

(defmethod size ((x erlang-tuple))
  "The number of elements of Erlang tuple X."
  (arity x))
