(in-package :cleric)

(defclass erlang-object ()
  ())

(defmethod make-instance :around ((class (eql 'erlang-object)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))


(defclass erlang-fun (erlang-object)
  ((module :reader module :initarg :module)
   (arity :reader arity :initarg :arity :documentation "The arity of an Erlang Fun."))
  (:documentation "Erlang fun."))

(defmethod make-instance :around ((class (eql 'erlang-fun)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))


(defclass erlang-identifier (erlang-object)
  ((node :reader node :initarg :node)
   (id :initarg :id)
   (creation :initarg :creation)))

(defmethod make-instance :around ((class (eql 'erlang-identifier)) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (error "Not possible to make an instance of class ~s" class))

(defmethod match-p ((a erlang-identifier) (b erlang-identifier))
  (and (eq (node a) (node b))
       (every #'= (slot-value a 'id) (slot-value b 'id))
       (= (slot-value a 'creation) (slot-value b 'creation))))
