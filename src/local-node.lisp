(in-package :cleric)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +lowest-version-supported+ 5
    "Lowest version of the Erlang distribution protocol supported.")

  (defconstant +highest-version-supported+ 5
    "Highest version of the Erlang distribution protocol supported.")
  )

(defvar *this-node* "lispnode@localhost"
  "The name and host for this node.")


(defun this-node ()
  *this-node*)

(defun (setf this-node) (node-name)
  ;; TODO: Add sanity checks.
  ;; The node name should be valid.
  ;; It should not be possible to change node name while connected to other
  ;; nodes and/or registered on the EPMD.
  (setf *this-node* node-name))


;;;
;;; Utility functions
;;;

(defun node-name (node-string)
  "Return the name part of a node identifier"
  ;; All characters up to a #\@ is the name
  (let ((pos (position #\@ node-string)))
    (if pos
        (subseq node-string 0 pos)
        node-string)))

(defun node-host (node-string)
  "Return the host part of a node identifier"
  ;; All characters after a #\@ is the host
  (let ((pos (position #\@ node-string)))
    (if pos
        (subseq node-string (1+ pos))
        "localhost"))) ;; OK with localhost??


;;;
;;; PID functions
;;;

(defvar *pid-id-counter* 0)

(defun generate-new-pid-id ()
  (uint32-to-bytes (incf *pid-id-counter*)))

(defun make-pid ()
  "Create a new Erlang PID."
  (make-instance 'erlang-pid
                 :node (make-symbol (this-node))
                 :id (generate-new-pid-id)
                 ;; What to set on serial and creation?
                 :serial (make-array 4
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents (list 0 0 0 0))
                 :creation 1))


;;;
;;; Ref functions
;;;

(defvar *reference-id-counter* 0)

(defun generate-new-reference-id ()
  (uint32-to-bytes (incf *reference-id-counter*)))

(defun make-reference ()
  "Create a new Erlang reference."
  (make-instance 'erlang-reference
                 :node (make-symbol (this-node))
                 :id (generate-new-reference-id)
                 :creation 1)) ;; What to set here?
