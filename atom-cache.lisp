;;;; Atom cache

(in-package :cleric)

(defstruct (atom-cache (:constructor make-atom-cache ())
		       (:print-object print-atom-cache))
  "Erlang atom cache"
  (segments (make-array '(8 256) :element-type 'symbol :initial-element nil)
	    :type '(array symbol (8 256))
	    :read-only t))

(defun print-atom-cache (cache stream)
  (print-unreadable-object (cache stream :type t :identity t)
    (format stream "~a/2048"
	    (loop
	       for index upto 2047
	       counting (row-major-aref (atom-cache-segments cache) index)))))

(defun atom-cache-print-atoms (cache &optional (stream *standard-output*))
  (dotimes (segment 8)
    (loop
       for internal upto 255
       for atom = (aref (atom-cache-segments cache) segment internal)
       if atom do (format stream "~&~d ~3d: ~a" segment internal atom))))

(defun atom-cache-location-of (symbol cache)
  (dotimes (segment 8)
    (loop
       for internal upto 255
       if (eq symbol (aref (atom-cache-segments cache) segment internal))
       do (return-from atom-cache-location-of (cons segment internal)))))

(defun atom-cache-find-free (cache)
  (dotimes (segment 8)
    (loop
       for internal upto 255
       if (null (aref (atom-cache-segments cache) segment internal))
       do (return-from atom-cache-find-free (cons segment internal)))))

(defun atom-cache-get (cache s-index internal-s-index)
  (aref (atom-cache-segments cache) s-index internal-s-index))

(defun atom-cache-add (symbol cache s-index internal-s-index)
  (setf (aref (atom-cache-segments cache) s-index internal-s-index) symbol))

(defun atom-cache-add-new (symbol cache)
  (let ((location (atom-cache-location-of symbol cache)))
    (if location
	(atom-cache-add symbol cache (car location) (cdr location))
	(let ((free (atom-cache-find-free cache)))
	  (atom-cache-add symbol cache (car free) (cdr free))))))
