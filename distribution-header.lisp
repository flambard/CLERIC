;;;; The distribution header

(in-package :cleric)

;; +-----+----+-----------------------+-------+---------------+
;; |  1  |  1 |            1          |    N  |               |
;; +-----+----+-----------------------+-------+---------------+
;; | 131 | 68 | NumberOfAtomCacheRefs | Flags | AtomCacheRefs |
;; +-----+----+-----------------------+-------+---------------+
;;
;; Where N = NumberOfAtomCacheRefs / 2 + 1 OR 0
;;

;; Flag field
;; +----------+--------------+
;; |   1 bit  |     3 bits   |
;; +----------+--------------+
;; | NewEntry | SegmentIndex |
;; +----------+--------------+
;;

;; (Last half byte after the Flag fields)
;; +-----------------+-----------+
;; |      3 bits     |   1 bit   |
;; +-----------------+-----------+
;; | CurrentlyUnused | LongAtoms |
;; +-----------------+-----------+
;;
;; If LongAtoms = 1, Length in AtomCacheRef takes 2 bytes instead of 1.
;;
;; Question: When there are an even number of AtomCacheRefs, in which part of
;;   the last (whole) byte are these 4 bits located?
;;

;; AtomCacheRef (if NewEntry = 1)
;; +----------------------+--------+----------+
;; |           1          |  1 | 2 |  Length  |
;; +----------------------+--------+----------+
;; | InternalSegmentIndex | Length | AtomText |
;; +----------------------+--------+----------+
;;

;; AtomCacheRef (if NewEntry = 0)
;; +----------------------+
;; |           1          |
;; +----------------------+
;; | InternalSegmentIndex |
;; +----------------------+
;;

(defconstant +distribution-header-tag+ 68)

(defun read-distribution-header (stream)
  ;; Assume +protocol-version+ is read
  (handler-case
      (let ((tag (read-byte stream))
	    (number-of-refs (read-byte stream)))
	(cond
	  ((/= tag +distribution-header-tag+)
	   (error 'unexpected-message-tag-error
		  :received-tag tag
		  :expected-tags +distribution-header-tag+))
	  ((= 0 number-of-refs)
	   (values (vector)))
	  (t
	   ;; Read Flags and pluck the last half byte
	   (let* ((half-bytes (read-flags number-of-refs stream))
		  (indices (subseq half-bytes 0 number-of-refs))
		  (last-half-byte (nth number-of-refs half-bytes))
		  (currently-unused (ldb (byte 3 1) last-half-byte))
		  (use-long-atoms (= 1 (ldb (byte 1 0) last-half-byte))))
	     (declare (ignore currently-unused))
	     (values
	      ;; Read AtomCacheRefs and return a vector of atoms.
	      (map 'vector
		   #'(lambda (flags)
		       (read-atom-cache-ref flags stream use-long-atoms))
		   indices) ) )) ))
    (end-of-file ()
      (error 'connection-closed-error))) )

(defun decode-distribution-header (bytes &optional (pos 0))
  (let ((tag (aref bytes pos))
	(number-of-refs (aref bytes (1+ pos))))
    (cond
      ((/= tag +distribution-header-tag+)
       (error 'unexpected-message-tag-error
	      :received-tag tag
	      :expected-tags +distribution-header-tag+))
      ((= 0 number-of-refs)
       (values (vector)
	       (+ 2 pos)))
      (t
       ;; Read Flags and pluck the last half byte
       (multiple-value-bind (half-bytes pos1)
	   (decode-flags number-of-refs bytes (+ 2 pos))
	 (let* ((last-half-byte (nth number-of-refs half-bytes))
		(currently-unused (ldb (byte 3 1) last-half-byte))
		(use-long-atoms (= 1 (ldb (byte 1 0) last-half-byte))))
	   (declare (ignore currently-unused))
	   ;; Read AtomCacheRefs and return a vector of atoms.
	   (loop
	      for flags in (subseq half-bytes 0 number-of-refs)
	      for (atom p) = (multiple-value-list
			      (decode-atom-cache-ref
			       flags bytes use-long-atoms pos1))
	      do (setf pos1 p)
	      collect atom into atoms
	      finally (return (values (coerce atoms 'vector)
				      pos1))) ))))))


(defun make-distribution-header (&optional (cached-atoms #()))
  (concatenate 'vector
	       (vector +protocol-version+
		       +distribution-header-tag+
		       (length cached-atoms))
	       (encode-atom-cache-refs cached-atoms)))

(defun encode-atom-cache-refs (entries)
  (loop
     with currently-unused = #b000
     with long-atoms = (long-atoms-p entries)
     for (atom new-p index internal) across entries
     collect (logior index (if new-p (ash 1 3) 0)) into half-bytes
     nconc (if new-p
	       (let ((atom-string (symbol-name atom)))
		 `(,internal
		   ,@(if long-atoms
			 (coerce (uint16-to-bytes (length atom-string)) 'list)
			 (list (length atom-string)))
		   ,@(map 'list #'char-code atom-string)))
	       `(,internal)) into refs
     finally
       (nconc half-bytes
	      (list (logior (if long-atoms 1 0)
			    (ash currently-unused 1))))
       (return (concatenate 'vector (encode-flags half-bytes) refs)) ))

(defun encode-flags (half-bytes)
  (loop
     for least-significant = (pop half-bytes)
     for most-significant  = (or (pop half-bytes) 0)
     while least-significant
     collect (logior least-significant (ash most-significant 4)) ))


(defun long-atoms-p (entries)
  (loop
     for (atom new . nil) across entries
     thereis (and new (long-atom-p atom))))

(defun long-atom-p (atom)
  (< 255 (atom-length atom)))
  
(defun atom-length (atom)
  (length (symbol-name atom)))


;;; Helper functions

(defun read-flags (number-of-refs stream)
  (decode-flags number-of-refs
		(read-bytes (1+ (floor (/ number-of-refs 2))) stream)))

(defun decode-flags (number-of-refs bytes &optional (pos 0))
  (loop ;; Read Flags and divide into half-bytes
     with fbytes = (1+ (floor (/ number-of-refs 2)))
     for byte across (subseq bytes pos (+ pos fbytes))
     collect (ldb (byte 4 0) byte) into half-bytes ;; Least significant
     collect (ldb (byte 4 4) byte) into half-bytes ;; Most significant
     finally (return (values half-bytes (+ pos fbytes))) ))


(defun read-atom-cache-ref (flags stream &optional long-atoms)
  (let ((new-entry (= 1 (ldb (byte 1 3) flags)))
	(segment-index (ldb (byte 3 0) flags))
	(internal-segment-index (read-byte stream)))
    (if new-entry
	(let* ((len (if long-atoms
			(read-uint16 stream)
			(read-byte stream)))
	       (atom (intern (read-bytes-as-string len stream))))
	  (atom-cache-add atom
			  *atom-cache*
			  segment-index
			  internal-segment-index)
	  atom)
	(atom-cache-get *atom-cache* segment-index internal-segment-index))))

(defun decode-atom-cache-ref (flags bytes &optional long-atoms (pos 0))
  (let ((new-entry (= 1 (ldb (byte 1 3) flags)))
	(segment-index (ldb (byte 3 0) flags))
	(internal-segment-index (aref bytes pos)))
    (if new-entry
	(let (pos1 len)
	  (if long-atoms
	      (progn
		(setf len (bytes-to-uint16 bytes (1+ pos)))
		(setf pos1 (+ 3 pos)))
	      (progn
		(setf len (aref bytes (1+ pos)))
		(setf pos1 (+ 2 pos))))
	  (let ((atom (intern (bytes-to-string bytes len pos1))))
	    (atom-cache-add atom
			    *atom-cache*
			    segment-index
			    internal-segment-index)
	    (values atom
		    (+ pos1 len))))
	(values (atom-cache-get *atom-cache*
				segment-index
				internal-segment-index)
		(1+ pos)))))
