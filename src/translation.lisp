(in-package :cleric)

;;;;
;;;; Encode Erlang term
;;;;

(defmethod encode ((x symbol) &key atom-cache-entries &allow-other-keys)
  (cond
    ((and (null x) *lisp-nil-is-erlang-empty-list*)
     (encode-external-nil))
    ((and (null x) *lisp-nil-symbol-is-erlang-false*)
     (encode '|false| :atom-cache-entries atom-cache-entries))
    ((and (eq T x) *lisp-t-symbol-is-erlang-true*)
     (encode '|true| :atom-cache-entries atom-cache-entries))
    (t
     (let ((index (when atom-cache-entries
                    (make-atom-cache-entry x atom-cache-entries))))
       (cond
         (index ;; Use an atom cache reference
          (encode-external-atom-cache-ref index))
         ;; Encode the atom as usual
         ((> 256 (length (symbol-name x)))
          (encode-external-small-atom x))
         (t
          (encode-external-atom x)) ))) ))

(defmethod encode ((x erlang-binary) &key &allow-other-keys)
  (if (= 8 (bits-in-last-byte x))
      (encode-external-binary x)
      (encode-external-bit-binary x)))

(defmethod encode ((x float) &key &allow-other-keys)
  ;; Should the old FLOAT_EXT ever be used?
  (encode-external-new-float (coerce x 'double-float)))

(defmethod encode ((x erlang-external-fun) &key &allow-other-keys)
  (encode-external-export x))

(defmethod encode ((x erlang-internal-fun) &key &allow-other-keys)
  ;; Determine if the Fun is new or old
  (error 'not-implemented-error
         :comment "One needs to determine whether the Fun is old or new."))

(defmethod encode ((x integer) &key &allow-other-keys)
  (typecase x
    ((unsigned-byte 8)
     (encode-external-small-integer x))
    ((integer #.(- (expt 2 31)) #.(1- (expt 2 31)))
     (encode-external-integer x))
    ((integer #.(- (expt 2 2040)) #.(expt 2 2040))
     (encode-external-small-big x))
    (t
     (encode-external-large-big x))))

(defmethod encode ((x list) &key atom-cache-entries &allow-other-keys)
  (if x
      (encode-external-list x atom-cache-entries)
      (encode-external-nil)))

(defmethod encode ((x string) &key &allow-other-keys)
  (cond (*lisp-string-is-erlang-binary*
         (encode (string-to-binary x)))
        ((> 65536 (length x))
         (encode-external-string x))
        (t
         (encode-external-list (map 'list #'char-code x)))))

(defmethod encode ((x erlang-pid) &key &allow-other-keys)
  (encode-external-pid x))

(defmethod encode ((x erlang-port) &key &allow-other-keys)
  (encode-external-port x))

(defmethod encode ((x erlang-reference) &key &allow-other-keys)
  (if (= 4 (length (slot-value x 'id)))
      (encode-external-reference x) ;; Perhaps always use new reference?
      (encode-external-new-reference x)))

(defmethod encode ((x erlang-tuple) &key atom-cache-entries &allow-other-keys)
  (if (> 256 (length (elements x)))
      (encode-external-small-tuple x atom-cache-entries)
      (encode-external-large-tuple x atom-cache-entries)))


;;;;
;;;; Decode Erlang term
;;;;

(defun decode (bytes &key (start 0) (version-tag nil))
  "Decode a sequence of bytes to an Erlang object."
  (when version-tag
    (let ((version (aref bytes start)))
      (unless (= version +protocol-version+)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list +protocol-version+))))
    (incf start))
  (let ((tag (aref bytes start)))
    (case tag
      (#.+new-float-ext+
       (decode-external-new-float bytes (1+ start)))
      (#.+bit-binary-ext+
       (decode-external-bit-binary bytes (1+ start)))
      (#.+atom-cache-ref+
       (decode-external-atom-cache-ref bytes (1+ start)))
      (#.+small-integer-ext+
       (decode-external-small-integer bytes (1+ start)))
      (#.+integer-ext+
       (decode-external-integer bytes (1+ start)))
      (#.+float-ext+
       (decode-external-float bytes (1+ start)))
      (#.+atom-ext+
       (decode-external-atom bytes (1+ start)))
      (#.+reference-ext+
       (decode-external-reference bytes (1+ start)))
      (#.+port-ext+
       (decode-external-port bytes (1+ start)))
      (#.+pid-ext+
       (decode-external-pid bytes (1+ start)))
      (#.+small-tuple-ext+
       (decode-external-small-tuple bytes (1+ start)))
      (#.+large-tuple-ext+
       (decode-external-large-tuple bytes (1+ start)))
      (#.+nil-ext+
       (decode-external-nil bytes (1+ start)))
      (#.+string-ext+
       (decode-external-string bytes (1+ start)))
      (#.+list-ext+
       (decode-external-list bytes (1+ start)))
      (#.+binary-ext+
       (decode-external-binary bytes (1+ start)))
      (#.+small-big-ext+
       (decode-external-small-big bytes (1+ start)))
      (#.+large-big-ext+
       (decode-external-large-big bytes (1+ start)))
      (#.+new-fun-ext+
       (decode-external-new-fun bytes (1+ start)))
      (#.+export-ext+
       (decode-external-export bytes (1+ start)))
      (#.+new-reference-ext+
       (decode-external-new-reference bytes (1+ start)))
      (#.+small-atom-ext+
       (decode-external-small-atom bytes (1+ start)))
      (#.+fun-ext+
       (decode-external-fun bytes (1+ start)))
      (#.+compressed-term+
       (decode-compressed-erlang-term bytes (1+ start)))
      (otherwise
       (error 'unexpected-message-tag-error
              :received-tag tag
              :expected-tags (list +new-float-ext+
                                   +bit-binary-ext+
                                   +atom-cache-ref+
                                   +small-integer-ext+
                                   +integer-ext+
                                   +float-ext+
                                   +atom-ext+
                                   +reference-ext+
                                   +port-ext+
                                   +pid-ext+
                                   +small-tuple-ext+
                                   +large-tuple-ext+
                                   +nil-ext+
                                   +string-ext+
                                   +list-ext+
                                   +binary-ext+
                                   +small-big-ext+
                                   +large-big-ext+
                                   +new-fun-ext+
                                   +export-ext+
                                   +new-reference-ext+
                                   +small-atom-ext+
                                   +fun-ext+
                                   +compressed-term+)
              )) )))


(defun read-erlang-term (stream &optional (use-version-tag nil))
  (when use-version-tag
    (let ((version (read-byte stream)))
      (unless (= version +protocol-version+)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list +protocol-version+)))))
  (let ((tag (read-byte stream)))
    (case tag
      (#.+new-float-ext+
       (read-external-new-float stream))
      (#.+bit-binary-ext+
       (read-external-bit-binary stream))
      (#.+atom-cache-ref+
       (read-external-atom-cache-ref stream))
      (#.+small-integer-ext+
       (read-external-small-integer stream))
      (#.+integer-ext+
       (read-external-integer stream))
      (#.+float-ext+
       (read-external-float stream))
      (#.+atom-ext+
       (read-external-atom stream))
      (#.+reference-ext+
       (read-external-reference stream))
      (#.+port-ext+
       (read-external-port stream))
      (#.+pid-ext+
       (read-external-pid stream))
      (#.+small-tuple-ext+
       (read-external-small-tuple stream))
      (#.+large-tuple-ext+
       (read-external-large-tuple stream))
      (#.+nil-ext+
       (read-external-nil stream))
      (#.+string-ext+
       (read-external-string stream))
      (#.+list-ext+
       (read-external-list stream))
      (#.+binary-ext+
       (read-external-binary stream))
      (#.+small-big-ext+
       (read-external-small-big stream))
      (#.+large-big-ext+
       (read-external-large-big stream))
      (#.+new-fun-ext+
       (read-external-new-fun stream))
      (#.+export-ext+
       (read-external-export stream))
      (#.+new-reference-ext+
       (read-external-new-reference stream))
      (#.+small-atom-ext+
       (read-external-small-atom stream))
      (#.+fun-ext+
       (read-external-fun stream))
      (#.+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-message-tag-error
              :received-tag tag
              :expected-tags (list +new-float-ext+
                                   +bit-binary-ext+
                                   +atom-cache-ref+
                                   +small-integer-ext+
                                   +integer-ext+
                                   +float-ext+
                                   +atom-ext+
                                   +reference-ext+
                                   +port-ext+
                                   +pid-ext+
                                   +small-tuple-ext+
                                   +large-tuple-ext+
                                   +nil-ext+
                                   +string-ext+
                                   +list-ext+
                                   +binary-ext+
                                   +small-big-ext+
                                   +large-big-ext+
                                   +new-fun-ext+
                                   +export-ext+
                                   +new-reference-ext+
                                   +small-atom-ext+
                                   +fun-ext+
                                   +compressed-term+)
              )) )))

(defun read-compressed-erlang-term (stream)
  (let ((uncompressed-size (read-uint32 stream)))
    (declare (ignore uncompressed-size))
    (error 'not-implemented-error
           :comment "Reading compressed Erlang terms is unsupported.")))

(defun decode-compressed-erlang-term (bytes &optional (pos 0))
  (let ((uncompressed-size (bytes-to-uint32 bytes pos)))
    (declare (ignore uncompressed-size))
    (error 'not-implemented-error
           :comment "Decoding compressed Erlang terms is unsupported.")))


;;;;
;;;; Erlang atom
;;;;

(defun read-erlang-atom (stream) ;; OBSOLETE?
  (let* ((tag (read-byte stream))
	 (symbol (case tag
		   (#.+atom-cache-ref+
		    (read-external-atom-cache-ref stream))
		   (#.+atom-ext+
		    (read-external-atom stream))
		   (#.+small-atom-ext+
		    (read-external-small-atom stream))
		   (#.+compressed-term+
		    (read-compressed-erlang-term stream))
		   (otherwise
		    (error 'unexpected-message-tag-error
			   :received-tag tag
			   :expected-tags (list +atom-cache-ref+
						+atom-ext+
						+small-atom-ext+
						+compressed-term+))) )))
    (cond
      ((and (eq symbol '|true|) *erlang-true-is-lisp-t-symbol*)
       T)
      ((and (eq symbol '|false|) *erlang-false-is-lisp-nil-symbol*)
       NIL)
      (t
       symbol)) ))

(defun decode-erlang-atom (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (multiple-value-bind (symbol pos2)
	(case tag
	  (#.+atom-cache-ref+
	   (decode-external-atom-cache-ref bytes (1+ pos)))
	  (#.+atom-ext+
	   (decode-external-atom bytes (1+ pos)))
	  (#.+small-atom-ext+
	   (decode-external-small-atom bytes (1+ pos)))
	  (#.+compressed-term+
	   (decode-compressed-erlang-term bytes (1+ pos)))
	  (otherwise
	   (error 'unexpected-message-tag-error
		  :received-tag tag
		  :expected-tags (list +atom-cache-ref+
				       +atom-ext+
				       +small-atom-ext+
				       +compressed-term+))) )
      (cond
	((and (eq symbol '|true|) *erlang-true-is-lisp-t-symbol*)
	 (values T pos2))
	((and (eq symbol '|false|) *erlang-false-is-lisp-nil-symbol*)
	 (values NIL pos2))
	(t
	 (values symbol pos2))) )))




;; ATOM_CACHE_REF
;; +----+-------------------+
;; |  1 |         1         |
;; +----+-------------------+
;; | 82 | AtomCacheRefIndex |
;; +----+-------------------+
;;

(defun encode-external-atom-cache-ref (reference-index)
  (concatenate 'vector
	       (vector +atom-cache-ref+)
	       (vector reference-index)))

(defun read-external-atom-cache-ref (stream) ;; OBSOLETE?
  ;; Assume tag +atom-cache-ref+ is read
  (decode-external-atom-cache-ref (read-bytes 1 stream)))

(defun decode-external-atom-cache-ref (bytes &optional (pos 0))
  (values (svref *cached-atoms* (aref bytes pos))
	  (1+ pos)))


;; ATOM_EXT
;; +-----+-----+----------+
;; |  1  |  2  |    Len   |
;; +-----+-----+----------+
;; | 100 | Len | AtomName |
;; +-----+-----+----------+
;;

(defun encode-external-atom (atom)
  (concatenate 'vector
	       (vector +atom-ext+)
	       (uint16-to-bytes (length (symbol-name atom)))
	       (string-to-bytes (symbol-name atom))))

(defun read-external-atom (stream) ;; OBSOLETE?
  ;; Assume tag +atom-ext+ is read
  (let* ((length-bytes (read-bytes 2 stream))
	 (length (bytes-to-uint16 length-bytes))
	 (atom-text (read-bytes length stream)))
    (decode-external-atom (concatenate 'vector length-bytes atom-text))))

(defun decode-external-atom (bytes &optional (pos 0))
  (let ((length (bytes-to-uint16 bytes pos))
	(pos2 (+ 2 pos)))
    (values (intern (bytes-to-string bytes length pos2))
	    (+ pos2 length))))


;; SMALL_ATOM_EXT
;; +-----+-----+----------+
;; |  1  |  1  |    Len   |
;; +-----+-----+----------+
;; | 115 | Len | AtomName |
;; +-----+-----+----------+
;;

(defun encode-external-small-atom (atom)
  (concatenate 'vector
	       (vector +small-atom-ext+)
	       (vector (length (symbol-name atom)))
	       (string-to-bytes (symbol-name atom))))

(defun read-external-small-atom (stream) ;; OBSOLETE?
  ;; Assume tag +atom-small-ext+ is read
  (let* ((length (read-byte stream))
	 (atom-text (read-bytes length stream)))
    (decode-external-atom (concatenate 'vector (vector length) atom-text))))

(defun decode-external-small-atom (bytes &optional (pos 0))
  (let ((length (aref bytes pos))
	(pos1 (1+ pos)))
    (values (intern (bytes-to-string bytes length pos1))
	    (+ pos1 length))))



;;;;
;;;; Erlang binary
;;;;


;; BINARY_EXT
;; +-----+-----+------+
;; |  1  |  4  |  Len |
;; +-----+-----+------+
;; | 109 | Len | Data |
;; +-----+-----+------+
;;

(defun encode-external-binary (erlang-binary)
  (with-slots (bytes) erlang-binary
    (concatenate 'vector
		 (vector +binary-ext+)
		 (uint32-to-bytes (length bytes))
		 bytes)))

(defun read-external-binary (stream) ;; OBSOLETE?
  ;; Assume tag +binary-ext+ is read
  (let* ((length (read-uint32 stream))
	 (bytes (read-bytes length stream)))
    (make-instance 'erlang-binary :bytes bytes)))

(defun decode-external-binary (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
	(pos4 (+ 4 pos)))
    (values (make-instance 'erlang-binary
			   :bytes (subseq bytes pos4 (+ pos4 length)))
	    (+ pos4 length))))


;; BIT_BINARY_EXT
;; +----+-----+------+------+
;; |  1 |  4  |   1  |  Len |
;; +----+-----+------+------+
;; | 77 | Len | Bits | Data |
;; +----+-----+------+------+
;;

(defun encode-external-bit-binary (erlang-binary)
  (with-slots (bytes bits) erlang-binary
    (concatenate 'vector
		 (vector +bit-binary-ext+)
		 (uint32-to-bytes (length bytes))
		 (vector bits)
		 bytes)))

(defun read-external-bit-binary (stream) ;; OBSOLETE?
  ;; Assume tag +bit-binary-ext+ is read
  (let* ((length (read-uint32 stream)))
    (make-instance 'erlang-binary
		   :bits (read-byte stream)
		   :bytes (read-bytes length stream))))

(defun decode-external-bit-binary (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
	(bits (aref bytes (+ 4 pos)))
	(pos5 (+ pos 5)))
    (values (make-instance 'erlang-binary
			   :bits bits
			   :bytes (subseq bytes pos5 (+ pos5 length)))
	    (+ pos5 length))))


;;;;
;;;; Erlang float
;;;;

;; NEW_FLOAT_EXT
;; +----+------------+
;; |  1 |      8     |
;; +----+------------+
;; | 70 | IEEE float |
;; +----+------------+
;;

(defun encode-external-new-float (double-float)
  (concatenate 'vector
	       (vector +new-float-ext+)
	       (double-float-to-bytes double-float)))

(defun read-external-new-float (stream) ;; OBSOLETE?
  ;; Assume tag +new-float-ext+ is read
  (decode-external-new-float (read-bytes 8 stream)))

(defun decode-external-new-float (bytes &optional (pos 0))
  (values (bytes-to-double-float (subseq bytes pos))
	  (+ 8 pos)))


;; FLOAT_EXT (superseded by NEW_FLOAT_EXT)
;; +----+--------------+
;; |  1 |      31      |
;; +----+--------------+
;; | 99 | Float String |
;; +----+--------------+
;;
;; The float is stored in string format, created with sprintf with format "%.20e".
;; To unpack the float use sscanf with format "%lf".

(defun encode-external-float (float)
  (concatenate 'vector
	       (vector +float-ext+)
	       (string-to-bytes (format nil "~(~,20E~)" float))))

(defun read-external-float (stream) ;; OBSOLETE?
  ;; Assume tag +float-ext+ is read
  (decode-external-float (read-bytes 31 stream)))

(defun decode-external-float (bytes &optional (pos 0))
  (let ((string (bytes-to-string bytes 31 pos)))
    (values (read-from-string string)
	    (+ 31 pos))))


;;;;
;;;; Erlang function ("Fun")
;;;;
    
;; FUN_EXT
;; +-----+---------+-----+--------+-------+------+-----------+
;; |  1  |     4   |     |        |       |      |           |
;; +-----+---------+-----+--------+-------+------+-----------+
;; | 117 | NumFree | Pid | Module | Index | Uniq | Free vars |
;; +-----+---------+-----+--------+-------+------+-----------+
;;

(defun encode-external-fun (fun)
  (with-slots (module pid index uniq free-vars) fun
    (concatenate 'vector
		 (vector +fun-ext+)
		 (uint32-to-bytes (length free-vars))
		 (encode pid)
		 (encode module)
		 (encode index)
		 (encode uniq)
		 (list-contents-to-bytes free-vars))))

(defun read-external-fun (stream) ;; OBSOLETE?
  ;; Assume tag +fun-ext+ is read
  (let ((free-vars-length (read-uint32 stream)))
    (make-instance 'erlang-internal-fun
		   :pid (read-erlang-pid stream)
		   :module (read-erlang-atom stream)
		   :index (read-erlang-integer stream)
		   :uniq (read-erlang-integer stream)
		   :free-vars (loop
				 repeat free-vars-length
				 collect (read-erlang-term stream)))))

(defun decode-external-fun (bytes &optional (pos 0))
  (let ((free-vars-length (bytes-to-uint32 bytes pos)))
    (multiple-value-bind* (((pid pos1) (decode-erlang-pid bytes (+ pos 4)))
			   ((module pos2) (decode-erlang-atom bytes pos1))
			   ((index pos3) (decode-erlang-integer bytes pos2))
			   ((uniq pos4) (decode-erlang-integer bytes pos3)))
      (loop
	 repeat free-vars-length
	 for (free-var p) = (multiple-value-list
			     (decode bytes :start pos4))
	 do (setf pos4 p)
	 collect free-var into free-vars
	 finally (return (values (make-instance 'erlang-internal-fun
						:pid pid
						:module module
						:index index
						:uniq uniq
						:free-vars free-vars)
				 pos)) ))))


;; NEW_FUN_EXT
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;; |  1  |   4  |   1   |  16  |    4  |     4   |        |          |         |     |           |
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;; | 112 | Size | Arity | Uniq | Index | NumFree | Module | OldIndex | OldUniq | Pid | Free vars |
;; +-----+------+-------+------+-------+---------+--------+----------+---------+-----+-----------+
;;

(defun encode-external-new-fun (fun)
  (with-slots (module arity pid index uniq free-vars new-uniq new-index) fun
    (let ((bytes (concatenate 'vector
			      (vector arity)
			      new-uniq
			      (uint32-to-bytes new-index)
			      (uint32-to-bytes (length free-vars))
			      (encode module)
			      (encode index)
			      (encode uniq)
			      (encode pid)
			      (list-contents-to-bytes free-vars))))
      (concatenate 'vector
		   (vector +new-fun-ext+)
		   (uint32-to-bytes (+ 4 (length bytes)))
		   bytes))))

(defun read-external-new-fun (stream) ;; OBSOLETE?
  ;; Assume tag +new-fun-ext+ is read
  (let ((size-bytes (read-bytes 4 stream)))
    (decode-external-new-fun
     (concatenate 'vector
		  size-bytes
		  (read-bytes (- (bytes-to-uint32 size-bytes) 4) stream)))))

(defun decode-external-new-fun (bytes &optional (pos 0))
  (let ((size (bytes-to-uint32 bytes pos))
	(arity (aref bytes (+ 4 pos)))
	(uniq (subseq bytes (+ 5 pos) (+ 21 pos)))
	(index (bytes-to-uint32 (+ 21 pos)))
	(free-vars-length (bytes-to-uint32 bytes (+ 25 pos))))
    (multiple-value-bind* (((module pos1) (decode-erlang-atom bytes (+ 29 pos)))
			   ((old-index pos2) (decode-erlang-integer bytes pos1))
			   ((old-uniq pos3) (decode-erlang-integer bytes pos2))
			   ((pid pos4) (decode-erlang-pid bytes pos3)))
      (loop
	 repeat free-vars-length
	 for (free-var p) = (multiple-value-list
			     (decode bytes :start pos4))
	 do (setf pos4 p)
	 collect free-var into free-vars
	 finally (return (progn
			   (assert (= p (+ size pos)))
			   (values (make-instance 'erlang-new-internal-fun
						  :arity arity
						  :uniq uniq
						  :index index
						  :module module
						  :old-index old-index
						  :old-uniq old-uniq
						  :pid pid
						  :free-vars free-vars)
				   p)) )))))



;; EXPORT_EXT
;; +-----+--------+----------+-------+
;; |  1  |        |          |       |
;; +-----+--------+----------+-------+
;; | 113 | Module | Function | Arity |
;; +-----+--------+----------+-------+
;;

(defun encode-external-export (fun)
  (with-slots (module function arity) fun
    (concatenate 'vector
		 (vector +export-ext+)
		 (encode module)
		 (encode function)
		 (encode arity))))

(defun read-external-export (stream) ;; OBSOLETE?
  ;; Assume tag +export-ext+ is read
  (make-instance 'erlang-external-fun
		 :module (read-erlang-atom stream)
		 :function (read-erlang-atom stream)
		 :arity (read-erlang-integer stream)))

(defun decode-external-export (bytes &optional (pos 0))
  (multiple-value-bind* (((module pos1) (decode-erlang-atom bytes pos))
			 ((function pos2) (decode-erlang-atom bytes pos1))
			 ((arity pos3) (decode-erlang-integer bytes pos2)))
    (values (make-instance 'erlang-external-fun
			   :module module
			   :function function
			   :arity arity)
	    pos3)))


;;;;
;;;; Erlang integer
;;;;

(defun read-erlang-integer (stream) ;; OBSOLETE?
  (let ((tag (read-byte stream)))
    (case tag
      (#.+small-integer-ext+
       (read-external-small-integer stream))
      (#.+integer-ext+
       (read-external-integer stream))
      (#.+small-big-ext+
       (read-external-small-big stream))
      (#.+large-big-ext+
       (read-external-large-big stream))
      (#'+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-message-tag-error
	      :received-tag tag
	      :expected-tags (list +small-integer-ext+
				   +integer-ext+
				   +small-big-ext+
				   +large-big-ext+
				   +compressed-term+))) )))

(defun decode-erlang-integer (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (case tag
      (#.+small-integer-ext+
       (decode-external-small-integer bytes (1+ pos)))
      (#.+integer-ext+
       (decode-external-integer bytes (1+ pos)))
      (#.+small-big-ext+
       (decode-external-small-big bytes (1+ pos)))
      (#.+large-big-ext+
       (decode-external-large-big bytes (1+ pos)))
      (#'+compressed-term+
       (decode-compressed-erlang-term bytes (1+ pos)))
      (otherwise
       (error 'unexpected-message-tag-error
	      :received-tag tag
	      :expected-tags (list +small-integer-ext+
				   +integer-ext+
				   +small-big-ext+
				   +large-big-ext+
				   +compressed-term+))) )))


;; SMALL_INTEGER_EXT
;; +----+-----+
;; |  1 |  1  |
;; +----+-----+
;; | 97 | Int |
;; +----+-----+
;;

(defun encode-external-small-integer (uint8) ;; (<= 0 X 255)
  (vector +small-integer-ext+ uint8))

(defun read-external-small-integer (stream) ;; OBSOLETE?
  ;; Assume tag +small-integer-ext+ is read
  (decode-external-small-integer (read-bytes 1 stream)))

(defun decode-external-small-integer (bytes &optional (pos 0))
  (values (aref bytes pos)
	  (1+ pos)))


;; INTEGER_EXT
;; +----+-----+
;; |  1 |  4  |
;; +----+-----+
;; | 98 | Int |
;; +----+-----+
;;

(defun encode-external-integer (int32) ;; (<= (- (expt 2 31)) X (1- (expt 2 31)))
  (concatenate 'vector (vector +integer-ext+) (uint32-to-bytes int32)))

(defun read-external-integer (stream) ;; OBSOLETE?
  ;; Assume tag +integer-ext+ is read
  (decode-external-integer (read-bytes 4 stream)))

(defun decode-external-integer (bytes &optional (pos 0))
  (values (bytes-to-signed-int32 bytes pos)
	  (+ 4 pos)))


;; SMALL_BIG_EXT
;; +-----+---+------+-----------------+
;; |  1  | 1 |   1  |        N        |
;; +-----+---+------+-----------------+
;; | 110 | N | Sign | d(0) ... d(N-1) |
;; +-----+---+------+-----------------+
;;
;; Sign = 0 if the integer is positive, 1 if it's negative.
;; B = 256
;; Formula: d(0)*B^0 + d(1)*B^1 + ... d(N-1)*B^(N-1)

(defun encode-external-small-big (bignum) ;; (< (- (expt 2 2040)) X (expt 2 2040))
  (let* ((sign (if (< bignum 0) 1 0))
	 (unsigned-bignum (abs bignum))
	 (length (bignum-byte-length unsigned-bignum)))
    (concatenate 'vector
		 (vector +small-big-ext+ length sign)
		 (bignum-to-bytes unsigned-bignum length))))

(defun read-external-small-big (stream) ;; OBSOLETE?
  ;; Assume tag +small-big-ext+ is read
  (let ((length (read-byte stream)))
    (decode-external-small-big
     (concatenate 'vector (vector length) (read-bytes (1+ length) stream)))))

(defun decode-external-small-big (bytes &optional (pos 0))
  (let ((length (aref bytes pos))
	(sign (if (= 1 (aref bytes (1+ pos))) -1 1))
	(pos2 (+ 2 pos)))
    (values (* sign (bytes-to-bignum bytes length pos2))
	    (+ length pos2))))


;; LARGE_BIG_EXT
;; +-----+---+------+-----------------+
;; |  1  | 4 |   1  |        N        |
;; +-----+---+------+-----------------+
;; | 111 | N | Sign | d(0) ... d(N-1) |
;; +-----+---+------+-----------------+
;;
;; Sign = 0 if the integer is positive, 1 if it's negative.
;; B = 256
;; Formula: d(0)*B^0 + d(1)*B^1 + ... d(N-1)*B^(N-1)

(defun encode-external-large-big (bignum)
  (let* ((sign (if (< bignum 0) 1 0))
	 (unsigned-bignum (abs bignum))
	 (length (bignum-byte-length unsigned-bignum)))
    (concatenate 'vector
		 (vector +large-big-ext+)
		 (uint32-to-bytes length)
		 (vector sign)
		 (bignum-to-bytes unsigned-bignum length))))

(defun read-external-large-big (stream)
  ;; Assume tag +large-big-ext+ is read
  (let ((length-bytes (read-bytes 4 stream)))
    (decode-external-large-big
     (concatenate 'vector
		  length-bytes
		  (read-bytes (bytes-to-uint32 length-bytes) stream)))))

(defun decode-external-large-big (bytes &optional (pos 0))
  (let ((length (bytes-to-uint32 bytes pos))
	(sign (if (= 1 (aref bytes (+ 4 pos))) -1 1))
	(pos5 (+ 5 pos)))
    (values (* sign (bytes-to-bignum bytes length pos5))
	    (+ length pos5))))



;;; Helper functions

(defun bignum-byte-length (bignum)
  ;; Very naive way of calculating the byte length...
  (let ((length (length (format nil "~x" bignum))))
    (/ (if (oddp length) (1+ length) length)
       2)))

(defun bignum-to-bytes (bignum length)
  (reverse (unsigned-integer-to-bytes bignum length)))

(defun bytes-to-bignum (bytes &optional number-of-bytes (pos 0))
  (bytes-to-unsigned-integer
   (reverse (subseq bytes pos (when number-of-bytes
				(+ pos number-of-bytes))))))


;;;;
;;;; Erlang list
;;;;

;; NIL_EXT
;; +-----+
;; |  1  |
;; +-----+
;; | 106 |
;; +-----+
;;

(defun encode-external-nil ()
  (vector +nil-ext+))

(defun read-external-nil (stream)
  ;; Assume tag +nil-ext+ is read
  (decode-external-nil (read-bytes 0 stream)))

(defun decode-external-nil (bytes &optional (pos 0))
  (declare (ignore bytes))
  (values nil pos))


;; STRING_EXT
;; +-----+--------+------------+
;; |  1  |    2   |   Length   |
;; +-----+--------+------------+
;; | 107 | Length | Characters |
;; +-----+--------+------------+
;;

(defun encode-external-string (chars)
  (concatenate 'vector
	       (vector +string-ext+)
	       (uint16-to-bytes (length chars))
	       (if (stringp chars)
		   (string-to-bytes chars)
		   (coerce chars 'vector))))

(defun read-external-string (stream) ;; OBSOLETE?
  ;; Assume tag +string-ext+ is read
  (let ((length-bytes (read-bytes 2 stream)))
    (decode-external-string
     (concatenate 'vector
		  length-bytes
		  (read-bytes (bytes-to-uint16 length-bytes) stream)))))

(defun decode-external-string (bytes &optional (pos 0))
  (let* ((length (bytes-to-uint16 bytes pos))
	 (bytes (subseq bytes (+ 2 pos) (+ 2 length pos))))
    (values (if *erlang-string-is-lisp-string*
		(bytes-to-string bytes)
		(coerce bytes 'list))
	    (+ 2 length pos))))



;; LIST_EXT
;; +-----+--------+----------+------+
;; |  1  |    4   |     N    |   M  |
;; +-----+--------+----------+------+
;; | 108 | Length | Elements | Tail |
;; +-----+--------+----------+------+
;;

(defun encode-external-list (list &optional atom-cache-entries)
  (multiple-value-bind (elements tail length)
      (list-contents-to-bytes list atom-cache-entries)
    (concatenate 'vector
		 (vector +list-ext+)
		 (uint32-to-bytes length)
		 elements
		 tail)))

(defun read-external-list (stream) ;; OBSOLETE?
  ;; Assume tag +list-ext+ is read
  (read-list-contents stream (read-uint32 stream)))

(defun decode-external-list (bytes &optional (pos 0))
  (decode-list-contents bytes (bytes-to-uint32 bytes pos) (+ 4 pos)))



;;; Helper functions

(defun list-contents-to-bytes (list &optional atom-cache-entries)
  (loop
     with bytes = #()
     for (element . tail) on list
     for length upfrom 1
     do (setf bytes (concatenate
		     'vector
		     bytes
		     (encode element :atom-cache-entries atom-cache-entries)))
     finally
       (let ((tail-bytes (if (and (null tail)
				  *lisp-nil-at-tail-is-erlang-empty-list*)
			     (encode-external-nil)
			     (encode tail
				     :atom-cache-entries atom-cache-entries))))
	 (return (values bytes tail-bytes length))) ))

(defun read-list-contents (stream length)
  (if (= 0 length)
      (read-erlang-term stream)
      (cons (read-erlang-term stream)
	    (read-list-contents stream (1- length)))))

(defun decode-list-contents (bytes length &optional (pos 0))
  (if (= 0 length)
      (decode bytes :start pos)
      (multiple-value-bind* (((term new-pos) (decode bytes :start pos))
			     ((tail end-pos)
			      (decode-list-contents bytes (1- length) new-pos)))
	(values (cons term tail) end-pos) )))


;;;;
;;;; Erlang pid
;;;;

(defun read-erlang-pid (stream) ;; OBSOLETE?
  (let ((tag (read-byte stream)))
    (case tag
      (#.+pid-ext+
       (read-external-pid stream))
      (#.+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-erlang-term
	      :received-tag tag
	      :expected-tags (list +pid-ext+
				   +compressed-term+))) )))

(defun decode-erlang-pid (bytes &optional (pos 0))
  (let ((tag (aref bytes pos)))
    (case tag
      (#.+pid-ext+
       (decode-external-pid bytes (1+ pos)))
      (#.+compressed-term+
       (decode-compressed-erlang-term bytes (1+ pos)))
      (otherwise
       (error 'unexpected-erlang-term
	      :received-tag tag
	      :expected-tags (list +pid-ext+
				   +compressed-term+))) )))


;; PID_EXT
;; +-----+------+----+--------+----------+
;; |  1  |   N  |  4 |    4   |     1    |
;; +-----+------+----+--------+----------+
;; | 103 | Node | ID | Serial | Creation |
;; +-----+------+----+--------+----------+
;;

(defun encode-external-pid (pid)
  (with-slots (node id serial creation) pid
    (concatenate 'vector
		 (vector +pid-ext+)
		 (encode node)
		 id
		 serial
		 (vector creation))))

(defun read-external-pid (stream) ;; OBSOLETE?
  ;; Assume tag +pid-ext+ is read
  (make-instance 'erlang-pid
		 :node (read-erlang-atom stream)
		 :id (read-bytes 4 stream)
		 :serial (read-bytes 4 stream)
		 :creation (read-byte stream)))

(defun decode-external-pid (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-pid
			   :node node
			   :id (subseq bytes pos1 (+ pos1 4))
			   :serial (subseq bytes (+ pos1 4) (+ pos1 8))
			   :creation (aref bytes (+ pos1 8)))
	    (+ pos1 9))))


;;;;
;;;; Erlang port
;;;;

;; PORT_EXT
;; +-----+------+----+----------+
;; |  1  |   N  |  4 |     1    |
;; +-----+------+----+----------+
;; | 102 | Node | ID | Creation |
;; +-----+------+----+----------+
;;

(defun encode-external-port (port)
  (with-slots (node id creation) port
    (concatenate 'vector
		 (vector +port-ext+)
		 (encode node)
		 id
		 (vector creation))))

(defun read-external-port (stream) ;; OBSOLETE?
  ;; Assume tag +port-ext+ is read
  (make-instance 'erlang-port
		 :node (read-erlang-atom stream)
		 :id (read-bytes 4 stream)
		 :creation (read-byte stream)))

(defun decode-external-port (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-port
			   :node node
			   :id (subseq bytes pos1 (+ pos1 4))
			   :creation (aref bytes (+ pos1 4)))
	    (+ pos1 5))))


;;;;
;;;; Erlang reference
;;;;

;; REFERENCE_EXT
;; +-----+------+----+----------+
;; |  1  |   N  |  4 |     1    |
;; +-----+------+----+----------+
;; | 101 | Node | ID | Creation |
;; +-----+------+----+----------+
;;

(defun encode-external-reference (ref)
  (with-slots (node id creation) ref
    (concatenate 'vector
		 (vector +reference-ext+)
		 (encode node)
		 id
		 (vector creation))))

(defun read-external-reference (stream) ;; OBSOLETE?
  ;; Assume tag +reference-ext+ is read
  (make-instance 'erlang-reference
		 :node (read-erlang-atom stream)
		 :id (read-bytes 4 stream)
		 :creation (read-byte stream)))

(defun decode-external-reference (bytes &optional (pos 0))
  (multiple-value-bind (node pos1) (decode-erlang-atom bytes pos)
    (values (make-instance 'erlang-reference
			   :node node
			   :id (subseq bytes pos1 (+ pos1 4))
			   :creation (aref bytes (+ pos1 4)))
	    (+ pos1 5))))


;; NEW_REFERENCE_EXT
;; +-----+-----+------+----------+--------+
;; |  1  |  2  |   N  |     1    |    N'  |
;; +-----+-----+------+----------+--------+
;; | 114 | Len | Node | Creation | ID ... |
;; +-----+-----+------+----------+--------+
;;

(defun encode-external-new-reference (ref)
  (with-slots (node creation id) ref
    (concatenate 'vector
		 (vector +new-reference-ext+)
		 (uint16-to-bytes (/ (length id) 4))
		 (encode node)
		 (vector creation)
		 id))) ;; Several 4-byte IDs..

(defun read-external-new-reference (stream) ;; OBSOLETE?
  ;; Assume tag +new-reference-ext+ is read
  (let ((length (read-uint16 stream)))
    (make-instance 'erlang-reference
		   :node (read-erlang-atom stream)
		   :creation (read-byte stream)
		   :id (read-bytes (* 4 length) stream))))

(defun decode-external-new-reference (bytes &optional (pos 0))
  (let ((length (bytes-to-uint16 bytes pos)))
    (multiple-value-bind (node pos1) (decode-erlang-atom bytes (+ 2 pos))
      (values (make-instance 'erlang-reference
			     :node node
			     :creation (aref bytes pos1)
			     :id (subseq bytes
					 (1+ pos1)
					 (+ 1 pos1 (* 4 length))))
	      (+ 1 pos1 (* 4 length))))))


;;;;
;;;; Erlang tuple
;;;;

(defun erlang-tuple-ref (tuple pos)
  (svref (elements tuple) pos))

(defun erlang-tuple-arity (tuple)
  (length (elements tuple)))


(defun read-erlang-tuple (stream &optional use-version-tag) ;; OBSOLETE?
  (when use-version-tag
    (let ((version (read-byte stream)))
      (unless (= version +protocol-version+)
	(error 'unexpected-message-tag-error
	       :received-tag version
	       :expected-tags (list +protocol-version+)))))
  (let ((tag (read-byte stream)))
    (case tag
      (#.+small-tuple-ext+ (read-external-small-tuple stream))
      (#.+large-tuple-ext+ (read-external-large-tuple stream))
      (#.+compressed-term+
       (read-compressed-erlang-term stream))
      (otherwise
       (error 'unexpected-message-tag-error
	      :received-tag tag
	      :expected-tags (list +small-tuple-ext+
				   +large-tuple-ext+
				   +compressed-term+))) )))

(defun decode-erlang-tuple (bytes &key (start 0) (version-tag nil))
  (when version-tag
    (let ((version (aref bytes start)))
      (unless (= version +protocol-version+)
	  (error 'unexpected-message-tag-error
		 :received-tag version
		 :expected-tags (list +protocol-version+))))
    (incf start))
  (let ((tag (aref bytes start)))
    (case tag
      (#.+small-tuple-ext+ (decode-external-small-tuple bytes (1+ start)))
      (#.+large-tuple-ext+ (decode-external-large-tuple bytes (1+ start)))
      (#.+compressed-term+
       (decode-compressed-erlang-term bytes (1+ start)))
      (otherwise
       (error 'unexpected-message-tag-error
	      :received-tag tag
	      :expected-tags (list +small-tuple-ext+
				   +large-tuple-ext+
				   +compressed-term+))) )))


;; SMALL_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   1   |     N    |
;; +-----+-------+----------+
;; | 104 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-small-tuple (tuple atom-cache-entries)
  (concatenate 'vector
	       (vector +small-tuple-ext+ (erlang-tuple-arity tuple))
	       (mapconc-vector
		#'(lambda (element)
		    (encode element :atom-cache-entries atom-cache-entries))
		(elements tuple))))

(defun read-external-small-tuple (stream) ;; OBSOLETE?
  ;; Assume tag +small-tuple-ext+ is read
  (make-instance 'erlang-tuple
		 :elements (read-tuple-contents stream (read-byte stream))))

(defun decode-external-small-tuple (bytes &optional (pos 0))
  (let ((arity (aref bytes pos)))
    (multiple-value-bind (elements new-pos)
	(decode-tuple-contents bytes arity (1+ pos))
      (values (make-instance 'erlang-tuple :elements elements)
	      new-pos))))


;; LARGE_TUPLE_EXT
;; +-----+-------+----------+
;; |  1  |   4   |     N    |
;; +-----+-------+----------+
;; | 105 | Arity | Elements |
;; +-----+-------+----------+
;;

(defun encode-external-large-tuple (tuple atom-cache-entries)
  (concatenate 'vector
	       (vector +large-tuple-ext+)
	       (uint32-to-bytes (erlang-tuple-arity tuple))
	       (mapconc-vector
		#'(lambda (element)
		    (encode element :atom-cache-entries atom-cache-entries))
		(elements tuple))))

(defun read-external-large-tuple (stream)
  ;; Assume tag +large-tuple-ext+ is read
  (make-instance 'erlang-tuple
		 :elements (read-tuple-contents stream (read-uint32 stream))))

(defun decode-external-large-tuple (bytes &optional (pos 0))
  (let ((arity (bytes-to-uint32 bytes pos)))
    (multiple-value-bind (elements new-pos)
	(decode-tuple-contents bytes arity (+ 4 pos))
      (values (make-instance 'erlang-tuple :elements elements)
	      new-pos))))


;;; Helper functions

(defun mapconc-vector (fn vector)
  (loop
     with bytes = #()
     for element across vector
     do (setf bytes (concatenate 'vector bytes (funcall fn element)))
     finally (return bytes)))

(defun read-tuple-contents (stream arity)
  (coerce (loop repeat arity collect (read-erlang-term stream)) 'vector))

(defun decode-tuple-contents (bytes arity pos)
  (loop
     repeat arity
     for (element pos1) = (multiple-value-list (decode bytes :start pos))
     do (setf pos pos1)
     collect element into elements
     finally (return (values (coerce elements 'vector) pos))))
