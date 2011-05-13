(in-package :cleric)

;;;;
;;;; Erlang atom
;;;;

;;;
;;; Methods
;;;

(defmethod match-p ((a symbol) (b symbol))
  (eql a b)) ;; What about symbols in different packages?


;;;
;;; Encode/Decode
;;;

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
