(in-package :cleric)

;;;;
;;;; Erlang integer
;;;;

;;;
;;; Methods
;;;

(defmethod match-p ((a integer) (b integer))
  (= a b))


;;;
;;; Encode/Decode
;;;

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
