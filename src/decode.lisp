(in-package :cleric)

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
