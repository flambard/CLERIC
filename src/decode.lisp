(in-package :cleric)

;;;;
;;;; Decode Erlang term
;;;;

(defun decode (bytes &key (start 0) (version-tag nil))
  "Decode a sequence of bytes to an Erlang object."
  (when (integerp version-tag)
    (let ((version (aref bytes start)))
      (unless (= version version-tag)
        (error 'unexpected-message-tag-error
               :received-tag version
               :expected-tags (list version-tag))))
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


(defun decode-compressed-erlang-term (bytes &optional (pos 0))
  (let ((uncompressed-size (bytes-to-uint32 bytes pos)))
    (declare (ignore uncompressed-size))
    (error 'not-implemented-error
           :comment "Decoding compressed Erlang terms is unsupported.")))
