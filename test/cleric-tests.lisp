(in-package :cleric-test)

(in-suite cleric)

(test sbcl-challenge-md5-digest-bug
    ;; Bug in CLERIC::CALCULATE-DIGEST, it returned same MD5sum for different
    ;; strings! Turns out that MD5:MD5SUM-SEQUENCE is deprecated for strings.
    ;; Use MD5:MD5SUM-STRING instead.
    (is (not (every #'=
                    (cleric::calculate-digest 1234567890 "kaka")
                    (cleric::calculate-digest 1234567891 "kaka"))))
  )

(test decode-distribution-header
  ;;; Atom cache ref decoding
  ;; Decode new short atom cache ref
  (let ((etf-aci:*atom-cache* (make-instance 'erlang-term-test::mock-atom-cache))
        (bytes (vector 0 4 65 66 66 65)))
    (is (equalp (list :abba t 0 0)
                (cleric::decode-atom-cache-ref #b1000 bytes nil 0))))
  ;; Decode new long atom cache ref
  (let ((etf-aci:*atom-cache* (make-instance 'erlang-term-test::mock-atom-cache))
        (bytes (vector 0 0 4 65 66 66 65)))
    (is (equalp (list :abba t 0 0)
                (cleric::decode-atom-cache-ref #b1000 bytes t 0))))
  ;; Decode already registered short atom cache ref
  (let ((etf-aci:*atom-cache* (make-instance 'erlang-term-test::mock-atom-cache))
        (bytes (vector 0)))
    (is (equalp (list :abba nil 0 0)
                (cleric::decode-atom-cache-ref #b0000 bytes nil 0))))
  ;; Decode already registered long atom cache ref
  (let ((etf-aci:*atom-cache* (make-instance 'erlang-term-test::mock-atom-cache))
        (bytes (vector 0)))
    (is (equalp (list :abba nil 0 0)
                (cleric::decode-atom-cache-ref #b0000 bytes t 0))))
  )

(test encode-distribution-header
  (is (equalp #(131 68 0)
              (cleric::make-distribution-header)))
  )
