(in-package #:cleric-test)

#+nil(def-suite cleric :in cleric)
(in-suite cleric)


(test sbcl-challenge-md5-digest-bug
    ;; Bug in CLERIC::CALCULATE-DIGEST, it returned same MD5sum for different
    ;; strings! Turns out that MD5:MD5SUM-SEQUENCE is deprecated for strings.
    ;; Use MD5:MD5SUM-STRING instead.
    (is (not (every #'=
                    (cleric::calculate-digest 1234567890 "kaka")
                    (cleric::calculate-digest 1234567891 "kaka"))))
  )
