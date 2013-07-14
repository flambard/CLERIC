(in-package :cleric-test)

(in-suite handshake)

(test name-message
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-name-message 5 0 "test@example.com"))))
         (read-name-message in))
       'name))
  )

(test status-message
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-status-message "ok"))))
         (read-status-message in))
       'status))
  )

(test challenge-message
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-challenge-message 5 0 8236487 "test@example.com"))))
         (read-challenge-message in))
       'challenge))
  )

(test challenge-reply-message
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-challenge-reply-message 8236487 #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))))
         (read-challenge-reply-message in))
       'challenge-reply))
  )

(test challenge-ack-message
  (is (typep
       (with-input-from-sequence
           (in (with-output-to-sequence (out)
                 (write-message out (make-challenge-ack-message #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))))
         (read-challenge-ack-message in))
       'challenge-ack))
  )
