(asdf:defsystem :cleric-test
  :description "Unit tests for CLERIC."
  :depends-on (:cleric :eos)
  :components
  ((:module :test
            :components
            ((:file "package")
             (:file "cleric-tests" :depends-on ("package"))
             (:file "handshake-tests" :depends-on ("package"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :cleric))))
  (asdf:load-system :cleric-test)
  (asdf:test-system :cleric-test))
