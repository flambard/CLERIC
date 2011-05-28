(defpackage :bert-system
  (:use :cl))

(in-package :bert-system)

(asdf:defsystem :bert
  :description "BERT serializer."
  :author "Markus Flambard <mflambard@common-lisp.net>"
  :version "0.1.0"
  :license "MIT License"
  :depends-on (:cleric :alexandria)
  :components
  ((:module :src
            :components
            ((:file "bert"))
            )))
