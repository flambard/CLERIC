;; BERT (Binary ERlang Term)
;;
;; See http://bert-rpc.org/
;;

(in-package :bert)

(defmethod encode (object)
  (cleric:encode object :version-tag t))

(defun decode (bytes)
  (cleric:decode bytes :version-tag t))

