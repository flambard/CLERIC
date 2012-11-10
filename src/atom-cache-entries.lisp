;;;; Atom cache entries

(in-package :cleric)

(defun make-atom-cache-entries ()
  (make-array 256
              :fill-pointer 0
              :adjustable nil
              :initial-element t))


(defun make-atom-cache-entry (atom atom-cache-entries)
  (let ((pos (atom-cache-entries-position atom atom-cache-entries)))
    (if pos
        pos ;; The atom is already in the atom cache entries.
        (let ((ac-pos (atom-cache-location-of atom *atom-cache*)))
          (if ac-pos
              ;; Add the atom to the atom cache entries and return the index.
              (atom-cache-entries-add-entry atom
                                            nil
                                            (car ac-pos)
                                            (cdr ac-pos)
                                            atom-cache-entries)
              ;; The atom is not present in atom cache. Let's add it.
              (let ((free-pos (atom-cache-find-free *atom-cache*)))
                (when free-pos
                  (let ((segment (car free-pos))
                        (internal (cdr free-pos)))
                    (atom-cache-add atom *atom-cache* segment internal)
                    (atom-cache-entries-add-entry atom
                                                  t
                                                  segment
                                                  internal
                                                  atom-cache-entries) ))) )))))


(defun atom-cache-entries-position (atom atom-cache-entries)
  (position atom atom-cache-entries :key #'car :test #'eq))

(defun atom-cache-entries-add-entry (atom new-p s-index internal-s-index entries)
  (when (vector-push (list atom new-p s-index internal-s-index) entries)
    (1- (fill-pointer entries))))
