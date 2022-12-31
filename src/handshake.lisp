;;;; Handshake between nodes

(in-package :cleric)

;;; Distribution header capability flags (from dist.hrl)
(defconstant +dflag-published+           #x0001)
(defconstant +dflag-atom-cache+          #x0002) ;; old atom cache
(defconstant +dflag-extended-references+ #x0004)
(defconstant +dflag-dist-monitor+        #x0008)
(defconstant +dflag-fun-tags+            #x0010)
(defconstant +dflag-dist-monitor-name+   #x0020)
(defconstant +dflag-hidden-atom-cache+   #x0040)
(defconstant +dflag-new-fun-tags+        #x0080)
(defconstant +dflag-extended-pids-ports+ #x0100)
(defconstant +dflag-export-ptr-tag+      #x0200)
(defconstant +dflag-bit-binaries+        #x0400)
(defconstant +dflag-new-floats+          #x0800)
(defconstant +dflag-unicode-io+          #x1000)
(defconstant +dflag-dist-hdr-atom-cache+ #x2000)
(defconstant +dflag-small-atom-tags+     #x4000)
(defconstant +dflag-utf8-atoms+         #x10000)
(defconstant +dflag-map-tag+            #x20000)
(defconstant +dflag-big-creation+       #x40000)
(defconstant +dflag-send-sender+        #x80000)
(defconstant +dflag-big-seqtrace-labels+
                                       #x100000)
(defconstant +dflag-exit-payload+      #x400000)
(defconstant +dflag-fragments+         #x800000)
(defconstant +dflag-handshake-23+     #x1000000)
(defconstant +dflag-unlink-id+        #x2000000)
(defconstant +dflag-spawn+          #x100000000)
(defconstant +dflag-name-me+        #x200000000)
(defconstant +dflag-v4-nc+          #x400000000)
(defconstant +dflag-alias+          #x800000000)
(defconstant +dflag-mandatory-25-digest+
                                   #x1000000000)

(define-condition try-again ()
  ((reason :reader reason :initarg :reason))
  (:documentation "This condition is signaled when trying to connect to a remote node that is busy."))

(defun try-again-condition-p (condition)
  (typep condition 'try-again))

(define-condition connection-closed-error (error)
  ;; END-OF-FILE
  ()
  (:documentation "This error is signaled when trying to read from a socket stream that has been closed."))

(define-condition handshake-failed-error (error)
  ((reason :reader reason :initarg :reason))
  (:documentation "This error is signaled if the handshake during connection to a remote node fails."))


;;;
;;; Client handshake
;;;

(defun perform-client-handshake (stream cookie)
  (write-message stream (make-name-message (capability-flags)
                                           (this-node-creation)
                                           (this-node)))
  (finish-output stream)
  (with-slots (status) (read-status-message stream)
    (cond
      ((or (string= status "ok")
           (string= status "ok_simultaneous"))
       (with-slots (flags challenge full-node-name)
           (read-challenge-message stream)
         (let ((new-challenge (generate-challenge)))
           (write-message stream
                          (make-challenge-reply-message
                           new-challenge (calculate-digest challenge cookie)))
           (finish-output stream)
           (with-slots (digest) (read-challenge-ack-message stream)
             (unless (equal-digests (calculate-digest new-challenge cookie)
                                    digest)
               (error 'handshake-failed-error
                      :reason "Received incorrect digest"))
             (values full-node-name flags))))) ;; Connect successful
      ((string= status "nok")
       (signal 'try-again
               :reason "Busy with other ongoing handshake"))
      ((string= status "not_allowed")
       (error 'handshake-failed-error
              :reason "Connection not allowed"))
      ((string= status "alive")
       (error 'handshake-failed-error
              :reason "Already connected."))
      (t
       (error "This should not happen (as usual).")) ) ))


;;;
;;; Server handshake
;;;

(defun perform-server-handshake (stream cookie)
  (with-slots (version flags full-node-name) (read-name-message stream)
    ;; TODO: Check if node is allowed to connect to us
    (write-message stream (make-status-message "ok"))
    (let ((challenge (generate-challenge)))
      (write-message stream (make-challenge-message (capability-flags)
                                                    challenge
                                                    (this-node-creation)
                                                    (this-node)))
      (finish-output stream)
      (with-slots (new-challenge digest) (read-challenge-reply-message stream)
        (unless (equal-digests (calculate-digest challenge cookie) digest)
          (error 'handshake-failed-error
                 :reason "Received incorrect digest"))
        (write-message stream (make-challenge-ack-message
                               (calculate-digest new-challenge cookie)))
        (finish-output stream)
        (values full-node-name flags version) )))) ;; Connect successful


;;;
;;; Helper functions
;;;

;; Erlang node capabilities: #x7FFD (all except DFLAG_ATOM_CACHE)

(defun capability-flags ()
  ;; According to distribution_handshake.txt, hidden nodes should
  ;; only set the flag EXTENDED_REFERENCES.
  ;; That does not work for me.
  (logior +dflag-handshake-23+
          +dflag-big-creation+
          +dflag-map-tag+
          +dflag-utf8-atoms+
          +dflag-bit-binaries+
          +dflag-export-ptr-tag+
          +dflag-new-fun-tags+
          +dflag-fun-tags+

          +dflag-extended-references+
          +dflag-extended-pids-ports+
          +dflag-new-floats+
          +dflag-dist-hdr-atom-cache+
          +dflag-small-atom-tags+))

(defun generate-challenge ()
  "Generate a random 32-bit unsigned integer."
  (random (expt 2 32)))

(defun equal-digests (d1 d2)
  (and (alexandria:length= d1 d2)
       (every #'= d1 d2)))
