;;;; Constants

(in-package :cleric)

;;; Node protocol type tags
(defconstant +pass-through+ 112)


;;; Erlang data tags
(defconstant +compressed-term+    80)
(defconstant +new-float-ext+      70)
(defconstant +bit-binary-ext+     77)
(defconstant +atom-cache-ref+     82)
(defconstant +small-integer-ext+  97)
(defconstant +integer-ext+        98)
(defconstant +float-ext+          99)
(defconstant +atom-ext+          100)
(defconstant +reference-ext+     101)
(defconstant +port-ext+          102)
(defconstant +pid-ext+           103)
(defconstant +small-tuple-ext+   104)
(defconstant +large-tuple-ext+   105)
(defconstant +nil-ext+           106)
(defconstant +string-ext+        107)
(defconstant +list-ext+          108)
(defconstant +binary-ext+        109)
(defconstant +small-big-ext+     110)
(defconstant +large-big-ext+     111)
(defconstant +new-fun-ext+       112)
(defconstant +export-ext+        113)
(defconstant +new-reference-ext+ 114)
(defconstant +small-atom-ext+    115)
(defconstant +fun-ext+           117)


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


;;; Erlang protocol version
(defconstant +lowest-version-supported+ 5
  "Lowest version of the Erlang distribution protocol supported.")
(defconstant +highest-version-supported+ 5
  "Highest version of the Erlang distribution protocol supported.")
