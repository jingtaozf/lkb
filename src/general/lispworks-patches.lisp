(in-package "COMMON-LISP-USER")

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :host %sys-host% :device %sys-device%
:directory general-dir :name "defsystem"))

(in-package "MAKE")

(defparameter %BINARY-DIR-NAME% 
    "lwasl"
    )

(defparameter %system-binaries%
    "win"
    )
