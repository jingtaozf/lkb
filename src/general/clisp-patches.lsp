;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; correct some deficiencies in lispish self-consciousness
;;;

#+:ansi-cl (pushnew :cltl2 *features*)
#+(and :unix :pc386) (pushnew :linux *features*)

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load
 (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defvar %binary-dir-name% 
  (or
   #+(and :pc386 :unix) ".lfas"
   #+(and :pc386 (not :unix)) ".wfas"
   ".cfas"))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+(and :pc386 :unix) "linux"
  #+(and :pc386 (not :unix)) "windows"
  #-(or (and :unix :pc386))
  (error "~&loadup: unable to determine system type; see file ~
          `clisp-patches.lisp'.~%"))

