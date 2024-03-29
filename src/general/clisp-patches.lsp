;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; correct some deficiencies in lispish self-consciousness
;;;
#+:ansi-cl (pushnew :cltl2 *features*)
#+(and :unix :pc386) (pushnew :linux *features*)

;;;
;;; apparently, contemporary versions of CLISP come with a newer, incompatible
;;; version of defsystem(); make sure we always load our own (and look into the
;;; newer version at some point in the future).                (25-may-03; oe)
;;;
(setf *features* (delete :mk-defsystem *features*))

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load
 (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defvar %binary-dir-name% 
  (or
   #+(and :pc386 :linux) ".lfas"
   #+(and :pc386 (not :unix)) ".wfas"
   ".cfas"))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;
(defvar %system-binaries%
  #+(and :pc386 :unix) "linux"
  #+(and (not :pc386) :unix) "ppc"
  #+(and :pc386 (not :unix)) "windows"
  #-(or (and :unix :pc386))
  (error "~&loadup: unable to determine system type; see file ~
          `clisp-patches.lisp'.~%"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-process)
  (setf (symbol-function 'run-process) 
    (symbol-function 'ext:run-shell-command)))
