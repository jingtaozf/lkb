;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: allegro-patches.lisp
;;;      module: PAGE patches to Allegro CL
;;;     version: 2.0 -- 4-jun-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 2-jun-00
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :common-lisp-user)

;;;
;;; correct some deficiencies in lispish self-consciousness
;;;

#+:allegro-v4.1 (pushnew :clim1 *features*)
#+:allegro-v4.2 (pushnew :cltl2 *features*)
#+:ansi-cl (pushnew :cltl2 *features*)

#+(and :linux86 (not :linux))
(pushnew :linux *features*)

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load 
 (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

(in-package :make)

(defvar %binary-dir-name% 
    (or
     #+(and (version>= 6 0) :linux86 :clim) ".l6cl"
     #+(and (version>= 6 0) :linux86 (not :clim)) ".l6sl"
     #+(and (version>= 6 0) :sparc :clim) ".s6cl" 
     #+(and (version>= 6 0) :sparc (not :clim)) ".s6sl"
     #+(and (version>= 6 0) :mswindows :clim) "w6cl" 
     #+(and (version>= 6 0) :mswindows (not :clim)) "w6sl"
     #+(and :sparc :clim) ".sacl" #+(and :sparc (not :clim)) ".sasl"
     #+(and :linux86 :clim) ".lacl" #+(and :linux86 (not :clim)) ".lasl"
     #+(and :alpha :clim) ".aacl" #+(and :alpha (not :clim)) ".aasl"
     #+(and :mswindows :clim) "wacl" #+(and :mswindows (not :clim)) "wasl"
     (remove-if-not #'(lambda (x) 
                        (or (alphanumericp x) 
                            (member x '(#\- #\_ #-:mswindows #\.))))
                    (substitute #\_ #\space 
                                (concatenate 'string 
                                  #-:mswindows "."
                                  (or (machine-type) "") "-" 
                                  (or (software-type) "") "-"
                                  (or (lisp-implementation-version) ""))))))

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+:prism "hppa"
  #+:linux86 "linux"
  #+:sunos4 "sunos"
  #+(and :sun :svr4) "solaris"
  #+:alpha "osf"
  #+:mswindows "windows"
  #-(or :prism :linux86 :sunos4 (and :sun :svr4) :alpha :mswindows)
  (error "~&loadup: unable to determine system type; see file ~
          `allegro-patches.lisp'.~%"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (let ((excl:*enable-package-locked-errors* nil))
    (shadowing-import
      '(make:defsystem make:undefsystem
        make:load-system make:compile-system make:clean-system)
      :excl)))

(defun run-process (&rest arguments)
  (apply #'excl:run-shell-command arguments))

(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-function)
  (setf (symbol-function 'run-function) 
        (symbol-function 'process-run-function)))
