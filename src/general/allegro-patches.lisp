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

#+(and (or :linux86 :linux86-64) (not :linux))
(pushnew :linux *features*)

;;;
;;; on Windoze, Allegro CL fails to respect *default-pathname-defaults* :-{.
;;;
#+:windows
(excl:chdir (make-pathname :device (pathname-device *load-truename*)
			   :directory '(:absolute)))

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load 
 (make-pathname :directory general-dir :name "defsystem"))

(in-package :make)

(defvar %binary-dir-name% 
    (or
     #+(and (version>= 8 2) :linux86-64 :clim) ".l2c4"
     #+(and (version>= 8 2) :linux86-64 (not :clim)) ".l2s4"
     #+(and (version>= 8 2) :linux86 :clim) ".l2cl"
     #+(and (version>= 8 2) :linux86 (not :clim)) ".l2sl"
     #+(and (version>= 8 2) :sparc :clim) ".s2cl" 
     #+(and (version>= 8 2) :sparc (not :clim)) ".s2sl"
     #+(and (version>= 8 2) :mswindows :clim) "w2cl" 
     #+(and (version>= 8 2) :mswindows (not :clim)) "w2sl"
     #+(and (version>= 8 2) :macosx :clim) ".m2cl" 
     #+(and (version>= 8 2) :macosx (not :clim)) ".m2sl"
     #+(and (version>= 8 1) :linux86-64 :clim) ".l1c4"
     #+(and (version>= 8 1) :linux86-64 (not :clim)) ".l1s4"
     #+(and (version>= 8 1) :linux86 :clim) ".l1cl"
     #+(and (version>= 8 1) :linux86 (not :clim)) ".l1sl"
     #+(and (version>= 8 1) :sparc :clim) ".s1cl" 
     #+(and (version>= 8 1) :sparc (not :clim)) ".s1sl"
     #+(and (version>= 8 1) :mswindows :clim) "w1cl" 
     #+(and (version>= 8 1) :mswindows (not :clim)) "w1sl"
     #+(and (version>= 8 1) :macosx :clim) ".m1cl" 
     #+(and (version>= 8 1) :macosx (not :clim)) ".m1sl"
     #+(and (version>= 8 0) :linux86-64 :clim) ".l8c4"
     #+(and (version>= 8 0) :linux86-64 (not :clim)) ".l8s4"
     #+(and (version>= 8 0) :linux86 :clim) ".l8cl"
     #+(and (version>= 8 0) :linux86 (not :clim)) ".l8sl"
     #+(and (version>= 8 0) :sparc :clim) ".s8cl" 
     #+(and (version>= 8 0) :sparc (not :clim)) ".s8sl"
     #+(and (version>= 8 0) :mswindows :clim) "w8cl" 
     #+(and (version>= 8 0) :mswindows (not :clim)) "w8sl"
     #+(and (version>= 8 0) :macosx :clim) ".m8cl" 
     #+(and (version>= 8 0) :macosx (not :clim)) ".m8sl"
     #+(and (version>= 7 0) :linux86-64 :clim) ".l7c4"
     #+(and (version>= 7 0) :linux86-64 (not :clim)) ".l7s4"
     #+(and (version>= 7 0) :linux86 :clim) ".l7cl"
     #+(and (version>= 7 0) :linux86 (not :clim)) ".l7sl"
     #+(and (version>= 6 0) :linux86 :clim) ".l6cl"
     #+(and (version>= 6 0) :linux86 (not :clim)) ".l6sl"
     #+(and (version>= 7 0) :sparc :clim) ".s7cl" 
     #+(and (version>= 7 0) :sparc (not :clim)) ".s7sl"
     #+(and (version>= 6 0) :sparc :clim) ".s6cl" 
     #+(and (version>= 6 0) :sparc (not :clim)) ".s6sl"
     #+(and (version>= 7 0) :mswindows :clim) "w7cl" 
     #+(and (version>= 7 0) :mswindows (not :clim)) "w7sl"
     #+(and (version>= 6 0) :mswindows :clim) "w6cl" 
     #+(and (version>= 6 0) :mswindows (not :clim)) "w6sl"
     #+(and (version>= 7 0) :macosx :clim) ".m7cl" 
     #+(and (version>= 7 0) :macosx (not :clim)) ".m7sl"
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
  #+:linux86 "linux.x86.32"
  #+:linux86-64 "linux.x86.64"
  #+:sunos4 "sunos"
  #+(and :sun :svr4) "solaris"
  #+:alpha "osf"
  #+:mswindows "windows"
  #+(and :macosx :powerpc) "macos.ppc.32"
  #-(or :prism :linux86 :linux86-64 :sunos4 (and :sun :svr4) :alpha :mswindows
	(and :macosx :powerpc))
  (error "~&loadup: unable to determine system type; see file ~
          `allegro-patches.lisp'.~%"))

;;
;; for a transition period, suppress compile-time warnings generated by the new
;; SMP API in 8.1.
;;
#+(and (version= 8 1) :smp-macros)
(setf excl::warn-smp-usage nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (let ((excl:*enable-package-locked-errors* nil))
    (shadowing-import
      '(make::defsystem make::undefsystem
        make::load-system make::compile-system make::clean-system 
        make::find-system)
      :excl)))

(defun run-process (&rest arguments)
  (apply #'excl:run-shell-command arguments))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'getenv) (symbol-function 'system:getenv))
  (setf (symbol-function 'user-name) (symbol-function 'system:user-name)))


(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-function)
  (setf (symbol-function 'run-function) 
        (symbol-function 'process-run-function)))
