;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: allegro-patches.lisp
;;;      module: PAGE patches to Allegro CL
;;;     version: 2.0 -- 4-jun-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 5-sep-96
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

;;;
;;; correct some deficiencies in lispish self-consciousness
;;;

#+:allegro-v4.1 (pushnew :clim1 *features*)
#+:allegro-v4.2 (pushnew :cltl2 *features*)
#+:ansi-cl (pushnew :cltl2 *features*)

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :device %sys-device% :directory general-dir :name "defsystem"))

;(load (make-pathname :directory general-dir :name "autozoom" :type "cl"))

(in-package "MAKE")

(defvar %BINARY-DIR-NAME% 
  #+(and :allegro-v4.2 :sparc) (if sys:*patches* ".fasl" ".fosl")
  #+(and (or :allegro-v4.3 :allegro-v4.3.1) :sparc) ".nasl"
  #+(and :allegro-v5.0 :sparc) ".basl"
  #+(and :allegro-v4.3 :linux86) ".lasl" ;; GN on 22/1/97
  #+(and :allegro-v4.3 :hpprism) ".pasl"
  #+(and :allegro-v4.3 :alpha) ".aasl"
  #-(or (and :allegro-v4.2 :sparc) 
        (and (or :allegro-v4.3 :allegro-v4.3.1) :sparc)
        (and :allegro-v5.0 :sparc)
        (and :allegro-v4.3 :linux86)
        (and :allegro-v4.3 :hpprism)
        (and :allegro-v4.3 :alpha)) "fasl")

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+:prism "hppa"
  #+:linux86 "linux"   ;; GN on 22/1/97
  #+:sunos4 "sunos"
  #+(and :sun :svr4) "solaris"
  #+:alpha "alpha"
  #+:mswindows "mswindows"
  #-(or :prism :sunos (and :sun :svr4) :alpha :linux86)
  (error "~&loadup: unable to determine system type; see file ~
          `allegro-patches.lisp'.~%"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (let ((excl:*enable-package-locked-errors* nil))
    (shadowing-import
      '(make:defsystem make:undefsystem
        make:load-system make:compile-system)
      "EXCL")))

(defun run-process (&rest arguments)
  (apply #'excl:run-shell-command arguments))
