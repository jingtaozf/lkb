;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lucid-patches.lisp
;;;      module: patches to Lucid CL 4.0 and 4.1
;;;     version: 0.0 -- 3-jun-1994 (experimental)
;;;  written by: oe, csli stanford --- bernd kiefer, dfki saarbruecken
;;; last update: 2-may-96
;;;  updated by: oe, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(or (and :lucid :ansi-packages))
(in-package "COMMON-LISP-USER")
#-(or (and :lucid :ansi-packages))
(in-package "USER")

;;;
;;; make sure that package names and nicknames are X3J13 compliant
;;;

(eval-when (load eval compile)
  (when (and (not (find-package "COMMON-LISP")) (find-package "LISP"))
    (rename-package "LISP" "COMMON-LISP" '("CL" "LISP")))
  (when (and (not (find-package "COMMON-LISP-USER")) (find-package "USER"))
    (rename-package "USER" "COMMON-LISP-USER" '("CL-USER" "USER")))
  (when (not (find-package "CL"))
    (rename-package "COMMON-LISP" "COMMON-LISP" '("CL"))))

;;;
;;; regex-compile() from zebu 3.x takes the Lucid 4.1 compiler in production
;;; mode into an infinite recursion |:-{.  (8-may-96  --  oe)
;;;
(proclaim '(optimize (compilation-speed 3)))

;;;
;;; Lucid 4.1 with CLIM 2.0 (beta) is totally belgiumed: `COMMON-LISP' and `LISP'
;;; are separate packages; likewise for `COMMON-LISP-USER' and `USER'.  ---
;;; unfortunately deleting the (bogus) `LISP' package makes it really unhappy.
;;; 

(eval-when (load eval compile)
  (when (not (equal (find-package "COMMON-LISP-USER") (find-package "USER")))
    (delete-package "USER")))
(rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER" "USER"))

;;;
;;; Lucid CL 4.0 has a defpackage() not affecting the compilation environment.
;;;

#+:clim-1-0
(unintern 'clim-lisp::defpackage "CLIM-LISP")
#+(and :lcl4.0 (not :lcl4.1))
(load (make-pathname :directory general-dir :name "defpackage.lisp"))

;;;
;;; CLIM 1.x (probably 2.0 as well) creates all these bogus (unbound) symbols.
;;;

#+:clim-1-0
(in-package "COMMON-LISP")
#+:clim-1-0
(eval-when (load eval compile)
  (dolist
    (symbol '(*print-lines* *print-miser-width*
              *print-pprint-dispatch* *print-right-margin*
              formatter
              copy-pprint-dispatch set-pprint-dispatch
              pprint-dispatch pprint-exit-if-list-exhausted pprint-fill
              pprint-indent pprint-linear pprint-logical-block
              pprint-newline pprint-pop pprint-tab pprint-tabular ))
    (unintern symbol "COMMON-LISP-USER")))


#+:clim-2-beta
(in-package "CLIM-LISP")
#+:clim-2-beta
(eval-when (load eval compile)
  (dolist
    (symbol '(interactive-stream-p open-stream-p input-stream-p
              with-open-stream stream-element-type close output-stream-p
              streamp define-condition))
    (unintern symbol "CLIM-LISP"))
  (unintern 'clim::interactive-stream-p "CLIM"))

(in-package "COMMON-LISP-USER")

;;;
;;; Lucid CL 4.1 for HP-UX (with :ansi-packages and :windows) has two
;;; conflicting in-packages() (in :lisp and :common-lisp respectively) and a
;;; bogus symbol in :windows.  those developers better be shot soon |:-{.
;;;                                                        (2-may -- oe@cl)
;;;

#+(and :lcl4.1 :ansi-packages :windows :hp-ux)
(eval-when (load eval compile)
  (shadowing-import 'common-lisp:in-package "WINDOWS")
  (unintern 'windows::restart "WINDOWS")
  (use-package "COMMON-LISP" "WINDOWS"))

;;; 
;;;
;;; load the pretty printing package by Richard C. Waters
;;;

#+(and :lcl4.0 (not :lcl4.1))
(eval-when (load eval compile)
  (load (make-pathname :directory general-dir :name "xp"))
  (pushnew :xp *features*))

(pushnew :no-loop-across *features*)

;;;
;;; not quite true but makes defsystem() happy (2-may-96 -- oe@cl.dfki.uni-sb.de)
;;;
#+(and :lcl4.1 :ansi-packages :hp-ux)
(pushnew :cltl2 *features*)

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defparameter %BINARY-DIR-NAME% 
  #+:sparc ".sbin"
  #+:pa ".hbin"
  #+:rios ".rbin")

;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defparameter %system-binaries%
  #+:pa "hppa"
  #+(and :sun-os (not :solaris)) "sunos"
  #+:solaris "solaris"
  #+:rios "6k"
  #-(or :pa :sun-os :rios)
  (error "~&loadup: unable to determine system type; see file ~
          `lucid-patches.lisp'.~%"))

;;;
;;; Lucid 4.0 believes in (functionp nil) --> t |:-{
;;;

(when (functionp :foo)
  (defun !functionp (beast)
    (and (functionp beast)
	 (not (symbolp beast))))
  (eval-when (load eval compile)
    (export '!functionp "MAKE")))

;;;
;;; the Allegro CL style run-shell-command() (since acl is home sweet home):
;;;

;;;
;;; for HP sucks we somehow do not want bash(1) but rather sh(1); additionally
;;; we note that HP sucks 9.x lex(1) miserably fails on the interpretation of
;;; `|' patterns while flex 2.4.6 (compiled with gcc 2.5.8 and gas 2.2.1) fails
;;; to determine the appropriate input mode (an easy patch is to enforce
;;; `yy_is_interactive' becomes true).  --- oe will want to debug this for the
;;; GNU people soon (15-jun-94).
;;;

(defun run-process (cmd &rest key-args &key (wait T) &allow-other-keys)
  (let ((login-shell (or (lcl:environment-variable "SHELL") "sh")))
    (multiple-value-bind (io-stream error-stream exit-status process-id)
	(apply #'lcl:run-program
	       "/bin/sh" :arguments (list "-c" cmd) :wait wait key-args)
      (if wait exit-status
	(values io-stream error-stream process-id exit-status)))))

(in-package "COMMON-LISP")

;;;
;;; add some X3J13 functionality missing in Lucid CL 4.x
;;;

#+(and :lcl4.0 (not :lcl4.1))
(defun map-into (result-sequence function &rest sequences)
  (loop for index below (apply #'min
                               (length result-sequence)
                               (mapcar #'length sequences))
      do (setf (elt result-sequence index)
           (apply function
                  (mapcar #'(lambda (seq) (elt seq index))
                          sequences))))
  result-sequence)

;;;
;;; now, this is a bogus symbol from ilisp |:-}
;;;

(eval-when (load eval compile)
  (when (find-package "ILISP")
    (unintern (read-from-string "ILISP::HANDLER-CASE") "ILISP")))

(eval-when (load eval compile)
  #-:ansi-packages
  (import '(lcl:handler-case lcl:ignore-errors) "COMMON-LISP")
  #+(and :lcl4.0 (not :lcl4.1))
  (import 'lcl:defpackage "COMMON-LISP"))

#-(and :lcl4.1 :ansi-packages :hp-ux)
(eval-when (load eval compile)
  (export
    '(#+(and :lcl4.0 (not :lcl4.1)) map-into
      handler-case ignore-errors
      #+(and :lcl4.0 (not :lcl4.1)) defpackage)))

(eval-when (load eval compile)
  (use-package "LUCID-COMMON-LISP" "COMMON-LISP-USER"))
