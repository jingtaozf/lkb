;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

(setf *load-verbose* t)

;;;
;;; make the SBCL compiler just a little less verbose
;;;
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions style-warning))

;;;
;;; we have a modern eval-when()
;;;
(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;; JAC 22-Nov-2018: commented out the following dangerous call
;;; (sb-ext:unlock-package :common-lisp)

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

;; also load "another system definition facility"
(require :asdf)

(defparameter src-home (merge-pathnames "src/" sys-home))

;; tell asdf where to find its system definitions
(setf asdf:*central-registry*
  (list
   (merge-pathnames (pathname "asdf/source/puri/") src-home)
   (merge-pathnames (pathname "asdf/source/cl-ppcre/") src-home)
   (merge-pathnames (pathname "asdf/source/acl-compat/") src-home)
   ))

(require :cl-ppcre)
(pushnew :ppcre *features*) ; as in systems/ppcre.system
(require :puri)
(require :acl-compat)

;; this hack ensures sbcl automatically recompiles any out-of-date
;; fasl files (rather than choking, which is the default behaviour)
(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (sb-ext:invalid-fasl ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

(in-package "MAKE")

(defvar %binary-dir-name% 
  (or
   #+(and :x86 :linux) ".luf"
   #+(and :x86-64 :darwin) ".duf"
   #+:win32 ".wuf"
   ".cuf"))
   
;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  (or
   #+(and :x86 :linux) "linux.x86.32"
   #+(and :x86-64 :linux) "linux.x86.64"
   #+(and :x86-64 :darwin) "darwin.x86.64"
   #+:win32 "windows"
   (error "~&loadup: unable to determine system type; see file ~
           `sbcl-patches.lisp'.~%")))

;;;
;;; the Allegro CL style run-shell-command() (since acl is home sweet home):
;;;

(defun run-process (command &rest args 
                    &key (wait t) &allow-other-keys)
  (let* ((shell "/bin/sh")
         (sbcl-args
           (loop for (key val) on args by #'cddr
             do
             (setq key
               (case key
                 (:error-output :error)
                 (:if-error-output-exists :if-error-exists)
                 (t key)))
             (when (equal val "/dev/null") (setq val nil))
             unless (eq key :if-error-output-does-not-exist)
             nconc (list key val)))
         (process (apply #'sb-ext:run-program
                         shell (list "-c" command) 
                         :wait wait sbcl-args)))
    (when (sb-ext:process-p process)
      (if wait 
        (sb-ext:process-exit-code process)
        (let ((stdout (make-two-way-stream 
                       (sb-ext:process-output process)
                       (sb-ext:process-input process)))
              (stderr (sb-ext:process-error process))
              (pid (sb-ext:process-pid process)))
          (values stdout stderr pid))))))

(defun getenv (name) 
  (sb-ext:posix-getenv name))

;;;
;;; Customise memory management for typical LKB grammar development
;;;

(setf (sb-ext:bytes-consed-between-gcs) 150000000) ; don't GC too often

(defun set-lkb-memory-management-parameters ()
  ;; These parameters assume a generous maximum memory size: it doesn't seem unreasonable
  ;; to specify large values for --dynamic-space-size (32000 for 64 bit and 2540 for 32 bit)
  ;; on LKB startup, or hardwired into a binary through (sb-ext:save-lisp-and-die ...
  ;; :save-runtime-options t)
  ;;
  ;; In the LKB there are essentially two distinct lifecycles for Lisp data: short-lived
  ;; (e.g. produced during the parse of a single sentence and then discarded), and long-lived
  ;; (e.g. the grammar and lexicon and their associated indexes). Use generations 0 and 1
  ;; for the former, and generation 2 (only) for the latter - do this by preventing promotion
  ;; out of generation 2
  (setf (sb-ext:generation-number-of-gcs-before-promotion 2) (1- (expt 2 31)))
  ;; ensure generation 2 is GCed as soon as any significant amount of new data is promoted
  ;; to it: for example in the case where the grammar is reloaded, we want the old grammar
  ;; to be GCed asap
  (setf (sb-ext:generation-minimum-age-before-gc 2) 0.1D0)
  ;; don't GC too often - after 300MB/150MB of new allocation
  (setf (sb-ext:bytes-consed-between-gcs) #+:x86-64 300000000 #-:x86-64 150000000))


;;; Turn off the option for the Lisp reader to normalize symbols to Normalization Form KC
;;; (NFKC) - otherwise trouble with grammars such as Zhong

(setf (sb-ext:readtable-normalization *readtable*) nil)


;;; A variant toplevel read function that avoids unexpected exits by requiring 3 Control-D's
;;; in a row (like Clozure CL's default behaviour)

(let ((eof-count 0))
  (defun sbcl-repl-read-form (in out)
    (declare (type stream in out) (ignore out))
    ;; Based on repl-read-form-fun in SBCL src/code/toplevel.lisp
    (when *read-suppress*
      (warn "Setting *READ-SUPPRESS* to NIL to restore toplevel usability.")
      (setf *read-suppress* nil))
    (let* ((eof-marker (cons nil nil))
           (form (read in nil eof-marker)))
      (cond
        ((not (eq form eof-marker))
          (setq eof-count 0)
          form)
        ((>= eof-count 2)
          (sb-ext:exit))
        (t
          (incf eof-count)
          (funcall sb-int:*repl-prompt-fun* *standard-output*)
          (force-output *standard-output*)
          (sbcl-repl-read-form in out))))))

;;; Make entry to the Lisp debugger a bit less verbose - users will either already know
;;; "Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL."
;;; or not find this information relevant

(setq sb-debug:*debug-beginner-help-p* nil)


;;
;; set up :multiprocessing package
;;

(defpackage :multiprocesing
  (:use #:common-lisp #:acl-compat.mp)
  (:nicknames #:mp)
  (:export 
   #:*current-process*         ;*
   #:process-kill              ;*
   #:process-preset            ;*
   #:process-name              ;*

   #:process-wait-function
   #:process-run-reasons 
   #:process-arrest-reasons
   #:process-whostate
   #:without-interrupts
   #:process-wait
   #:process-enable
   #:process-disable
   #:process-reset
   #:process-interrupt

   #:process-run-function      ;*
   #:process-property-list     ;*
   #:without-scheduling        ;*
   #:process-allow-schedule    ;*
   #:make-process              ;*
   #:process-add-run-reason    ;*
   #:process-revoke-run-reason ;*
   #:process-add-arrest-reason    ;*
   #:process-revoke-arrest-reason ;*
   #:process-allow-schedule    ;*
   #:with-timeout              ;*
   #:make-process-lock         ;*
   #:with-process-lock         ;*
   #:process-lock
   #:process-unlock

   #:current-process
   #:process-name-to-process
   #:process-wait-with-timeout
   #:wait-for-input-available
   #:process-active-p
   ))

(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-function)
  (setf (symbol-function 'run-function) 
        (symbol-function 'process-run-function)))

;;
;; set up :excl package
;;

(defpackage :excl
  (:use #:common-lisp #:acl-compat.excl)
  (:export
   #:if*
   #:*initial-terminal-io*
   #:*cl-default-special-bindings*
   #:filesys-size
   #:filesys-write-date
   #:stream-input-fn
   #:match-regexp
   #:compile-regexp
   #:*current-case-mode*
   #:intern*
   #:filesys-type
   #:errorset
   #:atomically
   #:fast
   #:without-package-locks
   #:fixnump
   ))

;; Fake definitions for unimplemented functions / packages

(defpackage :silica (:use :common-lisp)
  (:export "INHIBIT-UPDATING-SCROLL-BARS"))
(in-package :silica)

(defmacro inhibit-updating-scroll-bars (&body body)
  `(clim:changing-space-requirements () ,@body))

(defpackage :lep (:use :common-lisp)
  (:export "LEP-IS-RUNNING"))
(in-package :lep)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun lep-is-running () nil)
  (defun eval-in-emacs (str)
    (declare (ignore str))
    nil))

;; Below is also in ppcre/PATCHES.lisp, but would not get loaded since we require cl-ppcre above

;; PPCRE sets ppcre:*regex-char-code-limit* to the constant CHAR-CODE-LIMIT
;; under SBCL, CHAR-CODE-LIMIT is 1114112
;;  - this is way too high for almost any application
;;    (results in massive regex scanners)
;; under Allegro, CHAR-CODE-LIMIT is 65536
;;  - still excessively high, but usable
;; SO we set ppcre:*regex-char-code-limit* to 65536 for now...
(setf ppcre:*regex-char-code-limit* 65536)

