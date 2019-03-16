;;; -*- Mode: LISP; Package: MAKE -*-

(in-package "COMMON-LISP-USER")

;;;
;;; make the SBCL compiler just a little less verbose
;;;
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions style-warning))

;;;
;;; chances are we have a modern eval-when() ...
;;;
(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;;
(sb-ext:unlock-package :common-lisp)

;;
;; we don't want gc to fire too frequently
(setf (sb-ext:BYTES-CONSED-BETWEEN-GCS) 200000000)

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
   #+(and :x86 (not :linux)) ".wuf"
   ".cuf"))
   
;;;
;;; determine the system type (in terms of hardware and os) in order to set
;;; `bin-dir' (the location of external platform-specific executables)
;;; accordingly (6-feb-96 -- oe@csli)
;;;

(defvar %system-binaries%
  #+(and :x86 :linux) "linux.x86.32"
  #+(and :x86-64 :linux) "linux.x86.64"
  #+:win32 "windows"
  #-(or (and :x86 :linux) (and :x86-64 :linux) :win32)
  (error "~&loadup: unable to determine system type; see file ~
          `sbcl-patches.lisp'.~%"))

;;;
;;; the Allegro CL style run-shell-command() (since acl is home sweet home):
;;;
;; [bmw] broken: &rest swallows up ALL ARGUMENTS
(defun run-process (command &rest args 
                    ;;&key (wait t) (error-output :output)
                    ;;&allow-other-keys
		    )
  (let* ((wait (second (member :wait args)))
	 (shell "/bin/sh")
         (process (apply #'sb-ext:run-program
                         shell (list "-c" command) 
                         args)))
    (when (sb-ext:process-p process)
      (if wait 
        (sb-ext:process-exit-code process)
        (let ((stdout 
	       (if (sb-ext:process-input process)
		   (make-two-way-stream 
		    (sb-ext:process-output process)
		    (sb-ext:process-input process))
		 (sb-ext:process-output process)))
              (stderr (sb-ext:process-error process))
              (pid (sb-ext:process-pid process)))
          (values stdout stderr pid))))))

(defun getenv (name) 
  (sb-ext:posix-getenv name))

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

;; PPCRE sets ppcre:*regex-char-code-limit* to the constant CHAR-CODE-LIMIT
;; under SBCL, CHAR-CODE-LIMIT is 1114112
;;  - this is way too high for almost any application
;;    (results in massive regex scanners)
;; under Allegro, CHAR-CODE-LIMIT is 65536
;;  - still excessively high, but usable
;; SO we set ppcre:*regex-char-code-limit* to 65536 for now...
(setf ppcre:*regex-char-code-limit* 65536)
