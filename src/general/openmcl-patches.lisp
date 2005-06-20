;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: mcl-patches.lisp
;;;      module: DISCO loadup environment
;;;     version: 2.0 -- 4-jun-1994
;;;  written by: bernd kiefer, dfki saarbruecken
;;; last update: 6-jun-94
;;;  updated by: oe, dfki saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

(setf *load-verbose* t)

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;
(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;; correct some deficiencies in lispish self-consciousness
;;;
#+:linux-host
(pushnew :linux *features*)

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

;;;
;;; i fear quite a bit of code assumes :mcl really is MCL (not OpenMCL); for
;;; right now, avoid confusion (at the risk of breaking third-party libraries
;;; that might not distinguish between the two MCLs).         (16-feb-05; oe)
;;;
(setf *features* (remove :mcl *features*))

(in-package :make)

(defparameter %binary-dir-name% 
  (or
   #+(and :powerpc :linux) ".pfsl"
   #+(and :powerpc :darwin) ".mfsl"
   ".masl"))

(defparameter %system-binaries% 
  #+(and :powerpc :linux) "linux.ppc.32"
  #+(and :powerpc :darwin) "macos.ppc.32")

(in-package :cl-user)

;;;
;;; the Allegro CL style run-shell-command() (since acl is home sweet home):
;;;
(defun run-process (command &rest args 
                    &key (wait t)
                    &allow-other-keys)
  (let* ((shell "/bin/sh")
         (process (apply #'ccl:run-program
                         shell (list "-c" command) 
                         :wait wait args)))
    (when (ccl::external-process-p process)
      (if wait 
        (multiple-value-bind (status exit)
          (ccl:external-process-status process)
          (when (eq status :running)
            (error 
             "run-process(): ~
              non-null :wait argument, but process still running --- weird"))
          exit)
        (let ((stdout (make-two-way-stream 
                       (ccl:external-process-output-stream process)
                       (ccl:external-process-input-stream process)))
              (stderr (ccl:external-process-error-stream process))
              (pid (ccl:external-process-id process)))
          (values stdout stderr pid))))))

(defun getenv (name) 
  (ccl::getenv name))

(defun user-name ()
  (getenv "USER"))

(defpackage :mp (:use :common-lisp)
  (:use :cl)
  (:export "RUN-FUNCTION" "PROCESS-WAIT" "PROCESS-KILL"
	   "WITH-PROCESS-LOCK" "MAKE-PROCESS-LOCK"))

(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'run-function) 
    (symbol-function 'ccl:process-run-function))
  (setf (symbol-function 'process-wait) 
    (symbol-function 'ccl:process-wait))
  (setf (symbol-function 'process-kill) 
    (symbol-function 'ccl:process-kill))
  (defmacro with-process-lock ((lock) &body body) 
    `(ccl:with-lock-grabbed (,lock) ,@body))
  (setf (symbol-function 'make-process-lock) 
    (symbol-function 'ccl:make-lock))
  (import 'ccl:*current-process*))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '*current-process*))

(pushnew :multiprocessing *features*)

(defpackage :socket (:use :common-lisp)
  (:use :cl)
  (:export "MAKE-SOCKET" "SHUTDOWN" "ACCEPT-CONNECTION"
	   "REMOTE-HOST" "REMOTE-PORT" "IPADDR-TO-HOSTNAME"))
(in-package :socket)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'make-socket) 
    (symbol-function 'ccl:make-socket))
  (setf (symbol-function 'shutdown) 
    (symbol-function 'ccl:shutdown))
  (setf (symbol-function 'accept-connection) 
    (symbol-function 'ccl:accept-connection))
  (setf (symbol-function 'remote-host) 
    (symbol-function 'ccl:remote-host))
  (setf (symbol-function 'remote-port) 
    (symbol-function 'ccl:remote-port))
  (setf (symbol-function 'ipaddr-to-hostname) 
    (symbol-function 'ccl:ipaddr-to-hostname)))

(in-package :cl-user)

