;;; -*- Mode: LISP; Package: MAKE -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: openmcl-patches.lisp
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

(in-package :common-lisp-user)

(setf *load-verbose* t)

;;;
;;; chances are we have a modern eval-when()
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
  #+(and :powerpc :darwin) "macos.ppc.32"
  #+(and :x86-64 :darwin) "macos.x86.64")

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

(eval-when (:execute :load-toplevel :compile-toplevel)
  (import 'ccl:getenv))

(defun user-name ()
  (ccl:getenv "USER"))

;;;
;;; Customise memory management for typical LKB grammar development
;;;

(defun set-lkb-memory-management-parameters ()
  (ccl:configure-egc 180000 180000 180000) ; 180MB
  (ccl:set-lisp-heap-gc-threshold 300000000) ; 300MB
  (ccl:gc-retain-pages t))

;;
;; set up :multiprocessing package
;;

(defpackage :mp (:use :common-lisp)
  (:export "RUN-FUNCTION" "PROCESS-WAIT" "PROCESS-KILL" "PROCESS-WAIT-WITH-TIMEOUT"
	   "PROCESS-ADD-ARREST-REASON" "PROCESS-REVOKE-ARREST-REASON"
	   "WITH-PROCESS-LOCK" "MAKE-PROCESS-LOCK"))
(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function 'run-function) 
    (symbol-function 'ccl:process-run-function))
  (setf (symbol-function 'process-wait) 
    (symbol-function 'ccl:process-wait))
  (setf (symbol-function 'process-kill) 
    (symbol-function 'ccl:process-kill))
  (setf (symbol-function 'process-wait-with-timeout)
    (symbol-function 'ccl:process-wait-with-timeout))
  (defun process-add-arrest-reason (process reason)
    (declare (ignore reason))
    (ccl:process-suspend process))
  (defun process-revoke-arrest-reason (process reason)
    (declare (ignore reason))
    (ccl:process-resume process))
  (defmacro with-process-lock ((lock) &body body) 
    `(ccl:with-lock-grabbed (,lock) ,@body))
  (setf (symbol-function 'make-process-lock) 
    (symbol-function 'ccl:make-lock))
  (import 'ccl:*current-process*))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '*current-process*))

(pushnew :multiprocessing *features*)

(defpackage :socket (:use :common-lisp)
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

;; Fake definitions for unimplemented functions / packages

(defpackage :silica (:use :common-lisp)
  (:export "INHIBIT-UPDATING-SCROLL-BARS"))
(in-package :silica)

(defmacro inhibit-updating-scroll-bars (&body body) `(progn ,@body))

(defpackage :lep (:use :common-lisp)
  (:export "LEP-IS-RUNNING"))
(in-package :lep)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun lep-is-running () nil)
  (defun eval-in-emacs (str)
    (declare (ignore str))
    nil))

;;; Patch zpb-ttf-1.0.3 (open-font-loader-from-file in font-loader-interface.lisp) for CCL, so
;;; that Truetype fonts can be accessed from multiple threads. Otherwise get errors such as:
;;;
;;; Error: Stream #<BASIC-FILE-BINARY-INPUT-STREAM
;;; ("/opt/X11/share/fonts/TTF/VeraBd.ttf"/6 ISO-8859-1) ...> is private to #<PROCESS ...>
;;; While executing: CCL::CHECK-IOBLOCK-OWNER

(eval-when (:execute :load-toplevel :compile-toplevel)
  (when (find-package :zpb-ttf) (pushnew :zpb-ttf *features*)))

#+:zpb-ttf
(in-package :zpb-ttf)

#+:zpb-ttf
(let (#+ccl (zpb-lock (ccl:make-read-write-lock)))

(defun arrange-finalization (object stream)
  #+ccl (declare (ignore stream))
  (flet ((quietly-close (&optional object)
           (declare (ignore object))
           (ignore-errors (close stream))))
    #+sbcl
    (sb-ext:finalize object #'quietly-close)
    #+cmucl
    (ext:finalize object #'quietly-close)
    #+clisp
    (ext:finalize object #'quietly-close)
    #+allegro
    (excl:schedule-finalization object #'quietly-close)
    #+ccl
    (ccl:terminate-when-unreachable object)))

#+ccl
(defmethod ccl:terminate ((x font-loader))
  (ignore-errors (close (slot-value x 'input-stream))))

(defun open-font-loader-from-file (thing)
  (ccl:with-read-lock (zpb-lock)
    (let ((stream (open thing
                        :direction :input
                        #+ccl :sharing #+ccl :lock ; <- vital addition for CCL
                        :element-type '(unsigned-byte 8))))
      (let ((font-loader (open-font-loader-from-stream stream)))
        (arrange-finalization font-loader stream)
        font-loader))))
)


