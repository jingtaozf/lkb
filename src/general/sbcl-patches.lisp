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

;;;
;;; load the portable defsystem() from CMU
;;;
#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

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
  #-(or (and (or :x86 :x86-64) :linux))
  (error "~&loadup: unable to determine system type; see file ~
          `sbcl-patches.lisp'.~%"))

;;;
;;; the Allegro CL style run-shell-command() (since acl is home sweet home):
;;;
(defun run-process (command &rest args 
                    &key (wait t) (error-output :output)
                    &allow-other-keys)
  (let* ((shell "/bin/sh")
         (process (apply #'sb-ext:run-program
                         shell (list "-c" command) 
                         :wait wait args)))
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

