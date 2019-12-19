(in-package "COMMON-LISP-USER")

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))

;;;
;;; apparently this function may have been there in earlier versions but is no
;;; longer defined in 4.1                                    (8-jun-00  -  oe)
;;;
#+:lispworks7.0
(defun system::lispworks-version ()
  (values 4 1))

#+:lispworks7.0
(defparameter system::*current-working-directory* *load-truename*)

#+:lispworks7.0
(unintern 'lw::hardcopy-system :lw)

#+:lispworks7.0
(import 'system:call-system-showing-output :foreign)
#+:lispworks7.0
(export 'foreign::call-system-showing-output :foreign)
 
;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defvar %binary-dir-name% 
  (or
   #+(and :linux :clim) ".llcl" #+(and :linux (not :clim)) ".llsl"
   (remove-if-not #'(lambda (x) 
                      (or (alphanumericp x) 
                          (member x '(#\- #\_ #-:mswindows #\.))))
                  (substitute #\_ #\space 
                              (concatenate 'string 
                                #-:mswindows "."
                                (or (machine-type) "") "-" 
                                (or (software-type) "") "-"
                                (or (lisp-implementation-version) ""))))))

(defparameter %system-binaries%
  (or
    #+:linux "linux"  
    #+:windows "mswindows"))

(in-package :mp)

(export '(make-process-lock with-process-lock 
          process-add-arrest-reason process-revoke-arrest-reason
          run-function))

(setf (symbol-function 'make-process-lock) (symbol-function 'make-lock))
(defmacro with-process-lock ((lock) &body body)
  `(with-lock (,lock) ,@body))

(defun process-add-arrest-reason (process reason)
  (push reason (process-arrest-reasons process)))

(defun process-revoke-arrest-reason (process reason)
  (setf (process-arrest-reasons process)
    (delete reason (process-arrest-reasons process))))

(defun run-function (name function &rest arguments)
  (apply #'process-run-function name nil function arguments))
