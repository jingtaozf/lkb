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

;;;
;;; chances are we have a modern eval-when() ... i doubt it (11-jul-94 -- oe)
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew :ansi-eval-when *features*))


;;;
;;; Such that (user-homedir-pathname) will return just the drive MCL is on.
;;;

(setf ccl::*user-homedir-pathname*
      (make-pathname
       :directory (subseq (pathname-directory (mac-default-directory)) 0 2)))

;;;
;;; load the portable defsystem() from CMU
;;;

#-:mk-defsystem
(load (make-pathname :directory general-dir :name "defsystem"))

(in-package "MAKE")

(defparameter %binary-dir-name% 
    ".masl")

(defparameter %system-binaries%
    "mac")


(defpackage :mp (:use "COMMON-LISP")
   (:intern "RUN-FUNCTION" "PROCESS-WAIT" "PROCESS-KILL" "WITH-PROCESS-LOCK"
            "MAKE-PROCESS-LOCK"))
(in-package :mp)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export 'run-function)
  (setf (symbol-function 'run-function) 
        (symbol-function 'ccl:process-run-function))
  (export 'process-wait)
  (setf (symbol-function 'process-wait) 
        (symbol-function 'ccl:process-wait))
  (export 'process-kill)
  (setf (symbol-function 'process-kill) 
        (symbol-function 'ccl:process-kill))
  (export 'with-process-lock)
  (defmacro with-process-lock ((lock) &body body) 
     `(ccl:with-lock-grabbed (,lock) ,@body))
  (export 'make-process-lock)
  (setf (symbol-function 'make-process-lock) 
        (symbol-function 'ccl:make-lock)))
