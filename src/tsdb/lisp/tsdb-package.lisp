;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

(defpackage "TSDB"
  (:use "COMMON-LISP" "MAKE" #+:pvm "PVM"
        #+:allegro "FOREIGN-FUNCTIONS" #+:allegro "MULTIPROCESSING")
  (:nicknames "TSNLP")
  (:export
   "*TSDB-HOME*" "*TSDB-DATA*" "*TSDB-IO*" 
   "*TSDB-TREES-HOOK*" "*TSDB-SEMANTIX-HOOK*"
   "TSDB"))                

#+:page
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (unless (find-package "CSLI")
    (make-package "CSLI" :use (list "COMMON-LISP" "MAKE")))
  (unless (find-package "CSLI-UNIFY")
    (make-package "CSLI-UNIFY" :use (list "COMMON-LISP" "MAKE")))
  (unless (find-class (intern "FS" :csli-unify) nil)
    (defstruct (intern "FS" :csli-unify))))
  


