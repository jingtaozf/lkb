;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

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
   "TSDB" "READ-MEM"))                

#+:page
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (unless (find-package "CSLI")
    (make-package "CSLI" :use (list "COMMON-LISP" "MAKE")))
  (unless (find-package "CSLI-UNIFY")
    (make-package "CSLI-UNIFY" :use (list "COMMON-LISP" "MAKE")))
  (unless (find-class (intern "FS" :csli-unify) nil)
    (defstruct (intern "FS" :csli-unify))))
  


