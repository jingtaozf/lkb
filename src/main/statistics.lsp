;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: statistics.lsp
;;;      module: centralized accounting: parser, generator, et al. performance
;;;     version: 0.0 (17-oct-05)
;;;  written by: oe, uio
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lkb)

(defstruct statistics
  (unifications 0)
  (copies 0)
  (subsumptions 0)
  (mtasks 0)
  (ftasks 0)
  (etasks 0)
  (stasks 0)
  (pedges 0)
  (aedges 0)
  (uedges 0)
  (equivalent 0)
  (proactive 0)
  (retroactive 0)
  (frozen 0)
  (failures 0)
  (hypotheses 0)
  (decompositions 0)
  (comparisons 0))

(defmethod print-object ((object statistics) stream)
  (format 
   stream 
   "#[S unifier <~a:~a> ~
      processor <~a:~a:~a @ ~a:~a> ~
      packing <~a: ~a+~a+~a ~a:~a> ~
      unpacking <~a:~a @ ~a> ~
      comparisons <~a>]"
   (statistics-unifications object) (statistics-copies object)
   (statistics-ftasks object)
   (statistics-etasks object) (statistics-stasks object)
   (statistics-aedges object) (statistics-pedges object)
   (statistics-subsumptions object)
   (statistics-equivalent object)
   (statistics-proactive object)
   (statistics-retroactive object)
   (statistics-frozen object)
   (statistics-failures object)
   (statistics-hypotheses object)
   (statistics-decompositions object)
   (statistics-uedges object)
   (statistics-comparisons object)))

(defparameter *statistics* (make-statistics))

(defun reset-statistics (&optional (statistics *statistics*))
  (setf (statistics-unifications statistics) 0)
  (setf (statistics-copies statistics) 0)
  (setf (statistics-subsumptions statistics) 0)
  (setf (statistics-mtasks statistics) 0)
  (setf (statistics-ftasks statistics) 0)
  (setf (statistics-etasks statistics) 0)
  (setf (statistics-stasks statistics) 0)
  (setf (statistics-pedges statistics) 0)
  (setf (statistics-aedges statistics) 0)
  (setf (statistics-uedges statistics) 0)
  (setf (statistics-equivalent statistics) 0)
  (setf (statistics-proactive statistics) 0)
  (setf (statistics-retroactive statistics) 0)
  (setf (statistics-frozen statistics) 0)
  (setf (statistics-failures statistics) 0)
  (setf (statistics-hypotheses statistics) 0)
  (setf (statistics-decompositions statistics) 0)
  (setf (statistics-comparisons statistics) 0))
