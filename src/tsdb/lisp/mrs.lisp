;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

;;;
;;; various tests for MRS well-formedness, used in [incr tsdb()] browsing to
;;; identify candidate problems.  for the time being, we foresee the following:
;;; 
;;; - fewer relations than input tokens (theoretically possible with expletives
;;;   or semantically vacuous prepositions, say, but relatively unlikely);
;;; - scope resolution errors (typically indicating handle constraint errors);
;;; - connectivity (often indicating a failure to bind individual arguments);
;;; - occurence of specific relations, e.g. `fragment_rel';
;;; - something more i thought of en route back from Bergen (17-oct-03).
;;;

(in-package :tsdb)

;;;
;;; for the result cache in analyze() to work properly, all filter parameters
;;; need to be made known to analyze() too.
;;;
(defparameter *filter-test* nil)

(defparameter *filter-mrs-relations-ratio* 1.0)

(defun mrs-result-filter (item)
  (let (flags)
    #+:mrs
    (when (and (smember :sparseness *filter-test*)
               (numberp *filter-mrs-relations-ratio*)
               (not (zerop *filter-mrs-relations-ratio*)))
      (loop
          with length = (get-field :i-length item)
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (let ((mrs (get-field :mrs result)))
                      (if (stringp mrs)
                        (let ((mrs (mrs::read-mrs-from-string mrs)))
                          (setf (get-field :mrs result) mrs))
                        (and (mrs::psoa-p mrs) mrs)))
          for size = (and mrs (length (mrs::psoa-liszt mrs)))
          when (and (numberp length) (not (zerop length))
                    (or (null size) (zerop size)
                        (< (/ size length) *filter-mrs-relations-ratio*)))
          do (push (cons id :sparseness) flags)))
    (when flags (values item flags))))
