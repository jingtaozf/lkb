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
;;; - which most likely was to check for spurious ambiguity (equivalences).
;;;

(in-package :tsdb)

;;;
;;; for the result cache in analyze() to work properly, all filter parameters
;;; need to be made known to analyze() too.
;;;
(defparameter *filter-test* nil)

(defparameter *filter-verbose-p* t)

(defparameter *filter-mrs-relations-ratio* 1.0)

(defun result-filter (item &key (verbose *filter-verbose-p*))
  (let ((flags (make-hash-table :test #'eql)))
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
          do (push (list :sparseness size) (gethash id flags))))
    #+:mrs
    (when (smember :scope *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (let ((mrs (get-field :mrs result)))
                      (if (stringp mrs)
                        (let ((mrs (mrs::read-mrs-from-string mrs)))
                          (setf (get-field :mrs result) mrs))
                        (and (mrs::psoa-p mrs) mrs)))
          for stream = (make-string-output-stream)
          for scopes = (when mrs 
                         (let ((*standard-output* stream))
                           (multiple-value-bind (result error)
                               (ignore-errors
                                (mrs::make-scoped-mrs mrs))
                             (when error
                               (format stream "~a" error))
                             result)))
          when (and mrs (null scopes))
          do 
            (let* ((output (and verbose (get-output-stream-string stream)))
                   (output (normalize-string output))
                   (output (if (string= output "")
                             "unknown error in make-scoped-mrs()"
                             output)))
              (push (list :scope output) (gethash id flags)))))
    (unless (zerop (hash-table-count flags)) 
      (when verbose
        (format 
         t 
         "~&~%[~a] `~a' (~a)~%~%"
         (get-field :i-id item) (get-field :i-input item)
         (get-field :readings item))
        (loop
            for result in (get-field :results item)
            for id = (get-field :result-id result)
            for flag = (gethash id flags)
            when flag do
              (nconc result (list (cons :flags flag)))
              (format t "  result # ~a:~%" id)
              (loop
                  for foo in (reverse flag) do
                    (case (first foo)
                      (:sparseness
                       (format 
                        t 
                        "    sparseness: only ~a relation~p.~%"
                        (second foo) (second foo)))
                      (:scope
                       (format 
                        t 
                        "    scoping: `~a'.~%"
                        (second foo)))))))
      item)))
