;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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
  #+:debug
  (format 
   t 
   "~%~%[~a] `~a'.~%"
   (get-field :i-id item) (get-field :i-input item))
  (let ((flags (make-hash-table :test #'eql)))
    #+:mrs
    (loop
        for result in (get-field :results item)
	for id = (get-field :result-id result)
        for mrs = (get-field :mrs result)
        for edge = (get-field :edge result)
        for derivation = (get-field :derivation result)
	when (and derivation (smember :derivation *filter-test*)) do 
	  (multiple-value-bind (foo failure) (reconstruct derivation)
	    (if foo
	      (nconc result (acons :edge (setf edge foo) nil))
	      (push (list :derivation failure) (gethash id flags))))
        when (and (or (null mrs) (and (stringp mrs) (string= mrs "")))
                  (null edge)) 
        do 
          (setf edge (ignore-errors (reconstruct derivation)))
          (when edge (nconc result (acons :edge edge nil)))
        when (and (null mrs) edge)
        do
          (setf mrs (mrs::extract-mrs edge))
          (when (mrs:psoa-p mrs) (nconc result (acons :mrs mrs nil)))
        when (and (stringp mrs) (string= mrs "") edge)
        do
          (setf mrs (mrs::extract-mrs edge))
          (when (mrs:psoa-p mrs) (setf (get-field :mrs result) mrs))
        when (stringp mrs)
        do
          (setf (get-field :mrs result) (mrs::read-mrs-from-string mrs)))
    #+:mrs
    (when (and (smember :sparseness *filter-test*)
               (numberp *filter-mrs-relations-ratio*)
               (not (zerop *filter-mrs-relations-ratio*)))
      (loop
          with length = (get-field :i-length item)
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for size = (and mrs (length (mrs::psoa-liszt mrs)))
          when (and (numberp length) (not (zerop length))
                    (or (null size) (zerop size)
                        (< (/ size length) *filter-mrs-relations-ratio*)))
          do (push (list :sparseness size) (gethash id flags))))
    #+:mrs
    (when (smember :syntax *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for nulls = nil
          when mrs do
            (loop
                for ep in (mrs:psoa-liszt mrs)
                nconc
                  (loop
                      for role in (mrs:rel-flist ep)
                      for value = (mrs:fvpair-value role)
                      when (or (null value) 
                               (and (mrs:var-p value) 
                                    (null (mrs:var-type value))))
                      do (pushnew ep nulls)
                      else when (mrs:var-p value) do
                        (loop
                            for extra in (mrs:var-extra value)
                            when (null (mrs::extrapair-value extra))
                            do (pushnew value nulls))))
          when (or (null mrs) nulls)
          do 
            (let ((output (if nulls
                            (format
                             nil
                             "dubious ~{`~(~a~)'~^, ~}"
                             (loop
                                 for null in nulls
                                 when (mrs:var-p null)
                                 collect (mrs::var-string null)
                                 else when (mrs::rel-p null)
                                 collect (mrs:rel-pred null)))
                            "reader failure")))
              (push (list :syntax output) (gethash id flags)))))
    #+:mrs
    (when (or (smember :ascope *filter-test*) (smember :cscope *filter-test*)
              (smember :uscope *filter-test*) (smember :unet *filter-test*))
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for cheap = (when (and mrs (smember :cscope *filter-test*))
                        (let* ((stream (make-string-output-stream))
                               (*standard-output* stream)
                               result error)
                          (multiple-value-setq (result error)
                            (ignore-errors (mrs::produce-one-scope mrs)))
                          (when error 
                            (push 
                             (list :cscope (format nil "~a" error))
                             (gethash id flags)))
                          (when verbose
                            (let* ((output 
                                    (get-output-stream-string stream))
                                   (output (normalize-string output)))
                              (unless (string= output "")
                                (push
                                 (list :cscope output)
                                 (gethash id flags)))))
                          (unless (mrs:psoa-p result)
                            (push
                             (list :cscope "no cheap scope") 
                             (gethash id flags)))
                          result))
          for bindings = (when cheap 
                           (let* ((stream (make-string-output-stream))
                                  (*standard-output* stream)
                                  (mrs::*scoping-partial-results-p* nil)
                                  result error)
                             (multiple-value-setq (result error)
                               (ignore-errors (mrs::make-scoped-mrs cheap)))
                             (when error 
                               (push 
                                (list :cscope (format nil "~a" error))
                                (gethash id flags))
                               (push
                                (list :cscope "no valid cheap scope") 
                                (gethash id flags)))
                             (when verbose
                               (let* ((output 
                                       (get-output-stream-string stream))
                                      (output (normalize-string output)))
                                 (unless (string= output "")
                                   (push
                                    (list :cscope output)
                                    (gethash id flags)))))
                             result))
          for scopes = (when (and mrs (smember :ascope *filter-test*))
                         (let* ((stream (make-string-output-stream))
                                (*standard-output* stream)
                                result error)
                           (multiple-value-setq (result error)
                             (ignore-errors (mrs::make-scoped-mrs mrs)))
                           (when error 
                             (push 
                              (list :ascope (format nil "~a" error))
                              (gethash id flags)))
                           (when verbose
                             (let* ((output 
                                     (get-output-stream-string stream))
                                    (output (normalize-string output)))
                               (unless (string= output "")
                                 (push
                                  (list :ascope output)
                                  (gethash id flags)))))
                           result))
          for uscopes = (when (and mrs (smember :uscope *filter-test*))
                          (let* ((stream (make-string-output-stream))
                                 (*standard-output* stream)
                                 result error)
                            (multiple-value-setq (result error)
                              (ignore-errors (mt:utool-process
                                              mrs :action :solve)))
                            (when error 
                              (push 
                               (list :uscope (format nil "~a" error))
                               (gethash id flags)))
                            (when verbose
                              (let* ((output 
                                      (get-output-stream-string stream))
                                     (output (normalize-string output)))
                                (unless (string= output "")
                                  (push (list :uscope output)
                                        (gethash id flags)))))
                            result))
          when (smember :unet *filter-test*)
          do
            (let* ((stream (make-string-output-stream))
                   (*standard-output* stream)
                   result error)
              (multiple-value-setq (result error)
                (ignore-errors (mt:utool-net-p mrs)))
              (setf result result)
              (when error 
                (push 
                 (list :unet (format nil "~a" error))
                 (gethash id flags)))
              (when verbose
                (let* ((output 
                        (get-output-stream-string stream))
                       (output (normalize-string output)))
                  (unless (string= output "")
                    (push (list :unet output)
                          (gethash id flags))))))
          when bindings do
            (if (rest bindings)
              (push
               (list :cscope (format nil "~a cheap scopes" (length bindings)))
               (gethash id flags))
              #+:null
              (when (mrs::extra-bindings-p (first bindings))
                (push
                 (list :cscope "incomplete cheap scope")
                 (gethash id flags))))

          unless (or (not (smember :ascope *filter-test*)) (and mrs scopes))
          do (push (list :ascope "no valid scope(s)") (gethash id flags))

          unless (or (not (smember :uscope *filter-test*)) (and mrs uscopes))
          do (push (list :uscope "no valid scope(s)") (gethash id flags))

          when (and (smember :ascope *filter-test*)
                    (smember :uscope *filter-test*)
                    (not (assoc :ascope (gethash id flags)))
                    (not (assoc :uscope (gethash id flags)))
                    (not (= (length scopes) (length uscopes))))
          do (push (list
                    :scope
                    (format
                     nil
                     "~a vs. ~a solutions"
                     (length scopes) (length uscopes)))
                   (gethash id flags))))
    #+:mrs
    (when (smember :connectivity *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for eds = (mrs:eds-convert-psoa mrs)
          when (mrs:eds-fragmented-p eds)
          do 
            (let* ((fragments
                    (loop
                        for ed in (mrs::eds-relations eds)
                        when (smember :fragmented (mrs::ed-status ed))
                        collect (mrs::ed-linked-predicate ed :lnkp t)))
                   (output (format nil "~{~(~a~)~^, ~}" fragments)))
              (push (list :connectivity output) (gethash id flags)))))
    #+:mrs
    (when (smember :cycle *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for eds = (mrs:eds-convert-psoa mrs)
          when (mrs:eds-cyclic-p eds)
          do 
            (push (list :cycle "circular EDS") (gethash id flags))))
    #+:mrs
    (when (smember :fragmentation *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for fragments = (when mrs (mt:fragmentp mrs))
          when fragments
          do 
            (let ((output (format nil "~a fragment~p" fragments fragments)))
              (push (list :fragmentation output) (gethash id flags)))))
    #+:mrs
    (when (smember :lnk *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for bogus = nil
          when mrs do
            (loop
                for ep in (mrs:psoa-liszt mrs)
                unless (mrs::rel-lnk ep)
                do (push (mrs:rel-pred ep) bogus))
          when bogus
          do 
            (let ((output (format nil "~{`~(~a~)'~^, ~}" bogus)))
              (push (list :lnk output) (gethash id flags)))))
    #+:mt
    (when (smember :semi *filter-test*)
      (loop
          for result in (get-field :results item)
          for id = (get-field :result-id result)
          for mrs = (get-field :mrs result)
          for invalid = (when mrs (mt:test-semi-compliance mrs))
          when invalid
          do 
            (let* ((*package* (find-package mrs:*mrs-package*))
                   (unknown
                    (loop for foo in invalid collect (mrs::rel-pred foo)))
                   (output
                    (if mrs:*normalize-predicates-p*
                      (format
                       nil
                       "~@[invalid SEM-I predicates: ~{|~(~a~)|~^, ~}~]"
                       unknown)
                      (format
                       nil
                       "~@[invalid SEM-I predicates: ~{|~(~s~)|~^, ~}~]"
                       unknown))))
              (push (list :semi output) (gethash id flags)))))

    (when (or (and verbose (not (zerop (hash-table-count flags))))
              (eq verbose :all))
      (format 
       t 
       "~&~%[~a] `~a' (~a)~%~%"
       (get-field :i-id item) (get-field :i-input item)
       (get-field :readings item)))
    (unless (zerop (hash-table-count flags))
      (when verbose
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
                      (:derivation
		       (let ((failure (second foo)))
			 (format t "    reconstruct: ")
                         (derivation-pprint-failure
                          failure :stream t :break nil :prefix "")
                         (terpri t)))
                      (:sparseness
                       (format 
                        t "    sparseness: only ~a relation~p.~%"
                        (second foo) (second foo)))
                      (:syntax
                       (format 
                        t "    syntax: ~a.~%"
                        (second foo)))
                      (:ascope
                       (format 
                        t 
                        "    exhaustive scoping: `~a'.~%"
                        (normalize-string (second foo))))
                      (:cscope
                       (format 
                        t 
                        "    cheap scoping: `~a'.~%"
                        (normalize-string (second foo))))
                      ((:uscope :unet)
                       (format 
                        t 
                        "    UTool: `~a'.~%"
                        (normalize-string (second foo))))
                      (:scope
                       (format 
                        t 
                        "    scoping: `~a'.~%"
                        (normalize-string (second foo))))
                      (:fragmentation
                       (format 
                        t 
                        "    fragmentation: ~a.~%"
                        (second foo)))
                      (:cycle
                       (format 
                        t 
                        "    cycle: ~a.~%"
                        (second foo)))
                      (:connectivity
                       (format 
                        t 
                        "    unattached: ~a.~%"
                        (second foo)))
                      (:lnk
                       (format 
                        t 
                        "    surface linking: ~a.~%"
                        (second foo)))
                      (:semi
                       (format 
                        t 
                        "    SEM-I: ~a.~%"
                        (second foo)))))))
      item)))

(defun safe-mrs-equal-p (mrs1 mrs2)
  (and #+:mrs (mrs:psoa-p mrs1) #+:mrs (mrs:psoa-p mrs1)
       #+:mt (ignore-errors (mt::mrs= mrs1 mrs2))))

(defun safe-mrs-unequal-p (mrs1 mrs2)
  (not (safe-mrs-equal-p mrs1 mrs2)))