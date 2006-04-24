;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2005 -- 2006 Erik Velldal (erikve@ifi.uio.no)
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


(in-package :tsdb)

(defun batch-experiment
    (&key
     ;;
     ;; general parameters: `gold' input profile, output skeleton, how many
     ;; iterations of how many folds to run, et al.
     ;;
     (source *tsdb-gold*)
     (skeleton *tsdb-default-skeleton*)
     (nfold 10) 
     (niterations nfold)
     (type :mem) 
     (supersede nil)
     (prefix "")
     (identity (format nil "~a.~a" (current-user) (current-pid)))
     (recache t)
     (verbose t) (stream t)
     ;;
     ;; global Redwoods parameters
     ;;
     (use-preterminal-types-p *feature-use-preterminal-types-p*)
     (ngram-tag *feature-ngram-tag*)
     (score-similarities '(:bleu :wa))
     ;;
     ;; feature selection parameters
     ;;
     (grandparenting *feature-grandparenting*)
     (active-edges-p *feature-active-edges-p*)
     (lexicalization-p *feature-lexicalization-p*)
     (constituent-weight *feature-constituent-weight*)
     (ngram-size *feature-ngram-size*)
     (ngram-back-off-p *feature-ngram-back-off-p*)
     (lm-p *feature-lm-p*)
     (random-sample-size *feature-random-sample-size*)
     (counts-absolute 0) (counts-contexts 0)
     (counts-events 0) (counts-relevant 0)
     ;;
     ;; for learning curve computation: only use percentage of training data
     ;;
     (train-percentage '(100))
     ;;
     ;; estimation parameters: MaxEnt
     ;;
     (method *maxent-method*)
     (variance *maxent-variance*)
     (relative-tolerance *maxent-relative-tolerance*)
     (iterations *maxent-iterations*))
     
  (macrolet ((gridify (params &body body)
               (if (null (cdr params))
                 `(dolist ,(first params) ,@body)
                 `(dolist ,(first params)
                    (gridify ,(cdr params) ,@body)))))
    (labels ((listify (foo)
               (if (or (null foo) (atom foo)) (list foo) foo))
             (cross-product (&rest lists)
               (cross-product2 lists))
             (cross-product2 (lists)
               (if (null (rest lists))
                 (loop
                     for foo in (first lists) collect (list foo))
                 (loop
                     with rests = (cross-product2 (rest lists))
                     for foo in (first lists)
                     nconc (loop
                               for bar in rests
                               collect (cons foo bar))))))
      (let ((total 0)
            (experiment 0)
            (thresholds
             (loop
                 for (absolute contexts events relevant)
                 in (cross-product
                     (listify counts-absolute) (listify counts-contexts)
                     (listify counts-events) (listify counts-relevant))
                 collect (make-counts
                          :absolute absolute :contexts contexts
                          :events events :relevant relevant)))
            (*feature-use-preterminal-types-p* use-preterminal-types-p)
            (*feature-ngram-tag* ngram-tag)
            (*redwoods-score-similarities* score-similarities))
        
        ;;
        ;; from here on, essentially do two nested groups of loops (constructed
        ;; by virtue of an ingenious macro provided by erik :-): the outermost
        ;; gridify() will cross-multiply over feature selection parameters, so
        ;; that for each iteration at this level we need a fresh context cache.
        ;; conversely, in the inner gridify() scope, we are going through the
        ;; combinatorics of (learner-specific) estimation parameters.
        ;;
        (gridify ((*feature-grandparenting* (listify grandparenting))
                  (*feature-active-edges-p* (listify active-edges-p))
                  (*feature-lexicalization-p* (listify lexicalization-p))
                  (*feature-constituent-weight* (listify constituent-weight))
                  (*feature-ngram-size* (listify ngram-size))
                  (*feature-ngram-back-off-p* (listify ngram-back-off-p))
                  (*feature-lm-p* (listify lm-p))
                  (*feature-random-sample-size* (listify random-sample-size))
                  (*feature-frequency-threshold* thresholds))
          (case type
            (:mem
             (let ((run 0)
                   (skipped 0))
               (gridify ((*redwoods-train-percentage* 
                          (listify train-percentage))
                         (*maxent-method* (listify method))
                         (*maxent-variance* (listify variance))
                         (*maxent-relative-tolerance*
                          (listify relative-tolerance))
                         (*maxent-iterations* (listify iterations)))
                 ;;
                 ;; _fix_me_
                 ;; to adapt this function for SVMs too, all of the following
                 ;; could go into a function execute-experiment() or the like.
                 ;;                                            (31-mar-06; oe)
                 (let ((target (mem-environment
                                :format :string :full t :prefix prefix))
                       (recache (and (zerop run) recache)))
                   (cond 
                    ((test-experiment target :supersede supersede)

                     #-:debug
                     (progn
                       (excl:print-type-counts) (excl:gc)
                       (excl:print-type-counts) (room))
                     (tsdb :create target :skeleton skeleton)
                     (unwind-protect (rank-profile
                                      source target :type type
                                      :nfold nfold :niterations niterations
                                      :recache recache :identity identity)
                       (incf run)
                       (purge-profile-cache target)))
                    (t (incf skipped)))
                   (incf total)
                   (when verbose
                     (format
                      stream
                      "~&[~a] batch-experiment(): ~
                       experiment # ~d; grid # ~d (~d skipped); ~
                       (~d~@[; cc~]).~%"
                      (current-time :long :short)
                      experiment (+ run skipped -1) skipped
                      total recache)))))))
          (incf experiment)))
      (purge-profile-cache source))))

(defun test-experiment (target &key supersede (verbose t) (stream t))
  ;;
  ;; _fix_me_
  ;; make this function more intelligent, e.g. move old directory out of the 
  ;; way when superseding and check for appropriate number of folds et al. when
  ;; skipping an experiment.
  ;;
  (let ((path (find-tsdb-directory target)))
    (when (fad:file-exists-p path)
      (cond
       (supersede
        (when verbose
          (format
           stream
           "test-experiment(): purging `~a'.~%"
           target))
        (ignore-errors (fad:delete-directory-and-files path))
        (when (fad:file-exists-p path)
          (when verbose
            (format
             stream
             "test-experiment(): purge failed; skipping `~a'.~%"
             target))
          (return-from test-experiment)))
       (t
        (format
         stream
         "test-experiment(): skipping `~a'.~%"
         target)
        (return-from test-experiment)))))
  t)

#+:null
(batch-experiment
 :source "lingo/13-feb-06/csli/06-02-19/lkb" :skeleton "csli"
 :nfold 10 :niterations 2 :type :mem
 :prefix "test"
 :grandparenting '(0 1 2)
 :active-edges-p '(nil t)
 :lexicalization-p nil
 :constituent-weight '(0 1 2)
 :ngram-size '(0 1 2 3) :ngram-back-off-p '(nil t)
 :lm-p nil
 :random-sample-size nil
 :counts-absolute 0 :counts-contexts 0 :counts-events 0 :counts-relevant 1
 :variance '(nil 1e-2 1e-4 1e-6)
 :relative-tolerance '(1e-6 1e-8 1e-10))
