;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2007 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2005 -- 2007 Erik Velldal (erikve@ifi.uio.no)
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
     (compact nil)
     (identity (format nil "~a.~a" (current-user) (current-pid)))
     (recache t)
     (verbose t) (stream t) (debug nil)
     (evalp nil) ;; create eval.gz files with scores in the target profiles
     ;;
     ;; global Redwoods parameters
     ;;
     (use-preterminal-types-p *feature-use-preterminal-types-p*)
     (ngram-tag *feature-ngram-tag*)
     (score-similarities '(:neva :wa))
     enhancers (resolvedp t) normalizep
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
     (flags *feature-flags*)
     (random-sample-size *feature-random-sample-size*)
     (counts-absolute 0) (counts-contexts 0)
     (counts-events 0) (counts-relevant 0)
     ;;
     ;; finally, a generic `custom' feature selection mechanism
     ;;
     #+:onet
     (custom *feature-custom*)
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
     (absolute-tolerance *maxent-absolute-tolerance*)
     (iterations *maxent-iterations*)
     ;;
     ;; estimation parameters: SVM
     ;;
     (kernel *svm-kernel*)
     (rbf-g *svm-rbf-g*) 
     (poly-d *svm-poly-d*) 
     (sig-poly-s *svm-sig-poly-s*)
     (sig-poly-r *svm-sig-poly-r*)
     #+:null
     (iterations *svm-iterations*)
     (balance *svm-cost-balance*) 
     (error-to-margin *svm-error-to-margin*)
     (tolerance *svm-tolerance*))
     
  (macrolet ((gridify (parameters &body body)
               (if (null (rest parameters))
                 `(dolist ,(first parameters) ,@body)
                 `(dolist ,(first parameters)
                    (gridify ,(rest parameters) ,@body)))))
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
                               collect (cons foo bar)))))
             (report (experiment run skipped total recache)
               (format 
                stream
                "~&[~a] batch-experiment(): ~
                 experiment # ~d; grid # ~d (~d skipped); ~
                 (~d~@[; cc~]).~%"
                (current-time :long :short) experiment
                (+ run skipped -1) skipped total recache))
             (execute-experiment (&key target recache)
               (let ((executep (test-experiment target :supersede supersede)))
                 (when executep  
                   (tsdb :create target :skeleton skeleton)
                   (#-:debug handler-case #+:debug progn
                       (rank-profile
                        source target :type type
                        :nfold nfold :niterations niterations
                        :recache recache :identity identity
                        :enhancers enhancers
                        :resolvedp resolvedp
                        :normalizep normalizep)
                    #-:debug
                    (condition (condition)
                      (format
                       stream
                       "~&[~a] batch-experiment(): ~
                        error: `~a'.~%"
                       (current-time :long :short) condition)))
                   (when evalp
                     (create-evaluation-file 
                      target source :similarities score-similarities))
                   (purge-profile-cache target))
                 executep)))
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
                  (*feature-flags* (listify flags))
                  (*feature-random-sample-size* (listify random-sample-size))
                  (*feature-frequency-threshold* thresholds)
                  #+:onet
                  (*feature-custom* (listify custom)))
          (let ((run 0)
                (skipped 0))
            (case type
              (:mem
               (gridify ((*redwoods-train-percentage* 
                          (listify train-percentage))
                         (*maxent-method* (listify method))
                         (*maxent-variance* (listify variance))
                         (*maxent-relative-tolerance*
                          (listify relative-tolerance))
                         (*maxent-absolute-tolerance*
                          (listify absolute-tolerance))
                         (*maxent-iterations* (listify iterations)))

                 (when debug
                   (excl:print-type-counts) (excl:gc)
                   (excl:print-type-counts) (room))
                        
                 (let ((recache (and (zerop run) recache)))
                   (if (execute-experiment 
                        :target (mem-environment 
                                 :full t :prefix prefix
                                 :format (if compact :compact :string))
                        :recache recache)
                     (incf run)
                     (incf skipped))
                   (incf total)
                   (when verbose 
                     (report experiment run skipped total recache))
                   (incf experiment))))
              
              (:svm
               (gridify ((*redwoods-train-percentage* 
                          (listify train-percentage))
                         (*svm-kernel* (listify kernel))
                         (*svm-rbf-g* (listify rbf-g))
                         (*svm-poly-d* (listify poly-d)) 
                         (*svm-sig-poly-s* (listify sig-poly-s))
                         (*svm-sig-poly-r* (listify sig-poly-r))
                         (*svm-iterations* (listify iterations))
                         (*svm-error-to-margin* (listify error-to-margin))
                         (*svm-cost-balance* (listify balance))
                         (*svm-tolerance* (listify tolerance)))
                        
                 (let ((recache (and (zerop run) recache)))
                   (if (execute-experiment 
                        :target (svm-environment 
                                 :full t :prefix prefix
                                 :format (if compact :compact :string))
                        :recache recache)
                      (incf run)
                     (incf skipped))
                   (incf total)
                   (when verbose 
                     (report experiment run skipped total recache))
                   (incf experiment))))))))
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

(defun summarize-experiments (&key (stream t)
                                   pattern (score :accuracy))
  (when (stringp stream)
    (with-open-file (stream stream :direction :output :if-exists :supersede)
      (return-from summarize-experiments
        (summarize-experiments :stream stream :pattern pattern :score score))))
  (loop
      with key = (if (eq score :accuracy) :f-accuracy :f-extras)
      with profiles
      = (loop
            for profile in (find-tsdb-directories *tsdb-home* :pattern pattern)
            collect (get-field :database profile))
      for profile in profiles
      for values = (select
                    (list (format nil "~(~a~)" key) "f-iterations")
                    nil "fold" nil profile)
      for iterations = 0
      for scores = (loop
                       for value in values
                       for field = (get-field key value)
                       for n = (get-field :f-iterations value)
                       when (numberp n) do (incf iterations n)
                       when (eq score :accuracy)
                       collect (read-from-string field)
                       else 
                       collect (let* ((value (read-from-string field))
                                      (field (get-field score value)))
                                 (when (stringp field)
                                   (read-from-string field))))
      when (and scores (loop for score in scores always score))
      do
        (let* ((n (length scores))
               (sum (sum scores))
               (mean (/ sum n))
               (min (loop for score in scores minimize score))
               (max (loop for score in scores maximize score))
               (range (- max min))
               (variance
                (if (= n 1)
                  0
                  (/ (loop for score in scores sum (expt (- score mean) 2))
                     (- n 1))))
               (iterations (divide iterations n)))
          (format
           stream
           "~,6f ~,6f ~,6f ~,1f `~a'~%" 
           mean (sqrt variance) range iterations profile)
          (force-output stream))))
