;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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


;;; 
;;; typical TADM output:
;;;
;;;   Classes    = 26944
;;;   Contexts   = 600
;;;   Features   = 149774 / 283752
;;;   Non-zeros  = 24071755
;;;
;;; we interpret that as 26944 events in 600 contexts; 283752 total features of
;;; which 149774 have some property; and 24071755 actual (non-zero) feature 
;;; counts, i.e. actually observed feature occurences.  a day later, we think
;;; that relevant property of the 149774 sub-set of features might be that they
;;; are attested in active events, but this remains guesswork.
;;;

(in-package :tsdb)

(defparameter *maxent-method* :tao_lmvm)

(defparameter *maxent-iterations* 5000)

(defparameter *maxent-relative-tolerance* 1e-7)

(defparameter *maxent-absolute-tolerance* 1e-20)

(defparameter *maxent-variance* 1e2)

(defparameter *maxent-uniform* nil)

(defparameter *maxent-extra-options* nil)

(defparameter *maxent-options* 
    '(*maxent-method*
      *maxent-iterations*
      *maxent-relative-tolerance* 
      *maxent-absolute-tolerance*
      *maxent-variance*))

(defparameter *maxent-debug-p* t)

(defparameter *svm-kernel* 2)

(defparameter *svm-error-to-margin* nil)

(defparameter *svm-cost-factor* 1.0)

(defparameter *svm-iterations* 100000)

(defparameter *svm-tolerance* 0.001)

(defparameter *svm-poly-d* nil)

(defparameter *svm-rbf-g* nil)

(defparameter *svm-sig-poly-s* nil)

(defparameter *svm-sig-poly-r* nil)

(defparameter *svm-cache-size* 1000)

(defparameter *svm-options*
    '(*svm-kernel*
      *svm-rbf-g*
      *svm-poly-d*
      *svm-sig-poly-s*
      *svm-sig-poly-r*
      *svm-iterations*
      *svm-cost-factor*
      *svm-error-to-margin*
      *svm-tolerance*))

(defun feature-environment ()
  (format 
   nil
   "GP[~a] ~:[-~;+~]PT ~:[-~;+~]LEX ~:[-~;+~]AE NS[~a] ~
    NT[~(~a~)] ~:[-~;+~]NB LM[~:[0~*~;~a~]] FT[~a] RS[~a]"
   *feature-grandparenting* *feature-use-preterminal-types-p*
   *feature-lexicalization-p* *feature-active-edges-p*
   *feature-ngram-size* *feature-ngram-tag*
   *feature-ngram-back-off-p* *feature-lm-p* 
   *feature-lm-p* *feature-frequency-threshold* 
   *feature-random-sample-size*))
;;;
;;; _fix_me_
;;; once we have all the globals together, synchronize these and functions like
;;; mem-environment(), print-mem(), et al.               (11-jul-05; erik & oe)
;;;
(defun mem-environment ()
  (format 
   nil
   "MM[~(~a~)] MI[~a] RT[~a] VA[~a]"
   *maxent-method* (or *maxent-iterations* 0) 
   *maxent-relative-tolerance* *maxent-variance*))

(defun svm-environment ()
  (format 
   nil
   "K[~[lin~;pol~;rbf~;sig~;usr~]]~@[ G[~a]~]~
    ~@[ D[~a]~]~@[ S[~a]~]~@[ R[~a]~]~@[ IT[~a]~]~
    ~@[ C[~a]~]~@[ EM[~a]~]~@[ T[~a]~]"
   *svm-kernel* *svm-rbf-g* *svm-poly-d*
   *svm-sig-poly-s* *svm-sig-poly-r*
   *svm-iterations* *svm-cost-factor*
   *svm-error-to-margin* *svm-tolerance*))

(defun print-model (model &key (file "/dev/null") stream (format :rpm))
  (case format
    ((:mem :rpm)
     (with-open-file (foo file :direction :output :if-exists :supersede)
       (loop
           with stream = (or stream foo)
           for context in (model-contexts model)
           do 
             (print-context 
              context :stream stream :model model :format format))))
    (:export
     (with-open-file (foo file :direction :output :if-exists :supersede)
       (let  ((stream (or stream foo))
              (table (model-table model)))
         (format 
          stream 
          ";;;~%;;; ~a~%;;; (~a@~a; ~a)~%;;;~%"
          model (current-user) (current-host) (current-time :long :pretty))
         (format stream "~%:begin :model ~d.~%~%" (model-ncontexts model))
         (when (> (model-count model) 0)
           (loop
               with *print-case* = :downcase
               for key in *maxent-options*
               when (boundp key) do
                 (format stream "~a := ~s.~%~%" key (symbol-value key))))
         (format stream ":begin :features ~d.~%~%" (symbol-table-count table))
         (loop
             with *print-case* = :downcase
             with *package* = (find-package :lkb)
             with map = (model-map model)
             for code from 0 to (- (symbol-table-count table) 1)
             for symbol = (code-to-symbol code table)
             for weight = (or (aref (model-weights model) code) 0.0)
             for count = (or (aref (model-counts model) code) (make-counts))
             for mapped = (and map (gethash code (rest map)))
             do
               (format stream "(~d~@[:~d~]) " code mapped)
               (format stream "[~{~s~^ ~}] ~f " symbol weight)
               (format stream "{~{~d~^ ~}}" count)
               (format stream "~%"))
         (format stream "~%:end :features.~%~%:end :model.~%"))))))

(defun read-weights (model file)
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (when stream
      (loop
          for i from 0
          for weight = (read stream nil nil)
          while weight do
            (when (>= i (model-size model))
              (let ((n (setf (model-size model)
                         (max (+ i 1) (* (model-size model) 2)))))
                (setf (model-counts model)
                  (adjust-array
                   (model-counts model) n))
                (setf (model-weights model)
                  (adjust-array
                   (model-weights model) n :initial-element 0.0))))
            (setf (aref (model-weights model) i) weight)
            (incf (model-count model))))))

(defun read-model (file &key (verbose t))
  (labels ((|[|-reader (stream char)
               (declare (ignore char))
               (read-delimited-list #\] stream nil))
           (|{|-reader (stream char)
               (declare (ignore char))
               (read-delimited-list #\} stream nil)))
    (let* ((*readtable* (copy-readtable nil))
           (*package* (find-package :lkb))
           (model (make-model))
           (table (model-table model))
           (name (format 
                  nil "~a~@[.~a~]"
                  (pathname-name file) (pathname-type file))))
      (set-syntax-from-char #\. #\space *readtable*)
      (if (probe-file file)
        (with-open-file (stream file :direction :input)
          (unless (and (eq (read stream nil nil) :begin)
                       (eq (read stream nil nil) :model)
                       (integerp (read stream nil nil)))
            (format t "read-model(): invalid header in `~a'.~%" name)
            (return-from read-model))
          (when verbose
            (format t "~&read-model(): reading file `~a'.~%" name))
          (loop
              with bodyp = nil
              for form = (read stream nil :eof)
              while (not (eq form :eof))
              when (and (eq form :begin) (eq (read stream nil nil) :features))
              do
                (let ((n (read stream nil nil)))
                  (unless (and (integerp n) (>= n 0))
                    (format 
                     t 
                     "read-model(): invalid `:begin :feature' block in `~a'.~%"
                     name)
                    (return-from read-model))
                  (setf (model-size model) n)
                  (setf (model-counts model) (make-array n))
                  (setf (model-weights model) 
                    (make-array n :initial-element 0.0)))
                (setf *readtable* (copy-readtable nil))
                (set-syntax-from-char #\[ #\( *readtable*)
                (set-syntax-from-char #\] #\) *readtable*)
                (set-macro-character #\[ #'|[|-reader nil *readtable*)
                (set-syntax-from-char #\{ #\( *readtable*)
                (set-syntax-from-char #\} #\) *readtable*)
                (set-macro-character #\{ #'|{|-reader nil *readtable*)
                (setf bodyp t)
              else when (eq form :end) do
                (set-syntax-from-char #\. #\space *readtable*)
                (unless (and (eq (read stream nil nil) :features)
                             (eq (read stream nil nil) :end) 
                             (eq (read stream nil nil) :model))
                  (format t "read-model(): invalid model prologue.~%")
                  (return-from read-model))
                (return (setf %model% model))
              else when bodyp do
                (unless (consp form)
                  (format t "read-model(): invalid codes `~a'.~%" form)
                  (return-from read-model))
                ;;
                ;; _fix_me_;
                ;; with the new serialization format, we should really insert
                ;; the symbol -- code pair into the symbol table (and map, in
                ;; case we also get a mapped code) here.  (1-feb-06; oe & erik)
                ;;
                (let* ((symbol (let ((foo (read stream nil nil)))
                                 (unless (consp foo)
                                   (format
                                    t
                                    "read-model(): invalid symbol `~a'.~%"
                                    foo)
                                   (return-from read-model))
                                 foo))
                       (code (let ((code (symbol-to-code symbol table)))
                               (unless (and (numberp (first form))
                                            (= code (first form)))
                                 (format
                                  t
                                  "read-model(): invalid symbol `~a'.~%"
                                  symbol)
                                 (return-from read-model))
                               code))
                       (weight (read stream nil nil))
                       (counts (read stream nil nil)))
                  (unless (numberp weight)
                    (format 
                     t 
                     "read-model(): invalid weight on `~a'.~%"
                     form)
                    (return-from read-model))
                  (when (>= code (model-size model))
                    (format 
                     t 
                     "read-model(): mysterious feature overflow (~a vs. ~a).~%"
                     code (model-size model))
                    (return-from read-model))
                  (setf (aref (model-weights model) code) weight)
                  (setf (aref (model-counts model) code)
                    (make-counts
                     :absolute (first counts) :contexts (second counts)
                     :events (third counts) :relevant (fourth counts))))
                (incf (model-count model))))
        (format t "read-model(): unable to open `~a'.~%" name)))))

(defun estimate-model (items &key (identity (current-pid)) fold
                                (stream *tsdb-io*) model type)
  
  (declare (ignore stream))
  (let* ((model (or model (make-model)))
         (events (format 
                  nil
                  "/tmp/.model.~a.~a.events"
                  (current-user) (current-pid)))
         (trace (format 
                 nil
                 "/tmp/.model.~a.~a.trace"
                 (current-user) (current-pid)))
         (source (get-field :source (first items)))
         (cache (profile-find-context-cache source identity)))
    (unless (model-parameters model)
      (setf (model-parameters model)
        (format 
         nil
         "/tmp/.model.~a.~a.weights"
         (current-user) (current-pid))))
    (with-open-file (out events :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (loop
          for item in items
          for cc = (let ((foo (get-field :source item)))
                     (cond
                      ((string= source foo) cache)
                      (t
                       (setf source foo)
                       (setf cache
                         (profile-find-context-cache source identity)))))
          for iid = (get-field :i-id item)
          for file = (merge-pathnames
                      cc 
                      (make-pathname :name (format
                                            nil
                                            "~a~@[.~a~]"
                                            iid *feature-random-sample-size*)))
          for readings = (get-field :readings item)
          when (> readings 1) do 
            ;;
            ;; for items that were not annotated or for some other reason have
            ;; no information in the feature and, thus, context cache, cp() 
            ;; will just do nothing, when .file. does not exist.
            ;;
            (when (or (null *feature-random-sample-size*)
                      (<= readings *feature-random-sample-size*)
                      (null (cp file out)))
              ;;
              ;; even when random sampling is enabled, for contexts with fewer
              ;; events than the maximum sample size, the context cache will 
              ;; not contain two files, but rather just the base file name; so
              ;; try to fall back on the non-sampled file then.
              ;;
              (let ((file (merge-pathnames
                           cc 
                           (make-pathname :name (format nil "~a" iid)))))
                (cp file out)))))
    (let* ((parameters (model-parameters model))
           (variances (when  (numberp *maxent-variance*)
                        (let ((name (format 
                                     nil
                                     "/tmp/.model.~a.~a.variances"
                                     (current-user)
                                     (current-pid))))
                          (with-open-file (stream name
                                           :direction :output
                                           :if-exists :supersede)
                            (format stream "~,6f" *maxent-variance*))
                          name)))
           (command (case type 
                      (:mem
                       (format 
                        nil 
                        "tadm -events_in ~a -params_out ~a~
                        ~@[ -method ~(~a~)~]~
                        ~@[ -max_it ~a~]~@[ -frtol ~a~]~@[ -fatol ~a~]~
                        ~@[ -variances ~a~]~
                        ~:[~; -uniform~]~
                        ~@[ ~a~]"
                        events parameters
                        *maxent-method*
                        *maxent-iterations*
                        *maxent-relative-tolerance* *maxent-absolute-tolerance*
                        variances
                        *maxent-uniform* 
                        *maxent-extra-options*))
                      (:svm
                       (format 
                        nil 
                        "svm_learn -n 30 -q 40 -m ~a -z p -t ~a ~@[-g ~a~]~
                         ~@[ -d ~a~]~@[ -s ~a~]~@[ -r ~a~]~@[ -# ~a~]~
                         ~@[ -j ~a~]~@[ -e ~a~]~@[ -c ~a~] ~a ~a"
                        *svm-cache-size* *svm-kernel* *svm-rbf-g* *svm-poly-d* 
                        *svm-sig-poly-s* *svm-sig-poly-r* *svm-iterations*
                        *svm-cost-factor* *svm-tolerance*
                        *svm-error-to-margin* events parameters))))
           (output (if *maxent-debug-p* nil "/dev/null")))
      (when (and (zerop (run-process 
                         (format nil "~a | tee '~a'" command trace)
                         :wait t 
                         :output output :if-output-exists :supersede))
                 (probe-file parameters))
        (when (probe-file trace)
          (nconc fold (acons :f-estimation (read-file trace) nil)))
        (when (and (eq type :mem)
                   (probe-file parameters))
          (nconc
           fold
           (acons :f-features (get-field :lines (wc parameters)) nil)))
        (unless *maxent-debug-p*
          (ignore-errors (delete-file trace))
          (ignore-errors (delete-file events)))
        model))))

(defun learner-rank-items (items model &key (identity (current-pid)) fold 
                                            type (stream *tsdb-io*))

  (let* ((parameters (model-parameters model))
         (events (format 
                  nil
                  "/tmp/.model.~a.~a.events"
                  (current-user) (current-pid)))
         (source (get-field :source (first items)))
         (cache (profile-find-context-cache source identity))
         active)
    (when (null parameters)
      (format t "learner-rank-items(): invalid model: no parameters.~%")
      (return-from learner-rank-items))
    (format
     stream
     "~&[~a] learner-rank-items:() evaluating ~d item~p ~%"
     (current-time :long :short) (length items) (length items))
    ;;
    ;; in order to keep using this stream across multiple calls to cp(), its
    ;; :element-type needs to match the expecations of cp(), i.e. be byte-wise
    ;; binary data.
    ;;
    (with-open-file (out events :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (loop
          for item in items
          for cc = (let ((foo (get-field :source item)))
                     (cond
                      ((string= source foo) cache)
                      (t
                       (setf source foo)
                       (setf cache
                         (profile-find-context-cache source identity)))))
          for iid = (get-field :i-id item)
          for readings = (get-field :readings item)
          for file = (merge-pathnames
                      cc 
                      (make-pathname :name (format nil "~a" iid)))
          when (> readings 1) do
            (when (probe-file file)
              (cp file out)
              (push item active))))
    (setf active (nreverse active))
    (let* ((scores (format 
                    nil
                    "/tmp/.model.~a.~a.scores"
                    (current-user) (current-pid)))
           (output (format 
                    nil
                    "/tmp/.model.~a.~a.output"
                    (current-user) (current-pid)))
           (command 
            (case type
              (:mem            
               (format
                nil
                "evaluate -s '~a' '~a' '~a'"
                scores parameters events))                    
              (:svm 
               (format nil 
                       "svm_classify ~a ~a ~a" 
                       events parameters scores))))
           (nevents 0))
      (when (and (zerop (run-process 
                         command :wait t 
                         :output output :if-output-exists :supersede))
                 (probe-file scores) (probe-file output))
        (format
         t
         "~&[~a] learner-rank-items:() ranking ~d item~p ~%"
         (current-time :long :short) (length items) (length items))
        (with-open-file (in scores :direction :input)
          (loop
              for item in active
              for results = (get-field :results item)
              for ranks
              = (loop
                    for result in results
                    for rid = (get-field :result-id result)
                    do (incf nevents)
                    collect (pairlis '(:result-id :score)
                                     (list rid (read in nil nil))))
              do
                (let* ((ranks (sort
                               ranks (case type 
                                       (:mem #'>)
                                       (:svm #'<))
                               :key #'(lambda (foo) (get-field :score foo))))
                       (ranks (loop
                                  with last = (get-field :score (first ranks))
                                  with i = 1
                                  with j = 2
                                  for rank in ranks
                                  for score = (get-field :score rank)
                                  unless (= score last) do
                                    (setf i j) (setf last score) (incf j)
                                  collect (acons :rank i rank))))
                  (if (get-field :ranks item)
                    (setf (get-field :ranks item) ranks)
                    (nconc item (acons :ranks ranks nil))))))
        (when fold
          (nconc fold (acons :f-events nevents nil))
          (when (eq type :mem)
            (with-open-file (in output :direction :input)
              (let ((line (read-line in nil nil)))
                (when line
                  (multiple-value-bind (foo matches)
                      (ppcre:scan-to-strings "([0-9.]+) [0-9]*$" line)
                    (declare (ignore foo))
                    (when matches
                      (let ((accuracy (acons :accuracy (aref matches 0) nil)))
                      (if (get-field :f-extras fold)
                        (nconc (get-field :f-extras fold) accuracy)
                        (nconc
                         fold 
                         (acons :f-extras accuracy nil))))))))))))
      (unless *maxent-debug-p*
        (ignore-errors (delete-file events))
        (ignore-errors (delete-file scores))
        (ignore-errors (delete-file output))))
    active))

(defun score-feature (code model)
  (if (< code (model-count model)) (aref (model-weights model) code) 0.0))

(defun mem-score-configuration (edge daughters &optional (model %model%))
  ;;
  ;; _fix_me_
  ;; there is too much duplication in various bits of feature extraction code;
  ;; rework from scratch, one day.                             (30-dec-04; oe)
  ;;
  (if (model-p model)
    (let* ((table (model-table model))
           (roots (if daughters
                    (loop
                        for edge in daughters
                        collect (edge-root edge))
                    (lkb::edge-leaves edge)))
           (feature (cons (edge-root edge) roots))
           (code (symbol-to-code (list* 1 0 feature) table)))
      (score-feature code model))
    0.0))

(defconstant e (exp 1d0))

(defun scores-to-probabilities (scores)
  (loop
      with sum = 0d0
      for score in scores
      for foo = (if (stringp score) (read-from-string score) score)
      for p = (expt e (coerce foo 'long-float))
      collect p into probabilities
      do (incf sum p)
      finally
        (return (loop 
                    for p in probabilities
                    collect (/ p sum)))))

(defun entropy (probabilities)
  (loop
      with h = 0d0
      for p in probabilities
      do (incf h (* p (log p 2d0)))
      finally (return (- h))))

(defmacro do-grid (params &body body)
  (if (null (cdr params))
      `(dolist ,(first params)
         ,@body)
    `(dolist ,(first params)
       (do-grid ,(cdr params) ,@body))))

(defun mem-batch-experiment (&key
                             (ae-p *feature-active-edges-p*)
                             (ngram-scale *feature-lm-p*)
                             (ngrams *feature-ngram-size*)
                             (lexicalize *feature-lexicalization-p*)
                             (preterm-types *feature-use-preterminal-types-p*)
                             (ngram-tag *feature-ngram-tag*)
                             (grandparenting *feature-grandparenting*)
                             (back-off-p *feature-ngram-back-off-p*)
                             (source *tsdb-gold*)
                             (random-sample-size *feature-random-sample-size*)
                             (skeleton *tsdb-default-skeleton*) 
                             (nfold 10) 
                             (type :mem) 
                             (supersede-p nil)
                             (item-sets-p *redwoods-use-item-sets-p*)
                             target-prefix
                             identity
                             niterations
                             data 
                             (recache-p t)
                             (similarity-p (acons :bleu  #'bleu-similarity nil))
                             (variance *maxent-variance*)
                             (tolerance *maxent-relative-tolerance*)
                             (iterations *maxent-iterations*)
                             (method *maxent-method*)
                             (context 0)
                             (relevant 0))

;;  (mt::lm-restore-defaults lm-toolkit)
  (labels ((ennl (x);;ensure-non-nil-list
             (if (or (null x) 
                     (not (listp x)))
                 (list x) 
               x))
           (changed-triggers-p (env1 env2)
             (dolist (trigger *feature-options*)
               (if (not (equal (get-field trigger env1)
                               (get-field trigger env2)))
                   (return t)))))
    (let* ((counter-tot 0)
           (counter-cur 0)
           (old-env nil)
           (*redwoods-score-similarity-hooks* similarity-p)
           (*redwoods-use-item-sets-p* item-sets-p) 
           (identity (or identity 
                         (format 
                          nil "~a.~a" (current-user) (current-pid)))))
      
      (do-grid ((lex (ennl lexicalize))
                (preterm (ennl preterm-types))
                (tag (ennl ngram-tag))
                (gp (ennl grandparenting))
                (bo-p (ennl back-off-p))
                (lm (ennl ngram-scale))
                (ae-p (ennl ae-p))
                (ngrams (ennl ngrams))
                (rel (ennl relevant))
                (con (ennl context))
                ;; estimation params:
                (var (ennl variance))
                (tol (ennl tolerance))
                (met (ennl method)))
        (let* ((threshold (make-counts 
                           :absolute 0 :contexts con :events 0 :relevant rel))
               (target (format  
                        nil "~(~@[~a~]~a_lm~a_n~d_l~:[0~;1~]_p~:[0~;1~]_a~:[0~;1~]~
                            _g~d_b~:[0~;1~]_nt~:[0~;1~]_r~a_c~a~@[_rs~a~]_t~e_v~e~)"
                        target-prefix type lm ngrams lex preterm ae-p gp bo-p  
                        (eq tag :type) rel con random-sample-size tol var))
               (new-env (pairlis 
                         (append *feature-options* *maxent-options*)
                         (list 
                          gp preterm lex ae-p
                          ngrams tag bo-p lm
                          threshold
                          random-sample-size
                          met iterations tol 
                          *maxent-absolute-tolerance* 
                          var)))
               (recache-p (if (zerop counter-cur)
                              recache-p
                            (changed-triggers-p new-env old-env)))
               (experiment (unless (and (not supersede-p)
                                        (cl-fad:file-exists-p 
                                         (find-tsdb-directory target))) 
                             (make-experiment :source source
                                              :skeleton skeleton
                                              :target target
                                              :nfold nfold
                                              :environment new-env
                                              :identity identity
                                              :type type
                                              :data data))))
          (incf counter-tot)
          #+:debug
          (setq %experiment% experiment)
          (when experiment
            (setq old-env new-env)
            (run-experiment experiment :recache recache-p 
                            :niterations niterations)
            (incf counter-cur)
            (format t "~&Completed experiment no. ~a (~a).~%" 
                    counter-tot counter-cur))))))
  (purge-profile-cache source))

(defun svm-batch-experiment (&key
                             (ae-p *feature-active-edges-p*)
                             (ngram-scale *feature-lm-p*)
                             (ngrams *feature-ngram-size*)
                             (lexicalize *feature-lexicalization-p*)
                             (preterm-types *feature-use-preterminal-types-p*)
                             (ngram-tag *feature-ngram-tag*)
                             (grandparenting *feature-grandparenting*)
                             (back-off-p *feature-ngram-back-off-p*)
                             (source *tsdb-gold*)
                             (random-sample-size *feature-random-sample-size*)
                             (skeleton *tsdb-default-skeleton*) 
                             (nfold 10) 
                             (type :svm) 
                             (supersede-p nil)
                             (item-sets-p *redwoods-use-item-sets-p*)
                             target-prefix
                             identity
                             niterations
                             data 
                             (recache-p t)
                             (kernel *svm-kernel*)
                             (rbf-g *svm-rbf-g*) 
                             (poly-d *svm-poly-d*) 
                             (sig-poly-s *svm-sig-poly-s*)
                             (sig-poly-r *svm-sig-poly-r*)
                             (iterations *svm-iterations*)
                             (cost *svm-cost-factor*) 
                             (error-to-margin *svm-error-to-margin*)
                             (tolerance *svm-tolerance*)
                             (similarity-p (acons :bleu  #'bleu-similarity nil))
                             (context 0)
                             (relevant 0))
                                             
;;  (mt::lm-restore-defaults lm-toolkit)
  (labels ((ennl (x);;ensure-non-nil-list
             (if (or (null x) 
                     (not (listp x)))
                 (list x) 
               x))
           (changed-triggers-p (env1 env2)
             (dolist (trigger *feature-options*)
               (if (not (equal (get-field trigger env1)
                               (get-field trigger env2)))
                   (return t)))))
    (let ((counter-tot 0)
          (counter-cur 0)
	  (old-env nil)
          (*redwoods-score-similarity-hooks* similarity-p)
          (*redwoods-use-item-sets-p* item-sets-p)
          (identity (or identity 
                       (format 
                        nil "~a.~a" (current-user) (current-pid)))))
      (do-grid ((lex (ennl lexicalize))
                (preterm (ennl preterm-types))
                (tag (ennl ngram-tag)) 
                (gp (ennl grandparenting))
                (bo-p (ennl back-off-p))
                (lm (ennl ngram-scale))
                (ae-p (ennl ae-p))
                (ngrams (ennl ngrams))
                (rel (ennl relevant))
                (con (ennl context))
                ;; estimation params:
                (k (ennl kernel))
                (g (ennl rbf-g))
                (d (ennl poly-d)) 
                (s (ennl sig-poly-s))
                (r (ennl sig-poly-r))
                (it (ennl iterations))
                (c (ennl cost))
                (em-ratio (ennl error-to-margin))
                (tol (ennl tolerance)))
        (let* ((threshold (make-counts 
                           :absolute 0 :contexts con :events 0 :relevant rel))
               (target (format 
                        nil "~(~@[~a~]~a_lm~a_n~d_l~:[0~;1~]_p~:[0~;1~]~
                            _a~:[0~;1~]_g~d_b~:[0~;1~]_nt~:[0~;1~]_r~a_c~a~
                            ~@[_rs~a~]_~[lin~;pol~;rbf~;sig~;usr~]~
                            ~@[_g~a~]~@[_d~a~]~@[_s~a~]~@[_r~a~]~
                            ~@[_it~a~]~@[_c~a~]~@[_em~a~]~@[_t~a~]~)"
                        target-prefix type lm ngrams lex preterm 
                        ae-p gp bo-p (eq tag :type) rel con 
                        random-sample-size k g d s r it c em-ratio tol))
               (new-env (pairlis 
                         (append *feature-options* *svm-options*)
                         (list 
                          gp preterm lex ae-p
                          ngrams tag bo-p lm
                          threshold
                          random-sample-size
                          k g d s r it c em-ratio tol)))
               (recache-p (if (zerop counter-cur)
                              recache-p
                            (changed-triggers-p new-env old-env)))
               (experiment (unless (and (not supersede-p)
                                        (cl-fad:file-exists-p 
                                         (find-tsdb-directory target))) 
                             (make-experiment :source source
                                              :skeleton skeleton
                                              :target target
                                              :nfold nfold
                                              :environment new-env
                                              :identity identity
                                              :type type
                                              :data data))))
          (incf counter-tot)
          #+:debug
          (setq %experiment% experiment)
          (when experiment
            (setq old-env new-env)
            (run-experiment experiment :recache recache-p 
                            :niterations niterations)
            #+:null
            (purge-profile-cache target)
            (incf counter-cur)
            (format t "~&Completed experiment no. ~a (~a).~%" 
                    counter-tot counter-cur))))))
  (purge-profile-cache source))

#+:null
(mem-batch-experiment :ngram-scale '(nil 10)
                      :ngrams '(0 2)
                      :variance '(1.0e+1 1.0e+2 1.0e+3 1.0e+4)
                      :tolerance '(1.0e-6 1.0e-7 1.0e-8)
                      :source "lingo/15-nov-05/jh0/05-11-28/lkb.g"
                      :skeleton "jh0"
                      :target-prefix "jh0-"
                      :random-sample-size nil
                      :ae-p nil
                      :grandparenting '(0 2)
                      :back-off-p t
                      :supersede-p nil 
                      :item-sets-p t
                      :lexicalize '(nil t)
                      :preterm-types '(nil t))

(defun print-score-file (&key (output "/tmp/scores") gold name pattern condition
                              (similarity (acons :bleu #'bleu-similarity nil)))
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (loop
        with *redwoods-score-similarity-hooks* = similarity
        for db in (find-tsdb-directories *tsdb-home* 
                                         :pattern pattern :name name)
        for name = (let ((name (get-field :database db)))
                     (unless (string= name gold) name))
        when name do
          (let* ((scores (summarize-scores
                          name gold :condition condition
                          :n 1 :test :id :spartanp t :loosep t))
                 (total (rest (rest (find :total scores :key #'first))))
                 (nscores (get-field :scores total))
                 (exact (get-field :exact total))
                 (bleu (get-field :bleu (get-field :similarities total))))
            (purge-profile-cache name)
            (format
             stream
             "~,6f ~,6f `~a'~%"
             (* 100 (divide exact nscores)) (if bleu (divide bleu nscores) 0.0)
             name)
            (force-output stream))))
  (purge-profile-cache gold))

(defun summarize-folds (&key (output "/tmp/folds") name pattern)
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (loop
        for profile in (cond
                        ((not (null pattern))
                         (mapcar #'(lambda(db) (get-field :database db))
                                 (find-tsdb-directories 
                                  *tsdb-home* :pattern pattern :name name)))
                        ((and name (listp name)) name)
                        ((stringp name) (list name))
                        (t (error "summarize-folds(): name or pattern argument missing.")))
        for accuracies = (select '("f-accuracy") '(:string) 
                                             "fold" nil profile)
        when accuracies
        do
          (let* ((accuracies
                  (map 'list 
                    #'(lambda (x)
                        (read-from-string (cdar x) nil nil)) 
                    accuracies))
                 (n (length accuracies))
                 (sum (sum accuracies))
                 (mean (/ sum n))
                 (min (apply #'min accuracies))
                 (max (apply #'max accuracies))
                 (range (- max min))
                 (var (/ (sum 
                          (mapcar 
                           #'(lambda (x) 
                               (expt (- x mean) 2))
                           accuracies))
                         (- n 1)))
                 (std-dev (sqrt var)))
            (purge-profile-cache profile)
            (format stream "~,6f ~,6f ~,6f `~a'~%" 
                    mean std-dev range profile)
            (force-output stream)))))

(defun wilcoxon (list1 list2)
  (unless (= (length list1)
             (length list2))
    (error "wilcoxon(): given lists of different lengths: ~a and ~a~%"
           list1 list2))
  (let* ((signed-diffs 
          (remove-if #'zerop 
                     (mapcar #'(lambda (x y) (- x y))
                             list1 list2)))
         (ranked-diffs (sort (copy-list signed-diffs)
                             #'<
                             :key #'abs))
         (ranks 
          (loop 
              for (d1 d2) on ranked-diffs
              with ranks = nil
              with result = nil
              for rank from 1
              with push-p = nil
              with push-n = 1 
              with avg 
              do (cond ((and d2 (= (abs d1) (abs d2)))
                        (incf push-n)
                        (setq push-p nil))
                       (t 
                        (setq push-p t)))
                 (push rank ranks)
              when push-p do 
                (setq avg (float (/ (sum ranks) push-n)))
                (dotimes (n push-n)
                  (push avg result))
                (setq push-n 1)
                (setq ranks nil)
              finally (return (nreverse result))))
         (signed-ranks
          (loop
              for d in ranked-diffs
              for r in ranks
              collect (if (> d 0) r (- r))))
         (w-pos (loop for r in signed-ranks
                    when (> r 0) sum r))
         (w-neg (abs (loop for r in signed-ranks
                         when (< r 0) sum r))))
    (values (min w-pos (abs w-neg))
            (length signed-ranks))))

(defun t-test (list1 list2)
  (let* ((n1 (length list1))
         (n2 (length list2))
         (n (if (not (= n1 n2))
                (error "t-test(): given lists of different lengths: ~a and ~a~%"
                       list1 list2)
              n1))
         (diffs (mapcar #'(lambda (x y) (- x y))
                        list1 list2))
         (sum (sum diffs))
         (mean (/ sum n))
         (std-dev 
          (sqrt 
           (/ 
            (sum 
             (mapcar 
              #'(lambda (x) 
                  (expt (- x mean) 2)) 
              diffs)) 
            (- n 1))))
         (result (/ (* mean (sqrt n))
                    std-dev)))
    (values (abs result) (- n 1))))

(defun mw-t-test (list1 list2)
  (let* ((n1 (length list1))
         (n2 (length list2))
         (n (if (not (= n1 n2))
                  (error  "mw-t-test(): given lists of different lengths: ~a and ~a~%"
                         list1 list2)
                n1))
         (sum1 (sum list1))
         (sum2 (sum list2))
         (mean1 (/ sum1 n))
         (mean2 (/ sum2 n))
         (diffs1 (mapcar 
                  #'(lambda (x) 
                      (- x mean1)) 
                  list1))
         (diffs2 (mapcar 
                   #'(lambda (x) 
                       (- x mean2)) 
                   list2))
         (sum-squared-diffs
          (sum (mapcar 
                #'(lambda (x y) 
                    (expt (- x y) 2)) 
                diffs1 diffs2)))
         (result (* 
                  (- mean1 mean2)
                  (sqrt 
                   (/ (* n (- n 1))
                      sum-squared-diffs)))))    
    (values (abs result) (- n 1))))

(defparameter *t-dist* 
    '((0.10   (  3.078  1.886  1.638 1.533 1.476 1.44  1.415 1.397 1.383 1.372 
                 1.363  1.356  1.35  1.345 1.341 1.337 1.333 1.33  1.328 1.325))
      (0.05   (  6.314  2.92   2.353 2.132 2.015 1.943 1.895 1.86  1.833 1.812 
                 1.796  1.782  1.771 1.761 1.753 1.746 1.74  1.734 1.729 1.725))
      (0.025  ( 12.706  4.303  3.182 2.776 2.571 2.447 2.365 2.306 2.262 2.228 
                 2.201  2.179  2.16  2.145 2.131 2.12  2.11  2.101 2.093 2.086))
      (0.01   ( 31.821  6.965  4.541 3.747 3.365 3.143 2.998 2.896 2.821 2.764
                 2.718  2.681  2.65  2.624 2.602 2.583 2.567 2.552 2.539 2.528))   
      (0.005  ( 63.657  9.925  5.841 4.604 4.032 3.707 3.499 3.355 3.25  3.169 
                 3.106  3.055  3.012 2.977 2.947 2.921 2.898 2.878 2.861 2.845)) 
      (0.001) (318.313 22.327 10.215 7.173 5.893 5.208 4.782 4.499 4.296 4.143  
                 4.024  3.929  3.852 3.787 3.733 3.686 3.646 3.61  3.579 3.552))
  "t-test upper critical values")

(defparameter *wilcoxon-dist*
  '((0.05  (NIL NIL NIL NIL 1   2   4   6 8 11 14 17 21 26 30 36 41 47 54 60))
    (0.025 (NIL NIL NIL NIL NIL 1   2   4 6  8 11 14 17 21 25 30 35 40 46 52))
    (0.01  (NIL NIL NIL NIL NIL NIL 0   2 3  5  7 10 13 16 20 24 28 33 38 43))
    (0.005 (NIL NIL NIL NIL NIL NIL NIL 0 2  3  5  7 10 13 16 19 23 28 32 37)))     
  "wilcoxon upper critical values")

(defun get-critical-value (&key (level 0.05) (df 9) (sides 2) (test :ttest))
  (unless (or (= sides 1)
              (= sides 2))
    (error "critical-value(): sides should be 1 or 2. given ~a." sides))
  (nth (- df 1) (cadr (assoc (/ level sides) 
                             (case test 
                               ((:t :tee :ttest) *t-dist*)
                               ((:w :wilcoxon) *wilcoxon-dist*))))))
 
(defun compare-folds (name1 name2 &key (test :t) (stream *tsdb-io*)
                                       (level 0.05) (sides 2))
  (let* ((acc1 (select '("f-accuracy") '(:string) 
                       "fold" nil name1))
         (acc2 (select '("f-accuracy") '(:string) 
                       "fold" nil name2))
         (acc1 (map 'list 
                 #'(lambda (x) 
                     (read-from-string (cdar x) nil nil))
                 acc1))
         (acc2 (map 'list 
                 #'(lambda (x) 
                     (read-from-string (cdar x) nil nil)) 
                 acc2))
         (testfun (case test
                    ((:t :ttest) #'t-test)
                    ((:w :wilcoxon) #'wilcoxon))))
                 
    (multiple-value-bind (stat df)
        (funcall testfun acc1 acc2)

      (let ((cv (get-critical-value :level level 
                                    :sides sides :test test :df df)))
        
        (purge-profile-cache name1)
        (purge-profile-cache name2)
        (format stream "~&~,6f ~a ~a~%`~a'~%`~a'~%" 
                stat cv df name1 name2)))))
#+null
(compare-folds "dbg-hike-mem_lm10_n0_l0_p0_a0_g0_b0_nt1_t1.0e-8_v1.0e+1" 
               "dbg-hike-svm_lm10_n0_l0_p0_a0_g0_b0_nt1_rs10_rbf_g0.01_it500_c0.1_em0.1_t0.1" 
               :test :ttest :sides 2 :level 0.05)
