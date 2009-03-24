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
;;; the relevant property of the 149774 sub-set of features might be that they
;;; are attested in active events, but this remains guesswork.
;;;

(in-package :tsdb)

(defparameter *maxent-method* :tao_lmvm)

(defparameter *maxent-iterations* 5000)

(defparameter *maxent-relative-tolerance* 1e-10)

(defparameter *maxent-absolute-tolerance* 1e-20)

(defparameter *maxent-variance* 1e-2)

(defparameter *maxent-uniform* nil)

(defparameter *maxent-extra-options* nil)

(defparameter *maxent-options* 
  '(*maxent-method*
    *maxent-iterations*
    *maxent-relative-tolerance* 
    *maxent-absolute-tolerance*
    *maxent-variance*))

(defparameter *maxent-debug-p* t)

(defparameter *svm-kernel* 0) 

(defparameter *svm-error-to-margin* nil)

(defparameter *svm-cost-balance* 1.0)

(defparameter *svm-iterations* 1e+5)

(defparameter *svm-tolerance* 0.001)

(defparameter *svm-poly-d* nil)

(defparameter *svm-rbf-g* nil)

(defparameter *svm-sig-poly-s* nil)

(defparameter *svm-sig-poly-r* nil)

(defparameter *svm-cache-size* 5000.0)

(defparameter *svm-options*
  '(*svm-kernel*
    *svm-rbf-g*
    *svm-poly-d*
    *svm-sig-poly-s*
    *svm-sig-poly-r*
    *svm-iterations*
    *svm-cost-balance*
    *svm-error-to-margin*
    *svm-tolerance*))

(defun feature-environment (&key (format :string))
  (labels ((count (bar) (and (numberp bar) (> bar 0) bar)))
      (case format
        (:string
         (let ((counts
                (when (counts-p *feature-frequency-threshold*)
                  (list
                   (count (counts-absolute *feature-frequency-threshold*))
                   (count (counts-contexts *feature-frequency-threshold*))
                   (count (counts-events *feature-frequency-threshold*))
                   (count (counts-relevant *feature-frequency-threshold*)))))
               (ngramp (> *feature-ngram-size* 0))
               (weightp (and (numberp *feature-constituent-weight*)
                             (> *feature-constituent-weight* 0))))
           (if *feature-flags*
             (format
              nil
              "~{~a[~a] ~}~
               FT[~{~@[~a~]~^:~}] RS[~@[~a~]]"
              (loop
                  for flag in *feature-flags*
                  collect (second flag) collect (first flag))
              counts *feature-random-sample-size*)
             (format 
              nil
              "GP[~a] ~:[-~;+~]PT ~:[-~;+~]LEX CW[~@[~a~]] ~
               ~:[-~;+~]AE NS[~a] ~
               NT[~@[~(~a~)~]] ~:[-~;+~]NB LM[~:[0~*~;~a~]] ~
               FT[~{~@[~a~]~^:~}] RS[~@[~a~]]"
              *feature-grandparenting* *feature-use-preterminal-types-p*
              *feature-lexicalization-p*
              (and weightp *feature-constituent-weight*)
              *feature-active-edges-p* *feature-ngram-size*
              (and ngramp *feature-ngram-tag*)
              (and ngramp *feature-ngram-back-off-p*)
              *feature-lm-p* *feature-lm-p* counts
              *feature-random-sample-size*))))
        (:compact
         (let ((ngramp (> *feature-ngram-size* 0)))
           (format  
            nil "~(g~d_p~:[0~;1~]_l~:[0~;1~]_cw~d_a~:[0~;1~]_~
                 n~d_nt~:[0~;1~]_nb~:[0~;1~]_lm~a_c~:[0~;~:*~a~]_~
                 r~:[0~;~:*~a~]_rs~:[0~;~:*~d~]~)"         
            *feature-grandparenting* *feature-use-preterminal-types-p*
            *feature-lexicalization-p* (or *feature-constituent-weight* 0)
            *feature-active-edges-p* *feature-ngram-size*            
            (and ngramp (eq *feature-ngram-tag* :type))
            (and ngramp *feature-ngram-back-off-p*) *feature-lm-p*
            (and (counts-p *feature-frequency-threshold*)
                 (counts-contexts *feature-frequency-threshold*))
            (and (counts-p *feature-frequency-threshold*)
                 (counts-relevant *feature-frequency-threshold*))
            *feature-random-sample-size*)))
        (:list
         (loop
             for key in *feature-options*
             collect (cons key (symbol-value key)))))))

;;fix_me add experiment type to names
(defun mem-environment (&key (format :string) full prefix)
  (let ((features (and full (feature-environment :format format))))
    (case format
      (:string
       (format 
        nil
        "~@[[~a] ~]~@[~a ~]MM[~(~a~)] MI[~@[~a~]] ~
         RT[~@[~e~]] AT[~@[~e~]] VA[~@[~e~]]~
         ~@[~* PC[~a]~]"
        prefix features *maxent-method* *maxent-iterations*
        *maxent-relative-tolerance* *maxent-absolute-tolerance*
        *maxent-variance* full (or *redwoods-train-percentage* 100)))
      (:compact
       (format 
        nil
        "~(~@[~a~]mem_~@[~a_~]rt~e_at~e_v~e~@[_pc~d~]~)"
        prefix features *maxent-relative-tolerance* 
        *maxent-absolute-tolerance* *maxent-variance*
        *redwoods-train-percentage*))
      (:list
       (nconc
        features
        (loop
            for key in *maxent-options*
            collect (cons key (symbol-value key))))))))

(defun svm-environment  (&key (format :string) full prefix)
  (let ((features (and full (feature-environment :format format))))
    (case format
      (:string
       (format 
        nil
        "~@[[~a] ~]~@[~a ~] K[~[lin~;pol~;rbf~;sig~;usr~]]~
         ~@[ G[~a]~]~@[ D[~a]~]~@[ S[~a]~]~@[ R[~a]~]~
         ~@[ IT[~a]~]~@[ B[~a]~]~@[ EM[~a]~]~@[ T[~a]~]"
        prefix features *svm-kernel* *svm-rbf-g* *svm-poly-d*
        *svm-sig-poly-s* *svm-sig-poly-r* *svm-iterations* 
        *svm-cost-balance* *svm-error-to-margin* *svm-tolerance*))
      (:compact
       (format 
        nil
        "~(~@[~a~]svm_~@[~a_~]~[lin~;pol~;rbf~;sig~;usr~]~
         ~@[_g~a~]~@[_d~a~]~@[_s~a~]~@[_r~a~]~@[_it~e~]~
         ~@[_b~a~]~@[_em~a~]~@[_t~a~]~@[_pc~d~]~)"
        prefix features *svm-kernel* *svm-rbf-g* *svm-poly-d*
        *svm-sig-poly-s* *svm-sig-poly-r* *svm-iterations* 
        *svm-cost-balance* *svm-error-to-margin* *svm-tolerance*
        *redwoods-train-percentage*)))))

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
    ((:freeze :export)
     (with-open-file (foo file :direction :output :if-exists :supersede)
       (let  ((stream (or stream foo))
              (table (model-table model)))
         (format 
          stream 
          ";;;~%;;; ~a~%;;; (~a@~a; ~a)~%;;;~%"
          model (current-user) (current-host) (current-time :long :pretty))
         (format stream "~%:begin :model ~d.~%~%" (model-ncontexts model))
         (when (> (model-count model) 0)
           (if *feature-flags*
             (let ((*print-case* :downcase))
               (format
                stream
                "*feature-flags := [~{~a~^ ~}].~%~%"
                (loop for flag in *feature-flags* collect (first flag))))
             (loop
                 with *print-case* = :downcase
                 for key in *feature-options*
                 for value = (let ((foo (symbol-value key)))
                               (cond
                                ((null foo) "no")
                                ((eq foo t) "yes")
                                (t foo)))
                 when (boundp key)
                 do (format stream "~a := ~a.~%~%" key value)))
           (loop
               with *print-case* = :downcase
               for key in *maxent-options*
               when (boundp key)
               do (format stream "~a := ~a.~%~%" key (symbol-value key))))
         (format stream ":begin :features ~d.~%~%" (symbol-table-count table))
         (loop
             with *print-case* = :downcase
             with *package* = (find-package :lkb)
             with map = (model-map model)
             with i = 0
             for code from 0 to (- (symbol-table-count table) 1)
             for symbol = (code-to-symbol code table)
             for weight = (aref (model-weights model) code)
             for counts = (or (aref (model-counts model) code) (make-counts))
             for mapped = (and map (symbol-to-code code map :rop t))
             for minmax = (aref (model-minmax model) code)
             when (or (eq format :freeze) (numberp weight)) do
               (case format
                 (:freeze
                  (format stream "(~d~@[ ~d~]) " code mapped))
                 (:export
                  (format stream "(~d) " i)
                  (incf i)))
               (format
                stream
                "[~{~s~^ ~}] ~:[null~*~;~f~] "
                symbol weight weight)
               (print-object counts stream)
               (when minmax
                 (format
                  stream
                  " [~d ~d]"
                  (first minmax) (second minmax)))
               (format stream "~%"))
               (format stream "~%:end :features.~%~%:end :model.~%"))))))

(defun read-model (file &key (verbose t) id)
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
                       (integerp
                        (setf (model-ncontexts model) (read stream nil nil))))
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
                  (setf (model-minmax model) (make-array n))
                  (setf (model-counts model) (make-array n))
                  (setf (model-weights model) (make-array n)))

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
                (when id (push (cons id model) *models*))
                (return (setf %model% model))
              else when bodyp do
                (unless (and (consp form) (numberp (first form)))
                  (format t "read-model(): invalid codes `~a'.~%" form)
                  (return-from read-model))
                (let* ((symbol (let ((foo (read stream nil nil)))
                                 (unless (consp foo)
                                   (format
                                    t
                                    "read-model(): invalid symbol `~a'.~%"
                                    foo)
                                   (return-from read-model))
                                 foo))
                       (code (first form))
                       (mapped (second form))
                       (weight (read stream nil nil))
                       (counts (read-preserving-whitespace stream nil nil))
                       (minmax (unless (eq #\Newline 
                                           (peek-char nil stream nil nil))
                                 (read stream nil nil))))
                  (set-symbol-and-code symbol code table)
                  (when (numberp mapped)
                    (set-symbol-and-code code mapped (model-map model)))
                  (when (>= code (model-size model))
                    (format 
                     t 
                     "read-model(): mysterious feature overflow (~a vs. ~a).~%"
                     code (model-size model))
                    (return-from read-model))
                  (when (numberp weight)
                    (setf (aref (model-weights model) code) weight)
                    (incf (model-count model)))
                  (setf (aref (model-counts model) code)
                    (make-counts
                     :absolute (first counts) :contexts (second counts)
                     :events (third counts) :relevant (fourth counts)))
                  (when minmax
                    (setf (aref (model-minmax model) code)
                      minmax)))))
        (format t "read-model(): unable to open `~a'.~%" name)))))

(defun estimate-model (items
                       &key (identity (current-pid)) fold
                            (stream *tsdb-io*) model type)
  
  (declare (ignore stream))
  (let* ((model (or model (make-model)))
         (events (format 
                  nil
                  "~a/.model.~a.~a.events"
                  (tmp) (current-user) (current-pid)))
         (trace (format 
                 nil
                 "~a/.model.~a.~a.trace"
                 (tmp) (current-user) (current-pid)))
         (source (get-field :source (first items)))
         (cache (profile-find-context-cache source identity)))
    (unless (model-parameters model)
      (setf (model-parameters model)
        (format 
         nil
         "~a/.model.~a.~a.weights"
         (tmp) (current-user) (current-pid))))
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
                                     "~a/.model.~a.~a.variances"
                                     (tmp) (current-user) (current-pid))))
                          (with-open-file (stream name
                                           :direction :output
                                           :if-exists :supersede)
                            (format stream "~f" *maxent-variance*))
                          name)))
           (command (case type 
                      (:mem
                       (format 
                        nil 
                        "tadm -monitor -events_in ~a -params_out ~a~
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
                      (:perf
                       (format 
                        nil 
                        "svm_perform_learn -v 2 -y 2 -n 10 -q 40 -t 0~
                         ~@[ -# ~a~]~@[ -e ~a~]~@[ -c ~a~] ~a ~a"
                        *svm-iterations* *svm-tolerance*
                        *svm-error-to-margin* events parameters))
                      (:svm
                       (format 
                        nil 
                        "svm_learn -v 2 -n 10 -q 40 -m ~a -z p -t ~a~
                         ~@[ -g ~a~]~
                         ~@[ -d ~a~]~@[ -s ~a~]~@[ -r ~a~]~@[ -# ~a~]~
                         ~@[ -j ~a~]~@[ -e ~a~]~@[ -c ~a~] ~a ~a"
                        *svm-cache-size* *svm-kernel* *svm-rbf-g* *svm-poly-d* 
                        *svm-sig-poly-s* *svm-sig-poly-r* *svm-iterations*
                        *svm-cost-balance* *svm-tolerance*
                        *svm-error-to-margin* events parameters))))
           (output (if *maxent-debug-p* nil "/dev/null")))
      (when (and (zerop (run-process 
                         (format nil "~a | tee '~a'" command trace)
                         :wait t 
                         :output output :if-output-exists :supersede))
                 (probe-file parameters))

        (when (probe-file trace)
          (let ((trace (read-file trace))
                (iterations 0)
                events)
            ;;
            ;; extract number of event and features and count up the number 
            ;; of iterations
            ;;
            (multiple-value-bind (foo matches)
                (ppcre::scan-to-strings "\\nClasses    = ([0-9]+)" trace)
              (declare (ignore foo))
              (when matches
                (setf events
                  (parse-integer (aref matches 0) :junk-allowed t))))
            (ppcre:do-matches
                (start end
                 "\\n *[0-9]+    [0-9.e+-]+    [0-9.e+-]+    [0-9.e+-]+"
                 trace nil)
              (incf iterations))
            (nconc
             fold
             (pairlis '(:f-events :f-iterations :f-estimation)
                      (list events iterations trace)))))
        (when (and (eq type :mem)
                   (probe-file parameters))
          (nconc
           fold
           (acons :f-features (get-field :lines (wc parameters)) nil)))
        (unless *maxent-debug-p*
          (ignore-errors (delete-file trace))
          (ignore-errors (delete-file events))
          (ignore-errors (delete-file parameters)))
        model))))

(defun read-weights (model &optional (file (model-parameters model)))
  (with-open-file (stream file :direction :input :if-does-not-exist nil)
    (when stream
      (loop
          for i from 0
          for code = (if (model-map model)
                       (code-to-symbol i (model-map model))
                       i)
          for weight = (read stream nil nil)
          while weight do
            (when (>= code (model-size model))
              (let ((n (setf (model-size model)
                         (* (model-size model) 2))))
                (setf (model-counts model)
                  (adjust-array (model-counts model) n))
                (setf (model-weights model)
                  (adjust-array (model-weights model) n))))
            (setf (aref (model-weights model) code) weight)
            (incf (model-count model))))))

(defun learner-rank-items (items model 
                           &key (identity (current-pid)) fold 
                                type (stream *tsdb-io*))

  (let* ((parameters (model-parameters model))
         (events (format 
                  nil
                  "~a/.model.~a.~a.events"
                  (tmp) (current-user) (current-pid)))
         (source (get-field :source (first items)))
         (cache (profile-find-context-cache source identity))
         active)
    (when (null parameters)
      (format t "learner-rank-items(): invalid model: no parameters.~%")
      (return-from learner-rank-items))
    (format
     stream
     "~&[~a] learner-rank-items(): evaluating ~d item~p ~%"
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
          when (and (> readings 1) (probe-file file))
          do (cp file out) (push item active)
          else 
          do (format
              t
              "~&[~a] learner-rank-items(): mysteriously skipping item # ~d.~%"
              (current-time :long :short) (get-field :i-id item))))
    (setf active (nreverse active))
    (let* ((scores (format 
                    nil
                    "~a/.model.~a.~a.scores"
                    (tmp) (current-user) (current-pid)))
           (output (format 
                    nil
                    "~a/.model.~a.~a.output"
                    (tmp) (current-user) (current-pid)))
           (command 
            (case type
              (:mem            
               (format
                nil
                "evaluate -s '~a' '~a' '~a'"
                scores parameters events))
              (:perf
               (format
                nil 
                "svm_perform_classify ~a ~a ~a" 
                events parameters scores))
              (:svm 
               (format
                nil 
                "svm_classify '~a' '~a' '~a'" 
                events parameters scores)))))
      (when (and (zerop (run-process 
                         command :wait t 
                         :output output :if-output-exists :supersede))
                 (probe-file scores) (probe-file output))
        (format
         t
         "~&[~a] learner-rank-items(): ranking ~d item~p ~%"
         (current-time :long :short) (length active) (length active))
        (with-open-file (stream scores :direction :input)
          (loop
              for item in active
              for results = (get-field :results item)
              for ranks
              = (loop
                    for result in results
                    for rid = (get-field :result-id result)
                    for score = (read stream nil nil)
                    unless score do
                      (error "learner-rank-items(): mysterious score deficit")
                    collect (pairlis '(:result-id :score) (list rid score)))
              do
                (let* ((ranks (sort
                               ranks 
                               #'>
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
          (when (eq type :mem)
            (with-open-file (stream output :direction :input)
              (let ((line (read-line stream nil nil)))
                (when line
                  (multiple-value-bind (foo matches)
                      (ppcre:scan-to-strings "([0-9.]+) [0-9]*$" line)
                    (declare (ignore foo))
                    (when matches
                      (let ((accuracy (acons :eaccuracy (aref matches 0) nil)))
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

(defun baseline (profile &key condition (n 1) (resolvedp t) enhancers)
  (loop
      with nitems = 0
      for item
      in (loop
             for item
             in (let* ((condition
                        (if resolvedp
                          (if condition 
                            (format
                             nil
                             "readings > 1 && t-active >= 1 && (~a)"
                             condition)
                            "readings > 1 && t-active >= 1")
                          (if condition
                            (format nil "readings > 1 && (~a)" condition)
                            "readings > 1")))
                       (items
                        (analyze
                         profile :thorough '(:flags)
                         :condition condition :gold profile)))
                  (loop
                      for enhancer in enhancers
                      do
                        (loop
                            for item in items
                            do (call-raw-hook enhancer item)))
                  items)
             for readings = (length (get-field :results item))
             for ranks = (length (get-field :ranks item))
             unless (= readings ranks)
             collect item and do (incf nitems))
      for readings = (length (get-field :results item))
      for gold = (max n (length (get-field :ranks item)))
      sum gold into gsum
      sum readings into rsum
      sum (/ gold readings) into sum 
      finally
        (return (list (float (/ sum nitems))  ;;average random chance
                      (float (/ gsum nitems)) ;;average # gold
                      (float (/ rsum nitems)) ;;average # results
                      nitems))))              ;;# items

(defun print-score-file (&key (output (format nil "~a/scores" (tmp)))
                              gold name pattern condition
                              (similarities '(:bleu)))
  (with-open-file (stream output :direction :output :if-exists :supersede)
    (loop
        with *redwoods-score-similarities* = similarities
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
                 (tsims (get-field :tsimilarities total))
                 (nsims (get-field :nsimilarities total)))
;;;;             (bleu (get-field :bleu (get-field :similarities total))))
            (purge-profile-cache name)
            (format
             stream
             "~,6f ~a ~a `~a'~%"
;;;;         "~,6f ~,6f `~a'~%"
             (* 100 (divide exact nscores))
             tsims nsims
;;;;         (if bleu (divide bleu nscores) 0.0)
             name)
            (force-output stream))))
  (purge-profile-cache gold))

(defun summarize-folds (&key (output (format nil "~a/folds" (tmp)))
                             name pattern (score :accuracy) (type :total))
  (with-open-file (stream output :direction :output :if-exists :supersede
                          :if-not-exists :create)
    (let* ((key (if (and (eq type :total)
                         (eq score :accuracy))
                    :f-accuracy :f-extras))
           (foo (case type 
                  (:total :tsimilarities) 
                  (:nbest :nsimilarities)))
           (selector (cond ((eq :f-accuracy key)
                            #'(lambda (scores)
                                (read-from-string 
                                 (get-field key scores) nil)))
                           ((and (eq type :nbest)
                                 (eq score :accuracy))
                            #'(lambda (scores)
                                (get-field 
                                 :naccuracy
                                 (read-from-string 
                                  (get-field key scores) nil))))
                           (t
                            #'(lambda (scores)
                                (get-field 
                                 score (get-field 
                                        foo (read-from-string 
                                             (get-field key scores) nil))))))))
      (loop
          for profile in (cond
                          ((not (null pattern))
                           (mapcar #'(lambda(db) (get-field :database db))
                                   (find-tsdb-directories 
                                    *tsdb-home* :pattern pattern :name name)))
                          ((and name (listp name)) name)
                          ((stringp name) (list name))
                          (t (error "summarize-folds():~
                                     name or pattern argument missing.")))
          for values = (select (list (format nil "~(~a~)" key))
                         '(:string) "fold" nil profile)
          for scores = (when values (map 'list selector values))
          when (and scores (notany 'null scores))
          do
            (let* ((n (length scores))
                   (sum (sum scores))
                   (mean (/ sum n))
                   (min (apply #'min scores))
                   (max (apply #'max scores))
                   (range (- max min))
                   (var (if (= n 1) 0 
                          (/ (sum 
                              (mapcar 
                               #'(lambda (x) 
                                   (expt (- x mean) 2))
                               scores))
                             (- n 1))))
                   (std-dev (sqrt var)))
         (purge-profile-cache profile)
         (format stream "~,6f ~,6f ~,6f `~a'~%" 
                 mean std-dev range profile)
         (force-output stream))))))

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
    (values 
     ;;; t ratio:
     ;;; (Use this when consulting a table of Student's t-distribution
     ;;; confidence intervals to determine the significance level at which two
     ;;; distributions differ)
     (abs result) 
     ;;; degrees of freedom:
     (- n 1))))

(defun mw-t-test (list1 list2)
  (let* ((n1 (length list1))
         (n2 (length list2))
         (n (if (not (= n1 n2))
                (error  "mw-t-test(): given lists of different lengths:~
                         ~a and ~a~%" list1 list2)
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
    '((0.10     (3.078  1.886  1.638 1.533 1.476 1.44  1.415 1.397 1.383 1.372 
                 1.363  1.356  1.35  1.345 1.341 1.337 1.333 1.33  1.328 1.325))
      (0.05     (6.314  2.92   2.353 2.132 2.015 1.943 1.895 1.86  1.833 1.812 
                 1.796  1.782  1.771 1.761 1.753 1.746 1.74  1.734 1.729 1.725))
      (0.025   (12.706  4.303  3.182 2.776 2.571 2.447 2.365 2.306 2.262 2.228 
                 2.201  2.179  2.16  2.145 2.131 2.12  2.11  2.101 2.093 2.086))
      (0.01    (31.821  6.965  4.541 3.747 3.365 3.143 2.998 2.896 2.821 2.764
                 2.718  2.681  2.65  2.624 2.602 2.583 2.567 2.552 2.539 2.528))   
      (0.005   (63.657  9.925  5.841 4.604 4.032 3.707 3.499 3.355 3.25  3.169 
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

(defun create-evaluation-file (data &optional (gold data) 
                              &key (condition "readings > 1 && t-active > 0")
                                   (n 5) (test :id) supersede
                                   (loosep t) (stream *tsdb-io*)
                                   (similarities '(:neva :wa)))

  (let* ((compress-command "gzip -c -9")
         (tsdb-dir (find-tsdb-directory data))
         (eval (make-pathname :directory tsdb-dir 
                              :name "eval"))
         (eval-gz (make-pathname :directory tsdb-dir 
                                 :name "eval" :type "gz")))
    
    ;;; clean up:
    (when (or (cl-fad:file-exists-p eval-gz)
              (cl-fad:file-exists-p eval))
      (if (not supersede) 
          (return-from create-evaluation-file)
        (progn 
          (when (cl-fad:file-exists-p eval-gz)
            (delete-file eval-gz))
          (when (cl-fad:file-exists-p eval)
            (delete-file eval)))))
    
        (format 
         stream 
         "~&[~a] create-evaluation-file(): creating `eval.gz' for ~a~%"
         (current-time :long :short) data)
    
    (multiple-value-bind (eval-stream foo pid)
        (run-process
         compress-command :wait nil :input :stream
         :output eval-gz :if-output-exists :supersede
         :error-output nil)
      (declare (ignore foo))
                      
      (let* ((*redwoods-score-similarities* similarities) 
             (thorough (when (eq test :derivation) '(:derivation)))
             (thorough (if *redwoods-score-similarities*
                           (cons :surface thorough)
                         thorough))
             (gitems-unsifted (analyze gold
                                       :thorough thorough
                                       :condition condition :gold gold 
                                       :readerp (eq test :derivation)))
             (items (loop 
                        for item in 
                          (analyze gold
                                   :thorough thorough 
                                   :condition condition 
                                   :score data :scorep t
                                   :readerp (eq test :derivation))
                        for gitem in gitems-unsifted
                        for readings = (length (get-field :results gitem))
                        for ranks = (length (get-field :ranks gitem))
                        unless (or (= readings ranks)
                                   (not (= (get-field :i-id gitem) 
                                           (get-field :i-id item))))
                        collect (copy-tree item)))
             (gitems (loop  
                         for item in gitems-unsifted
                         for readings = (length (get-field :results item))
                         for ranks = (length (get-field :ranks item))
                         unless (= readings ranks)
                         collect (copy-tree item)))
             (nkeys (loop for sim in similarities
                        collect ;;; eg., (:nwa :nbleu :nneva) 
                          (read-from-string 
                           (format nil ":n~a" sim))))
             (nkey-alist (pairlis similarities nkeys))
             (keys (append 
                    (list :i-id :accuracy :naccuracy);; :r-id
                    similarities nkeys))
             (data (make-list (length keys)))
             (scores (pairlis keys data)))
        (loop
            for item in items
            for gitem in gitems
            for i-id = (get-field :i-id item)
            do 
              (multiple-value-bind (i score loosep similarities)
                  (score-item item gitem 
                              :test test :n n :loosep loosep)
;;;                     (= i 0)  means no match
;;;                     (<= i n) means we have a hit
;;;                     (= i 1)  means exact match              
                (declare (ignore loosep))
                (push (if (= i 1) (float score) 0) 
                      (get-field :accuracy scores))
                (push (if (<= i n) (float score) 0) 
                      (get-field :naccuracy scores))
                (push i-id (get-field :i-id scores))
                (loop
                    for (key score nscore) in similarities
                    do (push (float score) 
                             (get-field key scores))
                       (push (float nscore) 
                             (get-field 
                              (get-field key nkey-alist) scores)))))
        (loop 
            for (key . list) in scores
            do (setf (get-field key scores)
                 (nreverse list)))
        (loop 
            for list in scores
            do (prin1 list eval-stream)
               (terpri eval-stream)))
    
    (force-output eval-stream)
    (close eval-stream)
    (sys:os-wait nil pid))))

(defun batch-create-evaluation-files (&key pattern supersede gold n
                                (condition "readings > 1 && t-active > 0")
                                (similarities *redwoods-score-similarities*))
  (loop
      for db in 
        (find-tsdb-directories *tsdb-home* :pattern pattern)
      for name = (let ((name (get-field :database db)))
                   (unless (string= name gold) name))
      when name do
        (create-evaluation-file name gold 
                               :condition condition
                               :n n :supersede supersede
                               :similarities similarities)
        (purge-profile-cache name))
  (purge-profile-cache gold))

(defun read-evaluation-file (profile &key (score :accuracy))
  (let* ((tsdb-dir (find-tsdb-directory profile))
         (eval (make-pathname :directory tsdb-dir 
                              :name "eval"))
         (eval-gz (make-pathname :directory tsdb-dir 
                                 :name "eval" :type "gz"))
         (did-unzip-p nil))

    (when (and (not (cl-fad:file-exists-p eval-gz))
               (not (cl-fad:file-exists-p eval)))
      (error "read-evaluation-file(): `eval(.gz)' does not exists for `~a'." 
             profile))
    
    (when (cl-fad:file-exists-p eval-gz)
   ;;;allegro specific..
      (excl:run-shell-command 
       (format nil "gunzip ~a" eval-gz) :wait t)
      (setq did-unzip-p t))
    
    (if (not (cl-fad:file-exists-p eval))
        (error "read-evaluation-file(): cannot find `eval' file for `~a'." 
               profile)
      (with-open-file (in eval :direction :input)
        (loop 
            for list = (read in nil nil)
            until (or (null list)
                      (eq (first list) score))
            finally (progn 
                      (when did-unzip-p 
                        (excl:run-shell-command 
                         (format nil "gzip -9 ~a" eval) :wait t))
                      (return (rest list))))))))

(defun summarize-evaluation-file (profile &optional (stream *tsdb-io*))
  (let* ((tsdb-dir (find-tsdb-directory profile))
         (eval (make-pathname :directory tsdb-dir 
                              :name "eval"))
         (eval-gz (make-pathname :directory tsdb-dir 
                                 :name "eval" :type "gz"))
         (did-unzip-p nil))
    
    (when (and (not (cl-fad:file-exists-p eval-gz))
               (not (cl-fad:file-exists-p eval)))
      (error "summarize-evaluation-file(): `eval(.gz)' does not exists for `~a'." 
             profile))
    
    (when (cl-fad:file-exists-p eval-gz)
   ;;;allegro specific
      (excl:run-shell-command 
       (format nil "gunzip ~a" eval) :wait t)
      (setq did-unzip-p t))
    
    (if (not (cl-fad:file-exists-p eval))
        (error "summarize-evaluation-file(): cannot find `eval' file for `~a'." 
               profile)
      (with-open-file (in eval :direction :input)
        (loop 
            with total-items 
            for list = (read in nil nil)
            until (null list)
            do (case (car list)
                 ((:accuracy :naccuracy )
                  (format stream "~&~a: ~,2f~%" 
                          (car list)
                          (* (/ (sum (cdr list))
                                (length (cdr list)))
                             100)))
                 (:i-id
                  (setq total-items (length (cdr list))))
                 (t
                  (format stream "~&~a: ~,3f~%" 
                          (car list)
                          (/ (sum (cdr list))
                             (length (cdr list))))))
            finally (format stream "~&# Items: ~a~%" 
                            total-items))))
    (when did-unzip-p 
      (excl:run-shell-command 
       (format nil "gzip -9 ~a" eval) :wait t)))
  nil)

(defun test-evaluation-scores (profile1 profile2 
                               &key (score :accuracy) (test :signtest)
                                    (tails :both))
  (let ((scores1 (read-evaluation-file profile1 :score score))
        (scores2 (read-evaluation-file profile2 :score score)))
    
    (case test
      ((:signtest :binomial)
       (stats:sign-test-on-sequences 
        scores1 scores2 :tails tails))
      ((:wilcoxon :signed-rank :wilcoxon-signed-rank)
       (stats:wilcoxon-signed-rank-test-on-sequences 
        scores1 scores2 :tails tails))
      ((:ttest :paired-ttest)
       (stats:t-test-paired-on-sequences 
        scores1 scores2 :tails tails))
      (t
       (error  "test-evaluation-scores(): unknown test `~a'." test)))))
