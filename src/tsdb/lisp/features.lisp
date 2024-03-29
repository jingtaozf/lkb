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
;;; some notes on the encoding of features: to make sure we operate in separate
;;; namespaces for the various feature templates, each feature is prefixed with
;;; an integer identifying the feature type (aka template), viz.
;;;
;;;   1: local derivational configuration; a subtree of depth one taken from 
;;;      the derivation; e.g. [1 (0) hspec det_poss_my_le noptcomp]; the second
;;;      integer indicates the amount of grandparenting used, where parent 
;;;      labels will precede the root of the local configuration.
;;;   2: `active' local derivational configuration; similar to type 1, but with
;;;      only one of the daughters in each feature; thus, for a ternary rule,
;;;      feature type 2 will add three extra features, 
;;;      e.g. [2 (0) hspec noptcomp].
;;;   3: lexicalized local derivational configuration; much like type 1, but
;;;      the first symbol is the lexical head of the root of the configuration;
;;;      if we decided to lexicalize _all_ nodes in the feature, that would be
;;;      either a different template or additional parameter then.
;;;   4: lexicalized `active' local derivational configuration; one parameter,
;;;      viz. degree of grand-parenting.
;;;  10: n-gram features; e.g. [10 (3 1) "saw" ^ n_proper_le v_np_trans_le]; 
;;;      the first parameter is the n-gram size, the second an integer coding 
;;;      as to what is used in the n-grams: 0 -- lexical identifier, 1 -- le 
;;;      type.
;;;  11: preterminal-only n-gram features; like type 10, but without a surface
;;;      form; e.g. [11 (3 1) ^ n_proper_le v_np_trans_le].
;;;  42: language model score; the second integer is the number of bins used 
;;;      (if any), and the third the divisor used in scaling (*maxent-lm-p*), 
;;;      if any, e.g. [42 (0) -12.34].
;;;  43: experimental features in combining LOGON scores across components.
;;;
;;; also, we are using some pseudo-features to record additional information in
;;; the feature cache, viz.
;;;
;;;  -1: event frequencies; takes one parameter encoding the frequency
;;;      assignment function: 0 -- binary, i.e. 0 or 1 according to what comes
;;;      out of the annotations in the treebank; 1 and upwards -- various
;;;      weighted frequency functions, according to the global value of
;;;      *maxent-preference-weightings*.
;;;

(in-package :tsdb)

(defparameter *feature-grandparenting* 4)

(defparameter *feature-use-preterminal-types-p* t)

(defparameter *feature-lexicalization-p* t)

(defparameter *feature-constituent-weight* 2)

(defparameter *feature-active-edges-p* t)

(defparameter *feature-ngram-size* 4)

(defparameter *feature-ngram-tag* :type)

(defparameter *feature-ngram-back-off-p* t)

(defparameter *feature-lm-p* #-:logon nil #+:logon 10)

#+:null
(defparameter *feature-lm-normalize* '(:minmax 0 2))

(defparameter *feature-dependencies* nil)

(defparameter *feature-flags*
  (nconc
   #+:null
   '((0 :ascore) (1 :tscore) (2 :rscore))
   #+:null
   '((3 :lm) (4 :perplexity) (5 :lfn) (6 :lnf))
   #+:null
   '((10 :distortion) (11 :distance))
   #+:null
   '((12 :nmtrs) (13 :tratio) (14 :ratio))))

#+:onet
(defparameter *feature-custom* nil)

(defparameter *feature-preference-weightings*
  '((0 :binary) (1 :bleu) (2 :wa) (3 :waft)))

(defparameter *feature-frequency-threshold* nil)

(defparameter *feature-random-sample-size* nil)

(defparameter *feature-item-enhancers* (list 'lm-item-enhancer))

(defstruct counts
  (absolute 0) (contexts 0) (events 0) (relevant 0))

(defmethod print-object ((object counts) stream)
  (format 
   stream 
   "{~d ~d ~d ~d}"
   (counts-absolute object) (counts-contexts object)
   (counts-events object) (counts-relevant object)))

(defmacro counts>= (counts1 counts2)
  `(and (>= (counts-absolute ,counts1) (counts-absolute ,counts2))
        (>= (counts-contexts ,counts1) (counts-contexts ,counts2))
        (>= (counts-events ,counts1) (counts-events ,counts2))
        (>= (counts-relevant ,counts1) (counts-relevant ,counts2))))

(defmacro counts= (counts1 counts2)
  `(and (= (counts-absolute ,counts1) (counts-absolute ,counts2))
        (= (counts-contexts ,counts1) (counts-contexts ,counts2))
        (= (counts-events ,counts1) (counts-events ,counts2))
        (= (counts-relevant ,counts1) (counts-relevant ,counts2))))

(defparameter *feature-options*
  '(*feature-grandparenting*
    *feature-use-preterminal-types-p*
    *feature-lexicalization-p*
    *feature-constituent-weight*
    *feature-active-edges-p*
    *feature-ngram-size*
    *feature-ngram-tag*
    *feature-ngram-back-off-p*
    *feature-lm-p*
    *feature-frequency-threshold*
    *feature-random-sample-size*
    #+:onet *feature-custom*))

(defparameter *feature-float-valued-tids* '(42 43 -1))

(defconstant %feature-frequency-tid% -1)

(defstruct (model)
  (table (make-symbol-table :test #'equal))
  (map (make-symbol-table :test #'eql))
  contexts (ncontexts 0)
  (counts (make-array 512))
  (weights (make-array 512))
  (count 0)
  (size 512)
  parameters
  stream id task
  (minmax (make-array 512)))

(defmethod print-object ((object model) stream)
  (format 
   stream 
   "#[MODEL (~d context~p; ~d weight~p)]"
   (model-ncontexts object) (model-ncontexts object)
   (model-count object) (model-count object)))

;;;
;;; the `counts' vector in the model is a quadruple, containing per feature:
;;;
;;; - the absolute frequency of occurence for this feature
;;; - the total number of contexts that exhibit this feature
;;; - the total number of events that exhibit this feature
;;; - the number of times the feature is `relevant' in a context, i.e. there
;;;   are (at least) two events for which the count (aka `frequency' or value,
;;;   even) of this feature differs; see (van Noord & Malouf, 2004).
;;;

(defstruct (feature) 
  code
  mapped
  tid
  parameters
  symbol
  (count 1))

(defstruct (event) 
  id 
  frequency
  features 
  (size 0))

(defstruct (context) 
  id 
  (size 0)
  events
  tail)

(defun print-context (context &key (stream t) (format :rpm) model sample)
  (declare (ignore model))
  (case format
    ((:mem :rpm)
     (format
      stream
      "~d~%"
      (if sample (length sample) (context-size context)))))
  (case format
    ((:mem :svm :perf :rpm)
     (loop
         with iid = (context-id context)
         for event in (context-events context)
         when (or (not sample) (member (event-id event) sample :test #'=))
         do 
           (print-event
            event :stream stream :format format :iid iid)))))

(defun print-event (event &key (stream t) (format :rpm) iid)

  (macrolet ((fwrite (object stream)
               `(typecase ,object
                  #+:allegro
                  (fixnum
                   (if (minusp ,object)
                     (excl::print-negative-fixnum ,stream 10 ,object)
                     (excl::print-fixnum ,stream 10 ,object)))
                  (t
                   (write ,object :stream ,stream)))))
    (case format
      ((:mem :rpm)
       (fwrite (event-frequency event) stream)
       (write-char #\Space stream)
       (fwrite (event-size event) stream)
       (loop
           for feature in (event-features event)
           for code = (or (feature-mapped feature) 
                          (feature-code feature))
           do
             (write-char #\Space stream)
             (fwrite code stream)
             (write-char #\Space stream)
             (fwrite (feature-count feature) stream)))
      ((:svm :perf)
       (format stream "~,1f qid:~a" 
               (cond ((eq format :svm) 
                      (event-frequency event))
                     ((and (eq format :perf)
                           (zerop (event-frequency event))) -1)
                     (t 1))
               iid)
       (loop
           for feature in (event-features event)
           ;;;feature numbers run from 1 in svm_light (from 0 in tadm)
           for code = (+ 1 (or (feature-mapped feature) 
                               (feature-code feature)))
           do
             (write-char #\Space stream)
             (fwrite code stream)
             (write-char #\Colon stream)
             (fwrite (feature-count feature) stream)
           finally 
             (format stream " # ~a" (event-id event)))))
    (terpri stream)))

(defun print-feature (feature &key (stream t) (format :compact))
  (case format
    (:compact
     (format
      stream
      "[~a ~a {~{~a~^ ~}} ~a]"
      (feature-code feature) (feature-tid feature)
      (feature-parameters feature) (feature-count feature)))))

(defun read-feature (stream &key (format :compact))
  (case format
    (:compact
     (let* ((*readtable* (copy-readtable nil))
            (c (read-char stream nil nil)))
       (set-syntax-from-char #\[ #\( *readtable*)
       (set-syntax-from-char #\] #\) *readtable*)
       (set-syntax-from-char #\{ #\( *readtable*)
       (set-syntax-from-char #\} #\) *readtable*)
       (when (and c (char= c #\[))
         (let* ((code (read stream nil nil))
                (tid (and code (read stream nil nil)))
                (c (peek-char #\{ stream nil nil)))
           (when (and tid c)
             (read-char stream nil nil)
             (let ((parameters (read-delimited-list #\} stream))
                   (count (read stream nil nil))
                   (c (peek-char #\] stream nil nil)))
               (when (and count c)
                 (read-char stream nil nil)
                 (make-feature
                  :code code :tid tid
                  :parameters parameters :count count))))))))))

(declaim (inline record-feature))

(defun record-feature (feature event &optional model &key rop)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))

  ;;
  ;; there are two distinct contexts in which record-feature() gets called:
  ;; (a) when building the feature cache, features have not yet been assigned
  ;; a unique code, and we want to determine their usage counts.  conversely,
  ;; (b) when filling the context cache, features have been encoded (i.e. have
  ;; their unique code) but for increased efficiency, we want to map the codes
  ;; of features that actually get used (i.e. the active sub-set of features
  ;; from the complete feature cache) into a consecutive integer range.  for
  ;; the latter, the model provides a `map'ping symbol table.
  ;;
  
  ;;
  ;; for features that are encoded already and have a frequency count below the
  ;; current cut-off threshhold, return immediately, i.e. ignore this .feature.
  ;;
  (let ((code (feature-code feature)))
    (unless (or (null model) (null code) (null *feature-frequency-threshold*)
                (counts>= (aref (model-counts model) code)
                          *feature-frequency-threshold*))
      (return-from record-feature)))
  
  ;;
  ;; for features that are encoded already, map their code into a new range
  ;;
  (when (and model (not rop)
             (symbol-table-p (model-map model)) (feature-code feature)) 
    (setf (feature-mapped feature)
      (symbol-to-code (feature-code feature) (model-map model))))

  ;;
  ;; encode this feature, unless it has been assigned a unique identifier (with
  ;; respect to .model. already).
  ;;
  (unless (or (null model) (feature-code feature))
    (setf (feature-code feature)
      (symbol-to-code
       (list* (feature-tid feature) (feature-parameters feature)
              (feature-symbol feature))
       (model-table model)))

    ;;
    ;; at this point, ditch the symbolic representation of .feature. (which is
    ;; now recoverable from the symbol table in the .model.)
    ;;
    (setf (feature-symbol feature) nil)

    ;;
    ;; also, make sure the vectors in .model. indexed by codes grow in size as
    ;; we add features.
    ;;
    (when (and (not rop) (>= (feature-code feature) (model-size model)))
      (let ((n (setf (model-size model)
                 (max (+ (feature-code feature) 1) (* (model-size model) 2)))))
        (#+:allegro excl:tenuring #-:allegro progn
         (setf (model-minmax model) (adjust-array (model-minmax model) n))
         (setf (model-counts model) (adjust-array (model-counts model) n))
         (setf (model-weights model)
           (adjust-array (model-weights model) n))))))
  
  ;;
  ;; from here on, use either the original .code. or the .mapped. value as the
  ;; identifier for inserting .feature. into .event.
  ;;
  (let* ((id (if (feature-mapped feature) #'feature-mapped #'feature-code))
         (code (funcall id feature)))    
    ;;
    ;; the .features. storage in events, naturally, is organized as a simple,
    ;; ordered list, sorted by feature codes.  thus, the code below searches 
    ;; linearly through the list and either increments the counter for 
    ;; features that are present already, or inserts appropriately.
    ;;
    (cond
     ((or (zerop (event-size event)) 
          (< code (funcall id (first (event-features event)))))
      (push feature (event-features event))
      (incf (event-size event)))
     (t
      (loop
          for features on (event-features event)
          for this = (first features)
          for next = (first (rest features))
          when (= (funcall id this) code) do
            (incf (feature-count this) (feature-count feature))
            (return)
          else when (or (null next) (< code (funcall id next))) do
            (setf (rest features) (cons feature (rest features)))
            (incf (event-size event))
            (return))))))  

(defun record-features (features event &optional model)
  (loop
      for feature in features
      do (record-feature feature event model)))

(defun record-context (context model &key (format :rpm))
  (incf (model-ncontexts model))
  (if (open-stream-p (model-stream model))
    (print-context 
     context :stream (model-stream model) :model model :format format)
    (push context (model-contexts model))))

(defun normalize-features-n (event &key model (type :minmax))
  (case type
    ((:euclidean :length)
     (loop 
         with sum = (loop
                        for feature in (event-features event)
                        sum (expt (feature-count feature) 2))
         for feature in (event-features event)
         do
           (setf (feature-count feature) 
             (sqrt (/ (expt (feature-count feature) 2) sum)))))
    (:minmax
     (loop 
         for feature in (event-features event)
         for code = (feature-code feature)
         for minmax
         = (and (< code (model-size model)) (aref (model-minmax model) code))
         for min = (first minmax)
         for max = (second minmax)
         for normalized
         = (and minmax (/ (- (feature-count feature) min) (- max min)))
         when normalized do
           (setf (feature-count feature) 
             (if (typep normalized 'ratio) (float normalized) normalized)))))
  event)

(defun euclidean-length-features (event)
  (sqrt 
   (loop 
       with features = (event-features event)
       for feature in features
       sum (expt (feature-count feature) 2))))

(defun record-event (event context &key model normalizep)
  (when normalizep
    (normalize-features-n event :type normalizep :model model))
  (let ((cons (cons event nil)))
    (if (null (context-events context))
      (setf (context-events context) cons)
      (setf (rest (context-tail context)) cons))
    (setf (context-tail context) cons))
  (incf (context-size context)))

;;;
;;; an extension to the original model: populate a feature cache per profile to
;;; later be able to retrieve features without having to handle the original
;;; (heavy-duty) data.
;;;
(defun cache-features (items model &key (stream *tsdb-io*) verbose createp)
  
  #+:debug
  (setf %items items %model model)

  (unless items (return-from cache-features))
  
  (loop
      with *feature-frequency-threshold* = nil
      with source = (get-field :source (first items))
      with cache = (profile-find-feature-cache source :write :createp createp)
      for item in items
      for counts = (make-hash-table :test #'eql)
      for db = (let ((foo (get-field :source item)))
                     (cond
                      ((string= source foo) cache)
                      (t
                       (setf source foo)
                       (close-fc cache)
                       (setf cache
                         (profile-find-feature-cache
                          source :write :createp createp)))))
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 0)) do
        (when *feature-dependencies*
          ;;
          ;; when MRS-derived features are active, then reconstruct() needs to
          ;; actually re-build complete AVMs, in which case a lot of garbage
          ;; is generated, so we need to deliberately release quasi-destructive
          ;; intermediate structures; this was hard to debug.   (21-nov-07; oe)
          ;;
          (lkb::release-temporary-storage))

	(loop
	    with i = 0
            with *reconstruct-cache* = (make-hash-table :test #'eql)
	    for result in (get-field :results item)
	    for rid = (get-field :result-id result)
	    for event = (result-to-event result model)
            unless event do
              (format
               stream
               "~&[~a] cache-features(): ~
                ignoring item # ~d (no features for ~d)~%"
               (current-time :long :short) iid rid)
	      (return)
	    else do
              (loop
                  for feature in (event-features event)
                  for code = (feature-code feature)
                  for count = (or (gethash code counts)
                                  (setf (gethash code counts) (list 0 0)))
                  do
                    (incf (first count) (feature-count feature))
                    (incf (second count))
                    (pushnew (feature-count feature) (rest (rest count))))
                 
              (loop
                  for (i . type) in *feature-preference-weightings*
                  for count = (float (weigh-result item result type))
                  for feature = (make-feature
                                 :tid %feature-frequency-tid%
                                 :parameters (list i)
                                 :symbol type
                                 :count count)
                  do (record-feature feature event model))
              (loop
                  for feature in (event-features event)
                  do (store-feature db iid rid feature))
              (incf i)
              ;;
              ;; keep track of min/max feature values for later normalization.
              ;;
              (loop
                  with minmaxes = (model-minmax model)
                  for feature in (event-features event)
                  for code = (feature-code feature)
                  for count = (feature-count feature)
                  for minmax = (aref minmaxes code)
                  when (null minmax)
                  do (setf minmax (setf (aref minmaxes code) (list 0 0)))
                  when (< count (first minmax))
                  do (setf (first (aref minmaxes code)) count)
                  when (> count (second minmax))
                  do (setf (second (aref minmaxes code)) count))
	    finally
              (loop
                  for code being each hash-key
                  using (hash-value count) in counts
                  for match = (or (aref (model-counts model) code)
                                  (setf (aref (model-counts model) code)
                                    (make-counts)))
                  do
                    (incf (counts-absolute match) (abs (first count)))
                    (incf (counts-contexts match))
                    (incf (counts-events match) (second count))
                    ;;
                    ;; we call a feature `relevant' in this context, when there
                    ;; are at least two events for which either (a) the feature
                    ;; has different values or (b) only one of the two events
                    ;; actually has the feature.
                    ;;
                    (when (or (rest (rest (rest count)))
                              (< (second count) i))
                      (incf (counts-relevant match))))
              (incf (model-ncontexts model))
              (when verbose
                (format 
                 stream
                 "~&[~a] cache-features(): item # ~d: ~d event~p;~%" 
                 (current-time :long :short) iid i i)))
      finally
        (close-fc db)))

(defun cache-contexts (items model
                       &optional (identity -1)
                       &key (format :rpm) (stream *tsdb-io*) normalizep)

  ;;
  ;; _fix_me_
  ;; most of the record-xyz() functions used to assume we had a model; maybe
  ;; rethink that part, or always carry around a model here too?
  ;;                                                      (5-jul-05; erik & oe)
  (loop
      with source = (get-field :source (first items))
      with fcs = (list source)
      with fc = (profile-find-feature-cache source :read)
      with cc = (profile-find-context-cache source identity :createp t)
      for item in items
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 1))
      do
        (let ((foo (get-field :source item)))
          (unless (string= source foo)
            (setf source foo)
            (close-fc fc)
            (let ((createp (not (member source fcs :test #'string=))))
              (setf fc (profile-find-feature-cache source :read))
              (pushnew source fcs :test #'string=)
              (setf cc
                (profile-find-context-cache
                 source identity :createp createp)))))
        (loop
            with results = (get-field :results item)
            with n = (length results)
            with low = (when *feature-random-sample-size*
                         ;;
                         ;; determine whether result ids are 0- or 1-based.
                         ;;
                         (loop
                             for result in (get-field :results item) 
                             minimize (get-field :result-id result)))
            with active = (when *feature-random-sample-size*
                            ;;
                            ;; find active (aka preferred) results, in order to
                            ;; make sure all of them are in the random sample.
                            ;;
                            (loop
                                for result in results
                                for rid = (get-field :result-id result)
                                for feature = (first
                                               (retrieve-features
                                                fc iid rid
                                                %feature-frequency-tid%
                                                (list 0)))
                                when (and feature
                                          (= (feature-count feature) 1))
                                collect rid))
            with sample = (when (and *feature-random-sample-size*
                                    (< *feature-random-sample-size* 
                                       (- n (length active))))
                           ;;
                           ;; when random sampling is requested, and the total
                           ;; number of results (minus active ones) exceeds the
                           ;; requested sample size, make a random selection.
                           ;;
                           (random-sample 
                            low (if (zerop low) (- n 1) n)
                            (min 
                             (- n (length active)) 
                             *feature-random-sample-size*)
                            active))
            with context = (make-context :id iid)
            for result in results
            for rid = (get-field :result-id result)
            for event = (result-to-event-from-cache iid rid model fc)
            when event do (record-event
                           event context :normalizep normalizep :model model)
            finally
              (let ((n (context-size context)))
                (when (> n 0)
                  (let* ((file (merge-pathnames
                                cc
                                (make-pathname
                                 :name (format nil "~a" iid)))))
                    
                    (with-open-file (stream file
                                     :direction :output 
                                     :if-exists :supersede 
                                     :if-not-exists :create)
                      (print-context context :stream stream :format format))

                    (when sample
                      (let ((file
                             (merge-pathnames
                              cc
                              (make-pathname
                               :name (format
                                      nil
                                      "~a.~a"
                                      iid *feature-random-sample-size*)))))
                        (with-open-file (stream file
                                         :direction :output 
                                         :if-exists :supersede 
                                         :if-not-exists :create)
                          (print-context
                           context :stream stream
                           :format format :sample sample))))
                        
                    (format 
                     stream
                     "~&[~a] cache-contexts(): ~
                      item # ~d: ~a~@[ [~a]~] event~p;~%" 
                     (current-time :long :short) 
                     iid n (when sample (length sample)) n)))))
      finally (close-fc fc)))

(defun result-to-event (result model &key rop (fcp t))

  (let* ((derivationp (unless *feature-flags*
                        (or (>= *feature-grandparenting* 0)
                            (> *feature-ngram-size* 0)
                            *feature-dependencies*)))
         (derivation (and derivationp (get-field :derivation result)))
         (edge (or (get-field :edge result)
                   (when derivation
                     ;;
                     ;; when MRS-derived features are requested, we need to
                     ;; actually re-build complete feature structures, i.e.
                     ;; ask reconstruct() to perform all the unifications.
                     ;;
                     (reconstruct derivation *feature-dependencies*))))
         (mrs (and *feature-dependencies* edge (mrs::extract-mrs edge)))
         (event (make-event)))

    ;;
    ;; _fix_me_
    ;; why abort here?  we could still score the LM feature, say for fragmented
    ;; realizations in the LOGON pipeline.                     (13-may-07; oe)
    ;;
    #+:null
    (when (and derivationp (null edge)) (return-from result-to-event))

    ;;
    ;; for experimentation with the Google 1T n-gram data, we need to define a
    ;; `generic' interface for user-defined features.
    ;;
    #+:onet
    (loop
        for feature in (get-field :features result)
        do (record-feature feature event model :rop rop))
    
    ;;
    ;; in the LOGON universe, at least, .edge. can correspond to a fragmented
    ;; generator output, where there is no internal structure available besides
    ;; the concatenated surface string of the component fragments.
    ;;
    (when (and edge (lkb::edge-children edge))
      ;;
      ;; first, extract the configurational features.
      ;;
      (loop
          for feature in (edge-to-configurations edge)
          do (record-feature feature event model :rop rop))
      ;;
      ;; then the n-gram features over leaves of this edge.
      ;;
      (loop
          for feature in (edge-to-ngrams edge)
          do (record-feature feature event model :rop rop))
      
      ;;
      ;; semantic features extracted off the MRS of this edge.
      ;;
      (when mrs
        (loop
            for feature in (mrs-to-dependencies mrs)
            do (record-feature feature event model :rop rop))))

    ;;
    ;; often in a different universe, use whatever :flags properties off each
    ;; result.
    ;;
    (loop
        for feature in (result-to-flags result)
        do (record-feature feature event model :rop rop))

    ;;
    ;; finally, the feature(s) corresponding to LM score(s); for now, we record
    ;; the raw LM score in the feature cache.  however, when creating an actual
    ;; model, we want the scaling factor being part of the feature definition,
    ;; as its second parameter, so as to make sure that a serialized model can
    ;; be applied exactly the way it was trained.
    ;;
    (let ((lm (get-field :lm result)))
      (when (and (numberp lm)
                 (numberp *feature-lm-p*) (not (= *feature-lm-p* 0)))
        (record-feature
         (make-feature :tid 42 
                       :symbol (list 42)
                       :parameters (if fcp (list 0) (list 0 *feature-lm-p*))
                       :count lm)
         event model :rop rop)))
    event))

(defun edge-to-configurations (edge &key (parents '(lkb::^)))

  ;;
  ;; in order to support head lexicalization, perform feature extraction in a
  ;; pre-order tree transform: call ourselves recursively on all children (and
  ;; adjusting the .parents. list appropriately, i.e. inserting ourself as the
  ;; last parent on each recursive call); each recursive call will eventually
  ;; invoke edge-to-configurations() and accumulate the new features.
  ;;
  (unless (< *feature-grandparenting* 0)
    (nconc
     (loop
         with parents 
         = (when (> *feature-grandparenting* 0)
             (append
              (last parents (- *feature-grandparenting* 1))
              (list edge)))
         for edge in (lkb::edge-children edge)
         nconc (edge-to-configurations edge :parents parents))
     ;;
     ;; finally, operate on the local .edge.
     ;;
     (edge-to-configurations1 edge parents))))

(defun edge-to-configurations1 (edge parents)

  (let* ((root (edge-root edge))
         (parents (loop
                      for parent in parents
                      collect (if (lkb::edge-p parent)
                                (edge-root parent)
                                parent)))
         (daughters (lkb::edge-children edge)))
    ;;
    ;; _fix_me_
    ;; generator edges, sadly, do not show their morphological history in the
    ;; `children' slot, hence (much like compute-derivation-tree()), we would
    ;; have to interpret the idiosyncratic `found-lex-rule-list' here too :-{.
    ;;                                                         (13-may-07; oe)
    (cond
     ;;
     ;; at the terminal yield of a derivation, things are relatively simple:
     ;; create one feature for the local configuration at the yield, plus as
     ;; many as are possible to derive from grandparenting, i.e. prefixing the
     ;; tuple corresponding to the local feature with all suffixes of .parents.
     ;;
     ((null daughters)
      (let* ((symbol (cons root (lkb::edge-leaves edge)))
             (features 
              (list (make-feature :tid 1 :parameters '(0) :symbol symbol))))
        (loop
            for i from 1 to (min (length parents) *feature-grandparenting*)
            for iparents = (last parents i)
            for feature = (make-feature
                           :tid 1 :parameters (list i)
                           :symbol (append iparents symbol))
            do (push feature features))
        (when *feature-lexicalization-p* (setf (lkb::edge-head edge) root))
        features))
     (t
      (when *feature-lexicalization-p*
        ;;
        ;; decorate local edge with head lexicalization information: find the
        ;; head (or key) daughter in the local rule and project its head up
        ;; to the current edge.  this is the reason edge-to-configurations() 
        ;; does a pre-order transform, i.e. makes sure it has been called 
        ;; recursively on each edge _prior_ to actual feature extraction.
        ;;
        (let* ((rule (lkb::edge-rule edge))
               (key (lkb::rule-head rule)))
          (setf (lkb::edge-head edge) (lkb::edge-head (nth key daughters)))))
      (let* ((roots (loop
                        for edge in daughters
                        collect (edge-root edge)))
             (weights (loop
                          for edge in daughters
                          for from = (lkb::edge-from edge)
                          for to = (lkb::edge-to edge)
                          when (and (numberp from) (numberp to))
                          collect (- to from)
                          else return nil))
             (skew (when (rest weights)
                     (loop
                         with n = (length weights)
                         with average = (/ (sum weights) n)
                         ;;
                         ;; _fix_me_
                         ;; i suspect that should be (rest weights) below, to
                         ;; compute the degree of (im)balance among daughters.
                         ;;                                     (26-may-10; oe)
                         for foo in weights
                         sum (expt (- foo average) 2) into bar
                         finally
                           (return
                             (let ((bar (sqrt bar)))
                               (cond
                                ((= bar 0) 0) ((< bar 2) 1) ((>= 2) 2)))))))
             (weights (loop
                          for weight in weights
                          collect (cond
                                   ((= weight 1) 1)
                                   ((and (> weight 1) (<= weight 4)) 2)
                                   ((and (> weight 4) (<= weight 8)) 3)
                                   ((> weight 8) 4))))
             (head (lkb::edge-head edge))
             (symbol (cons root roots))
             (lsymbol (cons head symbol))
             (features
              (loop
                  with weightp = (and (numberp *feature-constituent-weight*)
                                      (> *feature-constituent-weight* 0)
                                      *feature-constituent-weight*)
                  for i from 0
                  to (min (length parents) *feature-grandparenting*)
                  for iparents = (last parents i)
                  collect (make-feature
                           :tid 1 :parameters (list i)
                           :symbol (append iparents symbol))
                  when *feature-lexicalization-p*
                  collect (make-feature
                           :tid 3 :parameters (list i)
                           :symbol (append iparents lsymbol))
                  when (and skew weightp)
                  collect (make-feature
                           :tid 5 :parameters (list 1 i)
                           :symbol (append iparents symbol (list skew)))
                  when (and weights weightp (> weightp 1))
                  collect (make-feature
                           :tid 5 :parameters (list 2 i)
                           :symbol (append iparents symbol weights)))))
 
        ;;
        ;; include (back-off, in a sense) features for partially instantiated
        ;; constituents (corresponding to active edges in the parser): for
        ;; each daughter, perform head lexicalization if necessary, and add
        ;; the resulting features to .codes.
        ;;
        (when (and *feature-active-edges-p* (rest daughters))
          (loop
              for edge in daughters
              for label = (edge-root edge)
              for symbol = (list root label)
              for lsymbol = (cons head symbol)
              do
                (loop
                    for i from 0
                    to (min (length parents) *feature-grandparenting*)
                    for iparents = (last parents i)
                    do
                      (push
                       (make-feature
                        :tid 2 :parameters (list i)
                        :symbol (append iparents symbol))
                       features)
                    when *feature-lexicalization-p* do
                      (push
                       (make-feature
                        :tid 4 :parameters (list i)
                        :symbol (append iparents lsymbol))
                       features))))
        features)))))

(defun edge-to-ngrams (edge)
  
  (loop
      with features = nil
      with forms = (lkb::edge-leaves edge)
      with ids = (lkb::edge-lex-ids edge)
      with type = (if (eq *feature-ngram-tag* :type) 1 0)
      with tags = (if (zerop type)
                    ids
                    (loop
                        for id in ids
                        collect (type-of-lexical-entry id)))
      initially
        (when (or (zerop *feature-ngram-size*)
                  (not (= (length forms) (length tags))))
          (return))
      for forms on (append (cons 'lkb::^ forms) '(lkb::$))
      for tags on (append (cons 'lkb::^ tags) '(lkb::$))
      while (first forms) do
        (loop
            for i from (if *feature-ngram-back-off-p* 1 *feature-ngram-size*)
            to *feature-ngram-size*
            for form = (nth (- i 1) forms)
            for itags = (ith-n tags 1 i)
            when (and form (not (and (= i 1)
                                     (smember form '(lkb::^ lkb::$))))) do
              (push
               (make-feature
                :tid 10 :parameters (list i type)
                :symbol (cons form itags))
               features)
              (push
               (make-feature
                :tid 11 :parameters (list i type) :symbol itags)
               features))
      finally (return features)))

(defun mrs-to-dependencies (mrs)
  (let* ((eds (mrs:eds-convert-psoa mrs))
	 (relations (and eds (mrs::eds-relations eds))))
    (loop for rel in relations
	for pred = (intern (mrs::ed-predicate rel) 'lkb)
	for args = (mrs::ed-arguments rel)
	for arg-list 
        = (loop for arg in args
              collect (list (intern (car arg) 'lkb)
                            (intern (if (stringp (cdr arg))
                                      (cdr arg)
                                      (mrs::ed-predicate (cdr arg))) 'lkb)))
	collect 
          (make-feature 
           :tid 20 :parameters (list 0)
           :symbol `(0 ,pred
                       ,@(loop 
                             for arg in arg-list
                             append (list (first arg) (second arg)))))
	append 
          (loop 
              for arg in arg-list
              collect 
                (make-feature
                 :tid 21 :parameters (list 0)
                 :symbol `(0 ,pred ,(first arg) ,(second arg))))
	collect 
          (make-feature
           :tid 22 :parameters (list 0)
           :symbol `(0 ,pred ,@(loop 
                                   for arg in arg-list
                                   collect (second arg))))
	append 
          (loop 
              for arg in arg-list
              collect (make-feature 
                       :tid 23 :parameters (list 0)
                       :symbol `(0 ,pred ,(second arg)))))))

(defun result-to-flags (result)
  (loop
      with flags = (let ((flags (get-field :flags result)))
                     (if (stringp flags)
                       (setf (get-field :flags result)
                         (ignore-errors (read-from-string flags)))
                       flags))
      for (i key) in *feature-flags*
      for value = (get-field key flags)
      for count = (and (numberp value) (coerce value 'single-float))
      for features
      = (cond
         (count
          (list
           (make-feature
            :tid 43 :symbol (list key) :parameters (list i) :count count)))
         ((eq key :mtrs)
          (loop
              with map = (make-hash-table)
              for key in value
              when (gethash key map) do (incf (gethash key map))
              else do (setf (gethash key map) 1)
              finally
                (return
                  (loop
                      for key being each hash-key
                      using (hash-value count) in map
                      collect
                        (make-feature
                         :tid 43 :symbol (list key)
                         :parameters (list i) :count count))))))
      nconc features))

(defun result-to-event-from-cache (iid rid model fc)

  (let* ((type (first *feature-preference-weightings*))
         (features (retrieve-features
                    fc iid rid %feature-frequency-tid% (list (first type))))
         (frequency (and features (feature-count (first features))))
         (event (and frequency (make-event :id rid :frequency frequency))))
    (unless event (return-from result-to-event-from-cache))
    
    ;;
    ;; at this point, we need to do something similiar to what happens during
    ;; construction of the feature cache, viz. interpret the various global
    ;; variables that determine which range of features to use.  alas.
    ;;
    (loop
        for i from 0 to *feature-grandparenting*
        for features = (retrieve-features fc iid rid 1 (list i))
        do (record-features features event model))
    (when *feature-active-edges-p*
      (loop
          for i from 0 to *feature-grandparenting*
          for features = (retrieve-features fc iid rid 2 (list i))
          do (record-features features event model)))
    (when (and (numberp *feature-constituent-weight*)
               (> *feature-constituent-weight* 0))
      (loop
          for i from 0 to *feature-grandparenting*
          do
            (loop
                for j from 1 to *feature-constituent-weight*
                for features = (retrieve-features fc iid rid 5 (list j i))
                do (record-features features event model))))
    (when *feature-lexicalization-p*
      (loop
          for i from 0 to *feature-grandparenting*
          for features = (retrieve-features fc iid rid 3 (list i))
          do (record-features features event model))
      (when *feature-active-edges-p*
        (loop
            for i from 0 to *feature-grandparenting*
            for features = (retrieve-features fc iid rid 4 (list i))
            do (record-features features event model))))
    (loop
        with type = (if (eq *feature-ngram-tag* :type) 1 0)
        for i from (if *feature-ngram-back-off-p* 1 *feature-ngram-size*)
        to *feature-ngram-size*
        for lfeatures = (retrieve-features fc iid rid 10 (list i type))
        for features = (retrieve-features fc iid rid 11 (list i type))
        do
          (record-features lfeatures event model)
          (record-features features event model))
    (when *feature-dependencies*
      (loop for tid from 20 to 23
	  for features = (retrieve-features fc iid rid tid (list 0))
	  do (record-features features event model)))
    (when (numberp *feature-lm-p*)
      ;;
      ;; the feature cache does /not/ record the scaling factor as a parameter
      ;; to the LM feature template (currently, template #42).
      ;;
      (let ((features (retrieve-features fc iid rid 42 (list 0))))
        #+:null
        (when *feature-lm-normalize*
          ;;
          ;; _fix_me_
          ;; incomplete code for normalization of LM values into a fixed range.
          ;;
          (loop 
              for feature in features
              for minmax = (aref (model-minmax model) (feature-code feature))))
        
        ;;
        ;; at this point, apply the LM scaling factor and record the actual
        ;; value used as part of the LM feature(s), so as to ensure that the
        ;; final model preserves this bit of information.
        ;;
        (loop
            for feature in features
            for count = (feature-count feature)
            do 
              (setf (feature-count feature) (divide count *feature-lm-p*))
              (nconc (feature-parameters feature) (list *feature-lm-p*)))
        (record-features features event model)))
    (when *feature-flags*
      (loop
          for foo in *feature-flags*
          for features = (retrieve-features fc iid rid 43 (list (first foo)))
          do (record-features features event model)))
    #+onet
    (loop
        for (tid . parameters) in *feature-custom*
        for features = (retrieve-features fc iid rid tid parameters)
        do (record-features features event model))
    event))

(defun edge-root (edge &optional
                       (preterminals *feature-use-preterminal-types-p*))
  (typecase (lkb::edge-rule edge)
    (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
    (string (let ((instance (first (lkb::edge-lex-ids edge))))
              (if preterminals
                (type-of-lexical-entry instance)
                instance)))
    (t (error "edge-root(): unknown rule in edge ~a~%" edge))))

(defun pos-of-lexical-entry (identifier)
  (let* ((type (type-of-lexical-entry identifier))
         (name (symbol-name type))
         (separator (and type (position #\_ name :test #'char=))))
    (when separator (intern (subseq name 0 separator) :keyword))))

(defun lm-item-enhancer (item)
  #+:logon
  (when (and (numberp *feature-lm-p*) (not (= *feature-lm-p* 0)))
    (loop
        with foo
        with results = (get-field :results item)
        with strings = (loop
                           for result in results
                           for string = (get-field :surface result)
                           when string 
                           collect string 
                           and do (push result foo))
        with scores
        = #+:lm (mt::lm-score-strings strings :measure :logprob) #-:lm 0
        for result in (nreverse foo)
        for score = (rest (pop scores))
        do (nconc result (acons :lm score nil))))
  item)

(defparameter %flags-ignore-fragments-p% t)

(defun flags-item-enhancer (item &key (key :neva))
  (nconc
   item
   (loop
       with *package* = (find-package :lkb)
       with length = (get-field :i-length item)
       with ranks
       for result in (get-field :results item)
       for olength
       = (let ((surface (get-field :surface result)))
           (and surface (+ 1 (count #\space surface))))
       for flags = (let ((flags (get-field :flags result)))
                     (if (stringp flags)
                       (setf (get-field :flags result)
                         (ignore-errors (read-from-string flags)))
                       flags))
       for fragmentp = (let ((fragments (get-field :fragments flags)))
                         (and (numberp fragments) (> fragments 0)))
       for value = (get-field key flags)
       unless (numberp value) do
         (format
          t
          "flags-item-enhancer(): no ~a score on item # ~a (result # ~a).~%"
          key (get-field :i-id item) (get-field :result-id result))
       and return nil
       else do
         ;;
         ;; _fix_me_
         ;; it appears that the :ratio computation in translate-item() was not
         ;; correct, hence the final HandOn profiles (and likely also the ones
         ;; we used for the 2007 TMI paper) always have zero :ratio values.
         ;; not hard to (re-)compute at this point, so possibly we should even
         ;; ditch the corresponding (and flawed) code in translate-item().
         ;;                                                    (30-jun-08; oe)
         (when olength
           (let ((ratio (divide olength length)))
             (if (get-field :ratio flags)
               (setf (get-field :ratio flags) ratio)
               (nconc flags (acons :ratio ratio nil)))))
         (push (acons key value result) ranks)
       finally
         (return
          (if (and fragmentp %flags-ignore-fragments-p%)
            (acons
             :ranks
             (loop
                 for rank in ranks
                 collect (acons :rank 1 rank))
             nil)
            (let ((ranks (sort ranks #'> :key #'(lambda (result)
                                                  (get-field key result)))))
              (acons
               :ranks
               (loop
                   with top = (get-field key (first ranks))
                   for rank in ranks
                   while (= (get-field key rank) top)
                   collect (acons :rank 1 rank))
               nil)))))))

(defun weigh-result (item result type)
  (let* ((active (loop
                     for rank in (get-field :ranks item)
                     for n = (get-field :rank rank)
                     for id = (get-field :result-id rank)
                     when (= n 1) collect id))
         (rid (get-field :result-id result))
         (frequency (if (member rid active :test #'=) 1 0)))
    (case (first type)
      (:binary frequency)
      ((:bleu :wa :waft)
       (let ((gold (get-field :i-input item))
             (surface (get-field :surface result)))
         (if surface
           (first
            (score-strings (list surface) (list gold) :type (first type)))
           0)))
      (:conll
       (let* ((id (get-field :result-id result))
              (score (item-to-conll item :result-id id :stream nil))
              (total (get-field :total score))
              (head (get-field :head score))
              (power (or (second type) 1))
              (factor (or (third type) 1)))
         (if (and total head)
           (* (expt (divide head total) power) factor)
           0)))
      (:flags
       (let* ((flags (get-field :flags result))
              (flags (if (stringp flags)
                       (setf (get-field :flags result)
                         (ignore-errors (read-from-string flags)))
                       flags)))
         ;;
         ;; _fix_me_
         ;; maybe this should really be NEVA, nowadays?          (5-apr-08; oe)
         ;;
         (or (get-field :bleu flags) 0)))
      (t 0))))

(defun random-sample (low high size &optional sample)
  ;;
  ;; returns a set of .size. unique random integers from [.low. -- .high.], not
  ;; including elements of the initial .sample., when supplied.
  ;;
  (loop
      with n = (+ (- high low) 1)
      for i = (+ low (random n))
      unless (smember i sample) do
        (push i sample)
        (decf size)
      while (> size 0)
      finally (return sample)))

(defun profile-find-feature-cache (profile &optional mode &key createp)
  (let ((file (make-pathname
               :directory (find-tsdb-directory profile) :name "fc")))
    (if mode (open-fc file mode :createp createp) file)))

(defun profile-find-model (profile &key testp)
  (let ((file (make-pathname
               :directory (find-tsdb-directory profile)
               :name "fc" :type "mlm")))
    (if testp (probe-file file) file)))

(defun profile-find-context-cache (profile
                                   &optional (identity -1)
                                   &key createp)
  (let ((path
         (dir-append
          (find-tsdb-directory profile)
          (list :relative (format nil "cc.~a" identity)))))
    (when createp
      #+:fad
      (ignore-errors
       (fad:delete-directory-and-files path :if-does-not-exist :ignore)
       (mkdir path)))
    path))

;;;
;;; from here on, functions to score various types of structures (full results
;;; or intermediate constituents) according to a model.
;;;

(defmacro score-feature (code model)
  ;;
  ;; _fix_me_
  ;; unless exported and read back in once, it is quite possible for models to
  ;; have meaningful feature codes beyond the `count' value (which appears to 
  ;; be the number of actual weights read after training); possibly we could
  ;; simply use the model `size' as the boundary instead, but then we would 
  ;; have had to make sure that unpopulated indices are initialized to zeros.
  ;;                                                     (7-apr-06; oe & erik)
  `(or (when (< ,code (model-count ,model))
         (aref (model-weights ,model) ,code))
       0.0))

(defun score-event (event model)
  (loop
      for feature in (event-features event)
      for count = (let ((count (or (feature-count feature) 0)))
                    ;;
                    ;; because the feature cache does /not/ include the scaling
                    ;; of LM scores, we need to apply the scaling factor every
                    ;; time we compute the contribution of the LM feature.
                    ;;
                    (if (= (feature-tid feature) 42)
                      (divide count *feature-lm-p*)
                      count))
      for score = (score-feature (feature-code feature) model)
      sum (* count score)))

(defun mem-score-result (result &optional (model %model%) &key normalizep)
  (unless (model-p model)
    (setf model (rest (assoc model *models*))))
  (if model
    (let ((event (result-to-event result model :rop t :fcp nil)))
      (when normalizep
        (normalize-features-n event :type normalizep :model model))
      (score-event event model))
    0.0))

(defun mem-score-configuration (edge daughters &optional (model %model%))
  (unless (model-p model)
    (setf model (rest (assoc model *models*))))
  (if (model-p model)
    (let* ((event (make-event))
           (roots (if daughters
                    (loop
                        for edge in daughters
                        collect (edge-root edge))
                    (lkb::edge-leaves edge)))
           (feature (make-feature
                     :tid 1 :parameters (list 0)
                     :symbol (cons (edge-root edge) roots))))
      (record-feature feature event model :rop t)
      (score-event event model))
    0.0))
