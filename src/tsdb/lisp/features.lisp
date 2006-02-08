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
;;; some notes on the encoding of features: to make sure we operate in separate
;;; namespaces for the various feature templates, each feature is prefixed with
;;; an integer identifying the feature type (aka template), viz.
;;;
;;;   1: local derivational configuration; a subtree of depth one taken from 
;;;      the derivation; e.g. [1 0 hspec det_poss_my_le noptcomp]; the second
;;;      integer indicates the amount of grandparenting used, where parent 
;;;      labels will precede the root of the local configuration.
;;;   2: `active' local derivational configuration; similar to type 1, but for
;;;      sub-sets of the daughters (corresponding to active edges), determined
;;;      by rule instantiation order (the .rhs. slot in LKB rule structures),
;;;      e.g. [2 0 hspec noptcomp].
;;;   3: lexicalized local derivational configuration; much like type 1, but
;;;      the first symbol is the lexical head of the root of the configuration;
;;;      if we decided to lexicalize _all_ nodes in the feature, that would be
;;;      either a different template or additional parameter then.
;;;   4: lexicalized `active' local derivational configuration; one parameter,
;;;      viz. degree of grand-parenting.
;;;  10: n-gram features; e.g. [10 3 1 "saw" ^ n_proper_le v_np_trans_le]; the
;;;      first parameter is the n-gram size, the second an integer coding as to
;;;      what is used in the n-grams: 0 -- lexical identifier, 1 -- le type.
;;;  11: preterminal-only n-gram features; like type 10, but without a surface
;;;      form; e.g. [11 3 1 ^ n_proper_le v_np_trans_le].
;;;  42: language model score; the second integer is the number of bins used 
;;;      (if any), and the third the divisor used in scaling (*maxent-lm-p*), 
;;;      e.g. [42 0 100]
;;;
;;; also, we are using some pseudo-features to record additional information in
;;; the feature cache, viz.
;;;
;;;  -1: event frequencies; takes one parameter encoding the frequency
;;;      assignment function: 0 -- binary, i.e. 0 or 1 according to what comes
;;;      out of the annotations in the treebank; 1 and upwards -- various
;;;      weighted frequency functions, according to the global value of
;;;      *maxent-preference-weigthings* .
;;;

(in-package :tsdb)

(defparameter *feature-grandparenting* 2)

(defparameter *feature-use-preterminal-types-p* t)

(defparameter *feature-lexicalization-p* t)

(defparameter *feature-active-edges-p* t)

(defparameter *feature-ngram-size* 2)

(defparameter *feature-ngram-tag* :type)

(defparameter *feature-ngram-back-off-p* t)

(defparameter *feature-lm-p* 10)

(defparameter *feature-preference-weigthings* '((0 :binary)))

(defparameter *feature-frequency-threshold*
  (make-counts :absolute 0 :contexts 0 :events 0 :relevant 0))

(defparameter *feature-random-sample-size* nil)

(defparameter *feature-item-enhancers* (list 'lm-item-enhancer))

;;;
;;; _fix_me_
;;; once we have all the globals together, synchronize these and functions like
;;; mem-environment(), print-mem(), et al.               (11-jul-05; erik & oe)
;;;
(defparameter *feature-options*
  '(*feature-grandparenting*
    *feature-use-preterminal-types-p*
    *feature-lexicalization-p*
    *feature-active-edges-p*
    *feature-ngram-size*
    *feature-ngram-tag*
    *feature-ngram-back-off-p*
    *feature-lm-p*
    *feature-frequency-threshold*
    *feature-random-sample-size*))

(defparameter *feature-float-valued-tids* '(42 -1))

(defconstant %feature-frequency-tid% -1)

(defstruct (model)
  (table (make-symbol-table :test #'equal))
  (map (cons 0 (make-hash-table :test #'eql)))
  contexts (ncontexts 0)
  (counts (make-array 512))
  (weights (make-array 512 :initial-element 0.0))
  (count 0)
  (size 512)
  parameters
  stream)

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
    ((:mem :svm :rpm)
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
           for code = (or (feature-mapped feature) (feature-code feature))
           do
             (write-char #\Space stream)
             (fwrite code stream)
             (write-char #\Space stream)
             (fwrite (feature-count feature) stream)))
      (:svm
       (format stream "~a qid:~a" (- (event-frequency event)) iid)
       (loop
           for feature in (event-features event)
           for code = (or (feature-mapped feature) (feature-code feature))
           do
             (write-char #\Space stream)
             (fwrite code stream)
             (write-char #\Colon stream)
             (fwrite (feature-count feature) stream)
           finally 
             (format stream " # ~a" (event-id event)))))
    (terpri stream))) ;todo, simplify this function.

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

(defun record-feature (feature event &optional model)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))

  (let ((code (feature-code feature)))
    (unless (or (null model) (null code) (null *feature-frequency-threshold*)
                (counts>= (aref (model-counts model) code)
                          *feature-frequency-threshold*))
      (return-from record-feature)))
  
  (let* ((map (model-map model))
         (mapped (gethash (feature-code feature) (rest map))))
    
    (when (and model (hash-table-p (rest (model-map model)))
               (feature-code feature)) 
      (if (null mapped)
        (setf (feature-mapped feature)
          (setf (gethash (feature-code feature) (rest map))
            (incf (first map)))) 
        (setf (feature-mapped feature) mapped))
      (setf mapped (feature-mapped feature)))
    ;;
    ;; encode this feature, unless it has been assigned a unique identifier 
    ;; (with  respect to .model. already).
    ;;
    (unless (or (null model) (feature-code feature))
      (setf (feature-code feature)
        (symbol-to-code
         (list* (feature-tid feature) (feature-parameters feature)
                (feature-symbol feature))
         (model-table model)))
      (setf (feature-symbol feature) nil))
    ;;
    ;; globally, i.e. in the model itself, keep track of feature frequencies. 
    ;; we expect to add to this and also keep track of how often a feature
    ;; appeared  `contrastively', that is either only occur in the preferred or
    ;; only the dis-preferred events (ultimately, pseudo-maximal or -minimal 
    ;; ones).
    ;;
    (let* ((identifier (if mapped #'feature-mapped #'feature-code))
           (code (funcall identifier feature)))
      ;;
      ;; _fix_me_
      ;; probably, we actually don't want to do this when mapping features, 
      ;; i.e. during context cache creation
      ;;
      (when model
        (when (>= code (model-size model))
          (let ((n (setf (model-size model)
                     (max (+ code 1) (* (model-size model) 2)))))
            (setf (model-counts model)
              (adjust-array (model-counts model) n))
            (setf (model-weights model)
              (adjust-array (model-weights model) n)))))
      ;;
      ;; the .features. storage in events, naturally, is organized as a simple,
      ;; ordered list, sorted by feature codes.  thus, the code below searches 
      ;; linearly through the list and either increments the counter for 
      ;; features that are present already, or inserts appropriately.
      ;;
      (cond
       ((or (zerop (event-size event)) 
            (< code (funcall identifier 
                             (first (event-features event)))))
        (push feature (event-features event))
        (incf (event-size event)))
       (t
        (loop
            for features on (event-features event)
            for this = (first features)
            for next = (first (rest features))
            when (= (funcall identifier this) code) do
              (incf (feature-count this) (feature-count feature))
              (return)
            else when (or (null next) (< code (funcall identifier next))) do
              (setf (rest features) (cons feature (rest features)))
              (incf (event-size event))
              (return)))))))

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

(defun record-event (event context)
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
      for enhancer in *feature-item-enhancers*
      do
        (loop
            for item in items
            do (call-raw-hook enhancer item)))
  
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
                       (setf cache
                         (profile-find-feature-cache
                          source :write :createp createp)))))
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 0)) do
	(loop
	    with i = 0
	    with active = (loop
                              for rank in (get-field :ranks item)
                              for i = (get-field :rank rank)
                              for id = (get-field :result-id rank)
                              when (= i 1) collect id)
            with *reconstruct-cache* = (make-hash-table :test #'eql)
	    for result in (get-field :results item)
	    for rid = (get-field :result-id result)
            for goldp = (if (member rid active :test #'=) :gold :lead)
	    for event = (result-to-event result model :goldp goldp)
            unless event do
              (format
               stream
               "~&[~a] cache-features(): ~
                ignoring item # ~d (no edge for ~d)~%"
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
                  for (i . type) in *feature-preference-weigthings*
                  for count = (float (weigh-result item result type))
                  for feature = (make-feature
                                 :tid %feature-frequency-tid%
                                 :parameters (list i)
                                 :symbol (list %feature-frequency-tid% i)
                                 :count count)
                  do (record-feature feature event model))
              (loop
                  for feature in (event-features event)
                  do (store-feature db iid rid feature))
              (incf i)
	    finally
              (loop
                  for code being each hash-key
                  using (hash-value count) in counts
                  for match = (or (aref (model-counts model) code)
                                  (setf (aref (model-counts model) code)
                                    (make-counts)))
                  do
                    (incf (counts-absolute match) (first count))
                    (incf (counts-contexts match))
                    (incf (counts-events match) (second count))
                    (when (rest (rest (rest count)))
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
                       &key (format :rpm) (stream *tsdb-io*))

  ;;
  ;; _fix_me_
  ;; most of the record-xyz() functions used to assume we had a model; maybe
  ;; rethink that part, or always carry around a model here too?
  ;;                                             (5-jul-05; erik & oe)
  (loop
      with source = (get-field :source (first items))
      with creation = (list source)
      with fc = (profile-find-feature-cache source :write)
      with cc = (profile-find-context-cache source identity :createp t)
      for item in items
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      when (and (integerp readings) (> readings 1))
      do
        (let ((foo (get-field :source item)))
          (unless (string= source foo)
            (setf source foo)
            (setf fc (profile-find-feature-cache source :write))
            (let ((createp (not (member source creation :test #'string=))))
              (setf cc
                (profile-find-context-cache source identity :createp createp))
              (when createp (push source creation)))))
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
            for sample = (when (and *feature-random-sample-size*
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
            when event do (record-event event context)
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
                      ~a~@[ [~a]~] event~p recorded for item # ~d;~%" 
                     (current-time :long :short) n
                     (when sample (length sample)) n iid)))))
      finally (close-fc fc)))

(defun result-to-event (result model &key goldp)
  (declare (ignore goldp))

  (let* ((derivation (get-field :derivation result))
         (edge (when derivation
                 (or (get-field :edge result)
                     (reconstruct derivation nil))))
         (event (make-event)))

    (when (null edge) (return-from result-to-event))

    ;;
    ;; first, extract the configurational features.
    ;;
    (loop
        for feature in (edge-to-configurations edge)
        do (record-feature feature event model))
    ;;
    ;; then the n-gram features over leaves of this edge.
    ;;
    (loop
        for feature in (edge-to-ngrams edge)
        do (record-feature feature event model))
    ;;
    ;; finally, the features corresponding to LM score(s)
    ;;
    ;; _fix_me_
    ;; we want to further generalize this, e.g. include multiple LM features,
    ;; maybe include the scaling factor as a parameter.
    ;;                                             (27-jun-05; erik & oe)
    (let ((lm (get-field :lm result)))
      (when (numberp lm)
        (record-feature
         (make-feature :tid 42 
                       :symbol (list 42) 
                       :parameters 
                       (list 0 ;; 0 = no binning
                             (if (numberp *feature-lm-p*)
                               *feature-lm-p*
                               0))
                       :count lm)
         event model)))
    event))

(defun edge-to-configurations (edge &key (parents '(lkb::^)))

  ;;
  ;; in order to support head lexicalization, perform feature extraction in a
  ;; pre-order tree transform: call ourselves recursively on all children (and
  ;; adjusting the .parents. list appropriately, i.e. inserting ourself as the
  ;; last parent on each recursive call); each recursive call will eventually
  ;; invoke edge-to-configurations() and accumulate the new features.
  ;;
  (nconc
   (loop
       with parents 
       = (when (> *feature-grandparenting* 0)
           (append (last parents (- *feature-grandparenting* 1)) (list edge)))
       for edge in (lkb::edge-children edge)
       nconc (edge-to-configurations edge :parents parents))
   ;;
   ;; finally, operate on the local .edge.
   ;;
   (edge-to-configurations1 edge parents)))

(defun edge-to-configurations1 (edge parents)

  (let* ((root (edge-root edge))
         (parents (loop
                      for parent in parents
                      collect (if (lkb::edge-p parent)
                                (edge-root parent)
                                parent)))
         (daughters (lkb::edge-children edge)))
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
      ;;
      ;; _fix_me_
      ;; lexicalized features, presumably, should use a separate identifier;
      ;; the complete lexicalization set-up needs testing.      (3-apr-05; oe)
      ;;
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
             (head (lkb::edge-head edge))
             (symbol (cons root roots))
             (lsymbol (cons head symbol))
             (features
              (loop
                  for i from 0
                  to (min (length parents) *feature-grandparenting*)
                  for iparents = (last parents i)
                  collect (make-feature
                           :tid 1 :parameters (list i)
                           :symbol (append iparents symbol))
                  when *feature-lexicalization-p*
                  collect (make-feature
                           :tid 3 :parameters (list i)
                           :symbol (append iparents lsymbol)))))
 
        ;;
        ;; include (back-off, in a sense) features for partially instantiated
        ;; constituents (corresponding to active edges in the parser): for
        ;; the rule instantiation order .rhs., for each prefix, extract the
        ;; (sub-)sets of corresponding daughters, perform head lexicalization
        ;; if necessary, and add the resulting features to .codes.
        ;;
        ;; _fix_me_
        ;; not quite sure what to do about grandparenting; would it make sense
        ;; on these `active edges' too?                          (3-apr-05; oe)
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

(defun result-to-event-from-cache (iid rid model fc)

  (let* ((type (first *feature-preference-weigthings*))
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
    (when *feature-lm-p*
      (let ((features (retrieve-features fc iid rid 42 
                                         (list 0 ;; 0 = no binning
                                               (if (numberp *feature-lm-p*) 
                                                   *feature-lm-p*
                                                 0)))))
        ;; 
        ;; _fix_me_
        ;; if we were to retain the LM scaling set-up, here would be a good
        ;; place to apply the scaling; for the time being, it gets applied in
        ;; mem-item-enhancer() already.                  (5-jul-05; erik & oe)
        ;;
        (record-features features event model)))
    event))

(defun edge-root (edge)
  (typecase (lkb::edge-rule edge)
    (lkb::rule (lkb::rule-id (lkb::edge-rule edge)))
    (string (let ((instance (first (lkb::edge-lex-ids edge))))
              (if *feature-use-preterminal-types-p*
                (type-of-lexical-entry instance)
                instance)))
    (t (error "edge-root(): unknown rule in edge ~a~%" edge))))

(defun lm-item-enhancer (item)
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
        with scores = (mt::lm-score-strings strings)
        for result in (nreverse foo)
        for score = (/ (rest (pop scores)) *feature-lm-p*)
        do (nconc result (acons :lm score nil))))
  item)

(defun weigh-result (item result type)
  (let* ((active (loop
                     for rank in (get-field :ranks item)
                     for i = (get-field :rank rank)
                     for id = (get-field :result-id rank)
                     when (= i 1) collect id))
         (rid (get-field :result-id result))
         (frequency (if (member rid active :test #'=) 1 0)))
    (case (first type)
      (:binary frequency)
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
               :directory (find-tsdb-directory profile)
               :name "fc" :type "abt")))
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
;;; _fix_me_
;;; the following two functions are legacy code, in order to run an old model
;;; for the LOGON 0.5 integration.  purge this soon :-}.       (21-sep-05; oe)
;;;
(defun edge-to-codes (edge parents model)
  (let* ((table (model-table model))
         (root (edge-root edge))
         (parents (loop
                      for parent in parents
                      collect (if (lkb::edge-p parent)
                                (edge-root parent)
                                parent)))
         (daughters (lkb::edge-children edge)))
    (cond
     ;;
     ;; see whether we have extracted features from this edge before; if so,
     ;; re-use cache of earlier results (safe-guarded by .model. identity).
     ;;
     ((and (eq (lkb::edge-foo edge) model) (consp (lkb::edge-bar edge)))
      (lkb::edge-bar edge))
     ;;
     ;; at the terminal yield of a derivation, things are relatively simple:
     ;; create one feature for the local configuration at the yield, plus as
     ;; many as are possible to derive from grandparenting, i.e. prefixing the
     ;; tuple corresponding to the local feature with all suffixes of .parents.
     ;;
     ((null daughters)
      (let* ((feature (cons root (lkb::edge-leaves edge)))
             (codes (list (symbol-to-code (list* 1 0 feature) table))))
        (loop
            for i from 1 to (min (length parents) *feature-grandparenting*)
            for iparents = (last parents i)
            for code = (symbol-to-code
                        (append (list 1 i) iparents feature)
                        table)
            do (push code codes))
        (when *feature-lexicalization-p* (setf (lkb::edge-head edge) root))
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) codes)))
     (t
      ;;
      ;; _fix_me_
      ;; lexicalized features, presumably, should use a separate identifier;
      ;; the complete lexicalization set-up needs testing.      (3-apr-05; oe)
      ;;
      (when *feature-lexicalization-p*
        ;;
        ;; decorate local edge with head lexicalization information: find the
        ;; head (or key) daughter in the local rule and project its head up
        ;; to the current edge.
        ;;
        (let* ((rule (lkb::edge-rule edge))
               (key (if (eq *feature-lexicalization-p* :head)
                      (lkb::rule-head rule)
                      (first (lkb::rule-rhs rule)))))
          (setf (lkb::edge-head edge) (lkb::edge-head (nth key daughters)))))
      (let* ((roots (loop
                        for edge in daughters
                        collect (edge-root edge)))
             (head (lkb::edge-head edge))
             (feature (cons root roots))
             (codes (nconc
                     (when *feature-lexicalization-p*
                       (list (symbol-to-code (list* 1 0 head feature) table)))
                     (list (symbol-to-code (list* 1 0 feature) table)))))
        
        (loop
            for i from 1 to (min (length parents) *feature-grandparenting*)
            for iparents = (last parents i)
            for code = (symbol-to-code
                        (append (list 1 i) iparents feature)
                        table)
            do (push code codes))

        ;;
        ;; include (back-off, in a sense) features for partially instantiated
        ;; constituents (corresponding to active edges in the parser): for
        ;; the rule instantiation order .rhs., for each prefix, extract the
        ;; (sub-)sets of corresponding daughters, perform head lexicalization
        ;; if necessary, and add the resulting features to .codes.
        ;;
        ;; _fix_me_
        ;; not quite sure what to do about grandparenting; would it make sense
        ;; on these `active edges' too?                          (3-apr-05; oe)
        ;;
        (when *feature-active-edges-p*
          (loop
              with rhs = (lkb::rule-rhs (lkb::edge-rule edge))
              for i from 1 to (- (length rhs) 1)
              for foo = (ith-n rhs 1 i)
              for roots = (loop
                              for j in foo
                              collect (edge-root (nth j daughters)))
              for feature = (cons root roots)
              when *feature-lexicalization-p* do
                (push
                 (symbol-to-code (list* 2 0 head feature) table)
                 codes)
              do
                (push (symbol-to-code (list* 2 0 feature) table) codes)))
        (setf (lkb::edge-foo edge) model)
        (setf (lkb::edge-bar edge) codes))))))

(defun mem-score-edge (edge 
                       &key (model %model%) (parents '(lkb::^)) recursivep lm)

  (if model
    (if (and (not recursivep) 
             (eq (lkb::edge-foo edge) model) (numberp (lkb::edge-score edge)))
      (lkb::edge-score edge)
      (let* ((codes (when (lkb::edge-rule edge)
                      (edge-to-codes edge parents model)))
             (score (if (numberp lm)
                      (let* ((table (model-table model))
                             (code
                              (symbol-to-code
                               (list 42 0 *feature-lm-p*) table)))
                        (* (score-feature code model) lm))
                      0)))
        (setf (lkb::edge-score edge)
          (+ (loop
                 for code in codes
                 sum (score-feature code model))
             score
             (loop
                 with parents 
                 = (when (> *feature-grandparenting* 0)
                     (append
                      (last parents (- *feature-grandparenting* 1))
                      (list edge)))
                 for edge in (lkb::edge-children edge)
                 sum (mem-score-edge
                      edge :model model :recursivep t :parents parents))))))
    0.0))

(defun mem-score-task (task model)
  ;;
  ;; _fix_me_
  ;; the following seem to not have an integer first() in their features.
  ;;                                                           (27-oct-04; oe)
  (cond
   ((lkb::chart-configuration-p task)
    (mem-score-edge (lkb::chart-configuration-edge task) :model model))
   ((lkb::edge-p task)
    (mem-score-edge task :model model))
   ((lkb::rule-p (first task))
    (let* ((rule (first task))
           (root (lkb::rule-id rule))
           (passive (lkb::chart-configuration-edge (rest task)))
           (daughter (edge-root passive))
           (feature (list root daughter))
           (code (symbol-to-code feature (model-table model))))
      #+:debug
      (format t "r&p: `~a'" feature)
      (+ (score-feature code model)
         (mem-score-edge passive :model model))))
   ((lkb::chart-configuration-p (first task))
    (let* ((active (lkb::chart-configuration-edge (first task)))
           (rule (lkb::edge-rule active))
           (root (lkb::rule-id rule))
           (daughters (loop
                          for daughter in (lkb::edge-children active)
                          collect (edge-root daughter)))
           (passive (lkb::chart-configuration-edge (rest task)))
           (daughter (edge-root passive))
           (feature (cons root (nconc daughters (list daughter))))
           (code (symbol-to-code feature (model-table model))))
      #+:debug
      (format t "a&p: `~a'" feature)
      (+ (score-feature code model)
         (loop
             for edge in (lkb::edge-children active)
             sum (mem-score-edge edge :model model))
         (mem-score-edge passive :model model))))
   (t -10)))
