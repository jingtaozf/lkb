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

;;;
;;; while preparing for the sep-99 release of the LKB we need to do some magic 
;;; to support the pre-release and current development versions in the same
;;; source file.  searching for find-symbol() and friends should be a good way
;;; to find the all-too-many kludges that can be eliminated once the release
;;; has been made public ... sigh.                         (22-aug-99  -  oe)
;;;

(in-package :lkb)

;;;
;;; _fix_me_
;;; even more: in the (current) MT set-up, the `mt' system tends to be loaded
;;; _after_ the [incr tsdb()] code.                          (1-nov-03; oe)
;;;
(eval-when #+:ansi-eval-when (:compile-toplevel :load-toplevel :execute)
	   #-:ansi-eval-when (compile load eval)
  (unless (find-package :mt) (make-package :mt)))

(defun tsdb::current-grammar ()
  (or (tsdb::clients-grammar)
      (cond 
       ((and (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
             (boundp (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)))
        (symbol-value (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)))
       ((and (member :lkb *features*) 
             (find-package :lkb)
             (find-symbol "*GRAMMAR-VERSION*" :lkb)
             (boundp (find-symbol "*GRAMMAR-VERSION*" :lkb)))
        (symbol-value (find-symbol "*GRAMMAR-VERSION*" :lkb)))
       (t "anonymous"))))


(defun get-test-run-information ()
  (let* ((*package* *lkb-package*)
         (exhaustivep (null *first-only-p*))
         (active-parsing-p (and (find-symbol "*ACTIVE-PARSING-P*")
                                (boundp (find-symbol "*ACTIVE-PARSING-P*"))
                                (symbol-value 
                                 (find-symbol "*ACTIVE-PARSING-P*"))))
         (hyper-activity-p (and (find-symbol "*HYPER-ACTIVITY-P*")
                                (boundp (find-symbol "*HYPER-ACTIVITY-P*"))
                                (symbol-value 
                                 (find-symbol "*HYPER-ACTIVITY-P*"))))
         (chart-packing-p *chart-packing-p*)
         (agendap (or (and (not active-parsing-p) (not exhaustivep))
                      (and active-parsing-p (not exhaustivep) 
                           (find :agenda *features*))))
         (npaths (length *check-paths-optimised*))
         (ntemplates (when (and (find-symbol "*TEMPLATES*")
                                (boundp (find-symbol "*TEMPLATES*")))
                       (length (symbol-value 
                                (find-symbol "*TEMPLATES*"))))))
    `((:avms . ,(- (hash-table-count *types*) (or ntemplates 0)))
      (:sorts . 0)
      (:templates . ,ntemplates)
      (:rules . ,(hash-table-count *rules*))
      (:lrules . ,(hash-table-count *lexical-rules*))
      (:lexicon . ,(size-of-lexicon))
      (:grammar . ,(tsdb::current-grammar))
      (:application . ,(format 
                        nil 
                        "LKB (~A mode~@[; version `~a'~]; ~
                         ~d qc paths; ~
                         ~@[packing ~]~
                         ~:[passive~*~;~:[~;hyper~]active~] ~
                         [~:[~d~;all~*~]~:[~;; agenda~]])" 
                        *lkb-system-version* 
                        (and (find-symbol "*CVS-VERSION*")
                             (boundp (find-symbol "*CVS-VERSION*"))
                             (symbol-value 
                              (find-symbol "*CVS-VERSION*")))
                        npaths
                        (and active-parsing-p chart-packing-p)
                        active-parsing-p hyper-activity-p
                        exhaustivep 
                        (if (numberp *first-only-p*) *first-only-p* 1)
                        agendap)))))


(defun tsdb::parse-word (word &key load trace)
  ;; .load. can be one of
  ;;
  ;;   (:warn :quiet :collect nil)
  ;;   (:fair :modest)
  ;;   (:full :all :verbose t)
  ;;
  ;; and allows more or less verbose output of actions performed by
  ;; PAGE; as there is considerably less action going on in LKB, i
  ;; suggest you ignore .load. and make sure not to produce any
  ;; printout; .trace. in PAGE is forwarded to the processor and
  ;; typically prevents printout of information on processing phases
  ;;
  (declare (ignore load))
  (ignore-errors
   (let* ((*package* *lkb-package*)
          (*chasen-debug-p* nil)
          (str (make-string-output-stream)) ; capture any warning messages
          (*standard-output* (if trace
                               (make-broadcast-stream *standard-output* str)
                               str))
          (input (split-into-words (preprocess-sentence-string word))))
     (declare (special *chasen-debug-p*))
     (parse input nil)
     (summarize-chart))))

(defun tsdb::initialize-run (&key interactive exhaustive nanalyses
                            protocol custom)
  (declare (ignore interactive protocol custom))
  ;; returns whatever it likes; the return value will be given to
  ;; finalize-test-run() to restore the interactive environment if
  ;; necessary
  (let ((*package* *lkb-package*)
        (first-only-p *first-only-p*))
    (clear-type-cache)
    (setf *first-only-p* (if exhaustive
                           nil
                           (if (integerp nanalyses)
                             (or (zerop nanalyses) nanalyses)
                             (if (integerp *first-only-p*) *first-only-p* 1))))
    (let ((context (pairlis '(:first-only-p)
                            (list first-only-p))))
      (acons :context context (get-test-run-information)))))

(defun tsdb::finalize-run (context &key custom)
  (declare (ignore custom))
  ;; called after completion of test run
  (let ((lexicon 0)
        (*package* *lkb-package*))
    (loop 
        for id in (collect-expanded-lex-ids *lexicon*)
        do 
          (pushnew id *lex-ids-used*)
          (incf lexicon)) 
    (clear-type-cache)
    (uncache-lexicon)
    (loop
        for (variable . value) in context do
          (case variable
            (:first-only-p 
             (setf *first-only-p* value))))
    (pairlis '(:lexicon) (list lexicon))))


;;; sets the processor into exhaustive mode if requested; parses
;;; .string. without producing any printout (unless .trace. is set);
;;; funcall()s .semantix-hook. and .trees-hook. to obtain MRS and tree
;;; representations (strings); all times in thousands of secs

(defun tsdb::parse-item (string 
                   &key id exhaustive nanalyses trace
                        edges derivations semantix-hook trees-hook
                        filter burst (nresults 0))
  (declare (ignore id derivations filter)
           (special tsdb::*fakes*))
  
  (when tsdb::*fakes*
    (return-from tsdb::parse-item
      (tsdb::search-fake string)))
  
  (let* ((*package* *lkb-package*)
         (*chasen-debug-p* nil)
         (*repp-debug-p* nil)
         (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                      *maximum-number-of-edges*
                                      edges))
         (*first-only-p* (if (or exhaustive *chart-packing-p*)
                           nil
                           (if (integerp nanalyses)
                             (or (zerop nanalyses) nanalyses)
                             (if (integerp *first-only-p*) *first-only-p* 1))))
         (*do-something-with-parse* nil))
    (declare (special *chasen-debug-p* *repp-debug-p*))
    (multiple-value-bind (return condition)
      (#-:debug ignore-errors #+:debug progn
       (let* ((sent
               (split-into-words (preprocess-sentence-string string)))
              (str (make-string-output-stream)) ; capture any warning messages
              (*standard-output* 
               (if trace
                 (make-broadcast-stream *standard-output* str)
                 str))
              tgc tcpu treal conses symbols others)
         ;;
         ;; this really ought to be done in the parser ...  (30-aug-99  -  oe)
         ;;
         (setf *sentence* string)
         (reset-statistics)
         (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks m-tasks)
             (tsdb::time-a-funcall
              #'(lambda () (parse-tsdb-sentence sent trace))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setq tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                       conses (* scons 8) symbols (* ssym 24) others sother)))
	  (declare (ignore m-tasks))
          (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                 (packingp *chart-packing-p*)
                 (output (get-output-stream-string str))
                 (unifications (statistics-unifications *statistics*))
                 (copies (statistics-copies *statistics*))
                 utcpu
                 utgc
                 uspace
                 (readings (if packingp
                             (tsdb::time-a-funcall
                              #'(lambda () 
                                  ;;
                                  ;; _fix_me_
                                  ;; this should really go into the parser, but
                                  ;; just now patch things up for francis, so
                                  ;; he can submit something to TMI.
                                  ;;                            (11-may-07; oe)
                                  (setf *parse-record*
                                    (selectively-unpack-edges
                                     *parse-record* nanalyses))
                                  (length *parse-record*))
                              #'(lambda (tgcu tgcs tu ts tr scons ssym sother
                                         &rest ignore)
                                  (declare (ignore tr ignore))
                                  (setf utcpu (- (+ tu ts) (+ tgcu tgcs)))
                                  (setf utgc (+ tgcu tgcs))
                                  (setf uspace
                                    (+ (* scons 8) (* ssym 24) sother))))
                             (length *parse-record*)))
                 (readings (if (or (equal output "") (> readings 0))
                              readings
                             -1))
                 (best-first-p (> (length *parse-times*) 2))
                 (end (pop *parse-times*))
                 (times (nreverse *parse-times*))
                 (start (pop times))
                 (total (round (* (- end start) 1000) 
                               internal-time-units-per-second))
                 (first (if best-first-p
                          (round (* (- (first times) start) 1000) 
                                 internal-time-units-per-second)
                          (if (> readings 0) total -1)))
                 #+:pooling
                 (pool (and (find-symbol "*DAG-POOL*")
                            (boundp (find-symbol "*DAG-POOL*"))
                            (symbol-value (find-symbol "*DAG-POOL*"))))
                 #+:pooling
                 (position (when pool
                            (funcall 
                             (symbol-function (find-symbol "POOL-POSITION"))
                             pool)))
                 #+:pooling
                 (garbage (when pool
                            (funcall 
                             (symbol-function (find-symbol "POOL-GARBAGE"))
                             pool)))
                 (comment 
                  #+:pooling
                  (format nil "(:pool . ~d) (:garbage . ~d)" position garbage)
                  #-:pooling
                  "")
                 (comment
                  (if packingp
                    (format 
                     nil 
                     "(:utcpu . ~d) (:utgc . ~d) (:uspace . ~d) 
                      (:subsumptions . ~d) (:equivalence . ~d) ~
                      (:proactive . ~d) (:retroactive . ~d)  ~
                      (:trees . ~d) (:frozen . ~d) (:failures . ~d) ~a"
                     utcpu utgc uspace 
                     (statistics-subsumptions *statistics*)
                     (statistics-equivalent *statistics*)
                     (statistics-proactive *statistics*) 
                     (statistics-retroactive *statistics*)
                     (length *parse-record*)
                     (statistics-frozen *statistics*) 
                     (statistics-failures *statistics*)
                     comment)
                    comment))
                 (summary (summarize-chart :derivationp (< nresults 0))))
            (multiple-value-bind (l-s-tasks redges words)
                (parse-tsdb-count-lrules-edges-morphs)
              (declare (ignore l-s-tasks words))
              `((:others . ,others) (:symbols . ,symbols) 
                (:conses . ,conses)
                (:treal . ,treal) (:tcpu . ,tcpu)
                (:tgc . ,tgc)
                (:rpedges . ,redges) 
                (:pedges . ,(rest (assoc :pedges summary)))
                (:aedges . ,(rest (assoc :aedges summary)))
                (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks) 
                (:p-ftasks . ,f-tasks) (:p-ctasks . ,c-tasks) 
                (:l-stasks . ,(rest (assoc :l-stasks summary)))
                (:words . ,(rest (assoc :words summary)))
                (:total . ,total) (:first . ,first) 
                (:unifications . ,unifications) (:copies . ,copies)
                (:readings . ,readings)
                (:error . ,(pprint-error output))
                (:comment . ,comment)
                (:results .
                 ,(append
                   (unless (and packingp nil)
                     (loop
                         with *package* = *lkb-package*
                         with nresults = (if (<= nresults 0)
                                           (length *parse-record*)
                                           nresults)
                         for i from 0
                         for parse in (reverse *parse-record*)
                         for time = (if (integerp (first times))
                                      (round (* (- (pop times) start) 1000)
                                             internal-time-units-per-second )
                                      total)
                         for derivation = (with-standard-io-syntax
                                            (let ((*package* *lkb-package*))
                                              (write-to-string
                                               (compute-derivation-tree parse) 
                                               :case :downcase)))
                         for r-redges = (length 
                                         (parse-tsdb-distinct-edges parse nil))
                         for size = (parse-tsdb-count-nodes parse)
                         for tree = (tsdb::call-hook trees-hook parse)
                         for mrs = (tsdb::call-hook semantix-hook parse)
                         for score = (edge-score parse)
                         for flags = (acons :ascore score (edge-flags parse))
                         while (>= (decf nresults) 0) collect
                           (pairlis '(:result-id :mrs :tree
                                      :derivation :r-redges :size
                                      :r-stasks :r-etasks 
                                      :r-ftasks :r-ctasks
                                      :time :flags)
                                    (list i mrs tree
                                          derivation r-redges size
                                          -1 -1 
                                          -1 -1 
                                          time flags))))
                   (when (< nresults 0)
                     (loop
                         for i from (- (length *parse-record*) 1)
                         for derivation in (rest (assoc :derivations summary))
                         for string = (with-standard-io-syntax
                                        (let ((*package* *lkb-package*))
                                          (write-to-string 
                                           derivation :case :downcase)))
                         collect (pairlis '(:result-id :derivation)
                                          (list i string))))))))))))
    (unless trace (release-temporary-storage))
    (append
     (when condition
         (let* ((error (tsdb::normalize-string 
                        (format nil "~a" condition)))
                (error (pprint-error error)))
           (pairlis '(:readings :condition :error)
                    (list -1 (unless burst condition) error))))
     return))))

;;;
;;; ToDo
;;; 
;;; - more error reporting from inside of generate-from-mrs()
;;; 

(defun tsdb::generate-item (mrs
                      &key id string exhaustive nanalyses trace
                           edges derivations semantix-hook trees-hook
                           filter burst (nresults 0))
  (declare (ignore exhaustive derivations trees-hook)
           (special tsdb::*process-scope-generator-input-p*))

  (let* ((*package* *lkb-package*)
         (mrs:*lnkp* :characters)
         (filterp (member :surface filter))
         (*maximum-number-of-edges* 
          (if (or (null edges) (zerop edges)) *maximum-number-of-edges* edges))
         (*gen-first-only-p* nil)
         (*do-something-with-parse* nil)
         (stream (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         (start (get-internal-run-time)) stop
         tgc tcpu treal conses symbols others)
    (nconc
     (handler-case
         (multiple-value-bind (strings f-tasks e-tasks s-tasks
                               unifications copies aedges pedges)
             (tsdb::time-a-funcall
              #'(lambda ()
                  (when (mrs:eds-p mrs)
                    (let* ((item (pairlis '(:-id :i-input) (list id string)))
                           (client 
                            (tsdb::allocate-client
                             item :class :eds :task :transfer :wait 1)))
                      (unless client (error "unable to transfer input EDS"))
                      (let* ((item
                              (pvm:revaluate 
                               (tsdb::client-tid client)
                               `(tsdb::transfer-item
                                 ,(format nil "~a" mrs) :trace t)))
                             (result (first (tsdb:get-field :results item))))
                        (tsdb::free-client client)
                        (setf mrs (tsdb:get-field :mrs result))
                        (when (stringp mrs)
                          (setf mrs (mrs::read-mrs-from-string mrs))))))
                  (when (or (null mrs) (not (mrs::psoa-p mrs)))
                    (error "null or malformed input MRS"))
                  (unless (or (null tsdb::*process-scope-generator-input-p*)
                              (mrs::make-scoped-mrs mrs))
                    (error "input MRS does not scope"))
                  (let (#+:pooling (*dag-recycling-p* (null trace)))
                    (generate-from-mrs mrs :signal t :nanalyses nanalyses)))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                  (declare (ignore ignore))
                  (setf stop (get-internal-run-time))
                  (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                        conses (* scons 8) symbols (* ssym 24) 
                        others sother)))

           ;;
           ;; score all edges, including using the LM feature, and re-order the
           ;; global *gen-record*; really, the generator should do this itself.
           ;;
           #+:logon
           (let* ((lms #+:lm
                       (when mt::*lm-model*
                         (mt::lm-score-strings
                          strings :measure '(:logprob :perplexity)))
                       #-:lm
                       nil)
                  (scores
                   (loop
                       for edge in *gen-record*
                       for foo = (rest (pop lms))
                       for lm = (first foo)
                       for perplexity = (second foo)
                       for flags
                       = (pairlis '(:lm :perplexity) (list lm perplexity))
                       when (edge-flags edge)
                       do (nconc (edge-flags edge) flags)
                       else do (setf (edge-flags edge) flags)
                       collect
                         ;;
                         ;; _fix_me_
                         ;; treat fragments properly.           (18-may-07; oe)
                         ;; --- but then, the original motivation for using the
                         ;; perplexity (rather than logprob) score was to avoid
                         ;; penalizing longer strings, which often correspond
                         ;; to more MRS fragments succeeding in generation.  
                         ;; thus, it is not entirely clear what `properly' 
                         ;; should mean here.  just using the perplexity score,
                         ;; on the other hand, throws off re-ranking, skewing
                         ;; the range of values it will see as the realization 
                         ;; ranker score.                       (29-jan-08; oe)
                         ;;
                         (tsdb::mem-score-result
                          (pairlis '(:edge :lm) (list edge lm)))
                         #+:null
                         (if (edge-dag edge)
                           (tsdb::mem-score-result
                            (pairlis '(:edge :lm) (list edge lm)))
                           (- perplexity)))))
             (loop
                 for edge in *gen-record*
                 for score in scores
                 do (setf (edge-score edge) score))
             (setf *gen-record* (sort *gen-record* #'> :key #'edge-score)))

           (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                  (output (get-output-stream-string stream))
                  (words (length %generator-lexical-items%))
                  (readings (length strings))
                  (readings (if (or (equal output "") (> readings 0))
                              readings
                              -1))
                  #+:pooling
                  (pool (and (find-symbol "*DAG-POOL*")
                             (boundp (find-symbol "*DAG-POOL*"))
                             (symbol-value (find-symbol "*DAG-POOL*"))))
                  #+:pooling
                  (position (when pool
                              (funcall 
                               (symbol-function (find-symbol "POOL-POSITION"))
                               pool)))
                  #+:pooling
                  (garbage (when pool
                             (funcall 
                              (symbol-function (find-symbol "POOL-GARBAGE"))
                              pool)))
                  (comment (format nil "~{~(~s~)~^ ~}" %generator-statistics%))
                  (comment 
                   (if *gen-packing-p*
                    (format
                     nil
                     "~a (:subsumptions . ~d) (:equivalence . ~d) ~
                      (:proactive . ~d) (:retroactive . ~d)"
                     comment 
                     (statistics-subsumptions *statistics*)
                     (statistics-equivalent *statistics*)
                     (statistics-proactive *statistics*) 
                     (statistics-retroactive *statistics*))
                    comment))
                  #+:pooling
                  (comment
                   (format 
                    nil 
                    "~a (:pool . ~d) (:garbage . ~d)" 
                    comment position garbage))
                  surfaces)
             `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
               (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
               (:pedges . ,pedges) (:aedges . ,aedges)
               (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks)
               (:p-ftasks . ,f-tasks)
               (:unifications . ,unifications) (:copies . ,copies)
               (:words . ,words) (:readings . ,readings)
               (:error . ,(pprint-error output))
               (:comment . ,comment)
               (:results .
                ,(loop
                     with edges = *gen-record*
                     with *package* = *lkb-package*
                     with nresults = (if (<= nresults 0)
                                       (length edges)
                                       nresults)
                     for i from 0
                     for edge in edges
                     for surface = (let ((surface (edge-string edge)))
                                     (if (consp surface)
                                       (format nil "~{~(~a~)~^ ~}" surface)
                                       surface))
                     for derivation = (if edge
                                        (with-standard-io-syntax
                                          (let ((*package* *lkb-package*))
                                            (write-to-string
                                             (compute-derivation-tree edge) 
                                             :case :downcase)))
                                        "")
                     for size = (if edge (parse-tsdb-count-nodes edge) -1)
                     for mrs = (if edge
                                 (if (edge-mrs edge)
                                   (with-output-to-string (stream)
                                     (mrs::output-mrs1
                                      (edge-mrs edge) 'mrs::simple stream))
                                   (tsdb::call-hook semantix-hook edge))
                                 "")
                     for score = (edge-score edge)
                     for flags = (acons :rscore score (edge-flags edge))
                     while (>= (decf nresults) 0)
                     unless (when filterp
                              (member surface surfaces :test #'equal))
                     collect (pairlis '(:result-id :mrs :tree :surface
                                        :derivation :score :size :flags)
                                      (list i mrs surface surface
                                            derivation score size flags))
                     and do (push surface surfaces)
                     finally (release-temporary-storage :task :generate)))))) 

       (condition (condition)
         (unless stop (setf stop (get-internal-run-time)))
         (unless trace (release-temporary-storage :task :generate))
         
         (let* ((error (tsdb::normalize-string 
                        (format nil "~a" condition)))
                (error (pprint-error error)))
           (pairlis '(:readings :condition :error)
                    (list -1 (unless burst condition) error)))))
     (let ((total
            (round (* (- stop start) 1000) internal-time-units-per-second)))
       (acons :total total nil)))))


(defun pprint-error (string)
  (let* ((string (tsdb::normalize-string string))
         (limit "probable runaway rule:")
         (n (min (length string) (min (length limit))))
         (index "Some lexical entries could not be found from MRS"))
    (cond
     ((string-equal string limit :end1 n)
      (format nil "edge limit (~a)" *maximum-number-of-edges*))
     ((search index string)
      "unknown input relation(s): generator may be uninitialized")
     (t string))))

;;;
;;; _fix_me_ 
;;; this is part of the solution for functionality that francis and eric
;;; require in their efforts: for them to try and learn things from partial
;;; transfer results, they want `fragment' transfers in the profile, much like
;;; a regular result.  this would likely not make sense within the MT pipeline,
;;; as there is little reason to expect the generator to gracefully handle such
;;; fragment transfers, but for off-line experimentation there may be utility
;;; in this.  presumably, though, [incr tsdb()] should have a general notion of
;;; fragments and abstract from specific tasks, i.e. this parameter will
;;; eventually morph into something more like *tsdb-fragments-p* (or
;;; thereabout), hence we will not document it (not even on the wiki, francis),
;;; and there is no commitment, express or implied, to supporting this specific
;;; parameter.                                                  (12-jul-07; oe)
;;;
(defparameter tsdb::*tsdb-transfer-include-fragments-p* nil)

(defun tsdb::transfer-item (mrs
                      &key id string exhaustive nanalyses trace
                           edges derivations semantix-hook trees-hook
                           filter burst (nresults 0) (fragmentp t)
                           (partialp
                            tsdb::*tsdb-transfer-include-fragments-p*))
  
  (declare (ignore edges derivations string id exhaustive nanalyses
                   filter semantix-hook trees-hook))

  (let* ((*package* *lkb-package*)
         (stream (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         tgc tcpu treal conses symbols others)

    (multiple-value-bind (return condition)
        (#-:debug ignore-errors #+:debug progn
         (when (stringp mrs)
           (setf mrs (mrs:read-mrs-or-eds-from-string mrs)))
         (when (mrs:eds-p mrs)
           (setf mrs (mrs:eds-to-mrs mrs)))
         (when (or (null mrs) (not (mrs::psoa-p mrs)))
           (error "null or malformed input MRS"))
         (when (and (null fragmentp) (mt:fragmentp mrs))
           (error "fragmented input MRS"))
         (let* ((size (length (mrs:psoa-liszt mrs)))
                (edges
                 (tsdb::time-a-funcall
                  #'(lambda () 
                      (mt:transfer-mrs
                       mrs :filter nil :debug nil
                       :preemptive (not partialp) :block t))
                  #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                      (declare (ignore ignore))
                      (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                            conses (* scons 8) symbols (* ssym 24) 
                            others sother))))
                (pedges (loop
                            for edge in edges
                            maximize (mt::edge-id edge)))
                (partial (loop
                             for edge in edges
                             when (mt::edge-source edge) collect edge))
                (unknown (loop
                             with result
                             for edge in partial
                             do
                               (loop
                                   for ep in (mt::edge-source edge)
                                   for pred = (mrs:rel-pred ep)
                                   do (pushnew pred result :test #'equal))
                             finally (return (sort result #'string-lessp))))
                (invalid (loop
                             with result
                             for edge in edges
                             when (and (null (mt::edge-source edge))
                                       (mt::edge-semi edge))
                             do
                               (loop
                                   for ep in (mt::edge-semi edge)
                                   for pred = (mrs:rel-pred ep)
                                   do (pushnew pred result :test #'equal))
                           finally (return (sort result #'string-lessp))))
                (*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (output (if unknown
                          (format
                           nil
                           "invalid transfer predicates: ~{|~(~s~)|~^, ~}"
                           unknown)
                          (get-output-stream-string stream)))
                (output (if invalid
                          (format
                           nil
                           "~@[~a; ~]~
                            invalid output predicates: ~{|~(~s~)|~^, ~}"
                           (and output (not (equal output "")) output) invalid)
                          output))
                (readings (- (length edges) (if partialp 0 (length partial))))
                (readings (if (or (equal output "") (> readings 0))
                              readings -1))
                (n 0))
           (unless partialp
             (setf edges 
               (loop 
                   for edge in edges 
                   unless (or (mt::edge-source edge) (mt::edge-semi edge))
                   collect edge)))
           #+:debug
           (when invalid
             (format
              excl:*initial-terminal-io*
              "~&transfer-item(): ~{~(~s~)~^, ~}.~%" invalid))
           
           `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
             (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
             (:readings . ,readings)
             (:pedges . ,pedges)
             (:error . ,(pprint-error output))
             (:unknown . ,unknown)
             (:results .
              ,(loop
                   with *package* = *lkb-package*
                   with nresults = (if (<= nresults 0) (length edges) nresults)
                   for i from 0
                   for edge in edges
                   for tree = (with-standard-io-syntax
                                (let ((*package* *lkb-package*))
                                  (write-to-string edge :case :downcase)))
                   for mrs = (let ((mrs (mt::edge-mrs edge)))
                               (with-output-to-string (stream)
                                 (mrs::output-mrs1 mrs 'mrs::simple stream)))
                   for mtrs
                   = (loop
                         for next = edge then (mt::edge-daughter next)
                         for mtr = (and next (mt::edge-rule next))
                         while mtr
                         collect (intern (mt::mtr-id mtr) :tsdb))
                   for ratio = (tsdb::divide
                                (length (mrs:psoa-liszt (mt::edge-mrs edge)))
                                size)
                   for flags
                   = (pairlis '(:tscore :tratio :mtrs :nmtrs :unknown)
                              (list (mt::edge-score edge) (float ratio)
                                    mtrs (length mtrs)
                                    (length (mt::edge-source edge))))
                   while (<= (incf n) nresults) collect
                     (pairlis '(:result-id :mrs :tree :flags)
                              (list i mrs tree flags))))
             (:nresults . ,n))))
      (append
       (when condition
         (let* ((*print-readably* nil)
                (error (tsdb::normalize-string 
                        (format nil "~a" condition)))
                (error (pprint-error error)))
           (pairlis '(:readings :condition :error)
                    (list -1 (unless burst condition) error))))
       return))))


(defun compute-derivation-tree (edge)
  (labels ((edge-label (edge)
             (intern 
              (typecase (edge-rule edge)
                (string (string-upcase (edge-rule edge)))
                (symbol (string (edge-rule edge)))
                (rule (string (rule-id (edge-rule edge))))
                (t :unknown))
              *lkb-package*))
           (unfold-generator-leaf (edge id score start end rules)
             (if (null rules)
               (list id (first (edge-lex-ids edge)) score start end
                     (list (edge-rule edge) start end))
               (list id (intern (first rules) *lkb-package*) score start end
                     (unfold-generator-leaf
                      edge id score start end (rest rules))))))
                 
    (let* ((id (edge-id edge))
           (score (float (or (edge-score edge) 0)))
           (configuration (and (null (edge-children edge))
                               (find-chart-configuration :edge edge)))
           (start (or (edge-from edge)
                      (when configuration 
                        (chart-configuration-begin configuration))
                      -1))
           (end (or (edge-to edge)
                    (when configuration 
                      (chart-configuration-end configuration))
                    -1))
           (lexemes (when (g-edge-p edge) (g-edge-lexemes edge))))
      
      (cond
       ((and (g-edge-p edge) lexemes
             (not (null (mrs::found-lex-rule-list (first lexemes))))
             (null (edge-children edge)))
        (let ((rules (mrs::found-lex-rule-list (first lexemes))))
          (unfold-generator-leaf edge id score start end rules)))

       ((null (edge-children edge))
        (list id (first (edge-lex-ids edge)) score start end
              (list (edge-rule edge) start end)))

       (t
        (let* ((start *chart-limit*)
               (end 0)
               (children
                (loop
                    for child in (edge-children edge)
                    for derivation = (compute-derivation-tree child)
                    for cstart = (fourth derivation)
                    for cend = (fifth derivation)
                    when cstart do (setf start (min start cstart))
                    when cend do (setf end (max end cend))
                    collect derivation)))
          (nconc (list id (edge-label edge) score start end)
                 children)))))))

(defun find-chart-configuration (&key edge id)
  (loop
      for i from 0 to (- *chart-limit* 1)
      for configurations = (aref *chart* i 0)
      thereis
        (loop
            for configuration in configurations
            for candidate = (chart-configuration-edge configuration)
            when (or (and edge (eq edge candidate))
                     (and id (eq id (edge-id candidate))))
            return configuration)))

(defun parse-tsdb-sentence (user-input &optional trace)
  (multiple-value-prog1
      (let (#+:pooling
            (*dag-recycling-p* (null trace)))
        (parse user-input trace))))


;;;
;;; _fix_me_
;;; this function is flawed in that it miscounts words and successful lexical
;;; rule applications (see comment in summarize-chart()); hence, the caller
;;; currently ignores everything but the `redges' count.   (26-may-99  -  oe)
;;;
;;; _fix_me_
;;; when migrating to the chart-based application of orthographemics, none of
;;; the following functions were updated properly; thus, it is very likely that
;;; chart-related counts are now wrong.                         (3-aug-05; oe)
;;;
(defun parse-tsdb-count-lrules-edges-morphs ()
   (let ((distinct-parse-edges nil)
         (successful-lrule-applications 0))
      (dolist (p *parse-record*)
         (setq distinct-parse-edges
           (parse-tsdb-distinct-edges p distinct-parse-edges)))
      (dotimes (vertex (- *chart-limit* 1))
         (when (aref *chart* (+ 1 vertex) 0)
           (dolist (config (aref *chart* (+ 1 vertex) 0))
             (when (lexical-rule-p
                    (edge-rule (chart-configuration-edge config)))
                (incf successful-lrule-applications)))))
      (values successful-lrule-applications (length distinct-parse-edges)
              0)))

(defun summarize-chart (&key derivationp)
  (loop
      with pedges = 0
      with i-stasks = 0
      with words = 0
      with l-stasks = 0
      with derivations = nil
      for i from 0 to (- *chart-limit* 1)
      for configurations = (aref *chart* i 0)
      sum (length (aref *achart* i 0)) into aedges
      when configurations
      do
        (loop
            for configuration in configurations
            for edge = (chart-configuration-edge configuration)
            for rule = (edge-rule edge)
            for tdfs = (edge-dag edge)
            for dag = (and tdfs (tdfs-indef tdfs))
            do 
              (when derivationp 
                (when (and dag (dag-inflected-p dag))             
                  (push (compute-derivation-tree edge) derivations)))
              (cond
               #+:null
               ((and (rule-p rule) (tsdb::inflectional-rule-p (rule-id rule)))
                (incf i-stasks)
                (incf words))
               ((rule-p rule)
                (incf pedges)
                (when (lexical-rule-p rule) (incf l-stasks)))
               ((not (rule-p rule))
                (when (and dag (dag-inflected-p dag)) (incf words)))))
      finally (return (pairlis '(:pedges :aedges 
                                 :words :i-stasks :l-stasks
                                 :derivations)
                               (list (+ pedges words) aedges 
                                     words i-stasks l-stasks
                                     derivations)))))
              

(defun summarize-generator-chart (&key derivationp)
  (loop
      with pedges = 0
      with i-stasks = 0
      with words = 0
      with l-stasks = 0
      with derivations = nil
      for i from 0 to (- *chart-limit* 1)
      for configurations in (reverse *gen-chart*)
      sum (length (aref *achart* i 0)) into aedges
      when configurations
      do
        (loop
            for configuration in configurations
            for edge = (chart-configuration-edge configuration)
            for rule = (edge-rule edge)
            for tdfs = (edge-dag edge)
            for dag = (and tdfs (tdfs-indef tdfs))
            do 
              (when derivationp 
                (when (and dag (dag-inflected-p dag))             
                  (push (compute-derivation-tree edge) derivations)))
              (cond
               #+:null
               ((and (rule-p rule) (tsdb::inflectional-rule-p (rule-id rule)))
                (incf i-stasks)
                (incf words))
               ((rule-p rule)
                (incf pedges)
                (when (lexical-rule-p rule) (incf l-stasks)))
               ((not (rule-p rule))
                (when (and dag (dag-inflected-p dag)) (incf words)))))
      finally (return (pairlis '(:pedges :aedges 
                                 :words :i-stasks :l-stasks
                                 :derivations)
                               (list (+ pedges words) aedges 
                                     words i-stasks l-stasks
                                     derivations)))))

(defun parse-tsdb-distinct-edges (edge found)
   ;; collect edge for top lrule on each branch and all above
   (pushnew edge found :test #'eq)
   (when (and (edge-children edge)
            (not (lexical-rule-p (edge-rule edge))))
      (dolist (c (edge-children edge))
         (setq found (parse-tsdb-distinct-edges c found))))
   found)

(defun parse-tsdb-count-nodes (edge)
   (labels
      ((parse-tsdb-count-nodes1 (dag n)   
          (unless (dag-visit dag)
             (setf (dag-visit dag) t)
             (incf n)
             (dolist (arc (dag-arcs dag))
                (setq n (parse-tsdb-count-nodes1 (dag-arc-value arc) n))))
          n))
      (invalidate-visit-marks)
      (when (edge-dag edge)
        (parse-tsdb-count-nodes1 (tdfs-indef (edge-dag edge)) 0))))

;;;
;;; abstract from recent changes in LKB lexicon interface (28-jan-99  -  oe)
;;;

(defun uncache-lexicon ()
  (cond 
   ((and (find-symbol "CLEAR-EXPANDED-LEX" :lkb)
         (fboundp (find-symbol "CLEAR-EXPANDED-LEX" :lkb)))
    (funcall (symbol-function (find-symbol "CLEAR-EXPANDED-LEX" :lkb))))
   ((and (find-symbol "*PSORTS*") (boundp (find-symbol "*PSORTS*")))
    (maphash
     #'(lambda (id value)
         (declare (ignore id))
         (setf (cddr value) nil))
     (symbol-value (find-symbol "*PSORTS*"))))))


(defun size-of-lexicon ()
  (let ((*package* *lkb-package*))
    (cond
     ((and (find-symbol "*LEXICON*") (boundp (find-symbol "*LEXICON*")))
      (length (collect-expanded-lex-ids 
               (symbol-value (find-symbol "*LEXICON*")))))
     ((and (find-symbol "*PSORTS*") (boundp (find-symbol "*PSORTS*")))
      (hash-table-count (symbol-value (find-symbol "*PSORTS*"))))
     (t
      -1))))
    

;;;
;;; make an attempt to release references to temporary objects; because the
;;; Lisp gc() mechanism cannot interpret the generation counter mechanism, we
;;; need to explicitly reset all scratch slots.
;;;

(defun release-temporary-storage (&key (task :parse))
  (purge-edge-registry)
  (invalidate-marks)
  (invalidate-visit-marks)
  #+:pooling
  (reset-pools :compressp t)
  (case task
    (:parse
     (loop
         for i from 0 to (- *chart-limit* 1)
         for configurations = (aref *chart* i 0)
         while configurations
         do
           (loop
               for configuration in configurations
               for edge = (chart-configuration-edge configuration)
               for odag = (edge-odag edge)
               for tdfs = (if (dag-p odag) odag (edge-dag edge))
               for dag = (tdfs-indef tdfs)
               unless (safe-dag-p dag) do (compress-dag dag))
         finally
           (clear-chart)
           (clear-achart)
           (setf *parse-times* nil)))
    (:generate
     (loop
         for item in %generator-lexical-items%
         for tdfs = (mrs::found-lex-inst-fs item)
         when (tdfs-p tdfs) do (compress-dag (tdfs-indef tdfs)))
     (clear-gen-chart)))
  (loop
      for rule in (get-matching-lex-rules nil)
      for tdfs = (if (or (and (eq task :parse) *chart-packing-p*)
                         (and (eq task :generate) *gen-packing-p*))
                   (rule-rtdfs rule)
                   (rule-full-fs rule))
      for dag = (tdfs-indef tdfs)
      do (compress-dag dag))
  (loop
      for rule in (get-matching-rules nil nil)
      for dag = (tdfs-indef (rule-full-fs rule)) do
        (compress-dag dag))
  (ignore-errors
    (if (listp *start-symbol*)
      (loop
          for root in *start-symbol* 
          for tdfs = (get-tdfs-given-id root)
          for dag = (and tdfs (tdfs-indef tdfs))
          when (dag-p dag) do (compress-dag dag))
      (let* ((tdfs (get-tdfs-given-id *start-symbol*))
             (dag (and tdfs (tdfs-indef tdfs))))
        (when (dag-p dag) (compress-dag dag)))))
  (let* ((*terminal-io* excl:*initial-terminal-io*)
         (*standard-output* *terminal-io*))
    (when (eq task :generate)
      #+:debug
      (sys:gsgc-parameters)
      #+:debug
      (excl:gc)
      #+:debug
      (sys:gsgc-parameters)
      #+:debug
      (room t))))

;;;
;;; interface functions for reconstruction of derivations (in UDF --- unified
;;; derivation format |:-).
;;;

(defparameter tsdb::*reconstruct-hook*
  #-:tty
  #'(lambda (edge &optional (i-input "reconstructed item"))
      (when (edge-p edge)
        (if #+:lui (lui-status-p :tree) #-:lui nil
          #+:lui (lui-show-parses (list edge) i-input) #-:lui nil
          (display-parse-tree edge nil))))
  #+:tty
  nil)

(defun tsdb::find-lexical-entry (form instance 
                                 &optional id start end (dagp t))

  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (name (string-upcase (string instance)))
         (n (length name))
         (bracket (position #\[ name))
         (prefix (if (and bracket (char= (char name (- n 1)) #\]))
                   (subseq name 0 bracket)
                   name))
         (surface (when bracket
                    (subseq name (+ bracket 1) (- n 1))))
         (name (intern prefix *lkb-package*))
         (instance (ignore-errors (get-unexpanded-psort-entry name))))
    (when instance 
      (let* ((length (length (lex-entry-orth instance)))
             (end (if (numberp end)
                    end
                    (and (numberp start) (+ start length))))
             (instance (if (smember dagp '(:word t))
                         (get-lex-entry-from-id name)
                         instance))
             (tdfs (when (smember dagp '(:word t))
                     (if surface
                       (instantiate-generic-lexical-entry instance surface)
                       (copy-tdfs-completely (lex-entry-full-fs instance)))))
             ;;
             ;; _fix_me_
             ;; by calling lnk-tdfs() outside of a unification context, we end
             ;; up making another copy of .tdfs. :-{.  it should suffice to use
             ;; lnk-tdfs() directly on the FS of the lexical entry (as it will
             ;; then end up making a copy after the unifications, but then we
             ;; would still have to worry about getting the LNK effect into the
             ;; instantiate-generic-lexical-entry() call.        (7-dec-06; oe)
             ;;
             (tdfs (if (eq mrs::*lnkp* :id) (lnk-tdfs tdfs (list id)) tdfs))
             (ids (list (lex-entry-id instance))))
        (values
         (make-edge :id id :category (and tdfs (indef-type-of-tdfs tdfs))
                    :rule form :leaves (list form) :lex-ids ids
                    :dag tdfs :from start :to end
                    #-:logon :cfrom #-:logon (mrs::find-cfrom-hack start)
                    #-:logon :cto #-:logon (mrs::find-cto-hack start))
         length)))))

(defun tsdb::find-lexical-type (form instance 
                                &optional id start end (dagp t))

  (let* ((*package* *lkb-package*)
         (*edge-registry* nil)
         (name (string-upcase (string instance)))
         (offset (if (char= (char name 0) #\@) 1 0))
         (name (intern (subseq name offset) *lkb-package*))
         (instance (ignore-errors
                    (eval-possible-leaf-type *leaf-types* name)
                    (get-type-entry name))))
    (when instance 
      (let* ((tdfs (when (smember dagp '(:word t))
                     (copy-tdfs-completely (ltype-tdfs instance))))
             ;;
             ;; _fix_me_
             ;; by calling lnk-tdfs() outside of a unification context, we end
             ;; up making another copy of .tdfs. :-{.  it should suffice to use
             ;; lnk-tdfs() directly on the FS of the lexical entry (as it will
             ;; then end up making a copy after the unifications, but then we
             ;; would still have to worry about getting the LNK effect into the
             ;; instantiate-generic-lexical-entry() call.        (7-dec-06; oe)
             ;;
             (tdfs (if (eq mrs::*lnkp* :id) (lnk-tdfs tdfs (list id)) tdfs)))
        (values
         (make-edge :id id :category (and tdfs (indef-type-of-tdfs tdfs))
                    :rule form :leaves (list form) :lex-ids nil
                    :dag tdfs :from start :to end
                    #-:logon :cfrom #-:logon (mrs::find-cfrom-hack start)
                    #-:logon :cto #-:logon (mrs::find-cto-hack start))
         (length form))))))

(defun tsdb::instantiate-lexical-entry (edge tokens 
                                        &optional (dagp t) robustp)

  (let* ((dagp (smember dagp '(:rule t)))
         (*unify-debug* :return)
         (%failure% nil)
         (status -1)
         (result (when dagp (edge-dag edge))))
    (when (and result *lexicon-tokens-path* *lexicon-last-token-path*)
      (with-unification-context (foo)
        (loop
            for token in tokens
            for path = *lexicon-tokens-path*
            then (append path *list-tail*)
            for i from 0
            while result do
              (setf result
                (if robustp
                  (debug-yadu!
                   result token (append path *list-head*)
                   :robustp robustp)
                  (yadu! result token (append path *list-head*))))
              (incf status)
                                  
            finally
              (when result
                (let ((token (first (last tokens))))
                  (setf result
                    (if robustp
                      (debug-yadu!
                       result token *lexicon-last-token-path* :robustp robustp)
                      (yadu! result token *lexicon-last-token-path*))))
                (when result
                  (setf (edge-dag edge) (copy-tdfs-elements result)))))))
    (if (or result (null dagp))
      (values edge nil)
      (values status %failure%))))

(defun tsdb::find-rule (instance)
  (let* ((name (intern (if (stringp instance)
                         (string-upcase instance)
                         instance)
                       *lkb-package*))
         (rule (or (get-lex-rule-entry name)
                   (get-grammar-rule-entry name))))
    rule))

(defun tsdb::instantiate-rule (rule edges id
                               &optional (dagp t) robustp)
  (let* ((dagp (smember dagp '(:rule t)))
         (*edge-registry* nil)
         (*unify-debug* :return)
         (%failure% nil)
         (status 0)
         (result (when dagp 
                   #+:restrict
                   (if *chart-packing-p* (rule-rtdfs rule) (rule-full-fs rule))
                   #-:restrict
                   (rule-full-fs rule)))
         (paths (rule-order rule)))
    (when result
      (with-unification-context (foo)
        (loop
            while result
            for path in (rest paths)
            for edge in edges
            for tdfs = (edge-dag edge)
            for i from 0
            do
              (setf status i)
              (setf result
                (if robustp
                  (debug-yadu! result tdfs path :robustp robustp)
                  (yadu! result tdfs path)))

            finally
              (when result
                (when (eq mrs::*lnkp* :id) (lnk-tdfs result (list id)))
                (setf result
                  (if robustp
                    (let ((dag (debug-copy-dag
                                (tdfs-indef result) nil :robustp robustp)))
                      (make-tdfs :indef dag))
                    (restrict-and-copy-tdfs result)))))))
    #+:null (check-dag (tdfs-indef result))
    (if (or result (null dagp))
      (values
       (make-edge :id id :category (and result (indef-type-of-tdfs result))
                  :rule rule
                  :leaves (loop for edge in edges append (edge-leaves edge))
                  :lex-ids (loop
                               for edge in edges
                               append (edge-lex-ids edge))
                  :dag result :children edges
                  :from (edge-from (first edges))
                  :to (edge-to (first (last edges))))
       nil)
      (values status %failure%))))

(defun tsdb::type-of-rule (name &optional (package *lkb-package*))
  (let* ((rule (tsdb::find-rule name))
         (type (and rule (indef-type-of-tdfs (rule-full-fs rule)))))
    (when type
      (intern type package))))

(defun tsdb::label-edge (edge)
  (labels ((recurse (edge node)
             (when (and edge node)
               (setf (edge-label edge) (get-string-for-edge node))
               (loop
                   for edge in (edge-children edge)
                   for node in (get node 'daughters)
                   do (recurse edge node)))))
    (recurse edge (make-new-parse-tree edge 1 t))))

;;;
;;; RMRS comparison
;;; 
;;; this stuff can't go in LKB files because the tsdb package isn't available
;;; when LKB is loaded
;;;

(defun get-test-suite-sentences (dir)
  ;;; returns assoc list of id and string
  (loop
      for item in (tsdb::analyze dir)
      for id = (tsdb::get-field :i-id item)
      for input = (tsdb::get-field :i-input item)
      collect
        (cons id input)))

;;; FIX - add some error checking!

(defun get-tsdb-selected-rasp-rmrs (item dir)
  (let* ((data dir)
         (frame (tsdb::browse-trees data :runp nil :verbose nil)))
    (tsdb::browse-tree 
     data item frame :runp nil :verbose nil)
    (let* ((edges (or (compare-frame-in frame) (compare-frame-edges frame)))
           (tsdb-rasp-tree (if edges (edge-bar (first edges))))
           (mrs::*rasp-xml-word-p* t)   ; FIX - RASP `script'
           (mrs::*initial-rasp-num*
            (when tsdb-rasp-tree
              (mrs::scan-rasp-for-first-num
               tsdb-rasp-tree most-positive-fixnum))))
      (when tsdb-rasp-tree
        (mrs::construct-sem-for-tree tsdb-rasp-tree :rasp :quiet)))))
          
(defun get-tsdb-selected-erg-rmrs (item dir)
  (let* ((data dir)
         (frame (tsdb::browse-trees data :runp nil :verbose nil)))
    ;;; force reconstruction of dags by browse-tree
    (tsdb::browse-tree 
     data item frame :runp nil :verbose nil)
    (let ((edges (or (compare-frame-in frame) (compare-frame-edges frame))))
      (if edges
          (let ((chosen (first edges)))
            (mrs::mrs-to-rmrs 
             (mrs::extract-mrs
              (tsdb::reconstruct (compute-derivation-tree chosen) t))))))))



    
