;;;
;;; while preparing for the sep-99 release of the LKB we need to do some magic 
;;; to support the pre-release and current development versions in the same
;;; source file.  searching for find-symbol() and friends should be a good way
;;; to find the all-too-many kludges that can be eliminated once the release
;;; has been made public ... sigh.                         (22-aug-99  -  oe)
;;;

(in-package :lkb)


(defparameter *lkb-package* 
  (or (find-package :lkb) (find-package :common-lisp-user)))
  
;;;
;;; another instance of versioning kludges: yadu!() only comes in with the new
;;; active parser in the current development version       (26-aug-99  -  oe)
;;;
(defmacro uday (tdfs1 tdfs2 path)
  `(yadu ,tdfs1 (create-temp-parsing-tdfs ,tdfs2 ,path)))

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
                             (unless (zerop nanalyses) nanalyses)
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
                        burst (nresults 0))
  (declare (ignore derivations))
  
  (let* ((*package* *lkb-package*)
         (*chasen-debug-p* nil)
         (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                      *maximum-number-of-edges*
                                      edges))
         (*first-only-p* (if exhaustive
                           nil
                           (if (integerp nanalyses)
                             (unless (zerop nanalyses) nanalyses)
                             (if (integerp *first-only-p*) 
                                *first-only-p* 1))))
         (*do-something-with-parse* nil))
    (declare (special *chasen-debug-p*))
    (multiple-value-bind (return condition)
      (ignore-errors
       (let* ((sent
               (split-into-words (preprocess-sentence-string string)))
              (str (make-string-output-stream)) ; capture any warning messages
              (*standard-output* 
               (if trace
                 (make-broadcast-stream *standard-output* str)
                 str))
              (*unifications* 0)
              (*copies* 0)
              (*subsumptions* 0)
              tgc tcpu treal conses symbols others)
         (declare (special *subsumptions*))
         ;;
         ;; this really ought to be done in the parser ...  (30-aug-99  -  oe)
         ;;
         (setf *sentence* string)
         (reset-packings)
         (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks)
             (tsdb::time-a-funcall
              #'(lambda () (parse-tsdb-sentence sent trace))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setq tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                       conses (* scons 8) symbols (* ssym 24) others sother)))
          (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                 (output (get-output-stream-string str))
                 (unifications *unifications*)
                 (copies *copies*)
                 (packingp *chart-packing-p*)
                 utcpu
                 utgc
                 uspace
                 (readings (if packingp
                             (tsdb::time-a-funcall
                              #'(lambda () 
                                  (loop
                                      for edge in *parse-record*
                                      sum (length (unpack-edge! id edge))))
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
                     *subsumptions* (packings-equivalent *packings*)
                     (packings-proactive *packings*) 
                     (packings-retroactive *packings*)
                     (length *parse-record*)
                     (packings-frozen *packings*) 
                     (packings-failures *packings*)
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
                   (unless packingp
                     (loop
                         with *package* = *lkb-package*
                         with nresults = (if (<= nresults 0)
                                           (length *parse-record*)
                                           nresults)
                         for i from 1
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
                         while (>= (decf nresults) 0) collect
                           (pairlis '(:result-id :mrs :tree
                                      :derivation :r-redges :size
                                      :r-stasks :r-etasks 
                                      :r-ftasks :r-ctasks
                                      :time)
                                    (list i mrs tree
                                          derivation r-redges size
                                          -1 -1 
                                          -1 -1 
                                          time))))
                   (when (< nresults 0)
                     (loop
                         for i from (length *parse-record*)
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
                           burst (nresults 0))
  (declare (ignore derivations string id trees-hook)
           (special tsdb::*process-scope-generator-input-p*))

  (let* ((*package* *lkb-package*)
         (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                      *maximum-number-of-edges*
                                      edges))
         (*gen-first-only-p* (if exhaustive
                               nil
                               (if (integerp nanalyses)
                                 (unless (zerop nanalyses) nanalyses)
                                 (if (integerp *gen-first-only-p*) 
                                   *gen-first-only-p*
                                   1))))
         (*do-something-with-parse* nil)
         (stream (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         (*unifications* 0)
         (*copies* 0)
         (*subsumptions* 0)
         tgc tcpu treal conses symbols others)

    (multiple-value-bind (return condition)
        (ignore-errors
         (when (or (null mrs) (not (mrs::psoa-p mrs)))
           (error "null or malformed input MRS"))
         (unless (or (null tsdb::*process-scope-generator-input-p*)
                     (mrs::make-scoped-mrs mrs))
           (error "input MRS does not scope"))
         (multiple-value-bind (strings f-tasks e-tasks s-tasks
                               unifications copies aedges pedges)
             (tsdb::time-a-funcall
              #'(lambda ()
                  (let (#+:pooling (*dag-recycling-p* (null trace)))
                    (generate-from-mrs mrs)))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                  (declare (ignore ignore))
                  (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                        conses (* scons 8) symbols (* ssym 24) 
                        others sother)))

           (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                  (output (get-output-stream-string stream))
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
                     *subsumptions* (packings-equivalent *packings*)
                     (packings-proactive *packings*) 
                     (packings-retroactive *packings*))
                    comment))
                  #+:pooling
                  (comment
                   (format 
                    nil 
                    "~a (:pool . ~d) (:garbage . ~d)" 
                    comment position garbage)))
             `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
                (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
                (:pedges . ,pedges) (:aedges . ,aedges)
                (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks)
                (:p-ftasks . ,f-tasks)
                (:unifications . ,unifications) (:copies . ,copies)
                (:readings . ,readings)
                (:error . ,(pprint-error output))
                (:comment . ,comment)
                (:results .
                 ,(loop
                      with *package* = *lkb-package*
                      with nresults = (if (<= nresults 0)
                                        (length *gen-record*)
                                        nresults)
                      for i from 1
                      for parse in *gen-record*
                      for string in strings
                      for derivation = (with-standard-io-syntax
                                         (let ((*package* *lkb-package*))
                                           (write-to-string
                                            (compute-derivation-tree parse) 
                                            :case :downcase)))
                      for size = (parse-tsdb-count-nodes parse)
                      for tree = #-:null (format nil "~{~a~^ ~}" string)
                                 #+:null (tsdb::call-hook trees-hook parse)
                      for mrs = (tsdb::call-hook semantix-hook parse)
                      while (>= (decf nresults) 0) collect
                        (pairlis '(:result-id :mrs :tree :string
                                   :derivation :size)
                                 (list i mrs tree string
                                       derivation size))))))))
      (unless trace 
        (release-temporary-storage :task :generate))
    
      (append
       (when condition
         (let* ((error (tsdb::normalize-string 
                        (format nil "~a" condition)))
                (error (pprint-error error)))
           (pairlis '(:readings :condition :error)
                    (list -1 (unless burst condition) error))))
       return))))


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

#-:mt
(defun tsdb::transfer-item (mrs
                      &key id string exhaustive nanalyses trace
                           edges derivations semantix-hook trees-hook
                           burst (nresults 0))
  (declare (ignore mrs 
                   id string exhaustive nanalyses trace
                   edges derivations semantix-hook trees-hook
                   burst nresults)))
  
#+:mt
(defun tsdb::transfer-item (mrs
                      &key id string exhaustive nanalyses trace
                           edges derivations semantix-hook trees-hook
                           burst (nresults 0))
  (declare (ignore edges derivations string id exhaustive nanalyses
                   semantix-hook trees-hook))

  (let* ((*package* *lkb-package*)
         (stream (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         tgc tcpu treal conses symbols others)

    (multiple-value-bind (return condition)
        (ignore-errors
         (when (or (null mrs) (not (mrs::psoa-p mrs)))
           (error "null or malformed input MRS"))
         (let* ((edges
                 (tsdb::time-a-funcall
                  #'(lambda () (mt::transfer-mrs mrs :filterp nil))
                  #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                      (declare (ignore ignore))
                      (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                            conses (* scons 8) symbols (* ssym 24) 
                            others sother))))
                (partial (loop
                             for edge in edges
                             when (mt::edge-source edge) collect edge))
                (unknown (loop
                             with result
                             for edge in partial
                             do
                               (loop
                                   for ep in (mt::edge-source edge)
                                   for pred = (mrs::rel-pred ep)
                                   do (pushnew pred result :test #'equal))
                             finally (return (sort result #'string-lessp))))
                (*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (output (if unknown
                          (format
                           nil
                           "unknown predicates: ~{~(~s~)~^, ~}"
                           unknown)
                          (get-output-stream-string stream)))
                (readings (- (length edges) (length partial)))
                (readings (if (or (equal output "") (> readings 0))
                              readings -1)))
           (setf edges 
             (loop 
                 for edge in edges 
                 unless (mt::edge-source edge) collect edge))
           
           `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
             (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
             (:readings . ,readings)
             (:error . ,(pprint-error output))
             (:results .
              ,(loop
                   with *package* = *lkb-package*
                   with nresults = (if (<= nresults 0)
                                     (length edges)
                                     nresults)
                   for i from 1
                   for edge in edges
                   for tree = (with-standard-io-syntax
                                (let ((*package* *lkb-package*))
                                  (write-to-string edge :case :downcase)))
                   for mrs = (let ((mrs (mt::edge-mrs edge)))
                               (with-output-to-string (stream)
                                 (mrs::output-mrs1 mrs 'mrs::simple stream)))
                   while (>= (decf nresults) 0) collect
                     (pairlis '(:result-id :mrs :tree)
                              (list i mrs tree)))))))
      (append
       (when condition
         (let* ((error (tsdb::normalize-string 
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
       ((edge-morph-history edge)
        (list id (edge-label edge) score start end
              (compute-derivation-tree (edge-morph-history edge))))

       #+:mrs
       ((and (g-edge-p edge) lexemes
             (not (null (mrs::found-lex-rule-list (first lexemes))))
             ;;
             ;; _fix_me_
             ;; what was i thinking?  in the face of one or more derivational
             ;; rules plus an inflectional rule, this list can be of arbitrary
             ;; length, of course.
             ;;
             #+:null
             (null (rest (mrs::found-lex-rule-list (first lexemes))))
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
      for entry = (aref *chart* i 0)
      thereis
        (and (chart-entry-p entry)
             (loop
                 for configuration in (chart-entry-configurations entry)
                 for candidate = (chart-configuration-edge configuration)
                 when (or (and edge (eq edge candidate))
                          (and id (eq id (edge-id candidate))))
                 return configuration))))

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

(defun parse-tsdb-count-lrules-edges-morphs ()
   (let ((distinct-parse-edges nil)
         (successful-lrule-applications 0))
      (dolist (p *parse-record*)
         (setq distinct-parse-edges (parse-tsdb-distinct-edges p distinct-parse-edges)))
      (dotimes (vertex (- *chart-limit* 1))
         (when (aref *chart* (+ 1 vertex) 0)
           (dolist (config (chart-entry-configurations 
                            (aref *chart* (+ 1 vertex) 0)))
             (when (lexical-rule-p (edge-rule (chart-configuration-edge config)))
                (incf successful-lrule-applications)))))
      (values successful-lrule-applications (length distinct-parse-edges)
         (reduce #'+ (map 'vector
                      #'(lambda (x)
                          (if (morph-edge-p x) 
                            (length (morph-edge-morph-results x)) 
                            0))
                      *morphs*)))))

(defun summarize-chart (&key derivationp)
  (loop
      with pedges = 0
      with i-stasks = 0
      with words = 0
      with l-stasks = 0
      with derivations = nil
      for i from 0 to (- *chart-limit* 1)
      for entry = (aref *chart* i 0)
      sum (length (aref *achart* i 0)) into aedges
      when (chart-entry-p entry)
      do
        (loop
            for configuration in (chart-entry-configurations entry)
            for edge = (chart-configuration-edge configuration)
            for rule = (edge-rule edge)
            for tdfs = (edge-dag edge)
            for dag = (and tdfs (tdfs-indef tdfs))
            do 
              (when derivationp 
                (when (and dag (dag-inflected-p dag))             
                  (push (compute-derivation-tree edge) derivations)))
              (cond
               ((and (rule-p rule) (tsdb::inflectional-rule-p (rule-id rule)))
                (incf i-stasks)
                (incf words)
                (when (edge-morph-history edge)
                  (let* ((child (edge-morph-history edge))
                         (tdfs (edge-dag child))
                         (dag (and tdfs (tdfs-indef tdfs))))
                    (when (and dag (dag-inflected-p dag))
                      (incf words)))))
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
      for entry in (reverse *gen-chart*)
      sum (length (aref *achart* i 0)) into aedges
      when (chart-entry-p entry)
      do
        (loop
            for configuration in (chart-entry-configurations entry)
            for edge = (chart-configuration-edge configuration)
            for rule = (edge-rule edge)
            for tdfs = (edge-dag edge)
            for dag = (and tdfs (tdfs-indef tdfs))
            do 
              (when derivationp 
                (when (and dag (dag-inflected-p dag))             
                  (push (compute-derivation-tree edge) derivations)))
              (cond
               ((and (rule-p rule) (tsdb::inflectional-rule-p (rule-id rule)))
                (incf i-stasks)
                (incf words)
                (when (edge-morph-history edge)
                  (let* ((child (edge-morph-history edge))
                         (tdfs (edge-dag child))
                         (dag (and tdfs (tdfs-indef tdfs))))
                    (when (and dag (dag-inflected-p dag))
                      (incf words)))))
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
      (parse-tsdb-count-nodes1 (tdfs-indef (edge-dag edge)) 0)))

;;;
;;; abstract from recent changes in LKB lexicon interface (28-jan-99  -  oe)
;;;

(defun uncache-lexicon ()
  (cond 
   ((and (find-symbol "CLEAR-EXPANDED-LEX")
         (fboundp (find-symbol "CLEAR-EXPANDED-LEX")))
    (funcall (symbol-function (find-symbol "CLEAR-EXPANDED-LEX"))))
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
  (invalidate-marks)
  (invalidate-visit-marks)
  #+:pooling
  (reset-pools :compressp t)
  (case task
    (:parse
     (loop
         for i from 0 to (- *chart-limit* 1)
         for entry = (aref *chart* i 0)
         while (chart-entry-p entry)
         do
           (loop
               for configuration in (chart-entry-configurations entry)
               for edge = (chart-configuration-edge configuration)
               for odag = (edge-odag edge)
               for tdfs = (if (dag-p odag) odag (edge-dag edge))
               for dag = (tdfs-indef tdfs)
               unless (safe-dag-p dag) do (compress-dag dag))
         finally
           (loop 
               for edge in *morph-records* do
                 (compress-dag (tdfs-indef (edge-dag edge))))
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
      for tdfs = #+:restrict
                 (if *chart-packing-p* (rule-rtdfs rule) (rule-full-fs rule))
                 #-:restrict
                 (rule-full-fs rule)
      for dag = (tdfs-indef tdfs) do
        (compress-dag dag))
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
  (excl:gc))
                  
;;;
;;; interface functions for reconstruction of derivations (in UDF --- unified
;;; derivation format |:-).
;;;

(defparameter tsdb::*reconstruct-hook*
  #-:tty
  #'(lambda (edge &optional (i-input "reconstructed item"))
      (declare (ignore i-input))
      (when (edge-p edge)
        (display-parse-tree edge nil)))
  #+:tty
  nil)

(defun tsdb::find-lexical-entry (form instance &optional id start end (dagp t))

  (let* ((*package* *lkb-package*)
         (name (intern (if (stringp instance)
                         (string-upcase instance)
                         instance)
                       *lkb-package*))
         (instance (ignore-errors (get-unexpanded-psort-entry name))))
    (when instance 
      (let ((tdfs (when (smember dagp '(:word t))
                    (copy-tdfs-completely 
                     (lex-entry-full-fs (get-lex-entry-from-id name)))))
            (ids (list (lex-entry-id instance))))
        (make-edge :id id :category (and tdfs (indef-type-of-tdfs tdfs))
                   :rule form :leaves (list form) :lex-ids ids
                   :dag tdfs :from start :to end)))))

(defun tsdb::find-affix (type)
  (let* ((*package* *lkb-package*)
         (name (string-upcase (string type)))
         (name (intern name *lkb-package*))
         (rule (tsdb::find-rule name)))
    (when (rule-p rule) rule)))

(defun tsdb::find-rule (instance)
  (let* ((name (intern (if (stringp instance)
                           (string-upcase instance)
                         instance)
                       *lkb-package*))
         (rule (or (get-lex-rule-entry name)
                   (get-grammar-rule-entry name))))
    rule))

(defun tsdb::instantiate-rule (rule edges id &optional (dagp t))
  (let* ((dagp (smember dagp '(:rule t)))
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
              (setf result (uday result tdfs path))
            finally
              (setf result (and result (restrict-and-copy-tdfs result))))))
    (if (or result (null dagp))
      (make-edge :id id :category (and result (indef-type-of-tdfs result))
                 :rule rule
                 :leaves (loop 
                             for edge in edges
                             append (edge-leaves edge))
                 :lex-ids (loop 
                             for edge in edges
                             append (edge-lex-ids edge))
                 :dag result
                 :children edges
                 :from (edge-from (first edges)) 
                 :to (edge-to (first (last edges))))
      (values status %failure%))))

(defun tsdb::instantiate-preterminal (preterminal mrule 
                                &optional id start end (dagp t))
  ;;
  ;; _fix_me_
  ;; this hardwires some assumptions about how affixation is carried out. 
  ;;                                                        (22-apr-99  -  oe)
  ;;
  (with-unification-context (foo)
    (let* ((dagp (smember dagp '(:irule :word t)))
           (*unify-debug* :return)
           (%failure% nil)
           (rtdfs (when dagp 
                    #+:restrict
                    (if *chart-packing-p* 
                      (rule-rtdfs mrule) 
                      (rule-full-fs mrule))
                    #-:restrict
                    (rule-full-fs mrule)))
           (tdfs (when dagp (edge-dag preterminal)))
           (result (when (and rtdfs tdfs)
                     (uday rtdfs tdfs '(args first))))
           (copy (and result (restrict-and-copy-tdfs result))))
      (if (or copy (null dagp))
        (make-edge :id id :category (and copy (indef-type-of-tdfs copy))
                   :rule mrule 
                   :leaves (copy-list (edge-leaves preterminal))
                   :lex-ids (copy-list (edge-lex-ids preterminal))
                   :from start :to end
                   :dag copy
                   :children (list preterminal))
        (values nil %failure%)))))

;;;
;;; _fix_me_
;;; obsolete this function (fix in `redwoods.lisp').          (3-nov-03; oe)
;;;
(defun tsdb::reconstruct-mrs (id mrs length)
  (make-edge :id id :mrs mrs :from 0 :to length))
