;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;;
;;; while preparing for the sep-99 release of the LKB we need to do some magic 
;;; to support the pre-release and current development versions in the same
;;; source file.  searching for find-symbol() and friends should be a good way
;;; to find the all-too-many kludges that can be eliminated once the release
;;; has been made public ... sigh.                         (22-aug-99  -  oe)
;;;

#+:LKB-V5.3
(in-package :lkb)
#-:LKB-V5.3
(in-package :cl-user)

(defparameter *lkb-package* 
  (or (find-package :lkb) (find-package :common-lisp-user)))
  
;;;
;;; another instance of versioning kludges: yadu!() only comes in with the new
;;; active parser in the current development version       (26-aug-99  -  oe)
;;;
(defmacro uday (tdfs1 tdfs2 path)
  `(yadu ,tdfs1 (create-temp-parsing-tdfs ,tdfs2 ,path)))


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
         (chart-packing-p #+:packing *chart-packing-p* #-:packing nil)
         (agendap (or (and (not active-parsing-p) (not exhaustivep))
                      (and active-parsing-p (not exhaustivep) 
                           (find :agenda *features*))))
         (npaths (length *check-paths-optimised*)))
    `((:avms . ,(- (hash-table-count *types*) (length *templates*)))
      (:sorts . 0)
      (:templates . ,(length *templates*))
      (:rules . ,(hash-table-count *rules*))
      (:lrules . ,(hash-table-count *lexical-rules*))
      (:lexicon . ,(size-of-lexicon))
      (:grammar . 
       ,(or (symbol-value (find-symbol "*GRAMMAR-VERSION*" *lkb-package*))
            (symbol-value (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))
            "unknown"))
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
                       


(defun parse-word (word &key load trace)
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
          (str (make-string-output-stream)) ; capture any warning messages
          (*standard-output* (if trace
                                 (make-broadcast-stream *standard-output* str)
                               str))
          (input (split-into-words (preprocess-sentence-string word))))
     (parse input nil)
     (summarize-chart))))

(defun initialize-test-run (&key interactive exhaustive)
  (declare (ignore interactive))
   ;; returns whatever it likes; the return value will be given to
   ;; finalize-test-run() to restore the interactive environment if
   ;; necessary
  (let ((*package* *lkb-package*)
        (first-only-p *first-only-p*))
    (clear-type-cache)
    (setf *first-only-p* (unless exhaustive 
                           (if (integerp *first-only-p*) *first-only-p* 1)))
    (pairlis '(:first-only-p)
             (list first-only-p))))

(defun finalize-test-run (environment)
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
        for (variable . value) in environment do
          (case variable
            (:first-only-p 
             (setf *first-only-p* value))))
    (pairlis '(:lexicon) (list lexicon))))


;;; sets the processor into exhaustive mode if requested; parses
;;; .string. without producing any printout (unless .trace. is set);
;;; funcall()s .semantix-hook. and .trees-hook. to obtain MRS and tree
;;; representations (strings); all times in thousands of secs

(defun parse-item (string 
                   &key id exhaustive trace
                        readings edges derivations semantix-hook trees-hook
                        burst derivationp)
  (declare (ignore derivations #-:packing id))
  
  (multiple-value-bind (return condition)
      (ignore-errors
       (let* ((*package* *lkb-package*)
              (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                           *maximum-number-of-edges*
                                           edges))
              (*first-only-p* (unless exhaustive
                                (if (integerp readings) readings 1)))
              (*do-something-with-parse* nil)
              (sent
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
         #+:packing
         (reset-packings)
         (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks)
             #+allegro
             (excl::time-a-funcall
              #'(lambda () (parse-tsdb-sentence sent trace))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setq tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                       conses (* scons 8) symbols (* ssym 24) others sother)))
             #-allegro
             (multiple-value-prog1
                 (progn
                   (setq treal (get-internal-real-time)
                         tcpu (get-internal-run-time)
                         tgc #+mcl (ccl:gctime) #-mcl 0
                         others #+mcl (ccl::total-bytes-allocated) #-mcl 0)
                   (parse-tsdb-sentence sent trace))
               (let ((rawgc (- #+mcl (ccl:gctime) tgc)))
                 (setq symbols 0 conses 0
                       others
                       (- #+mcl (- (ccl::total-bytes-allocated) 24) #-mcl -1 
                          others)
                       tcpu
                       (round (* (- (get-internal-run-time) tcpu) 1000)
                              internal-time-units-per-second)
                       treal
                       (round (* (- (get-internal-real-time) treal) 1000)
                              internal-time-units-per-second)
                       tgc #+mcl (round (* rawgc 1000)
                                        internal-time-units-per-second)
                       #-mcl -1)))
          (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                 (output (get-output-stream-string str))
                 (unifications *unifications*)
                 (copies *copies*)
                 #+:packing
                 (packingp *chart-packing-p*)
                 #+:packing
                 utcpu
                 #+:packing
                 utgc
                 #+:packing
                 uspace
                 (readings #+:packing
                           (if packingp
                             (excl::time-a-funcall
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
                             (length *parse-record*))
                           #-:packing
                           (length *parse-record*))
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
                 #+:packing
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
                 (summary (summarize-chart :derivationp derivationp)))
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
                (:error . ,output)
                (:comment . ,comment)
                (:results .
                 ,(append
                   (unless #+:packing packingp #-:packing nil
                     (loop
                         with *package* = *lkb-package*
                         for i from 0
                         for parse in (reverse *parse-record*)
                         for time = (if (integerp (first times))
                                      (round (* (- (pop times) start) 1000)
                                             internal-time-units-per-second )
                                      total)
                         for derivation = (write-to-string
                                           (compute-derivation-tree parse)
                                           :escape t :case :downcase)
                         for r-redges = (length 
                                         (parse-tsdb-distinct-edges parse nil))
                         for size = (parse-tsdb-count-nodes parse)
                         for tree = (tsdb::call-hook trees-hook parse)
                         for mrs = (tsdb::call-hook semantix-hook parse)
                         collect
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
                   (when derivationp
                     (loop
                         for i from (length *parse-record*)
                         for derivation in (rest (assoc :derivations summary))
                         for string = (write-to-string derivation
                                       :escape t :case :downcase)
                         collect (pairlis '(:result-id :derivation)
                                          (list i string))))))))))))
    (unless trace (release-temporary-storage))
    (append
     (when condition
       (pairlis '(:readings 
                  :condition 
                  :error
                  :timeup)
                (list -1 
                      (unless burst condition)
                      (format nil "~a" condition)
                      (when (> *edge-id* *maximum-number-of-edges*)
                        (format nil "edge limit (~a)" *edge-id*)))))
     return)))



(defun compute-derivation-tree (edge)
  (flet ((edge-label (edge)
           (intern 
            (typecase (edge-rule edge)
              (string (string-upcase (edge-rule edge)))
              (symbol (edge-rule edge))
              (rule (rule-id (edge-rule edge)))
              (t :unknown))
            *lkb-package*)))
    (let* ((configuration (and (null (edge-children edge))
                               (find-chart-configuration :edge edge)))
           (start (and configuration 
                       (chart-configuration-begin configuration)))
           (end (and configuration 
                       (chart-configuration-end configuration))))
      (cond
       ((and (edge-morph-history edge) (edge-spelling-change edge))
        (let* ((daughter (edge-morph-history edge))
               (preterminal (first (edge-lex-ids daughter))))
          (list (edge-label edge)
                start end
                (list preterminal
                      start end
                      (list (edge-rule daughter)
                            start end)))))
       ((null (edge-children edge))
        (list (first (edge-lex-ids edge))
              start end
              (list (edge-rule edge) 0 1)))
       (t
        (let* ((start *chart-limit*)
               (end 0)
               (children
                (loop
                    for child in (edge-children edge)
                    for derivation = (compute-derivation-tree child)
                    do
                      (setf start (min start (second derivation)))
                      (setf end (max end (third derivation)))
                    collect derivation)))
          (nconc (list (edge-label edge) start end)
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
     ((and (find-symbol "*PSORTS*") (boundp (find-symbol "*PSORTS*")))
      (hash-table-count (symbol-value (find-symbol "*PSORTS*"))))
     ((and (find-symbol "*LEXICON*") (boundp (find-symbol "*LEXICON*")))
      (length (collect-expanded-lex-ids 
               (symbol-value (find-symbol "*LEXICON*")))))
     (t
      -1))))
    

;;;
;;; make an attempt to release references to temporary objects; because the
;;; Lisp gc() mechanism cannot interpret the generation counter mechanism, we
;;; need to explicitly reset all scratch slots.
;;;

(defun release-temporary-storage ()
  (invalidate-marks)
  (invalidate-visit-marks)
  #+:pooling
  (reset-pools :compressp t)
  (loop
      for i from 0 to (- *chart-limit* 1)
      for entry = (aref *chart* i 0)
      when (chart-entry-p entry)
      do
        (loop
            for configuration in (chart-entry-configurations entry)
            for edge = (chart-configuration-edge configuration)
            for tdfs = #+:packing 
                       (or (edge-odag edge) (edge-dag edge))
                       #-:packing
                       (edge-dag edge)
            for dag = (tdfs-indef tdfs)
            unless (safe-dag-p dag) do
              (compress-dag dag)))
  (loop 
      for edge in *morph-records* do
        (compress-dag (tdfs-indef (edge-dag edge))))
  (clear-chart)
  (when (and (fboundp (find-symbol "CLEAR-ACHART" *lkb-package*))
             (let ((foo (find-symbol "*ACTIVE-PARSING-P*" *lkb-package*)))
               (and foo (symbol-value foo))))
    (funcall (fboundp (find-symbol "CLEAR-ACHART" *lkb-package*)))) 
  (setf *cached-category-abbs* nil)
  (setf *parse-times* nil)
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
        (when (dag-p dag) (compress-dag dag))))))
                  
;;;
;;; interface functions for reconstruction of derivations (in UDF --- unified
;;; derivation format |:-).
;;;

(defparameter *reconstruct-hook*
  #-:tty
  #'(lambda (edge &optional (i-input "reconstructed item"))
      (declare (ignore i-input))
      (when (edge-p edge)
        (display-parse-tree edge nil)))
  #+:tty
  nil)

(defun find-lexical-entry (form instance)
  (let* ((*package* *lkb-package*)
         (name (intern (if (stringp instance)
                         (string-upcase instance)
                         instance)
                       *lkb-package*))
         (instance (ignore-errors (get-psort-entry name))))
    (when instance 
      (let ((tdfs (copy-tdfs-completely (lex-or-psort-full-fs instance)))
            (id (lex-or-psort-sense-id instance)))
        (make-edge :id 0 :category (indef-type-of-tdfs tdfs)
                   :rule form :leaves (list form) :lex-ids (list id)
                   :dag tdfs)))))

(defun find-affix (type)
  (let* ((*package* *lkb-package*)
         (name (string-upcase (string type)))
         (name (intern name *lkb-package*))
         (rule (find-rule name)))
    (when (rule-p rule) rule)))

(defun find-rule (instance)
  (let* ((name (intern (if (stringp instance)
                           (string-upcase instance)
                         instance)
                       *lkb-package*))
         (rule (or (get-lex-rule-entry name)
                   (get-grammar-rule-entry name))))
    rule))

(defun instantiate-rule (rule edges)
  (let* ((*unify-debug* :return)
         (%failure% nil)
         (status 0)
         (result (rule-full-fs rule))
         (paths (rule-order rule)))
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
            (setf result (and result (restrict-and-copy-tdfs result)))))
    (if result
      (make-edge :id 0 :category (indef-type-of-tdfs result) :rule rule
                 :leaves (loop 
                             for edge in edges
                             append (edge-leaves edge))
                 :lex-ids (loop 
                             for edge in edges
                             append (edge-lex-ids edge))
                 :dag result
                 :children edges)
      (values status %failure%))))

(defun instantiate-preterminal (preterminal mrule)
  ;;
  ;; _fix_me_
  ;; this hardwires some assumptions about how affixation is carried out. 
  ;;                                                        (22-apr-99  -  oe)
  ;;
  (with-unification-context (foo)
    (let* ((*unify-debug* :return)
           (%failure% nil)
           (rtdfs (rule-full-fs mrule))
           (tdfs (edge-dag preterminal))
           (result 
            (uday rtdfs tdfs '(args first)))
           (copy (and result (restrict-and-copy-tdfs result))))
      (if copy
        (make-edge :id 0 :category (indef-type-of-tdfs copy) :rule mrule 
                   :leaves (copy-list (edge-leaves preterminal))
                   :lex-ids (copy-list (edge-lex-ids preterminal))
                   :dag copy
                   :children (list preterminal))
        (values nil %failure%)))))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
   (import '(get-test-run-information
            parse-word
            initialize-test-run
            finalize-test-run
            parse-item
            uncache-lexicon
            *reconstruct-hook*
            find-lexical-entry find-affix find-rule
            instantiate-rule instantiate-preterminal)
          :tsdb))
      


