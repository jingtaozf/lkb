;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

(in-package "TSDB")

;;;
;;; hack around recent change in PAGE shell: because of a name conflict with
;;; Allegro CL 5.0 upwards, the `stream' component in PAGE modules has been
;;; renamed to `modstream' --- until everybody can upgrade to a recent PAGE
;;; version, install hacky kludge.                      (11-nov-98  -  oe)
;;;
(defmacro mstream (module)
  (if (find-symbol "MODSTREAM" "MAIN")
    (list (find-symbol "MODSTREAM" "MAIN") module)
    `(stream ,module)))

(defparameter
  *tsdb-morphology-protocol*
  '((main::*scanner* main::*morphology* :stop-bw)
    ((main::*morphology* :parse) main::*lexicon* main::*scanner*)
    (main::*lexicon* main::*standalone* :stop-bw)))

(defparameter
  *tsdb-parser-protocol*
  '((main::*scanner* main::*morphology* :stop-bw)
    ((main::*morphology* :parse) main::*lexicon* main::*scanner*)
    (main::*lexicon* main::*parser* main::*morphology*)
    (main::*parser* main::*standalone* :stop-bw)))

(defparameter *page-controller* main::*controller*)    

(defun run-protocol (protocol input &key (trace nil))
  (let* ((foo (open "/dev/null" :direction :output :if-exists :overwrite))
         (shell (mstream main::*page-shell*))
         (controller (mstream *page-controller*))
         (blinker (mstream main::*blinker*))
         streams)
    (unless trace
      (setf (mstream main::*page-shell*) foo)
      (setf (mstream *page-controller*) foo)
      (setf (mstream main::*blinker*) foo)
      (dolist (component (main::internal-protocol-rep protocol))
        (push (mstream (main::p-compo (main::p-caller component))) streams)
        (setf (mstream (main::p-compo (main::p-caller component))) foo)))
    (multiple-value-bind (result condition)
        (ignore-errors
         (catch 'main::stop
           (main::eval-proto *page-controller* protocol input)))
      (close foo)
      (unless trace
        (setf (mstream main::*page-shell*) shell)
        (setf (mstream *page-controller*) controller)
        (setf (mstream main::*blinker*) blinker)
        (do ((protocol (main::internal-protocol-rep protocol)
                       (rest protocol))
             (streams streams (rest streams)))
            ((null protocol))
          (setf (mstream (main::p-compo (main::p-caller (first protocol))))
            (first streams))))
      (values result condition))))

(defun parse-word (word &key load trace)
  
  (setf (main::output-stream main::*lexicon*) nil)
  (let ((tdl::*verbose-reader-p* 
         (case load
           ((:warn :quiet :collect nil :fair :modest) nil)
           ((:full :all :verbose t) t)))
        (tdl::*verbose-definition-p*
         (case load
           ((:warn :quiet :collect nil) nil)
           ((:fair :modest :full :all :verbose t) t)))
        (main::*draw-chart-p* nil))
    
    (multiple-value-bind (result condition)
        (run-protocol *tsdb-morphology-protocol* word :trace trace)
      (declare (ignore result))
      (when (equal load :collect)
        (let* ((entries (unless condition 
                          (main::output-stream main::*lexicon*))))
          (map nil #'(lambda (entry)
                       (when (main::typed-item-p entry)
                         (push (second (main::typed-item-args entry))
                               %tsdb-lexical-preterminals%)))
               entries)))
      (pg::summarize-lexicon))))

(defun extract-inflections (&optional (items %tsdb-lexical-preterminals%))
  (loop
      with result = nil
      for item in items
      for preterminal = (pg::combo-item-index item)
      for name = (when (symbolp preterminal)
                   (string-downcase (string preterminal)))
      finally (return result)
      when (eq (pg::combo-item-itype item) :lex-entry)
      do
        (let* ((daughters (pg::item-daughters item))
               (daughter (when (= (length daughters) 1)
                           (aref daughters 0)))
               (itype (and daughter (pg::combo-item-itype daughter)))
               (index (and daughter (pg::combo-item-index daughter))))
          (when (and name daughter itype index (eq itype :morph))
            (pushnew (list name index) result :test #'equal)))))

(defun initialize-test-run (&key interactive)
  (declare (special pg::*maximal-number-of-edges*))
  
  (let* ((storage (gensym ""))
         (parser (pg::get-parser :syntax))
         (environment 
          (pairlis '(tdl::*verbose-reader-p*
                     tdl::*verbose-definition-p*
                     main::*draw-chart-p*
                     pg::stat-rules
                     pg::ebl-parser-name-rule-fn
                     pg::ebl-parser-external-signal-fn
                     pg::*maximal-number-of-edges*)
                   (list tdl::*verbose-reader-p*
                         tdl::*verbose-definition-p*
                         main::*draw-chart-p*
                         (and parser
                           (pg::parser-stat-rules parser))
                         (and parser 
                           (pg::ebl-parser-name-rule-fn parser))
                         (and parser 
                           (pg::ebl-parser-external-signal-fn parser))
                         pg::*maximal-number-of-edges*))))

    (unless interactive
      (setf main::*draw-chart-p* nil)
      (setf tdl::*verbose-reader-p* nil)
      (setf tdl::*verbose-definition-p* nil)

      (when parser
        (setf (pg::ebl-parser-name-rule-fn (pg::get-parser :syntax))
          #'get-informative-item-label)
        (setf (pg::parser-stat-rules parser) *tsdb-rule-statistics-p*)))
    (scanning::init-scanner)
    (setf (get :environment storage) environment)
    storage))

(defun finalize-test-run (storage)
  (let ((parser (pg::get-parser :syntax))
        (environment (get :environment storage)))
    (dolist (pair environment)
      (case (first pair)
        (pg::parser-stat-rules
         (when parser
           (setf (pg::parser-stat-rules parser) (rest pair))))
        (pg::ebl-parser-name-rule-fn 
         (when parser
           (setf (pg::ebl-parser-name-rule-fn parser) (rest pair))))
        (pg::ebl-parser-external-signal-fn
         (when parser
           (setf (pg::ebl-parser-external-signal-fn parser) (rest pair))))
        (t
         (set (first pair) (rest pair)))))))

(defun parse-item (string &key exhaustive
                               (edges 0) trace derivations
                               semantix-hook trees-hook
                               burst)
  (declare (special pg::*maximal-number-of-edges*))

  (let* ((string (remove-and-insert-punctuation string))
         (tracep (main::trace-p main::*parser*))
         (main::*exhaustive* exhaustive)
         (lexicon-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon)))
         (syntax-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :syntax)))
         result tcpu tgc treal conses symbols others)

    (setf (main::trace-p main::*parser*) trace)
    (setf pg::*maximal-number-of-edges*
      (if (and (integerp edges) (> edges 0)) edges nil))
    (setf pg::*edge-id-counter* 0)
    (setf (pg::ebl-parser-external-signal-fn (pg::get-parser :syntax))
      (if (> edges 0)
        #'pg::maximal-number-of-edges-exceeded-p
        #'(lambda (parser) (declare (ignore parser)))))
    (setf pg::*edge-id-counter* 0)
    (udine::reset-costs)
    (setf (main::output-stream main::*lexicon*) nil)
    (setf (main::output-stream main::*parser*) nil)
    (when (and derivations *tsdb-lexical-oracle-p*)
      (install-lexical-oracle derivations))
    (when (and derivations *tsdb-phrasal-oracle-p*)
      (install-phrasal-oracle derivations))

    (multiple-value-bind (return condition)
        (excl::time-a-funcall #'(lambda ()
                                  (run-protocol *tsdb-parser-protocol* 
                                                string :trace trace))
                              #'(lambda (tgcu tgcs tu ts tr
                                         scons ssym sother 
                                         #+(and :allegro-version>= 
                                                (version>= 5 0))
                                         sstatic)
                                  #+(and :allegro-version>= (version>= 5 0))
                                  (declare (ignore sstatic))
                                  (setf tgc (+ tgcu tgcs)
                                        tcpu (+ tu ts)
                                        treal tr
                                        conses scons
                                        symbols ssym
                                        others sother)))
      
      (setf (main::trace-p main::*parser*) tracep)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon))
        lexicon-task-priority-fn)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :syntax))
        syntax-task-priority-fn)
      
      (let* ((unifications 
              (udine::unify-costs-unify udine::*unification-costs*))
             (copies 
              (udine::unify-costs-copy udine::*unification-costs*)))
        (push (cons :unifications unifications) result)
        (push (cons :copies copies) result)
        (let ((timeup (or (pg::maximal-number-of-edges-exceeded-p 
                           (pg::get-parser :syntax))
                          (pg::maximal-number-of-tasks-exceeded-p 
                           (pg::get-parser :syntax)))))
          (when timeup
            (push (cons :timeup timeup) result))))

      (append
       result
       (pairlis '(:tgc :tcpu :treal :conses :symbols :others)
                (list tgc tcpu treal conses symbols others))
       (pg::summarize-lexicon)
       (cond
        (condition
         (pairlis
          '(:readings :condition :error)
          (list -1 
                (unless burst condition) 
                (normalize-string (format nil "~a" condition)))))
        ((or (eq (first return) :incomplete-input)
             (null (main::output-stream main::*lexicon*)))
         (pairlis
          '(:readings :error)
          (list -1 "null parser input")))
        (t
         (let* ((items (main::output-stream main::*parser*))
                (readings (length items))
                (statistics 
                 (pg::parser-stats-readings (pg::get-parser :syntax)))
                (first (when statistics (first (last statistics))))
                (first (when first (pg::stats-time first)))
                (first (when first 
                         (* (/ first internal-time-units-per-second) 1000)))
                (global (pg::parser-global-stats (pg::get-parser :syntax)))
                (total (when global (pg::stats-time global)))
                (total (when total 
                         (* (/ total internal-time-units-per-second) 1000)))
                (ftasks (when global (pg::stats-filtered global)))
                (etasks (when global (pg::stats-executed global)))
                (stasks (when global (pg::stats-successful global)))
                (ctasks (if (and ftasks etasks) (+ ftasks etasks) -1))
                redges results)
           (when (= readings (length statistics))
             (do* ((i 0 (+ i 1))
                   (statistics statistics (rest statistics))
                   (statistic (first statistics) (first statistics))
                   (items items (rest items))
                   (item (first items) (first items)))
                 ((null statistics))
               (let* ((item (typecase item
                              (cons (seventh item))
                              (main::typed-item 
                               (second (main::typed-item-args item)))
                              (t item)))
                      (summary (pg::summarize-result item))
                      (r-aedges (get-field :aedges summary))
                      (r-pedges (get-field :pedges summary))
                      (tree (when trees-hook
                              (ignore-errors (funcall trees-hook item))))
                      (mrs (when semantix-hook
                             (ignore-errors (funcall semantix-hook item))))
                      (size (fs-size item))
                      (time (pg::stats-time statistic))
                      (time (when time 
                              (/ time internal-time-units-per-second)))
                      (ftasks (pg::stats-filtered statistic))
                      (etasks (pg::stats-executed statistic))
                      (stasks (pg::stats-successful statistic))
                      (ctasks (if (and ftasks etasks) (+ ftasks etasks) -1))
                      (derivation 
                       (format 
                        nil 
                        "~s"
                        (pg::item-to-node item (pg::get-parser :syntax)))))
                 (dolist (item (get-field :redges summary))
                   (pushnew item redges))
                 (push (pairlis
                        '(:result-id :time
                          :r-ctasks :r-ftasks :r-etasks :r-stasks
                          :size
                          :r-adges :r-pedges
                          :derivation :tree :mrs)
                        (list i time
                              ctasks ftasks etasks stasks
                              size
                              r-aedges r-pedges
                              derivation tree mrs))
                       results))))
           (let* ((rpedges (length redges))
                  (arities 
                   (map 'list 
                     #'(lambda (item) 
                         (when (pg::item-daughters item)
                           (1- (array-dimension (pg::item-daughters item) 0))))
                     redges))
                  (raedges 
                   (if (pg::parser-appl-rules-bup-fn (pg::get-parser :syntax))
                     (apply #'+ arities)
                     -1)))
             (append (pg::summarize-chart)
                     (pairlis
                      '(:readings :first :total
                        :p-ctasks :p-ftasks :p-etasks :p-stasks
                        :raedges :rpedges 
                        :results)
                      (list readings first total
                            ctasks ftasks etasks stasks
                            raedges rpedges
                            results)))))))))))

(defun get-test-run-information ()
  (let* ((application (if (boundp 'make::*page-version*)
                        (format nil "PAGE (~a)" make::*page-version*)
                        "PAGE"))
         (grammar (current-grammar))
         (avms (tdl::get-global :avms))
         (avms (when (hash-table-p avms) (hash-table-count avms)))
         (sorts (tdl::get-global :sorts))
         (sorts (when (hash-table-p sorts) (hash-table-count sorts)))
         (templates (tdl::get-global :templates))
         (templates
          (when (hash-table-p templates) (hash-table-count templates)))
         (lexicon (pg::combo-parser-lexicon (pg::get-parser :lexicon)))
         (lexicon (when (pg::lexicon-p lexicon) 
                    (pg::lexicon-lex-entries lexicon)))
         (lexicon (when (hash-table-p lexicon) (hash-table-count lexicon)))
         (lrules 
          (length (pg::combo-parser-lex-rules (pg::get-parser :lexicon))))
         (rules 
          (length (pg::combo-parser-syn-rules (pg::get-parser :syntax)))))
    (append (and avms (list (cons :avms avms)))
            (and sorts (list (cons :sorts sorts)))
            (and templates (list (cons :templates templates)))
            (and lexicon (list (cons :lexicon lexicon)))
            (pairlis '(:application :grammar
                       :lrules :rules)
                     (list application grammar
                           lrules rules)))))

(defmacro get-item-type (item)
  `(get-fs-type (lex::cfs-fs (pg::combo-item-cfs ,item))))

(defgeneric get-fs-type (fs))
(defmethod get-fs-type ((fs unify::node))
  (tdl::value-type (unify::get-type fs)))
(defmethod get-fs-type ((fs csli-unify::fs))
  (csli-unify::fs-type fs))

(defgeneric fs-size (fs))
(defmethod fs-size ((item pg::combo-item)) (fs-size (pg::combo-item-cfs item)))
(defmethod fs-size ((cfs lex::cfs)) (fs-size (lex::cfs-fs cfs)))
(defmethod fs-size ((fs csli-unify::fs))
  (cond ((csli-unify::tdl-atomic fs) 1)
        (t (+ 1 (loop
                    for arc in (csli-unify::fs-arcs fs)
                    sum (fs-size (rest arc)))))))
(defmethod fs-size ((fs t)) -1)

(defun get-informative-item-label (item)
  (when (pg::combo-item-p item)
    (case (pg::combo-item-itype item)
      ((:lex-entry :c-lex-entry)
       (if nil
         (let* ((spare (pg::combo-item-spare item))
                (spare (when (and spare (or (symbolp spare)
                                            (stringp spare)))
                         (string spare)))
                (cfs (pg::combo-item-cfs item))
                (fs (and cfs (pg::cfs-fs cfs))))
           (and fs (string-downcase 
                    (or spare (format nil "~a" (get-fs-type fs))))))
         (format nil "~(~a~)" (pg::combo-item-index item))))
      (t (format nil "~(~a~)" (pg::item-label item))))))

(defun remove-terminals (derivation)
  (let ((daughters (pg::pnode-daughters derivation)))
    (if daughters
      (append (list (pg::pnode-name derivation) 
                    (pg::pnode-start derivation)
                    (pg::pnode-end derivation))
              (remove nil (map 'list #'remove-terminals daughters)))
      nil)))
       
(defun extract-preterminals (derivation &optional (offset 0))
  (let ((daughters (pg::pnode-daughters derivation)))
    (if daughters
      (let* ((offset (+ offset (pg::pnode-start derivation))))
        (mapcan #'(lambda (daughter)
                    (extract-preterminals daughter offset))
                daughters))
      (list (list (pg::pnode-name derivation) 
                  (+ offset (pg::pnode-start derivation))
                  (+ offset (pg::pnode-end derivation)))))))

(defparameter *lexical-oracle* nil)

(defun install-lexical-oracle (derivations)
  (setf *lexical-oracle* nil)
  (when derivations
    (let* ((derivations (map 'list #'remove-terminals derivations))
           (preterminals (mapcan #'extract-preterminals derivations)))
      (setf *lexical-oracle* 
        (make-array (+ 1 (apply #'max (map 'list 
                                        #'pg::pnode-start preterminals)))))
      (map nil 
        #'(lambda (preterminal) 
            (push (intern (string-upcase (pg::pnode-name preterminal)) 
                          lex::*lex-package*)
                  (aref *lexical-oracle* (pg::pnode-start preterminal))))
        preterminals))
    (setf (pg::parser-task-priority-fn (pg::get-parser :lexicon))
      #'lexical-priority-oracle)))

(defun lexical-priority-oracle (rule daughter tasktype parser)
  (declare (ignore parser))
  #+:odebug
  (format t "~a: ~a [~a ~a] # ~a [~a ~a]~%" 
          tasktype 
          (when rule (get-item-type rule))
          (when rule (pg::item-start rule))
          (when rule (pg::item-end rule))
          (when daughter (get-item-type daughter))
          (when daughter (pg::item-start daughter))
          (when daughter (pg::item-end daughter)))
  (if (and *lexical-oracle*
           rule daughter
           (eq tasktype :cf-rule)
           (eq (pg::combo-item-itype rule) :lex-entry)
           (eq (pg::combo-item-itype daughter) :morph))
    (let ((start (pg::item-start daughter))
          (type (get-item-type rule)))
      (when (member type (aref *lexical-oracle* start)) 600))
    600))

;;;
;;; this is rather awkward because most of the code resides in the :filter
;;; package; maybe, it should eventually become part of core PAGE and assumed
;;; to be there |:-{.                                     (15-dec-97  -  oe)
;;;
(defun install-phrasal-oracle (derivations)
  (when (and (find-package "FILTER")
             (boundp (intern "*UNIVERSE*" "FILTER")))
    (set (intern "*LOCAL-TREES*" "FILTER") nil)
    (funcall (intern "COLLECT-LOCAL-TREES" "FILTER") derivations)
    (funcall (intern "COMPUTE-DERIVATION-ORACLE" "FILTER"))
    (setf (pg::parser-task-priority-fn (pg::get-parser :syntax))
      (symbol-function (intern "PHRASAL-PRIORITY-ORACLE" "FILTER")))))


(in-package "MAIN")


;;;
;;; register tsdb(1) as a module to the PAGE shell; the :close-fn value will
;;; be called on exit from the PAGE shell and image creation using dump().
;;;

(defclass tsdb (component) ())

(defparameter *tsdb*
  (make-instance 'tsdb 
    :name "tsdb(1)" :gui-name "tsdb(1)"
    :close-fn #'tsdb::shutdown-podium))

(define-command
  '(:tsdb
    &nec command &optional argument &key condition run skeleton load
    &doc 
    "Interact with tsdb(1) and profiling module; `tsdb :help' for details.")
  #'tsdb:tsdb)

(in-package "PARSING")

;;;
;;; this is code that started out in the CSLI `tuneup.lisp' a while or two
;;; ago (that night when people had had a bottle of champagne while admiring
;;; the half moon bay sunset |:-); since it is truly tsdb(1) specific, it seems
;;; plausible to move it here.                           (31-oct-97  -  oe)
;;;

(defun summarize-chart (&optional (parser (pg::get-parser :syntax)))
  (let* ((chart (parser-chart parser))
         (passive-items-starting-at (chart-passive-items-starting-at chart))
         (pedges (when passive-items-starting-at
                   (loop 
                       for foo across  passive-items-starting-at
                       sum (length foo))))
         (active-items-starting-at 
          (when chart (chart-active-items-starting-at chart)))
         (active-items-ending-at 
          (when chart (chart-active-items-ending-at chart)))
         (aedges 0))
    (when (and active-items-starting-at active-items-ending-at)
      (let ((n (array-dimension active-items-starting-at 0)))
        (dotimes (i n)
          (incf aedges (length (aref active-items-starting-at i)))))
      (let ((n (array-dimension active-items-ending-at 0)))
        (dotimes (i n)
          (incf aedges (length (aref active-items-ending-at i))))))
    (pairlis '(:aedges :pedges)
             (list aedges pedges))))

(defun summarize-result (item 
                         &optional (chart (parser-chart (get-parser :syntax))))

  (let* ((daughters (item-daughters item))
         (ndaughters 
          (if daughters (array-dimension (item-daughters item) 0) 0))
         (pedges 1)
         (aedges (if daughters (- ndaughters 1) 0))
         (redges (list item)))
    (when daughters
      (loop
          for i from 0 to (- ndaughters 1)
          for daughter = (aref daughters i)
          when (and daughter (not (eq (combo-item-itype daughter) :morph)))
          do
            (let ((summary (summarize-result daughter chart)))
              (incf pedges (tsdb::get-field :pedges summary))
              (incf aedges (tsdb::get-field :aedges summary))
              (nconc redges (tsdb::get-field :redges summary)))))
    (pairlis '(:pedges :aedges :redges)
             (list pedges 
                   (if (parser-appl-rules-bup-fn (get-parser :syntax)) 
                     aedges
                     -1)
                   redges))))

(defun summarize-rules ()
  (let* ((parser (get-parser :syntax))
         (chart (parser-chart parser))
         (pedges (chart-passive-items-starting-at chart))
         (saedges (chart-active-items-starting-at chart))
         (eaedges (chart-active-items-ending-at chart))
         (rules (combo-parser-syn-rules parser))
         (statistics (parser-rule-stats parser))
         (counts (make-array (combo-parser-rule-number parser))))
    (when statistics
      (flet ((count (edge type)
               (when (eq (combo-item-itype edge) :rule)
                 (let* ((key (combo-item-key edge)))
                   (incf (tsdb::get-field type (aref counts key)))))))
        (loop
            for rule in rules
            for name = (string-downcase (string (combo-item-index rule)))
            for key = (combo-item-key rule)
            for stats = (aref statistics key)
            for filtered = (stats-filtered stats)
            for executed = (stats-executed stats)
            for successful = (stats-successful stats)
            do
              (setf (aref counts key)
                (pairlis '(:rule 
                           :filtered :executed :successful 
                           :actives :passives)
                         (list name
                               filtered executed successful 
                               0 0))))
        (loop for edges across pedges
            do (loop for edge in edges
                   do (count edge :passives)))
        (loop for edges across saedges
            do (loop for edge in edges
                   do (count edge :actives)))
        (loop for edges across eaedges
            do (loop for edge in edges
                   do (count edge :actives)))
        (loop for rule across counts
            when rule collect rule)))))

(defun summarize-lexicon ()
  (let* ((entries (main::output-stream main::*lexicon*))
         (entries (remove "LE-L_STR" entries
                          :key #'main::typed-item-form
                          :test #'equal))
         (entries (remove "LE-R_STR" entries
                          :key #'main::typed-item-form
                          :test #'equal))
         (words (+ (count :lex-entry entries
                          :key #'(lambda (titem)
                                   (pg::combo-item-itype 
                                    (second (main::typed-item-args titem)))))
                   (count :c-lex-entry entries
                          :key #'(lambda (titem)
                                   (pg::combo-item-itype 
                                    (second (main::typed-item-args titem)))))))
         (lstasks (count :lex-rule entries
                         :key #'(lambda (titem)
                                  (pg::combo-item-itype 
                                   (second (main::typed-item-args titem)))))))
    (pairlis '(:words :l-stasks) (list words lstasks))))


;;;
;;; maximal-number-of-tasks-exceeded-p() is installed as external-signal-fn()
;;; in the syntax parser (because the :taskslice mechanism) is not readily
;;; accessible to us)
;;;

;;;
;;; for LKB comparability we want to restrict the number of edges rather than
;;; attempts to build edges (tasks).                   (24-sep-98  -  oe)
;;;
(defparameter *maximal-number-of-edges* 0)
(defparameter *maximal-number-of-edges-exceeded-p* nil)

(defun maximal-number-of-edges-exceeded-p (parser)
  (declare (ignore parser))
  (and *maximal-number-of-edges*
       (when (>= *edge-id-counter* *maximal-number-of-edges*)
         (format nil "edge limit (~a)" *edge-id-counter*))))

(defun maximal-number-of-tasks-exceeded-p (parser)
  (when (eq (first (main::add-keys main::*parser*)) :taskslice)
    (let* ((limit (second (main::add-keys main::*parser*)))
           (statistics (parser-global-stats parser))
           (executed (and statistics (stats-executed statistics))))
      (when (and (integerp limit) (>= executed limit))
        (format nil "task limit (~a)" executed)))))