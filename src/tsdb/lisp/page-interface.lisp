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
;;; it seems the slot was renamed once more ...         (23-aug-99  -  oe)
;;;
(defmacro mstream (module)
  (cond 
   ((find-symbol "MODULE-STREAM" "MAIN")
    (list (find-symbol "MODULE-STREAM" "MAIN") module))
   ((find-symbol "MODSTREAM" "MAIN")
    (list (find-symbol "MODSTREAM" "MAIN") module))
   (t `(stream ,module))))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (import
   '(pg::*reconstruct-hook*
     pg::find-lexical-entry pg::find-affix pg::find-rule
     pg::instantiate-rule pg::instantiate-preterminal)
    :tsdb))

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
         streams)
    (unless trace
      (setf (mstream main::*page-shell*) foo)
      (setf (mstream *page-controller*) foo)
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
        (loop
            for entry in (unless condition (pg::find-words))
            when (pg::combo-item-p entry)
            do (push entry %tsdb-lexical-preterminals%)))
      (pg::summarize-lexicon))))

(defun extract-inflections (&key (items %tsdb-lexical-preterminals%)
                                 pprint (stream t))
  (loop
      with result = nil
      for item in items
      for preterminal = (pg::combo-item-index item)
      for name = (when (symbolp preterminal)
                   (string-downcase (string preterminal)))
      finally 
        (if pprint
          (loop 
              for item in result
              for morph = (second item)
              do 
                (format 
                 stream 
                 "  {~(~s~), ~(~s~), ~(~s~), ~(~s~), ~d, ~d},~%"
                 (first item) (first morph) (second morph) (third morph)
                 (third item) (fourth item)))
            (return result))
      when (member (pg::combo-item-itype item) '(:c-lex-entry :lex-entry))
      do
        (let* ((daughters (pg::item-daughters item))
               (arity (length daughters))
               (rhs (loop for i from 0 to (- arity 1) collect i))
               (key (first (set-difference rhs (pg::combo-item-rhs item))))
               (daughter (when (>= (length daughters) 1)
                           (aref daughters key)))
               (itype (and daughter (pg::combo-item-itype daughter)))
               (index (and daughter (pg::combo-item-index daughter))))
          (when (and name daughter itype index (eq itype :morph))
            (pushnew (list name index key arity) result :test #'equal)))))

(defun initialize-test-run (&key interactive exhaustive)
  (declare (ignore exhaustive))
  
  (let* ((storage (gensym ""))
         (parser (pg::get-parser :syntax))
         (environment 
          (pairlis '(tdl::*verbose-reader-p*
                     tdl::*verbose-definition-p*
                     main::*draw-chart-p*
                     main::add-keys
                     pg::stat-rules
                     pg::combo-parser-external-signal-fn)
                   (list tdl::*verbose-reader-p*
                         tdl::*verbose-definition-p*
                         main::*draw-chart-p*
                         (main::add-keys main::*parser*)
                         (and parser
                           (pg::parser-stat-rules parser))
                         (and parser 
                           (pg::combo-parser-external-signal-fn parser))))))

    (unless interactive
      (setf main::*draw-chart-p* nil)
      (setf tdl::*verbose-reader-p* nil)
      (setf tdl::*verbose-definition-p* nil)

      (when parser
        (setf (pg::parser-stat-rules parser) *tsdb-rule-statistics-p*)))
    (scanning::init-scanner)
    (setf (get :environment storage) environment)
    storage))

(defun finalize-test-run (storage)
  (ignore-errors
   (let ((parser (pg::get-parser :syntax))
         (environment (get :environment storage)))
     (dolist (pair environment)
       (case (first pair)
         (main::add-keys
          (setf (main::add-keys main::*parser*) (rest pair)))
         (pg::parser-stat-rules
          (when parser
            (setf (pg::parser-stat-rules parser) (rest pair))))
         (pg::combo-parser-external-signal-fn
          (when parser
            (setf (pg::combo-parser-external-signal-fn parser) (rest pair))))
         (t
          (set (first pair) (rest pair))))))))

(defun parse-item (string &key exhaustive
                               (edges 0) readings trace derivations
                               semantix-hook trees-hook
                               derivationp
                               burst)
  (declare (ignore readings derivations derivationp))

  (let* ((string (remove-and-insert-punctuation string))
         (tracep (main::trace-p main::*parser*))
         (main::*exhaustive* exhaustive)
         (lexicon-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon)))
         (syntax-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :syntax)))
         result tcpu tgc treal conses symbols others)

    (setf (main::trace-p main::*parser*) trace)
    (when (and (integerp edges) (> edges 0))
      (setf (main::add-keys main::*parser*)
        (nconc (list :itemslice edges) (main::add-keys main::*parser*))))
    (udine::reset-costs)
    (setf (main::output-stream main::*lexicon*) nil)
    (setf (main::output-stream main::*parser*) nil)

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
                                        conses (* scons 8)
                                        symbols (* ssym 24)
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
                           (pg::get-parser :syntax))
                          (pg::time-exceeded-p (pg::get-parser :syntax)))))
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
                   (statistics (reverse statistics) (rest statistics))
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
                       (format nil "~s" (compute-derivation-tree item))))
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
  (let* ((grammar (current-grammar))
         (avms (tdl::get-global :avms))
         (avms (when (hash-table-p avms) (hash-table-count avms)))
         (sorts (tdl::get-global :sorts))
         (sorts (when (hash-table-p sorts) (hash-table-count sorts)))
         (templates (tdl::get-global :templates))
         (templates
          (when (hash-table-p templates) (hash-table-count templates)))
         (lparser (pg::get-parser :syntax))
         (parser (pg::get-parser :syntax))
         (lrules (and lparser (length (pg::combo-parser-lex-rules lparser))))
         (rules (and parser (length (pg::combo-parser-syn-rules parser))))
         (glbs (and (find-package :csli-unify)
                    (find-symbol "*GLB-TYPES*" :csli-unify)))
         (glbs (and glbs (boundp glbs) (symbol-value glbs)))
         (glbs (when (hash-table-p glbs) (hash-table-count glbs)))
         (application (format 
                       nil 
                       "PAGE (~a) [~(~a~); ~@[~d glbs~]]" 
                       make::*page-version* 
                       (symbol-name (type-of parser)) 
                       glbs)))
    (append (and avms (list (cons :avms avms)))
            (and sorts (list (cons :sorts sorts)))
            (and templates (list (cons :templates templates)))
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

;;;
;;; this is flawed: should not descend into reentrant structures twice ...
;;;
(defgeneric fs-size (fs))
(defmethod fs-size ((item pg::combo-item)) (fs-size (pg::combo-item-cfs item)))
(defmethod fs-size ((cfs lex::cfs)) (fs-size (lex::cfs-fs cfs)))
(defmethod fs-size ((fs csli-unify::fs))
  (cond ((csli-unify::tdl-atomic fs) 1)
        (t (+ 1 (loop
                    for arc in (csli-unify::fs-arcs fs)
                    sum (fs-size (rest arc)))))))
(defmethod fs-size ((fs t)) -1)

;;;
;;; _fix_me_ (some day)
;;; the parser item structure has changed; until we actually find some use for
;;; those derivation trees, why try and shoot a running fox ...
;;;                                                         (30-aug-99  -  oe)
;;;
(defun compute-derivation-tree (item &optional (offset (pg::item-start item)))
  (flet ((informative-item-label (item)
           (when (pg::combo-item-p item)
             (case (pg::combo-item-itype item)
               ((:lex-entry :c-lex-entry)
                (format nil "~(~a~)" (pg::combo-item-index item)))
               (t (format nil "~(~a~)" (pg::item-label item)))))))
    (cond
     ((eq (pg::combo-item-itype item) :morph)
      (list
       (format nil "~(~a~)" (informative-item-label item))
       (- (pg::item-start item) offset)
       (- (pg::item-end item) offset)
       (list
        (format nil "~(~a~)" (pg::combo-item-index item))
        (- (pg::item-start item) offset)
        (- (pg::item-end item) offset))))
     (t
      (cons
       (informative-item-label item)
       (cons 
        (- (pg::item-start item) offset)
        (cons
         (- (pg::item-end item) offset)
         (when (pg::item-daughters item)
           (loop
               for kid across (pg::item-daughters item)
               for start = (pg::item-start item)
               collect (compute-derivation-tree kid start))))))))))

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
      (flet ((ecount (edge type)
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
                   do (ecount edge :passives)))
        (loop for edges across saedges
            do (loop for edge in edges
                   do (ecount edge :actives)))
        (loop for edges across eaedges
            do (loop for edge in edges
                   do (ecount edge :actives)))
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

(defun find-words (&optional (parser (pg::get-parser :lexicon)))
  (let* ((chart (parser-chart parser))
         (passives (reduce #'append (chart-passive-items-starting-at chart)))
         (active-items-starting-at 
          (reduce #'append 
                  (when chart (chart-active-items-starting-at chart))))
         (active-items-ending-at 
          (reduce #'append 
                  (when chart (chart-active-items-ending-at chart))))
         (actives (union active-items-starting-at active-items-ending-at)))
    (loop 
        for item in (append passives actives)
        when (member (combo-item-itype item)
                     (rest (assoc :rule *legal-combinations*)))
        collect item)))

(defun maximal-number-of-edges-exceeded-p (parser)
  (let ((slice (loop
                   with all = (main::add-keys main::*parser*) 
                   for keys = all then (rest (rest keys))
                   while keys
                   thereis (and (eq (first keys) :itemslice)
                                (second keys)))))
    (when (and slice (integerp slice))
      (let* ((statistics (parser-global-stats parser))
             (successful (and statistics (stats-successful statistics))))
        (when (and (integerp successful) (>= successful slice))
          (format nil "item limit (~a)" successful))))))

(defun maximal-number-of-tasks-exceeded-p (parser)
  (let ((slice (loop
                   with all = (main::add-keys main::*parser*)
                   for keys = all then (rest (rest keys))
                   while keys
                   thereis (and (eq (first keys) :taskslice)
                                (second keys)))))
    (when (and slice (integerp slice))
      (let* ((statistics (parser-global-stats parser))
             (executed (and statistics (stats-executed statistics))))
        (when (and (integerp executed) (>= executed slice))
          (format nil "task limit (~a)" executed))))))

(defun time-exceeded-p (parser)
  (let ((slice (loop
                   with all = (main::add-keys main::*parser*)
                   for keys = all then (rest (rest keys))   
                   while keys
                   thereis (and (eq (first keys) :timeslice)
                                (second keys)))))
  (when (and slice (integerp slice))
    (let* ((statistics (parser-global-stats parser))
           (total (and statistics (stats-time statistics))))
      (when (and (integerp total) (>= total slice))
        (format nil "time limit (~a)" total))))))

;;;
;;; interface functions for reconstruction of derivations (in UDF --- unified
;;; derivation format |:-).
;;;

(defparameter *reconstruct-hook*
  #'(lambda (item &optional (i-input "reconstructed item"))
      (declare (ignore i-input))
      (when (and (combo-item-p item) 
                 (cfs-p (combo-item-cfs item)))
        (fed::call-fegramed (cfs-fs (combo-item-cfs item)) :wait nil))))

(defun find-lexical-entry (form instance)
  (main::lex-lookup form)
  (let* ((name (intern (if (stringp instance)
                         (string-upcase instance)
                         instance)
                       lex::*lex-package*))
         (instance (and (tdl::get-infon name *lex-package* :instances)
                        (first (tdl::get-instance name))))
         (cfs (and instance (create-cfs-from-instance instance))))
    (when cfs
      (make-combo-item
       :itype :lex-entry
       :cfs cfs))))

(defun find-affix (type)
  (let ((name 
         (intern (if (stringp type) (string-upcase type) type) *lex-package*)))
      (when (tdl::get-infon name *lex-package* :avms)
      (tdl::expand-type name :domain *lex-package*)
      (let ((prototype (tdl::get-prototype name *lex-package* :avms)))
        (make-cfs :fs (convert (feature-structure-term prototype) *unifier*)
                  :fc (feature-structure-funs prototype))))))

(defun find-rule (instance)
  (let ((name (intern (if (stringp instance)
                        (string-upcase instance)
                        instance)
                      lex::*lex-package*)))
    (or
     (find name (combo-parser-lex-rules (get-parser :lexicon))
           :key #'combo-item-index)
     (find name (combo-parser-syn-rules (get-parser :syntax))
           :key #'combo-item-index))))

(defun instantiate-rule (rule items)
  (let* ((csli-unify::*unify-debug* :return)
         (csli-unify::%failure% nil)
         (itype (combo-item-itype rule))
         (parser (get-parser (if (eq itype :lex-rule) :lexicon :syntax)))
         (result (cfs-fs (combo-item-cfs rule)))
         (status 0))
      
    (loop
        while result
        for item in items
        for i from 0
        do
          (setf status i)
          (setf result 
            (unify
             result
            (cfs-fs (combo-item-cfs item))
             (append *args-prefix* (fslist-nth-path i)))))

    (when (and result (combo-parser-result-restrictor parser))
      (setf result
        (restrict result
                  (rest (combo-parser-result-restrictor parser))
                  (first (combo-parser-result-restrictor parser)))))

    (if result
      (make-combo-item
       :start (item-start (first items))
       :end (item-end (first (last items)))
       :daughters (make-array 
                   (length (combo-item-rhs rule))
                   :initial-contents items)
       :cfs (make-cfs :fs result)
       :key (item-key rule)
       :label (item-label rule)
       :index (combo-item-index rule)
       :itype itype)
      (values status csli-unify::%failure%))))

(defun instantiate-preterminal (preterminal affix)
  ;;
  ;; _fix_me_
  ;; probably we would be better off with copies of .preterminal. and the cfs
  ;; that is destructively modified; right now, we hope nothing of this never
  ;; ever ends up in some cache                           (21-apr-99  -  oe)
  ;;
  (let* ((fs (cfs-fs (combo-item-cfs preterminal)))
         (csli-unify::*unify-debug* :return)
         (csli-unify::%failure% nil)
         (result (when fs
                   (unify
                    fs
                    (cfs-fs affix)
                    (append *args-prefix* 
                            (fslist-nth-path 0) 
                            (list lex::*affix-into*))))))
    (cond
     ((null fs)
      (error "instantiate-preterminal(): a mystery has happened."))
     ((null result)
      (values nil csli-unify::%failure%))
     (t
      (setf (cfs-fs (combo-item-cfs preterminal)) result)
      preterminal))))
