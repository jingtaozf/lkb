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

(defmacro current-parser ()
  (if (equal (elt make::*page-version* 0) #\2)
    '(pg::get-parser :syntax)
    'pg::*global-parser*))

(defun run-protocol (protocol input &key (trace nil))
  (let* ((foo (open "/dev/null" :direction :output :if-exists :overwrite))
         (shell (stream main::*page-shell*))
         (controller (stream *page-controller*))
         (blinker (stream main::*blinker*))
         streams)
    (unless trace
      (setf (stream main::*page-shell*) foo)
      (setf (stream *page-controller*) foo)
      (setf (stream main::*blinker*) foo)
      (dolist (component (main::internal-protocol-rep protocol))
        (push (stream (main::p-compo (main::p-caller component))) streams)
        (setf (stream (main::p-compo (main::p-caller component))) foo)))
    (multiple-value-bind (result condition)
        (progn;ignore-errors
         (catch 'main::stop
           (main::eval-proto *page-controller* protocol input)))
      (close foo)
      (unless trace
        (setf (stream main::*page-shell*) shell)
        (setf (stream *page-controller*) controller)
        (setf (stream main::*blinker*) blinker)
        (do ((protocol (main::internal-protocol-rep protocol)
                       (rest protocol))
             (streams streams (rest streams)))
            ((null protocol))
          (setf (stream (main::p-compo (main::p-caller (first protocol))))
            (first streams))))
      (values result condition))))

(defun parse-word (word &key load trace nil)
  
  (setf (main::output-stream main::*lexicon*) nil)
  (let ((verbose-reader-p tdl::*verbose-reader-p*)
        (verbose-definition-p tdl::*verbose-definition-p*))
    (case load
      ((:warn :quiet :collect nil)
       (setf tdl::*verbose-reader-p* nil)
       (setf tdl::*verbose-definition-p* nil))
      ((:fair :modest)
       (setf tdl::*verbose-reader-p* nil)
       (setf tdl::*verbose-definition-p* t))
      ((:full :all :verbose t)
       (setf tdl::*verbose-reader-p* t)
       (setf tdl::*verbose-definition-p* t)))
    
    (multiple-value-bind (result condition)
        (run-protocol *tsdb-morphology-protocol* word :trace trace)
      (declare (ignore result))
      (setf tdl::*verbose-reader-p* verbose-reader-p)
      (setf tdl::*verbose-definition-p* verbose-definition-p)
      (let* ((entries (unless condition (main::output-stream main::*lexicon*)))
             (words (length entries))
             (lstasks 0))
        (when (equal load :collect)
          (map nil #'(lambda (entry)
                       (when (main::typed-item-p entry)
                         (push (second (main::typed-item-args entry))
                               %tsdb-lexical-preterminals%)))
               entries))
        (pairlis '(:words :l-stasks)
                 (list words lstasks))))))

(defun initialize-test-run ()
  
  (let* ((parser (pg::get-parser :syntax))
         (environment 
          (pairlis '(tdl::*verbose-reader-p*
                     tdl::*verbose-definition-p*
                     main::*draw-chart-p*
                     pg::ebl-parser-name-rule-fn
                     pg::ebl-parser-external-signal-fn
                     pg::*maximal-number-of-tasks*
                     pg::*maximal-number-of-tasks-exceeded-p*)
                   (list tdl::*verbose-reader-p*
                         tdl::*verbose-definition-p*
                         main::*draw-chart-p*
                         (pg::ebl-parser-name-rule-fn parser)
                         (pg::ebl-parser-external-signal-fn parser)
                         pg::*maximal-number-of-tasks*
                         pg::*maximal-number-of-tasks-exceeded-p*))))

    (setf (pg::ebl-parser-name-rule-fn (pg::get-parser :syntax))
      #'get-informative-item-label)
    (scanning::init-scanner)
    environment))

(defun finalize-test-run (environment)
  (let ((parser (pg::get-parser :syntax)))
    (dolist (pair environment)
      (case (first pair)
        (pg::ebl-parser-name-rule-fn 
         (setf (pg::ebl-parser-name-rule-fn parser) (rest pair)))
        (pg::ebl-parser-external-signal-fn
         (setf (pg::ebl-parser-external-signal-fn parser) (rest pair)))
        (t
         (set (first pair) (rest pair)))))))

(defun parse-item (string &key (exhaustive *tsdb-exhaustive-p*)
                               (tasks 0) trace derivations
                               semantix-hook trees-hook)

  (let* ((string (remove-and-insert-punctuation string))
         (main::*exhaustive* exhaustive)
         (lexicon-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon)))
         (syntax-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :syntax)))
         result tcpu tgc treal conses symbols others)

    (setf pg::*maximal-number-of-tasks*
      (if (and (integerp tasks) (> tasks 0)) tasks nil))
    (setf pg::*current-number-of-tasks* 0)
    (setf pg::*maximal-number-of-tasks-exceeded-p* nil)
    (setf (pg::ebl-parser-external-signal-fn (pg::get-parser :syntax))
      (if (> tasks 0)
        #'pg::maximal-number-of-tasks-exceeded-p
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
                                         #+:allegro-v5.0 foo)
                                  #+:allegro-v5.0
                                  (declare (ignore foo))
                                  (setf tgc (/ (+ tgcu tgcs) 10)
                                        tcpu (/ (+ tu ts) 10)
                                        treal (/ tr 10)
                                        conses scons
                                        symbols ssym
                                        others sother)))
      
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
        (when pg::*maximal-number-of-tasks-exceeded-p*
          (push (cons :timeup pg::*current-number-of-tasks*) result)))
      
      (append
       result
       (pairlis '(:tgc :tcpu :treal :conses :symbols :others)
                (list tgc tcpu treal conses symbols others))
       (cond
        (condition
         (pairlis
          (list :readings :condition :error)
          (list -1 condition (format nil "~a" condition))))
        ((or (eq (first return) :incomplete-input)
             (null (main::output-stream main::*lexicon*)))
         (pairlis
          '(:readings :error)
          (list -1 "null parser input")))
        (t
         (let* ((items (main::output-stream main::*parser*))
                (readings (length items))
                (words (length (main::output-stream main::*lexicon*)))
                (l-stasks 0)
                (statistics 
                 (pg::parser-stats-readings (pg::get-parser :syntax)))
                (first (when statistics (first (last statistics))))
                (first (when first (pg::stats-time first)))
                (first 
                 (when first (* (/ first internal-time-units-per-second) 100)))
                (global (pg::parser-global-stats (pg::get-parser :syntax)))
                (total (when global (pg::stats-time global)))
                (total 
                 (when total (* (/ total internal-time-units-per-second) 100)))
                (ftasks (when global (pg::stats-filtered global)))
                (etasks (when global (pg::stats-executed global)))
                (stasks (when global (pg::stats-successful global)))
                (ctasks (if (and ftasks etasks) (+ ftasks etasks) -1))
                (aedges 0)
                (pedges 0)
                (redges 0)
                results)
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
                      (tree (when trees-hook
                              (ignore-errors (funcall trees-hook item))))
                      (mrs (when semantix-hook
                             (ignore-errors (funcall semantix-hook item))))
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
                 (push (pairlis
                        '(:result-id :time
                          :r-ctasks :r-ftasks :r-etasks :r-stasks
                          :derivation :tree :mrs)
                        (list i time
                              ctasks ftasks etasks stasks
                              derivation tree mrs))
                       results))))
           (pairlis
            '(:readings :first :total
              :words :l-stasks
              :p-ctasks :p-ftasks :p-etasks :p-stasks
              :aedges :pedges :redges 
              :results)
            (list readings first total
                  words l-stasks
                  ctasks ftasks etasks stasks
                  aedges pedges redges 
                  results)))))))))

(defun get-test-run-information ()
  (let* ((application (if (boundp 'make::*page-version*)
                        (format nil "PAGE (~a)" make::*page-version*)
                        "PAGE"))
         (grammar 
          (cond 
           ((boundp 'disco::*grammar-version*) disco::*grammar-version*)
           ((boundp 'common-lisp-user::*grammar-version*) 
            common-lisp-user::*grammar-version*)
           (t "anonymous grammar")))
         (avms (tdl::get-global :avms))
         (avms (when (hash-table-p avms) (hash-table-count avms)))
         (sorts (tdl::get-global :sorts))
         (sorts (when (hash-table-p sorts) (hash-table-count sorts)))
         (templates (tdl::get-global :templates))
         (templates
          (when (hash-table-p templates) (hash-table-count templates)))
         (lexicon 42)
         (lrules 
          (length (pg::combo-parser-lex-rules (pg::get-parser :lexicon))))
         (rules 
          (length (pg::combo-parser-syn-rules (pg::get-parser :syntax)))))
    (append (and avms (list (cons :avms avms)))
            (and sorts (list (cons :sorts sorts)))
            (and templates (list (cons :templates templates)))
            (pairlis '(:application :grammar
                       :lexicon :lrules :rules)
                     (list application grammar
                           lexicon lrules rules)))))

(defmacro get-item-type (item)
  `(get-fs-type (lex::cfs-fs (pg::combo-item-cfs ,item))))

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

(defmethod get-fs-type ((fs unify::node))
  (tdl::value-type (unify::get-type fs)))
(defmethod get-fs-type ((fs csli-unify::fs))
  (csli-unify::fs-type fs))

(defun get-informative-item-label (item)
  (when (pg::combo-item-p item)
    (case (pg::combo-item-itype item)
      ((:lex-entry :c-lex-entry)
       (let* ((spare (pg::combo-item-spare item))
              (spare (when (and spare (or (symbolp spare)
                                          (stringp spare)))
                       (string spare)))
              (cfs (pg::combo-item-cfs item))
              (fs (and cfs (pg::cfs-fs cfs))))
         (and fs (string-downcase 
                  (or spare (format nil "~a" (get-fs-type fs)))))))
      (t (string-downcase (format nil "~a" (pg::item-label item)))))))

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


(in-package "PARSING")

;;;
;;; this is code that started out in the CSLI `tuneup.lisp' a while or two
;;; ago (that night when people had had a bottle of champagne while admiring
;;; the half moon bay sunset |:-); since it is truly tsdb(1) specific, it seems
;;; plausible to move it here.                           (31-oct-97  -  oe)
;;;

(defun total-number-of-items (&optional (parser (pg::get-parser :syntax)))
  (let* ((chart (parser-chart parser))
         (passive-items-starting-at
          (when chart 
            (reduce #'append (chart-passive-items-starting-at chart))))
         (passive-items-ending-at
          (when chart 
            (reduce #'append (chart-passive-items-ending-at chart))))
         (active-items-starting-at 
          (when chart (chart-active-items-starting-at chart)))
         (active-items-ending-at 
          (when chart (chart-active-items-ending-at chart)))
         (total 
          (when (or passive-items-starting-at passive-items-ending-at)
            (length 
             (union passive-items-starting-at passive-items-ending-at)))))
    (when (and active-items-starting-at active-items-ending-at)
      (let ((n (array-dimension active-items-starting-at 0)))
        (dotimes (i n)
          (incf total (length (aref active-items-starting-at i)))))
      (let ((n (array-dimension active-items-ending-at 0)))
        (dotimes (i n)
          (incf total (length (aref active-items-ending-at i))))))
    total))

(defparameter *current-number-of-tasks* 0)
(defparameter *maximal-number-of-tasks* 0)
(defparameter *maximal-number-of-tasks-exceeded-p* nil)

(defun maximal-number-of-tasks-exceeded-p (parser)
  (declare (ignore parser))
  (setf *maximal-number-of-tasks-exceeded-p*
    (>= (incf *current-number-of-tasks*) *maximal-number-of-tasks*)))
