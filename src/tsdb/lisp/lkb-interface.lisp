;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; 5 interface functions supplied by lkb for tsdb proper:
;;;
;;;  get-test-run-information
;;;  parse-word
;;;  initialize-test-run
;;;  finalize-test-run
;;;  parse-item

(in-package :cl-user)

(defun get-test-run-information ()
   `((:AVMS . ,(- (hash-table-count *types*) (length *templates*)))
     (:SORTS . 0)
     (:TEMPLATES . ,(length *templates*))
     (:RULES . ,(hash-table-count *rules*))
     (:LRULES . ,(hash-table-count *lexical-rules*))
     (:LEXICON . ,(size-of-lexicon))
     (:GRAMMAR . ,(if (boundp '*grammar-version*) 
                      (symbol-value '*grammar-version*)
                      "unknown"))
     (:APPLICATION . ,(format 
                       nil 
                       "LKB (~A mode; version ~@[`~a'~])" 
                       *lkb-system-version* 
                       (and (find-symbol "*CVS-VERSION*" :cl-user)
                            (boundp (find-symbol "*CVS-VERSION*" :cl-user))
                            (symbol-value 
                             (find-symbol "*CVS-VERSION*" :cl-user)))))))


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
   (let* ((*package* (find-package "COMMON-LISP-USER"))
          (str (make-string-output-stream)) ; capture any warning messages
          (*standard-output* (if trace
                                 (make-broadcast-stream *standard-output* str)
                               str))
          (input (split-into-words (preprocess-sentence-string word))))
      (parse input nil)
      (multiple-value-bind (l-s-tasks redges words)
          (parse-tsdb-count-lrules-edges-morphs)
        (declare (ignore redges))
        `((:L-STASKS . ,l-s-tasks) (:WORDS . ,words))))))


#+allegro
(defvar *allegro-gccount* 0)

(defun initialize-test-run (&key interactive)
  (declare (ignore interactive))
   ;; returns whatever it likes; the return value will be given to
   ;; finalize-test-run() to restore the interactive environment if
   ;; necessary
  (let ((*package* (find-package "COMMON-LISP-USER")))
    (clear-type-cache)))

(defun finalize-test-run (environment)
   ;; called after completion of test run
   (declare (ignore environment))
  (let ((*package* (find-package "COMMON-LISP-USER")))
    (clear-type-cache)
    (uncache-lexicon)))

;;; sets the processor into exhaustive mode if requested; parses
;;; .string. without producing any printout (unless .trace. is set);
;;; funcall()s .semantix-hook. and .trees-hook. to obtain MRS and tree
;;; representations (strings); all times in hundredths of secs

(defun parse-item (string 
                   &key exhaustive
                        edges trace derivations semantix-hook trees-hook
                        burst)
  (declare (ignore derivations semantix-hook trees-hook))
  
  (multiple-value-bind (return condition)
    (ignore-errors
     (let* ((*package* (find-package "COMMON-LISP-USER"))
            (*maximum-number-of-edges* (if (or (null edges) (zerop edges))
                                         *maximum-number-of-edges*
                                         edges))
            (*first-only-p* (not exhaustive))
             (sent
              (split-into-words (preprocess-sentence-string string)))
             (str (make-string-output-stream)) ; capture any warning messages
             (*standard-output* (if trace
                                  (make-broadcast-stream *standard-output* str)
                                  str))
             tgc tcpu treal conses symbols others)
        (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks)
            #+allegro
            (excl::time-a-funcall
             #'(lambda () (parse-tsdb-sentence sent trace))
             #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setq tgc (+ tgcu tgcs)
                       tcpu (+ tu ts)
                       treal tr
                       conses scons
                       symbols ssym
                       others sother)))
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
                      (- #+mcl (ccl::total-bytes-allocated) #-mcl -1 others)
                      tcpu
                      (round 
                       (* (- (get-internal-run-time) tcpu #+mcl rawgc) 1000)
                       internal-time-units-per-second)
                      treal
                      (round (* (- (get-internal-real-time) treal) 1000)
                             internal-time-units-per-second)
                      tgc #+mcl (round (* rawgc 1000)
                                       internal-time-units-per-second)
                      #-mcl -1)))
          (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                 (output (get-output-stream-string str))
                 (readings (length *parse-record*))
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
                          (if (> readings 0) total -1))))
            (multiple-value-bind (l-s-tasks redges words)
                (parse-tsdb-count-lrules-edges-morphs)
              `((:TIMEUP) ;; we should be able to tell from *edge-id*
                (:COPIES . -1) (:UNIFICATIONS . -1)
                (:OTHERS . ,others) (:SYMBOLS . ,symbols) 
                (:CONSES . ,conses)
                (:TREAL . ,treal) (:TCPU . ,(+ tcpu tgc)) 
                (:TGC . ,tgc)
                (:REDGES . ,redges) 
                (:PEDGES . ,(- *edge-id* (length *morph-records*)))
                (:AEDGES . -1)
                (:P-STASKS . ,s-tasks) (:P-ETASKS . ,e-tasks) 
                (:P-FTASKS . ,f-tasks) (:P-CTASKS . ,c-tasks) 
                (:L-STASKS . ,l-s-tasks) (:WORDS . ,words)
                (:TOTAL . ,total) (:FIRST . ,first) 
                (:READINGS . ,readings)
                (:ERROR . ,(tsdb::normalize-string output))
                (:RESULTS .
                 ,(loop
                      for i from 0
                      for parse in (nreverse *parse-record*)
                      for time = (if (integerp (first times))
                                   (round (* (- (pop times) start) 1000)
                                          internal-time-units-per-second )
                                   total)
                      for derivation = (tsdb::normalize-string
                                        (format
                                         nil
                                         "~s"
                                         (compute-derivation-tree parse)))
                      for r-redges = (length 
                                      (parse-tsdb-distinct-edges parse nil))
                      for size = (parse-tsdb-count-nodes parse)
                      collect
                        (pairlis '(:result-id :mrs :tree
                                   :derivation :r-redges :size
                                   :r-stasks :r-etasks 
                                   :r-ftasks :r-ctasks
                                   :time)
                                 (list i "" ""
                                       derivation r-redges size
                                       -1 -1 
                                       -1 -1 
                                       time))))))))))
    (append
     (when condition
       (pairlis '(:readings 
                  :condition 
                  :error)
                (list -1 
                      (unless burst condition)
                      (tsdb::normalize-string (format nil "~a" condition)))))
     return)))



(defun compute-derivation-tree (edge &optional (offset 0))
  (cond
   ((null (edge-children edge))
    (list (format nil "~(~a~)" (first (edge-lex-ids edge)))
          offset (+ offset 1)
          (list (format nil "~(~a~)" (edge-rule-number edge)) 
                offset (+ offset 1))))
   (t
    (let* ((end offset)
           (children
            (loop 
                for kid in (edge-children edge)
                for derivation = (compute-derivation-tree kid end)
                do
                  (setf end (max end (third derivation)))
                collect derivation)))
      (nconc (list (format nil "~(~a~)" (edge-rule-number edge))
                   offset end)
             children)))))

(defun parse-tsdb-sentence (user-input &optional trace)
   (multiple-value-prog1
      (parse user-input trace)
      (when (fboundp *do-something-with-parse*)
         (funcall *do-something-with-parse*))))


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
  (cond
   ((and (find-symbol "*PSORTS*") (boundp (find-symbol "*PSORTS*")))
    (hash-table-count (symbol-value (find-symbol "*PSORTS*"))))
   ((and (find-symbol "*LEXICON*") (boundp (find-symbol "*LEXICON*")))
    (length (collect-expanded-lex-ids 
             (symbol-value (find-symbol "*LEXICON*")))))
   (t
    -1)))
    
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
   (when (find-package :TSDB)
      (import '(get-test-run-information
                parse-word
                initialize-test-run
                finalize-test-run
                parse-item
                uncache-lexicon)
              :TSDB)))
