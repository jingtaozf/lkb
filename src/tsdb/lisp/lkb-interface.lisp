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
     (:LEXICON . ,(hash-table-count *psorts*))
     (:GRAMMAR . ,(if (boundp '*grammar-version*) 
                      (symbol-value '*grammar-version*)
                      "unknown"))
     (:APPLICATION . ,(format nil "LKB (~A mode)" *lkb-system-version*))))


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
    (let ((*package* (find-package "COMMON-LISP-USER"))
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
    (clear-type-cache)
    (uncache-psorts)))

(defun finalize-test-run (environment)
   ;; called after completion of test run
   (declare (ignore environment))
  (let ((*package* (find-package "COMMON-LISP-USER")))
    (clear-type-cache)
    (uncache-psorts)))

;;; sets the processor into exhaustive mode if requested; parses
;;; .string. without producing any printout (unless .trace. is set);
;;; funcall()s .semantix-hook. and .trees-hook. to obtain MRS and tree
;;; representations (strings); all times in hundredths of secs

(defun parse-item (string &key exhaustive
                   edges trace derivations semantix-hook trees-hook)
  (declare (ignore exhaustive derivations semantix-hook trees-hook))
  
  (multiple-value-bind (return condition)
    (ignore-errors
     (let* ((*package* (find-package "COMMON-LISP-USER"))
            (*maximum-number-of-edges* 
             (if (or (null edges) (zerop edges)) (expt 2 42) edges))
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
             #'(lambda () (parse-tsdb-sentence sent))
             #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setq tgc (/ (+ tgcu tgcs) 10)
                       tcpu (/ (+ tu ts) 10)
                       treal (/ tr 10)
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
                  (parse-tsdb-sentence sent))
              (let ((rawgc (- #+mcl (ccl:gctime) tgc)))
                (setq symbols 0 conses 0
                      others
                      (- #+mcl (ccl::total-bytes-allocated) #-mcl -1 others)
                      tcpu
                      (round 
                       (* (- (get-internal-run-time) tcpu #+mcl rawgc) 100)
                       internal-time-units-per-second)
                      treal
                      (round (* (- (get-internal-real-time) treal) 100)
                             internal-time-units-per-second)
                      tgc #+mcl (round (* rawgc 100)
                                       internal-time-units-per-second)
                      #-mcl -1)))
          (let ((n 0)
                (*print-pretty* nil) (*print-level* nil) (*print-length* nil))
            (multiple-value-bind (l-s-tasks redges words)
                (parse-tsdb-count-lrules-edges-morphs)
              `((:TIMEUP) (:COPIES . -1) (:UNIFICATIONS . -1)
                          (:OTHERS . ,others) (:SYMBOLS . ,symbols) 
                          (:CONSES . ,conses)
                          (:TREAL . ,treal) (:TCPU . ,(+ tcpu tgc)) 
                          (:TGC . ,tgc)
                          (:REDGES . ,redges) 
                          (:PEDGES . ,*edge-id*) (:AEDGES . -1)
                          (:P-STASKS . ,s-tasks) (:P-ETASKS . ,e-tasks) 
                          (:P-FTASKS . ,f-tasks) (:P-CTASKS . ,c-tasks) 
                          (:L-STASKS . ,l-s-tasks) (:WORDS . ,words)
                          (:TOTAL . ,tcpu) (:FIRST . ,tcpu) 
                          (:READINGS . ,(length *parse-record*))
                          (:ERROR .
                           ,(substitute #\; #\newline
                             (remove 
                              #\@
                              (string-trim '(#\newline #\space) 
                                           (get-output-stream-string str)))))
                          (:RESULTS .
                           ,(mapcar
                             #'(lambda (parse)
                                 (prog1
                                     `((:MRS . "") (:TREE . "")
                                       (:DERIVATION .
                                        ,(prin1-to-string
                                          (parse-tree-structure parse)))
                                       (:R-REDGES .
                                        ,(length (parse-tsdb-distinct-edges
                                                  parse nil)))
                                       (:SIZE . ,(parse-tsdb-count-nodes parse))
                                       (:R-STASKS . -1) (:R-ETASKS . -1) 
                                       (:R-FTASKS . -1)
                                       (:R-CTASKS . -1) 
                                       (:TIME . ,tcpu) (:RESULT-ID . ,n))
                                   (setq tcpu 0 n (1+ n))))
                             *parse-record*))))))))
    (append
     (when condition
       (pairlis '(:readings :condition :error)
                (list -1 condition (format nil "~a" condition))))
     return)))


(defvar *do-something-with-parse* nil)

(defun parse-tsdb-sentence (user-input)
   (multiple-value-prog1
      (parse user-input nil)
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
             (when (get-lex-rule-entry 
                    (edge-rule-number (chart-configuration-edge config)))
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
            (not (get-lex-rule-entry (edge-rule-number edge))))
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

(defun uncache-psorts ()
   (maphash
      #'(lambda (id value)
          (declare (ignore id))
          (setf (cddr value) nil))
      *psorts*))


;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
   (when (find-package :TSDB)
      (import '(get-test-run-information
                parse-word
                initialize-test-run
                finalize-test-run
                parse-item)
               :TSDB)))




