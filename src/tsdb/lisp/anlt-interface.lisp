;;; Copyright John Carroll 1999 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; 5 interface functions supplied by Alvey Tools GDE for [incr tsdb()]:
;;;
;;;  get-test-run-information
;;;  parse-word
;;;  initialize-test-run
;;;  finalize-test-run
;;;  parse-item

(in-package :cl-user)

(defun tsdb::get-test-run-information ()
   `((:AVMS . 0)
     (:SORTS . 0)
     (:TEMPLATES . 0)
     (:RULES .
        ,(let ((n 0))
           (dolist (name *id-rules n)
              (incf n (length (compile-idrule name))))))
     (:LRULES . 0)
     (:LEXICON . ,(length *words))
     (:GRAMMAR . "unknown")
     (:APPLICATION .
        ,(format nil "A(N)NLT GDE (version `~a'; ~:[chart~;LR~] mode)" 
            *gde-version-no *lr1-parse))))
                       

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
  (declare (ignore load trace))
  (ignore-errors
   (let* ((*package* (find-package :common-lisp-user))
          (*suppress-dict-messages t)
          (*warning-messages nil)
          (input (preprocess-tsdb-input-string word)))
     (loop
         for word in input
         for results = (g-defns word)
         sum (length results) into words
         finally (return (pairlis '(:words :l-stasks) (list words 0)))))))


(defun tsdb::initialize-test-run (&key interactive exhaustive)
   (declare (ignore interactive exhaustive))
   ;; returns whatever it likes; the return value will be given to
   ;; finalize-test-run() to restore the interactive environment if
   ;; necessary
   (setf *current-parse-trees nil)
   (setf *chart-edges nil)
   (compile-world)
   (if *lr1-parse
     (make-lr1-parse-states nil)
     (make-parse-rule-tree nil)))

(defun tsdb::finalize-test-run (environment)
   ;; called after completion of test run
   (declare (ignore environment))
   (input-word-invalidations *cached-words 'normalised)
   (dolist (word *cached-words)
      (remprop word 'word))
   (prog1
      `((:lexicon . ,(length *cached-words)))
      (setq *cached-words nil)))


;;; sets the processor into exhaustive mode if requested; parses
;;; .string. without producing any printout (unless .trace. is set);
;;; funcall()s .semantix-hook. and .trees-hook. to obtain MRS and tree
;;; representations (strings); all times in thousands of secs

(defun tsdb::parse-item (string 
                   &key exhaustive trace
                        readings edges derivations semantix-hook trees-hook
                        burst derivationp)
  (declare (ignore edges derivations semantix-hook trees-hook))
  
  (multiple-value-bind (return condition)
    (ignore-errors
     (let* ((*package* (find-package "COMMON-LISP-USER"))
            (*show-bracketings (cond
                                 (exhaustive nil)
                                 ((integerp readings) readings)
                                 (t *show-bracketings)))
            (sent (preprocess-tsdb-input-string string))
            (*suppress-dict-messages t)
            (*warning-messages nil)
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
                       conses (* scons 8)
                       symbols (* ssym 24)
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
                 (readings (length *current-parse-trees))
                 (readings (if (or (equal output "") (> readings 0))
                              readings
                             -1))
                 (summary (summarize-chart :derivationp derivationp)))
              `((:others . ,others) (:symbols . ,symbols) 
                (:conses . ,conses)
                (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
                (:total . ,tcpu) (:first . ,tcpu)
                (:pedges . ,(rest (assoc :pedges summary)))
                (:aedges . ,(rest (assoc :aedges summary)))
                (:p-stasks . ,s-tasks) (:p-etasks . ,e-tasks) 
                (:p-ftasks . ,f-tasks) (:p-ctasks . ,c-tasks) 
                (:l-stasks . ,(rest (assoc :l-stasks summary)))
                (:words . ,(rest (assoc :words summary)))
                (:total . ,tcpu)
                (:readings . ,readings)
                (:error . ,output)
                (:results .
                 ,(append
                   (loop
                       for i from 0
                       for parse in *current-parse-trees
                       for derivation = (format
                                          nil
                                          "~s"
                                          (compute-derivation-tree 
                                           (cdr parse) (car parse)))
                       for size = (parse-tsdb-count-nodes (cdr parse))
                       collect
                         (pairlis '(:result-id :derivation :size)
                                  (list i derivation size)))
                   (when derivationp
                     (loop
                         for i from (length *current-parse-trees)
                         for derivation in (rest (assoc :derivations summary))
                         for string = (format nil "~s" derivation)
                         collect (pairlis '(:result-id :derivation)
                                          (list i string)))))))))))
    (append
     (when condition
       (pairlis '(:readings 
                  :condition 
                  :error)
                (list -1 
                      (unless burst condition)
                      (format nil "~a" condition))))
     return)))


(defun preprocess-tsdb-input-string (string)
  (let* ((tokens (get-reply1 string nil)))
      (if (member (car (last tokens)) '(|.| |!| |?|))
          (butlast tokens)
          tokens)))


(defun compute-derivation-tree (cat vt)
   (declare (ignore vt))
   (get-rule-labelling-from-parse-tree cat))


(defun parse-tsdb-sentence (user-input &optional trace)
   (declare (ignore trace))
   (setq *chart-edges nil)
   (setq *current-parse-trees
      (remove-if-not #'(lambda (r) (top-category-p (cadr r) (car r)))
         (invoke-parser1 user-input)))
   (values -1 ; e-tasks s-tasks c-tasks f-tasks
      (loop
         with s-tasks = 0
         for edge in (car *chart-edges)
         when (not (g-chart-edge-needed edge)) ; inactive edges
         do (incf s-tasks
               (1+ (length
                      (find-if #'(lambda (x) (and (consp x) (eq (car x) '*packed)))
                         (cdar (g-chart-edge-found edge))))))
         finally (return s-tasks))
      -1 -1))


(defun summarize-chart (&key derivationp)
  (loop
      with leaves = nil
      with pedges = 0
      with aedges = 0
      with words = 0
      with l-stasks = 0
      with derivations = nil
      for edge in (car *chart-edges) ; not including packed edges
      do
      (let ((defn (g-chart-edge-found edge)))
         (cond
            ((g-chart-edge-needed edge) 
               (incf aedges))
            ((and (cdr defn) (atom (cadr defn))) ; lexical edge
               (incf words)
               (pushnew (find-if #'atom (cdr defn)) leaves))
            (t
               (incf pedges)
               (when derivationp 
                  (push
                     (compute-derivation-tree defn (chart-edge-nvt edge))
                     derivations)))))
      finally (return (pairlis '(:pedges :aedges :words :l-stasks 
                                 :derivations)
                               (list (+ pedges words) aedges words l-stasks 
                                     derivations)))))
              

(defun parse-tsdb-count-nodes (tree)
   (labels ((nvals (cat)
              (let ((nv 0))
                 (dotimes (i (length cat) nv)
                    (when (> i 0)
                       (incf nv)
                       (when (vectorp (svref cat i))
                          (incf nv (nvals (svref cat i)))))))))
     (+ (nvals (caar tree)) ; number of feature values in category
        (cond
           ((and (cdr tree) (atom (cadr tree)))
              0)
           (t
              (let ((n 0))
                 (dolist (d (cdr tree))
                    (incf n (parse-tsdb-count-nodes d)))
                 n))))))


;;; End of file
