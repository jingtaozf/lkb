;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Temporary emulation of part of tsdb functionality for lkb

#|
parse:
  parse-id :integer :key                # unique parse identifier
  run-id :integer :key                  # test run for this parse
  i-id :integer :key                    # item parsed
  readings :integer                     # number of readings obtained
  first :integer                        # time to find first reading
  total :integer                        # total time for parsing
  tcpu :integer                         # total (cpu) time for processing
  tgc :integer                          # gc time used
  treal :integer                        # overall real time
  words :integer                        # lexical entries retrieved
  l-stasks :integer                     # successful lexical rule applications
  p-ctasks :integer                     # parser contemplated tasks (LKB)
  p-ftasks :integer                     # parser filtered tasks
  p-etasks :integer                     # parser executed tasks
  p-stasks :integer                     # parser succeeding tasks
  aedges :integer                       # active items in chart (PAGE)
  pedges :integer                       # passive items in chart
  redges :integer                       # items contributing to result(s)
  unifications :integer                 # number of (node) unifications
  copies :integer                       # number of (node) copy operations
  bytes :integer                        # bytes of memory allocated
  gcs :integer                          # number of garbage collections
  i-load :integer                       # initial load (start of parse)
  a-load :integer                       # average load
  date :date                            # date and time of parse
  error :string                         # error string (if applicable |:-)

result:
  parse-id :integer :key                # parse for this result
  result-id :integer                    # unique result identifier
  time :integer                         # time to find this result
  r-ctasks :integer                     # parser contemplated tasks
  r-ftasks :integer                     # parser filtered tasks
  r-etasks :integer                     # parser executed tasks
  r-stasks :integer                     # parser succeeding tasks
  size :integer                         # size of feature structure
  r-redges :integer                     # items contributing to this result
  derivation :string                    # derivation tree for this reading
  tree :string                          # phrase structure tree (CSLI labels)
  mrs :string                           # mrs for this reading

run:
  run-id :integer :key                  # unique test run identifier
  comment :string                       # descriptive narrative
  platform :string                      # common-lisp platform (version)
  application :string                   # application (version) used
  grammar :string                       # grammar (version) used
  avms :integer                         # number of avm types in image
  sorts :integer                        # number of sort types in image
  templates :integer                    # number of templates in image
  lexicon :integer                      # number of lexical entries
  lrules :integer                       # number of lexical rules
  rules :integer                        # number of (non-lexical) rules
  user :string                          # user who did the test run
  host :string                          # machine used for this run
  os :string                            # operating system (version)
  start :date                           # start time of this test run
|#

;;; Parsing sentences from file

(defun parse-tsdb-sentences (input-file parse-file result-file run-file)
   (with-open-file (istream input-file :direction :input)
      (parse-tsdb-sentences1
         istream (read-line istream nil 'eof) parse-file result-file run-file)))


(defun parse-tsdb-sentences1 (istream line parse-file result-file run-file)
   (flet ((parse-tsdb-sentences-get-filename (prompt file)
            (unless file
               (setq file (ask-user-for-new-pathname prompt)))
            (when file
               (if (probe-file file) (and (delete-file file) file) file))))
      (when
         (setq parse-file
            (parse-tsdb-sentences-get-filename "Parse file?" parse-file))
         (when
            (setq result-file
               (parse-tsdb-sentences-get-filename "Result file?" result-file))
            (when
               (setq run-file
                  (parse-tsdb-sentences-get-filename "Run file?" run-file))
               (format t "~%~A: ~A -> ~A, ~A..." 'parse-tsdb-sentences
                  (truename istream) parse-file result-file)
               (finish-output)
               (parse-tsdb-write-run-file run-file)
               (let ((run (get-internal-run-time))
                     (gc #+mcl (ccl:gctime)
                         #+allegro (nth-value 2 (excl::get-internal-run-times))
                         #-(or mcl allegro) 0))
                  (parse-tsdb-sentences2 istream line parse-file result-file)
                  (let ((run (- (get-internal-run-time) run))
                        (gc (- #+mcl (ccl:gctime)
                               #+allegro (nth-value 2 (excl::get-internal-run-times))
                               gc))
                        )
                     (format t "~%~%Total CPU time: ~A secs"
                        (round
                           #+(or mcl allegro) (- run gc) ; get-internal-run-time included gc time
                           #-(or mcl allegro) run
                           internal-time-units-per-second))
                     #+(or mcl allegro)
                     (format t " (+ ~A secs GC)"
                        (round gc internal-time-units-per-second)))))))))


(defun parse-tsdb-write-run-file (run-file)
   (with-open-file (ostream run-file :direction :output
                    :if-exists :supersede :if-does-not-exist :create)
      (format ostream "~@{~A~^@~}~%"
         (get-universal-time) "none"
         (format nil "~A (~A)" (lisp-implementation-type) (lisp-implementation-version))
         (format nil "LKB (~A mode)" *lkb-system-version*)
         (or (find :lingo *features*) "unknown")
         (- (hash-table-count *types*) (length *templates*))
         -1 (length *templates*) (hash-table-count *psorts*)
         (hash-table-count *lexical-rules*) (hash-table-count *rules*)
         (or #+(and allegro (or unix sunos)) (system:getenv "USER")
             #+(and mcl powerpc) (ccl:process-name ccl:*current-process*)
            "unknown")
         (format nil "~A (~A)" (machine-version) (short-site-name))
         (or (software-version) "unknown")
         (parse-tsdb-date-string))))


;;; parse:
;;;  l-stasks :integer                     # successful lexical rule applications
;;;  -> counted over all edges, so that this work could be factored out
;;;  p-ctasks :integer                     # parser contemplated tasks (LKB)
;;;  -> calls to static checkpaths
;;;  p-ftasks :integer                     # parser filtered tasks
;;;  -> both static and dynamic checkpaths failure
;;;  p-etasks :integer                     # parser executed tasks
;;;  -> calls to unifier
;;;  p-stasks :integer                     # parser succeeding tasks
;;;  -> successful unifications - but a subsequent failure in copy counts -1 
;;;
;;; I don't believe I can do the following since they refer to the history of
;;; individual analyses, which I don't keep
;;;
;;; result:
;;;  r-ctasks :integer                     # parser contemplated tasks
;;;  r-ftasks :integer                     # parser filtered tasks
;;;  r-etasks :integer                     # parser executed tasks
;;;  r-stasks :integer                     # parser succeeding tasks

(proclaim '(special *do-something-with-parse* *tdsb-progress-pos*))

(defun parse-tsdb-sentences2 (istream line parse-file result-file
                              &aux (nsent 0) (*tdsb-progress-pos* 0)
                              #+allegro (old-hook excl:*gc-after-hook*) #+allegro (gccount 0))
   #+allegro (setf (sys:gsgc-switch :hook-after-gc) t)
   #+allegro
   (let ((continuation excl:*gc-after-hook*))
      (setq excl:*gc-after-hook*
         #'(lambda (global new old efficiency &rest rest)
             (incf gccount)
             (when continuation
                (apply continuation global new old efficiency rest)))))
   ;; open and close output files for each sentence, so if run fails for some reason
   ;; we have all results in them until that point
   (clear-type-cache)
   (unwind-protect
      (loop
         (multiple-value-bind (id words)
               (parse-tsdb-sentence-read line)
            (when (eq id 'eof) (return))
            (incf nsent)
            ;; (when (eql (rem nsent 50) 1) (print nsent) (gc) (room))
            (when (eql (rem nsent 50) 1)
               ;; remove all cached lexical entries at start and after every 50 sentences
               (uncache-psorts))
            (parse-tsdb-sentences-progress id)
            (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                   (str (make-string-output-stream)) ; capture any warning messages
                   (*standard-output* (make-broadcast-stream *standard-output* str))
                   (real (get-internal-real-time))
                   (run (get-internal-run-time))
                   (gc #+mcl (ccl:gctime)
                       #+allegro (nth-value 2 (excl::get-internal-run-times))
                       #-(or mcl allegro) 0)
                   (gcs #+mcl (ccl:gccounts) #+allegro gccount #-(or mcl allegro) 0)
                   (bytes #+mcl (ccl::total-bytes-allocated)
                          #+allegro (sys:gsgc-totalloc-bytes t)
                          #-(or mcl allegro) 0))
               (multiple-value-bind (e-tasks s-tasks c-tasks f-tasks)
                     (parse-tsdb-sentence words)
                  (let* ((bytes (- #+mcl (ccl::total-bytes-allocated)
                                   #+allegro (sys:gsgc-totalloc-bytes t)
                                   #-(or mcl allegro) -1
                                   bytes))
                         (gcs (- #+mcl (ccl:gccounts) #+allegro gccount
                                 #-(or mcl allegro) -1
                                 gcs))
                         (rawgc (- #+mcl (ccl:gctime)
                                   #+allegro (nth-value 2 (excl::get-internal-run-times))
                                   gc))
                         (total ; all times in tenths of secs
                            (round (* (- (get-internal-run-time) run) 10)
                               internal-time-units-per-second))
                         (cpu
                            (round
                               (* (- (get-internal-run-time) run #+(or mcl allegro) rawgc)
                                  10)
                               internal-time-units-per-second))
                         (real
                            (round (* (- (get-internal-real-time) real) 10)
                               internal-time-units-per-second))
                         (gc #+(or mcl allegro)
                             (round (* rawgc 10) internal-time-units-per-second)
                             #-(or mcl allegro) -1)
                         (msgs
                            (substitute #\; #\newline
                               (remove #\@
                                  (string-trim '(#\newline #\space) (get-output-stream-string str)))))
                         (n 0))
                     (with-open-file (ostream parse-file :direction :output
                                     :if-exists :append :if-does-not-exist :create)
                        (multiple-value-bind (l-s-tasks redges words)
                              (parse-tsdb-count-lrules-edges-morphs)
                           (format ostream "~@{~A~^@~}~%"
                              id 1 id (length *parse-record*)
                              total total cpu gc real
                              words l-s-tasks c-tasks f-tasks e-tasks s-tasks
                              -1 *edge-id* redges
                              -1 -1 bytes gcs -1 -1 (parse-tsdb-date-string) msgs)))
                     (with-open-file (ostream result-file :direction :output
                                     :if-exists :append :if-does-not-exist :create)
                        (dolist (parse *parse-record*)
                           (format ostream "~@{~A@~}"
                              id n total -1 -1 -1 -1 
                              (parse-tsdb-count-nodes parse)
                              (length (parse-tsdb-distinct-edges parse nil)))
                           (format ostream "~A@~S@~A~%"
                              "" (parse-tree-structure parse) "")
                           (setq total 0) ; zero time for all parses after first
                           (incf n)))))))
         (setq line (read-line istream nil 'eof)))
     #+allegro (setq excl:*gc-after-hook* old-hook)
     (uncache-psorts)))


(defun parse-tsdb-count-lrules-edges-morphs ()
   (let ((distinct-parse-edges nil)
         (successful-lrule-applications 0))
      (dolist (p *parse-record*)
         (setq distinct-parse-edges (parse-tsdb-distinct-edges p distinct-parse-edges)))
      (dotimes (vertex (- *chart-limit* 1))
         (when (aref *chart* (+ 1 vertex))
            (dolist (config (chart-entry-configurations (aref *chart* (+ 1 vertex))))
               (when (get-lex-rule-entry (edge-rule-number (chart-configuration-edge config)))
                  (incf successful-lrule-applications)))))
      (values successful-lrule-applications (length distinct-parse-edges)
         (reduce #'+ *morphs* :key
            #'(lambda (x)
                (if (morph-edge-p x) (length (morph-edge-morph-results x)) 0))))))

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


(defun parse-tsdb-date-string nil
   (multiple-value-bind (sec min hour day mon year)
         (get-decoded-time)
      (format nil "~A-~A-~A ~2,'0D:~2,'0D:~2,'0D" day
         (aref #("jan" "feb" "mar" "apr" "may" "jun" "jul"
                 "aug" "sep" "oct" "nov" "dec") (1- mon))
         year hour min sec)))

(defun parse-tsdb-sentences-progress (id)
   ;; make sure output goes to the correct stream when clim is in use - also Ann
   ;; wanted explicit line-wrap so there's a sort-of-solution to this, except
   ;; it doesn't wrap at the same point on every line if warning messages come
   ;; out as well
   (#+(and clim (not tty)) with-output-to-top #+(and clim (not tty)) ()
    #-(and clim (not tty)) progn
      (let ((str (format nil " ~A" id))
            (out *standard-output*))
         (write-string str out)
         (finish-output out)
         (incf *tdsb-progress-pos* (length str))
         (when (> *tdsb-progress-pos* 60)
            (terpri out) (setq *tdsb-progress-pos* 0)))))


(defun parse-tsdb-sentence (user-input)
   (multiple-value-prog1
      (parse user-input nil)
      (when (fboundp *do-something-with-parse*)
         (funcall *do-something-with-parse*))))


(defun parse-tsdb-sentence-read (line)
   (if (or (eq line 'eof)
          (every #'(lambda (c) (member c '(#\space #\tab))) line))
      'eof
      (let
         ((id (subseq line 0 (position #\@ line)))
          (raw-sentence
             (subseq line (1+ (position-nth #\@ line 6)) (position-nth #\@ line 7))))
         (values id
            (split-into-words (preprocess-sentence-string raw-sentence))))))

(defun position-nth (char str n)
   (dotimes (ind (length str))
      (cond
         ((not (eql (char str ind) char)))
         ((eql n 1) (return ind))
         (t (decf n)))))


;;;

(defun uncache-psorts ()
   (maphash
      #'(lambda (id value)
          (declare (ignore id))
          (setf (cddr value) nil))
      *psorts*))
