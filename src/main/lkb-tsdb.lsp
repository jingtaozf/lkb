;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :lkb)

;;; tty interface functions to batch parsing - independent of tsdb functionality

;;; they use following functions defined in the tsdb file (lkb-interface.lisp)
;;;
;;;  get-test-run-information
;;;  initialize-test-run
;;;  finalize-test-run
;;;  parse-item
;;;
;;;  parse-word is not used
;;;
;;; they're in the lkb package but also exported to the tsdb package

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
      (let ((info
             (append
                `((:run-id . 1) (:comment . "none")
                  (:platform .
                     ,(format nil "~A (~A)"
                        (lisp-implementation-type) (lisp-implementation-version)))
                  (:user .
                     ,(or #+(and allegro (or unix sunos)) (system:getenv "USER")
                         #+(and mcl powerpc) (ccl:process-name ccl:*current-process*)
                         "unknown"))
                  (:host .
                     ,(format nil "~A (~A)" (machine-version) (short-site-name)))
                  (:os . ,(or (software-version) "unknown"))
                  (:start . ,(parse-tsdb-date-string)))
                (get-test-run-information))))
         (format ostream "~{~A~^@~}~%"
            (mapcar
               #'(lambda (key) (cdr (assoc key info :test #'eq)))
               '(:run-id :comment :platform :application :grammar
                 :avms :sorts :templates :lexicon :lrules :rules
                 :user :host :os :start))))))


(defun parse-tsdb-sentences2 (istream line parse-file result-file)
   (let ((environment (initialize-test-run))
         (progress-pos 0))
      ;; open and close output files for each sentence, so if run fails for some reason
      ;; we have all results in them until that point
      (unwind-protect
         (loop
            (multiple-value-bind (id string)
                  (parse-tsdb-sentence-read line)
               (when (eq id 'eof) (return))
               (setq progress-pos (parse-tsdb-sentences-progress id progress-pos))
               (let
                  ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                   (res (parse-item string)))
                  (with-open-file (ostream parse-file :direction :output
                                   :if-exists :append :if-does-not-exist :create)
                     (format ostream "~A@~A@~A@~{~A~^@~}~%" id 1 id
                        (mapcar
                           #'(lambda (key)
                               (if (eq key :date) (parse-tsdb-date-string)
                                  (cdr (assoc key res :test #'eq))))
                           '(:readings :first :total :tcpu :tgc :treal :words
                             :l-stasks :p-ctasks :p-ftasks :p-etasks :p-stasks
                             :aedges :pedges :redges :unifications :copies
                             :conses :symbols :others :gcs
                             :i-load :a-load :date :error))))
                  (with-open-file (ostream result-file :direction :output
                                   :if-exists :append :if-does-not-exist :create)
                     (dolist (sub (cdr (assoc :results res :test #'eq)))
                        (format ostream "~A@~{~A~^@~}~%" id
                           (mapcar
                              #'(lambda (key) (cdr (assoc key sub :test #'eq)))
                              '(:result-id :time :r-ctasks :r-ftasks :r-etasks
                                :r-stasks :size :r-redges :derivation :tree
                                :mrs)))))))
            (setq line (read-line istream nil 'eof)))
         (finalize-test-run environment))))


(defun get-tsdb-sentence (line)
  (multiple-value-bind (item sentence)
      (parse-tsdb-sentence-read line)
      (declare (ignore item))
      sentence))

(defun parse-tsdb-sentence-read (line)
   (flet ((position-nth (char str n)
            (dotimes (ind (length str))
               (cond
                  ((not (eql (char str ind) char)))
                  ((eql n 1) (return ind))
                  (t (decf n))))))
      (if (or (eq line 'eof)
         (every #'(lambda (c) (member c '(#\space #\tab))) line))
         'eof
         (let
            ((id (subseq line 0 (position #\@ line)))
             (raw-sentence
                (subseq line (1+ (position-nth #\@ line 6)) (position-nth #\@ line 7))))
            (values id raw-sentence)))))

(defun parse-tsdb-sentences-progress (id progress-pos)
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
         (incf progress-pos (length str))
         (when (> progress-pos 60)
            (terpri out) (setq progress-pos 0))
         progress-pos)))

(defun parse-tsdb-date-string nil
   (multiple-value-bind (sec min hour day mon year)
         (get-decoded-time)
      (format nil "~A-~A-~A ~2,'0D:~2,'0D:~2,'0D" day
         (aref #("jan" "feb" "mar" "apr" "may" "jun" "jul"
                 "aug" "sep" "oct" "nov" "dec") (1- mon))
         year hour min sec)))


