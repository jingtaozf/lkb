;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Temporary emulation of part of tsdb functionality for lkb

#|
Abrams showed the office to Browne
zero parses found

result:
  parse-id :integer :key                # parse for this result
  result-id :integer                    # unique result identifier
  time :integer                         # time to find this result - tenths of seconds
  r-etasks :integer                     # parser executed tasks - total complete edges
  r-stasks :integer                     # parser succeeding tasks - edges used in successful parses
  r-ftasks :integer                     # parser filtered tasks - edges filtered out by rule filter
  size :integer                         # size of feature structure
  r-edges :integer                      # number of items for this reading
  derivation :string                    # derivation tree for this reading - labeled bracketing
                                          showing rule and lex instance names 
  tree :string                          # phrase structure tree (CSLI labels) - parse tree as
                                          labeled bracketing with parse node labels
  mrs :string                           # mrs for this reading

e.g
1@0@1@98@13@70@-1@-1@("subjh" 0 2 ("proper_le" 0 1 ("abrams" 0 1))  ("mv_unacc_le" 1 2 ("works" 0 1)))@("S" ("NP" (("abrams" "abrams" "no-affix")))  ("VP" (("works" "work" "third_sg_fin_verb"))))@

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
  p-etasks :integer                     # parser executed tasks
  p-stasks :integer                     # parser succeeding tasks
  p-ftasks :integer                     # parser filtered tasks
  words :integer                        # lexical items in parser input
  edges :integer                        # total items in chart
  unifications :integer                 # number of unification operations
  copies :integer                       # number of copy operations
  conses :integer                       # cons() cells allocated
  symbols :integer                      # symbols allocated
  others :integer                       # other bytes allocated
  gcs :integer                          # number of garbage collections
  i-load :integer                       # initial load (start of parse)
  a-load :integer                       # average load
  date :date                            # date and time of parse
  error :string                         # error string (if applicable |:-)

e.g.
18@1@18@1@1@5@7@0@9@372@42@234@6@42@40@780@227840@36@2583960@0@89@89@15-may-1998 13:55:23@
|#

;;; Parsing sentences from file
;;; (parse-tsdb-sentences "Macintosh HD:lkb99-expt:big:grammar:tsdb:csli:item" "Macintosh HD:lkb99-expt:big:parsenew" "Macintosh HD:lkb99-expt:big:resultnew")
;;; (parse-tsdb-sentences "Macintosh HD:lkb99-expt:big:item1" "Macintosh HD:lkb99-expt:big:parsenew1" "Macintosh HD:lkb99-expt:big:resultnew1")

(defun parse-tsdb-sentences (input-file parse-file result-file)
   (with-open-file (istream input-file :direction :input)
      (parse-tsdb-sentences1
         istream (read-line istream nil 'eof) parse-file result-file)))


(defun parse-tsdb-sentences1 (istream line parse-file result-file)
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
            (format t "~%~A: ~A -> ~A, ~A..." 'parse-tsdb-sentences
               (truename istream) parse-file result-file)
            (finish-output)
            (let ((start-time (get-internal-run-time))
                  #+mcl (start-gc-time (gctime))
                  )
               (parse-tsdb-sentences2 istream line parse-file result-file)
               (format t "~%~%Total CPU time: ~A secs"
                  (round (- (get-internal-run-time) start-time)
                     internal-time-units-per-second))
               #+mcl
               (format t " (includes ~A secs GC)"
                  (round (- (gctime) start-gc-time)
                     internal-time-units-per-second))
               )))))


(proclaim '(special *do-something-with-parse*))

(defun parse-tsdb-sentences2 (istream line parse-file result-file &aux (nsent 0))
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
               (uncache-lexical-entries))
            (format t " ~A" id) (finish-output)
            (if (> (length words) *chart-limit*)
               (error "Sentence ~A too long" id)
               (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                      (str (make-string-output-stream)) ; capture any warning messages
                      (*standard-output* (make-broadcast-stream *standard-output* str))
                      (real (get-internal-real-time))
                      (run (get-internal-run-time))
                      (gcs #+mcl (ccl:gccounts) #-mcl 0)
                      (bytes #+mcl (ccl::total-bytes-allocated) #-mcl 0))
                  (multiple-value-bind (unifs fails)
                        (parse-tsdb-sentence words)
                     (declare (ignore fails))
                     (let* ((bytes #+mcl (- (ccl::total-bytes-allocated) bytes) #-mcl -1)
                            (gcs #+mcl (- (ccl:gccounts) gcs) #-mcl -1)
                            (run
                               (round (* (- (get-internal-run-time) run) 10)
                                  internal-time-units-per-second))
                            (real ; in tenths of secs
                               (round (* (- (get-internal-real-time) real) 10)
                                  internal-time-units-per-second))
                            (msgs
                               (substitute #\; #\newline
                                  (string-trim '(#\newline #\space) (get-output-stream-string str))))
                            (n 0))
                        (with-open-file (ostream parse-file :direction :output
                                           :if-exists :append :if-does-not-exist :create)
                           (format ostream "~@{~A~^@~}~%"
                              id 1 id (length *parse-record*)
                              run run run 0 real -1 -1 -1
                              (reduce #'+ *morphs* :key
                                 #'(lambda (x)
                                     (if (morph-edge-p x) (length (morph-edge-morph-results x)) 0)))
                              *edge-id* unifs -1 -1 -1 bytes gcs -1 -1
                              (multiple-value-bind (sec min hour day mon year)
                                   (decode-universal-time (get-universal-time))
                                 (format nil "~A-~A-~A ~2,'0D:~2,'0D:~2,'0D" day
                                    (aref #("jan" "feb" "mar" "apr" "may" "jun" "jul"
                                            "aug" "sep" "oct" "nov" "dec") (1- mon))
                                    year hour min sec))
                              msgs))
                        (with-open-file (ostream result-file :direction :output
                                           :if-exists :append :if-does-not-exist :create)
                           (dolist (parse *parse-record*)
                              (format ostream "~@{~A@~}"
                                 id n run *edge-id* -1 -1 -1 -1)
                              (format ostream "~A@~S@~A~%"
                                 "" (parse-tree-structure parse) "")
                              (setq run 0) ; zero time for all parses after first
                              (incf n))))))))
         (setq line (read-line istream nil 'eof)))
     (uncache-lexical-entries)))


(defun parse-tsdb-sentence (user-input)
   (let ((*safe-not-to-copy-p* t)
         (*parse-unifs* 0) (*parse-fails* 0))
        (clear-chart)
        (add-morphs-to-morphs user-input)
        (add-words-to-chart)
        (setf *parse-record*
        (find-spanning-edges 0 (length user-input)))
        (when (fboundp *do-something-with-parse*)
                 (funcall *do-something-with-parse*))
        (values *parse-unifs* *parse-fails*)))


(defun parse-tsdb-sentence-read (line)
   (if (or (eq line 'eof)
          (every #'(lambda (c) (member c '(#\space #\tab))) line))
      'eof
      (let
         ((id (subseq line 0 (position #\@ line)))
          (raw-sentence
             (subseq line (1+ (position-nth #\@ line 6)) (position-nth #\@ line 7))))
         (when (member (char raw-sentence (1- (length raw-sentence))) '(#\. #\! #\?))
            (setq raw-sentence (subseq raw-sentence 0 (1- (length raw-sentence)))))
         (values id
            (split-into-words (preprocess-sentence-string raw-sentence))))))

(defun position-nth (char str n)
   (dotimes (ind (length str))
      (cond
         ((not (eql (char str ind) char)))
         ((eql n 1) (return ind))
         (t (decf n)))))


;;;

(defun uncache-lexical-entries nil
   (let ((psorts nil))
      (dotimes (n (length *morphs*))
         (when (aref *morphs* n)
            (dolist (deriv (morph-edge-morph-results (aref *morphs* n)))
               (dolist (psort (gethash (string-upcase (car deriv)) *lexical-entries*))
                  (pushnew psort psorts)))))
      (dolist (psort psorts)
         (uncache-psort-entry psort))))


;;; (maphash #'(lambda (key val) (if (cddr val) (print (lex-or-psort-id (cddr val))))) *psorts*)
