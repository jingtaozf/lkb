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

(defun parse-tsdb-sentences (&optional input-file parse-file result-file)
   (unless input-file 
      (setq input-file (ask-user-for-existing-pathname "Sentence file?")))
   (when input-file
      (unless parse-file 
         (setq parse-file (ask-user-for-new-pathname "Parse file?")))
      (when parse-file
         (unless result-file 
            (setq result-file (ask-user-for-new-pathname "Result file?")))
         (when result-file
            (when (probe-file parse-file) (delete-file parse-file))
            (when (probe-file result-file) (delete-file result-file))
            (let ((start-time (get-internal-run-time))
                  #+mcl (start-gc-time (gctime))
                  )
               (with-open-file (istream input-file :direction :input)
                  (parse-tsdb-sentences1 istream parse-file result-file))
               (format t "~%Total CPU time: ~A secs"
                  (truncate (- (get-internal-run-time) start-time)
                     internal-time-units-per-second))
               #+mcl
               (format t " (includes ~A secs GC)"
                  (truncate (- (gctime) start-gc-time)
                     internal-time-units-per-second))
               )))))


(defun parse-tsdb-sentences1 (istream parse-file result-file &aux (nsent 0))
   ;; open and close output files for each sentence, so if run fails we have all
   ;; results in them until that point
   (loop
      (multiple-value-bind (id words)
            (parse-tsdb-sentence-read (read-line istream nil 'eof))
         (when (eq id 'eof) (return))
         (incf nsent)
         ;; (when (eql (rem nsent 50) 1) (print nsent) (gc) (room))
         (if (> (length words) *chart-limit*)
            (format t "~%Sentence ~A too long" id)
            (let ((*safe-not-to-copy-p* t)
                  (*parse-unifs* 0)
                  (start (get-internal-run-time)))
               (clear-chart)
               (add-morphs-to-morphs words)
               (add-words-to-chart)
               (setf *parse-record*
                  (find-spanning-edges 0 (length words)))
;               (cache-structures)
               (uncache-lexical-entries) ; *** remove all cached lexical entries
               (let ((n 0)
                     (time
                        (truncate (* (- (get-internal-run-time) start) 10) ; tenths of secs
                           internal-time-units-per-second))
                     (*print-pretty* nil))
                  (with-open-file (ostream parse-file :direction :output
                                     :if-exists :append :if-does-not-exist :create)
                     (format ostream "~@{~A~^@~}~%"
                        id 1 id (length *parse-record*) time time -1 -1 -1 -1 -1 -1
                        -1 -1 *parse-unifs* -1 -1 -1 -1 -1 -1 -1
                        (multiple-value-bind (sec min hour day mon year)
                             (decode-universal-time (get-universal-time))
                           (format nil "~A-~A-~A ~A:~A:~A" day
                              (aref #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep"
                                      "oct" "nov" "dec") (1- mon)) year hour min sec))
                        ""))
                  (with-open-file (ostream result-file :direction :output
                                     :if-exists :append :if-does-not-exist :create)
                     (dolist (parse *parse-record*)
                        (format ostream "~A@~A@~A@~A@@@-1@-1@@~S@~%"
                           id n time *edge-id* (parse-tree-structure parse))
                        (setq time 0) ; zero time for all parses after first
                        (incf n)))))))))


(defun parse-tsdb-sentence-read (line)
   (if (or (eq line 'eof) (every #'whitespacep line))
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


;;; (dolist (parse *parse-record*) (pprint (parse-tree-structure parse)))

(defun parse-tree-structure (edge-record)
   (let ((daughters (edge-children edge-record)))
      (if daughters
         (cons (tree-node-text-string
                  (or (find-category-abb (edge-dag edge-record))
                      (edge-category edge-record)))
            (mapcar
               #'(lambda (daughter)
                   (if daughter
                      (parse-tree-structure daughter)
                      '||)) ; active chart edge daughter
               daughters))
         (if *dont-show-morphology*
            (car (edge-leaves edge-record))
            (cons (car (edge-leaves edge-record))
               (morph-tree-structure
                  (edge-rule-number edge-record) (edge-morph-history edge-record)))))))

(defun morph-tree-structure (rule edge-record)
   (if rule
      (cons rule
         (if edge-record
            (morph-tree-structure nil (edge-morph-history edge-record))))))


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


;;; (maphash #'(lambda (key val) (if (cdr val) (print (lex-or-psort-id (cdr val))))) *psorts*)
