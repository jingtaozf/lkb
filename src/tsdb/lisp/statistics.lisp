;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: statistics.lisp
;;;      module: basic report generation routines for tsdb(1) profiles
;;;     version: 0.0 (experimental)
;;;  written by: oe, coli saarbruecken
;;; last update: 17-dec-97
;;;  updated by: oe, coli saarbruecken
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an attempt to collect a number of routines for report generation and
;;; profile analysis.  pretty hacky and incomplete at this point (17-dec-97).
;;;
;;; some examples of how i use some of the functions:
;;;
;;;   - compute coverage summary of profile stored in
;;;     `oct-97/english/25-nov-97' (relative to *tsdb-home*) and write latex(1)
;;;     code to `bar.tex' (relative to the current working directory of lisp
;;;     process; use `:cd' to change):
;;;
;;;     (analyze-competence "oct-97/english/25-nov-97" :file "/tmp/bar.tex")
;;;
;;;   - overgeneration summary for same profile (data is cached in the lisp
;;;     universe; hence the first access to a database is _slow_ but things
;;;     get much better once the data is cached):
;;;
;;;     (analyze-competence "oct-97/english/25-nov-97" 
;;;                         :wf 0 :file "/tmp/bar.tex")
;;;
;;;   - performance profile for items that had at least one reading (use the
;;;     optional :restrictor argument to specifiy a condition on items to
;;;     exclude):
;;;
;;;     (analyze-performance "oct-97/english/25-nov-97"
;;;                          :restrictor #'(lambda (foo)
;;;                                          (< (get-field :readings foo) 1))
;;;                          :file "/tmp/bar.tex")
;;;
;;;   - aggregate data on the basis of some property other than the phenomena
;;;     classification; e.g. by length:
;;;
;;;     (aggregate "oct-97/english/25-nov-97"
;;;                :dimension :i-length :aggregate 3 :upper 8)
;;;
;;;     the output of aggregate() can then be passed to the analysis functions
;;;     (see above) instead of the database (string) argument.
;;;
;;;   - compare two profiles performance-wise; set labels for the three major
;;;     columns:
;;;
;;;     (compare-performance "oct-97/english/25-nov-97"
;;;                          "oct-97/english/l+pfilter.15-dec-97"
;;;                          :olabel "oct-97" :nlabel "oct-97 (filter)"
;;;                          :clabel "improvement"
;;;                          :file "/tmp/bar.tex")
;;;
;;; besides, i usually keep a matrix latex(1) file that contains a plain
;;; document header plus an \include for `bar.tex' and an xdvi(1) for the
;;; matrix file around; thus, it only takes two command (one in the lisp world,
;;; one from the shell) to view a new profile view.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defvar *tsdb-profile-cache* (make-hash-table :size 42 :test #'equal))

(defparameter *statistics-select-condition* nil)

(defparameter *statistics-aggregate-dimension* :phenomena)

(defparameter *statistics-aggregate-size* 1)

(defparameter *statistics-aggregate-threshold* 1)

(defparameter *statistics-aggregate-lower* nil)

(defparameter *statistics-aggregate-upper* nil)

(defparameter *statistics-aggregate-maximum* 
  (min 20000 array-total-size-limit))

(defparameter *statistics-plot-width* 150)

(defparameter *statistics-plot-height* 100)

(defparameter *statistics-tcl-formats*
  "format title -font {helvetica 12 bold} -fill black -justify center~%~
   format aggregate -font {helvetica 12 bold} -fill black -justify center~%~
   format data -font {helvetica 12} -fill black -justify center~%~
   format total -font {helvetica 12 bold} -fill black -justify center~%~%")

(defparameter *phenomena*
  (list "S_Types"
        "C_Types"
        "C_Agreement"
        "C_Complementation"
        "C_Diathesis-Active"
        "C_Diathesis-Passive"
        "C_Tense-Aspect-Modality"
        "C_Negation"
        "C_Coordination"
        "C_Modification"
        "NP_Agreement"
        "NP_Modification"
        "NP_Coordination"))

(defparameter *statistics-predicates* (make-hash-table))

(defparameter *statistics-readers* (make-hash-table))

(defun latexify-string (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\_)
        (concatenate 'string "\\_" (latexify-string (subseq string 1)))
        (concatenate 
            'string (string prefix) (latexify-string (subseq string 1)))))
    string))

(defun find-attribute-predicate (attribute)
  (let* ((name (if (stringp attribute) (string-upcase attribute) attribute))
         (attribute (intern name :keyword)))
    (or (gethash attribute *statistics-predicates*) 
        #'(lambda (old new) (not (equal old new))))))

(defun find-attribute-reader (attribute)
  (let* ((name (if (stringp attribute) (string-upcase attribute) attribute))
         (attribute (intern name :keyword)))
    (gethash attribute *statistics-readers*) ))

(defun find-attribute-label (attribute)
  (case attribute
    (:i-length "string length")
    (:readings "parser analyses")
    (:first "first reading")
    (:total "all readings")
    (:tcpu "total cpu time")
    (:tgc "gc time")
    (:treal "total real time")
    (:words "lexical entries")
    (:l-stasks "lexical rule successes")
    (:p-ctasks "contemplated tasks")
    (:p-ftasks "filtered tasks")
    (:p-etasks "executed tasks")
    (:p-stasks "successful tasks")
    (:aedges "active edges")
    (:pedges "passive edges")
    (:raedges "active edges in result(s)")
    (:rpedges "passive edges in result(s)")
    (:space "total bytes allocated")
    (:gcs "global garbage collections")))

(defun find-attribute-symbol (attribute &optional index &key format)
  (let* ((symbols (case format
                    (:tcl 
                     '("circle" "diamond" "square" "triangle" 
                       "plus" "cross" "scross"))
                    (:latex
                     '("$\\bullet$" "$\\diamond$" "$\\star$" 
                       "$\\circ$" "$\\ast$"))))
         (length (- (length symbols) 1)))
    (or (case format
          (:tcl
           (case attribute
             (:first "circle")
             (:total "diamond")
             (:p-ftasks "square")
             (:p-etasks "diamond")
             (:p-stasks "circle")))
          (:latex
           (case attribute
             (:first "$\\bullet$")
             (:total "$\\diamond$")
             (:p-ftasks "$\\star$")
             (:p-etasks "$\\diamond$")
             (:p-stasks "$\\bullet$"))))
        (nth (if index (mod index length) (random length)) symbols))))

(defun find-attribute-colour (attribute &optional index)
  (declare (ignore index))
  (or (case attribute
        (:first "green")
        (:total "blue")
        (:p-ftasks "red")
        (:p-etasks "orange")
        (:p-stasks "green"))
      "black"))

(defun profile-granularity (data)
  (let* ((relations (read-database-schema data))
         (run (and relations
                   (find "run" relations :key #'first :test #'string=)))
         (parse (and relations
                   (find "parse" relations :key #'first :test #'string=))))
    (cond 
     ((null run) :historic)
     ((not (find "aedges" (rest parse) :key #'first :test #'string=)) 0)
     ((find "environment" (rest run) :key #'first :test #'string=) 9903)
     ((find "end" (rest run) :key #'first :test #'string=) 9902)
     (t 9808))))

(defun analyze (language &key condition meter message thorough)

  (let* ((message (when message
                    (format nil "retrieving `~a' data ..." language)))
         (key (format 
               nil 
               "~a @ ~a~@[ # ~{~(~a~)~^#~}~]" 
               language condition thorough))
         (relations (read-database-schema language))
         (parse (rest (find "parse" relations :key #'first :test #'string=)))
         pfields ptypes data)
    (when meter
      (when message (status :text message))
      (meter :value (get-field :start meter)))
    (loop while (eq (setf data (gethash key *tsdb-profile-cache*)) :seized))
    (unless data
      (setf (gethash key *tsdb-profile-cache*) :seized)
      (do* ((fields '("i-id" "parse-id" "readings" 
                      "first" "total" "tcpu" "tgc"
                      "p-etasks" "p-stasks" "p-ftasks"
                      "unifications" "copies"
                      "conses" "symbols" "others"
                      "words" "l-stasks"
                      "edges" "aedges" "pedges" "raedges" "rpedges"
                      "gcs" "error")
                    (rest fields))
            (field (first fields) (first fields)))
          ((null fields))
        (let ((match (find field parse :key #'first :test #'string=)))
          (when match
            (push (first match) pfields)
            (push (second match) ptypes))))
      (unwind-protect
        (let* ((pmeter (and meter (madjust * meter (if  0.4 0.5))))
               (imeter (when meter
                         (madjust + (madjust * meter (if thorough 0.1 0.25)) 
                                  (mduration pmeter))))
               (rmeter (if thorough
                         (madjust + (madjust * meter 0.4) 
                                  (+ (mduration pmeter) (mduration imeter)))
                         (and meter (make-meter 0 0))))
               (ameter (when meter
                         (madjust + (madjust * meter (if thorough 0.1 0.25)) 
                                  (+ (mduration pmeter) 
                                     (mduration rmeter)
                                     (mduration imeter)))))
               (parse (select pfields ptypes "parse" condition language
                              :meter pmeter :sort :i-id))
               (item (select '("i-id" "i-input" "i-length" "i-wf")
                             '(:integer :string :integer :integer)
                             "item" condition language 
                             :meter imeter :sort :i-id))
               (results (when thorough
                          (select (append '("parse-id" "result-id") 
                                          (loop for symbol in thorough
                                              collect (format 
                                                       nil 
                                                       "~(~a~)" 
                                                       symbol)))
                                  nil "result" condition language 
                                  :meter rmeter :sort :parse-id)))
               (all (njoin parse item :i-id :meter ameter)))
          (setf data all)
          (when results
            (loop
                for field in thorough
                for reader = (find-attribute-reader field)
                when reader
                do
                  (loop
                      for result in results
                      for value = (get-field field result)
                      when value
                      do 
                        (setf (get-field field result) 
                          (funcall reader value))))
            ;;
            ;; _fix_me_
            ;; this is sort of hacky: since we fail to guarantee unique parse
            ;; ids, the corresponding run id would have to be included in the
            ;; `result' relation; as it stands, this is not the case |:-(.
            ;; until we get his fixed, it hard-wires the assumption that we
            ;; will not use the same profile to represent multiple test runs.
            ;;                                          (10-mar-99  -  oe)
            (loop
                with all = (copy-list all)
                for item in (sort all #'< :key #'(lambda (foo)
                                                   (get-field :parse-id foo)))
                for key = (get-field :parse-id item)
                for matches =
                  (when (eql key (get-field :parse-id (first results)))
                    (loop
                        for result = (first results)
                        while (and result 
                                   (eql key (get-field :parse-id result)))
                        collect (pop results)))
                when matches
                do (nconc item (acons :results matches nil)))))
                  
        (setf (gethash key *tsdb-profile-cache*) data)))
    (when meter 
      (meter :value (get-field :end meter))
      (when message 
        (status :text (format nil "~a done" message) :duration 2)))
    data))

(defun analyze-aggregates (language
                           &key condition
                                phenomena
                                (dimension *statistics-aggregate-dimension*)
                                (format :latex) meter)

  (if (not (eq dimension :phenomena))
    (aggregate language 
               :condition condition :dimension dimension 
               :format format :meter meter)
    (let* ((phenomena (or phenomena
                          (gethash language *tsdb-phenomena*)
                          *phenomena*))
           (key (format nil "~a # phenomena" language))
           (imeter (madjust * meter (if phenomena 0.3 0.5)))
           (pmeter (madjust + (madjust * meter 0.5) (mduration imeter)))
           (increment (when meter
                        (* (mduration meter)
                           (/ (if phenomena 0.2 0.5) (length phenomena)))))
           (message (format nil "retrieving `~a' data ..." language))
           pdata items)
      (when meter
        (status :text message)
        (meter :value (get-field :start meter)))
      (let* ((idata (analyze language :condition condition :meter imeter)))
        (loop while (eq (setf pdata (gethash key *tsdb-profile-cache*)) 
                        :seized))
        (unless pdata
          (setf (gethash key *tsdb-profile-cache*) :seized)
          (unwind-protect
              (setf pdata
                (select '("i-id" "p-name")
                        '(:integer :string)
                        '("item-phenomenon" "phenomenon")
                        nil language :meter pmeter :sort :i-id))
            (setf (gethash key *tsdb-profile-cache*) pdata)))
        (when meter (meter :value (get-field :end pmeter)))
        (if (and phenomena pdata)
          (do* ((data (njoin idata pdata :i-id) data)
                (phenomena (reverse phenomena) (rest phenomena))
                (phenomenon (first phenomena) (first phenomena)))
              ((null phenomena))
            (let* ((plength (length phenomenon))
                   (pitems 
                    (remove-if-not 
                     #'(lambda (item)
                         (let* ((p-name (get-field :p-name item)))
                           (when p-name
                             (string= phenomenon p-name
                                      :end2 (min (length p-name) plength)))))
                     data)))
              (when pitems
                (push (cons (intern (string-upcase  phenomenon) "KEYWORD")
                            (cons phenomenon pitems))
                      items)))
            (when meter (meter-advance increment)))
          (push (cons :all (cons "All" idata)) items)))
      (when meter
        (meter :value (get-field :end meter))
        (status :text (format nil "~a done" message :duration 2)))
      items)))

(defun analyze-rules (language &key condition (format :tcl) meter)

  (let* ((key (format nil "~a ~@[~a ~]# rules" language condition))
         (data (gethash key *tsdb-profile-cache*)))
    (unless data
      (setf (gethash key *tsdb-profile-cache*) :seized)
      (unwind-protect
        (let* ((rules (select '("parse-id" "rule" 
                                "filtered" "executed" "successes"
                                "actives" "passives")
                              '(:integer :string 
                                :integer :integer :integer
                                :integer :integer) 
                              "rule"
                              condition language 
                              :meter meter :status meter))
               (counts (make-hash-table))
               foo)
          (loop 
              for rule in rules
              for name = (intern (string-upcase (get-field :rule rule)))
              for counter = (gethash name counts)
              when counter
              do
                (incf (get-field :filtered counter)
                      (get-field :filtered rule))
                (incf (get-field :executed counter)
                      (get-field :executed rule))
                (incf (get-field :successes counter)
                      (get-field :successes rule))
                (incf (get-field :actives counter)
                      (get-field :actives rule))
                (incf (get-field :passives counter)
                      (get-field :passives rule))
              else do
                (setf (gethash name counts)
                  (list (find :filtered rule :key #'first)
                        (find :executed rule :key #'first)
                        (find :successes rule :key #'first)
                        (find :actives rule :key #'first)
                        (find :passives rule :key #'first))))
          (maphash
           #'(lambda (key value)
               (let ((name (case format
                             (:tcl (string key))
                             (:latex (latexify-string (string key)))
                             (t (string key)))))
                 (push (cons key (cons name value)) foo)))
           counts)
          (setf data foo))
        (setf (gethash key *tsdb-profile-cache*) data)))
    data))

(defun load-cache (&key (home *tsdb-home*) name pattern trace meter background)
  (if #+allegro background #-allegro nil
    #+allegro
    (mp:process-run-function 
     (list :name "tsdb(1) cache loader")
     #'load-cache
     :home home :name name :pattern pattern 
     :trace trace :meter meter :background nil)
    (let ((dbs (sort (find-tsdb-directories 
                      home 
                      :name name :pattern pattern 
                      :trace trace :meter meter) 
                     #'string< :key #'(lambda (foo) (get-field :path foo)))))
      (dolist (db dbs)
        (let* ((name (get-field :database db)))
          (when trace
            (format
             *tsdb-io*
             "~&load-cache(): processing `~a';~%"
             name)
            (force-output *tsdb-io*))
          (analyze name)
          (analyze-aggregates name))))))

(defun purge-profile-cache (data)
  (let ((end (unless (eq data :all) (length data))))
    (maphash #'(lambda (key foo)
                 (declare (ignore foo))
                 (when (or (eq data :all) 
                           (search data key :end2 (min end (length key))))
                   (remhash key *tsdb-profile-cache*)))
             *tsdb-profile-cache*)))

(defun aggregate (&optional (language *tsdb-data*)
                  &key (condition nil)
                       (restrictor nil)
                       (dimension :i-length)
                       (aggregate (or *statistics-aggregate-size* 2))
                       (threshold (or *statistics-aggregate-threshold* 1))
                       (lower (or *statistics-aggregate-lower* 0))
                       (upper *statistics-aggregate-upper*)
                       (format :latex)
                       meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((imeter (madjust * meter 0.9))
         (items (if (stringp language) 
                  (analyze language :condition condition
                           :meter imeter :message (and meter t)) 
                  language))
         (items (if (eq dimension :space)
                  (loop 
                      with format = (profile-granularity language)
                      for item in items
                      for conses = (get-field :conses item)
                      for symbols = (get-field :symbols item)
                      for others = (get-field :others item)
                      for space = (when (and conses symbols others
                                             (>= conses 0)
                                             (>= symbols 0)
                                             (>= others 0))
                                    (if (>= format 9903)
                                      (+ conses symbols others)
                                      (+ (* conses 8) (* symbols 24) others)))
                      when (>= space lower)
                      collect (cons (cons :space space) item))
                  (remove-if #'(lambda (foo)
                                 (< (get-field dimension foo) lower))
                             items)))
         (items (if upper
                  (remove-if #'(lambda (foo)
                                 (> (get-field dimension foo) upper))
                             items)
                  items))
         (restrictor (case restrictor
                       ((:wf :well-formed :grammatical :positive)
                        #'(lambda (foo) (not (equal (get-field :i-wf foo) 1))))
                       ((:if :ill-formed :ungrammatical :negative)
                        #'(lambda (foo) (not (equal (get-field :i-wf foo) 0))))
                       ((:parses :accepts :analyses)
                        #'(lambda (foo)
                            (not (> (get-field :readings foo) 0))))
                       (:rejects
                        #'(lambda (foo)
                            (not (equal (get-field :readings foo) 0))))
                       (t restrictor)))
         (items (if (and restrictor (functionp restrictor))
                  (remove-if restrictor items)
                  items))
         (values (map 'list #'(lambda (foo) (get-field dimension foo)) items))
         (message (format 
                   nil
                   "aggregation [width: ~d; size ~d~
                    ~@[; lower ~d~]~@[; upper ~d~]] for `~(~a~)' ..."
                   aggregate threshold lower upper dimension)))
    (when meter (status :text message))
    (when values
      (let* ((minimum (apply #'min values))
             (aminimum (floor (/ minimum aggregate)))
             (maximum (apply #'max values))
             (amaximum (floor (/ maximum aggregate)))
             (width (+ (- amaximum aminimum) 1)))
        (when (> width *statistics-aggregate-maximum*)
          (let* ((base (/ width *statistics-aggregate-maximum*))
                 (precision (expt 10 (floor (log base 10))))
                 (base (* precision (ceiling base precision))))
            (setf aggregate base
                  aminimum (floor (/ minimum aggregate))
                  amaximum (floor (/ maximum aggregate))
                  width (+ (- amaximum aminimum) 1)))
          (when meter
            (beep)
            (status 
             :text (format
                    nil
                    "invalid (too small) aggregate width; ~
                     using ~d instead"
                    aggregate)
             :duration 10)
            (sleep 2)))
        (let ((storage (make-array width :initial-element nil))
              result)
          (dolist (item items)
            (let* ((value (get-field dimension item))
                   (class (- (floor (/ value aggregate)) aminimum)))
              (push item (aref storage class))))
          (dotimes (i (+ (- amaximum aminimum) 1) result)
            (let* ((data (aref storage i))
                   (class (* (+ i aminimum) aggregate))
                   (name (case format
                           (:latex
                            (if (= aggregate 1)
                              (format
                               nil
                               "\\multicolumn{1}{|c|} {\\em ~(~a~)\\/ $=$ ~d}"
                               dimension class)
                              (format 
                               nil 
                               "\\multicolumn{1}{|c|}~
                                {~d $\\leq$ {\\em ~(~a~)\\/} $<$ ~d}"
                               class dimension (+ class aggregate))))
                           (:tcl
                            (if (= aggregate 1)
                              (format
                               nil
                               "~(~a~) = ~d"
                               dimension class)
                              (format
                               nil
                               "~(~a~) in [~d .. ~d|"
                               dimension class (+ class aggregate)))))))
              (when (>= (length data) threshold)
                (push (cons class (cons name data)) result))))
          (when meter 
            (meter :value (get-field :end meter))
            (status :text (format nil "~a done" message) :duration 5))
          result)))))


(defun summarize-competence-parameters (items
                                        &key restrictor)

  (let ((itemtotal 0)
        (restrictedtotal 0)
        (lengthtotal 0)
        (wordstotal 0)
        (lstaskstotal 0)
        (parsestotal 0)
        (readingstotal 0)
        result)
    (dolist (phenomenon items)
      (let* ((data (rest (rest phenomenon)))
             (items (length data))
             (data (if restrictor
                     (remove-if restrictor data)
                     data))
             (restricted (length data))
             (lengths (map 'list #'(lambda (foo)
                                    (get-field :i-length foo))
                           data))
             (wordss (map 'list #'(lambda (foo)
                                    (get-field :words foo))
                          data))
             (lstaskss (map 'list #'(lambda (foo)
                                      (get-field+ :l-stasks foo -1))
                            data))
             (parses (remove-if-not #'(lambda (foo)
                                    (>= (get-field :readings foo) 1))
                                    data))
             (readingss (map 'list #'(lambda (foo)
                                       (get-field+ :readings foo -1))
                             parses)))
        (push (cons (first phenomenon)
                    (pairlis '(:items :restricted 
                               :i-length :words 
                               :l-stasks 
                               :lambiguity 
                               :analyses :results)
                             (list items restricted 
                                   (average lengths) (average wordss) 
                                   (average lstaskss)
                                   (divide (sum wordss) (sum lengths))
                                   (average readingss) (length parses))))
              result)
        (incf itemtotal items)
        (incf restrictedtotal restricted)
        (incf lengthtotal (sum lengths))
        (incf wordstotal (sum wordss))
        (incf lstaskstotal (sum lstaskss))
        (incf parsestotal (length parses))
        (incf readingstotal (sum readingss))))
    (cons (cons :total
                (pairlis '(:items :restricted
                           :i-length
                           :words :l-stasks
                           :lambiguity
                           :analyses
                           :results)
                         (list itemtotal restrictedtotal
                               (divide lengthtotal restrictedtotal)
                               (divide wordstotal restrictedtotal)
                               (divide lstaskstotal restrictedtotal)
                               (divide wordstotal lengthtotal)
                               (divide readingstotal parsestotal)
                               parsestotal)))
          (delete :all result :key #'first))))

(defun analyze-competence (&optional (language *tsdb-data*)
                           &key (condition *statistics-select-condition*)
                                (wf 1) file append (format :latex)
                                restrictor meter)
  (declare (ignore restrictor))

  (let* ((items (if (stringp language) 
                  (analyze-aggregates language :condition condition
                                      :meter meter :format format) 
                  language))
         (stream (create-output-stream file append))
         (averages 
          (summarize-competence-parameters 
           items :restrictor #'(lambda (foo) 
                                 (not (= (get-field :i-wf foo) wf)))))
         (naggregates (- (length averages) 1))
         (ncolumns 8)
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a -- (c) oe@coli.uni-sb.de)"
                   *tsdb-name* (current-time :long :pretty))))
                   
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{~d}{|c|}~%    {\\bf `~a' ~a Profile}\\\\~%  ~
          \\hline\\hline~%  ~
          & {\\bf  total} & {\\bf ~a} & {\\bf word} & {\\bf lexical}~%    ~
            & {\\bf parser} & {\\bf total} & {\\bf overall}\\\\~%  ~
          {\\bf ~a} & {\\bf items} & {\\bf items} & {\\bf string}~%    ~
            & {\\bf items} & {\\bf analyses} & {\\bf results}~%    ~
            & {\\bf coverage}\\\\~%  ~
          & $\\sharp$ & $\\sharp$ & $\\phi$ & $\\phi$ & $\\phi$~%    ~
            & $\\sharp$ & $\\%$\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         ncolumns
         (if (stringp language) language "")
         (if (= wf 1) "Coverage" "Overgeneration")
         (if (= wf 1) "positive" "negative")
         alabel))
      (:tcl
       (format stream *statistics-tcl-formats*)
       (format
        stream
        "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 8 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
        (+ naggregates 1) (+ naggregates 2))
       (format
        stream
        "cell 1 1 -contents ~a -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"~a\\nitems\\n#\" -format title~%~
         cell 1 4 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 5 -contents \"lexical\\nitems\\n\\330\" -format title~%~
         cell 1 6 -contents \"parser\\nanalyses\\n\\330\" -format title~%~
         cell 1 7 -contents \"total\\nresults\\n#\" -format title~%~
         cell 1 8 -contents \"overall\\ncoverage\\n%\" -format title~%~%"
        alabel (if (= wf 1) "positive" "negative"))))
    (do* ((items items (rest items))
          (i 2 (1+ i))
          (phenomenon (first items) (first items)))
        ((null items))
      (let* ((data (rest (assoc (first phenomenon) averages))))
        (when data
          (let* ((name (if (equal format :latex)
                         (latexify-string (second phenomenon))
                         (second phenomenon)))
                 (items (get-field :items data))
                 (restricted (get-field :restricted data))
                 (length (get-field :i-length data))
                 (words (get-field :words data))
                 (analyses (get-field :analyses data))
                 (results (get-field :results data))
                 (coverage (if (zerop restricted)
                             100
                             (float (* 100 (/ results restricted))))))
            (case format
              (:latex
                (format
                 stream
                 "  ~a & ~d & ~d & ~,2f & ~,2f & ~,2f & ~d & ~,1f\\\\~%"
                 name items restricted length words analyses results coverage))
              (:tcl
               (format
                stream
                "cell ~d 1 -contents \"~a\" -format aggregate~%~
                 cell ~d 2 -contents ~d -format data~%~
                 cell ~d 3 -contents ~d -format data~%~
                 cell ~d 4 -contents ~,2f -format data~%~
                 cell ~d 5 -contents ~,2f -format data~%~
                 cell ~d 6 -contents ~,2f -format data~%~
                 cell ~d 7 -contents ~d -format data~%~
                 cell ~d 8 -contents ~,1f -format data~%~%"
                i name
                i items
                i restricted
                i length
                i words
                i analyses
                i results
                i coverage)))))))
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (restricted (get-field :restricted data))
           (length (get-field :i-length data))
           (words (get-field :words data))
           (analyses (get-field :analyses data))
           (results (get-field :results data))
           (coverage (if (zerop restricted)
                       100
                       (float (* 100 (/ results restricted))))))
      (case format
        (:latex
          (format
           stream
           "  \\hline~%  \\hline~%  ~
            {\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~,2f}~%    ~
            & {\\bf ~,2f} & {\\bf ~d} & {\\bf ~,1f}\\\\~%  \\hline~%"
           name items restricted length words analyses results coverage)
          (format
           stream
           "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
            \\end{tabular}~%"
           ncolumns caption))
        (:tcl
         (format
            stream
            "cell ~d 1 -contents \"~a\" -format total~%~
             cell ~d 2 -contents ~d -format total~%~
             cell ~d 3 -contents ~d -format total~%~
             cell ~d 4 -contents ~,2f -format total~%~
             cell ~d 5 -contents ~,2f -format total~%~
             cell ~d 6 -contents ~,2f -format total~%~
             cell ~d 7 -contents ~d -format total~%~
             cell ~d 8 -contents ~,1f -format total~%~%"
            (+ naggregates 2) name
            (+ naggregates 2) items
            (+ naggregates 2) restricted
            (+ naggregates 2) length
            (+ naggregates 2) words
            (+ naggregates 2) analyses
            (+ naggregates 2) results
            (+ naggregates 2) coverage))))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun compare-competence (olanguage nlanguage
                           &key (condition *statistics-select-condition*)
                                (olabel "(g)old") (nlabel "new")
                                (format :latex)
                                file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (oitems
          (if (stringp olanguage) 
            (analyze-aggregates olanguage :condition condition
                                :meter ometer :format format)
            olanguage))
         (nitems
          (if (stringp nlanguage) 
            (analyze-aggregates nlanguage :condition condition
                                :meter nmeter :format format)
            nlanguage))
         (ncolumns 9)
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a -- (c) oe@coli.uni-sb.de)"
                   *tsdb-name* (current-time :long :pretty))))
    
    (cond 
     ((or (null oitems) (null nitems)) 1)
     ((not (= (length oitems) (length nitems))) 2)
     (t
      (let* ((stream (create-output-stream file append))
             (oaverages (summarize-competence-parameters oitems))
             (naggregates (- (length oaverages) 1))
             (owfaverages 
              (summarize-competence-parameters 
               oitems :restrictor #'(lambda (foo) 
                                      (not (= (get-field :i-wf foo) 1)))))
             (oifaverages 
              (summarize-competence-parameters 
               oitems :restrictor #'(lambda (foo) 
                                      (not (= (get-field :i-wf foo) 0)))))
             (naverages (summarize-competence-parameters nitems))
             (nwfaverages 
              (summarize-competence-parameters 
               nitems :restrictor #'(lambda (foo) 
                                      (not (= (get-field :i-wf foo) 1)))))
             (nifaverages 
              (summarize-competence-parameters 
               nitems :restrictor #'(lambda (foo) 
                                      (not (= (get-field :i-wf foo) 0)))))
             (*print-circle* nil))
       (case format
         (:latex
          (format
           stream
           "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|c|@{}}~%  ~
            \\hline~%  ~
            & \\multicolumn{4}{|c|}{\\bf ~a}~%    ~
            & \\multicolumn{4}{|c|}{\\bf ~a}\\\\~%  ~
            {\\bf ~a} ~
            & {\\bf lexical} & {\\bf parser} ~
            & {\\bf in} & {\\bf out}~%    ~
            & {\\bf lexical} & {\\bf parser} ~
            & {\\bf in} & {\\bf out}\\\\~%  ~
            & $\\phi$ & $\\phi$ & \\% & \\%~%   ~
            & $\\phi$ & $\\phi$ & \\% & \\%\\\\~%  ~
            \\hline~%  ~
            \\hline~%"
           olabel nlabel alabel))
         (:tcl
          (when *statistics-tcl-formats* 
            (format stream *statistics-tcl-formats*))
          (format
           stream
           "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
            layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
            layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 5 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 9 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 10 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ naggregates 2) (+ naggregates 3))
          (format
           stream
           "cell 1 1 -contents ~a -format title~%~
            region 1 1 2 1 -contents ~a -format title ~
              -hor_justify left -ver_justify center~%~
            region 1 2 1 5 -contents ~s -format title -hor_justify center~%~
            region 1 6 1 9 -contents ~s -format title -hor_justify center~%"
           alabel alabel olabel nlabel)
          (do ((labels '("lexical\\n\\330" "parser\\n\\330" 
                         "in\\n\\330" "out\\n\\330"
                         "lexical\\n\\330" "parser\\n\\330" 
                         "in\\n\\330" "out\\n\\330")
                        (rest labels))
               (i 2 (+ i 1)))
              ((null labels))
            (format
             stream
             "cell 2 ~d -contents \"~a\" -format title~%~
              region 2 ~d 2 ~d -contents \"~a\" -format title ~
                -hor_justify center~%"
             i (first labels) i i (first labels)))))
       (do* ((oitems (remove :all oitems :key #'first) (rest oitems))
             (i 3 (+ i 1)))
           ((null oitems))
         (let* ((phenomenon (first oitems))
                (odata (rest (assoc (first phenomenon) oaverages)))
                (owfdata (rest (assoc (first phenomenon) owfaverages)))
                (oifdata (rest (assoc (first phenomenon) oifaverages)))
                (ndata (rest (assoc (first phenomenon) naverages)))
                (nwfdata (rest (assoc (first phenomenon) nwfaverages)))
                (nifdata (rest (assoc (first phenomenon) nifaverages)))
                (name (if (eq format :latex)
                        (latexify-string (second phenomenon))
                        (second phenomenon)))
                (owords (get-field :lambiguity odata))
                (oanalyses (get-field :analyses odata))
                (owfrestricted (get-field :restricted owfdata))
                (owfresults (get-field :results owfdata))
                (owfcoverage (if (zerop owfrestricted)
                               100
                               (float (* 100 (/ owfresults owfrestricted)))))
                (oifrestricted (get-field :restricted oifdata))
                (oifresults (get-field :results oifdata))
                (oifcoverage (if (zerop oifrestricted)
                               100
                               (float (* 100 (/ oifresults oifrestricted)))))
                (nwords (get-field :lambiguity ndata))
                (nanalyses (get-field :analyses ndata))
                (nwfrestricted (get-field :restricted nwfdata))
                (nwfresults (get-field :results nwfdata))
                (nwfcoverage (if (zerop nwfrestricted)
                               100
                               (float (* 100 (/ nwfresults nwfrestricted)))))
                (nifrestricted (get-field :restricted nifdata))
                (nifresults (get-field :results nifdata))
                (nifcoverage (if (zerop nifrestricted)
                               100
                               (float (* 100 (/ nifresults nifrestricted))))))
           (case format
             (:latex
              (format
               stream
               "  ~a & ~,2f & ~,2f & ~,1f & ~,1f~%    ~
                & ~,2f & ~,2f & ~,1f & ~,1f\\\\~%"
               name 
               owords oanalyses owfcoverage oifcoverage
               nwords nanalyses nwfcoverage nifcoverage))
             (:tcl
              (format
               stream
               "cell ~d 1 -contents ~s -format aggregate~%~
                cell ~d 2 -contents ~,2f -format data~%~
                cell ~d 3 -contents ~,2f -format data~%~
                cell ~d 4 -contents ~,1f -format data~%~
                cell ~d 5 -contents ~,1f -format data~%~
                cell ~d 6 -contents ~,2f -format data~%~
                cell ~d 7 -contents ~,2f -format data~%~
                cell ~d 8 -contents ~,1f -format data~%~
                cell ~d 9 -contents ~,1f -format data~%"
               i name i owords i oanalyses i owfcoverage i oifcoverage
               i nwords i nanalyses i nwfcoverage i nifcoverage)))))

       (let* ((odata (rest (assoc :total oaverages)))
              (owfdata (rest (assoc :total owfaverages)))
              (oifdata (rest (assoc :total oifaverages)))
              (ndata (rest (assoc :total naverages)))
              (nwfdata (rest (assoc :total nwfaverages)))
              (nifdata (rest (assoc :total nifaverages)))
              (name "Total")
              (owords (get-field :lambiguity odata))
              (oanalyses (get-field :analyses odata))
              (owfrestricted (get-field :restricted owfdata))
              (owfresults (get-field :results owfdata))
              (owfcoverage (if (zerop owfrestricted)
                              100
                             (float (* 100 (/ owfresults owfrestricted)))))
              (oifrestricted (get-field :restricted oifdata))
              (oifresults (get-field :results oifdata))
              (oifcoverage (if (zerop oifrestricted)
                             100
                             (float (* 100 (/ oifresults oifrestricted)))))
              (nwords (get-field :lambiguity ndata))
              (nanalyses (get-field :analyses ndata))
              (nwfrestricted (get-field :restricted nwfdata))
              (nwfresults (get-field :results nwfdata))
              (nwfcoverage (if (zerop nwfrestricted)
                             100
                             (float (* 100 (/ nwfresults nwfrestricted)))))
              (nifrestricted (get-field :restricted nifdata))
              (nifresults (get-field :results nifdata))
              (nifcoverage (if (zerop nifrestricted)
                             100
                             (float (* 100 (/ nifresults nifrestricted))))))
         (case format
           (:latex
            (format
             stream
             "  \\hline~%  \\hline~%  ~
              {\\bf ~a} & {\\bf ~,2f} & {\\bf ~,2f} ~
              & {\\bf ~,1f} & {\\bf ~,1f}~%    ~
              & {\\bf ~,2f} & {\\bf ~,2f} ~
              & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%  \\hline~%"
             name owords oanalyses owfcoverage oifcoverage
             nwords nanalyses nwfcoverage nifcoverage)
            (format
             stream
             "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
              \\end{tabular}~%"
             ncolumns caption))
           (:tcl
            (format
             stream
             "cell ~d 1 -contents ~s -format total~%~
              cell ~d 2 -contents ~,2f -format total~%~
              cell ~d 3 -contents ~,2f -format total~%~
              cell ~d 4 -contents ~,1f -format total~%~
              cell ~d 5 -contents ~,1f -format total~%~
              cell ~d 6 -contents ~,2f -format total~%~
              cell ~d 7 -contents ~,2f -format total~%~
              cell ~d 8 -contents ~,1f -format total~%~
              cell ~d 9 -contents ~,1f -format total~%"
             (+ naggregates 3) name 
             (+ naggregates 3) owords 
             (+ naggregates 3) oanalyses 
             (+ naggregates 3) owfcoverage 
             (+ naggregates 3) oifcoverage
             (+ naggregates 3) nwords 
             (+ naggregates 3) nanalyses 
             (+ naggregates 3) nwfcoverage 
             (+ naggregates 3) nifcoverage))))
       (when (or (stringp file) (stringp append)) (close stream))
       0)))))

(defun intersect-results (oitem nitem fields)
  (loop
      with oresults = (get-field :results oitem)
      with nresults = (get-field :results nitem)
      for field in fields
      for predicate = (find-attribute-predicate field)
      for ovalues = (loop for result in oresults 
                        collect (get-field field result))
      for nvalues = (loop for result in nresults 
                        collect (get-field field result))
      for oplus = (loop
                      for ovalue in ovalues
                      unless (member ovalue nvalues :test-not predicate)
                      collect ovalue
                      do 
                        (setf nvalues
                          (delete ovalue nvalues 
                                  :count 1 :test-not predicate)))

      collect (cons oplus nvalues)))

(defun compare-in-detail (olanguage nlanguage
                          &key (condition *statistics-select-condition*)
                               (show '(:i-input :i-wf))
                               (compare '(:words :readings))
                               (format :tcl)
                               (olabel "(g)old") 
                               (nlabel "new")
                               file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (show (if (atom show) (list show) (delete :i-id show)))
         (show (cons :i-id show))
         (shows (length show))
         (compare (if (atom compare) (list compare) compare))
         (thorough (intersection '(:derivation :mrs) compare))
         (compare (set-difference compare thorough :test #'equal))
         (predicates (map 'list #'find-attribute-predicate compare))
         (compares (length compare))
         (oitems
          (if (stringp olanguage) 
            (analyze olanguage 
                     :condition condition :thorough thorough
                     :meter ometer :message t)
            olanguage))
         (oitems (sort (copy-seq oitems) 
                       #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (nitems
          (if (stringp nlanguage) 
            (analyze nlanguage 
                     :condition condition :thorough thorough
                     :meter nmeter :message t)
            nlanguage))
         (stream (create-output-stream file append))
         (nitems (sort (copy-seq nitems) 
                       #'< :key #'(lambda (foo) (get-field :i-id foo)))))
    
    (case format
      (:tcl
       ;;
       ;; get the table header printed out: number of columns, justification,
       ;; and labels depend on the attributes asked for.
       ;;
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
         cell 1 1 -contents \"i-id\" -format title~%~
         region 1 1 2 1 -contents \"i-id\" -format title -hor_justify center~%~
         region 1 ~d 1 ~d -contents \"~a\" -format title ~
           -hor_justify center~%~
         region 1 ~d 1 ~d -contents \"~a\" -format title ~
           -hor_justify center~%"
        shows (+ shows compares) (+ shows compares compares)
        (+ shows 1) (+ shows compares) olabel
        (+ shows compares 1) (+ shows compares compares) nlabel)
          
       (do ((show show (rest show))
            (i 1 (+ i 1)))
           ((null show))
         (let ((justification (case (first show)
                                (:i-input "left")
                                (:i-wf "center"))))
           (when justification
             (format
              stream
              "layout col ~d -m1 5 -r ~:[1~;2~] -m2 5 -c black -j ~a~%"
              i (null (rest show)) justification)))
         (format
          stream
          "cell 1 ~d -contents \"~(~a~)\" -format title~%~
           region 1 ~d 2 ~d -contents \"~(~a~)\" -format title ~
             -hor_justify center~%"
           i (first show) i i (first show)))
       (do ((compare (append compare compare) (rest compare))
            (i (+ shows 1) (+ i 1)))
           ((null compare))
         (let ((justification (case (first compare)
                                (:tree "left"))))
           (when justification
             (format
              stream
              "layout col ~d -m1 5 -r ~:[1~;2~] -m2 5 -c black -j ~a~%"
              i (null (rest compare)) justification)))
         (format
          stream
          "cell 2 ~d -contents \"~(~a~)\" -format title~%~
           region 2 ~d 2 ~d -contents \"~(~a~)\" -format title ~
             -hor_justify center~%"
          i (first compare)
          i i (first compare)))
       
       (loop
           for field in thorough
           for i from (+ shows compares compares 1) by 2
           do
             (format
              stream
              "region 1 ~d 1 ~d -contents \"~(~a~)\" -format title ~
               -hor_justify center~%"
              i (+ i 1) field)
             (format
              stream
              "cell 2 ~d -contents \"~(~a~)\" -format title~%~
               cell 2 ~d -contents \"~(~a~)\" -format title~%"
              i olabel (+ i 1) nlabel)
           finally
             (format
              stream
              "layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%"
              (+ i 2)))))
    
    ;;
    ;; my first attempt at loop()ing (if bernd knew |:-) (28-jul-98 - oe@csli)
    ;;
    (loop
        with row = 3
        with separator = 1
        when (= (- row 2) (* separator 10))
        do 
          (case format
            (:tcl
             (format
              stream
              "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              row)))
          (incf separator)
        finally
          (case format
            (:tcl
             (format
              stream
              "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              (- row 1))))
        while (or oitems nitems)
        do 
          (let* ((oitem (first oitems))
                 (oi-id (get-field :i-id oitem))
                 (nitem (first nitems))
                 (ni-id (get-field :i-id nitem))
                 (oshow (map 'list 
                          #'(lambda (attribute)
                              (get-field attribute oitem))
                          show))
                 (nshow (map 'list 
                          #'(lambda (attribute)
                              (get-field attribute nitem))
                          show))
                 (ocompare (map 'list 
                             #'(lambda (attribute)
                                 (get-field attribute oitem))
                             compare))
                 (ncompare (map 'list 
                             #'(lambda (attribute)
                                 (get-field attribute nitem))
                             compare))
                 clashes)

            (cond 
             ((and (eql oi-id ni-id)
                   (equal oshow nshow))
              ;;
              ;; two items of same identifier have equal values for all .show.
              ;; attributes (as they should |:-)
              ;;
              (setf clashes (intersect-results oitem nitem thorough))
              (when (or (some #'(lambda (clash)
                                  (or (first clash) (rest clash)))
                              clashes)
                        (loop
                            for predicate in predicates 
                            for ovalue in ocompare
                            for nvalue in ncompare
                            when (funcall predicate ovalue nvalue)
                            do (return t)
                            finally (return nil)))
                (case format
                  (:tcl
                   (do ((show show (rest show))
                        (oshow oshow (rest oshow))
                        (j 1 (+ j 1)))
                       ((null oshow))
                     (if (and (eq (first show) :i-input) (stringp olanguage))
                       (format
                        stream
                        "cell ~d ~d -contents ~s ~
                         -format data -key ~d -source {~a}~%"
                        row j (first oshow) oi-id olanguage)
                       (format
                        stream
                        "cell ~d ~d -contents ~s -format data~%"
                        row j (first oshow))))
                   (do ((acompare (append ocompare ncompare) (rest acompare))
                        (j (+ shows 1) (+ j 1)))
                       ((null acompare))
                     (format
                      stream
                      "cell ~d ~d -contents ~s -format data~%"
                      row j (first acompare)))
                   (loop
                       for j from (+ shows compares compares 1) by 2
                       for field in thorough
                       for (oclash . nclash) in clashes
                       for otag = (intern (gensym "") :keyword)
                       for ntag = (intern (gensym "") :keyword)
                       do
                         ;;
                         ;; _fix_me_
                         ;; this creates a potential memory leak: as soon as
                         ;; the window for this table is destroyed, there will
                         ;; be no further reference to the (tag) symbols used
                         ;; to store data on the lisp side.  yet, the values
                         ;; associated with the symbol properties will never
                         ;; become unbound.                   (11-mar-99)
                         ;;
                         (setf (get :source otag) olanguage)
                         (setf (get :contrast otag) nlanguage)
                         (setf (get :i-id otag) oi-id)
                         (setf (get :i-input otag) (get-field :i-input oitem))
                         (setf (get :field otag) field)
                         (setf (get :value otag) oclash)
                         (setf (get :source ntag) nlanguage)
                         (setf (get :contrast ntag) olanguage)
                         (setf (get :i-id ntag) ni-id)
                         (setf (get :i-input ntag) (get-field :i-input nitem))
                         (setf (get :field ntag) field)
                         (setf (get :value ntag) nclash)
                         (format
                          stream
                          "cell ~d ~d -contents ~s -format data ~
                           -action browse -tag ~a~%~
                           cell ~d ~d -contents ~s -format data ~
                           -action browse -tag ~a~%"
                          row j (length oclash) otag
                          row (+ j 1) (length nclash) ntag))))
                (incf row))
              (pop oitems)
              (pop nitems))
             ((or (null ni-id) (and oi-id (<= oi-id ni-id)))
              ;;
              ;; if .oi-id. is less or equal (which it should not) to .ni-id.
              ;; output .compare. values for `old' item and continue
              ;;
              (case format
                (:tcl
                 (do ((show show (rest show))
                      (oshow oshow (rest oshow))
                      (j 1 (+ j 1)))
                     ((null oshow))
                   (if (and (eq (first show) :i-input) (stringp olanguage))
                     (format
                      stream
                      "cell ~d ~d -contents ~s ~
                       -format data -key ~d -source {~a}~%"
                      row j (first oshow) oi-id olanguage)
                     (format
                      stream
                      "cell ~d ~d -contents ~s -format data~%"
                      row j (first oshow))))
                 (do ((ocompare ocompare (rest ocompare))
                      (j (+ shows 1) (+ j 1)))
                     ((null ocompare))
                   (format
                    stream
                    "cell ~d ~d -contents ~s -format data~%"
                    row j (first ocompare)))))
              (pop oitems)
              (incf row))
             (t
              ;;
              ;; otherwise (.ni-id. is less than .oi-id.) output .compare.
              ;; values for it and leave `old' item to next iteration
              ;;
              (case format
                (:tcl
                 (do ((show show (rest show))
                      (nshow nshow (rest nshow))
                      (j 1 (+ j 1)))
                     ((null nshow))
                   (if (and (eq (first show) :i-input) (string nlanguage))
                     (format
                      stream
                      "cell ~d ~d -contents ~s ~
                       -format data -key ~d -source {~a}~%"
                      row j (first nshow) ni-id nlanguage)
                     (format
                      stream
                      "cell ~d ~d -contents ~s -format data~%"
                      row j (first nshow))))
                 (do ((ncompare ncompare (rest ncompare))
                      (j (+ shows compares 1) (+ j 1)))
                     ((null ncompare))
                   (format
                    stream
                    "cell ~d ~d -contents ~s -format data~%"
                    row j (first ncompare)))))
              (pop nitems)
              (incf row)))))
    
    (when (or (stringp file) (stringp append)) (close stream))))

(defun execute-tag (action tag &key (format :tcl) file append)

  (let* ((tag (intern tag :keyword)))

    (case action
      (:browse
       (let* ((clashes (get :value tag))
              (stream (and clashes (create-output-stream file append))))
         (when clashes
           (case format
             (:tcl
              (when *statistics-tcl-formats* 
                (format stream *statistics-tcl-formats*))
              (format
               stream
               "layout col def -m1 5 -r 2 -m2 5 -c black -j center~%~
                layout row def -m1 5 -r 1 -m2 5 -c black -j center~%~
                layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%")
              (loop
                  with *print-pretty* = nil
                  for clash in clashes
                  for i from 1
                  for ntag = (intern (gensym "") :keyword)
                  do
                    (setf (get :i-id ntag) (get :i-id tag))
                    (setf (get :i-input ntag) (get :i-input tag))
                    (setf (get :source ntag) (get :source tag))
                    (setf (get :derivation ntag) clash)
                    (format
                     stream
                     "cell ~d 1 -contents {~s} -format title ~
                      -action reconstruct -tag ~a~%"
                     i clash ntag)
                  finally
                    (format
                     stream
                     "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                     i))))
           (when (or (stringp file) (stringp append)) (close stream)))))
      (:reconstruct
       (let* ((i-id (get :i-id tag))
              (i-input (get :i-input tag))
              (derivation (get :derivation tag))
              (derivation (if (stringp derivation) 
                            (read-from-string derivation) 
                            derivation)))
         ;;
         ;; _fix_me_
         ;; now that the number of host platform is steadily growing (+ 2 in
         ;; about half a year), all application-specific code should really go
         ;; into the interface file.                            (12-mar-99)
         ;;
         (reconstruct-item i-id i-input derivation))))))
  
(defun summarize-performance-parameters (items
                                         &key restrictor (format 9902))

  (let ((itemtotal 0)
        (readingstotal 0)
        (etaskstotal 0)
        (staskstotal 0)
        (ftaskstotal 0)
        (edgestotal 0)
        (aedgestotal 0)
        (pedgestotal 0)
        (raedgestotal 0)
        (rpedgestotal 0)
        (firsttotal 0)
        (totaltotal 0)
        (tcputotal 0)
        (tgctotal 0)
        (spacetotal 0)
        result)
    
    (dolist (phenomenon items)
      (let* ((data (rest (rest phenomenon)))
             (data (remove -1 data :key #'(lambda (foo)
                                            (get-field :readings foo))))
             (data (if restrictor
                     (remove-if restrictor data)
                     data))
             (items (length data))
             (readingss (length (remove 0 data 
                                        :key #'(lambda (foo)
                                                 (get-field :readings foo)))))
             (etaskss (map 'list #'(lambda (foo)
                                     (get-field+ :p-etasks foo -1))
                           data))
             (staskss (map 'list #'(lambda (foo)
                                     (get-field+ :p-stasks foo -1))
                           data))
             (ftaskss (map 'list #'(lambda (foo)
                                     (get-field+ :p-ftasks foo -1))
                           data))
             (edgess (map 'list #'(lambda (foo)
                                    (get-field+ :edges foo -1))
                           data))
             (aedgess (map 'list #'(lambda (foo)
                                     (get-field+ :aedges foo -1))
                           data))
             (pedgess (map 'list #'(lambda (foo)
                                     (get-field+ :pedges foo -1))
                           data))
             (raedgess (map 'list #'(lambda (foo)
                                      (get-field+ :raedges foo -1))
                            data))
             (rpedgess (map 'list #'(lambda (foo)
                                      (get-field+ :rpedges foo -1))
                            data))
             (firsts (map 'list #'(lambda (foo) 
                                    (when (> (get-field :readings foo) 0)
                                      (get-field :first foo)))
                          data))
             (firsts (map 'list #'(lambda (foo)
                                    (if (eql foo -1)
                                      -1
                                      (/ foo (cond
                                              ((zerop format) 10)
                                              ((= format 9808) 100)
                                              ((>= format 9902) 1000)))))
                          (remove nil firsts)))
             (totals (map 'list #'(lambda (foo)
                                    (get-field :total foo))
                          data))
             (totals (map 'list #'(lambda (foo)
                                    (if (eql foo -1)
                                      -1
                                      (/ foo (cond
                                              ((zerop format) 10)
                                              ((= format 9808) 100)
                                              ((>= format 9902) 1000)))))
                          totals))
             (tcpus (map 'list #'(lambda (foo) (get-field :tcpu foo))
                         data))
             (tcpus (map 'list #'(lambda (foo)
                                    (if (eql foo -1)
                                      -1
                                      (/ foo (cond
                                              ((zerop format) 10)
                                              ((= format 9808) 100)
                                              ((>= format 9902) 1000)))))
                          tcpus))
             (tgcs (map 'list #'(lambda (foo) (get-field :tgc foo))
                        data))
             (tgcs (map 'list #'(lambda (foo)
                                    (if (eql foo -1)
                                      -1
                                      (/ foo (cond
                                              ((zerop format) 10)
                                              ((= format 9808) 100)
                                              ((>= format 9902) 1000)))))
                          tgcs))
             (space (map 'list #'(lambda (foo)
                                   (let ((conses (get-field :conses foo))
                                         (symbols (get-field :symbols foo))
                                         (others (get-field :others foo)))
                                     (if (and conses symbols others)
                                       (if (>= format 9903)
                                         (+ conses symbols others)
                                         (+ (* conses 8) 
                                            (* symbols 24) 
                                            others))
                                       -1)))
                         data)))
        (push (cons (first phenomenon)
                    (pairlis '(:items :readingss
                               :p-etasks :p-stasks
                               :p-ftasks 
                               :edges
                               :aedges :pedges 
                               :raedges :rpedges
                               :first :total 
                               :tcpu :tgc
                               :space)
                             (list items readingss
                                   (average etaskss) (average staskss)
                                   (average ftaskss) 
                                   (average edgess)
                                   (average aedgess) (average pedgess)
                                   (average raedgess) (average rpedgess)
                                   (average firsts) (average totals)
                                   (average tcpus) (average tgcs)
                                   (average space))))
              result)
        (incf itemtotal items)
        (incf readingstotal readingss)
        (incf etaskstotal (sum etaskss))
        (incf staskstotal (sum staskss))
        (incf ftaskstotal (sum ftaskss))
        (incf edgestotal (sum edgess))
        (incf aedgestotal (sum aedgess))
        (incf pedgestotal (sum pedgess))
        (incf raedgestotal (sum raedgess))
        (incf rpedgestotal (sum rpedgess))
        (incf firsttotal (sum firsts))
        (incf totaltotal (sum totals))
        (incf tcputotal (sum tcpus))
        (incf tgctotal (sum tgcs))
        (incf spacetotal (sum space))))
    (cons (cons :total
                (pairlis (list :items :readings
                               :p-etasks
                               :p-stasks
                               :p-ftasks
                               :edges 
                               :aedges 
                               :pedges 
                               :raedges
                               :rpedges
                               :first
                               :total
                               :tcpu
                               :tgc
                               :space)
                         (list itemtotal readingstotal
                               (divide etaskstotal itemtotal)
                               (divide staskstotal itemtotal)
                               (divide ftaskstotal itemtotal)
                               (divide edgestotal itemtotal)
                               (divide aedgestotal itemtotal)
                               (divide pedgestotal itemtotal)
                               (divide raedgestotal itemtotal)
                               (divide rpedgestotal itemtotal)
                               (divide firsttotal readingstotal)
                               (divide totaltotal itemtotal)
                               (divide tcputotal itemtotal)
                               (divide tgctotal itemtotal)
                               (divide spacetotal itemtotal))))
          (delete :all result :key #'first))))

(defun analyze-performance (&optional (language *tsdb-data*)
                            &key (condition *statistics-select-condition*)
                                 file append 
                                 (format :latex) (view :parser)
                                 restrictor meter)

  (let* ((items (analyze-aggregates language :condition condition
                                    :meter meter :format format))
         (stream (create-output-stream file append))
         (averages
          (summarize-performance-parameters 
           items 
           :restrictor restrictor :format (profile-granularity language)))
         (naggregates (- (length averages) 1))
         (ncolumns 8)
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a -- (c) oe@coli.uni-sb.de)"
                   *tsdb-name* (current-time :long :pretty))))
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{8}{|c|}{~%    {\\bf `~a'} Performance Profile}\\\\~%  ~
          \\hline~%  \\hline~%  ~
          \\raisebox{-1.5ex}[0ex][0ex]{\\bf ~a}~%    & {\\bf items} ~
            & {\\bf etasks} & {\\bf filter} & {\\bf edges}~%    ~
            & {\\bf first}  & {\\bf total} & {\\bf space}\\\\~%  ~
          & $\\sharp$ & $\\phi$ & \\% & $\\phi$~%    ~
            & $\\phi$ (s) & $\\phi$ (s) & $\\phi$ (kb)\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         (if (stringp language) language)
         alabel))
      (:tcl
       (when *statistics-tcl-formats* (format stream *statistics-tcl-formats*))
       (case view
         (:performance
          (format
           stream
           "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
            layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
            layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 10 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ naggregates 1) (+ naggregates 2))
          (format
           stream
           "cell 1 1 -contents ~a -format title~%~
            cell 1 2 -contents \"items\\n#\" -format title~%~
            cell 1 3 -contents \"etasks\\n\\330\" -format title~%~
            cell 1 4 -contents \"filter\\n%\" -format title~%~
            cell 1 5 -contents \"edges\\n\\330\" -format title~%~
            cell 1 6 -contents \"first\\n\\330 (s)\" -format title~%~
            cell 1 7 -contents \"total\\n\\330 (s)\" -format title~%~
            cell 1 8 -contents \"tcpu\\n\\330 (s)\" -format title~%~
            cell 1 9 -contents \"tgc\\n\\330 (s)\" -format title~%~
            cell 1 10 -contents \"space\\n\\330 (kb)\" -format title~%~%"
           alabel))
         (:parser
          (format
           stream
           "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
            layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
            layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 8 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ naggregates 1) (+ naggregates 2) 
           (+ naggregates 3) (+ naggregates 4))
          (format
           stream
           "cell 1 1 -contents ~a -format title~%~
            cell 1 2 -contents \"items\\n#\" -format title~%~
            cell 1 3 -contents \"etasks\\n#\" -format title~%~
            cell 1 4 -contents \"stasks\\n#\" -format title~%~
            cell 1 5 -contents \"aedges\\n#\" -format title~%~
            cell 1 6 -contents \"pedges\\n#\" -format title~%~
            cell 1 7 -contents \"raedges\\n#\" -format title~%~
            cell 1 8 -contents \"rpedges\\n#\" -format title~%"
           alabel)))))
    (do* ((items items (rest items))
          (i 2 (1+ i))
          (phenomenon (first items) (first items)))
        ((null items))
      (let* ((data (rest (assoc (first phenomenon) averages))))
        (when data
          (let* ((name (if (equal format :latex)
                     (latexify-string (second phenomenon))
                     (second phenomenon)))
                 (items (get-field :items data))
                 (stasks (round (get-field :p-stasks data)))
                 (etasks (round (get-field :p-etasks data)))
                 (ftasks (round (get-field :p-ftasks data)))
                 (filter (unless (or (minus-one-p etasks) 
                                     (minus-one-p ftasks))
                           (float (* 100 (divide ftasks (+ etasks ftasks))))))
                 (aedges (round (get-field :aedges data)))
                 (pedges (round (get-field :pedges data)))
                 (edges (round (get-field :edges data)))
                 (edges (if (and edges (>= edges 0))
                          edges
                          (if (>= pedges 0) pedges -1)))
                 (raedges (round (get-field :raedges data)))
                 (rpedges (round (get-field :rpedges data)))
                 (first (float (get-field :first data)))
                 (total (float (get-field :total data)))
                 (tcpu (float (get-field :tcpu data)))
                 (tgc (float (get-field :tgc data)))
                 (space (round (/ (get-field :space data) (expt 2 10)))))
            (case format
              (:latex
               (format
                stream
                "  ~a~%    & ~d & ~d & ~@[~,1f~] & ~d & ~,2f & ~,2f & ~d\\\\~%"
                name items etasks filter edges first total space))
              (:tcl
               (case view
                 (:performance
                  (format
                   stream
                   "cell ~d 1 -contents \"~a\" -format aggregate~%~
                    cell ~d 2 -contents ~d -format data~%~
                    cell ~d 3 -contents ~d -format data~%~
                    cell ~d 4 -contents ~:[{-}~*~;~,1f~] -format data~%~
                    cell ~d 5 -contents ~d -format data~%~
                    cell ~d 6 -contents ~,2f -format data~%~
                    cell ~d 7 -contents ~,2f -format data~%~
                    cell ~d 8 -contents ~,2f -format data~%~
                    cell ~d 9 -contents ~,2f -format data~%~
                    cell ~d 10 -contents ~d -format data~%~%"
                   i name
                   i items
                   i etasks
                   i filter filter
                   i edges
                   i first
                   i total
                   i tcpu
                   i tgc
                   i space))
                 (:parser
                  (format
                   stream
                   "cell ~d 1 -contents \"~a\" -format aggregate~%~
                    cell ~d 2 -contents ~d -format data~%~
                    cell ~d 3 -contents ~,1f -format data~%~
                    cell ~d 4 -contents ~,1f -format data~%~
                    cell ~d 5 -contents ~,1f -format data~%~
                    cell ~d 6 -contents ~,1f -format data~%~
                    cell ~d 7 -contents ~,1f -format data~%~
                    cell ~d 8 -contents ~,1f -format data~%"
                   i name
                   i items
                   i etasks
                   i stasks
                   i aedges
                   i pedges
                   i raedges
                   i rpedges)))))))))
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (stasks (round (get-field :p-stasks data)))
           (etasks (round (get-field :p-etasks data)))
           (ftasks (round (get-field :p-ftasks data)))
           (filter (unless (or (minus-one-p etasks) (minus-one-p ftasks))
                     (float (* 100 (divide ftasks (+ etasks ftasks))))))
           (aedges (round (get-field :aedges data)))
           (pedges (round (get-field :pedges data)))
           (edges (round (get-field :edges data)))
           (edges (if (and edges (>= edges 0))
                    edges
                    (if (>= pedges 0) pedges -1)))
           (raedges (round (get-field :raedges data)))
           (rpedges (round (get-field :rpedges data)))
           (first (float (get-field :first data)))
           (total (float (get-field :total data)))
           (tcpu (float (get-field :tcpu data)))
           (tgc (float (get-field :tgc data)))
           (space (round (/ (get-field :space data) (expt 2 10))))
           (bytes (/ (get-field :space data) (expt 2 20))))
      (case format
        (:latex
         (format stream "  \\hline~%  \\hline~%")
         (format
          stream
          "  {\\bf ~a} & {\\bf ~d}~%    ~
           & {\\bf ~d} & ~@[{\\bf ~,1f}~] & {\\bf ~d} & {\\bf ~,2f}~%    ~
           & {\\bf ~,2f} & {\\bf ~d}\\\\~%  \\hline~%"
          name items etasks filter edges first total space)
         (format
          stream
          "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
            \\end{tabular}~%"
          ncolumns caption))
        (:tcl
         (case view
           (:performance
            (format
             stream
             "cell ~d 1 -contents \"~a\" -format total~%~
              cell ~d 2 -contents ~d -format total~%~
              cell ~d 3 -contents ~d -format total~%~
              cell ~d 4 -contents ~:[{-}~*~;~,1f~] -format total~%~
              cell ~d 5 -contents ~d -format total~%~
              cell ~d 6 -contents ~,2f -format total~%~
              cell ~d 7 -contents ~,2f -format total~%~
              cell ~d 8 -contents ~,2f -format total~%~
              cell ~d 9 -contents ~,2f -format total~%~
              cell ~d 10 -contents ~d -format total~%~%"
             (+ naggregates 2) name
             (+ naggregates 2) items
             (+ naggregates 2) etasks
             (+ naggregates 2) filter filter
             (+ naggregates 2) edges
             (+ naggregates 2) first
             (+ naggregates 2) total
             (+ naggregates 2) tcpu
             (+ naggregates 2) tgc
             (+ naggregates 2) space))
           (:parser
            (format
             stream
             "cell ~d 1 -contents \"~a\" -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%"
             (+ naggregates 2)  name
             (+ naggregates 2)  items
             (+ naggregates 2)  etasks
             (+ naggregates 2)  stasks
             (+ naggregates 2)  aedges
             (+ naggregates 2)  pedges
             (+ naggregates 2)  raedges
             (+ naggregates 2)  rpedges)
            (format
             stream
             "cell ~d 1 -contents \"~a\" -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%"
             (+ naggregates 3)  "Per Second"
             (+ naggregates 3)  items
             (+ naggregates 3)  (divide etasks tcpu)
             (+ naggregates 3)  (divide stasks tcpu)
             (+ naggregates 3)  (divide aedges tcpu)
             (+ naggregates 3)  (divide pedges tcpu)
             (+ naggregates 3)  (divide raedges tcpu)
             (+ naggregates 3)  (divide rpedges tcpu))
            (format
             stream
             "cell ~d 1 -contents \"~a\" -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%"
             (+ naggregates 4)  "Per MByte"
             (+ naggregates 4)  items
             (+ naggregates 4)  (divide etasks bytes)
             (+ naggregates 4)  (divide stasks bytes)
             (+ naggregates 4)  (divide aedges bytes)
             (+ naggregates 4)  (divide pedges bytes)
             (+ naggregates 4)  (divide raedges bytes)
             (+ naggregates 4)  (divide rpedges bytes)))))))
            
    (when (or (stringp file) (stringp append)) (close stream))))

(defun compare-performance (olanguage nlanguage 
                            &key (condition *statistics-select-condition*)
                                 (format :latex)
                                 (olabel "(g)old") (nlabel "new")
                                 (clabel "reduction")
                                 orestrictor nrestrictor restrictor
                                 file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (oitems 
          (if (stringp olanguage) 
            (analyze-aggregates olanguage :condition condition
                                :meter ometer :format format) 
            olanguage))
         (nitems 
          (if (stringp nlanguage) 
            (analyze-aggregates nlanguage :condition condition
                                :meter nmeter :format format) 
            nlanguage))
         (ncolumns 10)
         (caption (format 
                   nil "(generated by ~a at ~a -- (c) oe@coli.uni-sb.de)"
                   *tsdb-name* (current-time :long :pretty))))

    (cond
     ((or (null oitems) (null nitems)) 1)
     ((not (= (length oitems) (length nitems))) 2)
     (t
      (let* ((stream (create-output-stream file append))
             (oaverages (summarize-performance-parameters 
                         oitems :restrictor (or orestrictor restrictor)
                         :format (profile-granularity olanguage)))
             (naverages (summarize-performance-parameters 
                         nitems :restrictor (or nrestrictor restrictor)
                         :format (profile-granularity nlanguage)))
             (naggregates (- (length oaverages) 1))
             (*print-circle* nil)
             (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                       "Phenomenon"
                       "Aggregate")))

       (case format
         (:latex
          (format
           stream
           "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|c|c|@{}}~%  ~
            \\hline~%  ~
            & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
            & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
            & \\multicolumn{3}{|c|}{\\bf ~a}\\\\~%  ~
            {\\bf ~a} ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}\\\\~%  ~
            & $\\phi$ & $\\phi$ (s) & $\\phi$ (kb)~%   ~
            & $\\phi$ & $\\phi$ (s)& $\\phi$ (kb)~%   ~
              & $\\%$ & $\\%$ & $\\%$\\\\~%  ~
            \\hline~%  ~
            \\hline~%"
           olabel nlabel clabel alabel))
         (:tcl
          (when *statistics-tcl-formats* 
            (format stream *statistics-tcl-formats*))
          (format
           stream
           "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
            layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
            layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 7 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 10 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout col 11 -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ naggregates 2) (+ naggregates 3))
          (format
           stream
           "cell 1 1 -contents ~a -format title~%~
            region 1 1 2 1 -contents ~a -format title ~
              -hor_justify left -ver_justify center~%~
            region 1 2 1 4 -contents ~s -format title -hor_justify center~%~
            region 1 5 1 7 -contents ~s -format title -hor_justify center~%~
            region 1 8 1 10 -contents ~s -format title -hor_justify center~%"
           alabel alabel olabel nlabel clabel)
          (do ((labels '("tasks\\n\\330" "time\\n\\330" "space\\n\\330"
                         "tasks\\n\\330" "time\\n\\330" "space\\n\\330"
                         "tasks\\n%" "time\\n%" "space\\n%")
                       (rest labels))
               (i 2 (+ i 1)))
              ((null labels))
            (format
             stream
             "cell 2 ~d -contents \"~a\" -format title~%~
              region 2 ~d 2 ~d -contents \"~a\" -format title ~
                -hor_justify center~%"
             i (first labels) i i (first labels)))))

       (do* ((oitems (remove :all oitems :key #'first) (rest oitems))
             (i 3 (+ i 1)))
           ((null oitems))
         (let* ((phenomenon (first oitems))
                (odata (rest (assoc (first phenomenon) oaverages)))
                (ndata (rest (assoc (first phenomenon) naverages)))
                (name (if (eq format :latex)
                        (latexify-string (second phenomenon))
                        (second phenomenon)))
                (oetasks (round (get-field :p-etasks odata)))
                (otime (float (get-field :tcpu odata)))
                (ospace (round (/ (get-field :space odata) (expt 2 10))))
                (netasks (round (get-field :p-etasks ndata)))
                (ntime (float (get-field :tcpu ndata)))
                (nspace (round (/ (get-field :space ndata) (expt 2 10))))
                (taskreduction 
                 (float (* 100 (divide (- oetasks netasks) oetasks))))
                (timereduction 
                 (float (* 100 (divide (- otime ntime) otime))))
                (spacereduction 
                 (float (* 100 (divide (- ospace nspace) ospace)))))
           (case format
             (:latex
              (format
               stream
               "  ~a~%     & ~d & ~,2f & ~d ~
                & ~d & ~,2f & ~d ~
                & ~,1f & ~,1f & ~,1f\\\\~%"
               name oetasks otime ospace netasks ntime nspace
               taskreduction timereduction spacereduction))
             (:tcl
              (format
               stream
               "cell ~d 1 -contents ~s -format aggregate~%~
                cell ~d 2 -contents ~d -format data~%~
                cell ~d 3 -contents ~,2f -format data~%~
                cell ~d 4 -contents ~d -format data~%~
                cell ~d 5 -contents ~d -format data~%~
                cell ~d 6 -contents ~,2f -format data~%~
                cell ~d 7 -contents ~d -format data~%~
                cell ~d 8 -contents ~,1f -format data~%~
                cell ~d 9 -contents ~,1f -format data~%~
                cell ~d 10 -contents ~,1f -format data~%"
               i name
               i oetasks i otime i ospace 
               i netasks i ntime i nspace
               i taskreduction i timereduction i spacereduction)))))

       (let* ((odata (rest (assoc :total oaverages)))
              (ndata (rest (assoc :total naverages)))
              (name "Total")
              (oetasks (round (get-field :p-etasks odata)))
              (otime (float (get-field :tcpu odata)))
              (ospace (round (/ (get-field :space odata) (expt 2 10))))
              (netasks (round (get-field :p-etasks ndata)))
              (ntime (float (get-field :tcpu ndata)))
              (nspace (round (/ (get-field :space ndata) (expt 2 10))))
              (taskreduction 
               (float (* 100 (divide (- oetasks netasks) oetasks))))
              (timereduction 
               (float (* 100 (divide (- otime ntime) otime))))
              (spacereduction 
               (float (* 100 (divide (- ospace nspace) ospace)))))
         (case format
           (:latex
            (format
             stream 
             "  \\hline~%  \\hline~%  ~
              {\\bf ~a} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~d}~%    ~
              & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~d}~%    ~
              & {\\bf ~,1f} & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%  \\hline~%"
             name oetasks otime ospace netasks ntime nspace
             taskreduction timereduction spacereduction)
            (format
             stream
             "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
              \\end{tabular}~%"
             ncolumns caption))
           (:tcl
            (format
             stream
             "cell ~d 1 -contents ~s -format total~%~
              cell ~d 2 -contents ~d -format total~%~
              cell ~d 3 -contents ~,2f -format total~%~
              cell ~d 4 -contents ~d -format total~%~
              cell ~d 5 -contents ~d -format total~%~
              cell ~d 6 -contents ~,2f -format total~%~
              cell ~d 7 -contents ~d -format total~%~
              cell ~d 8 -contents ~,1f -format total~%~
              cell ~d 9 -contents ~,1f -format total~%~
              cell ~d 10 -contents ~,1f -format total~%"
             (+ naggregates 3) name
             (+ naggregates 3) oetasks 
             (+ naggregates 3) otime 
             (+ naggregates 3) ospace 
             (+ naggregates 3) netasks 
             (+ naggregates 3) ntime 
             (+ naggregates 3) nspace
             (+ naggregates 3) taskreduction 
             (+ naggregates 3) timereduction 
             (+ naggregates 3) spacereduction))))

       (when (or (stringp file) (stringp append)) (close stream))
       0)))))

(defun graph (data &key (condition *statistics-select-condition*)
                        file (format :tcl)
                        (dimension :words)
                        (aggregate 1) (threshold 5) lower upper
                        attributes labels symbols colours
                        title xtitle logscale meter)
  
  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (aggregates 
          (aggregate data 
                     :condition condition :dimension dimension 
                     :aggregate aggregate :threshold threshold
                     :lower lower :upper upper
                     :restrictor #'(lambda (foo)
                                     (eql (get-field :readings foo) -1))
                     :format format :meter meter))
         (saggregates (when attributes
                        (summarize-performance-parameters 
                         aggregates
                         :format (profile-granularity data))))
         (values (make-array (list (length attributes))))
         (units (nreverse (map 'list #'first aggregates)))
         (frequencies (map 'list 
                        #'(lambda (foo) (length (rest (rest foo))))
                        aggregates))
         (xmin (if units (apply #'min units) 0))
         (xmax (if units (apply #'max units) 42))
         ymin ymax lsymbols llabels)
    
    (when meter (status :text "computing graph layout and geometry ..."))
    (if attributes
      (loop
          for aggregate in (rest saggregates)
          for data = (rest aggregate)
          do
            (loop 
                for attribute in attributes
                for i from 0 by 1       
                for key = (if (stringp attribute)
                            (intern (string-upcase attribute) :keyword)
                            (intern attribute :keyword))
                for value = (get-field+ key data -1)
                do (push value (aref values i))
                   (setf ymin (if ymin (min ymin value) value))
                   (setf ymax (if ymax (max ymax value) value))))
      (setf ymin 0 ymax (apply #'max frequencies)))

    (let* ((title (or title "Generic tsdb(1) graph"))
           (xtitle (or xtitle 
                       (format nil "Item Dimension `~(~a~)'" dimension)))
           (width (if units (- xmax xmin) 0))
           (height (- ymax ymin))
           (intervals '(1 2 5))
           (goal 10)
           (xdivision
            ;;
            ;; this somewhat awkwardly-looking loop() computes the number of
            ;; ticks (divisions) on the x axis; it aims for an interval size
            ;; (distance between two ticks) that is a member of .intervals. or
            ;; a 10 multiply of one of the members; the loop() chooses the x
            ;; axis layout that comes closest to .goal. ticks.
            ;;
            (loop
                with division = 1
                for i from 1 by 1
                for n = 1 then (if (zerop (mod i (length intervals))) 
                                 (* n 10)
                                 n)
                for interval = (* (nth (mod i (length intervals)) intervals) n)
                for ticks = (round width division)
                for comparison = (round width interval)
                do
                  (if (< (abs (- goal comparison)) (abs (- goal ticks)))
                    (setf division interval)
                    (return division))))
            (ydivision
            ;;
            ;; similar computation for y axis
            ;;
            (loop
                with division = 1
                for i from 1 by 1
                for n = 1 then (if (zerop (mod i (length intervals))) 
                                 (* n 10)
                                 n)
                for interval = (* (nth (mod i (length intervals)) intervals) n)
                for ticks = (round height division)
                for comparison = (round height interval)
                do
                  (if (< (abs (- goal comparison)) (abs (- goal ticks)))
                    (setf division interval)
                    (return division)))))
      (case format
        (:tcl
         (if attributes
            (format 
             stream
             "graph -font {Helvetica 10 bold} -plotbackground white \\~%  ~
              -width 15c -height 10c -rightmargin 10  \\~%  ~
              -title ~s~%"
             title)
            (format 
             stream
             "barchart -font {Helvetica 10 bold} -plotbackground white \\~%  ~
              -width 15c -height 10c -rightmargin 10  \\~%  ~
              -barmode normal -barwidth ~f -invertxy no\\~%  ~
              -title ~s~%"
             (* aggregate 0.7) title))
         (format 
          stream "data x ~a~%" 
          (list2tcl units :format "~,4f"))
         (format
          stream
          "axis x -title ~s -stepsize ~d \\~%  ~
           -tickfont {Helvetica 9} -subdivisions 1~%"
          xtitle xdivision)
         (format
          stream
          "axis y -tickfont {Helvetica 9} -logscale ~:[no~;yes~]~%"
          logscale))
        (:latex
         ;;
         ;; enlarge plot area (vertically) slightly to guarantee room for the
         ;; copyright caption
         ;;
         (let* ((pad (* (- ymax ymin) 0.04))
                (ymin (- ymin pad))
                (height (- ymax ymin))
                (ytics 
                 (loop 
                     for tic = (* (ceiling ymin ydivision) ydivision)
                     then (+ tic ydivision)
                     while (<= tic ymax)
                     collect tic))
                (caption 
                 (format 
                  nil "(generated by ~a at ~a -- (c) oe@coli.uni-sb.de)"
                  *tsdb-name* (current-time :long :pretty))))

           (format
            stream
            "\\dimendef\\plotwidth=0~%\\dimendef\\plotheight=1~%~
             \\plotwidth=~amm~%\\plotheight=~amm~%~
             ~:[\\dimendef\\breadth=2\\breadth=\\plotwidth~%~;~]~
             \\divide\\plotwidth by ~a~%\\divide\\plotheight by ~a~%~
             ~:[\\divide\\breadth by ~a~%~;~*~]~
             \\beginpicture~%  ~
             \\setplotsymbol({\\rule{.4pt}{.4pt}})~%  ~
             \\setlinear~%  ~
             \\setcoordinatesystem units <\\plotwidth,\\plotheight>~%  ~
             \\setplotarea x from ~,4f to ~,4f, y from ~,4f to ~,4f~%  ~
             \\axis label {\\sf ~a}~%    ~
               bottom ticks numbered from  ~d to ~d by ~d /~%  ~
             \\axis left ticks numbered~%        at ~{~a ~}/ /~%  ~
             \\plotheading {\\frame <1pt> {\\frame <5pt> {\\Large\\sf ~a}}}~%"
            *statistics-plot-width* *statistics-plot-height*
            attributes
            (round (if attributes width (+ width 2))) (round height)
            attributes (round (* (+ width 2) 1.1))
            (if attributes xmin (- xmin 1)) (if attributes xmax (+ xmax 1))
            ymin ymax
            xtitle
            xmin xmax xdivision
            ytics
            title)
           (format
            stream
            "  \\put {\\tiny\\sf ~a} [r] at ~,4f ~,4f~%"
            caption xmax (+ ymin (/ pad 2)))))))
    (if attributes
      (loop
          for attribute in attributes
          for key = (if (stringp attribute)
                      (intern (string-upcase attribute) :keyword)
                      (intern attribute :keyword))
          for i from 0 by 1       
          for data = (nreverse (aref values i))
          for label = (or (nth i labels) (find-attribute-label key))
          and symbol = (or (nth i symbols) 
                           (find-attribute-symbol key i :format format))
          and colour = (or (nth i colours) (find-attribute-colour key i))
          do
            (case format
              (:tcl
               (format 
                stream 
                "data y~a ~a~%" 
                i (list2tcl data :format "~,4f"))
               (format
                stream
                "element e~a -xdata x -ydata y~a -label ~s \\~%  ~
                 -symbol ~a -pixel 3 \\~%  ~
                 -color ~a -fill defcolor -outline defcolor~%"
                i i label symbol colour))
              (:latex
               (format stream "  \\multiput{~a} at~%" symbol)
               (loop for x in units for y in data
                   do (format stream "    ~12,4f ~12,4f~%" x y))
               (format stream "  /~%  \\plot~%")
               (loop for x in units for y in data
                   do (format stream "    ~12,4f ~12,4f~%" x y))
               (format stream "  /~%")))
            (push symbol lsymbols)
            (push label llabels))
      (case format
        (:tcl
         (format 
          stream 
          "data y ~a~%~
           element e -xdata x -ydata y -relief flat~%"
          (list2tcl (nreverse frequencies) :format "~d")))
        (:latex
         (format
          stream
          "  \\setbars breadth <0mm> baseline at y = 0~%  ~
             \\linethickness=\\breadth~%  \\plot~%")
         (loop
             for x in units
             for y in (nreverse frequencies)
             do
               (format stream "~10d ~8d~%" x y))
         (format stream "  /~%"))))
    (case format
      (:tcl
       (if attributes
         (format 
          stream 
          "legend -font {Helvetica 9} -position plotarea -anchor nw \\~%  ~
           -relief ridge~%")
         (format stream "legend -hide yes~%")))
      (:latex
       (when attributes
         (let* ((x (+ xmin (* (- xmax xmin) 0.05)))
                (y (- ymax (* (- ymax ymin) 0.05))))
           (format
            stream
            "  \\put {\\frame <5pt> {\\lines [l] {~%")
           (loop
               for symbol in (nreverse lsymbols)
               and label in (nreverse llabels)
               do
                 (format stream "   {~a --- {\\sf ~a}}\\cr~%" symbol label))
           (format stream "  }}} [lt] at ~,4f ~,4f~%" x y)))
       (format
        stream
        "\\endpicture~%")))
    (force-output stream)
    (when file (close stream))))

(defun rule-statistics (data
                        &key condition (attributes '(:executed successes))
                             file (format :tcl) (view :graph) logscale meter)

  (when attributes
    (let* ((stream (if file
                     (create-output-stream file nil)
                     *tsdb-io*))
           (rules (analyze-rules data :condition condition 
                                 :format format :meter meter))
           (rules (sort (copy-seq rules) #'string-greaterp :key #'second))
           (indices (loop for i from 1 to (length rules) collect i))
           (filtered (loop for rule in rules 
                         collect (get-field :filtered (rest (rest rule)))))
           (executed (loop for rule in rules 
                         collect (get-field :executed (rest (rest rule)))))
           (successes (loop for rule in rules 
                          collect (get-field :successes (rest (rest rule)))))
           (actives (loop for rule in rules 
                        collect (get-field :actives (rest (rest rule)))))
           (passives (loop for rule in rules 
                         collect (get-field :passives (rest (rest rule)))))
           (label (format 
                   nil 
                   "~(~a~)~{ # ~(~a~)~}"
                   (first attributes) (rest attributes))))
      
      (case view
        (:graph
         (format 
           stream
           "barchart -font {Helvetica 10 bold} -plotbackground white \\~%  ~
            -width 16c -height 20c -barmode aligned -barwidth 0.75 \\~%  ~
            -title \"Rule Postulation and Success Distribution\" \\~%  ~
            -invertxy yes -rightmargin 10~%")
          (format stream "legend -hide yes~%")
          (format
           stream
           "axis x -stepsize 1 -tickfont {Helvetica 9} -subdivisions 1 \\~%  ~
            -labels ~a~%"
           (list2tcl (map 'list #'second rules)))
          (format
           stream
           "axis y -title {~a} \\~%  ~
            -logscale ~:[no~;yes~]~%" 
           label logscale)
          (format stream "data x1 ~a~%" (list2tcl indices))
          (when (member :filtered attributes :test #'eq)
            (format stream "data y1 ~a~%" (list2tcl filtered))
            (format 
             stream 
             "element e1 -xdata x1 -ydata y1 -fg red -relief flat~%"))
          (when (member :executed attributes :test #'eq)
            (format stream "data y2 ~a~%" (list2tcl executed))
            (format 
             stream 
             "element e2 -xdata x1 -ydata y2 -fg orange  -relief flat~%"))
          (when (member :successes attributes :test #'eq)
            (format stream "data y3 ~a~%" (list2tcl successes))
            (format 
             stream 
             "element e3 -xdata x1 -ydata y3 -fg yellow -relief flat~%"))
          (when (member :actives attributes :test #'eq)
            (format stream "data y4 ~a~%" (list2tcl actives))
            (format 
             stream 
             "element e4 -xdata x1 -ydata y4 -fg blue -relief flat~%"))
          (when (member :passives attributes :test #'eq)
            (format stream "data y5 ~a~%" (list2tcl passives))
            (format 
             stream 
             "element e5 -xdata x1 -ydata y5 -fg green -relief flat~%")))
        (:table
         (when *statistics-tcl-formats* 
           (format stream *statistics-tcl-formats*))
         (format
          stream
          "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
           layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
           layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
           layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
           layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
          (+ (length attributes) 1) 
          (+ (length rules) 1)
          (+ (length rules) 2))
         (format stream "cell 1 1 -contents \"Rule\" -format title~%")
         (loop
             for attribute in attributes
             for i from 2
             do
               (format 
                stream 
                "cell 1 ~d -contents \"~(~a~)\" -format title~%"
                i attribute))
         (loop
             for rule in rules
             for i from 2
             with totals = (pairlis 
                            attributes
                            (map 'list 
                              #'(lambda (foo) (declare (ignore foo)) 0)
                              attributes))
             do
               (format 
                stream 
                "cell ~d ~d -contents \"~a\" -format data~%"
                i 1 (second rule))
               (loop
                   for attribute in attributes
                   for value = (get-field attribute (rest (rest rule)))
                   for j from 2
                   when (not (= value -1))
                   do
                     (incf (get-field attribute totals) value)
                   do
                     (format 
                      stream 
                      "cell ~d ~d -contents \"~a\" -format data~%"
                      i j value))
             finally
               (format 
                stream 
                "cell ~d ~d -contents \"Total\" -format total~%"
                (+ i 1) 1)
               (loop
                   for attribute in attributes
                   for j from 2
                   do
                     (format
                      stream
                      "cell ~d ~d -contents \"~a\" -format total~%"
                      (+ i 1) j (get-field attribute totals))))))
      (force-output stream)
      (when file (close stream))
      (pairlis '(:names 
                 :passives :actives :successes :executed :filtered)
               (list (map 'list #'first rules)
                     passives actives successes executed filtered)))))
