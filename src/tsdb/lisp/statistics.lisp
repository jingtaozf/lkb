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

(defparameter *statistics-time-fields* 
  '(:first :total :tcpu :tgc :treal :utcpu))

(defparameter *statistics-exclude-tgc-p* '(:tcpu))

(defparameter *statistics-aggregate-dimension* :i-length)

(defparameter *statistics-aggregate-size* 5)

(defparameter *statistics-aggregate-threshold* 1)

(defparameter *statistics-aggregate-lower* nil)

(defparameter *statistics-aggregate-upper* nil)

(defparameter *statistics-detail-sloppy-alignment-p* nil)

(defparameter *statistics-detail-alignment-bracket* 50)

(defparameter *statistics-analogy-aggregation-p* nil)

(defparameter *statistics-aggregate-maximum* 
  (min 20000 array-total-size-limit))

(defparameter *statistics-result-filter* #'result-filter)

(defparameter *statistics-critical-line-threshold* 500)

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

(defparameter *statistics-performance-summary* 
  '(:first :total :tcpu :tgc :space 
    :edges :pedges :aedges :rpedges :raedges 
    :copies :unifications 
    :p-ftasks :p-stasks :p-etasks))

(defparameter *statistics-extra* 
  #+:oe
  '(:trees :utcpu :uspace 
    :subsumptions :equivalence :proactive :retroactive
    :frozen :failures)
  #-:oe
  nil)

(defparameter *statistics-readers* (make-hash-table))

(defparameter *statistics-predicates* (make-hash-table))

(defparameter *statistics-browsers* (make-hash-table))

(defun per-cent (count base)
  (when (and (numberp count) (numberp base))
    (if (zerop base)
      0
      (* 100 (/ count base)))))

(defun latexify-string (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\_)
        (concatenate 'string "\\_" (latexify-string (subseq string 1)))
        (concatenate 
            'string (string prefix) (latexify-string (subseq string 1)))))
    string))

(defun find-attribute-reader (attribute)
  (let* ((name (if (stringp attribute) 
                 (string-upcase attribute)
                  attribute))
         (attribute (intern name :keyword)))
    (find-function (gethash attribute *statistics-readers*))))

(defun find-attribute-predicate (attribute)
  (let* ((name (if (stringp attribute) 
                 (string-upcase attribute) 
                 attribute))
         (attribute (intern name :keyword))
         (predicate (gethash attribute *statistics-predicates*))
         (function (find-function predicate)))
    (or function #'(lambda (old new) (not (equal old new))))))

(defun find-attribute-browser (attribute)
  (let* ((name (if (stringp attribute) 
                 (string-upcase attribute) 
                 attribute))
         (attribute (intern name :keyword)))
    (find-function (gethash attribute *statistics-browsers*))))

(defun find-attribute-label (attribute)
  (case attribute
    (:i-length "string length")
    (:readings "distinct analyses")
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
    (:gcs "global garbage collections")
    (t (format nil "`~(~a~)'" attribute))))

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
                     (find "parse" relations :key #'first :test #'string=)))
         (edge (and relations
                     (find "edge" relations :key #'first :test #'string=)))
         (update (and relations
                      (find "update" relations :key #'first :test #'string=))))
    (cond 
     ((null run) :historic)
     ((or (not (find "aedges" (rest parse) :key #'first :test #'string=))
          (not (find "end" (rest run) :key #'first :test #'string=))) 0)
     ((not (find "environment" (rest run) :key #'first :test #'string=)) 9902)
     ((null update) 9903)
     ((and update 
           (find "e-epsilons" (rest edge) 
                 :key #'first :test #'string=)) 200210)
     ((and update 
           (not (find "e-parents" (rest edge) 
                      :key #'first :test #'string=))) 200305)
     ((and update 
           (find "e-parents" (rest edge) :key #'first :test #'string=)) 200306)
     (t
      (error "profile-granularity(): invalid `~a'" data)))))

(defun analyze (data 
                &key condition meter message thorough trees extras 
                     (readerp t) filter
                     score gold taggingp
		     commentp sloppyp scorep)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (let* ((message (when message
                    (format nil "retrieving `~a' data ..." data)))
         (extras (and extras t))
         (trees (and trees t))
         (filter (when (and filter
                            (functionp *statistics-result-filter*)
                            *filter-test*)
                   (format 
                    nil
                    "~{~(~a~)~^+~}~@[+~a~]"
                    *filter-test* *filter-mrs-relations-ratio*)))
         (key (format 
               nil 
               "~a~@[ @ ~a~]~@[ # ~a~]~@[~* : trees~]~
                ~@[ for ~a~]~@[~* : extras~]~@[ on ~a~@[ (scores)~]~]" 
               data condition 
               (if (consp thorough) (format nil "~{~(~a~)~^#~}" thorough) "t")
               trees filter extras
               (cond
                ((stringp score) score)
                (score "itself")
                (gold (format nil "~a (gold)" gold)))
               scorep))
         (relations (read-database-schema data))
         (parse (rest (find "parse" relations :key #'first :test #'string=)))
         pfields ptypes result)
    #+:debug
    (format t "~&analyze(): `~a'~%" key)
    (when message (status :text message))
    (when meter (meter :value (get-field :start meter)))
    (loop while (eq (setf result (gethash key *tsdb-profile-cache*)) :seized))
    (unless result
      (setf (gethash key *tsdb-profile-cache*) :seized)
      (loop
          for field in '("i-id" "parse-id" "readings" 
                         "first" "total" "tcpu" "tgc"
                         "p-etasks" "p-stasks" "p-ftasks"
                         "unifications" "copies"
                         "conses" "symbols" "others"
                         "words" "l-stasks"
                         "edges" "aedges" "pedges" "raedges" "rpedges"
                         "gcs" "error")
          for match = (find field parse :key #'first :test #'string=)
          when match do
            (push (first match) pfields)
            (push (second match) ptypes))
      (when extras
        (push "comment" pfields)
        (push :string ptypes))

      (unwind-protect
        ;;
        ;; _fix_me_
        ;; projecting :string fields that contain the field separator (`@') of
        ;; tsdb(1) breaks when used in conjuction with a `report' format (as is
        ;; the case in our current select() implementation.  hence, sort the
        ;; `error' field to the back where it happens not to break :-{.
        ;;                                                    (22-nov-99; oe)
        ;; i believe this has been fixed sometime in 2001?    (20-jan-02; oe)
        ;;          
        (let* ((pfields (nreverse pfields))
               (ptypes (nreverse ptypes))
               (pmeter (and meter (madjust * meter (if thorough 0.4 0.5))))
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
               (parse (select pfields ptypes "parse" condition data
                              :meter pmeter :sort :i-id))
               (item (select (append
                              '("i-id" "i-input" "i-length" "i-wf")
                              (when commentp '("i-comment")))
                             (append
                              '(:integer :string :integer :integer)
                              (when commentp '(:string)))
                             "item" condition data 
                             :meter imeter :sort :i-id))
	       (item (if taggingp
                       (loop
                           for foo in item
                           for i-input = (get-field :i-input foo)
                           for tags = (call-raw-hook 
                                       *tsdb-tagging-hook* i-input)
                           when tags do (nconc foo (acons :tags tags nil))
                           finally (return item))
		       item))
               (results (when thorough
                          (select (append '("parse-id" "result-id")
                                          (when (consp thorough)
                                            (loop for symbol in thorough
                                                collect (format 
                                                         nil 
                                                         "~(~a~)" 
                                                         symbol))))
                                  nil "result" condition data 
                                  :meter rmeter :sort :parse-id)))
               (trees (when trees
                        (select '("parse-id" "t-active" "t-version")
                                nil "tree" condition data
                                :sort :parse-id)))
               (all (njoin parse item :i-id :meter ameter))
               sorted)
          (setf result all)
          (when extras
            (loop
                for tuple in result
                for comment = (get-field :comment tuple)
                for stream = (and comment (make-string-input-stream comment))
                for extra = (when stream
                              (loop
                                  for field = (read stream nil nil)
                                  while field
                                  collect field
                                  finally (close stream)))
                when extra do
                  (nconc tuple extra)))
          (when results
            (when (consp thorough)
              (loop
                  for field in thorough
                  for reader = (when readerp (find-attribute-reader field))
                  when reader
                  do
                    (loop
                        for result in results
                        for value = (get-field field result)
                        when (and reader value) do 
                          (setf (get-field field result) 
                            (funcall reader value)))))
            ;;
            ;; _fix_me_
            ;; this is sort of hacky: since we fail to guarantee unique parse
            ;; ids, the corresponding run id would have to be included in the
            ;; `result' relation; as it stands, this is not the case |:-(.
            ;; until we get his fixed, it hard-wires the assumption that we
            ;; will not use the same profile to represent multiple test runs.
            ;;                                          (10-mar-99  -  oe)
            (unless sorted 
              (setf sorted 
                (sort (copy-list all) 
                      #'< :key #'(lambda (foo) (get-field :parse-id foo)))))
            (loop
                for item in sorted
                for key = (get-field :parse-id item)
                for matches =
                  (when (eql key (get-field :parse-id (first results)))
                    (loop
                        for result = (first results)
                        while (and result 
                                   (eql key (get-field :parse-id result)))
                        collect (pop results)))
                when matches
                do (nconc 
                    item 
                    (acons :results (sort 
                                     matches #'< 
                                     :key #'(lambda (foo)
                                              (get-field :result-id foo)))
                           nil))))
          (when trees
            (unless sorted 
              (setf sorted 
                (sort (copy-list all) 
                      #'< :key #'(lambda (foo) (get-field :parse-id foo)))))
            (loop
                for item in sorted
                for key = (get-field :parse-id item)
                for tree = (loop
                               with result = nil
                               for tree = (first trees)
                               for parse-id = (get-field :parse-id tree)
                               for version = (get-field :t-version tree)
                               while (eql parse-id key) do
                                 (pop trees)
                                 (when (or (null result)
                                           (> version 
                                              (get-field :t-version result)))
                                   (setf result tree))
                               finally (return result))
                when tree
                do (nconc item tree)))
          (when (or score gold)
            (unless sorted 
              (setf sorted 
                (sort (copy-list all) 
                      #'< :key #'(lambda (foo) (get-field :parse-id foo)))))
            (rank-items sorted :gold gold :score score :condition condition 
                        :sloppyp sloppyp :scorep scorep))
        
          (when filter
            (setf result 
              (loop
                  for item in result
                  for foo = (funcall *statistics-result-filter* item)
                  when foo collect foo))))
        (setf (gethash key *tsdb-profile-cache*) result)))

    (when message (status :text (format nil "~a done" message) :duration 2))
    (when meter (meter :value (get-field :end meter)))
    result))

(defun rank-items (items &key gold score condition sloppyp scorep)
  
  (declare (ignore condition))

  (loop
      with scores = (when (and (null gold) (stringp score))
                      (if scorep
                        (select '("parse-id" "result-id" "rank" "score")
                                '(:integer :integer :integer :string)
                                "score" nil score :sort :parse-id)
                        (select '("parse-id" "result-id" "rank")
                                '(:integer :integer :integer)
                                "score" nil score :sort :parse-id)))
      with golds = (when gold
                     (select '("parse-id" "t-version" "result-id")
                             '(:integer :integer :integer)
                             "preference" nil gold :sort :parse-id))
      for item in items
      for id = (get-field :parse-id item)
      for results = (get-field :results item)
      for matches = (when id
                      (cond
                       (golds
                        (loop
                            for gid = (get-field :parse-id (first golds))
                            while (and gid (< gid id)) do (pop golds))
                        (let* ((matches (loop
                                            for foo = (first golds)
                                            for gid = (get-field :parse-id foo)
                                            while (and gid (= gid id)) 
                                            collect (pop golds)))
                               (version 
                                (loop
                                    for foo in matches
                                    for bar = (get-field :t-version foo)
                                    maximize bar)))
                          (loop
                              for foo in matches
                              when (= (get-field :t-version foo) version)
                              collect (nconc (acons :rank 1 nil) foo))))
                       (scores
                        (loop
                            for sid = (get-field :parse-id (first scores))
                            while (and sid (< sid id)) do (pop scores))
                        (let ((matches 
                               (loop
                                   for score = (first scores)
                                   for sid = (get-field :parse-id score)
                                   while (and sid (= sid id)) 
                                   collect (pop scores))))
                          (sort matches #'< :key (lambda (foo) 
                                                   (get-field :rank foo)))))))
      when matches do
        (loop
            for match in matches 
            for id = (get-field :result-id match)
            for result = (loop
                             for result in results
                             for foo = (get-field :result-id result)
                             thereis (when (= id foo) result))
            when result do (nconc match result))
        (nconc item (acons :ranks matches nil))
      else do
        (nconc item
               (unless (and (null sloppyp) (or (stringp score) gold))
                 (let ((results (sort (copy-seq results) #'<
                                      :key #'(lambda (foo) 
                                               (get-field :result-id foo)))))
                   (acons :ranks
                          (loop
                              for result in results
                              for i from 1
                              collect (acons :rank i result))
                          nil)))))
  #+:debug
  (setf %items% items))

(defun analyze-aggregates (language
                           &key condition phenomena extras trees
                                (dimension *statistics-aggregate-dimension*)
                                (format :latex) meter message)

  (if (not (eq dimension :phenomena))
    (aggregate language 
               :condition condition :dimension dimension 
               :trees trees :extras extras
               :format format :meter meter)
    (let* ((phenomena (or phenomena
                          (loop
                              for key being each hash-key in *tsdb-phenomena*
                              when (and (stringp key) (search key language))
                              return (gethash key *tsdb-phenomena*))
                          *phenomena*))
           (key (format nil "~a # phenomena" language))
           (imeter (madjust * meter (if phenomena 0.3 0.5)))
           (pmeter (madjust + (madjust * meter 0.5) (mduration imeter)))
           (increment (when meter
                        (* (mduration meter)
                           (/ (if phenomena 0.2 0.5) (length phenomena)))))
           (message (when message
                      (format nil "retrieving `~a' data ..." language)))
           pdata items)
      (when meter (meter :value (get-field :start meter)))
      (when message (status :text message))
      (let* ((idata (analyze language :condition condition 
                             :extras extras :trees trees :meter imeter)))
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
      (when meter (meter :value (get-field :end meter)))
      (when message (status :text (format nil "~a done" message :duration 2)))
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

(defun purge-profile-cache (data &key (expiryp t))
  (when expiryp
    (close-connections :data (when (stringp data) data)))
  (loop
      for key being each hash-key in *tsdb-profile-cache*
      when (or (eq data :all) 
               (search data key :end2 (length data))) do
        (remhash key *tsdb-profile-cache*)))

(defun aggregate (&optional (language *tsdb-data*)
                  &key (condition nil)
                       (restrictor nil)
                       (dimension *statistics-aggregate-dimension*)
                       (aggregate (or *statistics-aggregate-size* 2))
                       (threshold (or *statistics-aggregate-threshold* 1))
                       (lower (or *statistics-aggregate-lower* 0))
                       (upper *statistics-aggregate-upper*)
                       extras trees
                       (format :latex)
                       meter)
           
  (when meter (meter :value (get-field :start meter)))
  (let* ((imeter (madjust * meter 0.9))
         (items (if (stringp language) 
                  (analyze language :condition condition 
                           :extras extras :trees trees
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
                      when (and space (>= space lower))
                      collect (cons (cons :space space) item))
                  (if (eq dimension :phenomena)
                    items
                    (remove-if #'(lambda (foo)
                                   (< (get-field dimension foo) lower))
                               items))))
         (items (if (and upper (not (eq dimension :phenomena)))
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
    (if (eq dimension :phenomena)
      (list (cons :all (cons "All" items)))
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
                                 dimension class (+ class aggregate))))
                             (:html
                              (if (= aggregate 1)
                                (format
                                 nil
                                 "~(~a~) = ~d"
                                 dimension class)
                                (format
                                 nil
                                 "~(~a~) &isin; [~d &ndash; ~d|"
                                 dimension class (+ class aggregate)))))))
                (when (>= (length data) threshold)
                  (push (cons class (cons name data)) result))))
            (when meter 
              (meter :value (get-field :end meter))
              (status :text (format nil "~a done" message) :duration 5))
            result))))))

(defun aggregate-by-analogy (data analogon
                             &key condition (key :i-id) loosep meter)
  
  (when meter (meter :value (get-field :start meter)))
  (labels ((find! (value items key)
             (loop
                 for item in items
                 thereis (when (eq value (get-field key item)) item))))
    (loop
        with data = (if (stringp data)
                      (analyze data :condition condition
                               :meter meter :message (and meter t))
                      data)
        for sample in analogon
        for items = (rest (rest sample))
        for analogy = (loop
                          for item in items
                          for value = (get-field key item)
                          for match = (find! value data key)
                          when match collect match
			  else unless loosep return nil)
        when analogy
        collect (list* (first sample) (second sample) analogy)
        finally
          (when meter (meter :value (get-field :end meter))))))

(defun aggregate-by-classes 
    (items classes
     &key (dimension *statistics-aggregate-dimension*)
          (threshold (or *statistics-aggregate-threshold* 1))
          (lower (or *statistics-aggregate-lower* 0))
          (upper *statistics-aggregate-upper*)
          (format :latex))
  (declare (ignore threshold lower upper))
  
  (loop
      with result = nil
      with size = (+ (length classes) 1)
      with results = (make-array size)
      for item in items
      for value = (when dimension (get-field dimension item))
      for index = (if (numberp value)
                    (loop
                        for i from 0
                        for class in classes
                        thereis (and (> class value) i)
                        finally (return i))
                    0)
      do (push item (aref results index))
      finally 
        (loop
            for i from 1
            for class on classes
            for this = (first class)
            for next = (second class)
            for name = (case format
                         (:latex
                          (if (and next (= next (+ this 1)))
                            (format
                             nil
                             "\\multicolumn{1}{|c|} {\\em ~(~a~)\\/ $=$ ~d}"
                             dimension this)
                            (format 
                             nil 
                             "\\multicolumn{1}{|c|}~
                                  {~d $\\leq$ {\\em ~(~a~)\\/}~@[ $<$ ~d}~]"
                             this dimension next)))
                         (:tcl
                          (if (and next (= next (+ this 1)))
                            (format
                             nil
                             "~(~a~) = ~d"
                             dimension this)
                            (format
                             nil
                             "~(~a~) in [~d .. ~@[~d~]|"
                             dimension this next))))
            for data = (aref results i)
            when data
            do (push (cons this (cons name data)) result))
        (return (nreverse result))))
                  

(defun summarize-competence-parameters-by-division (items division
                                                    &key restrictor)
  
  (loop
      with titems = 0 with trestricted = 0
      with tilength = 0 with twords = 0 with tlstasks = 0
      with treadings = 0 with tresults = 0
      with tierrors = 0 with tderrors = 0
      with result
      for iaggregate in items
      for idata = (rest (rest iaggregate))
      for daggregate = (when (eql (first iaggregate) (first (first division)))
                         (pop division))
      for ddata = (rest (rest daggregate))
      for ridata = (if restrictor (remove-if restrictor idata) idata)
      for rddata = (if restrictor (remove-if restrictor ddata) ddata)
      for items = (length idata)
      for restricted = (length ridata)
      for ilength = 0 for words = 0 for lstasks = 0
      for readings = 0
      for results = (length rddata)
      for ierrors = 0 for derrors = 0
      do
        (loop
            for tuple in ridata do
              (incf ilength (get-field :i-length tuple))
            when (minus-one-p (get-field :readings tuple)) do (incf ierrors)
            else do
              (incf words (get-field :words tuple))
              (incf lstasks (get-field+ :l-stasks tuple 0)))
        (loop
            for tuple in rddata
            when (minus-one-p (get-field :readings tuple)) do (incf derrors)
            else do
              (incf readings (get-field :readings tuple)))
        (push (cons (first iaggregate)
                    (pairlis '(:items :restricted 
                               :i-length 
                               :words 
                               :l-stasks 
                               :lambiguity 
                               :analyses 
                               :results :ierrors :derrors)
                             (list items restricted 
                                   (divide ilength restricted)
                                   (divide words (- restricted ierrors))
                                   (divide lstasks (- restricted ierrors))
                                   (divide words ilength)
                                   (divide readings (- results derrors))
                                   results ierrors derrors)))
              result)
        (incf titems items) (incf trestricted restricted)
        (incf tilength ilength) (incf twords words) (incf tlstasks lstasks)
        (incf treadings readings) (incf tresults results)
        (incf tierrors ierrors) (incf tderrors derrors)
      finally
        (push (cons :total 
                    (pairlis '(:items :restricted
                               :i-length
                               :words 
                               :l-stasks
                               :lambiguity
                               :analyses
                               :results :ierrors :derrors)
                             (list titems trestricted
                                   (divide tilength trestricted)
                                   (divide twords (- trestricted tierrors))
                                   (divide tlstasks (- trestricted tierrors))
                                   (divide twords tilength)
                                   (divide treadings (- tresults tderrors))
                                   tresults tierrors tderrors)))
              result)
        (return (delete :all result :key #'first))))

(defun summarize-competence-parameters (aggregates &key restrictor)
  
  (loop
      with titems = 0
      with tritems = 0
      with tlength = 0
      with trlength = 0
      with tulength = 0
      with talength = 0
      with tslength = 0
      with twords = 0
      with trwords = 0
      with tuwords = 0
      with tawords = 0
      with tswords = 0
      with tlstasks = 0
      with treadings = 0
      with tresults = 0
      with trreadings = 0
      with trresults = 0
      with tureadings = 0
      with turesults = 0
      with tareadings = 0
      with taresults = 0
      with tsreadings = 0
      with tsresults = 0
      with result = nil
      for aggregate in aggregates
      for items = (rest (rest aggregate))
      for nitems = (length items)
      for ritems = (if restrictor (remove-if restrictor items) items)
      for nritems = (length ritems)
      do
        (loop
            with alength = 0
            with arlength = 0
            with aulength = 0
            with aalength = 0
            with aslength = 0
            with awords = 0
            with arwords = 0
            with auwords = 0
            with aawords = 0
            with aswords = 0
            with alstasks = 0
            with areadings = 0
            with aresults = 0
            with arreadings = 0
            with arresults = 0
            with aureadings = 0
            with auresults = 0
            with aareadings = 0
            with aaresults = 0
            with asreadings = 0
            with asresults = 0
            for item in ritems
            for ilength = (get-field :i-length item)
            for iwords = (get-field :words item)
            for ilstasks = (get-field+ :l-stasks item -1)
            for ireadings = (get-field :readings item)
            for active = (get-field :t-active item)
            do
              (incf alength ilength)
              (incf awords iwords)
              (incf alstasks ilstasks)
              (when (and (numberp ireadings) (>= ireadings 1))
                (incf areadings ireadings)
                (incf aresults)
                (cond
                 ((eql active 0) 
                  (incf arlength ilength)
                  (incf arreadings ireadings)
                  (incf arwords iwords)
                  (incf arresults))
                 ((eql active 1)
                  (incf aulength ilength)
                  (incf aureadings ireadings)
                  (incf auwords iwords)
                  (incf auresults))
                 ((and (numberp active) (> active 1))
                  (incf aalength ilength)
                  (incf aareadings ireadings)
                  (incf aawords iwords)
                  (incf aaresults))
                 ((or (null active) (and (numberp active) (= active -1)))
                  (incf aslength ilength)
                  (incf asreadings ireadings)
                  (incf aswords iwords)
                  (incf asresults))))
            finally
              (push (cons (first aggregate)
                          (pairlis '(:items :restricted 
                                     :i-length 
                                     :rlength
                                     :ulength
                                     :alength
                                     :slength
                                     :words 
                                     :rwords
                                     :uwords
                                     :awords
                                     :swords
                                     :l-stasks 
                                     :lambiguity 
                                     :analyses 
                                     :results
                                     :ranalyses 
                                     :rresults
                                     :uanalyses 
                                     :uresults
                                     :aanalyses 
                                     :aresults
                                     :sanalyses
                                     :sresults)
                                   (list nitems nritems
                                         (divide alength nritems)
                                         (divide arlength arresults)
                                         (divide aulength auresults)
                                         (divide aalength aaresults)
                                         (divide aslength asresults)
                                         (divide awords nritems)
                                         (divide arwords arresults)
                                         (divide auwords auresults)
                                         (divide aawords aaresults)
                                         (divide aswords asresults)
                                         (divide alstasks nritems)
                                         (divide awords alength)
                                         (divide areadings aresults)
                                         aresults
                                         (divide arreadings arresults)
                                         arresults
                                         (divide aureadings auresults)
                                         auresults
                                         (divide aareadings aaresults)
                                         aaresults
                                         (divide asreadings asresults)
                                         asresults)))
                    result)
              (incf titems nitems)
              (incf tritems nritems)
              (incf tlength alength)
              (incf trlength arlength)
              (incf tulength aulength)
              (incf talength aalength)
              (incf tslength aslength)
              (incf twords awords)
              (incf trwords arwords)
              (incf tuwords auwords)
              (incf tawords aawords)
              (incf tswords aswords)
              (incf tlstasks alstasks)
              (incf treadings areadings)
              (incf tresults aresults)
              (incf trreadings arreadings)
              (incf trresults arresults)
              (incf tureadings aureadings)
              (incf turesults auresults)
              (incf tareadings aareadings)
              (incf taresults aaresults)
              (incf tsreadings asreadings)
              (incf tsresults asresults))

      finally
        (push (cons :total
                    (pairlis '(:items :restricted
                               :i-length
                               :rlength
                               :ulength
                               :alength
                               :slength
                               :words :l-stasks
                               :rwords
                               :uwords
                               :awords
                               :swords
                               :lambiguity
                               :analyses
                               :results
                               :ranalyses 
                               :rresults
                               :uanalyses 
                               :uresults
                               :aanalyses 
                               :aresults
                               :sanalyses 
                               :sresults)
                             (list titems tritems
                                   (divide tlength tritems)
                                   (divide trlength trresults)
                                   (divide tulength turesults)
                                   (divide talength taresults)
                                   (divide tslength tsresults)
                                   (divide twords tritems)
                                   (divide tlstasks tritems)
                                   (divide trwords trresults)
                                   (divide tuwords turesults)
                                   (divide tawords taresults)
                                   (divide tswords tsresults)
                                   (divide twords tlength)
                                   (divide treadings tresults)
                                   tresults 
                                   (divide trreadings trresults)
                                   trresults
                                   (divide tureadings turesults)
                                   turesults
                                   (divide tareadings taresults)
                                   taresults
                                   (divide tsreadings tsresults)
                                   tsresults)))
              result)
        (return (delete :all result :key #'first))))

(defun analyze-competence (&optional (language *tsdb-data*)
                           &key (condition *statistics-select-condition*)
                                (wf 1) division file append (format :latex)
                                restrictor meter)
  (declare (ignore restrictor))

  (let* ((stream (create-output-stream file append))
         (division (unless (or (null division) (equal division ""))
                     (if (or (null condition) (equal condition ""))
                       division
                       (format nil "(~a) and (~a)" condition division))))
         (imeter (if division (madjust * meter 0.5) meter))
         (dmeter (when division 
                   (madjust + (madjust * meter 0.5) (mduration imeter))))
         (items (if (stringp language) 
                  (analyze-aggregates language :condition condition
                                      :meter imeter :format format) 
                  language))
         (ditems (when (and division (stringp language))
                   (analyze-aggregates language :condition division
                                       :meter dmeter :format format)))
         
         (averages 
          (if ditems
            (summarize-competence-parameters-by-division 
             items ditems :restrictor #'(lambda (foo) 
                                          (not (= (get-field :i-wf foo) wf))))
            (summarize-competence-parameters 
             items :restrictor #'(lambda (foo) 
                                   (not (= (get-field :i-wf foo) wf))))))
         (naggregates (- (length averages) 1))
         (ncolumns 8)
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
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
            & {\\bf distinct} & {\\bf total} & {\\bf overall}\\\\~%  ~
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
        "flags 1~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
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
        "cell 1 1 -contents {~a} -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"~a\\nitems\\n#\" -format title~%~
         cell 1 4 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 5 -contents \"lexical\\nitems\\n\\330\" -format title~%~
         cell 1 6 -contents \"distinct\\nanalyses\\n\\330\" -format title~%~
         cell 1 7 -contents \"total\\nresults\\n#\" -format title~%~
         cell 1 8 -contents \"overall\\ncoverage\\n%\" -format title~%~%"
        alabel (if (= wf 1) "positive" "negative"))))
    (loop
        for aggregate in items
        for i from 2
        for data = (rest (assoc (first aggregate) averages))
        when data do
          (let* ((name (if (equal format :latex)
                         (latexify-string (second aggregate))
                         (second aggregate)))
                 (items (get-field :items data))
                 (restricted (get-field :restricted data))
                 (length (get-field :i-length data))
                 (words (get-field :words data))
                 (analyses (get-field :analyses data))
                 (results (get-field :results data))
                 (coverage (if (zerop restricted)
                             0
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
                "cell ~d 1 -contents {~a} -format aggregate~%~
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
                i coverage)))))
    
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
            "cell ~d 1 -contents {~a} -format total~%~
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
            (if *statistics-analogy-aggregation-p*
              (aggregate-by-analogy nlanguage oitems
                                    :condition condition :meter nmeter)
              (analyze-aggregates nlanguage :condition condition
                                  :meter nmeter :format format))
            nlanguage))
         (ncolumns 9)
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
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
            & {\\bf lexical} & {\\bf analyses} ~
            & {\\bf in} & {\\bf out}~%    ~
            & {\\bf lexical} & {\\bf analyses} ~
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
           "flags 1~%~
            layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
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
           "cell 1 1 -contents {~a} -format title~%~
            region 1 1 2 1 -contents {~a} -format title ~
              -hor_justify left -ver_justify center~%~
            region 1 2 1 5 -contents {~a} -format title -hor_justify center~%~
            region 1 6 1 9 -contents {~a} -format title -hor_justify center~%"
           alabel alabel olabel nlabel)
          (do ((labels '("lexical\\n\\330" "analyses\\n\\330" 
                         "in\\n\\330" "out\\n\\330"
                         "lexical\\n\\330" "analyses\\n\\330" 
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
               "cell ~d 1 -contents {~a} -format aggregate~%~
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
             "cell ~d 1 -contents {~a} -format total~%~
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

(defun intersect-results (oitem nitem fields subsetp bestp)
  (loop
      with oresults = (get-field :results oitem)
      with best = (when bestp
                    (let ((ranks (get-field :ranks oitem)))
                      (loop
                          for rank in ranks
                          for foo = (get-field :rank rank)
                          thereis (when (eql foo 1) 
                                    (get-field :result-id rank)))))
      with nresults = (get-field :results nitem)
      for field in fields
      for predicate = (find-attribute-predicate field)
      for ovalues = (loop 
                        for result in oresults 
                        when (or (null bestp) 
                                 (eql (get-field :result-id result) best))
                        collect (get-field field result))
      for nvalues = (loop 
                        for result in nresults 
                        collect (get-field field result))
      for common = nil
      for oplus = (loop
                      for ovalue in ovalues
                      unless (member ovalue nvalues :test-not predicate)
                      collect ovalue
                      else do (push ovalue common)
                      do 
                        (setf nvalues
                          (delete ovalue nvalues 
                                  :count 1 :test-not predicate)))

      collect (list oplus common (unless (and subsetp (null oplus)) nvalues))))

(defun compare-in-detail (olanguage nlanguage
                          &key (condition *statistics-select-condition*)
                               (show '(:i-input :i-wf))
                               (compare '(:words :readings))
                               (sloppyp *statistics-detail-sloppy-alignment-p*)
                               (format :tcl)
                               (olabel "(g)old") 
                               (nlabel "new")
                               subsetp bestp 
                               (analogyp *statistics-analogy-aggregation-p*)
                               file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (show (delete :i-id (if (atom show) (list show) show)))
         (shows (+ (length show) (if sloppyp 2 1)))
         (compare (if (atom compare) (list compare) compare))
         (thorough (nreverse (intersection '(:derivation :mrs :tree) compare)))
         (compare (set-difference compare thorough :test #'equal))
         (predicates 
          (loop for field in compare collect (find-attribute-predicate field)))
         (compares (length compare))
         (oitems
          (if (stringp olanguage) 
            (analyze olanguage 
                     :condition condition :thorough thorough
                     :gold (when bestp olanguage) :sloppyp bestp
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
       ;; get the table header printed: number of columns, justification,
       ;; and labels depend on the attributes asked for.
       ;;
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "flags 0~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%"
        (if sloppyp 2 1) shows)
       (if sloppyp
         (format
          stream
          "cell 1 1 -contents {<} -format title~%~
           region 1 1 1 2 -contents {i-id} ~
             -format title -hor_justify center~%~
           cell 2 1 -contents {<} -format title~%~
           region 2 1 2 1 -contents {<} ~
             -format title -hor_justify center~%~
           cell 2 2 -contents {>} -format title~%~
           region 2 2 2 2 -contents {>} ~
             -format title -hor_justify center~%")
         (format
          stream
          "cell 1 1 -contents {i-id} -format title~%~
           region 1 1 2 1 -contents {i-id} ~
             -format title -hor_justify center~%"))
       (unless (zerop compares)
         (format
          stream
          "layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
           region 1 ~d 1 ~d -contents {~a} -format title ~
             -hor_justify center~%~
           region 1 ~d 1 ~d -contents {~a} -format title ~
             -hor_justify center~%"
          (+ shows compares) (+ shows compares compares)
          (+ shows 1) (+ shows compares) olabel
          (+ shows compares 1) (+ shows compares compares) nlabel))
          
       (do ((show show (rest show))
            (i (if sloppyp 3 2) (+ i 1)))
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
          "cell 1 ~d -contents {~(~a~)} -format title~%~
           region 1 ~d 2 ~d -contents {~(~a~)} -format title ~
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
          "cell 2 ~d -contents {~(~a~)} -format title~%~
           region 2 ~d 2 ~d -contents {~(~a~)} -format title ~
             -hor_justify center~%"
          i (first compare)
          i i (first compare)))
       
       (loop
           for field in thorough
           for i from (+ shows compares compares 1) by 3
           do
             (format
              stream
              "region 1 ~d 1 ~d -contents {~(~a~)} -format title ~
               -hor_justify center~%"
              i (+ i 2) field)
             (format
              stream
              "cell 2 ~d -contents {~(~a~)} -format title~%~
               cell 2 ~d -contents {~(~a~)} -format title~%~
               cell 2 ~d -contents {~(~a~)} -format title~%"
              i "<" (+ i 1) "=" (+ i 2) ">")
             (format
              stream
              "layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%"
              (+ i 2)))))
    
    ;;
    ;; my first loop() (if bernd knew |:-) (28-jul-98 - oe@csli)
    ;;
    (loop
        with ooffset = 0 with noffset = 0
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
             (unless (zerop (- row 3))
               (format
                stream
                "cell ~d 1 -contents {~a} -format data~%~
                 layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                row (- row 3) (- row 1))
               (incf row))
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
                              (or (when (eq attribute :i-input)
                                    (get-field :o-input oitem))
                                  (get-field attribute oitem)))
                          show))
                 (nshow (map 'list 
                          #'(lambda (attribute)
                              (or (when (eq attribute :i-input)
                                    (get-field :o-input nitem))
                                  (get-field attribute nitem)))
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
            #+:cdebug
            (format
             t
             "[~d ~d] (~d ~a) (~d ~a)"
             ooffset noffset
             oi-id (or (get-field :o-input oitem) (get-field :i-input oitem))
             ni-id (or (get-field :o-input nitem) (get-field :i-input nitem)))
            ;;
            ;; in `align' mode: when necessary, determine distance to cloesest
            ;; alignment point (identical :i-input values); if there is none,
            ;; output all remaining items from `old' first, then `new'.
            ;;
            (when (and sloppyp 
                       (or (and (null ooffset) (null noffset))
                           (and ooffset (zerop ooffset)
                                noffset (zerop noffset)
                                (not (equal oshow nshow)))))
              (setf ooffset 
                (loop
                    with oi-input = (get-field :i-input oitem)
                    for nitem in nitems
                    for ni-input = (get-field :i-input nitem)
                    for i from 0 to *statistics-detail-alignment-bracket*
                    thereis (and (equal oi-input ni-input) i)))
              (setf noffset 
                (loop
                    with ni-input = (get-field :i-input nitem)
                    for oitem in oitems
                    for oi-input = (get-field :i-input oitem)
                    for i from 0 to *statistics-detail-alignment-bracket*
                    thereis (and (equal oi-input ni-input) i)))
              (if (and ooffset noffset)
                (if (< ooffset noffset)
                  (setf noffset nil)
                  (setf ooffset nil))
                (when (and (null ooffset) (null noffset))
                  (setf ooffset 1)
                  (setf noffset 1))))
            #+:cdebug 
            (format t " --- [~d ~d]~%" ooffset noffset)


            (cond 
             ((if sloppyp 
                (and ooffset (zerop ooffset) noffset (zerop noffset))
                (and (eql oi-id ni-id) (equal oshow nshow)))
              ;;
              ;; two items of same identifier have equal values for all 
              ;; .show. attributes (as they should |:-)
              ;;
              (setf clashes 
                (intersect-results oitem nitem thorough 
                                               subsetp bestp))
              (when (or (loop
                            for clash in clashes
                            thereis (or (first clash) (third clash)))
                        (loop
                            for predicate in predicates 
                            for ovalue in ocompare
                            for nvalue in ncompare
                            when (funcall predicate ovalue nvalue)
                            do (return t)
                            finally (return nil)))
                (case format
                  (:tcl
                   (format
                    stream
                    "cell ~d 1 -contents {~a} -format data~%"
                    row oi-id)
                   (when sloppyp
                     (format
                      stream
                      "cell ~d 2 -contents {~a} -format data~%"
                      row ni-id))
                   (loop
                       for j from (if sloppyp 3 2)
                       for key in show for value in oshow
                       when (and (eq key :i-input) (stringp olanguage)) do
                         (format
                          stream
                          "cell ~d ~d -contents {~a} ~
                             -format data -key ~d -source {~a}~%"
                          row j (tcl-escape-braces value) oi-id olanguage)
                       else do
                         (format
                          stream
                          "cell ~d ~d -contents {~a} -format data~%"
                          row j (tcl-escape-braces value)))
                   (loop
                       for value in (append ocompare ncompare)
                       for j from (+ shows 1)
                       do
                         (format
                          stream
                          "cell ~d ~d -contents {~a} -format data~%"
                          row j (tcl-escape-braces value)))
                   (loop
                       for j from (+ shows compares compares 1) by 3
                       for field in thorough
                       for (oclash common nclash) in clashes
                       for otag = (intern (gensym "") :keyword)
                       for ctag = (intern (gensym "") :keyword)
                       for ntag = (intern (gensym "") :keyword)
                       do
                         ;;
                         ;; _fix_me_
                         ;; this creates a potential memory leak: as soon as
                         ;; the window for this table is destroyed, there 
                         ;; will be no further reference to the (tag) 
                         ;; symbols used to store data on the lisp side.  
                         ;; yet, the values associated with the symbol 
                         ;; properties will never become unbound.
                         ;;                        (11-mar-99  - oe@csli)
                         ;;
                         (setf (get :source otag) olanguage)
                         (setf (get :contrast otag) nlanguage)
                         (setf (get :i-id otag) oi-id)
                         (setf (get :i-input otag) 
                           (or (get-field :o-input oitem)
                               (get-field :i-input oitem)))
                         (setf (get :field otag) field)
                         (setf (get :value otag) oclash)
                         (setf (get :source ctag) nlanguage)
                         (setf (get :contrast ctag) olanguage)
                         (setf (get :i-id ctag) ni-id)
                         (setf (get :i-input ctag)
                           (or (get-field :o-input nitem)
                               (get-field :i-input nitem)))
                         (setf (get :field ctag) field)
                         (setf (get :value ctag) common)
                         (setf (get :source ntag) nlanguage)
                         (setf (get :contrast ntag) olanguage)
                         (setf (get :i-id ntag) ni-id)
                         (setf (get :i-input ntag)
                           (or (get-field :o-input nitem)
                               (get-field :i-input nitem)))
                         (setf (get :field ntag) field)
                         (setf (get :value ntag) nclash)
                         (format
                          stream
                          "cell ~d ~d -contents ~d -format data ~
                           -action browse -tag ~a~%~
                           cell ~d ~d -contents ~d -format data ~
                           -action browse -tag ~a~%~
                           cell ~d ~d -contents ~d -format data ~
                           -action browse -tag ~a~%"
                          row j (length oclash) otag
                          row (+ j 1) (length common) ctag
                          row (+ j 2) (length nclash) ntag))))
                (incf row))
              (pop oitems)
              (pop nitems))
             ((if sloppyp
                (or (null ni-id) (and noffset (> noffset 0)))
                (or (null ni-id) (and oi-id (<= oi-id ni-id))))
              ;;
              ;; if .oi-id. is less or equal (which it should not) to 
              ;; .ni-id. output .compare. values for `old' item and continue
              ;;
              (unless analogyp
                (setf clashes 
                  (intersect-results oitem nil thorough nil bestp))
                (case format
                  (:tcl
                   (format
                    stream
                    "cell ~d ~d -contents {~a} -format data~%"
                    row 1 oi-id)
                   (loop
                       for j from (if sloppyp 3 2)
                       for key in show for value in oshow
                       do
                         (if (and (eq key :i-input) (stringp olanguage))
                           (format
                            stream
                            "cell ~d ~d -contents {~a} ~
                             -format data -key ~d -source {~a}~%"
                            row j (tcl-escape-braces value) oi-id olanguage)
                           (format
                            stream
                            "cell ~d ~d -contents {~a} -format data~%"
                            row j (tcl-escape-braces value))))
                   (loop
                       for value in ocompare for j from (+ shows 1)
                       do
                         (format
                          stream
                          "cell ~d ~d -contents {~a} -format data~%"
                          row j (tcl-escape-braces value)))
                   (loop
                       for j from (+ shows compares compares 1) by 3
                       for field in thorough
                       for (oclash common nclash) in clashes
                       for otag = (intern (gensym "") :keyword)
                       do
                         (setf nclash nclash)
                         (setf common common)
                         (setf (get :source otag) olanguage)
                         (setf (get :contrast otag) nlanguage)
                         (setf (get :i-id otag) oi-id)
                         (setf (get :i-input otag)
                           (or (get-field :o-input oitem)
                               (get-field :i-input oitem)))
                         (setf (get :field otag) field)
                         (setf (get :value otag) oclash)
                         (format
                          stream
                          "cell ~d ~d -contents ~d -format data ~
                           -action browse -tag ~a~%"
                          row j (length oclash) otag))))
                (incf row))
              (pop oitems)
              (when noffset 
                (when (and (zerop (decf noffset)) (null ooffset))
                  (setf ooffset 0))))
             (t
              ;;
              ;; otherwise (.ni-id. is less than .oi-id.) output .compare.
              ;; values for it and leave `old' item to next iteration
              ;;
              (unless analogyp
                (setf clashes 
                  (intersect-results nil nitem thorough nil nil))
                (case format
                  (:tcl
                   (format
                    stream
                    "cell ~d ~d -contents {~a} -format data~%"
                    row (if sloppyp 2 1) ni-id)
                   (loop
                       for j from (if sloppyp 3 2)
                       for key in show for value in nshow
                       do
                         (if (and (eq key :i-input) (string nlanguage))
                           (format
                            stream
                            "cell ~d ~d -contents {~a} ~
                             -format data -key ~d -source {~a}~%"
                            row j (tcl-escape-braces value) ni-id nlanguage)
                           (format
                            stream
                            "cell ~d ~d -contents {~a} -format data~%"
                            row j (tcl-escape-braces value))))
                   (loop
                       for value in ncompare for j from (+ shows compares 1)
                       do
                         (format
                          stream
                          "cell ~d ~d -contents {~a} -format data~%"
                          row j (tcl-escape-braces value)))
                   (loop
                       for j from (+ shows compares compares 1) by 3
                       for field in thorough
                       for (oclash common nclash) in clashes
                       for ntag = (intern (gensym "") :keyword)
                       do
                         (setf oclash oclash)
                         (setf common common)
                         (setf (get :source ntag) nlanguage)
                         (setf (get :contrast ntag) olanguage)
                         (setf (get :i-id ntag) ni-id)
                         (setf (get :i-input ntag)
                           (or (get-field :o-input nitem)
                               (get-field :i-input nitem)))
                         (setf (get :field ntag) field)
                         (setf (get :value ntag) nclash)
                         (format
                          stream
                          "cell ~d ~d -contents ~d -format data ~
                           -action browse -tag ~a~%"
                          row (+ j 2) (length nclash) ntag))))
                (incf row))
              (pop nitems)
              (when ooffset 
                (when (and (zerop (decf ooffset)) (null noffset))
                  (setf noffset 0)))))))
    
    (when (or (stringp file) (stringp append)) (close stream))))

(defun browse-results (data
                       &key (condition *statistics-select-condition*)
                            (format :tcl)
                            file append meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  ;;
  ;; _fix_me_
  ;; with large numbers of results, retrieval of all trees or MRSs can take
  ;; a long time, so maybe get user confirm first          (28-oct-03; oe)
  ;;
  (let* ((thorough '(:derivation :mrs :tree))
         (condition (if condition
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items
          (if (stringp data) 
            (analyze data 
                     :condition condition :thorough thorough
                     :filter t :meter meter :message t)
            data))
         (message (format nil "generating `~a' result view ..." data))
         (stream (create-output-stream file append))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo)))))

    (when meter
      (when (> (length items) *statistics-critical-line-threshold*)
        (send-to-podium "tsdb_beep" :wait t)
        (let* ((prompt (format
                        nil
                        "yes-or-no-p {table generation may be slow ~
                         (~d items); continue}"
                        (length items)))
               (result (send-to-podium prompt :wait t)))
          (when (and (eq (first result) :ok) (not (= (second result) 1)))
            (when (or (stringp file) (stringp append)) (close stream))
            (return-from browse-results nil))))
      (status :text message)
      (meter :value 0))
    (case format
      (:tcl
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "flags 1~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j center~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout col 2 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 6 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         cell 1 1 -contents {i-id} -format title~%~
         cell 1 2 -contents {i-input} -format title~%~
         cell 1 3 -contents {readings} -format title~%~
         cell 1 4 -contents {derivation} -format title~%~
         cell 1 5 -contents {mrs} -format title~%
         cell 1 6 -contents {tree} -format title~%"
        (+ (length items) 1))))
    (loop
        with increment = (and meter (/ 1 (if items (length items) 1)))
        with separator = 1
        with indices = (loop
                           with indices
                           for field in '(:i-id :i-input :o-input 
                                          :readings :results)
                           for i from 0
                           do
                             (setf (getf indices field) i)
                           finally (return indices))
        for row from 2 by 1
        for item in items
        for values = (loop
                         with values = (make-array 5)
                         for pair in item
                         for key = (first pair)
                         when (getf indices key) do
                           (setf (aref values (getf indices key)) (rest pair))
                         finally (return values))
          
        for i-id = (aref values (getf indices :i-id))
        for i-input = (aref values (getf indices :i-input))
        for o-input = (aref values (getf indices :o-input))
        for readings = (aref values (getf indices :readings))
        for results = (aref values (getf indices :results))
        for derivations = (loop
                              for result in results
                              for derivation = (get-field :derivation result)
                              when derivation collect derivation)
        for mrss = (loop
                       for result in results
                       for mrs = (get-field :mrs result)
                       when (and mrs (not (equal mrs ""))) collect mrs)
        for trees = (loop
                        for result in results
                        for tree = (get-field :tree result)
                        when (and tree (not (equal tree ""))) collect tree)
        for otag = (intern (gensym "") :keyword)
        for dtag = (intern (gensym "") :keyword)
        for mtag = (intern (gensym "") :keyword)
        for ttag = (intern (gensym "") :keyword)
        when (= (- row 1) (* separator 10))
        do 
          (case format
            (:tcl
             (format
              stream
              "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              row)))
          (incf separator)
        do
          (when increment (meter-advance increment))
          ;;
          ;; _fix_me_ this creates a potential memory leak: as soon as the
          ;; window for this table is destroyed, there will be no further
          ;; reference to the (tag) symbols used to store data on the lisp
          ;; side.  yet, the values associated with the symbol properties will
          ;; never become unbound.                              (11-apr-00)
          ;;
          (setf (get :source otag) data)
          (setf (get :i-id otag) i-id)
          (setf (get :i-input otag) (or o-input i-input))
          (setf (get :field otag) :o-input)
          (setf (get :value otag) i-input)
          (setf (get :source dtag) data)
          (setf (get :i-id dtag) i-id)
          (setf (get :i-input dtag) (or o-input i-input))
          (setf (get :field dtag) :derivation)
          (setf (get :value dtag) derivations)
          (setf (get :source mtag) data)
          (setf (get :i-id mtag) i-id)
          (setf (get :i-input mtag) (or o-input i-input))
          (setf (get :field mtag) :mrs)
          (setf (get :value mtag) mrss)
          (setf (get :source ttag) data)
          (setf (get :i-id ttag) i-id)
          (setf (get :i-input ttag) (or o-input i-input))
          (setf (get :field ttag) :tree)
          (setf (get :value ttag) trees)
          (case format
            (:tcl
             (format stream "cell ~d 1 -contents {~a} -format data~%" row i-id)
             (if (stringp data)
               (format 
                stream
                "cell ~d 2 -contents {~a} -format data -key ~d -source {~a}~
                      ~:[~*~; -action browse -stag ~a~]~%"
                row (tcl-escape-braces (or o-input i-input)) i-id data 
                o-input otag)
               (format 
                stream
                "cell ~d 2 -contents {~a} -format data~%"
                row (tcl-escape-braces (or o-input i-input))))
             (format
              stream
              "cell ~d 3 -contents {~a} -format data~%~
               cell ~d 4 -contents {~a} -format data -action browse -tag ~a~%~
               cell ~d 5 -contents {~a} -format data -action browse -tag ~a~%~
               cell ~d 6 -contents {~a} -format data -action browse -tag ~a~%"
              row readings
              row (length derivations) dtag
              row (length mrss) mtag
              row (length trees) ttag)))
        finally
          (case format
            (:tcl
             (format
              stream
              "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
               layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
              (- row 1) row)
             (format
              stream
              "cell ~d 1 -contents {~a} -format total~%~
               cell ~d 2 -contents {-} -format total~%~
               cell ~d 3 -contents {-} -format total~%~
               cell ~d 4 -contents {-} -format total~%~
               cell ~d 5 -contents {-} -format total~%~
               cell ~d 6 -contents {-} -format total~%"
              row (- row 2) row row row row row))))
             
    (when meter
      (status :text (format nil "~a done" message) :duration 10)
      (meter :value 1))

    (when (or (stringp file) (stringp append)) (close stream))
    (length items)))

(defun execute-tag (action tag &key (format :tcl) file append)

  (let* ((tag (intern tag :keyword)))

    (case action
      (:browse
       (let* ((clashes (get :value tag))
              (field (get :field tag))
              (i-input (get :i-input tag))
              (browser (find-attribute-browser field))
              (stream (and clashes (null browser)
                           (create-output-stream file append))))
         (when clashes
           (case format
             (:tcl
              (when stream
                (when *statistics-tcl-formats*
                  (format stream *statistics-tcl-formats*))
                (format
                 stream
                 "flags 0~%~
                  layout col def -m1 5 -r 2 -m2 5 -c black -j center~%~
                  layout row def -m1 5 -r 1 -m2 5 -c black -j center~%~
                  layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%"))
              (cond
               ((stringp clashes)
                (format
                 stream
                 "cell 1 1 -contents {~a} -format title~%~
                  layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%"
                 clashes))
               ;;
               ;; _fix_me_
               ;; we need a general way of distinguishing browsers that take a
               ;; set of results rather than one at a time.    (30-oct-03; oe)
               ;;
               #+:mt
               ((and (eq field :mrs)
                     (string-equal 
                      (gethash :mrs *statistics-browsers*) "mt::browse-mrss"))
                (funcall browser clashes i-input))
               (t
                (loop
                    with *print-pretty* = nil
                    with *print-case* = :downcase
                    for clash in clashes
                    for i from 1
                    for ntag = (unless browser (intern (gensym "") :keyword))
                    when browser do
                      (funcall browser clash i-input)
                    else do
                      (setf (get :i-id ntag) (get :i-id tag))
                      (setf (get :i-input ntag) (get :i-input tag))
                      (setf (get :source ntag) (get :source tag))
                      (setf (get :value ntag) clash)
                      (format
                       stream
                       "cell ~d 1 -contents {~a} -format title ~
                             -action reconstruct -tag ~a~%"
                       i (tcl-escape-braces clash) ntag)
                    finally
                      (when stream
                        (format
                         stream
                         "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                         i)))))))
           (when (and stream (or (stringp file) (stringp append)))
             (close stream)))))

      (:inspect
       (labels ((read-score (rank)
                  (let* ((score (get-field :score rank))
                         (score (cond
                                 ((numberp score) score)
                                 ((stringp score)
                                  (ignore-errors (read-from-string score))))))
                    (when score
                      (setf (get-field :score rank) score))
                    rank)))
         (let* ((i-id (get :i-id tag))
                (condition (format nil "i-id == ~a" i-id))
                (source (get :source tag))
                (match (get :match tag))
                (match (when match (read-score match)))
                (errors (get :errors tag))
                (errors (loop
                            for error in errors
                            collect (read-score error)))
                (others (get :others tag))
                (others (loop
                            for other in others
                            collect (read-score other)))
                (inspect (list errors match others)))
           (browse-trees 
            source :condition condition :interactive t :inspect inspect))))

      (:reconstruct
       (let* ((i-id (get :i-id tag))
              (i-input (get :i-input tag))
              (derivation (get :value tag))
              (derivation (if (stringp derivation) 
                            (read-from-string derivation) 
                            derivation)))
         (reconstruct-item i-id i-input derivation))))))

(defun summarize-performance-parameters 
    (items 
     &key (fields *statistics-performance-summary*)
          (extras *statistics-extra*)
          restrictor (format 9902))
  
  (loop
      with fields = (append fields extras)
      with nfields = (length fields)
      with rank = (- nfields 1)
      with values = (make-array nfields :element-type 'integer)
      with totals = (make-array nfields :element-type 'integer 
                                :initial-element 0)
      with aitems = 0
      with aanalyzed = 0
      with result = nil
      for class in items
      for items = 0
      for analyzed = 0
      for data = (rest (rest class)) 
      finally (let ((total
                     (cons 
                      :total
                      (nconc (pairlis '(:items :readings) 
                                      (list aitems aanalyzed))
                             (pairlis 
                              fields 
                              (loop
                                  for field in fields
                                  for value across totals
                                  when (eq field :first)
                                  collect (divide value aanalyzed)
                                  else collect (divide value aitems)))))))
                (return (cons total (remove :all result :key #'first))))
      do
        (loop for i from 0 to rank do (setf (aref values i) 0))
        (loop
            for tuple in data
            for readings = (get-field :readings tuple)
            unless (or (minus-one-p readings) 
                       (and restrictor (funcall restrictor tuple))) do
              (incf items)
              (when (> readings 0) (incf analyzed))
              (loop
                  with tgc = (let ((tgc (get-field+ :tgc tuple -1)))
                               (if (minus-one-p tgc) 0 tgc))
                  for i from 0
                  for field in fields
                  for value = (get-field+ field tuple -1)
                  when (and (not (minus-one-p value))
                            (member field *statistics-time-fields*)) do
                    (setf value 
                      (if (member field *statistics-exclude-tgc-p*)
                        (convert-time (- value tgc) format)
                        (convert-time value format)))
                  when (eq field :space) do
                    (let* ((conses (get-field+ :conses tuple -1))
                           (symbols (get-field+ :symbols tuple -1))
                           (others (get-field+ :others tuple -1)))
                      (if (and (minus-one-p conses) 
                               (minus-one-p symbols)
                               (minus-one-p others))
                        (setf value -1)
                        (setf value
                          (if (>= format 9903)
                            (+ (if (minus-one-p conses) 0 conses)
                               (if (minus-one-p symbols) 0 symbols) 
                               (if (minus-one-p others) 0 others))
                            (+ (* (if (minus-one-p conses) 0 conses) 8)
                               (* (if (minus-one-p symbols) 0 symbols) 24)
                               (if (minus-one-p others) 0 others))))))
                  when (eq field :first) do
                    (when (> readings 0) (incf (aref values i) value))
                  else do
                    (incf (aref values i) value)))
        (incf aitems items) (incf aanalyzed analyzed)
        (push (cons (first class)
                    (nconc (pairlis '(:items :readings) (list items analyzed))
                           (pairlis fields 
                                    (loop
                                        for i from 0
                                        for field in fields
                                        for value across values
                                        when (eq field :first)
                                        collect (divide value analyzed)
                                        and do (unless (zerop  analyzed)
                                                 (incf (aref totals i) value))

                                        else 
                                        do (incf (aref totals i) value)
                                        and collect (divide value items)))))
              result)))

(defun analyze-performance (&optional (language *tsdb-data*)
                            &key (condition *statistics-select-condition*)
                                 file append 
                                 (extras *statistics-extra*)
                                 (format :latex) (view :parser)
                                 restrictor meter)

  (let* ((items (analyze-aggregates language 
                                    :condition condition :extras extras
                                    :meter meter :format format))
         (stream (create-output-stream file append))
         (averages
          (summarize-performance-parameters 
           items :extras extras
           :restrictor restrictor :format (profile-granularity language)))
         (naggregates (- (length averages) 1))
         (ncolumns (+ 8 (if (eq format :tcl) (length extras) 0)))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
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
           "flags 1~%~
            layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
            layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
            layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
            layout col ~d  -m1 5 -r 2 -m2 5 -c black -j right~%~
            layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ (length extras) 10) (+ naggregates 1) (+ naggregates 2))
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
           alabel)
          (loop
              for extra in extras
              for i from 11
              do
                (format
                 stream
                 "cell 1 ~d -contents {~(~a~)} -format title~%"
                 i extra)))
         (:parser
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
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
            layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
           (+ naggregates 1) (+ naggregates 2) 
           (+ naggregates 3) (+ naggregates 4))
          (format
           stream
           "cell 1 1 -contents ~a -format title~%~
            cell 1 2 -contents \"items\\n#\" -format title~%~
            cell 1 3 -contents \"ftasks\\n#\" -format title~%~
            cell 1 4 -contents \"etasks\\n#\" -format title~%~
            cell 1 5 -contents \"stasks\\n#\" -format title~%~
            cell 1 6 -contents \"unifications\\n#\" -format title~%~
            cell 1 7 -contents \"copies\\n#\" -format title~%~
            cell 1 8 -contents \"aedges\\n#\" -format title~%~
            cell 1 9 -contents \"pedges\\n#\" -format title~%~
            cell 1 10 -contents \"rpedges\\n#\" -format title~%"
           alabel)))))
    (loop
        for phenomenon in items
        for i from 2
        for data = (rest (assoc (first phenomenon) averages))
        when data do
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
                 (unifications (round (get-field :unifications data)))
                 (copies (round (get-field :copies data)))
                 (aedges (round (get-field :aedges data)))
                 (pedges (round (get-field :pedges data)))
                 (edges (round (get-field :edges data)))
                 (edges (if (and edges (>= edges 0))
                          edges
                          (if (>= pedges 0) pedges -1)))
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
                   "cell ~d 1 -contents {~a} -format aggregate~%~
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
                   i space)
                  (loop
                      for extra in extras
                      for j from 11
                      for foo = (get-field+ extra data -1)
                      for value = (if (rationalp foo) (coerce foo 'float) foo)
                      do
                        (format
                         stream
                         "cell ~d ~d -contents {~a} -format data~%"
                         i j value)))
                 (:parser
                  (format
                   stream
                   "cell ~d 1 -contents {~a} -format aggregate~%~
                    cell ~d 2 -contents ~d -format data~%~
                    cell ~d 3 -contents ~,1f -format data~%~
                    cell ~d 4 -contents ~,1f -format data~%~
                    cell ~d 5 -contents ~,1f -format data~%~
                    cell ~d 6 -contents ~,1f -format data~%~
                    cell ~d 7 -contents ~,1f -format data~%~
                    cell ~d 8 -contents ~,1f -format data~%~
                    cell ~d 9 -contents ~,1f -format data~%~
                    cell ~d 10 -contents ~,1f -format data~%"
                   i name
                   i items
                   i ftasks
                   i etasks
                   i stasks
                   i unifications
                   i copies
                   i aedges
                   i pedges
                   i rpedges)))))))
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (stasks (round (get-field :p-stasks data)))
           (etasks (round (get-field :p-etasks data)))
           (ftasks (round (get-field :p-ftasks data)))
           (filter (unless (or (minus-one-p etasks) (minus-one-p ftasks))
                     (float (* 100 (divide ftasks (+ etasks ftasks))))))
           (unifications (round (get-field :unifications data)))
           (copies (round (get-field :copies data)))
           (aedges (round (get-field :aedges data)))
           (pedges (round (get-field :pedges data)))
           (edges (round (get-field :edges data)))
           (edges (if (and edges (>= edges 0))
                    edges
                    (if (>= pedges 0) pedges -1)))
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
             "cell ~d 1 -contents {~a} -format total~%~
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
             (+ naggregates 2) space)
            (loop
                for extra in extras
                for i from 11
                for foo = (get-field+ extra data -1)
                for value = (if (rationalp foo) (coerce foo 'float) foo)
                do
                  (format
                   stream
                   "cell ~d ~d -contents {~a} -format total~%"
                   (+ naggregates 2) i value)))
           (:parser
            (format
             stream
             "cell ~d 1 -contents {~a} -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%~
              cell ~d 9 -contents ~,1f -format data~%~
              cell ~d 10 -contents ~,1f -format data~%"
             (+ naggregates 2)  name
             (+ naggregates 2)  items
             (+ naggregates 2)  ftasks
             (+ naggregates 2)  etasks
             (+ naggregates 2)  stasks
             (+ naggregates 2)  unifications
             (+ naggregates 2)  copies
             (+ naggregates 2)  aedges
             (+ naggregates 2)  pedges
             (+ naggregates 2)  rpedges)
            (format
             stream
             "cell ~d 1 -contents {~a} -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%~
              cell ~d 9 -contents ~,1f -format data~%~
              cell ~d 10 -contents ~,1f -format data~%"
             (+ naggregates 3)  "Per Second"
             (+ naggregates 3)  items
             (+ naggregates 3)  (divide ftasks tcpu)
             (+ naggregates 3)  (divide etasks tcpu)
             (+ naggregates 3)  (divide stasks tcpu)
             (+ naggregates 3)  (divide unifications tcpu)
             (+ naggregates 3)  (divide copies tcpu)
             (+ naggregates 3)  (divide aedges tcpu)
             (+ naggregates 3)  (divide pedges tcpu)
             (+ naggregates 3)  (divide rpedges tcpu))
            (format
             stream
             "cell ~d 1 -contents {~a} -format aggregate~%~
              cell ~d 2 -contents ~d -format data~%~
              cell ~d 3 -contents ~,1f -format data~%~
              cell ~d 4 -contents ~,1f -format data~%~
              cell ~d 5 -contents ~,1f -format data~%~
              cell ~d 6 -contents ~,1f -format data~%~
              cell ~d 7 -contents ~,1f -format data~%~
              cell ~d 8 -contents ~,1f -format data~%~
              cell ~d 9 -contents ~,1f -format data~%~
              cell ~d 10 -contents ~,1f -format data~%"
             (+ naggregates 4)  "Per MByte"
             (+ naggregates 4)  items
             (+ naggregates 4)  (divide ftasks bytes)
             (+ naggregates 4)  (divide etasks bytes)
             (+ naggregates 4)  (divide stasks bytes)
             (+ naggregates 4)  (divide unifications bytes)
             (+ naggregates 4)  (divide copies bytes)
             (+ naggregates 4)  (divide aedges bytes)
             (+ naggregates 4)  (divide pedges bytes)
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
            (if *statistics-analogy-aggregation-p*
              (aggregate-by-analogy nlanguage oitems
                                    :condition nil :meter nmeter)
              (analyze-aggregates nlanguage :condition condition
                                  :meter nmeter :format format))
            nlanguage))
         (ncolumns 10)
         (caption (format 
                   nil "(generated by ~a at ~a)"
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
           "flags 1~%~
            layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
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
           "cell 1 1 -contents {~a} -format title~%~
            region 1 1 2 1 -contents {~a} -format title ~
              -hor_justify left -ver_justify center~%~
            region 1 2 1 4 -contents {~a} -format title -hor_justify center~%~
            region 1 5 1 7 -contents {~a} -format title -hor_justify center~%~
            region 1 8 1 10 -contents {~a} -format title -hor_justify center~%"
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
               "cell ~d 1 -contents {~a} -format aggregate~%~
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
             "cell ~d 1 -contents {~a} -format total~%~
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

(defmacro shift (value range offset)
  `(if (zerop ,offset)
     ,value
     (let* ((shift (* ,range ,offset 2))
            (increment (if (zerop shift) 0 (random shift)))
            (delta (- (* ,range ,offset) increment)))
       (+ ,value delta))))

(defun graph (data &key (condition *statistics-select-condition*)
                        division
                        file (format :tcl)
                        (dimension :words)
                        (aggregate 1) (threshold 5) lower upper
                        attributes labels symbols colours
                        title xtitle logscale meter 
                        (extras *statistics-extra*)
                        scatterp (offset 0.01))
  
  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (granularity (profile-granularity data))
         (nattributes (length attributes))
         (scatterp (and scatterp (= nattributes 1)))
         (division (if (or (null condition) (null division))
                         (unless (equal division "") division)
                         (format nil "(~a) and (~a)" condition division)))
         (ameter (if division (madjust / meter 2) meter))
         (dmeter (if division (madjust + ameter (mduration ameter)) nil))
         (aggregates 
          (aggregate data 
                     :condition condition :dimension dimension 
                     :aggregate aggregate :threshold threshold
                     :lower lower :upper upper :extras extras
                     :restrictor #'(lambda (foo)
                                     (eql (get-field :readings foo) -1))
                     :format format :meter ameter))
         (saggregates (when (and attributes aggregates)
                        (summarize-performance-parameters 
                         aggregates :extras extras
                         :format (profile-granularity data))))
         (daggregates
          (when (and (null attributes) division)
            (let ((daggregates
                   (aggregate 
                    data :condition division :dimension dimension 
                    :aggregate aggregate :threshold threshold
                    :lower lower :upper upper :extras extras
                    :restrictor #'(lambda (foo) 
                                    (eql (get-field :readings foo) -1))
                    :format format :meter dmeter)))
              (loop
                  for aggregate in aggregates
                  for key = (first aggregate)
                  for label = (second aggregate)
                  for match = (find key daggregates :key #'first)
                  collect (cons key (cons label (rest (rest match))))))))
         (values (make-array (list nattributes)))
         (points (make-array (list nattributes)))
         (units (nreverse (map 'list #'first aggregates)))
         (frequencies (map 'list 
                        #'(lambda (foo) (length (rest (rest foo))))
                        aggregates))
         (dfrequencies (loop
                           for aggregate in daggregates
                           collect (length (rest (rest aggregate)))))
         (xmin (if units (apply #'min units) 0))
         (xmax (if units (apply #'max units) 42))
         ymin ymax lsymbols llabels)

    (unless (or (null attributes) saggregates)
      (when file (close stream))
      (when (probe-file file) (delete-file file))
      (when meter
        (beep)
        (status :text "selection is empty; check the current TSQL condition"
                :duration 10))
      (return-from graph nil))
    
    (when meter (status :text "computing graph layout and geometry ..."))
    (if attributes
      (loop
          for aggregate in (rest saggregates)
          for x = (first aggregate)
          for summary = (rest aggregate)
          for data = (find x aggregates :key #'first)
          do
            (loop 
                for attribute in attributes
                for i from 0 by 1       
                for key = (if (stringp attribute)
                            (intern (string-upcase attribute) :keyword)
                            (intern attribute :keyword))
                for raw = (when scatterp
                            (loop
                                for tuple in (rest (rest data))
                                for i-id = (get-field :i-id tuple)
                                for tgc = (let ((tgc (get-field+ :tgc tuple 0)))
                                            (if (minus-one-p tgc) 0 tgc))
                                for y = (get-field+ key tuple -1)
                                unless (minus-one-p y) do
                                  (when (member key *statistics-time-fields*)
                                    (setf y
                                      (convert-time 
                                       (if (member
                                            key *statistics-exclude-tgc-p*)
                                         (- y tgc)
                                         y)
                                       granularity)))
                                  (setf ymin (if ymin (min ymin y) y))
                                  (setf ymax (if ymax (max ymax y) y))
                                and collect (list x y i-id)))
                for value = (get-field+ key summary -1)
                do (push value (aref values i))
                   (when scatterp
                     (setf (aref points i) (nconc  raw (aref points i))))
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
                  nil "(generated by ~a at ~a)"
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
            (if attributes xmin (- xmin 1)) (if attributes xmax (+ xmax 1)) 
            xdivision
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
          for raw = (nreverse (aref points i))
          for rawx = (when scatterp
                       (loop 
                           for point in raw 
                           collect (shift (first point) (- xmax xmin) offset)))
          for rawy = (when scatterp
                       (loop 
                           for point in raw 
                           collect (shift (second point) 
                                          (- ymax ymin) offset)))
          for i-ids = (when scatterp
                        (loop for point in raw collect (third point)))
          for label = (or (nth i labels) (find-attribute-label key))
          and symbol = (or (and rawx rawy (eq format :tcl) "none")
                           (nth i symbols) 
                           (find-attribute-symbol key i :format format))
          and colour = (or (nth i colours) (find-attribute-colour key i))
          do
            (case format
              (:tcl
               (when (and rawx rawy)
                 (format 
                  stream 
                  "data ids~a ~a~%" 
                  i (list2tcl i-ids :format "~d"))
                 (format 
                  stream 
                  "data xx~a ~a~%" 
                  i (list2tcl rawx :format "~,4f"))
                 (format 
                  stream 
                  "data yy~a ~a~%" 
                  i (list2tcl rawy :format "~,4f"))
                 (format
                  stream
                  "element ee~a -xdata xx~a -ydata yy~a -label \"\" \\~%  ~
                   -symbol ~a -pixel 2 -linewidth 0 \\~%  ~
                   -color ~a -fill defcolor -outline defcolor~%"
                  i i i "circle" colour))
               (format 
                stream 
                "data y~a ~a~%" 
                i (list2tcl data :format "~,8f"))
               (format
                stream
                "element e~a -xdata x -ydata y~a -label ~s \\~%  ~
                 -symbol ~a -pixel 3 -linewidth 1\\~%  ~
                 -color ~a -fill defcolor -outline defcolor~%"
                i i label symbol colour))
              (:latex
               (unless scatterp
                 (format stream "  \\multiput{~a} at~%" symbol)
                 (loop 
                     for x in units for y in data do 
                       (format stream "    ~12,4f ~12,4f~%" x y))
                 (format stream "  /~%"))
               (format stream "  \\plot~%")
               (loop 
                   for x in units for y in data do 
                     (format stream "    ~12,4f ~12,4f~%" x y))
               (format stream "  /~%")
               (when scatterp
                 (format stream "  \\multiput{~a} at~%" symbol)
                 (loop
                     for x in rawx for y in rawy do
                       (format stream "    ~12,4f ~12,4f~%" x y))
                 (format stream "  /~%"))))
            (push symbol lsymbols)
            (push label llabels))
      (case format
        (:tcl
         (format 
          stream 
          "data y ~a~%~
           element e -xdata x -ydata y ~
           -relief ~:[flat~;solid -bd 1 -fg white~]~%"
          (list2tcl (nreverse frequencies) :format "~d") dfrequencies)
         (when dfrequencies
           (format 
            stream 
            "data yy ~a~%~
           element ee -xdata x -ydata yy -relief flat~%"
            (list2tcl (nreverse dfrequencies) :format "~d"))))
        (:latex
         (let ((breadth (/ *statistics-plot-width* (* (- xmax xmin) 1.2))))
           (when dfrequencies
             (format
              stream
              "  \\setbars breadth <0mm> baseline at y = 0~%  ~
               \\linethickness=~,4fmm~%  \\plot~%"
              breadth)
             (loop
                 for x in units
                 for y in (nreverse dfrequencies)
                 do
                   (format stream "~10d ~8d~%" x y))
             (format stream "  /~%"))
           (format
            stream
            "  \\setbars breadth <~:[0mm~*~;~,4fmm~]> baseline at y = 0~%  ~
               \\linethickness=~:[\\breadth~;0.25pt~]~%  \\plot~%"
            dfrequencies breadth dfrequencies)
           (loop
               for x in units
               for y in (nreverse frequencies)
               do
                 (format stream "~10d ~8d~%" x y))
           (format stream "  /~%")))))
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
          "flags 0~%~
           layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
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
                "cell 1 ~d -contents {~(~a~)} -format title~%"
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
                "cell ~d ~d -contents {~a} -format data~%"
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
                      "cell ~d ~d -contents {~a} -format data~%"
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
                      "cell ~d ~d -contents {~a} -format total~%"
                      (+ i 1) j (get-field attribute totals))))))
      (force-output stream)
      (when file (close stream))
      (pairlis '(:names 
                 :passives :actives :successes :executed :filtered)
               (list (map 'list #'first rules)
                     passives actives successes executed filtered)))))
