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

(defparameter *statistics-plot-width* 150)

(defparameter *statistics-plot-height* 150)

(defparameter *statistics-tcl-formats*
  "format title -font {helvetica 12 bold} -fill black -justify center~%~
   format aggregate -font {helvetica 12 bold} -fill black -justify center~%~
   format data -font {helvetica 12} -fill black -justify center~%~
   format total -font {helvetica 12 bold} -fill black -justify center~%~%")

(defparameter *phenomena*
  (list "S_Types"
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

(defun latexify-string (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\_)
        (concatenate 'string "\\_" (latexify-string (subseq string 1)))
        (concatenate 
            'string (string prefix) (latexify-string (subseq string 1)))))
    string))

(defmacro average (values)
  `(let ((length (length ,values)))
     (if (zerop length) 0 (/ (apply #'+ ,values) length))))

(defmacro divide (numerator denominator)
  `(if (zerop ,denominator) 0 (/ ,numerator ,denominator)))

(defmacro sum (values)
  `(apply #'+ ,values))

(defun create-output-stream (file &optional append)
  (cond
   ((or (stringp file) (stringp append))
    (open (if (stringp append) append file)
          :direction :output 
          :if-exists (if append :append :supersede)
          :if-does-not-exist :create))
   ((or file append) (or file append))
   (t *tsdb-io*)))

(defun analyze (condition language &key meter sort message)

  (let* ((message (when message
                    (format nil "retrieving `~a' data ..." language)))
         (key (if (or (null condition) (equal condition ""))
                language
                (concatenate 'string language "@" condition)))
         data)
    (when meter
      (when message (status :text message))
      (meter :value (get-field :start meter)))
    (loop while (eq (setf data (gethash key *tsdb-profile-cache*)) :seized))
    (unless data
      (setf (gethash key *tsdb-profile-cache*) :seized)
      (unwind-protect
        (let* ((pmeter (madjust * meter 0.5))
               (imeter 
                (madjust + (madjust * meter 0.25) (mduration pmeter)))
               (ameter 
                (madjust + (madjust * meter 0.25) (+ (mduration pmeter)
                                                     (mduration imeter))))
               (parse (select (list "i-id" "readings" "first" "total"
                                    "p-etasks" "p-stasks" "p-ftasks"
                                    "unifications" "copies"
                                    "conses" "symbols" "others"
                                    "words" "l-stasks"
                                    "aedges" "pedges" "redges" 
                                    "gcs" "error")
                              (list :integer :integer :integer :integer 
                                    :integer :integer :integer 
                                    :integer :integer
                                    :integer :integer :integer 
                                    :integer :integer
                                    :integer :integer :integer
                                    :integer :string)
                              "parse" condition language 
                              :redirection :output :meter pmeter :sort sort))
               (item (select (list "i-id" "i-input" "i-length" "i-wf")
                             (list :integer :string :integer :integer)
                             "item" condition language 
                             :redirection :output :meter imeter :sort sort))
               (all (join parse item :i-id :meter ameter)))
          (setf data all))
        (setf (gethash key *tsdb-profile-cache*) data)))
    (when meter 
      (meter :value (get-field :end meter))
      (when message 
        (status :text (format nil "~a done" message) :duration 2)))
    data))

(defun load-cache (&key (home *tsdb-home*) name pattern trace meter background)
  (if #+allegro background #-allegro t
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
          (analyze nil name)
          (analyze-phenomena name))))))

#+:cray
(defun analyze (condition &optional (language *tsdb-data*))
  (let* ((all (select (list "i-id" "i-length" "i-wf" "p-name"
                            "readings" "first" "total"
                            "p-etasks" "p-stasks" "p-ftasks"
                            "unifications" "copies"
                            "conses" "symbols" "others"
                            "words" "edges" "gcs")
                      (list :integer :integer :integer :string
                            :integer :integer :integer 
                            :integer :integer :integer 
                            :integer :integer
                            :integer :integer :integer 
                            :integer :integer :integer)
                      (list "item" "item-phenomenon" "phenomenon" "parse")
                      condition language :redirection :output)))
    all))

;;;
;;; needs major speedup; should probably use raw data (with phenomenon name)
;;; and sort out the phenomenon grouping itself.           (14-dec-97  -  oe)
;;;
(defun analyze-phenomena (language
                          &key phenomena meter)

  (let* ((phenomena (or phenomena 
                        (gethash language *tsdb-phenomena*)
                        *phenomena*))
         (increment (madjust / meter (length phenomena)))
         (duration (mduration increment))
         (message (format nil "retrieving `~a' data ..." language))
         items)
    (when meter
      (status :text message)
      (meter :value (get-field :start meter)))
    (do* ((phenomena (reverse phenomena) (rest phenomena))
          (phenomenon (first phenomena) (first phenomena))
          (meter increment (madjust + meter (mduration increment))))
        ((null phenomena) items)
      (let* ((condition (format nil "~@[p-name ~~ \"^~a\"~]" phenomenon))
             (data (analyze condition language :meter meter)))
        (when data
          (push (cons (intern (string-upcase (or phenomenon "all")) "KEYWORD")
                      (cons (or phenomenon "All") data))
                items)))
      (when meter 
        (meter :value (get-field :end meter))))
    (when meter
      (meter :value (get-field :end meter))
      (status :text (format nil "~a done" message :duration 2)))
    items))

(defun summarize-coverage-parameters (items
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
                                      (get-field :l-stasks foo))
                            data))
             (parses (remove-if-not #'(lambda (foo)
                                    (>= (get-field :readings foo) 1))
                                    data))
             (readingss (map 'list #'(lambda (foo)
                                       (get-field :readings foo))
                             parses)))
        (push (cons (first phenomenon)
                    (pairlis '(:items :restricted :i-length 
                               :words :l-stasks
                               :analyses :results)
                             (list items restricted (average lengths) 
                                   (average wordss) (average lstaskss)
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
                           :analyses
                           :results)
                         (list itemtotal restrictedtotal
                               (divide lengthtotal restrictedtotal)
                               (divide wordstotal restrictedtotal)
                               (divide lstaskstotal restrictedtotal)
                               (divide readingstotal parsestotal)
                               parsestotal)))
          result)))

(defun compare-competence (olanguage nlanguage
                           &key (olabel "(g)old") (nlabel "new")
                                (format :latex)
                                file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (oitems
          (if (stringp olanguage) 
            (analyze-phenomena olanguage :meter ometer)
            olanguage))
         (nitems
          (if (stringp nlanguage) 
            (analyze-phenomena nlanguage :meter nmeter)
            nlanguage)))
    (if (not (and oitems nitems (= (length oitems) (length nitems))))
      1
      (let ((naggregates (length oitems))
            (stream (create-output-stream file append))
            (oaverages (summarize-coverage-parameters oitems))
            (owfaverages 
             (summarize-coverage-parameters 
              oitems :restrictor #'(lambda (foo) 
                                     (not (= (get-field :i-wf foo) 1)))))
            (oifaverages 
             (summarize-coverage-parameters 
              oitems :restrictor #'(lambda (foo) 
                                     (not (= (get-field :i-wf foo) 0)))))
            (naverages (summarize-coverage-parameters nitems))
            (nwfaverages 
             (summarize-coverage-parameters 
              nitems :restrictor #'(lambda (foo) 
                                     (not (= (get-field :i-wf foo) 1)))))
            (nifaverages 
             (summarize-coverage-parameters 
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
            {\\bf Phenomenon} ~
            & {\\bf lexical} & {\\bf parser} ~
            & {\\bf in} & {\\bf out}~%    ~
            & {\\bf lexical} & {\\bf parser} ~
            & {\\bf in} & {\\bf out}\\\\~%  ~
            & $\\propto$ & $\\propto$ & \\% & \\%~%   ~
            & $\\propto$ & $\\propto$ & \\% & \\%\\\\~%  ~
            \\hline~%  ~
            \\hline~%"
           olabel nlabel))
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
           "cell 1 1 -contents \"Phenomenon\" -format title~%~
            region 1 1 2 1 -contents \"Phenomenon\" -format title ~
              -hor_justify center~%~
            region 1 2 1 5 -contents ~s -format title -hor_justify center~%~
            region 1 6 1 9 -contents ~s -format title -hor_justify center~%"
           olabel nlabel)
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
       (do  ((oitems oitems (rest oitems))
             (i 3 (+ i 1)))
           ((null oitems))
         (let* ((phenomenon (first oitems))
                (odata (rest (assoc (first phenomenon) oaverages)))
                (owfdata (rest (assoc (first phenomenon) owfaverages)))
                (oifdata (rest (assoc (first phenomenon) oifaverages)))
                (ndata (rest (assoc (first phenomenon) naverages)))
                (nwfdata (rest (assoc (first phenomenon) nwfaverages)))
                (nifdata (rest (assoc (first phenomenon) nifaverages)))
                (name (if (eq :format :latex)
                        (latexify-string (second phenomenon))
                        (second phenomenon)))
                (owords (get-field :words odata))
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
                (nwords (get-field :words ndata))
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
               name owords oanalyses owfcoverage oifcoverage
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
              (owords (get-field :words odata))
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
              (nwords (get-field :words ndata))
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
              & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%  \\hline~%~
              \\end{tabular}~%"
             name owords oanalyses owfcoverage oifcoverage
             nwords nanalyses nwfcoverage nifcoverage))
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
       0))))

(defun compare-in-detail (olanguage nlanguage
                          &key (show '(:i-input :i-wf))
                               (compare '(:words :readings))
                               condition
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
         (compares (length compare))
         (oitems
          (if (stringp olanguage) 
            (analyze condition olanguage :meter ometer :sort t :message t)
            olanguage))
         (oitems (sort (copy-seq oitems) 
                       #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (nitems
          (if (stringp nlanguage) 
            (analyze condition nlanguage :meter nmeter :sort t :message t)
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
          i i (first compare)))))
    
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
                             compare)))

            (cond 
             ((and (eql oi-id ni-id)
                   (equal oshow nshow))
              ;;
              ;; two items of same identifier have equal values for all .show.
              ;; attributes (as they should |:-)
              ;;
              (unless (equal ocompare ncompare)
                (case format
                  (:tcl
                   (do ((oshow oshow (rest oshow))
                        (j 1 (+ j 1)))
                       ((null oshow))
                     (format
                      stream
                      "cell ~d ~d -contents ~s -format data~%"
                      row j (first oshow)))
                   (do ((acompare (append ocompare ncompare) (rest acompare))
                        (j (+ shows 1) (+ j 1)))
                       ((null acompare))
                     (format
                      stream
                      "cell ~d ~d -contents ~s -format data~%"
                      row j (first acompare)))))
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
                 (do ((oshow oshow (rest oshow))
                      (j 1 (+ j 1)))
                     ((null oshow))
                   (format
                    stream
                    "cell ~d ~d -contents ~s -format data~%"
                    row j (first oshow)))
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
                 (do ((nshow nshow (rest nshow))
                      (j 1 (+ j 1)))
                     ((null nshow))
                   (format
                    stream
                    "cell ~d ~d -contents ~s -format data~%"
                    row j (first nshow)))
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

(defun analyze-competence (&optional (language *tsdb-data*)
                           &key (wf 1) file append (format :latex)
                                restrictor meter)
  (declare (ignore restrictor))

  (let* ((items (if (stringp language) 
                  (analyze-phenomena language :meter meter) 
                  language))
         (stream (create-output-stream file append))
         (averages 
          (summarize-coverage-parameters 
           items :restrictor #'(lambda (foo) 
                                 (not (= (get-field :i-wf foo) wf)))))
         (naggregates (length items)))
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{8}{|c|}~%    {\\bf `{\\bf ~a}' ~a Profile~%     ~
          {\\bf [~a]}}\\\\~%  \\hline\\hline~%  ~
          & {\\bf  total} & {\\bf ~a} & {\\bf word} & {\\bf lexical}~%    ~
            & {\\bf parser} & {\\bf total} & {\\bf overall}\\\\~%  ~
          {\\bf Phenomenon} & {\\bf items} & {\\bf items} & {\\bf string}~%    ~
            & {\\bf items} & {\\bf analyses} & {\\bf results}~%    ~
            & {\\bf coverage}\\\\~%  ~
          & $\\sharp$ & $\\sharp$ & $\\propto$ & $\\propto$ & $\\propto$~%    ~
            & $\\sharp$ & $\\%$\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         (if (stringp language) language "")
         (if (= wf 1) "Coverage" "Overgeneration")
         (current-time :long t)
         (if (= wf 1) "positive" "negative")))
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
        "cell 1 1 -contents \"Phenomenon\" -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"~a\\nitems\\n#\" -format title~%~
         cell 1 4 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 5 -contents \"lexical\\nitems\\n\\330\" -format title~%~
         cell 1 6 -contents \"parser\\nanalyses\\n\\330\" -format title~%~
         cell 1 7 -contents \"total\\nresults\\n#\" -format title~%~
         cell 1 8 -contents \"overall\\ncoverage\\n%\" -format title~%~%"
        (if (= wf 1) "positive" "negative"))))
    (do* ((items items (rest items))
          (i 2 (1+ i))
          (phenomenon (first items) (first items)))
        ((null items))
      (let* ((data (rest (assoc (first phenomenon) averages)))
             (name (if (equal format :latex)
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
            & {\\bf ~,2f} & {\\bf ~d} & {\\bf ~,1f}\\\\~%  \\hline~%~
            \\end{tabular}~%"
           name items restricted length words analyses results coverage))
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

(defun summarize-performance-parameters (items
                                         &key restrictor)

  (let ((itemtotal 0)
        (readingstotal 0)
        (etaskstotal 0)
        (staskstotal 0)
        (ftaskstotal 0)
        (aedgestotal 0)
        (pedgestotal 0)
        (redgestotal 0)
        (firsttotal 0)
        (totaltotal 0)
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
             (readingss
              (length (remove 0 data :key #'(lambda (foo)
                                              (get-field :readings foo)))))
             (etaskss (map 'list #'(lambda (foo)
                                     (get-field :p-etasks foo))
                           data))
             (staskss (map 'list #'(lambda (foo)
                                     (get-field :p-stasks foo))
                           data))
             (ftaskss (map 'list #'(lambda (foo)
                                     (get-field :p-ftasks foo))
                           data))
             (aedgess (map 'list #'(lambda (foo)
                                     (get-field :aedges foo))
                           data))
             (pedgess (map 'list #'(lambda (foo)
                                     (get-field :pedges foo))
                           data))
             (redgess (map 'list #'(lambda (foo)
                                     (get-field :redges foo))
                           data))
             (firsts (map 'list #'(lambda (foo)
                                    (when (> (get-field :readings foo) 0)
                                      (/ (get-field :first foo) 100)))
                          data))
             (firsts (remove nil firsts))
             (totals (map 'list #'(lambda (foo)
                                    (/ (get-field :total foo) 100))
                          data))
             (space (map 'list #'(lambda (foo)
                                   (+ (* 8 (get-field :conses foo))
                                      (* 24 (get-field :symbols foo))
                                      (get-field :others foo)))
                         data)))
        (push (cons (first phenomenon)
                    (pairlis '(:items :readingss
                               :p-etasks :p-stasks
                               :p-ftasks 
                               :aedges :pedges :redges
                               :first :total :space)
                             (list items readingss
                                   (average etaskss) (average staskss)
                                   (average ftaskss) 
                                   (average aedgess) (average pedgess)
                                   (average redgess)
                                   (average firsts) (average totals)
                                   (average space))))
              result)
        (incf itemtotal items)
        (incf readingstotal readingss)
        (incf etaskstotal (sum etaskss))
        (incf staskstotal (sum staskss))
        (incf ftaskstotal (sum ftaskss))
        (incf aedgestotal (sum aedgess))
        (incf pedgestotal (sum pedgess))
        (incf redgestotal (sum redgess))
        (incf firsttotal (sum firsts))
        (incf totaltotal (sum totals))
        (incf spacetotal (sum space))))
    (cons (cons :total
                (pairlis (list :items :readings
                               :p-etasks
                               :p-stasks
                               :p-ftasks
                               :aedges 
                               :pedges 
                               :redges
                               :first
                               :total
                               :space)
                         (list itemtotal readingstotal
                               (divide etaskstotal itemtotal)
                               (divide staskstotal itemtotal)
                               (divide ftaskstotal itemtotal)
                               (divide aedgestotal itemtotal)
                               (divide pedgestotal itemtotal)
                               (divide redgestotal itemtotal)
                               (divide firsttotal readingstotal)
                               (divide totaltotal itemtotal)
                               (divide spacetotal itemtotal))))
          result)))

(defun aggregate (language
                  &key (condition nil)
                       (restrictor nil)
                       (dimension :i-length)
                       (aggregate 2)
                       (lower 0)
                       (upper nil)
                       meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((imeter (madjust * meter 0.9))
         (items (if (stringp language) 
                  (analyze condition language 
                           :meter imeter :message (and meter t)) 
                  language))
         (items (remove-if #'(lambda (foo)
                               (< (get-field dimension foo) lower))
                           items))
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
                            (not (equal (get-field :readings foo) 0))))))
         (items (if (and restrictor (functionp restrictor))
                  (remove-if restrictor items)
                  items))
         (values (map 'list #'(lambda (foo) (get-field dimension foo)) items)))
    (when values
      (let* ((minimum (apply #'min values))
             (aminimum (floor (/ minimum aggregate)))
             (maximum (apply #'max values))
             (amaximum (floor (/ maximum aggregate)))
             (storage (make-array (+ (- amaximum aminimum) 1)
                                  :initial-element nil))
             result)
        (dolist (item items)
          (let* ((value (get-field dimension item))
                 (class (- (floor (/ value aggregate)) aminimum)))
            (push item (aref storage class))))
        (dotimes (i (+ (- amaximum aminimum) 1) result)
          (let* ((class (* (+ i aminimum) aggregate))
                 (name (format 
                        nil 
                        "\\multicolumn{1}{|c|}~
                         {~d $\\leq$ {\\em ~(~a~)\\/} $<$ ~d}"
                        class dimension (+ class aggregate))))
            (push (cons class (cons name (aref storage i))) result)))
        (when meter (meter :value (get-field :end meter)))
        result))))


(defun analyze-performance (&optional (language *tsdb-data*)
                            &key file append (format :latex) 
                                 restrictor meter)

  (let* ((items (if (stringp language) 
                  (analyze-phenomena language :meter meter)
                  language))
         (stream (create-output-stream file append))
         (averages
          (summarize-performance-parameters items :restrictor restrictor))
         (naggregates (length items)))
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{8}{|c|}{~%    {\\bf `~a} Performance Profile~%     ~
          {\\bf [~a]}}\\\\~%  \\hline~%  \\hline~%  ~
          \\raisebox{-1.5ex}[0ex][0ex]{\\bf Phenomenon}~%    & {\\bf items} ~
            & {\\bf etasks} & {\\bf filter} & {\\bf edges}~%    ~
            & {\\bf first}  & {\\bf total} & {\\bf space}\\\\~%  ~
          & $\\sharp$ & $\\propto$ & \\% & $\\propto$~%    ~
            & $\\propto$ (s) & $\\propto$ (s) & $\\propto$ (kb)\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         (if (stringp language) language)
         (current-time :long t)))
      (:tcl
       (when *statistics-tcl-formats* (format stream *statistics-tcl-formats*))
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
        "cell 1 1 -contents \"Phenomenon\" -format title~%~
         cell 1 2 -contents \"items\\n#\" -format title~%~
         cell 1 3 -contents \"etasks\\n\\330\" -format title~%~
         cell 1 4 -contents \"filter\\n%\" -format title~%~
         cell 1 5 -contents \"edges\\n\\330\" -format title~%~
         cell 1 6 -contents \"first\\n\\330 (s)\" -format title~%~
         cell 1 7 -contents \"total\\n\\330 (s)\" -format title~%~
         cell 1 8 -contents \"space\\n\\330 (kb)\" -format title~%~%")))
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
                 (etasks (round (get-field :p-etasks data)))
                 (ftasks (round (get-field :p-ftasks data)))
                 (filter (float (* 100 (divide ftasks (+ etasks ftasks)))))
                 (pedges (round (get-field :pedges data)))
                 (first (float (get-field :first data)))
                 (total (float (get-field :total data)))
                 (space (round (/ (get-field :space data) (expt 2 10)))))
            (case format
              (:latex
               (format
                stream
                "  ~a~%    & ~d & ~d & ~,1f & ~d & ~,1f & ~,1f & ~d\\\\~%"
                name items etasks filter pedges first total space))
              (:tcl
               (format
                stream
                "cell ~d 1 -contents \"~a\" -format aggregate~%~
                 cell ~d 2 -contents ~d -format data~%~
                 cell ~d 3 -contents ~d -format data~%~
                 cell ~d 4 -contents ~,1f -format data~%~
                 cell ~d 5 -contents ~d -format data~%~
                 cell ~d 6 -contents ~,1f -format data~%~
                 cell ~d 7 -contents ~,1f -format data~%~
                 cell ~d 8 -contents ~d -format data~%~%"
                i name
                i items
                i etasks
                i filter
                i pedges
                i first
                i total
                i space)))))))
    (let* ((data (rest (assoc :total averages)))
           (name "Total")
           (items (get-field :items data))
           (etasks (round (get-field :p-etasks data)))
           (ftasks (round (get-field :p-ftasks data)))
           (filter (float (* 100 (divide ftasks (+ etasks ftasks)))))
           (pedges (round (get-field :pedges data)))
           (first (float (get-field :first data)))
           (total (float (get-field :total data)))
           (space (round (/ (get-field :space data) (expt 2 10)))))
      (case format
        (:latex
         (format stream "  \\hline~%  \\hline~%")
         (format
          stream
          "  {\\bf ~a} & {\\bf ~d}~%    ~
           & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d} & {\\bf ~,1f}~%    ~
           & {\\bf ~,1f} & {\\bf ~d}\\\\~%"
          name items etasks filter pedges first total space)
         (format stream "  \\hline~%\\end{tabular}~%"))
        (:tcl
         (format
          stream
          "cell ~d 1 -contents \"~a\" -format total~%~
           cell ~d 2 -contents ~d -format total~%~
           cell ~d 3 -contents ~d -format total~%~
           cell ~d 4 -contents ~,1f -format total~%~
           cell ~d 5 -contents ~d -format total~%~
           cell ~d 6 -contents ~,1f -format total~%~
           cell ~d 7 -contents ~,1f -format total~%~
           cell ~d 8 -contents ~d -format total~%~%"
          (+ naggregates 2) name
          (+ naggregates 2) items
          (+ naggregates 2) etasks
          (+ naggregates 2) filter
          (+ naggregates 2) pedges
          (+ naggregates 2) first
          (+ naggregates 2) total
          (+ naggregates 2) space))))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun compare-performance (olanguage nlanguage 
                            &key (format :latex)
                                 (olabel "(g)old") (nlabel "new")
                                 (clabel "reduction")
                                 orestrictor nrestrictor restrictor
                                 file append meter)

  (let* ((ometer (madjust / meter 2))
         (nmeter (madjust + ometer (mduration ometer)))
         (oitems 
          (if (stringp olanguage) 
            (analyze-phenomena olanguage :meter ometer) 
            olanguage))
         (nitems 
          (if (stringp nlanguage) 
            (analyze-phenomena nlanguage :meter nmeter) 
            nlanguage)))
    (if (not (and oitems nitems (= (length oitems) (length nitems))))
      1
      (let ((stream (create-output-stream file append))
            (oaverages (summarize-performance-parameters 
                        oitems :restrictor (or orestrictor restrictor)))
            (naverages (summarize-performance-parameters 
                        nitems :restrictor (or nrestrictor restrictor)))
            (naggregates (length oitems))
            (*print-circle* nil))

       (case format
         (:latex
          (format
           stream
           "\\begin{tabular}{@{}|l|c|c|c|c|c|c|c|c|c|@{}}~%  ~
            \\hline~%  ~
            & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
            & \\multicolumn{3}{|c|}{\\bf ~a}~%    ~
            & \\multicolumn{3}{|c|}{\\bf ~a}\\\\~%  ~
            {\\bf Phenomenon} ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}~%    ~
              & {\\bf tasks} & {\\bf time} & {\\bf space}\\\\~%  ~
            & $\\propto$ & $\\propto$ (s) & $\\propto$ (kb)~%   ~
            & $\\propto$ & $\\propto$ (s)& $\\propto$ (kb)~%   ~
              & $\\%$ & $\\%$ & $\\%$\\\\~%  ~
            \\hline~%  ~
            \\hline~%"
           olabel nlabel clabel))
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
           "cell 1 1 -contents \"Phenomenon\" -format title~%~
            region 1 1 2 1 -contents \"Phenomenon\" -format title ~
              -hor_justify center~%~
            region 1 2 1 4 -contents ~s -format title -hor_justify center~%~
            region 1 5 1 7 -contents ~s -format title -hor_justify center~%~
            region 1 8 1 10 -contents ~s -format title -hor_justify center~%"
           olabel nlabel clabel)
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

       (do* ((oitems oitems (rest oitems))
             (phenomenon (first oitems) (first oitems))
             (i 3 (+ i 1))
             (position (length oitems) (1- position)))
           ((null oitems))
         (let* ((odata (rest (assoc (first phenomenon) oaverages)))
                (ndata (rest (assoc (first phenomenon) naverages)))
                (name (if (eq format :latex)
                        (latexify-string (second phenomenon))
                        (second phenomenon)))
                (oetasks (round (get-field :p-etasks odata)))
                (otime (float (get-field :total odata)))
                (ospace (round (/ (get-field :space odata) (expt 2 10))))
                (otimepertask (divide otime oetasks))
                (ospacepertask (divide ospace oetasks))
                (netasks (round (get-field :p-etasks ndata)))
                (ntime (float (get-field :total ndata)))
                (nspace (round (/ (get-field :space ndata) (expt 2 10))))
                (ntimepertask (divide ntime netasks))
                (nspacepertask (divide nspace netasks))
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
               "  ~a~%     & ~d & ~,1f & ~d ~
                & ~d & ~,1f & ~d ~
                & ~,1f & ~,1f & ~,1f\\\\~%"
               name oetasks otime ospace netasks ntime nspace
               taskreduction timereduction spacereduction))
             (:tcl
              (format
               stream
               "cell ~d 1 -contents ~s -format aggregate~%~
                cell ~d 2 -contents ~d -format data~%~
                cell ~d 3 -contents ~,1f -format data~%~
                cell ~d 4 -contents ~d -format data~%~
                cell ~d 5 -contents ~d -format data~%~
                cell ~d 6 -contents ~,1f -format data~%~
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
              (otime (float (get-field :total odata)))
              (ospace (round (/ (get-field :space odata) (expt 2 10))))
              (otimepertask (divide otime oetasks))
              (ospacepertask (divide ospace oetasks))
              (netasks (round (get-field :p-etasks ndata)))
              (ntime (float (get-field :total ndata)))
              (nspace (round (/ (get-field :space ndata) (expt 2 10))))
              (ntimepertask (divide ntime netasks))
              (nspacepertask (divide nspace netasks))
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
              {\\bf ~a} & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d}~%    ~
              & {\\bf ~d} & {\\bf ~,1f} & {\\bf ~d}~%    ~
              & {\\bf ~,1f} & {\\bf ~,1f} & {\\bf ~,1f}\\\\~%"
             name oetasks otime ospace netasks ntime nspace
             taskreduction timereduction spacereduction)
            (format stream "  \\hline~%\\end{tabular}"))
           (:tcl
            (format
             stream
             "cell ~d 1 -contents ~s -format total~%~
              cell ~d 2 -contents ~d -format total~%~
              cell ~d 3 -contents ~,1f -format total~%~
              cell ~d 4 -contents ~d -format total~%~
              cell ~d 5 -contents ~d -format total~%~
              cell ~d 6 -contents ~,1f -format total~%~
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
       0))))

(defun graph-words (data &key file append)
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (all (make-array (+ mwords 1) :initial-element 0))
         (parses (make-array (+ mwords 1) :initial-element 0)))
    (multiple-value-bind (mall mparses)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (mall 0 mall)
              (mparses 0 mparses))
            ((null data) (values mall mparses))
          (setf mall (max mall (incf (aref all words))))
          (when (> (get-field :readings current) 0)
            (setf mparses (max mparses (incf (aref parses words))))))
      ;;;
      ;;; plot words histogram
      ;;;
      (let ((breadth (* 0.7 (/ *statistics-plot-width* mwords))))
        (format stream "\\beginpicture~%")
        (format stream 
                "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
                (/ *statistics-plot-width* mwords)
                (/ *statistics-plot-height* mall))
        (format stream 
                "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
                mwords mall)
        (format stream 
                "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
                mwords)
        (format stream 
                "  \\axis left ticks numbered from 0 to ~d by 50 /~%" 
                mall)
        (format stream 
                "  \\setbars breadth <~,2fmm> baseline at y = 0~%"
                breadth)
        (format stream "  \\plot~%")
        (do ((i 1 (+ i 1)))
            ((> i mwords))
          (when (> (aref all i) 0)
            (format stream "    ~3d ~4d~%" i (aref all i))))
        (format stream "  /~%")
        (format stream "  \\linethickness=~,2fmm~%" breadth)
        (format stream "  \\setbars breadth <0mm> baseline at y = 0~%")
        (format stream "  \\plot~%")
        (do ((i 1 (+ i 1)))
            ((> i mwords))
          (when (> (aref parses i) 0)
            (format stream "    ~3d ~4d~%" i (aref parses i))))
        (format stream "  /~%")
        (format stream "\\endpicture~%")))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-first (data &key file append)
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (firsts (make-array (+ mwords 1) :initial-element nil))
         (totals (make-array (+ mwords 1) :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts words))
          (push (/ total 10) (aref totals words)))
      ;;;
      ;;; plot words # first graph
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling mfirst)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling mfirst))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 50 /~%" 
              (ceiling mfirst))
      (format stream "  \\multiput{\\tiny$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (do ((times (aref firsts i) (rest times)))
          ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-total (data &key file append
                                   (threshold 1))
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (firsts (make-array (+ mwords 1) :initial-element nil))
         (totals (make-array (+ mwords 1) :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts words))
          (push (/ total 10) (aref totals words)))
      ;;;
      ;;; plot words # total graph (with words # first overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling mtotal)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling mtotal))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 10 /~%" 
              (ceiling mtotal))
      (format stream "  \\multiput{\\tiny$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (do ((times (aref totals i) (rest times)))
          ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-tasks-total (data &key file append)
  (let* ((stream (create-output-stream file append))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :p-etasks foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :first foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :total foo))))
         (data 
          (remove -1 data :key #'(lambda (foo) (get-field :readings foo))))
         (mtasks
          (apply #'max (map 'list #'(lambda (foo) 
                                      (get-field :p-etasks foo))
                            data)))
         (firsts (make-array (+ (ceiling (/ mtasks 50)) 1)
                             :initial-element nil))
         (totals (make-array (+ (ceiling (/ mtasks 50)) 1)
                             :initial-element nil)))
    (multiple-value-bind (mfirst mtotal)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (tasks (get-field :p-etasks current)
                     (get-field :p-etasks current))
              (first (get-field :first current) (get-field :first current))
              (mfirst 0 (if first (max mfirst first) mfirst))
              (total (get-field :total current) (get-field :total current))
              (mtotal 0 (if total (max mtotal total) mtotal)))
            ((null data) (values (/ mfirst 10) (/ mtotal 10)))
          (push (/ first 10) (aref firsts (floor (/ tasks 50))))
          (push (/ total 10) (aref totals (floor (/ tasks 50)))))
      ;;;
      ;;; plot tasks # total graph (with tasks # first overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,2fmm,~,2fmm>~%"
              (/ *statistics-plot-width* (ceiling (/ mtasks 50)))
              (/ *statistics-plot-height* (ceiling mtotal)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              (ceiling (/ mtasks 50)) (ceiling mtotal))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 50 /~%"
              (ceiling (/ mtasks 50)))
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 10 /~%" 
              (ceiling mtotal))
      (format stream "  \\multiput{\\tiny$\\bullet$} at~%")
      (do ((i 0 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (do ((times (aref totals i) (rest times)))
            ((null times))
          (format stream "    ~4d ~8,2f~%" i (first times))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref firsts i)
          (format stream "    ~4d ~8,2f~%" i (average (aref firsts i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i (floor (/ mtasks 50))))
        (when (aref totals i)
          (format stream "    ~4d ~8,2f~%" i (average (aref totals i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun plot-words-etasks-stasks-ftasks (data &key file append
                                                  (threshold 1))
  (let* ((stream (create-output-stream file append))
         (mwords 
          (do ((data data (rest data))
               (wordss (list (get-field :words (first data)))
                       (cons (get-field :words (first data)) wordss)))
              ((null data) (apply #'max wordss))))
         (etaskss (make-array (+ mwords 1) :initial-element nil))
         (staskss (make-array (+ mwords 1) :initial-element nil))
         (ftaskss (make-array (+ mwords 1) :initial-element nil))
         (aetasks 0)
         (astasks 0)
         (aftasks 0))
    (multiple-value-bind (metasks mstasks mftasks)
        (do* ((data data (rest data))
              (current (first data) (first data))
              (words (get-field :words current) (get-field :words current))
              (etasks (get-field :p-etasks current)
                      (get-field :p-etasks current))
              (stasks (get-field :p-stasks current)
                      (get-field :p-stasks current))
              (ftasks (get-field :p-ftasks current)
                      (get-field :p-ftasks current))
              (metasks 0 (if etasks (max metasks etasks) metasks))
              (mstasks 0 (if stasks (max mstasks stasks) mstasks))
              (mftasks 0 (if ftasks (max mftasks ftasks) mftasks)))
            ((null data) (values metasks mstasks mftasks))
          (when etasks
            (push etasks (aref etaskss words)))
          (when stasks
            (push stasks (aref staskss words)))
          (when ftasks
            (push ftasks (aref ftaskss words))))
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (let ((etasks (aref etaskss i))
              (stasks (aref staskss i))
              (ftasks (aref ftaskss i)))
          (when etasks
            (setf aetasks (max aetasks (average etasks))))
          (when stasks
            (setf astasks (max astasks (average stasks))))
          (when ftasks
            (setf aftasks (max aftasks (average ftasks))))))
      ;;;
      ;;; plot words # etasks graph (with words # stasks overlay)
      ;;;
      (format stream "\\beginpicture~%")
      (format stream 
              "  \\setcoordinatesystem units <~,4fmm,~,4fmm>~%"
              (/ *statistics-plot-width* mwords)
              (/ *statistics-plot-height* (ceiling aetasks)))
      (format stream 
              "  \\setplotarea x from 0 to ~d, y from 0 to ~d~%"
              mwords (ceiling aetasks))
      (format stream "  \\setplotsymbol({\\rule{.4pt}{.4pt}})~%")
      (format stream "  \\setlinear~%")
      (format stream 
              "  \\axis bottom ticks numbered from 0 to ~d by 5 /~%"
              mwords)
      (format stream 
              "  \\axis left ticks numbered from 0 to ~d by 100 /~%" 
              (ceiling aetasks))
      (format stream "  \\multiput{\\large$\\bullet$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref etaskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref etaskss i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref etaskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref etaskss i)))))
      (format stream "  /~%")
      (format stream "  \\multiput{\\large$\\circ$} at~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref staskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref staskss i)))))
      (format stream "  /~%")
      (format stream "  \\plot~%")
      (do ((i 1 (+ i 1)))
          ((> i mwords))
        (when (aref staskss i)
          (format stream "    ~4d ~8,2f~%" i (average (aref staskss i)))))
      (format stream "  /~%")
      (format stream "\\endpicture~%"))
    (when (or (stringp file) (stringp append)) (close stream))))

(defun graph-words-etasks-stasks-ftasks (data 
                                         &key file (format :tcl)
                                              logscale meter
                                              (threshold 5))

  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (aggregates 
          (aggregate data :dimension :words :aggregate 1 :meter meter))
         (aggregates (if threshold
                       (remove-if #'(lambda (aggregate)
                                      (< (length (rest (rest aggregate)))
                                         threshold))
                                  aggregates)
                       aggregates))
         (saggregates (summarize-performance-parameters aggregates))
         wordss etaskss staskss ftaskss) 
    (dolist (aggregate (rest saggregates))
      (let* ((words (first aggregate))
             (data (rest (rest aggregate)))
             (etasks (get-field :p-etasks data))
             (stasks (get-field :p-stasks data))
             (ftasks (get-field :p-ftasks data)))
        (push words wordss)
        (push etasks etaskss)
        (push stasks staskss)
        (push ftasks ftaskss)))
    (format 
     stream
     "graph -font {Helvetica 10 bold} -plotbackground white \\~%  ~
      -width 15c -height 10c -rightmargin 10  \\~%  ~
      -title \"Parser Tasks Distribution\"~%")
    (format stream "data x1 ~a~%" (list2tcl (nreverse wordss)))
    (format stream "data y1 ~a~%" (list2tcl (nreverse staskss)))
    (format stream "data y2 ~a~%" (list2tcl (nreverse etaskss)))
    (format stream "data y3 ~a~%" (list2tcl (nreverse ftaskss)))
    (format
     stream
     "axis x -title \"input length (lexical items)\" -stepsize 5 \\~%  ~
      -tickfont {Helvetica 9} -subdivisions 1~%")
    (format
     stream
     "axis y -tickfont {Helvetica 9} -stepsize 5 -logscale ~:[no~;yes~]~%"
     logscale)
    (format
     stream
     "element e1 -xdata x1 -ydata y1 -label \"successful\" \\~%  ~
      -symbol circle -pixel 3\\~%  ~
      -color yellow -fill defcolor -outline defcolor~%")
    (format
     stream
     "element e2 -xdata x1 -ydata y2 -label \"executed\" \\~%  ~
      -symbol diamond -pixel 3\\~%  ~
      -color orange -fill defcolor -outline defcolor~%")
    (format
     stream
     "element e3 -xdata x1 -ydata y3 -label \"filtered\" \\~%  ~
      -symbol square -pixel 3\\~%  ~
      -color red -fill defcolor -outline defcolor ~%")
    (format 
     stream 
     "legend -font {Helvetica 9} -position plotarea -anchor nw \\~%  ~
      -relief ridge~%")
    (force-output stream)
    (when file (close stream))))

(defun graph-words-first-total (data 
                                &key file (format :tcl)
                                     logscale meter
                                     (aggregate 1)
                                     (threshold 5))

  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (aggregates 
          (aggregate data :dimension :words
                     :aggregate aggregate :meter meter))
         (aggregates (if threshold
                       (remove-if #'(lambda (aggregate)
                                      (< (length (rest (rest aggregate)))
                                         threshold))
                                  aggregates)
                       aggregates))
         (saggregates (summarize-performance-parameters aggregates))
         firsts totals wordss etaskss staskss ftaskss) 
    (dolist (aggregate (rest saggregates))
      (let* ((words (first aggregate))
             (data (rest (rest aggregate)))
             (first (* 100 (get-field :first data)))
             (total (* 100 (get-field :total data)))
             (etasks (get-field :p-etasks data))
             (stasks (get-field :p-stasks data))
             (ftasks (get-field :p-ftasks data)))
        (push first firsts)
        (push total totals)
        (push words wordss)
        (push etasks etaskss)
        (push stasks staskss)
        (push ftasks ftaskss)))
    (format 
     stream
     "graph -font {Helvetica 10 bold} -plotbackground white \\~%  ~
      -width 15c -height 10c -rightmargin 10  \\~%  ~
      -title \"Parsing Time Distribution\"~%")
    (format stream "data x1 ~a~%" (list2tcl (nreverse wordss)))
    (format stream "data y1 ~a~%" (list2tcl (nreverse firsts)))
    (format stream "data y2 ~a~%" (list2tcl (nreverse totals)))
    (format
     stream
     "axis x -title \"input length (lexical items)\" -stepsize 5 \\~%  ~
      -tickfont {Helvetica 9} -subdivisions 1~%")
    (format
     stream
     "axis y -tickfont {Helvetica 9} -logscale ~:[no~;yes~]~%"
     logscale)
    (format
     stream
     "element e1 -xdata x1 -ydata y1 -label \"first reading\" \\~%  ~
      -symbol circle -pixel 3~%")
    (format
     stream
     "element e2 -xdata x1 -ydata y2 -label \"exhaustive search\" \\~%  ~
      -symbol diamond -pixel 3~%")
    (format 
     stream 
     "legend -font {Helvetica 9} -position plotarea -anchor nw \\~%  ~
      -relief ridge~%")
    (force-output stream)
    (when file (close stream))))
