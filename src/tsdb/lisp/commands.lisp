;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "TSDB")

(defun retrieve (&optional condition (data *tsdb-data*)
                 &key (stream *tsdb-io*) (verbose t) meter)

  (initialize-tsdb)
  (when meter
    (status :text (format nil "retrieving `~a' data ..." data)))
  (let* ((imeter (if *tsdb-ignore-output-p*
                   meter
                   (madjust * meter 0.5)))
         (ometer (unless *tsdb-ignore-output-p*
                   (madjust + (madjust * meter 0.5) (mduration imeter))))
         (items (select '("i-id" "i-wf" "i-input")
                        '(:integer :integer :string)
                        "item"
                        condition
                        data
                        :unique nil :sort :i-id
                        :meter imeter))
         (outputs (unless *tsdb-ignore-output-p*
                    (select '("i-id" "o-ignore" "o-wf" "o-gc" "o-edges")
                        '(:integer :string :integer :integer :integer)
                        "output"
                        condition
                        data
                        :unique nil :sort :i-id
                        :meter ometer)))
         (all (if *tsdb-ignore-output-p*
                (map 'list
                  #'(lambda (foo)
                      (append foo (pairlis '(:o-ignore :o-wf :o-gc :o-edges)
                                           '("" -1 -1 -1))))
                  items)
                (loop
                    for item in items
                    for output = (or (find (get-field :i-id item)
                                           outputs
                                           :key #'(lambda (foo)
                                                    (get-field :i-id foo)))
                                     (pairlis '(:o-ignore :o-wf :o-gc :o-edges)
                                              '("" -1 -1 -1)))
                    do 
                      (setf (rest (last item)) output)
                    collect item))))
    (when verbose
      (format
       stream 
       "~&retrieve(): found ~a item~:p (~a output specification~:p).~%" 
       (length items) (length outputs)))
    (when meter (meter :value (get-field :end meter)))
    (when meter
      (status :text (format nil "retrieving `~a' data ... done" data)))
    all))

(defun tsdb-do-vocabulary (language &key condition (load :warn) 
                                         (stream *tsdb-io*)
                                         meter interrupt)
  
  (initialize-tsdb)
  (let* ((*tsdb-gc-message-p* nil)
         (condition (if (equal condition "") nil condition))
         (loadp (not (member load (list nil :off :no :none :fuck))))
         (whitespace '(#\Space #\Newline #\Tab))
         (garbage (append whitespace 
                          (loop
                              for string in *tsdb-tokens-to-ignore* 
                              when (and (stringp string) (= (length string) 1))
                              collect (schar string 0)
                              else when (characterp string)
                              collect string)))
         (imeter (madjust * meter 0.1))
         (wmeter (madjust + (madjust * meter 0.9) (mduration imeter)))
         (items (retrieve condition language))
         (strings (map 'list #'(lambda (foo) (get-field :i-input foo)) items))
         (frequencies (make-hash-table :test #'equal))
         (lexicon (make-hash-table :test #'equal ))
         (lstasks (make-hash-table :test #'equal ))
         (maximal-frequency 0)
         (message (format nil "retrieving `~a' vocabulary ..." language))
         words unknown-words)
    (when meter
      (status :text message)
      (meter :value (get-field :start meter)))
    (when (eq load :collect) (setf %tsdb-lexical-preterminals% nil))
    (when strings
      (format stream "~%")
      #+:lkb (uncache-lexicon)
      (dolist (string strings words)
        (do* ((i (position-if #'(lambda (c) (member c whitespace)) string)
                 (position-if #'(lambda (c) (member c whitespace)) string))
              (word (when i (subseq string 0 i))
                    (when i (subseq string 0 i)))
              (word (string-downcase (string-trim garbage word))
                    (string-downcase (string-trim garbage word)))
              (n (gethash word frequencies)
                 (gethash word frequencies))
              (string (if i (subseq string i) string)
                      (if i (subseq string i) string))
              (string (string-left-trim whitespace string)
                      (string-left-trim whitespace string)))
            ((not i)
             (let* ((word (string-downcase (string-trim garbage string)))
                    (n (gethash word frequencies)))
               (when (and word (> (length word) 0))
                 (setf (gethash word frequencies) (+ (or n 0) 1))
                 (setf (gethash word lexicon) 0)
                 (setf (gethash word lstasks) 0)
                 (setf maximal-frequency 
                   (max maximal-frequency (+ (or n 0) 1)))
                (pushnew word words :test #'equal))))
          (when (and word (> (length word) 0))
            (setf (gethash word frequencies) (+ (or n 0) 1))
            (setf (gethash word lexicon) 0)
            (setf (gethash word lstasks) 0)
            (setf maximal-frequency (max maximal-frequency (+ (or n 0) 1)))
            (pushnew word words :test #'equal))))
      (let* ((width (apply #'max (map 'list #'length words)))
             (tabulation (format 
                          nil
                          "~~~d,0t| ~~~dd"
                          (+ width 2 1) 
                          (length (format nil "~d" maximal-frequency))))
             (increment (when wmeter
                          (/ (mduration wmeter) (length words)))))
        (do* ((words (sort (copy-seq words) #'string-lessp) (rest words))
              (word (first words) (first words)))
            ((null words) unknown-words)
          (when (and interrupt (probe-file interrupt))
            (delete-file interrupt)
            (format
             stream
             "do-vocabulary(): received external interrupt signal.~%")
            (when meter 
              (status :text (format nil "~a interrupt" message) :duration 5))
            (return-from tsdb-do-vocabulary))
          (when (and loadp 
                     (not (member word *tsdb-tokens-to-ignore* 
                                  :test #'string-equal)))
            (let ((entries (parse-word word :load load)))
              (setf (gethash word lexicon) (get-field :words entries))
              (setf (gethash word lstasks) (get-field :l-stasks entries))
              (unless entries (push word unknown-words)))
            (format 
             stream 
             "~&  ~a ~@? reference(s)~:[~; | [~d + ~d] lexical entrie(s)~];~%" 
             word tabulation (gethash word frequencies)
             loadp (or (gethash word lexicon) -1) 
             (or (gethash word lstasks) -1)))
          (when increment (meter-advance increment)))))
    (format stream "~&~%")
    (when meter 
      (meter :value (get-field :end meter))
      (status :text (format nil "~a done" message) :duration 5))
    (length items)))

(defun tsdb (&optional action argument 
             &key condition run skeleton load 
                  (file nil filep) (reset nil resetp))
  
  (initialize-tsdb)
  (if (stringp action)
    (let* ((result (call-tsdb action (or argument *tsdb-data*))))
      (when (and result (not (zerop (length result))))
        (format *tsdb-io* "~&~%~a~%" result)))
    (let ((action (string action)))
      (case (intern (subseq action 0 (min (length action) 3)) :keyword)

        ((:initialize :ini)
         (setf *tsdb-initialized-p* nil)
         (initialize-tsdb argument :pattern load :background run)
         (tsdb :info)
         (format *tsdb-io* "~&~%"))

        ((:podium :pod :po)
         (init-podium)
         (when argument
           (load-cache :pattern load :background t)))
        
        ((:cpus :cpu :cpu :cp)
         (format *tsdb-io* "~&~%")
         (cond
          ((null argument)
           (tsdb-do-cpus :stream *tsdb-io*))
          ((eq argument :active)
           (tsdb-do-cpus :action :active :stream *tsdb-io*))
         (t
           (cond
            ((and filep resetp)
             (initialize-cpus :classes argument 
                              :file file :reset reset 
                              :stream *tsdb-io* :prefix "  "))
            (filep 
             (initialize-cpus :classes argument 
                              :file file :stream *tsdb-io* :prefix "  "))
            (resetp
             (initialize-cpus :classes argument 
                              :reset reset :stream *tsdb-io* :prefix "  "))
            (t
             (initialize-cpus :classes argument 
                              :stream *tsdb-io* :prefix "  ")))))
         (format *tsdb-io* "~&~%"))
         
        ((:info :inf)
         (tsdb-do-status :all :stream *tsdb-io*))
        
        ((:home :hom :ho)
         (if (stringp argument)
           (tsdb-do-set (quote *tsdb-home*) argument)
           (tsdb-do-status :home :stream *tsdb-io*)))
        
        ((:default :def :de :d)
         (if (stringp argument)
           (tsdb-do-set (quote *tsdb-data*) argument)
           (tsdb-do-status :default :stream *tsdb-io*)))
        
        ((:list :lis :li :l)
         (tsdb-do-list (or argument *tsdb-home*)))
        
        ((:skeletons :ske :sk :s)
         (format *tsdb-io* "~&~%")
         (tsdb-do-skeletons argument))
        
        ((:create :cre :cr :c)
         (format *tsdb-io* "~&~%")
         (tsdb-do-create argument (or skeleton *tsdb-default-skeleton*))
         (format *tsdb-io* "~&~%"))
        
        ((:process :pro :pr)
         (format *tsdb-io* "~&~%")
         (tsdb-do-process (if (or (null argument)
                                  (member argument (list nil t "")))
                            *tsdb-data*
                            argument)
                          :condition condition :run-id run))
        
        ((:vocabulary :voc :vo :v)
         (format *tsdb-io* "~&~%")
         (tsdb-do-vocabulary (if (or (null argument)
                                     (member argument (list nil t "")))
                               *tsdb-data*
                               argument) 
                             :condition condition :load (or load :quiet)))
        
        ((:help :hel :he)
         (format *tsdb-io* "~&~%")
         (tsdb-do-help (if argument
                         (intern (subseq (string argument) 0 3) :keyword)
                         :all)))
        
        (t 
         (format 
          *tsdb-io* 
          "~&~%  tsdb(): invalid or ambiguous command `~(~:a~)'; ~
           try `tsdb :help'.~%~%"
          action))))))

(defun tsdb-do-set (variable value)
  (format *tsdb-io* "~%")
  (let ((value (case variable
                 ((*tsdb-home*) 
                  (namestring (make-pathname :directory value)))
                 (t value))))
    (set variable value)))

(defun tsdb-do-status (name &key (stream *tsdb-io*) (prefix "  "))

  (format stream "~&~%")
  (when (member name (list :all))
    (format 
     stream 
     "~atsdb(1) application: `~a';~%" 
     prefix 
     *tsdb-application*))
  (when (member name (list :all :home))
    (format 
     stream 
     "~atsdb(1) database root: `~a';~%" 
     prefix 
     *tsdb-home*))
  (when (member name (list :all :default))
    (format 
     stream 
     "~adefault test suite database `~a'~%" 
     prefix 
     *tsdb-data*))
  (when (member name (list :all))
    (format 
     stream
     "~askeletons directory: `~a';~%"
     prefix *tsdb-skeleton-directory*)
    (format
     stream
     "~adefault test suite skeleton `~a';~%"
     prefix *tsdb-default-skeleton*))
  (when (member name (list :all))
    (format 
     stream 
     "~awrite run: ~:[no~;yes~]; write parse: ~:[no~;yes~]; ~
      write result: ~:[no~;yes~];~%"
     prefix
     *tsdb-write-run-p* *tsdb-write-parse-p* *tsdb-write-result-p*))
  (when (member name (list :all))
    (format 
     stream 
     "~awrite output: ~:[no~;yes~]; ~
      write lexicon chart: ~:[no~;yes~]; write syntax chart: ~:[no~;yes~];~%"
     prefix
     *tsdb-write-output-p* 
     *tsdb-write-lexicon-chart-p* *tsdb-write-syntax-chart-p*))
  (when (member name (list :all))
    (format 
     stream 
     "~acache database writes ~:[no~;yes~]; flush cache threshold ~a~%"
     prefix
     *tsdb-cache-database-writes-p* *tsdb-flush-cache-threshold*))
  (when (member name (list :all))
    (format 
     stream 
     "~atrees hook: ~:[none~;`~(~a~)()'~]; ~
      semantix hook: ~:[none~;`~(~a~)()'~];~%"
     prefix
     *tsdb-trees-hook* *tsdb-trees-hook* 
     *tsdb-semantix-hook* *tsdb-semantix-hook*))
  (when (member name (list :all))
    (format 
     stream 
     "~aexhaustive search: ~:[no~;yes~]; ~
     maximal number of edges ~a; item factor: ~a.~%"
     prefix
     *tsdb-exhaustive-p* *tsdb-maximal-number-of-edges* *tsdb-edge-factor*))
  (format stream "~%"))

(defun find-tsdb-directories (&optional (home *tsdb-home*) 
                              &key name pattern trace meter)
  (declare (ignore trace))
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((prefix (length *tsdb-home*))
         (absolute (not (equal home *tsdb-home*)))
         (directories (subdirectories home))
         (directories (map 'list
                        #'(lambda (directory)
                            (if (search *tsdb-home* directory)
                                (subseq directory prefix)))
                        directories))
         (directories (if name
                        (remove name directories :test-not #'equal)
                        directories))
         (directories (if pattern
                        (remove pattern directories :test-not #'search)
                        directories))
         (dmeter (madjust * meter 0.2))
         (dbmeter (madjust + (madjust * meter 0.8) (mduration dmeter)))
         (increment (when (and directories dbmeter)
                      (/ (mduration dbmeter) (length directories))))
         dbs)
                    
    (do* ((directories directories (rest directories))
          (directory (first directories) (first directories)))
        ((null directories))
      (push (verify-tsdb-directory directory :absolute absolute) dbs)
      (when increment (meter-advance increment)))
    (when dbmeter (meter :value (get-field :end dbmeter)))
    (remove nil dbs)))

(defun tsdb-do-list (home &key (stream *tsdb-io*) 
                               (prefix "  ")
                               (format :ascii)
                               name meter index)
  
  (when stream (format stream "~%"))
  (let ((dbs (sort (find-tsdb-directories home :name name 
                                          :meter (madjust * meter 0.95))
                   #'string< :key #'(lambda (foo) (get-field :database foo))))
        result)
    (do* ((dbs dbs (rest dbs))
          (db (first dbs) (first dbs))
          (i 0 (+ i 1)))
        ((null dbs))
      (case format
        (:ascii
         (format 
          stream 
          "~a`~a'~@[ (~(~a~))~]: ~a items; ~a parses;~%"
          prefix
          (get-field :database db) (get-field :status db) 
          (get-field :items db) (get-field :parses db)))
        (:tcl
         (format 
          stream 
          "set test_suites(~d) {~s \"~(~a~)\" ~d ~d ~:[0~;1~]};~%"
          (if index (+ index i) i)
          (get-field :database db) (get-field :status db) 
          (get-field :items db) (get-field :parses db)
          (get-field :chart db)))
        (:list
         (push (format 
                nil 
                "{~s \"~(~a~)\" ~d ~d ~:[0~;1~]}"
                (get-field :database db) (get-field :status db) 
                (get-field :items db) (get-field :parses db)
                (get-field :chart db))
               result))))
    (when (and stream dbs) (format stream "~%"))
    (when meter (meter :value (get-field :end meter)))
    result))

(defun tsdb-do-skeletons (source &key (stream *tsdb-io*) 
                                      (prefix "  ")
                                      (format :ascii)
                                      index meter)
  
  (when meter (meter :value (get-field :start meter)))
  (setf *tsdb-skeletons* nil)
  (initialize-tsdb nil :action :skeletons)
  (let ((directory *tsdb-skeleton-directory*)
        (skeletons 
         (sort (copy-seq *tsdb-skeletons*) #'string<
               :key #'(lambda (foo) (get-field :content foo)))))
    (if source
      (when (member (string source) skeletons 
                    :key #'(lambda (foo) (get-field :path foo))
                    :test #'equal)
        (setf *tsdb-default-skeleton* (string source)))
      (let ((n (+ (length skeletons) 1)))
        (do* ((skeletons skeletons (rest skeletons))
              (skeleton (first skeletons) (first skeletons))
              (i 0 (+ i 1))
              (imeter (madjust * meter (/ i n)) (madjust * meter (/ i n))))
            ((null skeletons))
          (when imeter (meter :value (get-field :end imeter)))
          (let* ((path
                  (namestring (dir-append 
                               (namestring directory)
                               (list :relative (get-field :path skeleton)))))
                 (content (get-field :content skeleton))
                 (items (length (select "i-id" :integer "item" nil path 
                                        :absolute t :unique nil
                                        :quiet t :ro t))))
            (case format
              (:ascii
               (format  
                stream 
                "~a~a (`~a'): ~a items;~%"
                prefix
                content (get-field :path skeleton) items))
              (:tcl
               (format 
                stream 
                "set skeletons(~d) {~s ~s ~d};~%"
                (if index (+ index i) i)
                (get-field :path skeleton) content items)))))))
    (when meter (meter :value (get-field :end meter)))
    (when skeletons (format stream "~%"))))

(defun tsdb-do-phenomena (&key (stream *tsdb-io*) 
                               (prefix "  ")
                               (format :tcl))
  (declare (ignore prefix)
           (special *phenomena*))
  
  (do ((phenomena *phenomena* (rest phenomena))
       (i 0 (+ i 1)))
      ((null phenomena))
    (case format
      (:tcl
       (format 
        stream 
        "set phenomena(~d) ~s;~%"
        i (first phenomena))))))

(defun tsdb-do-cpus (&key (action :list) (format :ascii)
                         (stream *tsdb-io*) (prefix "  "))
  
  (case action
    (:list
     (loop 
         for cpu in (sort 
                     (copy-list *pvm-cpus*) #'string< 
                     :key #'(lambda (cpu) 
                              (let ((class (cpu-class cpu)))
                                (symbol-name
                                 (typecase class
                                   (cons (first class))
                                   (t class))))))
                                         
         for host = (cpu-host cpu)
         for spawn = (cpu-spawn cpu)
         for options = (cpu-options cpu)
         for class = (cpu-class cpu)
         do
           (case format
             (:ascii
              (format
               stream
               "~&~%~a- `~(~:[~a~;~{~a~^ | ~}~]~)' (`~a');~%~
                ~a  command: `~a';~%~
                ~a  options: `~{~a~^ ~}';~%~%"
               prefix (consp class) class host
               prefix spawn
               prefix options)))))
    (:active
     (loop 
         for client in (sort 
                        (copy-list *pvm-clients*) #'string<
                        :key #'(lambda (client) 
                                 (cpu-host (client-cpu client))))
         for cpu = (client-cpu client)
         for host = (cpu-host cpu)
         for spawn = (cpu-spawn cpu)
         for task = (client-task client)
         for tid = (get-field :tid task)
         for pid = (get-field :pid task)
         for status = (client-status client)
         for protocol = (client-protocol client)
         do
           (format
               stream
               "~&~%~a- `~(~a~)' (pid: ~a --- tid: <~a>);~%~
                ~a  command: `~a';~%~
                ~a  status: ~(~a~) --- protocol: ~(~a~);~%~%"
               prefix host pid tid
               prefix spawn
               prefix status protocol)))))

(defun tsdb-do-schema (data &key (stream *tsdb-io*) (format :tcl))
  
  (let* ((schema (read-database-schema data))
         (relations (map 'list #'first schema))
         (fields (map 'list #'rest schema))
         (fields  (reduce #'(lambda (foo bar)
                              (union foo bar 
                                     :key #'first 
                                     :test #'string=))
                          fields))
         (attributes (map 'list #'first fields)))
    (case format
      (:tcl
       (format
        stream
        "set globals(relations) ~a;~%set globals(attributes) ~a;~%"
        (list2tcl relations) (list2tcl attributes))))))

(defun tsdb-do-create (name skeleton-name 
                       &key (stream *tsdb-io*) 
                            create meter)
  (declare (ignore create))
  
  (when meter (meter :value (get-field :start meter)))
  (if (null name)
    (if stream
      (format 
       stream
       "tsdb(): new database name required as argument for `:create'.~%")
      1)
      (let* ((name (if (equal name t) "" name))
             (name (string name))
             (new (if (or (equal name "")
                          (equal (elt name (- (length name) 1)) #\/))
                    (let ((parent (find-tsdb-directory name)))
                      (when (probe-file parent)
                        (let* ((date (current-time :long :usa))
                               (name (concatenate 'string name date)))
                          (find-tsdb-directory name))))
                    (find-tsdb-directory name)))
             (skeleton (find-skeleton skeleton-name))
             (old (when skeleton (find-skeleton-directory skeleton))))
        (cond
         ((null skeleton)
          (if stream
            (format 
             stream
             "tsdb(): unknown test suite skeleton `~a'.~%"
             skeleton-name)
            2))
         ((null new)
          (if stream
            (format 
             stream 
             "tsdb(): no parent directory `~a' for new database `~a'.~%" 
             name (current-time :long :usa))
            3))
         ((not (probe-file old))
          (if stream
            (format 
             stream
             "tsdb(): no skeleton directory `~a'.~%"
             old)
            4))
         ((probe-file new)
          (if stream
            (format 
             stream
             "tsdb(): database `~a' already exists.~%"
             (string-trim '(#\/) (string-strip *tsdb-home* new)))
            5))
         (t
          (when meter (meter :value (get-field :start meter)))
          (let ((status 
                 (run-process 
                  (format nil "cp -pr '~a' '~a'" old new) :wait t)))
            (if (zerop status)
              (let ((imeter (madjust / meter 2)))
                (when imeter (meter :value (get-field :end imeter)))
                (when (select "i-id" :integer "item" nil new :absolute t)
                  (setf *tsdb-data* 
                    (string-trim '(#\/) (string-strip *tsdb-home* new)))
                  (when meter (meter :value (get-field :end meter)))
                  (if stream
                    (format 
                     stream
                     "tsdb(): `~a' created as new default test suite.~%"
                     *tsdb-data*)
                    0)))
              (if stream
                (format 
                 stream
                 "tsdb(): mysterious problems creating database `~a'.~%"
                 (string-trim '(#\/) (string-strip *tsdb-home* new)))
                42))))))))

(defun tsdb-do-help (command)
  
  (when (equal command :all)
    (format
     *tsdb-io*
     "  The `tsdb' command encapsulates all interaction with the test suite
  database tsdb(1) and profiling machinery; it has the general synopsis:

    tsdb _action_ [ _argument_ ] [ :key _option_ ]+

  where _action_ can be one of the following:~%~%"))
  
  (when (member command (list :all :info :inf))
    (format
     *tsdb-io*
     "    - :info

        display summary of tsdb(1) global parameters and options; see below
        for commands that query and set individual parameters;~%~%"))
  
  (when (member command (list :all :home :hom :ho))
    (format
     *tsdb-io*
     "    - :home [ _string_ ]

        query or set tsdb(1) databases root directory;~%~%"))
  
  (when (member command (list :all :default :def :de :d))
    (format
     *tsdb-io*
     "    - :default [ _string_ ]

        query or set tsdb(1) default test suite database;~%~%"))
  
  (when (member command (list :all :initialize :ini))
    (format
     *tsdb-io*
     "    - :initialize [ :cache _string_ ] [ :run _bool_ ]

        reload `.tsdbrc' file (if available) from user home directory and 
        rescan the test suite skeleton directory; optional :cache argument,
        loads tsdb(1) profile cache for databases matching _string_; :run t
        background the cache load; finally, display resulting status;~
      ~%~%"))
  
  (when (member command (list :all :list :lis :li :l))
    (format
     *tsdb-io*
     "    - :list [ _string_ ]

        search directory _string_ (tsdb(1) home by default) for well-formed
        tsdb(1) databases; display database status and size;~%~%"))

  (when (member command (list :all :skeletons :ske :sk :s))
    (format
     *tsdb-io*
     "    - :skeletons [ _string_ ]

        list available skeleton databases (name, description, and size) or
        make _string_ the new default skeleton;~%~%"))

  (when (member command (list :all :create :cre :cr))
    (format
     *tsdb-io*
     "    - :create [ _string_ ] [ :skeleton _string_ ]

        create new database called _string_ (as a subdirectory in the tsdb(1)
        home directory) from the skeleton database (or the default skeleton if
        the :skeleton option ist omitted; if the name for the new database has
        a trailing `/' and is an existing directory, a new name will be
        generated from the current date;)~%~%"))

  (when (member command (list :all :vocabulary :voc :vo :v))
    (format
     *tsdb-io*
     "    - :vocabulary [_string_ ] [ :condition _string_ ] [ :load _keyword_ ]

        extract alphabetically sorted list of vocabulary used in database 
        _string_ (use :condition options as with :process below); the :load 
        option determines whether lexical entries are expanded as needed and
        whether the loading is quiet or verbose; _keyword_ can be one of
        :off, :quiet (default), or :verbose;~%~%"))

  (when (member command (list :all :process :pro :pr))
    (format
     *tsdb-io*
     "    - :process [ _string_ ] [ :condition _string_ ] [ :run _integer_ ]

        select test data from database called _string_ (or the default db);
        use optional (T)SQL condition to constrain the selection (e.g. use
        `i-wf = 1 && i-length >= 10' for longish grammatical sentences) and
        number the test run _integer_ (a new unique identifier will be used
        without the :run option);~%~%"))
  
  (when (member command (list :all :podium :pod :po))
    (format
     *tsdb-io*
     "    - :podium [ :cache _string_ ]

        start up tsdb(1) podium (the graphical user interface); see :initialize
        above for the optional :cache argument;~%~%"))

  (when (member command (list :all :cpus :cpu :cp))
    (format
     *tsdb-io*
     "    - :cpus [ _keyword_ ] [ :file _string_ ] [ :reset _bool_ ]

        list or activate [incr tsdb()] cpus; _keyword_ is a class name (used
        in the cpu definition); write cpu output to file _string_ (defaults to
        `/tmp/pvm.debug.user' --- where `user' is the active account name);
        :reset defaults to `t' and shuts down all existing cpus before cpu
        initialization;~%~%"))
  
  (when (member command (list :all :help :hel :he))
    (format
     *tsdb-io*
     "    - :help [ _keyword_ ]

        display help on tsdb(1) command _keyword_ or all commands.~%~%"))
  
  (when (equal command :all)
    (format
     *tsdb-io*
     "  _action_ may be abbreviated as long as the prefix is unambiguous; any of
  the options can only be specified when all optional arguments are present;
  option keys cannot be abbreviated.~%~%")))


