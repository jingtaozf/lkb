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

(defun reset-tsdb-paths ()
  (setf
    *tsdb-application*
    (format
      nil "exec ~a"
      (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                                 :name "tsdb"))))
  (setf
    *tsdb-home* 
    (namestring (dir-append (get-sources-dir "tsdb") '(:relative "tsdb")))))

(defun install-gc-strategy (gc &key (verbosity *tsdb-gc-verbosity*)
                                    (tenure *tsdb-tenure-p*)
                                    burst verbose)

  #+:allegro
  (let ((environment (pairlis '(:print :stats :verbose :auto-step)
                              (list (sys:gsgc-switch :print)
                                    (sys:gsgc-switch :stats)
                                    (sys:gsgc-switch :verbose)
                                    (sys:gsgc-parameter :auto-step))))
        (statsp (member :stats verbosity :test #'eq))
        (verbosep (member :verbose verbosity :test #'eq))
        (*tsdb-tenured-bytes-limit* nil))
    (setf (system:gsgc-switch :dump-on-error) t)
    (setf (sys:gsgc-switch :print) (or verbosep statsp))
    (setf (sys:gsgc-switch :verbose) verbosep)
    (setf (sys:gsgc-switch :stats) statsp)
    (setf (sys:gsgc-parameter :auto-step) tenure)
    (setf (sys:gsgc-parameter :generation-spread) *tsdb-generation-spread*)
    (unless tenure
      (setf (system:gsgc-switch :gc-old-before-expand) t)
      (when verbose
        (format
         *tsdb-io*
         "install-gc-strategy(): ~
          disabling tenure; global garbage collection ..."))
      #-(version>= 5 0)
      (busy :gc :start)
      (excl:gc (if burst :mark-for-tenure :tenure))
      #-(version>= 5 0)
      (busy :gc :end)
      (setf *tsdb-tenured-bytes* 0)
      (when verbose (format *tsdb-io* " done.~%")))
    (when (and (null tenure) (eq gc :global) verbose)
      (format
       *tsdb-io*
       "install-gc-strategy(): ~
        tenure disabled; supressing preliminary gc()s.~%"))
    (acons :gc (if (and (null tenure) (eq gc :global)) nil gc) environment)))

(defun restore-gc-strategy (strategy)
  (when strategy
    #+:allegro
    (setf (sys:gsgc-switch :print) (get-field :print strategy))
    #+:allegro
    (setf (sys:gsgc-switch :stats) (get-field :stats strategy))
    #+:allegro
    (setf (sys:gsgc-switch :verbose) (get-field :verbose strategy))
    #+:allegro
    (setf (sys:gsgc-parameter :auto-step) (get-field :auto-step strategy))))

(defun gc-statistics-reset (&optional code)
  (unless (arrayp *tsdb-gc-statistics*)
    (setf *tsdb-gc-statistics* (make-array 5)))
  (loop
      for key in '(:global :scavenge :new :old)
      do (setf (gc-statistics key) 0))
  (when (eq code :all) (setf (gc-statistics :efficiency) nil))
  *tsdb-gc-statistics*)

(defun find-function (description)
  (ignore-errors
   (typecase description
     (null nil)
     (string (find-function (read-from-string description)))
     (symbol (symbol-function description))
     (function description))))

(defun remove-and-insert-punctuation (string)
  (let* ((string (remove #\, string))
         (string (remove #\; string))
         (string (remove #\: string))
         (string (remove #\. string))
         (string (remove #\? string))
         (string (remove #\! string))
         (string (string-right-trim (list #\space #\tab) string)))
    (concatenate 'string string ".")))

(defun normalize-string (string &key escape (normalize t))
  (if string
    (loop
        with string = (if (stringp string)
                        string 
                        (with-standard-io-syntax 
                          (let ((*package* (find-package :tsdb)))
                            (write-to-string string))))
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        with space = t
        for c across string
        ;;
        ;; _fix_me_
        ;; as it stands, normalize-string() gets only called on strings that
        ;; we are about to insert into tsdb(1); in case we write the data
        ;; files directly, we have to obey tsdb(1) escape conventions; thus,
        ;; the `@' --> `\s' translation should usually be deactivated.
        ;;                                              (26-aug-99  -  oe)
        when (and escape (or (char= c *tsdb-ofs*) (char= c #\\))) do
          (vector-push #\\ result)
          (vector-push (if (char= c *tsdb-ofs*) #\s #\\) result)
          (when (zerop (decf padding))
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
          (setf space nil)
        else when (and normalize (member c '(#\Space #\Newline #\Tab))) do
          (when space (incf padding))
          (unless space
            (vector-push #\Space result)
            (setf space :space))
        else do
          (vector-push c result)
          (setf space nil)
        finally
          (when (and (eq space :space) (not (zerop (fill-pointer result))))
            (decf (fill-pointer result)))
          (return result))
    ""))

(defun string-strip (prefix string)
  (if (search prefix string)
    (subseq string (length prefix))
    string))

(defun shell-escape-quotes (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\')
        (concatenate 'string "'\\''" (shell-escape-quotes (subseq string 1)))
        (concatenate 
            'string (string prefix) (shell-escape-quotes (subseq string 1)))))
    string))

(defun tsdb-escape-quotes (string)
  (if (and (stringp string) (>= (length string) 1))
    (let ((prefix (elt string 0)))
      (if (equal prefix #\")
        (concatenate 'string "\\\"" (tsdb-escape-quotes (subseq string 1)))
        (concatenate 
            'string (string prefix) (tsdb-escape-quotes (subseq string 1)))))
    string))

(defun tcl-escape-braces (string)
  (if (and string (stringp string))
    (loop
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        when (or (char= c #\{) (char= c #\})) do
          (vector-push #\\ result)
          (vector-push c result)
          (when (zerop (decf padding))
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
        else do
          (vector-push c result)
        finally
          (return result))
    string))

(defun ith-nth (list ith nth)
  (loop
      with i = 1
      with n = (floor (length list) (if (zerop nth) 1 nth))
      for j from 1
      for foo in list
      when (or (zerop nth) (and (not (zerop n)) (= i ith)))
      collect foo into result
      else collect foo into complement
      when (and (not (zerop n)) (zerop (mod j n))
                (not (and (= ith nth) (= i ith))))
      do (incf i)
      finally (return (values result complement))))

(defun ith-n (list ith n)
  (when (> ith 0)
    (loop
        with list = (loop
                        for foo on list 
                        for i from 1 to (* n (- ith 1))
                        finally (return foo))
        for foo in list
        while (> n 0) collect foo do (decf n))))

(defun complement! (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun find! (item sequence 
              &key (test #'eql) key)
  (funcall #'remove item sequence 
           :test #'(lambda (&rest items)
                     (not (apply test items)))
           :key key))

(defun clients-grammar ()
  (let* ((grammars (loop
                       for client in *pvm-clients*
                       for cpu = (client-cpu client)
                       for grammar = (cpu-grammar cpu)
                       when grammar collect grammar))
         (grammars 
          (remove-duplicates grammars :test #'string-equal)))
    (when (= (length grammars) 1) (first grammars))))

(defun current-application ()
  (cond ((and *pvm-clients*
              (let* ((names (loop
                                for client in *pvm-clients*
                                for cpu = (client-cpu client)
                                for name = (cpu-name cpu)
                                when name collect name))
                     (names (remove-duplicates names :test #'string-equal)))
                (when (= (length names) 1) (first names)))))
        ((and (member :page *features* :test #'eq) 
              (not (member :lkb *features* :test #'eq)))
         "PAGE")
        ((and (member :lkb *features* :test #'eq) 
              (not (member :page *features* :test #'eq)))
         "LKB")
        ((and (member :lkb *features* :test #'eq) 
              (member :page *features* :test #'eq))
         "JANUS")
        ((member :chic *features* :test #'eq)
         "CHIC")
        ((member :anlt *features* :test #'eq)
         "A(N)NLT")
        ((member :babel *features* :test #'eq)
         "BABEL")
        (t
         "standalone")))

(defun current-tsdb ()
  *tsdb-version*)

#-:pvm
(defun current-user ()
  (or #+(and :allegro-version>= (version>= 5 0)) 
      (sys:user-name)
      #+(and :allegro (not (and :allegro-version>= (version>= 5 0))))
      (system:getenv "USER")
      #+(and :mcl :powerpc) 
      (ccl:process-name ccl:*current-process*)
      #+:lucid 
      (lcl:environment-variable "USER")
      "nobody"))

(defun current-platform ()
  (format 
   nil 
   "~a (~a)"
   (lisp-implementation-type) (lisp-implementation-version)))

(defun current-os ()
  (software-version))

(defun current-host ()
  (short-site-name))

#+(and (not :pvm) :allegro-version>= (version>= 5 0))
(def-foreign-call 
    (current-pid "getpid")
    (:void)
  :returning :int)
#+(and (not :pvm) :allegro-version>= (not (version>= 5 0)))
(defforeign 
    'current-pid
    :entry-point "getpid"
    :arguments nil
    :return-type :integer)
#+(and (not :pvm) (not :allegro-version>=))
(defun getpid () (random (expt 2 15)))

(defun current-time (&key long)
  (decode-time (get-universal-time) :long long))

(defun decode-time (time &key long)
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (decode-universal-time time)
    (declare (ignore foo bar baz))
    (let ((months '("jan" "feb" "mar" "apr" "may" "jun" 
                    "jul" "aug" "sep" "oct" "nov" "dec")))
      (cond
       ((null long)
        (format nil "~a-~a-~a" day month year))
       ((member long '(:usa :us :reverse))
        (format nil "~2,'0d-~2,'0d-~2,'0d" (mod year 100) month day))
       ((member long '(:tsdb))
        (format
         nil "~a-~a-~a ~2,'0d:~2,'0d" 
         day (nth (- month 1) months) year hour minute))
       ((member long '(:pretty :readable))
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d h)" 
         day (nth (- month 1) months) year hour minute))
       ((eq long :short)
        (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))
       (t
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
         day month year hour minute second))))))

(defun parse-date (date)
  (multiple-value-bind (day offset) (parse-integer date :junk-allowed t)
    (let* ((month (subseq date (+ offset 1) (+ offset 4)))
           (month (cond
                   ((string-equal month "jan") 1)
                   ((string-equal month "feb") 2)
                   ((string-equal month "mar") 3)
                   ((string-equal month "apr") 4)
                   ((string-equal month "may") 5)
                   ((string-equal month "jun") 6)
                   ((string-equal month "jul") 7)
                   ((string-equal month "aug") 8)
                   ((string-equal month "sep") 9)
                   ((string-equal month "oct") 10)
                   ((string-equal month "nov") 11)
                   ((string-equal month "dec") 12))))
      (multiple-value-bind (year offset)
          (parse-integer date :start (+ offset 5) :junk-allowed t)
        (multiple-value-bind (hour offset)
            (parse-integer date :start (+ offset 1) :junk-allowed t)
          (multiple-value-bind (minutes offset)
              (parse-integer date :start (+ offset 1) :junk-allowed t)
            (multiple-value-bind (seconds)
                (parse-integer date :start (+ offset 1) :junk-allowed t)
              (encode-universal-time 
               (or seconds 0) (or minutes 0) (or hour 0)
               day month year))))))))

(defun pprint-memory-usage (result &optional (separator #\Space))
  (let* ((conses (get-field+ :conses result 0))
         (symbols (get-field+ :symbols result 0))
         (others (get-field+ :others result 0))
         (total (+ conses symbols others)))
    (concatenate 'string
      (pprint-potentially-large-integer conses)
      (unless (zerop conses)
        (string separator))
      (pprint-potentially-large-integer symbols)
      (unless (zerop symbols)
        (string separator))
      (pprint-potentially-large-integer others)
      (unless (zerop others)
        (string separator))
      (unless (zerop total)
        "= ")
      (pprint-potentially-large-integer total))))


(defun pprint-potentially-large-integer (n)
  (cond ((zerop n) "")
        ((>= n (expt 2 30)) (format nil "~,1fG" (/ n (expt 2 30))))
        ((>= n (expt 2 20)) (format nil "~,1fM" (/ n (expt 2 20))))
        ((>= n (expt 2 10)) (format nil "~,1fK" (/ n (expt 2 10))))
        (t (format nil "~d" n))))

(defun create-output-stream (file &optional append &key (encoding :utf-8))
  #-(and :allegro-version>= (version>= 6 0))
  (declare (ignore encoding))
  (let ((stream
         (cond
          ((or (stringp file) (pathnamep file))
           (open file
                 :direction :output 
                 :if-exists :supersede
                 :if-does-not-exist :create))
          ((or (stringp append) (pathnamep append))
           (open append
                 :direction :output 
                 :if-exists :append
                 :if-does-not-exist :create))
          ((or file append) (or file append))
          (t *tsdb-io*))))
    #+(and :allegro-version>= (version>= 6 0))
    (unless (or (eq stream file) (eq stream append) (eq stream *tsdb-io*))
      (setf (stream-external-format stream) 
        (excl:find-external-format encoding)))
    stream))

(defun verify-tsdb-directory (language &key absolute skeletonp)
  (let ((data 
         (if absolute (namestring language) (find-tsdb-directory language))))
    (when (and data 
               (probe-file (make-pathname :directory data :name "relations")))
      (let* ((command (format
                       nil 
                       "~a -home=~a -verify -quiet -pager=null"
                       *tsdb-application* data))
             (status (if skeletonp 0 (run-process command :wait t))))
        (when (zerop status)
          (let* ((status 
                  (cond
                   (skeletonp :skeleton)
                   ((probe-file 
                     (make-pathname :directory data :name "item.gz"))
                    :ro)
                   (t :rw)))
                 (result
                  (or (file-size 
                       (make-pathname :directory data :name "result"))
                      (file-size 
                       (make-pathname :directory data :name "result.gz"))))
                 (resultp (and (not skeletonp) (numberp result) (> result 0)))
                 (rule
                  (or (file-size 
                       (make-pathname :directory data :name "rule"))
                      (file-size 
                       (make-pathname :directory data :name "rule.gz"))))
                 (rulep (and (not skeletonp) (numberp rule) (> rule 0)))
                 (tree
                  (or (file-size 
                       (make-pathname :directory data :name "tree"))
                      (file-size 
                       (make-pathname :directory data :name "tree.gz"))))
                 (treep (and (not skeletonp) (numberp tree) (> tree 0)))
                 (score
                  (or (file-size 
                       (make-pathname :directory data :name "score"))
                      (file-size 
                       (make-pathname :directory data :name "score.gz"))))
                 (scorep (and (not skeletonp) (numberp score) (> score 0)))
                 (items (tcount data "item" :absolute t :quiet skeletonp))
                 (parses (unless skeletonp (tcount data "parse" :absolute t))))
            (pairlis (list :database 
                           :path :status :items :parses 
                           :resultp :rulep :treep :scorep)
                     (list (namestring language) 
                           data status items parses 
                           resultp rulep treep scorep))))))))

(defun file-size (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (stream (open path :direction :input :if-does-not-exist nil))
         (size (and stream (file-length stream))))
    (when stream
      (close stream)
      size)))

(defun directoryp (path)
  #+(and :allegro-version>= (version>= 6 0))
  (excl:probe-directory path)
  #-(and :allegro-version>= (version>= 6 0))
  (probe-file (make-pathname :directory path :name ".")))

(defun subdirectories (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (pattern (make-pathname :directory path :name :wild))
         (contents (directory pattern)))
    (when contents
      (cons path (mapcan #'subdirectories contents)))))

(defun cp (source target)
  (when (probe-file source)
    (with-open-file (in source :direction :input
                     :element-type '(unsigned-byte 8))
      (with-open-file (out target :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
        (loop
            for c = (read-byte in nil nil)
            while c
            do (write-byte c out))))
    target))

(defun wc (file)
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (loop
          with characters = 0
          with lines = 0
          for c = (read-char stream nil nil)
          while c
          when (char= c #\newline) do (incf lines)
          do (incf characters)
          finally 
            (return (pairlis '(:characters :lines)
                             (list characters lines)))))))

(defun touch (file &key (if-exists :supersede))
  (with-open-file (foo file :direction :output
                   :if-exists if-exists :if-does-not-exist :create)))

(defun purge-directory (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (pattern (make-pathname :directory path :name :wild))
         (contents (directory pattern)))
    (when contents
      (dolist (file contents)
        (delete-file file))
      path)))

(defun mkdir (path &key parentp)
  (if parentp
    (loop
        with path = (if (pathnamep path) path (make-pathname :directory path))
        with directories = (pathname-directory path)
        for i from (- (length directories) 2) downto 0
        for directory = (merge-pathnames
                         (make-pathname :directory (butlast directories i))
                         path)
        unless (probe-file directory) do (mkdir directory)
        finally (return (probe-file path)))
    (or
     #+(and :allegro-version>= (version>= 5 0) :excl)
     (return-from mkdir
       (excl:make-directory path))
     #+:unix
     (return-from mkdir
       (run-process (format nil "/bin/mkdir ~a" (namestring path)) :wait t))
     (error 
      "mkdir(): ~
       unable to create directories on this platform; ~
       see `utilities.lisp'."))))

(defun directory2file (string)
  (substitute #\. *tsdb-slash* string :test #'char=))

(defun remove-key-argument (key arguments &optional result)
  (cond
   ((null arguments) (nreverse result))
   ((null (rest arguments)) (nreverse (cons (first arguments) result)))
   ((eq key (first arguments))
    (remove-key-argument key (rest (rest arguments)) result))
   (t
    (remove-key-argument key (rest (rest arguments)) 
                         (append (list (second arguments) (first arguments))
                                 result)))))

(defun find-key-argument (key arguments)
  (let ((position (position key arguments)))
    (when position (nth (+ position 1) arguments))))

(defun purge-test-run (data &key (action :forget))

  (purge-profile-cache data)
  (unless (eq data :all)
    (let* ((directory (find-tsdb-directory data)))
      (case action
        ((:purge :tree :score)
         (loop
             for file in (case action
                           (:score '("score"))
                           (:tree *tsdb-redwoods-files*)
                           (:purge *tsdb-profile-files*))
             for name = (concatenate 'string directory file)
             for compressed = (concatenate 'string name ".gz")
             when (probe-file name) do (delete-file name)
             when (probe-file compressed) do (delete-file compressed)
             do
               (with-open-file (foo name :direction :output 
                                :if-exists :supersede))))
        (:empty
         (loop
             with pattern = (make-pathname :directory directory :name :wild)
             with files = (directory pattern)
             for file in files
             for name = (pathname-name file)
             unless (equal name "relations") do
               (with-open-file (foo file :direction :output 
                                :if-exists :supersede))))))))

(defun suggest-test-run-directory (skeleton &key (absolute t))
  (let* ((grammar (current-grammar))
         (grammar (when (stringp grammar) grammar))
         (open (when (stringp grammar) (position #\( grammar)))
         (close (when (stringp grammar) (position #\) grammar :from-end t)))
         (version (when (and open close) (subseq grammar (+ open 1) close)))
         (grammar (if version (subseq grammar 0 open) grammar))
         (grammar (string-downcase (string-trim '(#\Space) grammar)))
         (date (current-time :long :usa))
         (system (current-application))
         (template (or (let* ((templates (loop
                                             for client in *pvm-clients*
                                             for cpu = (client-cpu client)
                                             for template = (cpu-template cpu)
                                             when template collect template))
                              (templates (remove-duplicates 
                                          templates :test #'string-equal)))
                         (when (= (length templates) 1) (first templates)))
                       *tsdb-instance-template*))
         (result (make-array 42 
                             :element-type 'character 
                             :adjustable t
                             :fill-pointer 0)))
    (if (stringp template)
      (loop
          initially (when absolute
                      (loop
                          for c across *tsdb-home*
                          do (vector-push-extend c result 42)))
          with skip = 0
          for c across template
          for special = (if control
                          (case c
                            (#\g grammar)
                            (#\v version)
                            (#\t skeleton)
                            (#\d date)
                            (#\s system)
                            (#\% "%")
                            (t ""))
                          :null)
          for control = (unless control (char= c #\%))
          do
            (cond
             ((null special)
              (incf skip))
             ((stringp special)
              (loop
                  for c across (string-downcase special)
                  do (vector-push-extend c result 42)))
             ((> skip 0)
              (decf skip))
             ((null control)
              (vector-push-extend c result 42)))
          finally (return result))
      (format 
       nil 
       "~:[~*~;~a~]~a/~@[~a/~]~a/~a/~a" 
       absolute *tsdb-home* grammar version skeleton date system))))

(defun list2tcl (list &key format)
  (let ((format (format nil "{~~{~a ~~}}" (or format "~s"))))
    (format nil format list)))
