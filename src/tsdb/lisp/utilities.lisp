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
                                    verbose)

  #+:allegro
  (let ((environment (pairlis '(:print :stats :verbose :auto-step)
                              (list (sys:gsgc-switch :print)
                                    (sys:gsgc-switch :stats)
                                    (sys:gsgc-switch :verbose)
                                    (sys:gsgc-parameter :auto-step))))
        (statsp (member :stats verbosity :test #'eq))
        (verbosep (member :verbose verbosity :test #'eq)))
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
      (excl:gc :tenure)
      #-(version>= 5 0)
      (busy :gc :end)
      (setf *tsdb-tenured-bytes* 0)
      (when verbose (format *tsdb-io* " done.~%")))
    (when (and (null tenure) (eq gc :global) verbose)
      (format
       *tsdb-io*
       "install-gc-strategy(): ~
        tenure disabled; supressing preliminary gc()s.~%"))
    (cons (cons :gc (if (and (null tenure) (eq gc :global)) nil gc)) 
          environment)))

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

(defun remove-and-insert-punctuation (string)
  (let* ((string (remove #\, string))
         (string (remove #\; string))
         (string (remove #\: string))
         (string (remove #\. string))
         (string (remove #\? string))
         (string (remove #\! string))
         (string (string-right-trim (list #\space #\tab) string)))
    (concatenate 'string string ".")))

#+:cray
(defun normalize-string (string)
  (if string
    (let* ((string (nsubstitute #\Space #\Newline string))
           (string (nsubstitute #\# #\@ string))
           (string 
            (if (> (length string) 2)
              (reduce #'(lambda (x y)
                          (if (and (eq x #\Space)
                                   (eq (char (string y) 0) #\Space))
                              (string y)
                            (concatenate 'string (string x) (string y))))
                      string :from-end t)
              string)))
      (string-trim '(#\Space #\Tab) string))
    ""))

(defun normalize-string (string &key escape)
  (if string
    (let* ((result (make-array 4096
                               :element-type 'character
                               :adjustable t :fill-pointer 0)))
      (loop
          with space = t
          for c across string
          ;;
          ;; _fix_me_
          ;; as it stands, normalize-string() gets only called on strings that
          ;; we are about to insert into tsdb(1); in case we write the data
          ;; files directly, we have to obey tsdb(1) escape conventions; thus,
          ;; the `@' --> `\s' translation should usually be deactivated.
          ;;                                              (26-aug-99  -  oe)
          when (and escape (char= c *tsdb-ofs*)) do
            (vector-push-extend #\\ result 2048)
            (vector-push-extend #\s result 2048)
          else when (member c '(#\Space #\Newline #\Tab)) do
            (unless space
              (vector-push-extend #\Space result 2048)
              (setf space t))
          else do
            (vector-push-extend c result 2048)
            (setf space nil)
          finally
            (when (and space (not (zerop (fill-pointer result))))
              (decf (fill-pointer result))))
      result)
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

(defun complement! (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun find! (item sequence 
              &key (test #'eql) key)
  (funcall #'remove item sequence 
           :test #'(lambda (&rest items)
                     (not (apply test items)))
           :key key))

(defun current-application ()
  (cond ((and (member :page *features* :test #'eq) 
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
        ((member :babel *features* :test #'eq)
         "BABEL")
        (t
         "standalone")))

(defun current-grammar ()
  (cond 
   ((and (find-symbol "*GRAMMAR-VERSION*" "USER")
         (boundp (find-symbol "*GRAMMAR-VERSION*" "USER")))
    (symbol-value (find-symbol "*GRAMMAR-VERSION*" "USER")))
   ((and (member :page *features*) 
         (find-package "DISCO")
         (find-symbol "*GRAMMAR-VERSION*" "DISCO")
         (boundp (find-symbol "*GRAMMAR-VERSION*" "DISCO")))
    (symbol-value (find-symbol "*GRAMMAR-VERSION*" "DISCO")))
   (t "anonymous")))

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
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (get-decoded-time)
    (declare (ignore foo bar baz))
    (let ((months '("jan" "feb" "mar" "apr" "may" "jun" 
                    "jul" "aug" "sep" "oct" "nov" "dec")))
      (cond
       ((null long)
        (format nil "~a-~a-~a" day month year))
       ((member long '(:usa :us :reverse))
        (format nil "~2,'0d-~2,'0d-~2,'0d" (mod year 100) month day))
       ((member long '(:pretty :readable))
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d h)" 
         day (nth (- month 1) months) year hour minute))
       (t
        (format
         nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
         day month year hour minute second))))))

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

(defun create-output-stream (file &optional append)
  (cond
   ((or (stringp file) (stringp append))
    (open (if (stringp append) append file)
          :direction :output 
          :if-exists (if append :append :supersede)
          :if-does-not-exist :create))
   ((or file append) (or file append))
   (t *tsdb-io*)))

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
                 (chart
                  (unless skeletonp
                    (let ((n (tcount data "rule" :absolute t :quiet t)))
                      (and n (not (zerop n))))))
                 (items (tcount data "item" :absolute t :quiet skeletonp))
                 (parses (unless skeletonp (tcount data "parse" :absolute t))))
            (pairlis (list :database 
                           :path :status :items :parses :chart)
                     (list (namestring language) 
                           data status items parses chart))))))))

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

(defun purge-directory (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (pattern (make-pathname :directory path :name :wild))
         (contents (directory pattern)))
    (when contents
      (dolist (file contents)
        (delete-file file))
      path)))

(defun mkdir (path)
  #+(and :allegro-version>= (version>= 5 0) :excl)
  (return-from mkdir
    (excl:make-directory path))
  #+:unix
  (return-from mkdir
    (run-process (format nil "/bin/mkdir ~a" (namestring path)) :wait t))
  (error 
   "mkdir(): ~
    unable to create directories on this platform; see `utilities.lisp'."))

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
        (:purge
         (dolist (file *tsdb-profile-files*)
           (let* ((name (concatenate 'string directory file))
                  (compressed (concatenate 'string name ".gz")))
             (when (probe-file name)
               (delete-file name))
             (when (probe-file compressed)
               (delete-file compressed))
             (with-open-file (foo name :direction :output 
                              :if-exists :supersede)))))))))

(defun suggest-test-run-directory (skeleton)
  (let* ((grammar (current-grammar))
         (grammar (when (stringp grammar) grammar))
         (open (when (stringp grammar) (position #\( grammar)))
         (close (when (stringp grammar) (position #\) grammar :from-end t)))
         (version (when (and open close) (subseq grammar (+ open 1) close)))
         (grammar (if version (subseq grammar 0 open) grammar))
         (grammar (string-downcase (string-trim '(#\Space) grammar)))
         (date (current-time :long :usa))
         (system (current-application))
         (result (make-array 42 
                             :element-type 'character 
                             :adjustable t
                             :fill-pointer 0)))
    (if (stringp *tsdb-instance-template*)
      (loop
          initially (loop
                        for c across *tsdb-home*
                        do (vector-push-extend c result 42))
          with skip = 0
          for c across *tsdb-instance-template*
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
       "~a~a/~@[~a/~]~a/~a/~a" 
       *tsdb-home* grammar version skeleton date system))))

(defun list2tcl (list &key format)
  (let ((format (format nil "{~~{~a ~~}}" (or format "~s"))))
    (format nil format list)))
