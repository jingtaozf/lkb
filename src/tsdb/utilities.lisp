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

(defmacro item-i-id (item)
  `(first ,item))

(defmacro item-i-wf (item)
  `(second ,item))

(defmacro item-i-input (item)
  `(third ,item))

(defmacro item-o-ignore (item)
  `(fourth ,item))

(defmacro item-o-wf (item)
  `(fifth ,item))

(defmacro item-o-gc (item)
  `(sixth ,item))

(defmacro item-o-tasks (item)
  `(seventh ,item))

(defmacro item-o-derivation (item)
  `(eighth ,item))

(defmacro get-field (field alist)
  `(rest (assoc ,field ,alist)))

(defmacro tsdb-ignore-p (&rest foo)
  (declare (ignore foo))
  t)

(defmacro find-tsdb-directory (language)
  `(let* ((data (dir-append (make-pathname :directory *tsdb-home*)
                            (list :relative ,language))))
     (namestring data)))

(defmacro find-skeleton (name)
  `(let* ((name (if (keywordp ,name) (string ,name) ,name)))
     (find name *tsdb-skeletons* 
           :key #'(lambda (foo) (get-field :path foo))
           :test #'equal)))

(defmacro find-skeleton-directory (skeleton)
  `(let* ((path (dir-append (namestring *tsdb-skeleton-directory*)
                            (list :relative (get-field :path ,skeleton)))))
     (namestring path)))

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

(defun remove-and-insert-punctuation (string)
  (let* ((string (remove #\, string))
         (string (remove #\; string))
         (string (remove #\: string))
         (string (remove #\. string))
         (string (remove #\? string))
         (string (remove #\! string))
         (string (string-right-trim (list #\space #\tab) string)))
    (concatenate 'string string ".")))

(defun normalize-string (string)
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
    (string-trim '(#\Space #\Tab) string)))

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

(defun current-grammar ()
  (cond 
   #+:page
   ((boundp 'disco::*grammar-version*) disco::*grammar-version*)
   ((boundp 'common-lisp-user::*grammar-version*) 
    common-lisp-user::*grammar-version*)
   (t "anonymous")))

(defun current-user ()
  (or #+:allegro-v5.0 (sys:user-name)
      #+(and :allegro (not :allegro-v5.0)) (system:getenv "USER")
      #+(and :mcl :powerpc) (ccl:process-name ccl:*current-process*)
      #+:lucid (lcl:environment-variable "USER")
      "nobody"))

(defun current-platform ()
  (format 
   nil 
   "~a (~a)"
   (lisp-implementation-type) (lisp-implementation-version)))

(defun current-os ()
  (software-version))

(defun current-time (&key long)
  (multiple-value-bind (second minute hour day month year foo bar baz)
      (get-decoded-time)
    (declare (ignore foo bar baz))
    (cond
     ((null long)
      (format nil "~a-~a-~a" day month year))
     ((member long (list :usa :us :reverse))
      (format nil "~2,'0d-~2,'0d-~2,'0d" (mod year 100) month day))
     (t
      (format
       nil "~a-~a-~a (~2,'0d:~2,'0d:~2,'0d)"
       day month year hour minute second)))))

(defun load-average ()
  (multiple-value-bind (output foo pid)
    (run-process "/bin/uptime" :wait nil
                 :output :stream :input "/dev/null" :error-output nil)
    (declare (ignore foo))
    #+:allegro (sys:os-wait nil pid)
    (let* ((line (read-line output nil :eof))
           (colon (position #\: line :from-end t))
           (line (subseq line (+ colon 1)))
           (current (read-from-string line nil 0))
           (comma (position #\, line))
           (line (subseq line (+ comma 1)))
           (recent (read-from-string line nil 0))
           (comma (position #\, line))
           (line (subseq line (+ comma 1)))
           (past (read-from-string line nil 0)))
      (close output)
      (list current recent past))))

(defun current-host ()
  (short-site-name))

(defun pprint-memory-usage (result &optional (separator #\Space))
  (let* ((conses (* (or (get-field :conses result) 0) 8))
         (symbols (* (or (get-field :symbols result) 0) 24))
         (others (get-field :others result)))
    (concatenate 'string
      (pprint-potentially-large-integer conses)
      (string separator)
      (pprint-potentially-large-integer symbols)
      (string separator)
      (pprint-potentially-large-integer others)
      " = "
      (pprint-potentially-large-integer (+ conses symbols others)))))


(defun pprint-potentially-large-integer (n)
  (cond ((zerop n) "")
        ((>= n (expt 2 30)) (format nil "~,1fG" (/ n (expt 2 30))))
        ((>= n (expt 2 20)) (format nil "~,1fM" (/ n (expt 2 20))))
        ((>= n (expt 2 10)) (format nil "~,1fK" (/ n (expt 2 10))))
        (t (format nil "~d" n))))

(defun verify-tsdb-directory (language &key absolute)
  (let ((data 
         (if absolute (namestring language) (find-tsdb-directory language))))
    (when (and data 
               (probe-file (namestring (make-pathname :directory data 
                                                      :name "relations"))))
      (let* ((command (format
                       nil 
                       "~a -home=~a -verify -quiet"
                       *tsdb-application* data))
             (status (run-process command :wait t)))
        (when (zerop status)
          (let ((status 
                 (if (probe-file (namestring (make-pathname :directory data
                                                            :name "item.gz")))
                   :ro
                   :rw))
                (chart
                 (or 
                  (probe-file 
                   (namestring (make-pathname :directory data 
                                              :name "edge.cache")))
                  (probe-file 
                   (namestring (make-pathname :directory data 
                                              :name "edge.lisp")))))
                (items (length (select "i-id" :integer "item" nil data 
                                       :absolute t :unique t)))
                (parses (length (select "parse-id" :integer "parse" nil data 
                                        :absolute t :unique t))))
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

(defun directory2file (string)
  (substitute #\. #\/ string :test #'char=))

(defmacro make-meter (start end)
  `(pairlis (list :start :end) (list ,start ,end)))

(defmacro mduration (meter)
  `(when ,meter (- (get-field :end ,meter) (get-field :start ,meter))))

(defmacro madjust (action meter value)
  `(when ,meter
     (let* ((start (get-field :start ,meter))
            (end (get-field :end ,meter))
            (duration (- end start)))
       (case ',action
         (* (setf end (+ start (* duration ,value))))
         (/ (setf end (+ start (/ duration ,value))))
         (+ (setf start (+ start ,value))
            (setf end (+ end ,value))))
       (make-meter start end))))

(defun remove-key-argument (key arguments &optional result)
  (cond
   ((null arguments) (nreverse result))
   ((eq key (first arguments))
    (remove-key-argument key (rest (rest arguments)) result))
   (t
    (remove-key-argument key (rest (rest arguments)) 
                         (append (list (second arguments) (first arguments))
                                 result)))))

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

(defun purge-profile-cache (data)
  (maphash #'(lambda (key foo)
               (declare (ignore foo))
               (let* ((position (position #\@ key))
                      (prefix (subseq key 0 position)))
                 (when (or (eq data :all) (string= data prefix))
                   (remhash key *tsdb-profile-cache*))))
           *tsdb-profile-cache*))

(defun suggest-test-run-directory (skeleton)
  (let* ((grammar (current-grammar))
         (grammar (when (stringp grammar) grammar))
         (open (when (stringp grammar) (position #\( grammar)))
         (close (when (stringp grammar) (position #\) grammar :from-end t)))
         (version (when (and open close) (subseq grammar (+ open 1) close)))
         (grammar (if version (subseq grammar 0 open) grammar))
         (grammar (string-downcase (string-trim '(#\Space) grammar)))
         (date (current-time :long :usa)))
    (format 
     nil 
     "~a~a/~@[~a/~]~a/~a/~a" 
     *tsdb-home* grammar version skeleton date #+:page "page" #+:lkb "lkb")))

(defun list2tcl (list)
  (format nil "{~{~s ~}}" list))
