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

(defun initialize-tsdb (&optional cache &key action background name pattern)
  
  (unless (and *tsdb-initialized-p* (null action))
    (let* ((tsdbrc (dir-and-name (user-homedir-pathname) ".tsdbrc"))
           (index (make-pathname :directory  *tsdb-skeleton-directory*
                                 :name *tsdb-skeleton-index*)))
      (when (and (or (null action) (member action '(:tsdbrc :all)))
                 (probe-file tsdbrc))
        (load tsdbrc))
      (let* ((home (if (stringp *tsdb-home*)
                     (make-pathname :directory *tsdb-home*)
                     *tsdb-home*)))
        (setf *tsdb-home* (when home (namestring home))))
      (when (and (or (null action) (member action '(:skeletons :all)))
                 (probe-file index))
        (setf *tsdb-skeletons* (with-open-file (stream index 
                                                :direction :input
                                                :if-does-not-exist :create)
                                 (read stream nil nil))))
      (when (and (or (null action) (member action '(:cache :all))) cache)
        (load-cache :background background :name name :pattern pattern))
      (unless action (setf *tsdb-initialized-p* t)))))

(defun call-tsdb (query language
                  &key redirection cache absolute unique quiet ro)
  (if *tsdb-server-mode-p*
    (call-tsdbd query language)
    (if cache
      (cache-query query language cache)
      (let* ((user (current-user))
             (file (format
                    nil "/tmp/.tsdb.io.~a.~a"
                    user (string-downcase (string (gensym "")))))
             (data (if absolute 
                     (namestring language) 
                     (find-tsdb-directory language)))
             (command (format
                       nil 
                       "~a -home=~a~:[~; -uniquely-project=off~]~
                       ~:[~; -quiet~]~:[~; -read-only~] ~
                        -string-escape=lisp -pager=null -max-results=0"
                       *tsdb-application* data unique quiet ro))
             (command (format
                       nil "~a -query='do \"~a\"'"
                       command file))
             (query (string-trim '(#\Space #\Tab #\Newline) query))
             (query (if (equal (elt query (- (length query) 1)) #\.)
                      (subseq query 0 (- (length query) 1))
                      query))
             (output (when (eq redirection :output)
                       (format
                        nil "/tmp/.tsdb.data.~a.~a"
                        user (string-downcase (string (gensym ""))))))
             (redirection 
              (if output (concatenate 'string " > \"" output "\"") ""))
             (query (concatenate 'string query redirection ".")))
        (with-open-file (stream file :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
          (format stream "~a~%" query))
        (multiple-value-bind (stream foo pid)
          (run-process
            command :wait nil
            :output :stream
            :input "/dev/null" :error-output nil)
          (declare (ignore foo))
          (let ((result (make-array
                         4096
                         :element-type 'character
                         :adjustable t             
                         :fill-pointer 0)))
            (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
                ((equal c :eof))
              (vector-push-extend c result 1024))
            (close stream)
            #+:allegro (sys:os-wait nil pid)
            (when (and output (probe-file output))
              (let ((stream 
                     (open output :direction :input :if-does-not-exist nil)))
                (do ((c (read-char stream nil :eof) 
                        (read-char stream nil :eof)))
                    ((equal c :eof))
                  (vector-push-extend c result 1024))
                (close stream)))
            (unless *tsdb-debug-mode-p*
              (delete-file file)
              (when (and output (probe-file output))
                (delete-file output)))
            result))))))

(defun create-cache (language &key (verbose t))
  (let* ((user (current-user))
         (file (format
                nil "/tmp/.tsdb.cache.~a.~a"
                user (string-downcase (string (gensym "")))))
         (stream (open file 
                       :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)))
    (when stream
      (format stream "set implicit-commit off.~%")
      (when verbose
        (format 
         *tsdb-io*
         "~&create-cache(): tsdb(1) write cache in `~a'.~%"
         file)
        (force-output *tsdb-io*)))
    (pairlis (list :database :file :stream :count)
             (list language file stream 0))))

(defun cache-query (query language cache)
  (let* ((database (get-field :database cache))
         (stream (get-field :stream cache))
         (query (string-trim '(#\Space #\Tab #\Newline) query))
         (query (if (equal (elt query (- (length query) 1)) #\.)
                  query
                  (concatenate 'string query "."))))
    (if (string-equal language database)
      (when stream 
        (format stream "~a~%" query)
        (force-output stream)
        (incf (get-field :count cache))
        (when (>= (get-field :count cache) *tsdb-flush-cache-threshold*)
          (flush-cache cache :verbose *tsdb-verbose-cache-flush-p*)
          (setf (get-field :count cache) 0)
          (let ((stream 
                 (open (get-field :file cache)
                       :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)))
            (format stream "set implicit-commit off.~%")
            (setf (get-field :stream cache) stream))))
      (format
       *tsdb-io*
       "~&cache-query() ignoring query to `~a' (on `~a' cache).~%"
       language database))))

(defun flush-cache (cache
                    &key (verbose t))
  (let ((database (get-field :database cache))
        (file (get-field :file cache))
        (stream (get-field :stream cache)))
    (format stream "~&commit.~%")
    (force-output stream)
    (close stream)
    (let* ((query (format nil "do \"~a\"" file)))
      (call-tsdb query database))
    (when verbose
      (format 
       *tsdb-io*
       "~&flush-cache(): tsdb(1) cache for `~a' flushed.~%"
       database file)
      (force-output *tsdb-io*))
    (unless *tsdb-debug-mode-p*
      (delete-file file))))

(defun largest-result-key (&optional (language *tsdb-data*)
                           &key (verbose t))
  (let* ((query "select c-id from csli")
         (result (call-tsdb query language)))
    (with-input-from-string (stream result)
      (do ((c-ids nil c-ids)
           (c-id (read stream nil :eof) (read stream nil :eof)))
        ((equal c-id :eof)
         (let ((c-id (if c-ids (apply #'max c-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-result-key(): largest `c-id' is ~a.~%"
              c-id)
             (force-output *tsdb-io*))
           c-id))
        (when (integerp c-id) (push c-id c-ids))))))

(defun largest-run-id (&optional (language *tsdb-data*)
                       &key (verbose t))     
  (let* ((query "select run-id from run")
         (result (call-tsdb query language)))
    (with-input-from-string (stream result)
      (do ((run-ids nil run-ids)
           (run-id (read stream nil :eof) (read stream nil :eof)))
        ((equal run-id :eof)
         (let ((run-id (if run-ids (apply #'max run-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-run-id(): largest `run-id' is ~a.~%"
              run-id))
           run-id))
        (when (integerp run-id) (push run-id run-ids))))))

(defun largest-parse-id (run-id &optional (language *tsdb-data*)
                         &key (verbose t))
  (let* ((data (select "parse-id" :integer "parse"
                       (format nil "run-id = ~d" run-id) language))
         (parse-ids (map 'list #'(lambda (foo) 
                                   (get-field :parse-id foo)) data))
         (parse-id (if parse-ids (apply #'max parse-ids) 0)))
    (when verbose
      (format
       *tsdb-io* 
       "~&largest-parse-id(): largest `parse-id' (for `run' ~d) is ~a.~%"
       run-id parse-id))
    parse-id))

(defun merge-with-output-specifications (items language
                                         &key (verbose t))
  (if *tsdb-ignore-output-p*
    (map 'list
      #'(lambda (foo)
          (append foo (list -1 -1 *tsdb-maximal-number-of-tasks*)))
      items)
    (let (outputs)
      (do* ((length (length items) (length items))
            (current (butlast items (- length 100))
                     (butlast items (- length 100)))
            (items (nthcdr 100 items) (nthcdr 100 items)))
          ((null current))
        (let (query)
          (dolist (item current)
            (push (format nil "i-id = ~d" (first item)) query))
          (let* ((query (reduce #'(lambda (foo bar)
                                    (concatenate 'string foo " | " bar))
                                query))
                 (query (concatenate 'string
                          "select i-id o-ignore o-wf o-gc o-tasks "
                          "from output where "
                          query
                          " report \"(%d \\\"%s\\\" %d %d %d)\""))
                 (result (call-tsdb query language)))
            (with-input-from-string (stream result)
              (do ((line (read stream nil) (read stream nil)))
                  ((null line))
                (push line outputs))))))
      (when verbose
        (format
         *tsdb-io*
         "~&merge-with-output-specifications(): ~
          found ~a output specification~:p.~%" (length outputs)))
      (dolist (item items items)
        (let ((output (find (item-i-id item) outputs :key #'first)))
          (if output
            (setf (rest (last item)) (rest output))
            (setf (rest (last item))
              (list "" -1 -1 *tsdb-maximal-number-of-tasks*))))))))

(defun select-o-derivations (language &key item)
  (let* ((condition (when item (format nil "i-id = ~d" item)))
         (derivations 
          (select "o-derivation" :string "output" condition language))
         (derivations
          (and derivations
               (map 'list #'(lambda (tuple) (get-field :o-derivation tuple))
                    derivations))))
    (and derivations
         (map 'list (lambda (string) (read-from-string string nil))
              derivations))))

(defun select-derivations (language &key item)
  (let* ((condition (when item (format nil "i-id = ~d" item)))
         (derivations 
          (select "derivation" :string "result" condition language))
         (derivations
          (and derivations
               (map 'list #'(lambda (tuple) (get-field :derivation tuple))
                    derivations))))
    (and derivations
         (map 'list (lambda (string) (read-from-string string nil))
              derivations))))

#+junk
(defun parse-item (string &key (tasks 0) (derivations nil)
                               (trace nil) (gc *tsdb-gc-p*))
  (let* ((foo (open "/dev/null" :direction :output :if-exists :overwrite))
         (string (remove-and-insert-punctuation string))
         (exhaustive main::*exhaustive*)
         (lexicon-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon)))
         (syntax-task-priority-fn 
          (pg::combo-parser-task-priority-fn (pg::get-parser :syntax)))
         tcpu tgc treal conses symbols others)
    (setf main::*exhaustive* *tsdb-exhaustive-p*)
    (setf pg::*maximal-number-of-tasks*
      (if (and (integerp tasks) (> tasks 0)) tasks nil))
    (setf pg::*current-number-of-tasks* 0)
    (setf pg::*maximal-number-of-tasks-exceeded-p* nil)
    (setf (pg::ebl-parser-external-signal-fn (pg::get-parser :syntax))
      (if (> tasks 0)
        #'pg::maximal-number-of-tasks-exceeded-p
        #'(lambda (parser) (declare (ignore parser)))))
    (setf pg::*edge-id-counter* 0)
    (when (and derivations *tsdb-lexical-oracle-p*)
      (install-lexical-oracle derivations))
    (when (and derivations *tsdb-phrasal-oracle-p*)
      (install-phrasal-oracle derivations))
    (udine::reset-costs)
    (setf (main::output-stream main::*lexicon*) nil)
    (setf (main::output-stream main::*parser*) nil)
    (multiple-value-bind (result condition)
        (excl::time-a-funcall #'(lambda ()
                                  (run-protocol *tsdb-parser-protocol* 
                                                string :trace trace))
                              #'(lambda (tgcu tgcs tu ts tr
                                         scons ssym sother 
                                         #+:allegro-v5.0 foo)
                                  #+:allegro-v5.0
                                  (declare (ignore foo))
                                  (setf tgc (+ tgcu tgcs)
                                        tcpu (+ tu ts)
                                        treal tr
                                        conses scons
                                        symbols ssym
                                        others sother)))
      (close foo)
      (setf main::*exhaustive* exhaustive)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :lexicon))
        lexicon-task-priority-fn)
      (setf (pg::combo-parser-task-priority-fn (pg::get-parser :syntax))
        syntax-task-priority-fn)
      (append 
       (pairlis (list :tgc :tcpu :treal :conses :symbols :others)
                (list tgc tcpu treal conses symbols others))
       (cond
        (condition
         (pairlis
          (list :readings :condition :error)
          (list -1 condition (format nil "~a" condition))))
        ((or (eq (first result) :INCOMPLETE-INPUT)
             (null (main::output-stream main::*lexicon*)))
         (pairlis
          (list :readings :error)
          (list -1 "null parser input")))
        (t
         (let* ((items (main::output-stream main::*parser*))
                (readings (length items))
                (words (length (main::output-stream main::*lexicon*)))
                (statistics 
                 (pg::parser-stats-readings (pg::get-parser :syntax)))
                (first (when statistics (first (last statistics))))
                (first (when first (pg::stats-time first)))
                (first (when first (/ first internal-time-units-per-second)))
                (global (pg::parser-global-stats (pg::get-parser :syntax)))
                (total (when global (pg::stats-time global)))
                (total (when total (/ total internal-time-units-per-second)))
                (etasks (when global (pg::stats-executed global)))
                (stasks (when global (pg::stats-successful global)))
                (ftasks (when global (pg::stats-filtered global)))
                (edges (pg::total-number-of-items (pg::get-parser :syntax)))
                results)
           (when (= readings (length statistics))
             (do* ((i 0 (+ i 1))
                   (statistics statistics (rest statistics))
                   (statistic (first statistics) (first statistics))
                   (items items (rest items))
                   (item (first items) (first items)))
                 ((null statistics))
               (let* ((item (typecase item
                              (cons (seventh item))
                              (main::typed-item 
                               (second (main::typed-item-args item)))
                              (t item)))
                      (time (pg::stats-time statistic))
                      (time (when time 
                              (/ time internal-time-units-per-second)))
                      (etasks (pg::stats-executed statistic))
                      (stasks (pg::stats-successful statistic))
                      (ftasks (pg::stats-filtered statistic))
                      (derivation 
                       (format 
                        nil 
                        "~s"
                        (pg::item-to-node item (pg::get-parser :syntax)))))
                 (push (pairlis
                        (list :result-id :time
                              :r-etasks :r-stasks :r-ftasks
                              :derivation)
                        (list i time
                              etasks stasks ftasks
                              derivation))
                       results))))
           (pairlis
            (list :readings :first :total
                  :p-etasks :p-stasks :p-ftasks
                  :words :edges :results)
            (list readings first total
                  etasks stasks ftasks
                  words edges results)))))))))

(defun write-run (result language
                  &key cache)

  (when *tsdb-write-run-p*
    (let* ((*print-circle* nil)
           (run-id (get-field :run-id result))
           (comment (normalize-string (get-field :comment result)))
           (platform (normalize-string (get-field :platform result)))
           (application (normalize-string (get-field :application result)))
           (grammar (normalize-string (get-field :grammar result)))
           (avms (or (get-field :avms result) -1))
           (sorts (or (get-field :sorts result) -1))
           (templates (or (get-field :templates result) -1))
           (lexicon (or (get-field :lexicon result) -1))
           (lrules (or (get-field :lrules result) -1))
           (rules (or (get-field :rules result) -1))
           (user (or (normalize-string (get-field :user result)) ""))
           (host (or (normalize-string (get-field :host result)) ""))
           (os (or (normalize-string (get-field :os result)) ""))
           (start (get-field :start result))
           (query
            (format
             nil
             "insert into run values ~
              ~d ~s ~
              ~s ~s ~s ~
              ~d ~d ~d ~d ~d ~d ~
              ~s ~s ~s ~a"
             run-id comment 
             platform application grammar
             avms sorts templates lexicon lrules rules
             user host os start)))
      (call-tsdb query language :cache cache))))

(defun write-parse (result language
                    &key cache)
  (when *tsdb-write-parse-p*
    (let* ((*print-circle* nil)
           (parse-id (get-field :parse-id result))
           (run-id (get-field :run-id result))
           (i-id (get-field :i-id result))
           (readings (or (get-field :readings result) -1))
           (first (get-field :first result))
           (first (if first (round first) -1))
           (total (get-field :total result))
           (total (if total (round total) -1))
           (tcpu (get-field :tcpu result))
           (tcpu (if tcpu (round tcpu) -1))
           (tgc (get-field :tgc result))
           (tgc (if tgc (round tgc) -1))
           (treal (get-field :treal result))
           (treal (if treal (round treal) -1))
           (words (or (get-field :words result) -1))
           (l-stasks (or (get-field :l-stasks result) -1))
           (p-ctasks (or (get-field :p-ctasks result) -1))
           (p-ftasks (or (get-field :p-ftasks result) -1))
           (p-etasks (or (get-field :p-etasks result) -1))
           (p-stasks (or (get-field :p-stasks result) -1))
           (aedges (or (get-field :aedges result) -1))
           (pedges (or (get-field :pedges result) -1))
           (redges (or (get-field :redges result) -1))
           (unifications (or (get-field :unifications result) -1))
           (copies (or (get-field :copies result) -1))
           (conses (or (get-field :conses result) -1))
           (symbols (or (get-field :symbols result) -1))
           (others (or (get-field :others result) -1))
           (gcs (or (get-field :gcs result) -1))
           (i-load (get-field :i-load result))
           (i-load (if i-load (round (* 100 i-load)) -1))
           (a-load (get-field :a-load result))
           (a-load (if a-load (round (* 100 a-load)) -1))
           (date (current-time :long t))
           (error (normalize-string (or (get-field :error result) "")))
           (query "insert into parse values")
           (query
            (format
             nil
             "~a ~d ~d ~d ~
              ~d ~d ~d ~d ~d ~d ~
              ~d ~d ~
              ~d ~d ~d ~d ~
              ~d ~d ~d ~
              ~d ~d ~
              ~d ~d ~d ~
              ~d ~d ~d ~
              ~a ~s"
             query
             parse-id run-id i-id
             readings first total tcpu tgc treal
             words l-stasks
             p-ctasks p-ftasks p-etasks p-stasks
             aedges pedges redges
             unifications copies
             conses symbols others
             gcs i-load a-load
             date error)))
      (call-tsdb query language :cache cache))))

(defun write-results (parse-id results
                      &optional (language *tsdb-data*)
                      &key cache)
  (when *tsdb-write-result-p*
    (dolist (result results)
      (let* ((*print-circle* nil)
             (result-id (get-field :result-id result))
             (time (round (* 10 (or (get-field :time result) -1))))
             (r-ctasks (or (get-field :r-ctasks result) -1))
             (r-ftasks (or (get-field :r-ftasks result) -1))
             (r-etasks (or (get-field :r-etasks result) -1))
             (r-stasks (or (get-field :r-stasks result) -1))
             (size (or (get-field :size result) -1))
             (r-edges (or (get-field :r-edges result) -1))
             (derivation
              (or (normalize-string (get-field :derivation result)) ""))
             (tree (or (normalize-string (get-field :tree result)) ""))
             (mrs (or (normalize-string (get-field :mrs result)) ""))
             (query "insert into result values")
             (query
              (format
               nil
               "~a ~d ~d ~d ~d ~d ~d ~d ~d ~d ~s ~s ~s"
               query
               parse-id result-id
               time
               r-ctasks r-etasks r-stasks r-ftasks
               size r-edges
               (normalize-string derivation)
               (normalize-string (or tree "")) 
               (normalize-string (or mrs "")))))
        (call-tsdb query language :cache cache)))))

(defun write-output (i-id application 
                     tree mrs tasks 
                     user date
                     language
                     &key cache)
  (when *tsdb-write-output-p*
    (let* ((*print-circle* nil)
           (tree (shell-escape-quotes (remove #\@ (normalize-string tree))))
           (mrs (shell-escape-quotes (remove #\@ (normalize-string mrs))))
           (query
            (format
             nil
             "insert into output values ~a ~d ~s ~s ~s ~d ~s ~a"
             i-id application
             tree mrs tasks
             user date)))
      (call-tsdb query language :cache cache)))) 
