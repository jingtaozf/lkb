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
        (when (and output (probe-file output)) (delete-file output))
        (with-open-file (stream file :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
          (format stream "~a~%" query))
        (multiple-value-bind (stream foo pid)
          (run-process
            command :wait nil
            :output (if output "/dev/null" :stream)
            :if-output-exists :append
            :input "/dev/null" :error-output nil)
          (declare (ignore foo #-:allegro pid))
          (if output
            ;;
            ;; we are in a rather intricate situation here: possibly the child
            ;; has not yet been created (os-wait() returns nil nil) or is still
            ;; running (os-wait() returns nil .pid.); then, in turn, something
            ;; may have gone wrong in the process creation; in this case we
            ;; better not block eternally.  without os-specific functions (like
            ;; os-wait in Allegro CL) there is no way to handle this situation.
            ;;
            #+:allegro
            (multiple-value-bind (status epid) 
                (sys:os-wait t pid)
              (when (null status)
                (loop
                    with i = 0
                    do
                      (sleep 0.1)
                      (multiple-value-setq (status epid) (sys:os-wait t pid))
                      (when (null epid) (incf i))
                    while (and (< i 42) (not status))))
              (when (probe-file output)
                (unless *tsdb-debug-mode-p*
                  (delete-file file))
                output))
            #-:allegro
            (error "call-tsdb(): cannot process :output redirection; ~
                    see `tsdb.lisp'.")
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
              (unless *tsdb-debug-mode-p*
                (delete-file file))
              result)))))))

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
    (when verbose
      (format 
       *tsdb-io*
       "~&flush-cache(): flushing `~a' cache ..."
       database)
      (force-output *tsdb-io*))
    (format stream "~&commit.~%")
    (force-output stream)
    (close stream)
    (let* ((query (format nil "do \"~a\"" file)))
      (call-tsdb query database))
    (when verbose
      (format *tsdb-io* " done.~%")
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
          (append foo (list -1 -1 -1)))
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
                          "select i-id o-ignore o-wf o-gc o-edges "
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
              (list "" -1 -1 -1))))))))

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

(defun read-database-schema (data &key absolute)
  
  (let* ((relations (call-tsdb "info relations" data :ro t :absolute absolute))
         schema)
    (with-input-from-string (stream relations)
      (do* ((line (read-line stream nil nil) (read-line stream nil nil)))
          ((null line) schema)
        (let* ((line (string-trim '(#\Space #\Tab) line))
               (colon (position #\: line)))
          (unless (or (string= line "") (null colon))
            (let ((relation (subseq line 0 colon))
                  fields)
              (do* ((line (read-line stream nil nil) 
                          (read-line stream nil nil)))
                  ((or (null line) 
                       (string= (string-trim '(#\Space #\Tab #\Newline) line) 
                                "")))
                (let* ((line (string-trim '(#\Space #\Tab) line))
                       (space (or (position #\Space line) (length line))))
                  (unless (or (string= line "") (null space))
                    (let* ((name (subseq line 0 space))
                           (flags (subseq line space))
                           (type (cond 
                                  ((search "integer" flags) :integer)
                                  ((search "string" flags) :string)
                                  ((search "date" flags) :date)
                                  ((search "position" flags) :position)))
                           (field 
                            (append (list name type) 
                                    (and (search "key" flags) '(:key))
                                    (and (search "unique" flags) '(:unique)))))
                      (push field fields)))))
              (push (cons relation (nreverse fields)) schema))))))))
                      

(defun write-run (result language
                  &key cache)

  (when *tsdb-write-run-p*
    (let* ((*print-circle* nil)
           (run-id (get-field :run-id result))
           (comment (normalize-string (get-field :comment result)))
           (platform (normalize-string (get-field :platform result)))
           (tsdb (normalize-string (get-field :tsdb result)))
           (application (normalize-string (get-field :application result)))
           (grammar (normalize-string (get-field :grammar result)))
           (avms (get-field+ :avms result -1))
           (sorts (get-field+ :sorts result -1))
           (templates (get-field+ :templates result -1))
           (lexicon (get-field+ :lexicon result -1))
           (lrules (get-field+ :lrules result -1))
           (rules (get-field+ :rules result -1))
           (user (normalize-string (get-field+ :user result "")))
           (host (normalize-string (get-field+ :host result "")))
           (os (normalize-string (get-field+ :os result "")))
           (start (get-field :start result))
           (query
            (format
             nil
             "insert into run values ~
              ~d ~s ~
              ~s ~s ~s ~s ~
              ~d ~d ~d ~d ~d ~d ~
              ~s ~s ~s ~a"
             run-id comment 
             platform tsdb application grammar
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
           (readings (get-field+ :readings result -1))
           (first (round (get-field+ :first result -1)))
           (total (round (get-field+ :total result -1)))
           (tcpu (round (get-field+ :tcpu result -1)))
           (tgc (round (get-field+ :tgc result -1)))
           (treal (round (get-field+ :treal result -1)))
           (words (get-field+ :words result -1))
           (l-stasks (get-field+ :l-stasks result -1))
           (p-ctasks (get-field+ :p-ctasks result -1))
           (p-ftasks (get-field+ :p-ftasks result -1))
           (p-etasks (get-field+ :p-etasks result -1))
           (p-stasks (get-field+ :p-stasks result -1))
           (aedges (get-field+ :aedges result -1))
           (pedges (get-field+ :pedges result -1))
           (raedges (get-field+ :raedges result -1))
           (rpedges (get-field+ :rpedges result -1))
           (unifications (get-field+ :unifications result -1))
           (copies (get-field+ :copies result -1))
           (conses (get-field+ :conses result -1))
           (symbols (get-field+ :symbols result -1))
           (others (get-field+ :others result -1))
           (gcs (get-field+ :gcs result -1))
           (i-load (get-field :i-load result))
           (i-load (if i-load (round (* 100 i-load)) -1))
           (a-load (get-field :a-load result))
           (a-load (if a-load (round (* 100 a-load)) -1))
           (date (current-time :long t))
           (error (normalize-string (get-field+ :error result "")))
           (query "insert into parse values")
           (query
            (format
             nil
             "~a ~d ~d ~d ~
              ~d ~d ~d ~d ~d ~d ~
              ~d ~d ~
              ~d ~d ~d ~d ~
              ~d ~d ~d ~d ~
              ~d ~d ~
              ~d ~d ~d ~
              ~d ~d ~d ~
              ~a ~s"
             query
             parse-id run-id i-id
             readings first total tcpu tgc treal
             words l-stasks
             p-ctasks p-ftasks p-etasks p-stasks
             aedges pedges raedges rpedges
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
             (time (round (get-field+ :time result -1)))
             (r-ctasks (get-field+ :r-ctasks result -1))
             (r-ftasks (get-field+ :r-ftasks result -1))
             (r-etasks (get-field+ :r-etasks result -1))
             (r-stasks (get-field+ :r-stasks result -1))
             (size (get-field+ :size result -1))
             (r-aedges (get-field+ :r-aedges result -1))
             (r-pedges (get-field+ :r-pedges result -1))
             (derivation (normalize-string (get-field+ :derivation result "")))
             (tree (normalize-string (get-field+ :tree result "")))
             (mrs (normalize-string (get-field+ :mrs result "")))
             (query "insert into result values")
             (query
              (format
               nil
               "~a ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ~s ~s ~s"
               query
               parse-id result-id
               time
               r-ctasks r-etasks r-stasks r-ftasks
               size r-aedges r-pedges
               derivation tree mrs)))
        (call-tsdb query language :cache cache)))))

(defun write-rules (parse-id statistics
                    &optional (language *tsdb-data*)
                    &key cache)
  (when *tsdb-rule-statistics-p*
    (loop 
        for rule in statistics
        for name = (normalize-string (get-field+ :rule rule ""))
        for filtered = (get-field+ :filtered rule -1)
        for executed = (get-field+ :executed rule -1)
        for successful = (get-field+ :successful rule -1)
        for query = (format
                     nil
                     "insert into rule values ~d ~s ~d ~d ~d"
                     parse-id name filtered executed successful)
        do (call-tsdb query language :cache cache))))

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
