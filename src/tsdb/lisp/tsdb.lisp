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
    (let* ((*tsdb-initialized-p* t)
           (tsdbrc (dir-and-name (user-homedir-pathname) ".tsdbrc"))
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
                                                :direction :input)
                                 (read stream nil nil))))
      (when (and (or (null action) (member action '(:cache :all))) cache)
        (load-cache :background background :name name :pattern pattern))))
  (unless action (setf *tsdb-initialized-p* t)))

(defun call-tsdb (query language
                  &key (format :string)
                       cache absolute unique quiet ro)
  (if *tsdb-server-mode-p*
    #+:server (call-tsdbd query language) #-:server nil
    (if cache
      (cache-query query language cache)
      (let* ((user (current-user))
             (file (format
                    nil "/tmp/.tsdb.io.~a.~a.~a"
                    user (current-pid) (string-downcase (string (gensym "")))))
             (data (if absolute 
                     (namestring language) 
                     (find-tsdb-directory language)))
             (command (format
                       nil 
                       "~a -home=~a -uniquely-project=~:[off~;on~]~
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
             (output (when (eq format :file)
                       (format
                        nil "/tmp/.tsdb.data.~a.~a.~a"
                        user (current-pid) 
                        (string-downcase (string (gensym ""))))))
             (redirection 
              (if output (concatenate 'string " > \"" output "\"") ""))
             (query (concatenate 'string query redirection ".")))
        (when (and output (probe-file output)) (delete-file output))
        (with-open-file (stream file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
          (format stream "~a~%" query)
          (force-output stream))
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
                      (sleep 0.5)
                      (multiple-value-setq (status epid) (sys:os-wait t pid))
                      (when (null epid) (incf i))
                    while (and (< i 100) (not status))
                    finally
                      (when (>= i 100)
                        (format
                         *tsdb-io*
                         "call-tsdb(): failed to salvage tsdb(1) child ~d~%"
                         pid))))
              (when (probe-file output)
                (unless *tsdb-debug-mode-p*
                  (delete-file file))
                output))
            #-:allegro
            (error "call-tsdb(): cannot process :file output format; ~
                    see `tsdb.lisp'.")
            (case format
              (:string
               (let ((result (make-array
                              32768
                              :element-type 'character
                              :adjustable t             
                              :fill-pointer 0)))
                 (loop
                     for c = (read-char stream nil :eof)
                     until (eq c :eof)
                     do
                       (vector-push-extend c result 32768))
                 (close stream)
                 #+:allegro (sys:os-wait nil pid)
                 (unless *tsdb-debug-mode-p*
                   (delete-file file))
                 result))
              (:lisp
               (let ((result (loop
                                 for form = (read stream nil :eof)
                                 until (eq form :eof)
                                 collect form)))
                 (close stream)
                 #+:allegro 
                 (loop until (sys:os-wait nil pid))
                 (unless *tsdb-debug-mode-p*
                   (delete-file file))
                 result)))))))))

(defun create-cache (data &key (protocol :cooked) (verbose t) schema)
  (if (eq protocol :raw)
    (loop 
        with cache = (pairlis '(:database :count :protocol)
                              (list data 0 protocol))
        with path = (find-tsdb-directory data)
        initially (when verbose
                    (format 
                     *tsdb-io*
                     "~&create-cache(): write-through mode for `~a'.~%"
                     data)
                    (force-output *tsdb-io*))
        for file in *tsdb-profile-files*
        for key = (intern (string-upcase file) :keyword)
        when (assoc file schema :test #'string=) do
          (let ((stream (open (make-pathname :directory path :name file) 
                              :direction :output 
                              :if-exists :append 
                              :if-does-not-exist :create)))
            (push (cons key stream) cache))
        finally (return cache))
    (let* ((user (current-user))
           (file (format
                  nil "/tmp/.tsdb.cache.~a.~a.~a"
                  user (current-pid) (string-downcase (string (gensym "")))))
           (stream (open file 
                         :direction :output 
                         :if-exists :supersede :if-does-not-exist :create)))
      (when stream
        (format stream "set implicit-commit \"exit\".~%")
        (when verbose
          (format 
           *tsdb-io*
           "~&create-cache(): tsdb(1) write cache in `~a'.~%"
           file)
          (force-output *tsdb-io*)))
      (pairlis '(:database :file :stream :count :protocol)
               (list data file stream 0 protocol)))))

(defun cache-query (query data cache)
  (let* ((database (get-field :database cache))
         (stream (get-field :stream cache))
         (query (string-trim '(#\Space #\Tab #\Newline) query))
         (query (if (equal (elt query (- (length query) 1)) #\.)
                  query
                  (concatenate 'string query "."))))
    (if (string-equal data database)
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
            (format stream "set implicit-commit \"exit\".~%")
            (setf (get-field :stream cache) stream))))
      (format
       *tsdb-io*
       "~&cache-query() ignoring query to `~a' (on `~a' cache).~%"
       data database))))

(defun flush-cache (cache
                    &key (verbose t))
  (let ((database (get-field :database cache))
        (protocol (get-field :protocol cache)))
    (when verbose
      (format 
       *tsdb-io*
       "~&flush-cache(): flushing `~a' cache ..."
       database)
      (force-output *tsdb-io*))
    (if (eq protocol :raw)
      (loop
          for file in *tsdb-profile-files*
          for key = (intern (string-upcase file) :keyword)
          for stream = (get-field key cache)
          when stream do
            (force-output stream)
            (close stream))
      (let ((file (get-field :file cache))
            (stream (get-field :stream cache)))
        (format stream "~&commit.~%")
        (force-output stream)
        (close stream)
        (let* ((query (format nil "do \"~a\"" file)))
          (call-tsdb query database))
        (unless *tsdb-debug-mode-p*
          (delete-file file))))
    (when verbose
      (format *tsdb-io* " done.~%")
      (force-output *tsdb-io*))))

(defun largest-result-key (&optional (data *tsdb-data*)
                           &key (verbose t))
  (let* ((query "select c-id from csli")
         (result (call-tsdb query data)))
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

(defun largest-i-id (&optional (data *tsdb-data*)
                     &key (verbose t))     
  (let* ((query "select i-id from item")
         (result (call-tsdb query data))
         (i-id 
          (with-input-from-string (stream result)
            (loop
                for i-id = (read stream nil :eof)
                until (equal i-id :eof)
                maximize i-id))))
    (when verbose
      (format *tsdb-io* "~&largest-i-id(): largest `i-id' is ~a.~%" i-id))
    i-id))

(defun largest-run-id (&optional (data *tsdb-data*)
                       &key (verbose t))     
  (let* ((query "select run-id from run")
         (result (call-tsdb query data)))
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

(defun select-phenomena (data &key (format :tcl) (stream *tsdb-io*))
  (declare (ignore format stream))
  (let* ((phenomena (select '("p-id" "p-name")
                            '(:integer :string)
                            "phenomenon"
                            nil data)))
    (sort phenomena #'string< 
          :key #'(lambda (tuple)
                   (get-field :p-name tuple)))))

(defun analyze-phenomena (phenomena &optional prefix)
  (loop
      with result
      for tuple = (pop phenomena)
      for name = (get-field :p-name tuple)
      for next = (get-field :p-name (first phenomena))
      for common = (and next (mismatch name next))
      do
        (cond
         ((and common (null prefix))
          (multiple-value-bind (analysis residuum)
              (analyze-phenomena phenomena common)
            (push (cons common analysis) result)
            (setf phenomena residuum)))
         ((and common prefix (< (mismatch common prefix) (length prefix)))
          )
         (t
          (push tuple result)))
      finally (return (nreverse result))))

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

(defun write-run (result language &key cache)

  (when *tsdb-write-run-p*
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (*print-escape* t)
           (rawp (and cache (eq (get-field :protocol cache) :raw)))
           (run-id (get-field+ :run-id result -1))
           (comment 
            (normalize-string (get-field :comment result) :escape rawp))
           (platform 
            (normalize-string (get-field :platform result) :escape rawp))
           (tsdb (normalize-string (get-field :tsdb result) :escape rawp))
           (application 
            (normalize-string (get-field :application result) :escape rawp))
           (context 
            (normalize-string (get-field :context result) :escape rawp))
           (grammar 
            (normalize-string (get-field :grammar result) :escape rawp))
           (avms (get-field+ :avms result -1))
           (sorts (get-field+ :sorts result -1))
           (templates (get-field+ :templates result -1))
           (lexicon (get-field+ :lexicon result -1))
           (lrules (get-field+ :lrules result -1))
           (rules (get-field+ :rules result -1))
           (user (normalize-string (get-field+ :user result "") :escape rawp))
           (host (normalize-string (get-field+ :host result "") :escape rawp))
           (os (normalize-string (get-field+ :os result "") :escape rawp))
           (start (get-field :start result))
           (end (get-field :end result))
           (items (get-field+ :items result -1))
           (status 
            (normalize-string (get-field+ :status result "") :escape rawp)))
      (if rawp
        (let ((stream (get-field :run cache))
              (ofs *tsdb-ofs*))
          (write run-id :stream stream) (write-char ofs stream)
          (write-string comment stream) (write-char ofs stream)
          (write-string platform stream) (write-char ofs stream)
          (write-string tsdb stream) (write-char ofs stream)
          (write-string application stream) (write-char ofs stream)
          (write-string context stream) (write-char ofs stream)
          (write-string grammar stream) (write-char ofs stream)
          (write avms :stream stream) (write-char ofs stream)
          (write sorts :stream stream) (write-char ofs stream)
          (write templates :stream stream) (write-char ofs stream)
          (write lexicon :stream stream) (write-char ofs stream)
          (write lrules :stream stream) (write-char ofs stream)
          (write rules :stream stream) (write-char ofs stream)
          (write-string user stream) (write-char ofs stream)
          (write-string host stream) (write-char ofs stream)
          (write-string os stream) (write-char ofs stream)
          (write-string start stream) (write-char ofs stream)
          (write-string end stream) (write-char ofs stream)
          (write items :stream stream) (write-char ofs stream)
          (write-string status stream) 
          (terpri stream)
          (force-output stream)
          (incf (get-field :count cache)))
        (let* ((query
                (format
                 nil
                 "insert into run values ~
                  ~d ~s ~
                  ~s ~s ~s ~s ~s ~
                  ~d ~d ~d ~d ~d ~d ~
                  ~s ~s ~s ~a ~a ~d ~s"
                 run-id comment 
                 platform tsdb application context grammar
                 avms sorts templates lexicon lrules rules
                 user host os start end items status)))
          (call-tsdb query language :cache cache))))))

(defun write-parse (result language &key cache)
  
  (when *tsdb-write-parse-p*
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (*print-escape* t)
           (rawp (and cache (eq (get-field :protocol cache) :raw)))
           (parse-id (get-field+ :parse-id result -1))
           (run-id (get-field+ :run-id result -1))
           (i-id (get-field+ :i-id result -1))
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
           (error 
            (normalize-string (get-field+ :error result "") :escape rawp))
           (comment 
            (normalize-string (get-field+ :comment result "") :escape rawp)))
      (if rawp
        (let ((stream (get-field :parse cache))
              (ofs *tsdb-ofs*))
          (write parse-id :stream stream) (write-char ofs stream)
          (write run-id :stream stream) (write-char ofs stream)
          (write i-id :stream stream) (write-char ofs stream)
          (write readings :stream stream) (write-char ofs stream)
          (write first :stream stream) (write-char ofs stream)
          (write total :stream stream) (write-char ofs stream)
          (write tcpu :stream stream) (write-char ofs stream)
          (write tgc :stream stream) (write-char ofs stream)
          (write treal :stream stream) (write-char ofs stream)
          (write words :stream stream) (write-char ofs stream)
          (write l-stasks :stream stream) (write-char ofs stream)
          (write p-ctasks :stream stream) (write-char ofs stream)
          (write p-ftasks :stream stream) (write-char ofs stream)
          (write p-etasks :stream stream) (write-char ofs stream)
          (write p-stasks :stream stream) (write-char ofs stream)
          (write aedges :stream stream) (write-char ofs stream)
          (write pedges :stream stream) (write-char ofs stream)
          (write raedges :stream stream) (write-char ofs stream)
          (write rpedges :stream stream) (write-char ofs stream)
          (write unifications :stream stream) (write-char ofs stream)
          (write copies :stream stream) (write-char ofs stream)
          (write conses :stream stream) (write-char ofs stream)
          (write symbols :stream stream) (write-char ofs stream)
          (write others :stream stream) (write-char ofs stream)
          (write gcs :stream stream) (write-char ofs stream)
          (write i-load :stream stream) (write-char ofs stream)
          (write a-load :stream stream) (write-char ofs stream)
          (write-string date stream) (write-char ofs stream)
          (write-string error stream) (write-char ofs stream)
          (write-string comment stream)
          (terpri stream)
          (force-output stream)
          (incf (get-field :count cache)))
        (let* ((query "insert into parse values")
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
                  ~a ~s ~s"
                 query
                 parse-id run-id i-id
                 readings first total tcpu tgc treal
                 words l-stasks
                 p-ctasks p-ftasks p-etasks p-stasks
                 aedges pedges raedges rpedges
                 unifications copies
                 conses symbols others
                 gcs i-load a-load
                 date error comment)))
          (call-tsdb query language :cache cache))))))

(defun write-results (parse-id results
                      &optional (language *tsdb-data*)
                      &key cache)
  (when *tsdb-write-result-p*
    (loop
        with *print-circle* = nil
        with *print-level* = nil
        with *print-length* = nil
        with rawp = (and cache (eq (get-field :protocol cache) :raw))
        for result in results
        for id from 0
        for result-id = (or (get-field :result-id result) id)
        for time = (round (get-field+ :time result -1))
        for r-ctasks = (get-field+ :r-ctasks result -1)
        for r-ftasks = (get-field+ :r-ftasks result -1)
        for r-etasks = (get-field+ :r-etasks result -1)
        for r-stasks = (get-field+ :r-stasks result -1)
        for size = (get-field+ :size result -1)
        for r-aedges =  (get-field+ :r-aedges result -1)
        for r-pedges = (get-field+ :r-pedges result -1)
        for derivation = (normalize-string 
                          (get-field :derivation result) :escape rawp)
        for tree = (normalize-string (get-field :tree result) :escape rawp)
        for mrs = (normalize-string (get-field :mrs result) :escape rawp)
        do
          (if rawp
            (let ((stream (get-field :result cache))
                  (ofs *tsdb-ofs*))
              (write parse-id :stream stream) (write-char ofs stream)
              (write result-id :stream stream) (write-char ofs stream)
              (write time :stream stream) (write-char ofs stream)
              (write r-ctasks :stream stream) (write-char ofs stream)
              (write r-ftasks :stream stream) (write-char ofs stream)
              (write r-etasks :stream stream) (write-char ofs stream)
              (write r-stasks :stream stream) (write-char ofs stream)
              (write size :stream stream) (write-char ofs stream)
              (write r-aedges :stream stream) (write-char ofs stream)
              (write r-pedges :stream stream) (write-char ofs stream)
              (write-string derivation stream) (write-char ofs stream)
              (write-string tree stream) (write-char ofs stream)
              (write-string mrs stream) 
              (terpri stream)
              (force-output stream)
              (incf (get-field :count cache)))
            (let* ((query (format
                           nil
                           "insert into result values ~
                            ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ~s ~s ~s"
                           parse-id result-id
                           time
                           r-ctasks r-ftasks r-etasks r-stasks
                           size r-aedges r-pedges
                           derivation tree mrs)))
              (call-tsdb query language :cache cache))))))

(defun write-rules (parse-id statistics
                    &optional (language *tsdb-data*)
                    &key cache)
  (when *tsdb-rule-statistics-p*
    (loop 
        with *print-circle* = nil
        with *print-level* = nil
        with *print-length* = nil
        with rawp = (and cache (eq (get-field :protocol cache) :raw))
        for rule in statistics
        for name = (normalize-string (get-field+ :rule rule "") :escape rawp)
        for filtered = (get-field+ :filtered rule -1)
        for executed = (get-field+ :executed rule -1)
        for successful = (get-field+ :successful rule -1)
        for actives = (get-field+ :actives rule -1)
        for passives = (get-field+ :passives rule -1)
        do
          (if rawp
            (let ((stream (get-field :rule cache))
                  (ofs *tsdb-ofs*))
              (write parse-id :stream stream) (write-char ofs stream)
              (write-string name stream) (write-char ofs stream)
              (write filtered :stream stream) (write-char ofs stream)
              (write executed :stream stream) (write-char ofs stream)
              (write successful :stream stream) (write-char ofs stream)
              (write actives :stream stream) (write-char ofs stream)
              (write passives :stream stream)
              (terpri stream)
              (force-output stream)
              (incf (get-field :count cache)))
            (let ((query (format
                          nil
                          "insert into rule values ~d ~s ~d ~d ~d ~d ~d"
                          parse-id name filtered executed successful
                          actives passives)))
              (call-tsdb query language :cache cache))))))

(defun write-output (i-id application 
                     tree mrs tasks 
                     user date
                     language
                     &key cache)
  (when *tsdb-write-output-p*
    (when (and cache (eq (get-field :protocol cache) :raw))
      (error "write-output(): unable to write to raw cache; see `tsdb.lisp'"))
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (rawp nil)
           (tree (normalize-string tree :escape rawp))
           (mrs (normalize-string mrs :escape rawp))
           (query
            (format
             nil
             "insert into output values ~a ~d ~s ~s ~s ~d ~s ~a"
             i-id application
             tree mrs tasks
             user date)))
      (call-tsdb query language :cache cache)))) 
