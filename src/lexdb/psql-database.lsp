(in-package :lkb)

;;;
;;; --- psql-database methods
;;;

(defmethod connect ((lexicon psql-database)) 
  (do ((conn (connect-aux lexicon)
	     (connect-aux lexicon)))
      (conn t)
    (let ((user
	   (ask-user-for-x 
	    "PostgreSQL login" 
	    (cons 
	     (format nil "~a~%Username?"
		     (pg:error-message (connection lexicon))) 
	     (or (user lexicon) 
		 "guest")))))
      (if user
	  (setf (user lexicon) user)
	(return)))))
    
(defmethod connect-aux ((lexicon psql-database))
  (with-slots (port host dbname password connection user) lexicon
    ;; attempt connection w/ default pwd
    (setf password user)
    (or
     (connect-aux2 lexicon)
     (and
      (setf password 
	(ask-user-for-x 
	 "PostgreSQL login" 
	 (cons 
	  (format nil "Password for ~a?" 
		  user) 
	  user)))
      (connect-aux2 lexicon)))))
      
(defmethod connect-aux2 ((lexicon psql-database))
  (with-slots (port host dbname password user connection) lexicon
    (setf connection 
      (pg:connect-db-with-handler 
       (concatenate 'string 
	 (and port 
	      (format nil "port='~a' " (sql-escape-string port)))
	 ;;; if postgres running locally w/o TCPIP
	 ;;; then set host = nil
	 (and host
	      (format nil "host='~a' " (sql-escape-string host)))
	 (format nil "dbname='~a' " (sql-escape-string dbname))
	 (format nil "user='~a' " (sql-escape-string user))
	 (and password 
	      (format nil "password='~a'" (sql-escape-string password))))))
    (let ((status 
	   (pg:decode-connection-status 
	    (pg:status connection))))
      (when (eq :connection-ok status)
	  (setf (server-version lexicon) 
	    (get-server-version lexicon))
	  (setf (lexdb-version lexicon) 
	    (get-db-version lexicon))	
	))))

(defmethod disconnect ((lexicon psql-database))
  (with-slots (connection) lexicon
  ;:close connection cleanly
    (when connection 
      (pg:finish connection)
      (setf connection nil))))

(defmethod reconnect ((lexicon psql-database))
  (disconnect lexicon)
  (connect lexicon))

(defmethod run-command-stdin ((database psql-database) command filename)
  (with-slots (connection) database
    (unless connection
      (error "Database ~s has no active connection." database))
    (pg::stdin-command-file connection command filename)))

(defmethod run-command ((database psql-database) command)
  (run-query database 
	     (make-instance 'sql-query :sql-string command)))

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (multiple-value-bind (recs cols)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs
            (columns query) (mapcar #'str-2-keyword cols)))
    query))

(defmethod clear-scratch ((lexicon psql-database))
  (fn-get-records lexicon ''clear-scratch))

(defmethod close-lex ((lexicon psql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (disconnect lexicon))

(defmethod true-port ((lexicon psql-database))
  (let* ((port (or
		(port lexicon)
		(car (excl.osi::command-output "echo $PGPORT")))))
    (if (equal port "")
	5432
      port)))
 
(defmethod open-lex ((lexicon psql-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (lexdb-version server-version dbname host user connection) lexicon
    (close-lex lexicon)    
    (format t "~%Connecting to lexical database ~a@~a:~a" 
	    dbname
	    host
	    (true-port lexicon))
    (force-output)
    (setf *postgres-tmp-lexicon* lexicon)
    (cond
     ((connect lexicon)
      (format t "~%Connected as user ~a" user)
      (format t "~%Opening ~a" dbname)
      (unless (string>= server-version "7.3")
	(error *trace-output* 
	       "PostgreSQL server version is ~a. Please upgrade to version 7.4 or above." 
	       server-version))
      (cond
       ((not (stringp lexdb-version))
	(error "Unable to determine LexDB version"))
       ((string> (compat-version lexdb-version)
		 *psql-lexdb-compat-version*)
	(error "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax). Try obtaining a more recent LKB binary." lexdb-version *psql-lexdb-compat-version*))
       ((string< (compat-version lexdb-version)
		 *psql-lexdb-compat-version*)
       (error "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax).
 You must load updated setup files.
 See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html" lexdb-version *psql-lexdb-compat-version*)))
      (make-field-map-slot lexicon)
      (retrieve-fn-defns lexicon)
      (initialize-userschema lexicon)
      (setf (name lexicon) name)
      lexicon)
     (t
      (format t "~%unable to connect to ~s:~%  ~a" 
	      (pg:db connection) 
	      (pg:error-message connection))
      nil))))

(defmethod initialize-lex ((lexicon psql-database) &key semi)
  (when (open-lex lexicon)
    (build-lex lexicon :semi semi)))
  
(defmethod vacuum-current-grammar ((lexicon psql-database) &key verbose)
  (let ((command
	 (if verbose
	     "vacuum full analyze verbose current_grammar"
	   "vacuum full analyze current_grammar")))
    (format t "~%Please wait: vacuuming private table")
    (force-output)
    (run-command lexicon command)
    (lkb-beep)))

(defmethod vacuum-public-revision ((lexicon psql-database) &key verbose)
  (with-slots (dbname host port) lexicon
    (let ((l2 (make-instance 'psql-database
		:dbname dbname
		:host host
		:port port
		:user (raw-get-val lexicon "SELECT db_owner()")))
	  (command
	   (if verbose
	       "vacuum full analyze verbose public.revision"    
	     "vacuum full analyze public.revision")))
      (format t "~%Please wait: vacuuming public table")
      (force-output)
      (connect l2)
      (run-command l2 command))))

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-database))
  (let* 
      ((sql-str "SELECT version();")
       (version-str (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    (second (split-on-char version-str))))
    
(defmethod get-db-version ((lexicon psql-database))
  (let* 
      ((sql-str "SELECT val FROM public.meta WHERE var='db-version' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod get-filter ((lexicon psql-database))
  (let* 
      ((sql-str "SELECT val FROM meta WHERE var='filter' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod next-version (id (lexicon psql-database))
  (let* (
	 (sql-str (sql-next-version lexicon (string-downcase id)))
	 (res (caar (records (run-query 
			      lexicon 
			      (make-instance 'sql-query :sql-string sql-str))))))
    (str-2-num res 0)))

(defmethod get-records ((lexicon psql-database) sql-string)
  (make-column-map-record
   (get-raw-records lexicon sql-string)))

(defmethod get-raw-records ((lexicon psql-database) sql-string)
   (run-query 
    lexicon 
    (make-instance 'sql-query :sql-string sql-string)))

(defmethod fn-get-records ((lexicon psql-database) fn-name &rest rest)
  (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))

(defmethod fn-get-raw-records ((lexicon psql-database) fn-name &rest rest)
  (get-raw-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))


(defmethod fn-get-record ((lexicon psql-database) fn-name &rest rest)
  (let ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest)))))
    (if (> (length res) 1)
        (error "too many records returned")
      (first res))))
  
(defmethod fn-get-val ((lexicon psql-database) fn-name &rest rest)
  (let* ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))
         (rec (first res)))
    (if (> (length res) 1)
        (error "too many records returned")
      (if (> (length rec) 1)
          (error "multiple columns returned")
        (cdar rec)))))
  
(defmethod raw-get-val ((lexicon psql-database) sql-str)
  (let* ((res (get-records lexicon sql-str))
         (rec (first res)))
    (if (> (length res) 1)
        (error "too many records returned")
      (if (> (length rec) 1)
          (error "multiple columns returned")
        (cdar rec)))))
  
  
(defmethod fn ((lexicon psql-database) fn-name &rest rest)
  (unless (connection lexicon)
    (error "no connection to psql lexicon"))
  (let ((lex-fn (assoc fn-name (fns lexicon))))
    (if lex-fn
	(eval (append (list (cdr lex-fn)) rest))
      (error "Embedded-SQL fn ~a not defined. Is the latest embedded-code.sql loaded into the LexDB?" fn-name))))
  
(defmethod sql-next-version ((lexicon psql-database) id)
  (fn lexicon 'next-version id))

(defmethod sql-orthography-set ((lexicon psql-database))
  (fn lexicon 'orthography-set))

(defmethod sql-lex-id-set ((lexicon psql-database))
  (fn lexicon 'lex-id-set))

(defmethod sql-lookup-word ((lexicon psql-database) word)
  (fn lexicon 'lookup-word (string-downcase word)))

(defmethod sql-retrieve-entries-by-orthkey ((lexicon psql-database) select-list word)
  (fn lexicon 'retrieve-entries-by-orthkey select-list (string-downcase word)))

(defmethod sql-retrieve-entry ((lexicon psql-database) select-list word)
  (fn lexicon 'retrieve-entry select-list word))

(defmethod sql-retrieve-all-entries ((lexicon psql-database) select-list)
  (fn lexicon 'retrieve-all-entries select-list))

(defmethod build-lex ((lexicon psql-database) &key (semi t))
  (build-lex-aux lexicon)
  (if semi
      (cond
       ((semi-up-to-date-p lexicon)
	(format t "~%(loading SEM-I into memory)")
	(unless (mrs::semi-p 
		 (catch 'pg::sql-error
		   (mrs::populate-*semi*-from-psql)))
	  (format t "~% (unable to retrieve database SEM-I)"))
	(index-lexical-rules)
	(index-grammar-rules))
       (t
	(format t "~%WARNING: no lexical entries indexed for generator"))))
  lexicon)

(defmethod build-lex-aux ((lexicon psql-database))
  (reconnect lexicon) ;; work around server bug
  (cond 
   ((not (user-read-only-p lexicon))
    (fn-get-records lexicon ''initialize-current-grammar (get-filter lexicon)))
   (t
    (format t "~%(user ~a has read-only privileges)" (user lexicon))))    
  (format t "~%(LexDB filter: ~a )" (get-filter lexicon))
  (let ((size (fn-get-val lexicon ''size-current-grammar)))
    (if (string= "0" size)
	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
      (format t "~%(active lexical entries: ~a )" size)))
  (empty-cache lexicon))
  
(defmethod user-read-only-p ((lexicon psql-database))
  (or (string= "t" (fn-get-val lexicon ''user-read-only-p (user lexicon)))
      (string= "T" (fn-get-val lexicon ''user-read-only-p (user lexicon)))))

(defmethod dump-db ((lexicon psql-database))  
    (fn-get-val lexicon ''dump-db))

(defmethod dump-scratch-db ((lexicon psql-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''dump-scratch-db filename))

(defmethod show-scratch ((lexicon psql-database))
  (fn-get-records lexicon ''show-scratch))

(defmethod merge-into-db ((lexicon psql-database) 
			  rev-filename)  
  (run-command-stdin lexicon 
		     (format nil "~a;~%~a;" 
			     "DELETE FROM temp" 
			     "COPY temp FROM stdin DELIMITERS ',' WITH NULL AS ''") 
		     rev-filename)
  (let ((count-new
	 (str-2-num
	  (fn-get-val lexicon ''merge-into-db2)))) 
    (format t "~%(~a new entries)" count-new)
    (unless (equal 0 count-new)
      (vacuum-public-revision lexicon))
    count-new))

(defmethod merge-defn ((lexicon psql-database) 
		       dfn-filename)  
  (when (catch 'pg:sql-error 
	  (run-command lexicon "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL;"))
    (run-command lexicon "DROP TABLE temp_defn")
    (run-command lexicon "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL ;"))
	
  (run-command-stdin lexicon 
		     "COPY temp_defn FROM stdin" 
		     dfn-filename)
  (let ((count-new-dfn 
	 (str-2-num (fn-get-val lexicon ''merge-defn))))
    (run-command lexicon "DROP TABLE temp_defn")
    (format t "~%(~a new field mappings)" count-new-dfn)
    count-new-dfn))

(defmethod initialize-userschema ((lexicon psql-database))
  (unless
      (fn-get-val lexicon ''test-user (user lexicon))
    (format t "~%(creating private space for user ~a)" (user lexicon))
    (fn-get-val lexicon ''create-schema (user lexicon))
    (if *postgres-mwe-enable*
	(mwe-initialize-userschema lexicon))))

(defmethod retrieve-fn-defns ((lexicon psql-database))
  (let* ((sql-str (format nil "SELECT * FROM qry;"))
	 (records (make-column-map-record (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str)))))
    (loop
      for record in records
	do
	  (retrieve-fn-defn lexicon record))))

(defmethod retrieve-fn-defn ((lexicon psql-database) record)
  (let* ((fn (get-val :fn record))
	 (arity (str-2-num (get-val :arity record)))
	 (sql-code (get-val :sql_code record))
	 (sql-str (format nil "SELECT * FROM qrya WHERE fn='~a';" fn))
	 (ergqa-records 
	  (make-column-map-record 
	   (run-query 
	    lexicon 
	    (make-instance 'sql-query :sql-string sql-str))))
	 (type-list 
	  (mapcar #'(lambda (record) 
		      (cons 
		       (str-2-num (get-val :arg record))
		       (str-2-symb (get-val :type record))))
		  ergqa-records)))
    (unless (= arity (length type-list))
      (error "wrong number of argument defns for embedded SQL fn ~a in lexical database ~a" fn (dbname lexicon)))
    (push (cons (str-2-symb fn) 
		(make-db-access-fn fn sql-code type-list))
	  (fns lexicon))))

(defmethod update-pgpass-file ((lexicon psql-database))
  (let ((entry
	 (format nil "~a:~a:~a:~a:~a"
		 (or (host lexicon)
		     "local")
		 (port lexicon)
		 (dbname lexicon)
		 (user lexicon)
		 (password lexicon)))
	(pgpass-entries (read-pgpass)))
    (unless (member entry
		    pgpass-entries
		    :test 'string=)
      (format t "(updating ~~/.pgpass)")
      (with-open-file 
	  (fstream
	   "~/.pgpass"
	   :direction :output
	   :if-exists :supersede 
	   :if-does-not-exist :create)
	(format fstream "~a~%" entry)
	(loop
	    for line in pgpass-entries
	    do
	      (format fstream "~a~%" line))))))

(defmethod semi-setup-1 ((lexicon psql-database))  
  (fn-get-records lexicon ''semi-setup-1))

(defmethod semi-setup-2 ((lexicon psql-database))  
  (fn-get-records lexicon ''semi-setup-2))

(defmethod semi-up-to-date-p ((lexicon psql-database))  
  (string= "t" (fn-get-val lexicon ''semi-up-to-date-p)))

(defmethod current-timestamp ((lexicon psql-database))  
  (cdaar (get-records *lexicon* "select current_timestamp")))

(defmethod semi-out-of-date ((lexicon psql-database))  
  (mapcar #'(lambda (x) (2-symb (car x))) 
	  (records 
	   (fn-get-raw-records lexicon ''semi-out-of-date))))
