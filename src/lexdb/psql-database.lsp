;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;;
;;; --- psql-database methods
;;;

(defmacro with-lexdb-client-min-messages ((lexdb min-messages) &body body)
  `(let ((current-min-messages (caar (recs (get-records ,lexdb "show client_min_messages")))))
     (run-command ,lexdb (format nil "set client_min_messages to ~a" ,min-messages))
     ,@body
    (run-command ,lexdb (format nil "set client_min_messages to ~a" current-min-messages))))

#+:allegro
(defmacro with-lexdb-locale (&body body)
  `(let ((excl::*locale* *lexdb-locale*))
     ,@body))

#+:sbcl
(defmacro with-lexdb-locale (&body body)
  `(progn
     ,@body))
  
(defmethod copy-column-to-psql ((db psql-database) table list)
  (let ((conn (connection db))
	(table-str (quote-ident db table)))
    (run-command db (format nil "CREATE TABLE ~a (x text)" table-str))
    (pq:exec conn (format nil "COPY ~a FROM stdin" table-str))
    (loop
	for l in list
	do 
	  (with-lexdb-locale (putline conn (lkb::psql-copy-val l))))
    (with-lexdb-locale (putline conn "\\."))
    (endcopy conn)))

;; CONNECT

(defmethod connect ((lexicon psql-database)) 
  (disconnect lexicon)
  (do ((conn (connect-aux lexicon)
	     (connect-aux lexicon)))
      (conn t)
    (let ((user
	   (ask-user-for-x 
	    "PostgreSQL login" 
	    (cons 
	     (format nil "~a~%Username?"
		     (error-msg (connection lexicon))) 
	     (or (user lexicon) 
		 "guest")))))
      (if user
	  (setf (user lexicon) user)
	(return)))))
    
(defmethod connect-aux ((lexicon psql-database))
  (with-slots (port host dbname password connection user) lexicon
    ;; attempt connection w/ default pwd
    (if (or (null password)
	    (equal "" password))
	(setf password user))
    (or
     (connect-aux2 lexicon)
     (and
      (setf password 
	(ask-user-for-x 
	 "PostgreSQL login" 
	 (cons 
	  (format nil "Password for ~a?" 
		  user) 
	  password)))
      (connect-aux2 lexicon)))))
      
(defmethod connect-aux2 ((lexicon psql-database))
  (with-slots (port host dbname password user connection) lexicon
    (let ((connect-timeout *psql-database-connect-timeout*)) 
      (setf connection 
	(connect-db 
	 (concatenate 'string 
	   (and connect-timeout 
		(format nil "connect_timeout=~a " (psql-quote-literal (2-str connect-timeout))))
	   (and port 
		(format nil "port=~a " (psql-quote-literal (2-str port))))
	   (and host
		(format nil "host=~a " (psql-quote-literal host)))
	   (format nil "dbname=~a " (psql-quote-literal dbname))
	   (format nil "user=~a " (psql-quote-literal user))
	   (and password 
		(format nil "password=~a" (psql-quote-literal password)))
	   )))
      (when (connection-ok lexicon)
	(unless (check-libpq-protocol-version connection)
	  (disconnect lexicon))
	(pq:set-client-encoding connection "UNICODE")
	t))))

;; move code here from header file???
(defparameter *lexdb-libpq-protocol-supported* 3)
(defun check-libpq-protocol-version (connection)
  (let ((version (pq:protocol-version connection)))
    (cond
     ((= version *lexdb-libpq-protocol-supported*)
      t)
     (t
      (finish-db connection)
      (format t "~&(LexDB) unsupported libpq protocol (~a)" version)
      nil))))

(defmethod disconnect ((lexicon psql-database))
  (with-slots (connection) lexicon
  ;:close connection cleanly
    (when connection 
      (finish-db connection)
      (setf connection nil))))

(defmethod reconnect ((lexicon psql-database))
  (disconnect lexicon)
  (connect lexicon))

;;;
;;; execute db queries/commands
;;;

(defmethod get-records ((database psql-database) sql-string &key (ignore-errors nil))
  (with-slots (connection) database
    (unless connection
      (error "psql-database ~s has no active connection." database))
      (multiple-value-bind (recs cols)
	  (execute connection sql-string :tup :col :ignore-errors ignore-errors)
	(if (typep recs 'sql-error)
	    recs
	  (make-instance 'psql-database-table :recs recs :cols cols)))))

(defmethod get-raw-records ((database psql-database) sql-string)
  (with-slots (connection) database
    (unless connection
      (error "psql-database ~s has no active connection." database))
    (execute connection sql-string :tup t)))

(defmethod sql-get-val ((db psql-database) sql-string)
  (let ((recs (get-raw-records db sql-string)))
    (unless (and (= 1 (length recs))
		 (= 1 (length (car recs))))
      (error "single value expected from query: ~a~%  got: ~a" sql-string recs))
    (caar recs)))

(defmethod sql-get-bool ((db psql-database) sql-string)
  (let ((x (sql-get-val db sql-string)))
    (cond
     ((string= "t" x) t)
     ((string= "f" x) nil)
     (t (error "cannot decode SQL bool val")))))
  
(defmethod sql-get-num ((db psql-database) sql-string)
  (let ((x (sql-get-val db sql-string)))
    (str-2-num x)))

  ;; run command with stdin = filename
(defmethod run-command-stdin-from-file ((db psql-database) command filename)
  (with-open-file (istr filename
		   :direction :input)
    (run-command-stdin db command istr)))

(defmethod run-command-stdin ((db psql-database) command istrm)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (execute connection command :in (list istrm))))

(defmethod run-command-stdin-from-list ((db psql-database) table list)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (pq:exec connection (format nil "COPY ~a FROM stdin" (quote-ident db table)))
    (loop
	for row in list
	do 
	  (with-lexdb-locale (pq:putline connection (to-psql-copy-rec2 row))))
    (with-lexdb-locale (putline connection "\\."))
    (endcopy connection)))
    
(defmethod run-command-stdin-from-hash-vals ((db psql-database) table hash)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (pq:exec connection (format nil "COPY ~a FROM stdin" (quote-ident db table)))
    (loop
	for row being each hash-value in hash
	do 
	  (with-lexdb-locale (pq:putline connection (to-psql-copy-rec2 row))))
    (with-lexdb-locale (putline connection "\\."))
    (endcopy connection)))
    
(defmethod run-command-stdin-from-hash-val-rows ((db psql-database) table hash)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (pq:exec connection (format nil "COPY ~a FROM stdin" (quote-ident db table)))
    (loop
	for rows being each hash-value in hash
	do
	  (loop for row in rows
	      do 
		(with-lexdb-locale (pq:putline connection (to-psql-copy-rec2 row)))))
    (with-lexdb-locale (putline connection "\\."))
    (endcopy connection)))

(defvar *nrows* 2)
(defmethod run-command-stdin-from-hash-val-rows2 ((db psql-database) table hash)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (pq:exec connection (format nil "COPY ~a FROM stdin" (quote-ident db table)))
    (loop
	with s = (make-string-output-stream)
	with i = 1
	for rows being each hash-value in hash
	do
	  (loop for row in rows
	      do 
		(princ (to-psql-copy-rec2 row) s)
		(incf i)
	      unless (< i 2) 
	      do 
		(with-lexdb-locale (pq:putline connection (get-output-stream-string s)))
		(setf i 1)
		))
    (with-lexdb-locale (putline connection "\\."))
    (endcopy connection)))
    

#+:null
(defmethod put-normalized-lex-keys ((lex psql-lex-database) recs)
  (when recs
    (let ((conn (connection lex)))
					;    (run-command lex "DELETE FROM lex_key")
      (with-lexdb-client-min-messages (lex "error")
	(run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
      (pq:exec conn "COPY lex_key FROM stdin")
      (loop
	  for rec in recs
	  do 
	    (with-lexdb-locale (pq:putline conn (to-psql-copy-rec2 rec))))
      (with-lexdb-locale (putline conn "\\."))
      (endcopy conn)
      (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)")
      )))

;; run command with stdout = filename
(defmethod run-command-stdout-to-file ((db psql-database) command filename)
  (with-open-file (ostrm filename
		   :direction :output
		   :if-exists :rename)
    (run-command-stdout db command ostrm)))

(defmethod run-command-stdout ((db psql-database) command ostrm)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (execute connection command :out (list ostrm))))

(defmethod run-command-ignore-errors ((db psql-database) command)
  (with-lexdb-client-min-messages (db "error")
    (run-command db command :ignore-errors t)))

(defmethod run-command-coe ((db psql-database) command)
  (let ((res (run-command db command :ignore-errors t)))
    (when (typep res 'sql-error)
      (format t "~%(LexDB) (postgres) ~a ... continuing" (slot-value res 'message)))
    res))
  
(defmethod run-command ((db psql-database) command &key ignore-errors)
  (with-slots (connection) db
    (unless connection
      (error "psql-database ~s has no active connection." db))
    (execute connection command :com t :ignore-errors ignore-errors)))

(defmethod quote-ident ((lex psql-lex-database) field)
  (let* ((quote-ident-cache (quote-ident-cache lex))
	 (cached (cdr (assoc field quote-ident-cache))))
    (or cached
	(let ((query-quote-ident (query-quote-ident lex field)))
	  (setf (quote-ident-cache lex)
	    (push (cons field query-quote-ident)
		  quote-ident-cache))
	  query-quote-ident))))

(defmethod query-quote-ident ((db psql-database) x)
  (unless (stringp x)
    (setf x (string-downcase (2-str x))))
  (caar 
   (get-raw-records db
    (format nil "SELECT quote_ident(~a)" 
	    (psql-quote-literal x)))))

;(SELECT c.oid FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = $1 AND c.relname = $2)

;(defmethod get-field-info ((db psql-database) schema table)
;  (get-records db
;	       (format nil
;		       "SELECT attname, typname, atttypmod FROM (SELECT attname, atttypmod, atttypid FROM pg_catalog.pg_attribute WHERE attrelid=return_oid(~a,~a)) AS a JOIN pg_catalog.pg_type AS t ON (typelem=atttypid)"
;		       (psql-quote-literal schema)
;			   (psql-quote-literal table)
;			   )))

(defmethod get-field-info ((db psql-database) schema table)
  (get-records db
	       (format nil
		       "SELECT attname, typname, atttypmod FROM (SELECT attname, atttypmod, atttypid FROM pg_catalog.pg_attribute WHERE attrelid=(SELECT c.oid FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = ~a AND c.relname = ~a)) AS a JOIN pg_catalog.pg_type AS t ON (typelem=atttypid)"
		       (psql-quote-literal schema)
			   (psql-quote-literal table)
			   )))

(defmethod get-field-info2 ((db psql-database) schema table)
  (get-records db
	       (format nil
		       "SELECT a.attname::text as field, pg_catalog.format_type(a.atttypid, a.atttypmod) as type
	FROM pg_catalog.pg_attribute a
	WHERE a.attrelid = (SELECT c.oid FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = ~a AND c.relname = ~a) AND a.attnum > 0 AND NOT a.attisdropped
	ORDER BY a.attnum"
		       (psql-quote-literal schema)
			   (psql-quote-literal table)
			   )))

;;;
;;;
;;;

(defmethod close-lex ((lexicon psql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password port quote-ident-cache) lexicon
    ;(setf dbname nil)
    ;(setf host nil)
    ;(setf user nil)
    ;(setf password nil)
    ;(setf port nil)
    ;(setf quote-ident-cache nil)
    )
  (disconnect lexicon)
  (if (next-method-p) (call-next-method)))

;;;
;;; 
;;;

(defmethod true-port ((lexicon psql-database))
  (let* ((port (or
		(port lexicon)
		#+:allegro (car (excl.osi::command-output "echo $PGPORT"))
		)))
    (if (equal port "")
	5432
      port)))

#+:null
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
      (format t "(LexDB) updating ~~/.pgpass")
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

#+:null
(defun read-pgpass nil
  (with-open-file 
      (fstream
       "~/.pgpass"
       :direction :input
       :if-does-not-exist nil)
    (when fstream
      (loop
	  for line = (read-line fstream nil :eof)
	  while (not (eq line :eof))
	  collect line))))

;;;
;;; defuns
;;;

#+:null
(defun length-as-native-string (str)
  (multiple-value-bind (dummy len)
      (excl:string-to-native str)
    (setf dummy dummy) ;;avoid compiler warning
    (1- len))) ;; len includes null byte

#+:allegro
(defun psql-quote-literal (str)
  (with-lexdb-locale
      (unless (stringp str)
	(setf str (2-str str)))
    (multiple-value-bind (native-str native-len)
	(excl:string-to-native str)
      
      (concatenate 'string "'"
		   (let* ((len (length str))
			  (x (make-array (+ len
					    native-len
					    1) ;;safe for UTF-8 coz ascii byte means ascii char
					 :element-type '(unsigned-byte 8))))
		     (pq:escape-string x native-str native-len)
		     (excl:octets-to-string x))
		   "'"))))

#+:sbcl
(defparameter *psql-quote-literal-cstr0* 
    (sb-alien:make-alien (sb-alien:c-string :external-format :utf8)))

#+:sbcl
(defparameter *psql-quote-literal-cstr* 
    (sb-alien:make-alien 
     (sb-alien:array sb-alien:char 0)))

#+:sbcl
(defun alien-array-dimensions (alien-array)
  (SB-ALIEN-INTERNALS:ALIEN-ARRAY-TYPE-dimensions 
   (SB-ALIEN-INTERNALS:ALIEN-POINTER-TYPE-to 
    (SB-ALIEN-INTERNALS:ALIEN-VALUE-type alien-array))))

#+:sbcl
(defun psql-quote-literal (str)
  (concatenate 
      'string 
    "'"
    (psql-quote-literal-aux str)
    "'"))

#+:sbcl
(defun psql-quote-literal-aux (str)
  (unless (stringp str)
    (setf str (2-str str)))
  (let* ((len0 (nlength str))
	 (maxlen (max 1
		      (+ (length str)
			 len0
			 1))))
    (when (> maxlen (car (alien-array-dimensions *psql-quote-literal-cstr*)))
      ;;we need to reserve more memory
      (format t "~%expanding *psql-quote-literal-cstr* to ~a elements" maxlen)
      (sb-alien:free-alien *psql-quote-literal-cstr*)
      (setf *psql-quote-literal-cstr* (eval
		  `(sb-alien:make-alien 
		    (sb-alien:array sb-alien:char
			   ,maxlen
			   )))))
    ;; call the foreign fn
    (pq:escape-string (sb-alien:cast *psql-quote-literal-cstr* (* sb-alien:char)) str len0)
    ;; extract the string value we want
    (setf (sb-alien:deref *psql-quote-literal-cstr0*) 
      (sb-alien:cast *psql-quote-literal-cstr* (* sb-alien:char)))
    ))

(defun nlength (str)
  (loop
      for c across str
      sum (nlength-char c)))

(defun nlength-char (char)
  (let ((code (char-code char)))
    (cond
     ((< code #x00000080)
      1)
     ((< code #x00000800)
      2)
     ((< code #x00010000)
      3)
     ((< code #x00200000)
      4)
     ((< code #x04000000)
      5)
     (t 
      (error "char code out of range for Unicode!")))))
     
(defun sql-like-text (id)
  (format nil "~a" (sql-like-text-aux (2-str id))))

(defun sql-like-text-aux (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\_)
    (format nil "\\_~a" (sql-like-text-aux (subseq str 1))))
   ((eq (char str 0) #\%)
    (format nil "\\%~a" (sql-like-text-aux (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-like-text-aux (subseq str 1))))))

;;
;;
;;

(defun retrieve-tuples (result &key col)
  (let* ((nfields (pq:nfields result))
	 (cols (and col 
		    (loop
			for c below nfields
			collect (str-2-keyword 
				 (with-lexdb-locale (pq:fname result c))
				 )
				)))
	 (ntuples (pq:ntuples result))
	 (recs (loop
		   for r below ntuples
		   collect
		     (loop
			 for c below nfields
			 collect
			   (with-lexdb-locale (pq:getvalue result r c))
			   ))))
    (list recs cols)))

(defun command-result (res)
  (let ((str-res (pq:cmd-tuples res)))
    (if (string= "" str-res)
	-1
      (str-2-num str-res))))

(defun execute (conn sql-str &key com tup out in ignore-errors)
  (let* ((result (with-lexdb-locale (pq:exec conn sql-str)))
	 (status-kw (result-status-kw result)))
    (unwind-protect
	(case status-kw
	  (:PGRES_EMPTY_QUERY 
	   (format t  "~%(LexBD) WARNING:  empty query sent to PSQL DB ~a" (pq:db conn)))
	  (:PGRES_COMMAND_OK
	   (if com
	       (command-result result)
	     (error "unexpected `command returning no data' sent to PSQL DB ~a" (pq:db conn))))
	  (:PGRES_TUPLES_OK
	   (if tup
	       (values-list (retrieve-tuples result :col (eq tup :col)))
	     (error "unexpected `query returning tuples' sent to PSQL DB ~a" (pq:db conn))))
	  (:PGRES_COPY_OUT 
	   (unless out
	     (error "unexpected `COPY OUT' data transfer operation from PSQL DB ~a" (pq:db conn)))
	   (loop
	       for ostrm in out
	       do (copy-out-stream conn ostrm))
	   t)
	  (:PGRES_COPY_IN 
	   (unless in
	     (error "unexpected `COPY IN' data transfer operation sent from PSQL DB ~a" (pq:db conn)))
	   (loop
	     for istrm in in
	       do (copy-in-stream conn istrm))
	   t)
	  (:PGRES_NON_FATAL_ERROR
	   (let* ((error-message (with-lexdb-locale (pq:result-error-message result)))
		  (sql-error (make-instance 'sql-error 
			       :type status-kw
			       :message error-message)))
	     (unless ignore-errors
	       (format t "~%(LexBD) WARNING:  (postgres) ~a" error-message))
	     sql-error))
	  (:PGRES_FATAL_ERROR
	   (let* ((error-message (with-lexdb-locale (pq:result-error-message result)))
		 (sql-error (make-instance 'sql-error 
		   :type :PGRES_FATAL_ERROR
		   :message error-message)))
	     (unless ignore-errors
	       (format t "~%(LexDB) (postgres) ~a" error-message)
	       (throw :sql-error sql-error))
	     sql-error))
	  (t
	   (error "unhandled result status")
	   nil))
      (pq:clear result)
      nil)))

(defclass sql-error ()
  ((type :initform nil :accessor type :initarg :type)
   (message :initform nil :accessor message :initarg :message)))
 
(defun copy-in-stream (conn istream)
  (do* ((line (read-line istream nil) (read-line istream nil)))
      ((null line))
    (with-lexdb-locale (putline conn line)))
  (with-lexdb-locale (putline conn "\\."))
  (endcopy conn))

(defun putline (conn line)
  (unless (= 0 (with-lexdb-locale 
		   (pq:putline conn (format nil "~a~%" line))))
    ;; fix_me
    ;;(format t "~%PSQL ~a" error-message)
    (throw :sql-error (cons :putline "unable to send string")))) 

(defun endcopy (conn)
  (unless (= 0 (pq:endcopy conn))
    ;;fix_me
    ;;(format t "~%PSQL ~a" error-message)
    (throw :sql-error (cons :putline "endcopy failed")))) 

;;TO_DO: move to PQputCopyData
;; coz getline etc. are 'Obsolete Functions for COPY'

(defparameter *psql-c-str-len* 1000) ;;must be >150
#+:allegro
(defun copy-out-stream (conn ostream)
  (let* ((c-str (ff::string-to-native 
		 (make-string *psql-c-str-len*))))
    (unwind-protect
	(do* ((line 
	       (getline conn c-str *psql-c-str-len*) 
	       (getline conn c-str *psql-c-str-len*)))
	    ((string= line "\\."))
	  (write-line line ostream)))
    (excl::aclfree c-str)
    (endcopy conn)))

#+:sbcl
(defparameter *copy-out-stream-buffer*
    (eval
     `(sb-alien:make-alien
       (sb-alien:array sb-alien:char ,*psql-c-str-len*))))

#+:sbcl
(defparameter *copy-out-stream-c-string*
    (sb-alien:make-alien (sb-alien:c-string :external-format :utf8)))

#+:sbcl
(defun copy-out-stream (conn ostream)
  (let* ((c-str *copy-out-stream-buffer*))
    (unwind-protect
	(do* ((line 
	       (getline conn c-str *psql-c-str-len*) 
	       (getline conn c-str *psql-c-str-len*)))
	    ((string= line "\\."))
	  (write-line line ostream)))
    (endcopy conn)))

#+:sbcl
(defun getline (conn c-str len)
  (loop
      for i = (pq:getline conn (sb-alien:cast c-str (* sb-alien:char)) len)
      for new-text = (setf (sb-alien:deref *copy-out-stream-c-string*)
		(sb-alien:cast c-str (* sb-alien:char)))
      for line = (concatenate 'string line new-text)
      while (= i 1)
      finally (return line)))

#+:allegro
(defun getline (conn c-str len)
  (loop
      for i = (pq:getline conn c-str len)
      for line = (concatenate 
		     'string 
		   line 
		   (with-lexdb-locale (ff::native-to-string c-str))
		   )
      while (= i 1)
      finally (return line)))

(defun result-status-kw (result)
  (cdr 
   (assoc 
    (pq:result-status result)
    pq:exec-status-kw-map
    :test #'=)))

(defun conn-status-kw (conn)
  (cdr 
   (assoc 
    (pq:status conn)
    pq:conn-status-kw-map
    :test #'=)))

(defmethod connection-ok ((lexicon psql-database))
  (equal (conn-status-kw (connection lexicon)) :CONNECTION_OK))

(defun error-msg (conn)
  (with-lexdb-locale (pq:error-message conn)))

(defun connect-db (conninfo)
  (with-lexdb-locale 
      (connect-db2 conninfo)
    ))

(defun connect-db2 (conninfo)
  (handler-case
      (pq:connectdb conninfo)
    (simple-error () (error "PostgreSQL functionality not available~% (perhaps load libpq.so?)"))))

(defun finish-db (conn)
  (pq:finish conn))