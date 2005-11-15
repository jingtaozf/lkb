;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; --- psql-database methods
;;;

(defmacro with-lexdb-locale (&body body)
  `(let ((excl::*locale* *lexdb-locale*))
     ,@body))
  
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
		(format nil "password=~a" (psql-quote-literal password))))))
      (when (connection-ok lexicon)
	(unless (check-libpq-protocol-version connection)
	  (disconnect lexicon))
	(pq:set-client-encoding connection "UNICODE")
	t))))

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

(defmethod get-records ((database psql-database) sql-string)
  (with-slots (connection) database
    (unless connection
      (error "psql-database ~s has no active connection." database))
      (multiple-value-bind (recs cols)
	  (execute connection sql-string :tup :col)
	(make-instance 'psql-database-table :recs recs :cols cols))))

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

(defmethod run-command ((database psql-database) command)
  (with-slots (connection) database
    (unless connection
      (error "psql-database ~s has no active connection." database))
    (execute connection command :com t)))

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

(defmethod get-field-info ((db psql-database) schema table)
  (get-records db
	       (format nil
		       "SELECT attname, typname, atttypmod FROM (SELECT attname, atttypmod, atttypid FROM pg_catalog.pg_attribute WHERE attrelid=return_oid(~a,~a)) AS a JOIN pg_catalog.pg_type AS t ON (typelem=atttypid)"
		       (psql-quote-literal schema)
			   (psql-quote-literal table)
			   )))

(defmethod get-field-info2 ((db psql-database) schema table)
  (get-records db
	       (format nil
		       "SELECT a.attname::text as field, pg_catalog.format_type(a.atttypid, a.atttypmod) as type
	FROM pg_catalog.pg_attribute a
	WHERE a.attrelid = return_oid(~a,~a) AND a.attnum > 0 AND NOT a.attisdropped
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
		(car (excl.osi::command-output "echo $PGPORT")))))
    (if (equal port "")
	5432
      port)))

;; unused?
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

(defun psql-quote-literal (str)
  (concatenate 'string "'"
	       (let* ((len (length str))
		      (x (make-array (1+ (* 2 len)) :element-type '(unsigned-byte 8))))
		 (excl:with-native-string (native-str str)
		   (pq:escape-string x native-str len))
		 (excl:octets-to-string x))
	       "'"))

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

;(defun 2-kw (str)
;  (intern str :keyword))
;  (intern (string-upcase str) :keyword))

(defun execute (conn sql-str &key com tup out in)
  (let* ((result (with-lexdb-locale (pq:exec conn sql-str)))
	 (status-kw (result-status-kw result)))
    (unwind-protect
	(case status-kw
	  (:PGRES_EMPTY_QUERY 
	   (format t  "~%(LexBD) WARNING:  empty query sent to PSQL DB ~a" (pq:db conn)))
	  (:PGRES_COMMAND_OK
	   (if com t
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
	   (let ((error-message (with-lexdb-locale (pq:result-error-message result))))
	     (format t "~%(LexBD) WARNING:  (postgres error) ~a" error-message))
	   nil)
	  (:PGRES_FATAL_ERROR
	   (let ((error-message (with-lexdb-locale (pq:result-error-message result))))
	     (format t "~%(LexDB) (postgres ERROR) ~a" error-message)
	     (throw :sql-error (cons status-kw error-message))))
	  (t
	   (error "unhandled result status")
	   nil))
      (pq:clear result)
      nil)))

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

(defparameter *psql-c-str-len* 1000) ;;must be >150 (WHY???)
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

(defun getline (conn c-str len)
  (loop
      for i = (pq:getline conn c-str len)
      for line = (concatenate 'string line (with-lexdb-locale (ff::native-to-string c-str)))
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
  (with-lexdb-locale (connect-db2 conninfo)))

(defun connect-db2 (conninfo)
  (handler-case
      (pq:connectdb conninfo)
    (simple-error () (error "PostgreSQL functionality not available~% (perhaps load libpq.so?)"))))

(defun finish-db (conn)
  (pq:finish conn))