;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; --- psql-database methods
;;;

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
		(format nil "connect_timeout=~a " (sql-embedded-text (2-str connect-timeout))))
	   (and port 
		(format nil "port=~a " (sql-embedded-text (2-str port))))
	   (and host
		(format nil "host=~a " (sql-embedded-text host)))
	   (format nil "dbname=~a " (sql-embedded-text dbname))
	   (format nil "user=~a " (sql-embedded-text user))
	   (and password 
		(format nil "password=~a" (sql-embedded-text password))))))
      (when (connection-ok lexicon)
	t))))

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

;;;
;;;
;;;

(defmethod close-lex ((lexicon psql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password port) lexicon
    ;(setf dbname nil)
    ;(setf host nil)
    ;(setf user nil)
    ;(setf password nil)
    ;(setf port nil)
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

(defun sql-embedded-text (str)
  (format nil "'~a'" (sql-embedded-text-aux str)))

;; ' -> \'
;; \ -> \\
(defun sql-embedded-text-aux (str)
  (cond
   ((equal str "")
    "")
   ((eq (char str 0) #\')
    (format nil "\\'~a" (sql-embedded-text-aux (subseq str 1))))
   ((eq (char str 0) #\\)
    (format nil "\\\\~a" (sql-embedded-text-aux (subseq str 1))))
   (t
    (format nil "~a~a" (char str 0) (sql-embedded-text-aux (subseq str 1))))))

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
			collect (2-kw (pq:fname result c)))))
	 (ntuples (pq:ntuples result))
	 (recs (loop
		   for r below ntuples
		   collect
		     (loop
			 for c below nfields
			 collect
			   (pq:getvalue result r c)))))
    (list recs cols)))

(defun 2-kw (str)
  (intern (string-upcase str) :keyword))

(defun execute (conn sql-str &key com tup out in)
  (let* ((result (pq:exec conn sql-str))
	 (status-kw (result-status-kw result)))
    (unwind-protect
	(case status-kw
	  (:PGRES_EMPTY_QUERY 
	   (format t  "~%WARNING: empty query sent to PSQL DB ~a" (pq:db conn)))
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
	   (copy-out-stream conn (car out))
	   (setf out (cdr out))
	   )
	  (:PGRES_COPY_IN 
	   (unless in
	     (error "unexpected `COPY IN' data transfer operation sent from PSQL DB ~a" (pq:db conn)))
	   (copy-in-stream conn (car in))
	   (setf in (cdr in))	     
	   )
	  (:PGRES_NON_FATAL_ERROR
	   (let ((error-message (pq:result-error-message result)))
	     (format t "~%WARNING: (pgres error) ~a" error-message))
	   nil)
	  (:PGRES_FATAL_ERROR
	   (let ((error-message (pq:result-error-message result)))
	     (format t "~%PSQL ~a" error-message)
	     (throw :sql-error (cons status-kw error-message))))
	  (t
	   (error "unhandled result status")))
      (pq:clear result)
      nil)))

(defun copy-in-stream (conn istream)
  (do* ((line (read-line istream nil) (read-line istream nil)))
      ((null line))
    (putline conn line))
  (putline conn "\\.")
  (endcopy conn))

(defun putline (conn line)
  (unless (= 0 (pq:putline conn (format nil "~a~%" line)))
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
      for line = (concatenate 'string line (ff::native-to-string c-str))
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
  (pq:error-message conn))

(defun connect-db (conninfo)
  (handler-case
      (pq:connectdb conninfo)
    (simple-error () (error "PostgreSQL functionality not available~% (perhaps load libpq.so?)"))))

(defun finish-db (conn)
  (pq:finish conn))