;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
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
		     (pg:error-message (connection lexicon))) 
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
	(pg:connect-db-with-handler 
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
      (when (eq :connection-ok (connection-status lexicon))
	t))))

(defmethod connection-status ((lexicon psql-database))
  (pg:decode-connection-status 
	    (pg:status (connection lexicon))))

(defmethod disconnect ((lexicon psql-database))
  (with-slots (connection) lexicon
  ;:close connection cleanly
    (when connection 
      (pg:finish connection)
      (setf connection nil))))

(defmethod reconnect ((lexicon psql-database))
  (disconnect lexicon)
  (connect lexicon))

;;;
;;; execute db queries/commands
;;;

(defmethod get-records ((lexicon psql-database) sql-string)
  (make-column-map-record (run-command lexicon sql-string)))

(defmethod get-raw-records ((lexicon psql-database) sql-string)
  (records (run-command lexicon sql-string)))

;;;

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "psql-database ~s has no active connection." database))
    (multiple-value-bind (recs cols)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs
            (columns query) (mapcar #'str-2-keyword cols)))
    query))

;; run command with stdin = filename
(defmethod run-command-stdin ((database psql-database) command filename)
  (with-slots (connection) database
    (unless connection
      (error "psql-database ~s has no active connection." database))
    (pg::stdin-command-file connection command filename)))

(defmethod run-command ((database psql-database) command)
  (run-query database 
	     (make-instance 'sql-query :sql-string command)))

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

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-database))
  (unless (string< (lexdb-version lexicon) "3.34")
    (error "obsolete function call"))
  (let ((version-str 
	 (caar (get-raw-records lexicon "SELECT * FROM VERSION()"))))
    (second (split-on-char version-str))))

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
