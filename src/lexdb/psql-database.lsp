;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

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

;;;
;;; execute db queries/commands
;;;

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (multiple-value-bind (recs cols)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs
            (columns query) (mapcar #'str-2-keyword cols)))
    query))

(defmethod run-command-stdin ((database psql-database) command filename)
  (with-slots (connection) database
    (unless connection
      (error "Database ~s has no active connection." database))
    (pg::stdin-command-file connection command filename)))

(defmethod run-command ((database psql-database) command)
  (run-query database 
	     (make-instance 'sql-query :sql-string command)))

;;;
;;;
;;;

(defmethod close-lex ((lexicon psql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (server-version fns) lexicon
    (disconnect lexicon)
    (setf server-version nil)
    (setf fns nil)
    (if (next-method-p) (call-next-method))))

;;;
;;; virtual fns
;;;

;(defmethod fn-get-records ((lexicon psql-database) fn-name &rest rest)
;  (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))

;(defmethod fn-get-raw-records ((lexicon psql-database) fn-name &rest rest)
;  (get-raw-results lexicon (eval (append (list 'fn lexicon fn-name) rest))))


;(defmethod fn-get-record ((lexicon psql-database) fn-name &rest rest)
;  (let ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest)))))
;    (if (> (length res) 1)
;        (error "too many records returned")
;      (first res))))
  
;(defmethod fn-get-val ((lexicon psql-database) fn-name &rest rest)
;  (let* ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))
;         (rec (first res)))
;    (if (> (length res) 1)
;        (error "too many records returned")
;      (if (> (length rec) 1)
;          (error "multiple columns returned")
;        (cdar rec)))))
  
;(defmethod raw-get-val ((lexicon psql-database) sql-str)
;  (let* ((res (get-records lexicon sql-str))
;         (rec (first res)))
;    (if (> (length res) 1)
;        (error "too many records returned")
;      (if (> (length rec) 1)
;          (error "multiple columns returned")
;        (cdar rec)))))
  
;(defmethod fn ((lexicon psql-database) fn-name &rest rest)
;  (unless (connection lexicon)
;    (error "no connection to psql lexicon"))
;  (let ((lex-fn (assoc fn-name (fns lexicon))))
;    (if lex-fn
;	(eval (append (list (cdr lex-fn)) rest))
;      (error "Embedded-SQL fn ~a not defined. Is the latest embedded-code.sql loaded into the LexDB?" fn-name))))

(defun sql-fn-string (fn &key args fields)
  (unless (member fn *postgres-sql-fns*)
    (error "~a not in *postgres-sql-fns*" fn))
  (unless fields
    (setf fields '(:*)))
  (let ((fields-str (str-list-2-str
		     (mapcar #'string fields)
		     :sep-c #\,))
	(fn-str (string fn))
	(args-str 
	  (str-list-2-str
	   (mapcar
	    #'sql-fn-arg
	    args)
	   :sep-c #\,
	   :esc nil)))
    (format nil "SELECT ~a FROM ~a(~a)" fields-str fn-str args-str)))

(defun sql-fn-arg (x)
  (cond
   ((stringp x)
    (sql-embedded-text x))
   ((listp x)
    (sql-embedded-text
     (str-list-2-str
      (mapcar
       #'sql-fn-arg
       x)
      :sep-c #\,)))
   (t
    (2-str x))))

(defmethod sql-fn-get-records ((lexicon psql-database) fn &key args fields)
  (get-records lexicon
	       (sql-fn-string fn :args args :fields fields)))
  
(defmethod sql-fn-get-raw-records ((lexicon psql-database) fn &key args fields)
  (get-raw-records lexicon
		   (sql-fn-string fn :args args :fields fields)))
  
(defmethod sql-fn-get-val ((lexicon psql-database) fn &key args fields)
  (caar (sql-fn-get-raw-records lexicon fn :args args :fields fields)))
  
;(defmethod retrieve-fn-defns ((lexicon psql-database))
;  (let* ((sql-str (format nil "SELECT * FROM qry;"))
;       (records (make-column-map-record (run-query
;                     lexicon
;                     (make-instance 'sql-query :sql-string sql-str)))))
;    (loop
;      for record in records
;      do
;        (retrieve-fn-defn lexicon record))))

;(defmethod retrieve-fn-defn ((lexicon psql-database) record)
;  (let* ((fn (get-val :fn record))
;	 (arity (str-2-num (get-val :arity record)))
;	 (sql-code (get-val :sql_code record))
;	 (sql-str (format nil "SELECT * FROM qrya WHERE fn='~a';" fn))
;	 (ergqa-records 
;	  (make-column-map-record 
;	   (run-query 
;	    lexicon 
;	    (make-instance 'sql-query :sql-string sql-str))))
;	 (type-list 
;	  (mapcar #'(lambda (record) 
;		      (cons 
;		       (str-2-num (get-val :arg record))
;		       (str-2-symb (get-val :type record))))
;		  ergqa-records)))
;    (unless (= arity (length type-list))
;      (error "wrong number of argument defns for embedded SQL fn ~a in lexical database ~a" fn (dbname lexicon)))
;    (push (cons (str-2-symb fn) 
;		(make-db-access-fn fn sql-code type-list))
;	  (fns lexicon))))

;;;
;;;
;;;

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

(defmethod current-timestamp ((lexicon psql-database))
  (sql-fn-get-val lexicon 
		  :retrieve_current_timestamp))

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-database))
  (let ((version-str 
	 (sql-fn-get-val lexicon
			 :version)))
    (second (split-on-char version-str))))

;;;
;;;
;;;

;(defun make-db-access-fn (str-fn-name-in str type-list)
;  (let* ((fn-name (new-fn-name (concatenate 'string "sql-query-string-" (string str-fn-name-in))))
;	 (tmp (prepare-db-access-fn str type-list str-fn-name-in))
;	 (format-cmd (append '(format nil) (car tmp)))
;	 (args (cdr tmp))
;	 (fn-defn (list 'defun fn-name args format-cmd)))
;    (eval fn-defn)))

;(defun new-fn-name (str)
;  (loop
;      with i = 0
;      with fn-name
;      do
;	(setf fn-name (str-2-symb (concatenate 'string str (num-2-str i))))
;	(unless (fboundp fn-name)
;	  (return fn-name))
;	(setf i (1+ i))))
	
;(defun prepare-db-access-fn (str type-list str-fn-name)
;  (let ((stream (make-string-output-stream))
;	(args)
;	(arg-vars '(a b c d e f g h i j))
;	(arity (length type-list)))
;  (loop
;      with max = (1- (length str))
;      and c
;      for i from 0 to max
;      with max-arg = -1
;      and arg
;      and type
;      and explicit-type-str
;      do
;	(setf c (aref str i))
;	(cond 
;	 ((eq c #\~)
;	  (format stream "~~~~"))
;	 ((eq c #\\)
;	  (if (= i max)
;	      (error "invalid string ('\\' cannot be string final)"))
;	  (format stream "~a" (aref str (1+ i)))
;	  (setf i (1+ i)))
;	 ((eq c #\$)
;	  (if (= i max)
;	      (error "invalid string ('$' cannot be string final)"))
;;	  (unless (numberp (char-2-symb (aref str (1+ i))))
;	  (setf arg (char-2-num (aref str (1+ i))))
;	  (unless arg
;	    (error "invalid string ('$' can only preceed a digit)"))
;	  (if (> arg (1- arity))
;	      (error "whilst compiling embedded SQL function ~a(~a). Argument $~a is not valid in 
;~%~a~a~a" 
;		     str-fn-name 
;		     (str-list-2-str-by-str (get-$-args arity) ",") 
;		     arg 
;		     (if (> (- i 20) 0) "..." "")
;		     (subseq str 
;			     (max 0 (- i 20)) 
;			     (min (length str) (+ i 20)))
;		     (if (< (+ i 20) (length str)) "..." "")
;		     ))
;	  (setf max-arg (max max-arg arg))
;	  (setf type (cdr (assoc arg type-list)))
;	  (setf explicit-type-str (get-explicit-type str (1+ i)))
;	  (when explicit-type-str
;	    (setf type (str-2-symb explicit-type-str))
;	    (setf i (+ i 1 (length explicit-type-str))))
;	  (cond
;	   ((equal type 'text)
;	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
;	   ((equal type 'like-text)
;	    (push (list 'sql-like-text (nth arg arg-vars)) args))
;	   ((equal type 'select-list)
;	    (push (nth arg arg-vars) args))
;	   ((equal type 'value-list)
;	    (push (nth arg arg-vars) args))
;	   ((equal type 'where-subcls)
;	    (push (nth arg arg-vars) args))
;	   
;	   ((equal type 'e-text)
;	    (push (list 'sql-embedded-text (list 'sql-embedded-text (nth arg arg-vars))) args))
;	   ((equal type 'e-like-text)
;	    (push (list 'sql-embedded-text (list 'sql-like-text (nth arg arg-vars))) args))
;	   ((equal type 'e-select-list)
;	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
;	   ((equal type 'e-value-list)
;	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
;	   ((equal type 'e-where-subcls)
;	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
;	   (t
;	    (error "unknown type: ~A" type)))
;	  (format stream "~~a")
;
;	  (setf i (1+ i)))
;	 (t
;	  (format stream "~a" (aref str i)))))
;  (cons (cons (get-output-stream-string stream) (reverse args)) 
;	(subseq arg-vars 0 arity))))
;
;(defun get-$-args (arity)
;    (loop
;	for i from 1 to arity
;	collect (format nil "$~a" (1- i))))
;
;(defun get-explicit-type (str i)
;  (let* ((j (1+ i))
;	 (end-char-set '(#\Space #\Newline #\Return))
;	 (type-str
;	  (and (< (1+ j) (length str))
;	       (eq (aref str j) #\:)
;	       (not 
;		 (member (aref str (1+ j)) end-char-set))
;	       (subseq str (1+ j) (position-char-set end-char-set str :start j)))))
;    type-str))

;(defun position-char-set (char-set string &key (start 0))
;  (loop
;      for i from start to (1- (length string))
;      do
;	
;      (if 
;	  (member (aref string i) char-set)
;	  (return-from position-char-set i)))
;      nil)
      
