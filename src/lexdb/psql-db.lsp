;;; Copyright (c) 2002-2004
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Postgres interface
;;;

;;;
;;; moved here from `src/main/initializations.lsp'.           (20-may-04; oe)
;;;

(defun psql-initialize ()
  ;;
  ;; make sure we `mark' the current universe as PSQL-enabled.
  ;;
  (pushnew :psql *features*)
  (handler-case (load "libpq.so") 
    (file-error () 
      ;; some feedback to user
      (format t ";   Warning: cannot load libpq.so")
      (format t "~%;            (PSQL lexicon functionality will be unavailable)")
      (format t "~%;            (hint: are the PostgreSQL libraries installed on your machine?)")
      ;; need this for backward compatibility with ERG script
      ;; (also a good idea anyway)
      (setf *features* (remove :psql *features*)))))


(defmethod make-field-map-slot ((lexicon psql-lex-database))
  ;; stores the mapping of fields to lex-entry structure slots
  (setf (fields-map lexicon)
    (sort
     (mapcar #'(lambda (x) 
		 (list (str-2-keyword (first x))
		       (str-2-keyword (second x))
		       (third x)
		       (2-symb-or-list (fourth x))))
	     (records (run-query lexicon 
				 (make-instance 'sql-query
				   :sql-string (format 
						nil 
						"SELECT slot,field,path,type FROM defn WHERE mode='~a';"
						(fields-tb lexicon))))))
     #'(lambda (x y) (declare (ignore y)) (eq (car x) :unifs))))
    (if (null (fields-map lexicon))
	(format t "~%WARNING: No definitions for mode='~a' found in table public.defn of database ~a. (Hint: check the value of :table in *psql-lexicon-parameters*, ensure DB table public.defn matches the definitions in lexicon.dfn. If necessary 'Merge new entries' from LexDB menu)" 
	       (fields-tb lexicon) (dbname lexicon))
      )
    (fields-map lexicon))

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
  (cond 
   ((not (user-read-only-p lexicon))
    (fn-get-records lexicon ''initialize-current-grammar (get-filter lexicon))
    )
   (t
    (format t "~%(user ~a has read-only privileges)" (user lexicon))))    
  (format t "~%(LexDB filter: ~a )" (get-filter lexicon))
  (let ((size (fn-get-val lexicon ''size-current-grammar)))
    (if (string= "0" size)
	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
      (format t "~%(active lexical entries: ~a )" size)))

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

(defun build-current-grammar (lexicon)
  (cond 
   ((not (user-read-only-p lexicon))
    (fn-get-records  lexicon ''build-current-grammar)
    ;;(format *postgres-debug-stream* "~%(vacuuming current_grammar)")
    ;;(run-query lexicon (make-instance 'sql-query :sql-string "VACUUM current_grammar"))
    )
   (t
    (format t "~%(user ~a had read-only privileges)" (user lexicon))))
  (format *postgres-debug-stream* "~%(LexDB filter: ~a )" (get-filter lexicon))
  (format *postgres-debug-stream* "~%(active lexical entries: ~a )" (fn-get-val lexicon ''size-current-grammar))
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

(defun dump-psql-lexicon (filename)
  (when
      (catch 'pg:sql-error
	(progn
	  (get-postgres-temp-filename)
	  (let* ((revision-filename 
		 (namestring (pathname (format nil "~a.csv" filename))))
		(defn-filename 
		    (namestring (pathname (format nil "~a.dfn" filename))))
		 (pg-files 
		  (string-2-str-list-on-spc
		   (dump-db *psql-lexicon*)
		   :esc nil))
		 (pg-rev (first pg-files))
		 (pg-dfn (second pg-files)))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-rev
							 revision-filename))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-dfn
							 defn-filename))
	    nil)))
    (format t "~%Dump aborted...")))

(defun dump-scratch (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (dump-scratch-db *psql-lexicon* *postgres-temp-filename*)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

(defun absolute-namestring (format str)
  (namestring (pathname (format nil format str))))

(defun merge-into-psql-lexicon (lexicon filename)
  "reconnect as db owner and merge new data into lexdb"
  (with-slots (dbname host port) lexicon
    (let ((conn-db-owner (make-instance 'psql-database
			   :dbname dbname
			   :host host
			   :port port
			   :user (raw-get-val lexicon "SELECT db_owner()")))
	  (count-new-dfn 0)
	  (count-new 0))
      (connect conn-db-owner)
      (retrieve-fn-defns conn-db-owner)
      (when
	  (catch 'pg:sql-error
	    (progn
	      (get-postgres-temp-filename)
	      (let* ((rev-filename 
		      (absolute-namestring "~a.csv" 
					   filename))
		     (dfn-filename 
		      (absolute-namestring "~a.dfn" 
					   filename)))
		(setf count-new 
		  (merge-into-db conn-db-owner 
				 rev-filename))
		(if (probe-file dfn-filename)
		    (setf count-new-dfn
		      (merge-defn conn-db-owner 
				  dfn-filename)))
		nil
		)))
	(format t "Merge new entries aborted..."))
      (if (and 
	   (equal count-new 0)
	   (equal count-new-dfn 0))
	  nil
	(initialize-psql-lexicon)))))

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

(defun make-db-access-fn (str-fn-name-in str type-list)
  (let* ((fn-name (new-fn-name (concatenate 'string "sql-query-string-" (string str-fn-name-in))))
	 (tmp (prepare-db-access-fn str type-list str-fn-name-in))
	 (format-cmd (append '(format nil) (car tmp)))
	 (args (cdr tmp))
	 (fn-defn (list 'defun fn-name args format-cmd)))
    (eval fn-defn)))

(defun new-fn-name (str)
  (loop
      with i = 0
      with fn-name
      do
	(setf fn-name (str-2-symb (concatenate 'string str (num-2-str i))))
	(unless (fboundp fn-name)
	  (return fn-name))
	(setf i (1+ i))))
	
(defun prepare-db-access-fn (str type-list str-fn-name)
  (let ((stream (make-string-output-stream))
	(args)
	(arg-vars '(a b c d e f g h i j))
	(arity (length type-list)))
  (loop
      with max = (1- (length str))
      and c
      for i from 0 to max
      with max-arg = -1
      and arg
      and type
      and explicit-type-str
      do
	(setf c (aref str i))
	(cond 
	 ((eq c #\~)
	  (format stream "~~~~"))
	 ((eq c #\\)
	  (if (= i max)
	      (error "invalid string ('\\' cannot be string final)"))
	  (format stream "~a" (aref str (1+ i)))
	  (setf i (1+ i)))
	 ((eq c #\$)
	  (if (= i max)
	      (error "invalid string ('$' cannot be string final)"))
;	  (unless (numberp (char-2-symb (aref str (1+ i))))
	  (setf arg (char-2-num (aref str (1+ i))))
	  (unless arg
	    (error "invalid string ('$' can only preceed a digit)"))
	  (if (> arg (1- arity))
	      (error "whilst compiling embedded SQL function ~a(~a). Argument $~a is not valid in 
~%~a~a~a" 
		     str-fn-name 
		     (str-list-2-str-by-str (get-$-args arity) ",") 
		     arg 
		     (if (> (- i 20) 0) "..." "")
		     (subseq str 
			     (max 0 (- i 20)) 
			     (min (length str) (+ i 20)))
		     (if (< (+ i 20) (length str)) "..." "")
		     ))
	  (setf max-arg (max max-arg arg))
	  (setf type (cdr (assoc arg type-list)))
	  (setf explicit-type-str (get-explicit-type str (1+ i)))
	  (when explicit-type-str
	    (setf type (str-2-symb explicit-type-str))
	    (setf i (+ i 1 (length explicit-type-str))))
	  (cond
	   ((equal type 'text)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'like-text)
	    (push (list 'sql-like-text (nth arg arg-vars)) args))
	   ((equal type 'select-list)
	    (push (nth arg arg-vars) args))
	   ((equal type 'value-list)
	    (push (nth arg arg-vars) args))
	   ((equal type 'where-subcls)
	    (push (nth arg arg-vars) args))
	   
	   ((equal type 'e-text)
	    (push (list 'sql-embedded-text (list 'sql-embedded-text (nth arg arg-vars))) args))
	   ((equal type 'e-like-text)
	    (push (list 'sql-embedded-text (list 'sql-like-text (nth arg arg-vars))) args))
	   ((equal type 'e-select-list)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'e-value-list)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   ((equal type 'e-where-subcls)
	    (push (list 'sql-embedded-text (nth arg arg-vars)) args))
	   (t
	    (error "unknown type: ~A" type)))
	  (format stream "~~a")

	  (setf i (1+ i)))
	 (t
	  (format stream "~a" (aref str i)))))
  (cons (cons (get-output-stream-string stream) (reverse args)) 
	(subseq arg-vars 0 arity))))

(defun get-$-args (arity)
    (loop
	for i from 1 to arity
	collect (format nil "$~a" (1- i))))

(defun get-explicit-type (str i)
  (let* ((j (1+ i))
	 (end-char-set '(#\Space #\Newline #\Return))
	 (type-str
	  (and (< (1+ j) (length str))
	       (eq (aref str j) #\:)
	       (not 
		 (member (aref str (1+ j)) end-char-set))
	       (subseq str (1+ j) (position-char-set end-char-set str :start j)))))
    type-str))

(defun position-char-set (char-set string &key (start 0))
  (loop
      for i from start to (1- (length string))
      do
	
      (if 
	  (member (aref string i) char-set)
	  (return-from position-char-set i)))
      nil)
      
(defun get-postgres-temp-filename nil
  (setf *postgres-temp-filename*
    (format nil "~a.~a" "/tmp/postgres-temp" (sys:user-name))))

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
