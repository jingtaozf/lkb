;;; Copyright (c) 2002-2004
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Postgres interface
;;;

(defmethod make-field-map-slot ((lexicon psql-lex-database))
  ;; stores the mapping of fields to lex-entry structure slots
  (setf (fields-map lexicon)
    (mapcar #'(lambda (x) 
		(append (list (str-2-keyword (first x))
			      (str-2-keyword (second x)))
			(cddr x)))
            (records (run-query lexicon 
                                (make-instance 'sql-query
                                  :sql-string (format 
                                               nil 
                                               "SELECT slot,field,path,type FROM defn WHERE mode='~a';"
                                               (fields-tb lexicon)))))))
  (if (null (fields-map lexicon))
      (format t "~%WARNING: empty fields map in ~a mode ~a !!!" 
              (dbname lexicon) (fields-tb lexicon)))
  (fields-map lexicon))

;;; returns version, eg. "7.3.2"
(defmethod get-server-version ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT version();")
       (version-str (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    (second (split-on-char version-str))))
    
(defmethod get-db-version ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT val FROM public.meta WHERE var='db-version' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod get-filter ((lexicon psql-lex-database))
  (let* 
      ((sql-str "SELECT val FROM meta WHERE var='filter' LIMIT 1;"))
    (caar (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))
    
(defmethod next-version (id (lexicon psql-lex-database))
  (let* (
	 (sql-str (sql-next-version lexicon (string-downcase id)))
	 (res (caar (records (run-query 
			      lexicon 
			      (make-instance 'sql-query :sql-string sql-str))))))
    (str-2-num res 0)))

(defmethod get-records ((lexicon psql-lex-database) sql-string)
  (make-column-map-record 
   (run-query 
    lexicon 
    (make-instance 'sql-query :sql-string sql-string))))

(defmethod fn-get-records ((lexicon psql-lex-database) fn-name &rest rest)
  (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))

(defmethod fn-get-record ((lexicon psql-lex-database) fn-name &rest rest)
  (let ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest)))))
    (if (> (length res) 1)
        (error "too many records returned")
      (first res))))
  
(defmethod fn-get-val ((lexicon psql-lex-database) fn-name &rest rest)
  (let* ((res (get-records lexicon (eval (append (list 'fn lexicon fn-name) rest))))
         (rec (first res)))
    (if (> (length res) 1)
        (error "too many records returned")
      (if (> (length rec) 1)
          (error "multiple columns returned")
        (cdar rec)))))
  
  
(defmethod fn ((lexicon psql-lex-database) fn-name &rest rest)
  (let ((lex-fn (assoc fn-name (fns lexicon))))
    (if lex-fn
	(eval (append (list (cdr lex-fn)) rest))
      (error "Embedded-SQL fn ~a not defined. Is the latest embedded-code.sql loaded into the LexDB?" fn-name))))
  
(defmethod sql-next-version ((lexicon psql-lex-database) id)
  (fn lexicon 'next-version id))

(defmethod sql-orthography-set ((lexicon psql-lex-database))
  (fn lexicon 'orthography-set))

(defmethod sql-lex-id-set ((lexicon psql-lex-database))
  (fn lexicon 'lex-id-set))

(defmethod sql-lookup-word ((lexicon psql-lex-database) word)
  (fn lexicon 'lookup-word (string-downcase word)))

(defmethod sql-retrieve-entries-by-orthkey ((lexicon psql-lex-database) select-list word)
  (fn lexicon 'retrieve-entries-by-orthkey select-list (string-downcase word)))

(defmethod sql-retrieve-entry ((lexicon psql-lex-database) select-list word)
  (fn lexicon 'retrieve-entry select-list word))

(defmethod sql-retrieve-all-entries ((lexicon psql-lex-database) select-list)
  (fn lexicon 'retrieve-all-entries select-list))

(defmethod build-lex ((lexicon psql-database))
  (format *postgres-debug-stream* "~%(building current_grammar)")
  (fn-get-records lexicon ''initialize-current-grammar (get-filter lexicon))
  (format *postgres-debug-stream* "~%(vacuuming current_grammar)")
  (run-query lexicon (make-instance 'sql-query :sql-string "VACUUM"))
  (format *postgres-debug-stream* "~%(lexicon filter: ~a )" (get-filter lexicon))
  (format *postgres-debug-stream* "~%(active lexical entries: ~a )" (fn-get-val lexicon ''size-current-grammar))
  lexicon)

(defun build-current-grammar (lexicon)
  (format *postgres-debug-stream* "~%(building current_grammar)")
  (fn-get-records  lexicon ''build-current-grammar)
  (format *postgres-debug-stream* "~%(vacuuming current_grammar)")
  (run-query lexicon (make-instance 'sql-query :sql-string "VACUUM"))
  (format *postgres-debug-stream* "~%(lexicon filter: ~a )" (get-filter lexicon))
  (format *postgres-debug-stream* "~%(active lexical entries: ~a )" (fn-get-val lexicon ''size-current-grammar))
  (empty-cache lexicon))
  
(defmethod dump-db ((lexicon psql-lex-database) revision-filename defn-filename)  
    (fn-get-records lexicon ''dump-db revision-filename defn-filename))

(defmethod dump-scratch-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''dump-scratch-db filename))

(defmethod show-scratch ((lexicon psql-lex-database))
  (fn-get-records lexicon ''show-scratch))

(defmethod merge-into-db ((lexicon psql-lex-database) 
			  revision-filename 
			  new-entries-filename)  
  (format t "~%")
  (let ((num-new-entries (fn-get-val lexicon ''merge-into-db 
				     revision-filename 
				     new-entries-filename)))
;    (format *postgres-debug-stream* "~%(vacuuming)")
;    (run-query lexicon (make-instance 'sql-query :sql-string "VACUUM"))
    num-new-entries))

(defmethod merge-defn ((lexicon psql-lex-database) 
			  defn-filename)  
  (fn-get-val lexicon ''merge-defn 
	      defn-filename))

;(defmethod merge-into-db ((lexicon psql-lex-database) 
;			  revision-filename 
;			  filename-new-entries
;;			  defn_filename)  
;  (get-postgres-temp-filename)
;  (setf filename (namestring (pathname filename)))
;  (setf filename-new-entries (namestring (pathname filename-new-entries)))
;  (format t "~%")
;  (let ((out (fn-get-val lexicon ''merge-into-db filename *postgres-temp-filename*)))
;    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
;						 *postgres-temp-filename*
;						 filename-new-entries))
;    out))

;;(defmethod add-to-db ((lexicon psql-lex-database) filename)  
;;  (setf filename (namestring (pathname filename)))
;;  (if (catch 'pg:sql-error 
;;	(fn-get-records lexicon ''add-to-db filename))
;;      (error "cannot update lexical database ~a. Check that ~~/tmp/lexdb.new_entries does not contain attempts to redefine existing entries. No tuple of the form <name,userid,version,...> should correspond to an existing entry. [In particular, the MODSTAMP of a tuple cannot change.]" (dbname lexicon))))
      
(defun dump-psql-lexicon (filename)
  (get-postgres-temp-filename)
  (let ((revision-filename (namestring (pathname (format nil "~a.csv" filename))))
	(defn-filename (namestring (pathname (format nil "~a.dfn" filename))))
	(postgres-tmp1 (format nil "~a.1" *postgres-temp-filename*))
	(postgres-tmp2 (format nil "~a.2" *postgres-temp-filename*)))
  (dump-db *psql-lexicon* postgres-tmp1 postgres-tmp2)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       postgres-tmp1
					       revision-filename))
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       postgres-tmp2
					       defn-filename))
  ))

(defun dump-scratch (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (dump-scratch-db *psql-lexicon* *postgres-temp-filename*)
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

;;(defun normalize-csv-lexicon (filename-in filename-out)
;;  (get-postgres-temp-filename)
;;  (setf filename-in (namestring (pathname filename-in)))
;;  (setf filename-out (namestring (pathname filename-out)))
;;  (fn-get-records *psql-lexicon* ''normalize-csv-lexicon filename-in *postgres-temp-filename*)
;;  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
;;					       *postgres-temp-filename*
;;					       filename-out)))

;;(defun merge-into-psql-lexicon (filename)
;;  (setf filename (namestring (pathname filename)))
;;  (let* ((dump-filename (format nil "~a/lexdb-temp.merge" *postgres-user-temp-dir*))
;;	 (filename-normalized (format nil "~a.new" dump-filename))
;;	 (filename-sorted (format nil "~a.s" filename-normalized))
;;	 (dump-filename-sorted (format nil "~a.s" dump-filename))
;;	 (add-filename (format nil "~a.add" dump-filename))
;;	 (command-str-sort-file (format nil "LANG=c sort ~a > ~a" filename-normalized filename-sorted))
;;	 (command-str-sort-dumpfile (format nil "LANG=c sort ~a > ~a" dump-filename dump-filename-sorted))
;;	 (command-str-add (format nil "LANG=c diff ~a ~a | LANG=c grep -e '^> ' | LANG=c sed 's/^> //' > ~a" dump-filename-sorted filename-sorted add-filename))
;;	 (command-str-rm-files (format nil "rm ~a*" dump-filename))
;;	 )
;;    (unless
;;	(and *psql-lexicon* (connection *psql-lexicon*))
;;      (initialize-psql-lexicon))
;;    (format *postgres-debug-stream* "~%(dumping current lexicon)")
;;    (dump-psql-lexicon dump-filename)
;;    (format *postgres-debug-stream* "~%(normalizing new lexicon)")
;;    (normalize-csv-lexicon filename filename-normalized)
;;    (format *postgres-debug-stream* "~%(sorting files)")
;;    (common-lisp-user::run-shell-command command-str-sort-file)
;;    (common-lisp-user::run-shell-command command-str-sort-dumpfile)
;;    (format *postgres-debug-stream* "~%(updating db)")
;;    (common-lisp-user::run-shell-command command-str-add)
;;    (add-to-db *psql-lexicon* add-filename)
;;    (common-lisp-user::run-shell-command (format nil "mv ~a ~a/lexdb.new_entries" add-filename *postgres-user-temp-dir*))
;;    (common-lisp-user::run-shell-command command-str-rm-files)
;;    (format *postgres-debug-stream* "~%(building current_grammar)")
;;    (build-current-grammar *psql-lexicon*)))

(defun merge-into-psql-lexicon2 (lexicon filename)
  (get-postgres-temp-filename)
  (let ((revision-filename (namestring (pathname (format nil "~a.csv" filename))))
	(new-entries-filename 
	 (namestring (pathname (format nil "~a/lexdb.new_entries" *postgres-user-temp-dir*))))
	(defn-filename (namestring (pathname (format nil "~a.dfn" filename))))
	)
    
    (format t "~%(~a new revision entries)"
	    (merge-into-db lexicon 
			   revision-filename
			    *postgres-temp-filename*))
    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
						 *postgres-temp-filename*
						 new-entries-filename))
    
    (if (probe-file defn-filename)
	(format t "~%(~a new defn entries)"
		(merge-defn lexicon 
			    defn-filename)))
    (build-current-grammar *psql-lexicon*)
    ))

(defmethod initialize-userschema ((lexicon psql-database))
  (unless
      (fn-get-val lexicon ''test-user *postgres-current-user*)
    (format *postgres-debug-stream* "~%(initializing schema ~a)" *postgres-current-user*)
    (fn-get-val lexicon ''create-schema *postgres-current-user*)
    (if *postgres-mwe-enable*
	(mwe-initialize-userschema lexicon))
    ))

(defmethod retrieve-fn-defns ((lexicon psql-lex-database))
  (let* ((sql-str (format nil "SELECT * FROM qry;"))
	 (records (make-column-map-record (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str)))))
    (loop
      for record in records
	do
	  (retrieve-fn-defn lexicon record))))

(defmethod retrieve-fn-defn ((lexicon psql-lex-database) record)
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
		     (str-list-2-str (get-$-args arity) ",") 
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

