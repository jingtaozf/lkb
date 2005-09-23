;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;; --- psql-lex-database methods
;;;

(defmacro with-lexdb-user-lexdb ((lexdb-lexdb lexdb) &body body)
  `(with-slots (dbname host port fields-tb) ,lexdb
     (let ((,lexdb-lexdb
	    (make-instance 'psql-lex-database
	      :dbname dbname
	      :fields-tb fields-tb
	      :host host
	      :port port
	      :user (db-owner ,lexdb))))
       (open-lex ,lexdb-lexdb)
       ,@body
       (close-lex ,lexdb-lexdb))))
  
(defmacro with-lexdb-user-x ((user lexdb-lexdb lexdb) &body body)
  `(with-slots (dbname host port fields-tb) ,lexdb
     (let ((,lexdb-lexdb
	    (make-instance 'psql-lex-database
	      :dbname dbname
	      :fields-tb fields-tb
	      :host host
	      :port port
	      :user ,user)))
       (open-lex ,lexdb-lexdb)
       ,@body
       (close-lex ,lexdb-lexdb))))
  
(defmacro lexdb-time ((start-msg end-msg) &body body)
  `(let (time out)
    (format t "~&(LexDB) ~a ..." ,start-msg)
    (force-output)
    (setf time (get-internal-real-time))
    (setf out (progn ,@body))
    (format t "~&(LexDB) ~a [~F sec]" ,end-msg 
	    (/ (- (get-internal-real-time) time) internal-time-units-per-second))
    out
    ))
  
(defmethod lookup-word ((lex psql-lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*))
  (with-slots (lexical-entries) lex
  (let ((hashed (gethash orth lexical-entries)))
    (cond 
     (hashed
      (if (eq hashed :EMPTY)
	  (setf hashed nil)
	hashed))
     (t 
      (let ((value (lookup-word-no-cache lex orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth lexical-entries) 
	    (if value value :EMPTY)))
	value))))))

;; orthkey must be mapped to normalized form before entering PSQL universe
(defmethod lookup-word-no-cache ((lex psql-lex-database) orth)
  (declare (ignore cache))
  (if (connection lex)
      (let* ((table 
	      (get-records lex
			   (format nil "SELECT ~a FROM (SELECT lex.* FROM lex JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a) AS foo"
				   (fields-str (grammar-fields lex))
				   (quote-literal lex (sql-like-text (normalize-orthkey orth))))))
	     (ids (lookup-word-aux2 lex table)))
	ids)))

(defmethod uncached-orthkeys ((lex psql-lex-database) list-orth)
  (with-slots (lexical-entries) lex
    (loop
	for orth in list-orth
	unless (gethash orth lexical-entries)
	collect orth)))

(defun prune-quotes (str)
  (let ((len (length str)))
    (subseq str 1 (1- len))))

(defmethod lookup-words-cache ((lex psql-lex-database) table uc-list-orth)
  (with-slots (psorts lexical-entries record-cache dfn) lex
    (let ((name-field (second (assoc :id dfn))))
      (mapc #'(lambda (x)
		(unless (gethash x lexical-entries)
		  (setf (gethash x lexical-entries) :empty)))
	    uc-list-orth)
      
      (loop
	  with cols = (cols table)
	  for rec in (recs table)
	  for orthkey = (prune-quotes (car rec))
	  for record = (cdr rec)
	  for columns = (cdr cols)
	  for id = (str-2-symb (get-val name-field record columns))
	  do
	    (unless (gethash id record-cache)
	      (setf (gethash id record-cache) 
		record))
	    ;; is this necessary?
	    (unless (gethash id psorts)
	      (setf (gethash id psorts) 
		(make-psort-struct lex record columns)))
	    ;; cache orthkey id
	    (setf (gethash orthkey lexical-entries)
	      (add-w-empty id (gethash orthkey lexical-entries)))))))

(defmethod lookup-word-aux2 ((lex psql-lex-database) table)
  (with-slots (psorts record-cache dfn) lex
    (let ((name-field (second (assoc :id dfn))))
      (loop
	  with cols = (cols table)
	  for rec in (recs table)
	  for id = (str-2-symb (get-val name-field rec cols))
	  do
	    ;; cache values
	    (unless (gethash id record-cache)
	      (setf (gethash id record-cache) 
		rec))
	    (unless (gethash id psorts)
	      (setf (gethash id psorts) 
		(make-psort-struct lex rec cols)))
	  collect id))))

;

;;; (used to index for generator)
(defmethod lex-words ((lex psql-lex-database))
  (mapcar #'(lambda (x) (string-upcase (car x)))
	  (get-raw-records *lexdb* "select distinct key from lex_key")))
  
(defmethod regenerate-orthkeys ((lex psql-lex-database))
  (run-command lex "DELETE FROM lex_key")
  (get-raw-records lex "SELECT * FROM public.deindex_lex_key()")
  (generate-missing-orthkeys lex :from :lex :to :lex_key)
  (get-raw-records lex "SELECT * FROM public.index_lex_key()"))

(defmethod generate-missing-orthkeys ((lex psql-lex-database) 
				      &key 
				      (from "lex natural left join lex_key where lex_key.key is null")
				      (to :lex_key))
  (lexdb-time ("ensuring 'lex_key' table is up-to-date" "done ensuring 'lex_key' table is up-to-date")
	      (let ((new-key-lines (generate-orthkeys-COPY-str lex :from from)))
		(if new-key-lines
		    (run-command-stdin lex (format nil "COPY ~a FROM stdin" to)
				       (make-string-input-stream new-key-lines))
		  ))))

(defmethod generate-orthkeys-COPY-str ((lex psql-lex-database) &key from)
  (unless from
    (error "no :from string specified"))
  (unless (dfn lex)
    (error "no dfn definitions available"))
  (let* ((orth-raw-mapping (assoc :orth (dfn lex)))
	 (raw-orth-field-str (2-str (second orth-raw-mapping)))
       
	 (numo-t (get-records lex
			      (format nil "SELECT name,userid,modstamp,~a FROM ~a"
				      raw-orth-field-str
				      from)))
	 (recs (recs numo-t))
	 (len-recs (length recs)))
    (format t "~&(LexDB) [~a keyless entries]" len-recs)
    (when (> len-recs 0)
      (join-str-lines
       (mapcar #'to-psql-COPY-rec
	       (rev-to-rev-key lex recs))))))

(defmethod rev-to-rev-key ((lex psql-lex-database) recs)
  (let* ((orth-raw-mapping (assoc :orth (dfn lex)))
	 (orth-raw-value-mapping (fourth orth-raw-mapping)))
    (loop
	for rec in recs
	append 
	  (loop
	    for key in (mapcar #'normalize-orthkey
			       (car (work-out-value orth-raw-value-mapping (fourth rec))))
	      collect
		(list (first rec) (second rec) (third rec) key)))))

(defparameter *newline-str* (string (code-char 10)))
(defun join-str-lines (lines)
  (if (null lines) ""
    (loop
	with strm = (make-string-output-stream)
	for line in lines
	do
	  (princ line strm)
	  (terpri strm)
	finally
	  (return (get-output-stream-string strm)))))
      
(defun to-psql-COPY-rec (lst &key (delim-char #\tab) (null "\\N"))
  (cond
   ((null lst)
    "")
   (t
    (apply #'concatenate 'string 
	   (cons (psql-COPY-val (car lst) :delim-char delim-char :null null)
		 (loop
		     with delim = (string delim-char)
		     for x in (cdr lst)
		     collect delim
		     collect (psql-COPY-val x :delim-char delim-char :null null)))))))
  
   
(defun psql-COPY-val (val &key (delim-char #\tab) (null "\\N"))
  (cond
   ((null val)
    null)
   (t
    (coerce
     (loop
	 with str = (2-str val)
	 for char across str
	 for code = (char-code char)
	 when (= code 8)
	 collect #\\
	 and collect #\b
	 else when (= code 9)
	 collect #\\
	 and collect #\t
	 else when (= code 10)
	 collect #\\
	 and collect #\n
	 else when (= code 11)
	 collect #\\
	 and collect #\v
	 else when (= code 12)
	 collect #\\
	 and collect #\f
	 else when (= code 13)
	 collect #\\
	 and collect #\r
	 else when (eq char #\\)
	 collect #\\
	 and collect #\\
	 else when (eq char delim-char)
	 collect #\\
	 and collect delim-char
	 else	 
	 collect char)
     'string))))

;;;
  

(defmethod collect-psort-ids ((lex psql-lex-database) &key (cache t) (recurse t))
  (declare (ignore recurse))
  (with-slots (cache-lex-list) 
      lex
    (let ((lex-list cache-lex-list))
      (when (null cache-lex-list)
	(setf lex-list 
	  (collect-psort-ids-aux lex))
	(if (null lex-list)
	    (setf lex-list :empty))
	(if cache 
	    (setf cache-lex-list lex-list)))
      (case lex-list
	(:empty nil)
	(otherwise lex-list)))))

(defmethod collect-psort-ids-aux ((lex psql-lex-database))
  (let ((query-res 
	 (get-raw-records lex "SELECT DISTINCT name FROM lex")))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

;

(defmethod retrieve-all-records ((lex psql-lex-database) &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (get-records lex 
		 (format nil "SELECT ~a FROM lex"
			 (concat-str
			  (mapcar #'string reqd-fields)
			  :sep-c #\,))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

(defmethod retrieve-raw-record ((lex psql-lex-database) id &key (cache t) (reqd-fields '("*")))
  (with-slots (record-cache) lex
    (let ((hashed (gethash id record-cache)))
      (cond (hashed
	     (unless (eq hashed :EMPTY)
	       hashed))
	    (t
	     (let* ((record (retrieve-raw-record-no-cache lex id reqd-fields)))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-entry ((lex psql-lex-database) name &key (reqd-fields '("*")))
  (get-records lex
	       (format nil
		       "SELECT ~a FROM lex WHERE name LIKE ~a"
		       (fields-str reqd-fields)
		       (quote-literal lex name))))

(defmethod retrieve-raw-record-no-cache ((lex psql-lex-database) id &optional (reqd-fields '("*")))
  (cond 
   ((connection lex)
    (let* ((id-str (symb-2-str id))
	   (column-records 
	    (recs (retrieve-entry lex (sql-like-text id-str) :reqd-fields reqd-fields))))
      (if (> (length column-records) 1)
	  (error (format nil " too many records returned"))
	(first column-records))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

(defmethod retrieve-head-record-str ((lex psql-lex-database) id &optional (reqd-fields '("*")))
  (retrieve-head-record lex (str-2-symb id) reqd-fields))

(defmethod retrieve-head-record ((lex psql-lex-database) id &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((id-str (2-str id))
	   (table 
	    (retrieve-entry lex (sql-like-text id-str) :reqd-fields reqd-fields)))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

(defmethod retrieve-record-ium ((lex psql-lex-database) id name modstamp &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((table
	    (get-records lex
			 (format nil
				 "SELECT ~a FROM rev_all WHERE (name,userid,modstamp) = (~a,~a,~a)"
				 (fields-str reqd-fields)
				 (quote-literal lex id)
				 (quote-literal lex name)
				 (quote-literal lex modstamp)))))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

(defmethod grammar-fields ((lex psql-lex-database))
  (unless (dfn lex)
    (complain-no-dfn lex)
    (error "operation aborted"))
  (let ((g-fields
	 (remove-duplicates 
	  (mapcar #'second (dfn lex)))))
    (when (member :_tdl (fields lex))
      (pushnew :_tdl g-fields))
    g-fields))

;

(defmethod read-psort ((lex psql-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (with-slots (psorts) lex
    (let ((hashed (and (not new-instance) (gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed :EMPTY) hashed))
	    (t
	     (let ((entry (read-psort-aux lex id :cache cache)))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry :EMPTY)))
	       entry))))))

(defmethod read-psort-aux ((lex psql-lex-database) id &key (cache t))
  (with-slots (psorts) lex
    (let* ((cols (grammar-fields lex))
	   (raw-record 
	    (retrieve-raw-record 
	     lex id 
	     :reqd-fields cols
	     :cache cache)))
      (if raw-record 
	  (make-psort-struct lex raw-record cols)))))

(defmethod make-psort-struct ((lex psql-lex-database) raw-record cols)
  (apply #'make-lex-entry 
	 (make-strucargs lex raw-record cols)))

;; provide args to make-lex-entry
(defmethod make-strucargs ((lex psql-lex-database) raw-record cols)
  ;; make a-list with empty values
  (let* ((strucargs 
	 (mapcar #'(lambda (x) (list x)) 
		 (remove-duplicates (mapcar #'first (dfn *lexdb*))))))
    ;; instantiate values in a messy way
    ;; fix_me
    (loop 
	for (slot-key slot-field slot-path slot-type) in (dfn lex)
	for slot-value-list = (work-out-value slot-type 
					      (get-val slot-field raw-record cols)
					      :path (work-out-rawlst slot-path))
	when slot-value-list
	do 
	  (setf (cdr (assoc slot-key strucargs))
	    (append (cdr (assoc slot-key strucargs))
		    (mapcar #'(lambda (x) (make-strucargs-aux x slot-path)) 
			    slot-value-list))))
    ;; messy
    (let ((unifs (cdr (assoc :unifs strucargs)))
	  (id (cadr (assoc :id strucargs)))
	  (orth (cadr (assoc :orth strucargs))))
      ;; if using :_tdl field (undecomposed TDL) the raw tdl contributes to lex entry
      (when (member :_tdl (fields lex))
	(setf unifs 
	  (append unifs (tdl-to-unifs (get-val :_tdl raw-record cols)))))
      ;; finally, build the list of arguments
      (list :unifs unifs
	    :id id
	    :orth orth
	    :infl-pos (and (> (length orth) 1)
			   (find-infl-pos nil orth nil))))))

(defmethod record-to-tdl ((lex psql-lex-database) record)
  (let ((cols (mapcar #'car record))
	(rec (mapcar #'cdr record)))
    (raw-record-to-tdl lex rec cols)))
  
(defmethod raw-record-to-tdl ((lex psql-lex-database) rec cols)
  (to-tdl (make-psort-struct lex rec cols)))
  
;; lex is open
(defmethod load-lex-from-files ((lex psql-lex-database) file-names syntax)
  (setf *ordered-lex-list* nil) ;;fix_me
  (cond
   ((check-load-names file-names 'lexical)
    (let ((*lexicon-in* lex)) ;; *lexicon-in* is needed deep inside read-...-file-aux
      (dolist (file-name file-names)
	(ecase syntax
	  (:tdl (read-tdl-lex-file-aux-internal file-name))
	  (:path (read-lex-file-aux-internal file-name)))))
    t)
   (t
    (cerror "Continue" "Lexicon file not found")
    nil)))

;;;
;;; db filter
;;;

(defmethod set-filter ((lex psql-lex-database) &rest rest)  
  (let* ((old-filter (get-filter lex))
	 (filter
	  (cond
	   ((= (length rest) 0)
	    (ask-user-for-x "Alter filter" 
			    (cons "New filter?" 
				  old-filter)))
	   ((= (length rest) 1)
	    (first rest))
	   (t
	    (error "too many arguments")))))
    (when (catch :sql-error
	    (format t "~&(LexDB) recreating 'lex' table")
	    (force-output)
	    (unless (set-filter-aux lex filter)
	      (format t "~&(LexDB) filter unchanged")
	      (return-from set-filter))
	    nil)
      (lkb-beep)
      (set-filter lex))
    (format t "~&(LexDB) filter: ~a" 
	    (get-filter lex))
    (format t "~&(LexDB) active entries in 'lex' table: ~a" 
	    ;(sql-fn-get-val lex :size_lex)
	    (sql-get-val lex "SELECT count(*) FROM lex")
	    )))

(defmethod set-filter-aux ((lex psql-lex-database) filter)
  (unless (or (null filter) 
	      (equal (get-filter lex) filter))
    (reconnect lex) ;; must reconnect to avoid server bug...
    (force-output)
    (and
     (sql-fn-get-val lex 
		     :update_lex 
		     :args (list filter))
     (generate-missing-orthkeys lex))
    (empty-cache lex)))

;;;
;;; cache
;;;

(defmethod cache-all-lex-records ((lex psql-lex-database))
  (let* ((table (retrieve-all-records lex 
				      (grammar-fields lex)))
	 (recs (recs table))
	 (cols (cols table)))
    (with-slots (record-cache) lex
      (clrhash record-cache)
      (mapc
       #'(lambda (rec)  
	   (setf (gethash (record-id rec cols lex) record-cache) 
	     rec))
       recs))))

(defmethod cache-all-lex-records-orth ((lex psql-lex-database))
  (let ((table (retrieve-all-records lex (grammar-fields lex))))
    (with-slots (recs cols) table
      (with-slots (record-cache lexical-entries) lex
	;; clear cache
	(clrhash lexical-entries)
	(clrhash record-cache)
	;; for each record...
	(mapc
	 #'(lambda (rec)
	     (let* ((id (record-id rec cols lex))
		    (orth (record-orth rec cols lex)))
	       ;; update cache for each component word...
	       (mapc
		#'(lambda (y)
		    (setf (gethash y lexical-entries) 
		      (cons id (gethash y lexical-entries))))
		(split-into-words orth))
	       ;; update record cache
	       (setf (gethash id record-cache)
		 rec)))
	 recs)))))

(defmethod make-field-map-slot ((lex psql-lex-database))
  "stores the mapping of fields to lex-entry structure slots"
  (with-slots (dfn fields) lex
    (setf fields (get-fields lex))
    (setf dfn
      (sort
       (mapcar #'(lambda (x) 
		   (let* ((slot (str-2-keyword (first x)))
			  (field (str-2-keyword (second x)))
			  (path (third x))
			  (type2 (2-symb-or-list (fourth x)))
			  (type (if (listp type2) type2 (list type2))))
		     ;; correct any obsolete types
		     (setf (car type)
		       (or (cdr (assoc (car type) *lexdb-fmtype-alt*))
			   (car type)))
		     (list slot field path type)))
	       (get-raw-records lex (format nil "SELECT slot,field,path,type FROM dfn WHERE mode='~a' OR mode IS NULL" (fields-tb lex))))
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :unifs))))
    (if (null dfn)
	(complain-no-dfn lex))
    dfn))

(defmethod complain-no-dfn ((lex psql-lex-database))
  (format t "~&(LexDB) no dfn entries found in ~a" (dbname lex)))

(defmethod get-internal-table-dfn ((lex psql-lex-database))
  (get-field-info lex "public" "rev"))  

(defmethod get-field-size-map ((lex psql-lex-database))
  (let* ((table (get-internal-table-dfn lex))
	(recs (recs table))
	(cols (cols table)))
    (mapcar 
     #'(lambda (x) (field-size-elt x cols)) 
     recs)))

(defmethod lookup ((lex psql-lex-database) field-kw val-str &key (ret-flds "*") (from "lex"))
  (cond
   (val-str
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM ~a WHERE ~a ILIKE ~a"
			     ret-flds from
			     (quote-ident lex field-kw)
			      (quote-literal lex val-str))))
   (t
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM ~a WHERE ~a IS NULL"
			     ret-flds from
			     (quote-ident lex field-kw))))))

(defmethod lookup-rev-all ((lex psql-lex-database) field-kw val-str &key (ret-flds "*"))
  (lookup lex field-kw val-str :ret-flds ret-flds :from "rev_all"))

;;

(defmethod list-fld ((lex psql-lex-database))
  (mapcar #'car
	  (recs 
	   (get-field-info2 lex "public" "rev"))))

(defmethod complete ((lex psql-lex-database) field-kw val-str)
  (let ((qi-field (quote-ident lex (symb-2-str field-kw)))
	(ql-val (quote-literal lex (format nil "~a%" val-str))))
    (mapcar #'car
	    (get-raw-records lex
			     (format nil
				     "SELECT DISTINCT ~a AS field FROM lex WHERE ~a ILIKE ~a"
				     qi-field qi-field ql-val)))))

;; called from pg-interface
(defmethod new-entries ((lex psql-lex-database))
  (let ((records (get-raw-records lex
				  (format nil "SELECT userid,name,modstamp FROM public.tmp"))))
    (cons (list "userid" "name" "modstamp") records)))

(defmethod current-timestamp ((lex psql-lex-database))
  (caar (get-raw-records *lexdb* "SELECT current_timestamp")))

;;;
;;; low-level
;;;

(defmethod set-lex-entry-from-record ((lex psql-lex-database) fv-pairs)
  (set-lex-entry lex
		 (make-instance 'psql-lex-entry :fv-pairs fv-pairs)))

;;; insert lex entry into db
(defmethod set-lex-entry ((lex psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :orthkey (lexicon-le-orthkey lex psql-le))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid (user lex))
  (set-lex-entry-aux lex psql-le))
  
(defmethod set-lex-entry-aux ((lex psql-lex-database) (psql-le psql-lex-entry))
  (let* ((symb-list (copy-list (fields lex)))
	 (symb-list (remove :name symb-list))
	 (symb-list (remove-duplicates symb-list))
	 (symb-list (remove-if 
		     #'(lambda (x) (or (null x) 
				       (and (stringp x)
					    (string= x ""))))
		     symb-list
		     :key #'(lambda (x) (retr-val psql-le x))))
	 (name (retr-val psql-le :name)))
    (unless (string= name (symb-2-str (str-2-symb name)))
      (format t "(LexDB) WARNING: lex id ~a should be written ~a" 
	      name (symb-2-str (str-2-symb name)))
      (lkb-beep)
      (setf name (symb-2-str (str-2-symb name))))
    (sql-fn-get-val lex :update_entry
		    :args (list name
				symb-list
				(ordered-val-list symb-list psql-le))) ;; tmp contains new entry only
    (generate-missing-orthkeys lex :from :tmp) ;; use new entry stored in tmp
    (unless
	(check-lex-entry (str-2-symb name)
			 lex)
      (error "Invalid lexical entry ~a -- see Lisp buffer output" (retr-val psql-le :name)))))

#+:mwe
(defmethod mwe-initialize-lex ((lex psql-lex-database))
  (mwe-initialize-userschema lex))

#+:mwe
(defmethod mwe-initialize-userschema ((lex psql-lex-database))
  (format t "~%(mwe initializing private schema)")
  (sql-fn-get-records lex 
		      :mwe_initialize_schema))

;;;
;;; postgres interface
;;;

#+:mwe
(defmethod mwe-retrieve-id-set ((lex psql-lex-database))
  (mapcar
   #'(lambda (x) (str-2-symb (car x))) 
   (sql-fn-get-raw-records lex 
			   :mwe_retrieve_id_set)))

#+:mwe
(defmethod mwe-retrieve-type ((lex psql-lex-database) mwe-id)
  (str-2-symb 
   (sql-fn-get-val lex 
			   :mwe_retrieve_type (symb-2-str mwe-id))))

#+:mwe
(defmethod mwe-retrieve-keyrels ((lex psql-lex-database) mwe-id)
  (let* ((raw-results 
	  (sql-fn-get-records lex 
			      :mwe_retrieve_keyrels (symb-2-str mwe-id)))
	 (s (make-sequence 'vector (length raw-results))))
    (mapcar #'(lambda (x) 
		(setf (aref s (1- (str-2-num (get-assoc-val :slot x))))
		  (str-to-mixed (get-assoc-val :keyrel x))))
	    raw-results
	    )))

#+:mwe
(defmethod dump-multi-db ((lex psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (sql-fn-get-records lex 
		      :dump_multi_db filename))

#+:mwe
(defmethod merge-multi-into-db ((lex psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (sql-fn-get-records lex 
		      :merge_multi_into_db filename))

#+:mwe
(defmethod mwe-to-tdl ((lex psql-lex-database) mwe-id)
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str mwe-id)
   (p-2-tdl (mwe-build-P-list (mwe-retrieve-type lex mwe-id)
			      (mapcar #'(lambda (x) (cons 'PRED (list (list x))))
				      (mwe-retrieve-keyrels lex mwe-id))))))
	     
;;;
;;; script file fn
;;;

#+:mwe
(defmethod mwe-read-roots ((lex psql-lex-database))
  (format  t "~%Loading MWE roots from lexical database ~a" (dbname lex))
  (let ((*readtable* (make-tdl-break-table)))
    (mapcar 
     #'(lambda (x)
	 (with-input-from-string (istream (mwe-to-tdl lex x))
	   (read-tdl-psort-stream istream :root)))
     (mwe-retrieve-id-set lex)))
  (finalize-psort-file :root))

#+:mwe
(defmethod mwe-read-root-entry ((lex psql-lex-database) mwe-id)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream (mwe-to-tdl lex mwe-id))
      (read-tdl-psort-stream istream :root))))

#+:mwe
(defmethod reload-roots-mwe ((lex psql-lex-database))
    (mwe-read-roots lex)
    (format  t "~%MWE roots reloaded from lexical database ~a" (dbname lex)))

(defmethod close-lex ((lex psql-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (lexdb-version semi dbname host user connection) lex
    (setf lexdb-version nil)
    (if (next-method-p) (call-next-method))))

(defmethod open-lex ((lex psql-lex-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (dbname host user connection) lex
    (close-lex lex)    
    (force-output)
    (setf (name lex) name)
    (or (open-lex-aux lex)
	(format t "~&unable to open connection to lexical database ~a(~a)@~a:~a (~a)" 
		dbname user host (true-port lex)
		(error-msg connection)))))

(defmethod open-lex-aux ((lex psql-lex-database)) 
  (with-slots (dbname host user) 
      lex
    (when (connect lex)
      (check-psql-server-version lex)
      (check-lexdb-version lex)
      (make-field-map-slot lex)
      (initialize-userschema lex)
      lex)))

(defmethod check-lexdb-version ((lex psql-lex-database))
  (with-slots (lexdb-version) 
      lex
    (cond
     ((not (stringp lexdb-version))
      (error "Unable to determine LexDB version"))
     ((string> (compat-version lexdb-version)
	       *lexdb-major-version*)
      (error *lexdb-message-old-lkb* lexdb-version *lexdb-major-version*))
     ((string< (compat-version lexdb-version)
	       *lexdb-major-version*)
      (error *lexdb-message-old-lexdb* lexdb-version *lexdb-major-version*)))))

(defmethod check-psql-server-version ((lex psql-lex-database))
  (let* ((supported
	  (mapcar #'car
		  (get-raw-records lex "SELECT val FROM public.meta WHERE var='supported-psql-server'")))
	 (actual-full (caar (get-raw-records lex "SELECT split_part((SELECT version()),' ',2)")))
	 (actual-full-l (string-2-str-list actual-full :sep #\.))
	 (actual (concatenate 'string 
		   (nth 0 actual-full-l)
		   "."
		   (nth 1 actual-full-l))))
    (unless (member actual supported :test #'string=)
      (format t "~&(LexDB) WARNING: Unsupported PSQL server version ~a.~% Supported versions are: ~a)" actual supported))))

(defmethod initialize-lex ((lex psql-lex-database))
  (when (open-lex lex)
    (with-slots (dbname user host) lex
        (format t "~&(LexDB) connected to LexDB ~a@~a:~a as database user ~a" 
		dbname host (true-port lex) user))
    (update-lex lex)))
  
(defmethod connect ((lex psql-lex-database)) 
  (if (next-method-p) (call-next-method))
  (when (connection-ok lex)
    (setf (lexdb-version lex) 
      (get-db-version lex))
      (get-pub-fns lex)
    t))	

(defmethod get-pub-fns ((lex psql-lex-database)) 
  (setf (pub-fns lex)
    (mapcar #'(lambda (x)
		(str-2-keyword (car x)))
	    (get-raw-records lex "SELECT val FROM public.meta WHERE var='pub-fn'"))))

;;;
;;;
;;;

(defmethod get-db-version ((lex psql-lex-database))
  (caar 
   (get-raw-records lex 
		    "SELECT val FROM public.meta WHERE var='lexdb-version' LIMIT 1")))
    
(defmethod get-filter ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))  

(defmethod update-lex ((lex psql-lex-database))
  (unless (quick-load lex)
    (update-lex-aux lex))
  (cond
   ((null (semi lex))
    nil)
   ((semi-up-to-date-p lex)
    (unless 
	    (lexdb-time ("loading SEM-I into memory" "done loading SEM-I into memory")
			(mrs::semi-p 
			 (catch :sql-error
			   (mrs::populate-*semi*-from-psql)
			   )))
      (format t "~&(LexDB) unable to retrieve database SEM-I"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~&(LexDB) WARNING:  no lexical entries indexed for generator")))
  lex)

(defmethod update-lex-aux ((lex psql-lex-database))
  (reconnect lex) ;; work around server bug

  (let (vac)
    (if (lexdb-time 
	 ("ensuring 'lex' table is up-to-date" "done ensuring 'lex' table is up-to-date")
	 (string= "t" (sql-fn-get-val lex :update_lex :args (list (get-filter lex)))))
	(setf vac t))
    (if (generate-missing-orthkeys lex)
	(setf vac t))
    (if vac (vacuum lex)))
	
  (format t "~&(LexDB) filter: ~a " (get-filter lex))
  (let ((size
	 (sql-get-val lex "SELECT count(*) FROM lex")))
    (if (string= "0" size)
	(format t "~&(LexDB) WARNING:  0 entries passed the LexDB filter" size)
      (format t "~&(LexDB) active entries in 'lex' table: ~a" size)))
  (empty-cache lex))

;;;
;; semi
;;;

(defmethod index-new-lex-entries ((lex psql-lex-database))
  (let ((semi-out-of-date (semi-out-of-date lex)))
    (format t "~&(LexDB) indexing ~a entries" (length semi-out-of-date))
    (when semi-out-of-date
      (mrs::populate-*semi*-from-psql)
      (index-lexical-rules)
      (index-grammar-rules)
      (mapc 
       #'(lambda (x)
	   (update-semi-entry lex x))
       semi-out-of-date)
      (mrs::dump-*semi*-to-psql))))
  
(defmethod update-semi-entry ((lex psql-lex-database) lexid)
  (let* ((entry (read-psort lex lexid :cache nil))
	 (new-fs (and
		  (expand-psort-entry entry)
		  (lex-entry-full-fs entry))))
    (if (and new-fs 
	     (not (eq new-fs :fail)))
	(mrs::extract-lexical-relations entry)
      (format t "~&No feature structure for ~A~%" 
	      (lex-entry-id entry))))
    (forget-psort lex lexid))

;;;
;;;
;;;

(defmethod get-fields ((lex psql-lex-database))
  (mapcar 
   #'(lambda (x) (intern (string-upcase x) :keyword))
   (list-fld lex)))

(defmethod show-scratch ((lex psql-lex-database))
  (get-raw-records lex "SELECT name,userid,modstamp FROM rev"))

(defmethod scratch-records ((lex psql-lex-database))
  (get-raw-records lex "SELECT * FROM rev"))

(defmethod merge-rev ((lex psql-lex-database) rev-filename)
  ;; empty temporary tables
  (run-command lex "DELETE FROM tmp")
  (with-lexdb-user-lexdb (lex2 lex)
    ;; vacuum at start
    (vacuum lex2)
    (let (count-new)
      ;;;
      ;; populate temporary tables
      ;;;
      (lexdb-time ("populating temporary tables" "done populating temporary tables")
		  (run-command-stdin-from-file lex "COPY tmp FROM stdin" rev-filename))
      ;;;
      ;; update main tables
      ;;;
      (lexdb-time ("copying into 'rev' table" "done copying into 'rev' table")
		  (sql-fn-get-val lex :merge_public_rev_from_tmp)    
		  (setf count-new (read-from-string (caar (get-raw-records *lexdb* "SELECT count(*) from public.tmp")))))
      
      (format t "~&(LexDB) ~a new 'rev' entries" count-new)
      (make-field-map-slot lex2)
      ;; vacuum at end
      (vacuum lex2)
      count-new)))

(defmethod merge-dfn ((lex psql-lex-database) dfn-filename)  
  (when (catch :sql-error 
	  (run-command lex "CREATE TABLE tmp_dfn AS SELECT * FROM dfn WHERE NULL;"))
    (run-command lex "DROP TABLE tmp_dfn")
    (run-command lex "CREATE TABLE tmp_dfn AS SELECT * FROM dfn WHERE NULL ;"))
  (run-command-stdin-from-file lex 
			       "COPY tmp_dfn FROM stdin" 
			       dfn-filename)
  (let ((count-new-dfn 
	 (str-2-num 
	  (sql-fn-get-val lex :merge_dfn_from_tmp_dfn))))
    (run-command lex "DROP TABLE tmp_dfn")
    (format t "~&(LexDB) ~a new dfn entries" count-new-dfn)
    count-new-dfn))

(defmethod initialize-userschema ((lex psql-lex-database))
  (sql-fn-get-val lex :initialize_user_schema )
  #+:mwe
  (if *postgres-mwe-enable*
      (mwe-initialize-userschema lex)))

(defmethod semi-setup-pre ((lex psql-lex-database))  
  (reconnect lex)
  (run-command lex "SELECT semi_drop_indices(); DELETE FROM semi_pred; DELETE FROM semi_frame; DELETE FROM semi_var; DELETE FROM semi_extra; DELETE FROM semi_mod;"))
  
(defmethod semi-setup-post ((lex psql-lex-database))  
  (reconnect lex)
  (run-command lex "SELECT semi_create_indices(); INSERT INTO semi_mod (SELECT DISTINCT name,userid,lex.modstamp,CURRENT_TIMESTAMP FROM lex JOIN semi_pred ON name=lex_id); SET ENABLE_HASHJOIN TO false;"))
 
(defmethod semi-up-to-date-p ((lex psql-lex-database))  
  (string= "t"
	   (sql-get-val lex "SELECT mod_time() < (SELECT min(modstamp0) FROM semi_mod )")))
  
;; returns record-ids
(defmethod semi-out-of-date ((lex psql-lex-database))
  (with-slots (record-cache) lex
    (let* ((cols (grammar-fields lex))
	   (table 
	    (get-records lex 
			 (format nil
				 "SELECT ~a FROM (SELECT g.* FROM lex as g NATURAL LEFT JOIN semi_mod as s WHERE g.modstamp > COALESCE(s.modstamp0,\'-infinity\')) AS foo"
				 (fields-str cols))))
	   (recs (recs table)))
      ;; cache records
      (mapcar
       #'(lambda (rec)
	   (let ((rec-id (record-id rec cols lex)))
	     (setf (gethash rec-id record-cache) rec)
	     rec-id))
	   recs))))

(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    
(defmethod get-value-set ((lex psql-lex-database) field)
  (let ((qi-field (quote-ident lex (2-str field))))
    (mapcar #'car
	    (get-raw-records lex
			     (format nil "SELECT DISTINCT ~a::text AS foo FROM rev_all WHERE ~a IS NOT NULL"
				     qi-field qi-field)))))

(defun fields-str (fields)
  (concat-str
   (mapcar #'string fields)
   :sep-c #\,))

;; return sql code to call db function and return
;; appropriate fields
(defmethod sql-fn-string ((lex psql-lex-database) fn &key args fields)
  (with-slots (lexdb-version pub-fns) lex
    (when (not (member fn pub-fns))
      (error "~a not `LexDB external function'" fn))
    (unless fields
      (setf fields '(:*)))
    (let ((fields-str (concat-str
		       (mapcar #'string fields)
		       :sep-c #\,))
	  (fn-str (string fn))
	  (args-str 
	   (concat-str
	    (mapcar #'sql-fn-arg args)
	    :sep-c #\,
	    ;:esc nil
	    )))
      (format nil "SELECT ~a FROM ~a(~a)" fields-str fn-str args-str))))

(defun concat-str (str-list &key (sep-c #\Space))
  (unless (listp str-list)
    (error "list expected"))
  (let ((sep (string sep-c)))
    (cond
     ((null str-list) "")
     (t (apply 'concatenate
	       (cons
		'string
		(cons
		 (pop str-list)
		 (mapcan #'(lambda (x) (list sep
					     (if x 
						 x
					       "")
					     ))
			 str-list))))))))

(defun sql-fn-arg (x)
  (cond
   ((stringp x)
    (sql-embedded-text x))
   ((listp x)
    (sql-embedded-text
     (concat-str
      (mapcar
       #'sql-fn-arg
       x)
      :sep-c #\,)))
   (t
    (2-str x))))

(defmethod sql-fn-get-records ((lex psql-lex-database) fn &key args fields)
  (get-records lex
	       (sql-fn-string lex fn :args args :fields fields)))

;;;
;;; this approach is not right
;;;
(defmethod sql-fn-get-records-union ((lex psql-lex-database) fn &key list-args fields)
  (when list-args
    (let* ((first-arg (pop list-args))
	   (sql-str
	    (apply #'concatenate 'string
		   (cons (sql-fn-string lex fn 
					:args first-arg
					:fields (cons (sql-fn-arg first-arg) fields))
			 (mapcan #'(lambda (x) (list " UNION " 
						     (sql-fn-string lex fn
								    :args x
								    :fields (cons (sql-fn-arg x) fields))))
				 list-args)))))
      (get-records lex sql-str))))

(defmethod sql-fn-get-raw-records ((lex psql-lex-database) fn &key args fields)
  (get-raw-records lex
		   (sql-fn-string lex fn :args args :fields fields)))
  
(defmethod sql-fn-get-val ((lex psql-lex-database) fn &key args fields)
  (caar (sql-fn-get-raw-records lex fn :args args :fields fields)))
  
(defmethod sql-fn-get-vals ((lex psql-lex-database) fn &key args fields)
  (mapcar #'car
	  (sql-fn-get-raw-records lex fn :args args :fields fields)))
  
;;
;;
;;

(defmethod merge-into-lexdb ((lex psql-lex-database) filename)
  "connect as db owner and merge new data into lexdb"
  (with-lexdb-user-lexdb (lex2 lex)
    (let* ((rev-filename (absolute-namestring "~a.rev" filename))
	   (dfn-filename (absolute-namestring "~a.dfn" filename))
	   count-new)
      ;; dfn table
      (cond
       ((probe-file dfn-filename)
	(merge-dfn lex2 dfn-filename)
	(make-field-map-slot lex))
       (t
	(format t "~&(LexDB) WARNING:  cannot find file ~a" dfn-filename)))
      ;; rev table
      (cond
       ((probe-file rev-filename)
	(merge-rev lex2 rev-filename)
	(setf count-new (read-from-string (caar (get-raw-records *lexdb* "SELECT count(*) from public.tmp")))))
       (t
	(format t "~&(LexDB) WARNING:  cannot find file ~a" rev-filename)))
      (if (or (null count-new) (equal count-new 0))
	  (empty-cache lex)
	(update-lex-aux lex))
      )))

(defmethod vacuum ((lex psql-lex-database))
  (let (time client-min-messages)
    (format t "~&(LexDB) performing vacuum/analyze on database (as user ~a)..." (user lex))
    (force-output)
    (setf time (get-internal-real-time))
    (setf client-min-messages 
      (caar (recs (get-records *lexdb* "show client_min_messages;"))))
    (run-command lex "set client_min_messages to error") 
    (run-command lex "vacuum full analyze")
    (run-command lex (format nil
			     "set client_min_messages to ~a" 
			     client-min-messages))
    (format t "~&(LexDB) vacuum/analyze complete [~F sec]" 
	    (/ (- (get-internal-real-time) time) internal-time-units-per-second))))

(defmethod db-owner ((lex psql-lex-database))
  (let* ((uid (sql-get-val lex "SELECT datdba FROM pg_catalog.pg_database WHERE datname=current_database()"))
	 (uname (sql-get-val lex 
			     (concatenate 'string
			       "SELECT usename FROM pg_catalog.pg_user WHERE usesysid=" (quote-literal lex uid)))))
    uname))

(defmethod merge-into-lexicon-dfn ((lex psql-lex-database) filename)
  "reconnect as db owner and merge new dfn into lexdb"
  (with-slots (dbname host port) lex
    (unless dbname
      (error "please set :dbname"))
    (let ((conn-db-owner 
	   (make-instance 'psql-lex-database
	     :dbname dbname
	     :host host
	     :port port
	     :user (db-owner lex))))
      (connect conn-db-owner)
      (when
	  (catch :sql-error
	    (progn
	      (let ((dfn-filename 
		     (absolute-namestring "~a" filename)))
		(cond
		 ((probe-file dfn-filename)
		  (merge-dfn conn-db-owner 
			      dfn-filename)
		  (make-field-map-slot lex))
		 (t
		  (format t "~%WARNING: no file ~a" dfn-filename)))
		nil
		)))
	(format t "~&(LexDB) merge new dfn entries aborted ..."))
      (empty-cache lex)
      (disconnect conn-db-owner))))

(defmethod get-field-vals ((x lex-entry) (lex psql-lex-database))
  (with-slots (dfn) lex
    (let* (;; copy slots as we will destructively delete during processing
	   (s (copy-slots x dfn))
	   ;; perhaps should warn about duplicates?
	   (extraction-fields 
	    (remove :_tdl
		    (remove-duplicates
		     (cons :name (grammar-fields lex)))))
	   ;; extract field values from tdl structure 
	   ;; and remove unifs as they are found
	   (extraction-field-vals (mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x dfn)))
			 extraction-fields))
	   ;; convert any remaining unifs into raw tdl fragment
	   (skip (unifs-to-tdl-body (cdr (assoc :unifs s))))
	   (skip (if (string= skip "") nil skip))
	   ;; necessary fields
	   (hard-coded-field-vals (list
				   (cons :userid *lexdb-dump-user*)
				   (cons :modstamp *lexdb-dump-timestamp*)
				   (cons :dead "f")
				   (cons :_tdl skip)))
	   ;; additional (useful) fields
	   ;; if not all fields occur in LexDB they will be silently ignored
	   (other-field-vals (list
			      (cons :lang *lexdb-dump-lang*)
			      (cons :country *lexdb-dump-country*)
			      (cons :confidence 1)
			      (cons :source *lexdb-dump-source*)))
	   ;; combine all field values
	   (field-vals (append extraction-field-vals
			       hard-coded-field-vals
			       other-field-vals)))
      ;; construct ordered a-list of field values
      ;; fields not in LexDB silently ignored
      field-vals)))
  

(defmethod to-db-dump-rev ((x lex-entry) (lex psql-lex-database) &key (skip-stream t))
  "provide line entry for lex db import file"
  (let* (;;ordered a-list of field values
	 (field-vals (get-field-vals x lex))
	 (skip (cdr (assoc :_tdl field-vals)))
	 (name (cdr (assoc :name field-vals)))
	 (ordered-field-vals (ordered-symb-val-list (fields lex) field-vals))
	 ;; construct CVS copy line
	 (line 
	  (format nil "~a~%" 
		  (str-list-2-line
		   (mapcar
		    #'(lambda (x)
			(let ((val (cdr x)))
			  (if val
			      (2-str val)
			    nil)))
		    ordered-field-vals)
		   :sep-c #\tab
		   :null-str "\\N"))))
    (cond
     ;; no components of lex entry skipped
     ((null skip)
      line)
     ;; component(s) skipped, but :skip field available in db
     ((member :_tdl (fields lex))
      (format t "~&(LexDB) Unhandled TDL fragment in lexical entry ~a: ~%~t~a~%~%" name skip)
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
	line)
     ;; component(s) skipped and no :skip field in db
     (t
      (format t "~&~%(LexDB) Lex entry ~a skipped due to unhandled TDL fragment: ~%~t~a~%" name skip)
      (format skip-stream "~a" (to-tdl x))
      ""))))

(defmethod to-db ((x lex-entry) (lex psql-lex-database))
  "insert lex-entry into lex db (user scratch space)"
  (let* (;;ordered a-list of field values
	 (field-vals (get-field-vals x lex))
	 (skip (cdr (assoc :_tdl field-vals)))
	 (name (cdr (assoc :name field-vals)))
	 (ordered-field-vals (ordered-symb-val-list (fields lex) field-vals))
	 (psql-le (make-instance 'psql-lex-entry :fv-pairs ordered-field-vals)))	 
    (cond
     ;; no components of lex entry skipped
     ((null skip)
      (set-lex-entry lex psql-le)
      (empty-cache lex))
     ;; component(s) skipped, but :skip field available in db
     ((member :_tdl (fields lex))
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
      (set-lex-entry lex psql-le)
      (empty-cache lex))
     ;; component(s) skipped and no :skip field in db
     (t
      (format t "~&~%(LexDB) Lex entry ~a skipped due to unhandled TDL fragment: ~%~t~a~%" name skip)
      nil))))

;; NOTE: not suited to batch import
;; import lex to LexDB
(defmethod export-to-db ((lex lex-database) (lexdb psql-lex-database))
  (mapc
   #'(lambda (x) 
       (to-db (read-psort lex x :recurse nil :new-instance t) 
	      lexdb))
   (collect-psort-ids lex :recurse nil))
  (update-lex-aux lexdb))

(defmethod record-id (raw-record cols (lex psql-lex-database))
  (str-2-symb (get-val :name raw-record cols)))  

(defmethod record-orth (raw-record cols (lex psql-lex-database))
  (get-val (second (assoc :orth (dfn lex))) raw-record cols))

;;
;;
;;

(defmethod dump-lexdb ((lexdb psql-lex-database) filebase &key tdl)
  (format t "~%(dumping LexDB)")
  (force-output)
  (dump-rev lexdb filebase)
  (dump-dfn lexdb filebase)
  (dump-fld lexdb filebase)
  (dump-meta lexdb filebase)
  (when tdl (dump-tdl lexdb filebase))
  t)

(defmethod dump-rev ((lex psql-lex-database) filebase)
  (sql-fn-get-val lex :dump_public_rev_to_tmp)
  (run-command-stdout-to-file lex "COPY tmp TO stdout" 
			      (namestring (pathname (format nil "~a.rev" 
							    filebase))))
  t)

(defmethod dump-dfn ((lexdb psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.dfn TO stdout" 
			      (namestring (pathname (format nil "~a.dfn" 
							    filebase))))
  t)

(defmethod dump-fld ((lexdb psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.fld TO stdout" 
			      (namestring (pathname (format nil "~a.fld" 
							    filebase))))
  t)

(defmethod dump-meta ((lexdb psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.meta TO stdout" 
			      (namestring (pathname (format nil "~a.meta" 
							    filebase))))
  t)

(defmethod dump-tdl ((lexdb psql-lex-database) filebase)
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (get-filter lexdb))))))
    (format t "~&(LexDB) exporting filtered ~a LexDB to TDL file ~a" (dbname lexdb) tdl-file)
    (force-output)
    (export-to-tdl-to-file lexdb tdl-file)))

(defmethod commit-private-rev ((lex psql-lex-database)) 
  ;; insert into public.rev and register public modtime
  (let* ((qi-user (quote-ident lex (user lex)))
	 (new-rev-sql 
	  (format nil "SELECT * FROM ~a.rev WHERE (name,modstamp) IN (SELECT name, max(modstamp) AS modstamp FROM ~a.rev GROUP BY name)" qi-user qi-user)))
    (with-lexdb-user-lexdb (lex-public lex)
      (format t "~%(LexDB) updating 'public.rev' with ~a new entries from private 'rev'"
	      (sql-get-val lex (format nil "SELECT count(*) FROM (~a) AS a" new-rev-sql)))
      (run-command lex-public 
					;     (format nil "INSERT INTO public.rev (SELECT * FROM ~a.rev)" 
		   (format nil "INSERT INTO public.rev ~a" new-rev-sql ))
      (cond
       ;; lex was up-to-date, and remains so
       ((and
	 (string> (sql-get-val lex "SELECT val FROM meta WHERE var='build_time'")
		  (sql-get-val lex "SELECT val FROM public.meta WHERE var='mod_time'"))
	 (string> (sql-get-val lex "SELECT val FROM meta WHERE var='build_time'")
		  (sql-get-val lex "SELECT val FROM meta WHERE var='mod_time'")))
	(get-raw-records lex-public "SELECT register_mod_time()")
	(run-command lex "DELETE FROM rev")
	(get-raw-records lex "SELECT register_mod_time()")
	(get-raw-records lex "SELECT register_build_time()"))
       ;; lex was not up to date
       (t
	(get-raw-records lex-public "SELECT register_mod_time()")
	(run-command lex "DELETE FROM rev")
	(get-raw-records lex "SELECT register_mod_time()"))))
    (empty-cache lex)))

(defmethod table-size ((lex psql-lex-database) table)
  (str-2-num 
   (sql-get-val *lexdb* 
		(format nil "SELECT count(*) FROM ~a" table))))

(defmethod table-head-count ((lex psql-lex-database) table)
  (str-2-num 
   (sql-get-val *lexdb* 
		(format nil "SELECT count(*) FROM ~a WHERE (name,modstamp) IN (SELECT name, max(modstamp) AS modstamp FROM ~a GROUP BY name)" table table))))


(defmethod clear-private-rev ((lex psql-lex-database))
  (unless (> (table-size lex :rev)
	     0)
    (format t "~%(LexDB) private 'rev' is already empty")
    (return-from clear-private-rev nil))
  
   (empty-cache lex)
   
   ;; store names of deleted revisions
   (run-command lex "CREATE TABLE tmp_name (name TEXT, PRIMARY KEY (name))")
   (run-command lex "INSERT INTO tmp_name SELECT DISTINCT name FROM rev")
   
   ;; delete rev entries
   (run-command lex "DELETE FROM rev")

   ;; store filtered revisions with name in above
   (run-command lex "DELETE FROM filt_tmp")
   (run-command lex (format nil "INSERT INTO filt_tmp SELECT * FROM rev_all WHERE name IN (SELECT name FROM tmp_name) AND ~a" (get-filter lex)))
   
   ;; delete potentially affected lex/lex_key entries
   (run-command lex "DELETE FROM lex WHERE name IN (SELECT name FROM tmp_name)")
   (run-command lex "DELETE FROM lex_key WHERE name IN (SELECT name FROM tmp_name)")
   ;;(sql-get-val lex "SELECT public.deindex_lex()") ;;generally quicker not to bother
   (run-command lex "DROP TABLE tmp_name")
   
   ;; update lex entries
   (lexdb-time 
    ("updating 'lex' table" "done updating 'lex' table")
    (run-command lex "INSERT INTO lex SELECT fil.* FROM (filt_tmp AS fil NATURAL JOIN (SELECT name, max(modstamp) AS modstamp FROM filt_tmp GROUP BY name) AS t1) WHERE dead=\'0\'")
    )
   ;;(sql-get-val lex "SELECT public.index_lex()") ;;generally quicker not to bother
   
   (reconnect lex) ;; work around server bug
   ;; update lex_key entries
   (generate-missing-orthkeys lex))
