(in-package :lkb)

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (setf orth 
    (string-downcase orth))
  (let ((hashed 
	 (gethash 
	  orth 
	  (slot-value 
	   lexicon 
	   'lexical-entries))))
    (cond 
     (hashed
      (if (eq hashed 
	      :EMPTY)
	  (setf hashed 
	    nil))
      hashed)
     (t 
      (let ((value 
	     (lookup-word-psql-lex-database lexicon orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth 
			 (slot-value lexicon 
				     'lexical-entries)) 
	    (if value 
		value 
	      :EMPTY)))
	value)))))

(defun lookup-word-psql-lex-database (lexicon orth)
  (declare (ignore cache))
  (if (connection lexicon)
       (let* ((orthstr orth)
	      (sql-str 
	       (sql-retrieve-entries-by-orthkey 
		lexicon 
		(make-requested-fields lexicon) 
		orthstr))
	      (query-res 
	       (run-query 
		lexicon 
		(make-instance 
		    'sql-query 
		  :sql-string sql-str)))
	      (ids 
	       (lookup-word-aux 
		query-res 
		lexicon)))
	 ids)))

;;; (used to index for generator)
;;; fix_me: inefficient implementation
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((sql-str (sql-orthography-set lexicon))
         (query-res (run-query 
                     lexicon 
                     (make-instance 
			 'sql-query 
		       :sql-string sql-str))))
    (mapcan 
     #'(lambda (x) 
	 (split-into-words 
	  (string-upcase 
	   (car x))))
     (records query-res))))

(defmethod collect-psort-ids ((lexicon psql-lex-database) &key (cache t) (recurse t))
  (declare (ignore recurse))
  (with-slots (cache-lex-list) 
      lexicon
    (let ((lex-list cache-lex-list))
      (when (null cache-lex-list)
	(setf lex-list 
	  (collect-psort-ids-aux lexicon))
	(if (null lex-list)
	    (setf lex-list :empty))
	(if cache 
	    (setf cache-lex-list lex-list)))
      (case lex-list
	(:empty nil)
	(otherwise lex-list)))))

(defmethod collect-psort-ids-aux ((lexicon psql-lex-database))
  (let* ((sql-str (sql-lex-id-set lexicon))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str))))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     (records query-res))))

(defmethod retrieve-all-records ((lexicon psql-lex-database) &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-all-records nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (sql-retrieve-all-entries 
	   lexicon 
	   reqd-fields))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str))))
    (when (records query-res) 
      (make-column-map-record query-res))))

(defmethod retrieve-record ((lexicon psql-lex-database) id &key (cache t) reqd-fields)
  (with-slots (record-cache) lexicon
    (let ((hashed (gethash id record-cache)))
      (cond (hashed
	     (unless (eq hashed 
			 :EMPTY)
	       hashed))
	    (t
	     (let* ((record 
		     (retrieve-record-str 
		      lexicon 
		      (symb-2-str id) 
		      reqd-fields)))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-record-str 
    ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-record-str nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (sql-retrieve-entry 
	   lexicon 
	   reqd-fields 
	   id-str))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str)))
	 (records 
	  (when (records query-res) 
	    (make-column-map-record query-res))))
    (if (> (length records) 
	   1)
	(error (format nil "database error (too many records returned)"))
      (car records))))

(defmethod retrieve-head-record-str 
    ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-head-record-str nil))
  (unless reqd-fields 
    (setf reqd-fields "*"))
  (let* ((sql-str 
	  (fn 
	   lexicon 
	   'retrieve-head-entry 
	   reqd-fields 
	   id-str))
	 (query-res 
	  (run-query 
	   lexicon 
	   (make-instance 
	       'sql-query 
	     :sql-string sql-str)))
	 (records (when (records query-res) 
		    (make-column-map-record query-res))))
    (if (> (length records) 
	   1)
	(error (format nil "database error (too many records returned)"))
      (car records))))

(defmethod read-psort 
    ((lexicon psql-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (with-slots (psorts) lexicon
    (let ((hashed 
	   (and (not new-instance)
		(gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed 
			 :EMPTY)
	       hashed))
	    (t
	     (let* ((record 
		     (retrieve-record 
		      lexicon id 
		      :reqd-fields (make-requested-fields lexicon)
		      :cache cache))
		    (entry 
		     (if record 
			 (make-psort-struct lexicon record))))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry 
		       :EMPTY)))
	       entry))))))

(defmethod make-psort-struct ((lexicon psql-lex-database) query-res)
  (let* 
      ((strucslots 
	(loop 
	    for (slot-key slot-field slot-path slot-type) 
	    in (fields-map lexicon)
	    for slot-value-list = 
	      (work-out-value 
	       slot-type 
	       (get-val slot-field query-res)
	       :path (work-out-value 
		      'list 
		      slot-path)
	       )		
	      ;; if empty third argument (ie. path), 
	      ;; then add (:key "field")
	    when slot-value-list
	    append (mapcar 
		    #'(lambda (x) 
			(make-psort-struct-aux 
			 slot-key x slot-path
			 )) 
		    slot-value-list)))
       ;; groups slots with same key together in a list
       (strucargs 
	(loop
	    for unique-slot in (remove-duplicates 
				(mapcar #'car strucslots))
	    append
	      (list unique-slot 
		    (let ((values 
			   (loop 
			       for (psort-slot psort-value) 
			       in strucslots
			       when (eql psort-slot unique-slot)
			       collect psort-value)))
		      ;;
		      ;; _fix_me_
		      ;; pretty bad fix to avoid getting lists where 
		      ;; they are not needed
		      ;;
		      (if (> (list-length values) 1)
			  values
			(car values)))))))
    (let ((orth-str (second (member :orth strucargs))))
      (if (and (listp orth-str) 
	       (cdr orth-str))
	  ;; infl-pos is only relevant for multi-word entries
	  (setf strucargs
	    (append 
	     (list :infl-pos
		   (find-infl-pos nil orth-str nil))
	     strucargs))))
    (apply #'make-lex-entry strucargs)))

;; lexicon is open
(defmethod load-lex-from-files ((lexicon psql-lex-database) file-names syntax)
  ;;  (setf *lex-file-list* file-names) ;;fix_me
  (setf *ordered-lex-list* nil) ;;fix_me
  (cond
   ((check-load-names file-names 'lexical)
    (let ((*lexicon-in* lexicon)) ;; *lexicon-in* is needed deep inside read-...-file-aux
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

(defmethod set-filter ((lexicon psql-lex-database) &rest rest)  
  (let* ((old-filter (get-filter lexicon))
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
    (when (catch 'pg:sql-error
	    (format *postgres-debug-stream* 
		    "~%Please wait: recreating database cache for new filter")
	    (force-output)
	    (unless (set-filter-aux lexicon filter)
	      (format t "~%(LexDB filter unchanged)")
	      (return-from set-filter))
	    nil)
      (lkb-beep)
      (set-filter lexicon))
    (format *postgres-debug-stream* 
	    "~%(lexicon filter: ~a )" 
	    (get-filter lexicon))
    (format *postgres-debug-stream* 
	    "~%(active lexical entries: ~a )" 
	    (fn-get-val lexicon ''size-current-grammar))))

(defmethod set-filter-aux ((lexicon psql-lex-database) filter)
  (unless (or (null filter) 
	      (equal (get-filter lexicon) filter))
    (reconnect lexicon) ;; must reconnect to avoid server bug...
    (force-output)
    (fn-get-records lexicon ''initialize-current-grammar filter)

    (empty-cache lexicon)))

;;;
;;;
;;; cache
;;;

(defmethod cache-all-lex-records ((lexicon psql-lex-database))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)
    (mapc
     #'(lambda (record)  
	 (setf (gethash (record-id record) record-cache) 
	   record))
     (retrieve-all-records lexicon 
			   (make-requested-fields lexicon)))))

(defmethod cache-all-lex-records-orth ((lexicon psql-lex-database))
  (with-slots (record-cache lexical-entries) lexicon
    (clrhash lexical-entries)
    (mapc 
     #'(lambda (record) 
	 (let* ((id (record-id record))
		(orth (record-orth record)))
	   (mapc 
	    #'(lambda (y)
		(setf (gethash y lexical-entries) 
		  (cons id 
			(gethash y lexical-entries))))
	    (split-into-words orth))
	   (setf (gethash (record-id record) 
			  record-cache) 
	     record) ))
     (retrieve-all-records lexicon 
			   (make-requested-fields lexicon)))))

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

(defmethod get-internal-table-defn ((lexicon psql-lex-database))
  (unless (string>= (server-version lexicon) "7.3")
    (return-from get-internal-table-defn nil))
  
  (let* 
      ((sql-str "SELECT attname, typname, atttypmod FROM pg_catalog.pg_attribute AS attribute JOIN (SELECT * FROM pg_catalog.pg_class WHERE relname='revision') AS class ON attribute.attrelid=class.relfilenode JOIN pg_catalog.pg_type AS type ON (type.typelem=attribute.atttypid);"))
       (make-column-map-record (run-query lexicon (make-instance 'sql-query :sql-string sql-str)))))

(defmethod get-field-size-map ((lexicon psql-lex-database))
  (mapcar 
   #'field-size-elt
   (get-internal-table-defn lexicon)))

(defmethod get-value-set ((lexicon psql-lex-database) field)
  (let* 
      ((sql-str (fn lexicon 'value-set field)))
       (mapcar #'car (records (run-query lexicon (make-instance 'sql-query :sql-string sql-str))))))

(defmethod lookup ((lexicon psql-lex-database) field-kw val-str)
  (cond
   (val-str
    (mapcar 'cdar 
	    (fn-get-records *psql-lexicon* 
			    ''lookup-general 
			    (symb-2-str field-kw)
			    val-str)))
   (t
    (mapcar 'cdar 
	    (fn-get-records *psql-lexicon* 
			    ''lookup-general-null
			    (symb-2-str field-kw))))))

(defmethod complete ((lexicon psql-lex-database) field-kw val-str)
  (mapcar 'cdar 
	  (fn-get-records *psql-lexicon* 
			  ''complete 
			  (symb-2-str field-kw)
			  val-str)))

(defmethod new-entries ((lexicon psql-lex-database))
  (let ((res (get-raw-records 
	      lexicon 
	      "SELECT userid, version, name FROM revision_new")))
    (cons (columns res)
	  (records res))))

;;;
;;; low-level stuff
;;;

;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid (user lexicon))
  (set-lex-entry-aux lexicon psql-le)
  )
  
(defmethod set-lex-entry-aux ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-version psql-le lexicon) 
  (if *postgres-export-timestamp* (set-val psql-le :modstamp *postgres-export-timestamp*))
  (let* ((symb-list '(:type :orthography :orthkey :keyrel :altkey :alt2key :keytag :altkeytag :compkey :ocompkey :comments :exemplars :lang :country :dialect :domains :genres :register :confidence :version :source :flags :modstamp :userid))
	 (symb-list (remove-if #'(lambda (x) (or (null x) 
						 (and (stringp x)
						      (string= x ""))))
			       symb-list
			       :key #'(lambda (x) (retr-val psql-le x))))) 
    (run-query lexicon 
	       (make-instance 'sql-query
		 :sql-string (format nil
				     (fn lexicon 
					 'update-entry 
					 (retr-val psql-le :name) 
					 (sql-select-list-str symb-list) 
					 (sql-val-list-str symb-list psql-le)))))
    (unless
	(check-lex-entry (str-2-symb (retr-val psql-le :name)))
      (error "Invalid lexical entry ~a -- see Lisp buffer output" (retr-val psql-le :name)))
    ))

(defmethod mwe-initialize-lex ((lexicon psql-lex-database))
  (mwe-initialize-userschema lexicon))

(defmethod mwe-initialize-userschema ((lexicon psql-lex-database))
  (format *postgres-debug-stream* "~%(mwe initializing private schema)")
  (fn-get-val lexicon ''mwe-initialize-schema))

;;;
;;; postgres interface
;;;

(defmethod mwe-retrieve-id-set ((lexicon psql-lex-database))
  (mapcar
   #'(lambda (x) (str-2-symb (cdar x))) 
   (fn-get-records lexicon ''mwe-retrieve-id-set)))

(defmethod mwe-retrieve-type ((lexicon psql-lex-database) mwe-id)
  (str-2-symb (fn-get-val lexicon ''mwe-retrieve-type (symb-2-str mwe-id))))

(defmethod mwe-retrieve-keyrels ((lexicon psql-lex-database) mwe-id)
  (let* ((raw-results (fn-get-records lexicon ''mwe-retrieve-keyrels (symb-2-str mwe-id)))
	 (s (make-sequence 'vector (length raw-results))))
    (mapcar #'(lambda (x) 
		(setf (aref s (1- (str-2-num (get-assoc-val :slot x))))
		  (str-to-mixed (get-assoc-val :keyrel x))))
	    raw-results
	    )))

(defmethod dump-multi-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''dump-multi-db filename))

(defmethod merge-multi-into-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (fn-get-records lexicon ''merge-multi-into-db filename))

(defmethod mwe-to-tdl ((lexicon psql-lex-database) mwe-id)
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str mwe-id)
   (p-2-tdl (mwe-build-P-list (mwe-retrieve-type lexicon mwe-id)
			      (mapcar #'(lambda (x) (cons 'PRED (list (list x))))
				      (mwe-retrieve-keyrels lexicon mwe-id))))))
	     
;;;
;;; script file fn
;;;

(defmethod mwe-read-roots ((lexicon psql-lex-database))
  ;;(initialise-psort-file file-name :root)
  (format  t "~%Loading MWE roots from lexical database ~a" (dbname lexicon))
  (let ((*readtable* (make-tdl-break-table)))
    (mapcar 
     #'(lambda (x)
	 (with-input-from-string (istream (mwe-to-tdl lexicon x))
	   (read-tdl-psort-stream istream :root)))
     (mwe-retrieve-id-set lexicon)))
  (finalize-psort-file :root))

(defmethod mwe-read-root-entry ((lexicon psql-lex-database) mwe-id)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream (mwe-to-tdl lexicon mwe-id))
      (read-tdl-psort-stream istream :root))))

(defmethod reload-roots-mwe ((lexicon psql-lex-database))
    (mwe-read-roots lexicon)
    (format  t "~%MWE roots reloaded from lexical database ~a" (dbname lexicon)))

;;;
;;; export to DB
;;;

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x 
				    :recurse nil
				    :new-instance t) output-lexicon))
   (collect-psort-ids lexicon :recurse nil))
  (build-lex-aux *psql-lexicon*))

(defmethod export-to-tdl ((lexicon lex-database) stream)
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records *lexicon*)
    (format t "~%(caching complete)"))
  (mapc
   #'(lambda (id)
       (format stream "~a" (to-tdl (read-psort lexicon id
					       :new-instance t)))
       (unexpand-psort lexicon id))
   (collect-psort-ids lexicon))
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(emptying cache)")
    (empty-cache *lexicon*)))

(defmethod export-to-tdl-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

