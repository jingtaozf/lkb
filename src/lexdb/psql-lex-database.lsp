;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

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
	     (records
	      (sql-fn-get-records lexicon 
				  :retrieve_entries_by_orthkey
				  :args (list (sql-like-text orthstr))
				  :fields (make-requested-fields-as-list lexicon)))
	      (ids 
	       (lookup-word-aux 
		records
		lexicon)))
	 ids)))

;;; (used to index for generator)
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((orth-raw-mapping (assoc :orth (fields-map *lexicon*)))
	 (orth-raw-value-mapping (fourth orth-raw-mapping))
	 (raw-orth-field (second orth-raw-mapping))
	 (values 
	  (get-value-set lexicon (2-str raw-orth-field))))
    (mapcan 
     #'(lambda (x) 
	 (mapcar #'string-upcase
		 (car (work-out-value
		       orth-raw-value-mapping
		       x))))
     values)))

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
  (let ((query-res 
	 (sql-fn-get-raw-records lexicon :lex_id_set)))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

(defmethod retrieve-all-records ((lexicon psql-lex-database) &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-all-records nil))
  (sql-fn-get-records lexicon
		      :retrieve_all_entries
		      :fields reqd-fields))

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
  (let ((column-records (sql-fn-get-records lexicon
					     :retrieve_entry
					     :fields reqd-fields
					     :args (list (sql-like-text id-str)))))
    (if (> (length column-records) 
	   1)
	(error (format nil "too many records returned"))
      (first column-records))))


(defmethod retrieve-head-record-str ((lexicon psql-lex-database) id-str &optional reqd-fields)
  (unless (connection lexicon)
    (format t "~%WARNING: no connection to psql-lex-database")
    (return-from retrieve-head-record-str nil))
  (let ((records (sql-fn-get-records lexicon
				     :retrieve_head_entry
				     :fields reqd-fields
				     :args (list (sql-like-text id-str)))))
    
    (if (> (length records) 
	   1)
	(error (format nil "too many records returned"))
      (car records))))

(defmethod id-to-tdl ((lexicon psql-lex-database) id)
  (to-tdl (read-psort lexicon id)))

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
		      :reqd-fields (make-requested-fields-as-list lexicon)
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
	    for (slot-key slot-field slot-path slot-type) in (fields-map lexicon)
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
		    "~%~%Please wait: recreating database cache for new filter")
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
	    (sql-fn-get-val lexicon 
			:size_current_grammar))))

(defmethod set-filter-aux ((lexicon psql-lex-database) filter)
  (unless (or (null filter) 
	      (equal (get-filter lexicon) filter))
    (reconnect lexicon) ;; must reconnect to avoid server bug...
    (force-output)
    (sql-fn-get-val lexicon 
		    :initialize_current_grammar 
		    :args (list filter))

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
			   (make-requested-fields-as-list lexicon)))))

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
			   (make-requested-fields-as-list lexicon)))))

(defmethod make-mneum-f-slot ((lexicon psql-lex-database))
  (with-slots (mneum-f) lexicon
    (setf mneum-f
      (mapcar 
       #'(lambda (x)
	   (cons (first x) (second x)))
       (sql-fn-get-raw-records lexicon 
			       :mneum_f_map 
			       :args (list (fields-tb lexicon)))))
    (unless mneum-f
      (format t "~%WARNING: no field-name mappings defined in LexDB ~a (mode = `~a'~% Using following backwards-compatibility mapping:~%~a"
	      (dbname lexicon) (fields-tb lexicon) *psql-mneum-f-back-compat-map*)
      (setf mneum-f *psql-mneum-f-back-compat-map*)
    mneum-f)))

(defmethod mneum-2-f ((lexicon psql-lex-database) mneum)
  (or 
   (cdr (assoc mneum (mneum-f lexicon) :test #'string=))
   mneum))

(defmethod make-field-map-slot ((lexicon psql-lex-database))
  "stores the mapping of fields to lex-entry structure slots"
  (setf (record-features lexicon) *postgres-record-features*)
  (make-mneum-f-slot lexicon)
  (setf (fields-map lexicon)
    (sort
     (mapcar #'(lambda (x) 
		 (list (str-2-keyword (first x))
		       (str-2-keyword (mneum-2-f lexicon (second x)))
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
	(complain-no-fields-map lexicon))
    (fields-map lexicon))

(defmethod complain-no-fields-map ((lexicon psql-lex-database))
  (format t "~%WARNING: No fields-map definitions available in LexDB ~a (mode='~a').
 (Hint: If necessary 'Merge new entries' from LexDB menu)" 
	       (dbname lexicon) (fields-tb lexicon)))

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

(defmethod lookup ((lexicon psql-lex-database) field-kw val-str)
  (let* ((sql-fn (if val-str
		    :lookup_general
		   :lookup_general_null))
	 (field-str (2-str field-kw))
	 (args (if val-str
		   (list field-str (sql-like-text val-str))
		 (list field-str)))
	 (records
	  (sql-fn-get-raw-records *psql-lexicon*
				  sql-fn
				  :args args)))
    (mapcar #'car records)))
  
(defmethod complete ((lexicon psql-lex-database) field-kw val-str)
  (mapcar #'car 
	  (sql-fn-get-raw-records *psql-lexicon* 
				  :complete 
				  :args (list (symb-2-str field-kw)
					      (sql-like-text val-str)))))

(defmethod new-entries ((lexicon psql-lex-database))
  (let ((records (sql-fn-get-raw-records lexicon 
				  :revision_new
				  :fields '(:userid :version :name))))
    (cons (list "userid" "version" "name") records)))

;;;
;;; low-level
;;;

;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :orthkey (lexicon-le-orthkey lexicon psql-le))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid (user lexicon))
  (set-lex-entry-aux lexicon psql-le))
  
(defmethod set-lex-entry-aux ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-version psql-le lexicon) 
  (if *postgres-export-timestamp* 
      (set-val psql-le :modstamp *postgres-export-timestamp*))
  (let* ((symb-list (copy-list *postgres-record-features*))
	 (symb-list (remove :name symb-list))
	 (symb-list (remove-duplicates symb-list))
	 (symb-list (remove-if 
		     #'(lambda (x) (or (null x) 
				       (and (stringp x)
					    (string= x ""))))
		     symb-list
		     :key #'(lambda (x) (retr-val psql-le x))))) 
    (sql-fn-get-val lexicon
		    :update_entry
		    :args (list (retr-val psql-le :name)
				symb-list
				(ordered-val-list symb-list psql-le)))
    (unless
	(check-lex-entry (str-2-symb (retr-val psql-le :name)))
      (error "Invalid lexical entry ~a -- see Lisp buffer output" (retr-val psql-le :name)))))

#+:mwe
(defmethod mwe-initialize-lex ((lexicon psql-lex-database))
  (mwe-initialize-userschema lexicon))

#+:mwe
(defmethod mwe-initialize-userschema ((lexicon psql-lex-database))
  (format *postgres-debug-stream* "~%(mwe initializing private schema)")
  (sql-fn-get-records lexicon 
		      :mwe_initialize_schema))

;;;
;;; postgres interface
;;;

#+:mwe
(defmethod mwe-retrieve-id-set ((lexicon psql-lex-database))
  (mapcar
   #'(lambda (x) (str-2-symb (car x))) 
   (sql-fn-get-raw-records lexicon 
			   :mwe_retrieve_id_set)))

#+:mwe
(defmethod mwe-retrieve-type ((lexicon psql-lex-database) mwe-id)
  (str-2-symb 
   (sql-fn-get-val lexicon 
			   :mwe_retrieve_type (symb-2-str mwe-id))))

#+:mwe
(defmethod mwe-retrieve-keyrels ((lexicon psql-lex-database) mwe-id)
  (let* ((raw-results 
	  (sql-fn-get-records lexicon 
			      :mwe_retrieve_keyrels (symb-2-str mwe-id)))
	 (s (make-sequence 'vector (length raw-results))))
    (mapcar #'(lambda (x) 
		(setf (aref s (1- (str-2-num (get-assoc-val :slot x))))
		  (str-to-mixed (get-assoc-val :keyrel x))))
	    raw-results
	    )))

#+:mwe
(defmethod dump-multi-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (sql-fn-get-records lexicon 
		      :dump_multi_db filename))

#+:mwe
(defmethod merge-multi-into-db ((lexicon psql-lex-database) filename)  
  (setf filename (namestring (pathname filename)))
  (sql-fn-get-records lexicon 
		      :merge_multi_into_db filename))

#+:mwe
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

#+:mwe
(defmethod mwe-read-roots ((lexicon psql-lex-database))
  (format  t "~%Loading MWE roots from lexical database ~a" (dbname lexicon))
  (let ((*readtable* (make-tdl-break-table)))
    (mapcar 
     #'(lambda (x)
	 (with-input-from-string (istream (mwe-to-tdl lexicon x))
	   (read-tdl-psort-stream istream :root)))
     (mwe-retrieve-id-set lexicon)))
  (finalize-psort-file :root))

#+:mwe
(defmethod mwe-read-root-entry ((lexicon psql-lex-database) mwe-id)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream (mwe-to-tdl lexicon mwe-id))
      (read-tdl-psort-stream istream :root))))

#+:mwe
(defmethod reload-roots-mwe ((lexicon psql-lex-database))
    (mwe-read-roots lexicon)
    (format  t "~%MWE roots reloaded from lexical database ~a" (dbname lexicon)))

;;;
;;; export to DB
;;;

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) 
       (to-db (read-psort lexicon x 
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

;(defmethod make-requested-fields ((lexicon psql-lex-database))
;  "constructs the argument string to sql SELECT with all necessary fields"
;  (unless (fields-map lexicon)
;    (complain-no-fields-map)
;    (error "operation aborted"))
;  (let* ((fields 
;	  (remove-duplicates 
;	   (mapcar #'cadr 
;		   (fields-map lexicon))
;	   :test #'equal))
;         (fields-str 
;	  (symb-2-str 
;	   (pop fields))))
;    (loop 
;        for element in fields
;        do 
;	  (setf fields-str 
;	    (concatenate 'string 
;	      fields-str 
;	      ", " 
;	      (symb-2-str element))))
;    fields-str))

(defmethod make-requested-fields-as-list ((lexicon psql-lex-database))
  (unless (fields-map lexicon)
    (complain-no-fields-map lexicon)
    (error "operation aborted"))
  (remove-duplicates 
   (mapcar #'cadr 
	   (fields-map lexicon))
   :test #'equal))
    
(defmethod close-lex ((lexicon psql-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (lexdb-version semi) lexicon
    (setf lexdb-version nil)
    ;(setf semi nil)
    (if (next-method-p) (call-next-method))))

(defmethod open-lex ((lexicon psql-lex-database) &key name parameters)
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
	(error "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax).
 Try obtaining a more recent LKB binary." lexdb-version *psql-lexdb-compat-version*))
       ((string< (compat-version lexdb-version)
		 *psql-lexdb-compat-version*)
       (error "Your LexDB version (~a) is incompatible with this LKB version (requires v. ~ax).
 You must load updated setup files.
 See http://www.cl.cam.ac.uk/~~bmw20/DT/initialize-db.html" lexdb-version *psql-lexdb-compat-version*)))
      ;(retrieve-fn-defns lexicon)
      (make-field-map-slot lexicon)
      (initialize-userschema lexicon)
      (setf (name lexicon) name)
      lexicon)
     (t
      (format t "~%unable to connect to ~s:~%  ~a" 
	      (pg:db connection) 
	      (pg:error-message connection))
      nil))))

(defmethod initialize-lex ((lexicon psql-lex-database))
  (when (open-lex lexicon)
    (build-lex lexicon)))
  
(defmethod vacuum-current-grammar ((lexicon psql-database) &key verbose)
  (let ((command
	 (if verbose
	     "vacuum full analyze verbose current_grammar"
	   "vacuum full analyze current_grammar")))
    (format t "~%~%Please wait: vacuuming private table")
    (force-output)
    (run-command lexicon command)
    (lkb-beep)))

(defmethod vacuum-public-revision ((lexicon psql-lex-database) &key verbose)
  (with-slots (dbname host port) lexicon
    (let ((l2 (make-instance 'psql-database
		:dbname dbname
		:host host
		:port port
		:user (sql-fn-get-val lexicon 
				      :db_owner)))
;		:user (raw-get-val lexicon "SELECT db_owner()")))
	  (command
	   (if verbose
	       "vacuum full analyze verbose public.revision"    
	     "vacuum full analyze public.revision")))
      (format t "~%~%Please wait: vacuuming public table")
      (force-output)
      (connect l2)
      (run-command l2 command)
      (disconnect l2))))

(defmethod connect ((lexicon psql-lex-database)) 
  (if (next-method-p) (call-next-method))
  (setf (lexdb-version lexicon) 
    (get-db-version lexicon)))	

;;;
;;;
;;;

(defmethod get-db-version ((lexicon psql-lex-database))
  (sql-fn-get-val lexicon
		  :lexdb_version))
    
(defmethod get-filter ((lexicon psql-lex-database))
  (sql-fn-get-val lexicon
		  :filter))

(defmethod next-version (id (lexicon psql-lex-database))
  (let* ((res (sql-fn-get-val lexicon 
			      :next_version 
			      :args (list (sql-like-text id)))))
    (str-2-num res 0)))

(defmethod build-lex ((lexicon psql-lex-database))
  (build-lex-aux lexicon)
  ;(if (semi lexicon)   
  (cond
   ((null (semi lexicon))
    nil)
   ((semi-up-to-date-p lexicon)
    (format t "~%(loading SEM-I into memory)")
    (unless (mrs::semi-p 
	     (catch 'pg::sql-error
	       (mrs::populate-*semi*-from-psql)))
      (format t "~% (unable to retrieve database SEM-I)"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~%WARNING: no lexical entries indexed for generator")))
    ;)
  lexicon)

(defmethod build-lex-aux ((lexicon psql-lex-database))
  (reconnect lexicon) ;; work around server bug
  (cond 
   ((not (user-read-only-p lexicon (user lexicon)))
    (sql-fn-get-raw-records lexicon 
			    :initialize_current_grammar 
			    :args (list (get-filter lexicon))))
   (t
    (format t "~%(user ~a has read-only privileges)" (user lexicon))))    
  (format t "~%(LexDB filter: ~a )" (get-filter lexicon))
  (let ((size (sql-fn-get-val lexicon 
			      :size_current_grammar)))
    (if (string= "0" size)
	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
      (format t "~%(active lexical entries: ~a )" size)))
  (empty-cache lexicon))


;;;
;;;
;;;

(defmethod user-read-only-p ((lexicon psql-lex-database) user-str)
  (string= "t" 
	   (sql-fn-get-val lexicon 
				   :user_read_only_p 
				   :args (list user-str))))

(defmethod show-scratch ((lexicon psql-lex-database))
  (sql-fn-get-raw-records lexicon :show_scratch))

(defmethod merge-into-db ((lexicon psql-lex-database) rev-filename)  
  (run-command-stdin lexicon 
		     (format nil "~a;~%~a;" 
			     "DELETE FROM temp" 
			     "COPY temp FROM stdin DELIMITERS ',' WITH NULL AS ''") 
		     rev-filename)
  (let ((count-new
	 (str-2-num
	  (sql-fn-get-val lexicon
			  :merge_into_db2))))
    (format t "~%(~a new entries)" count-new)
    (unless (equal 0 count-new)
      (vacuum-public-revision lexicon))
    count-new))

(defmethod merge-defn ((lexicon psql-lex-database) 
		       dfn-filename)  
  (when (catch 'pg:sql-error 
	  (run-command lexicon 
		       "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL;"))
    (run-command lexicon 
		 "DROP TABLE temp_defn")
    (run-command lexicon 
		 "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL ;"))
  (run-command-stdin lexicon 
		     "COPY temp_defn FROM stdin" 
		     dfn-filename)
  (let ((count-new-dfn 
	 (str-2-num 
	  (sql-fn-get-val lexicon :merge_defn))))
    (run-command lexicon "DROP TABLE temp_defn")
    (format t "~%(~a new field mappings)" count-new-dfn)
    count-new-dfn))

(defmethod initialize-userschema ((lexicon psql-lex-database))
  (unless
      (sql-fn-get-val lexicon 
		      :test_user 
		      :args (list (user lexicon)))
    (format t "~%(creating private space for user ~a)" (user lexicon))
    (sql-fn-get-val lexicon 
		    :create_schema 
		    :args (list (user lexicon)))
    #+:mwe
    (if *postgres-mwe-enable*
	(mwe-initialize-userschema lexicon))))

(defmethod semi-setup-1 ((lexicon psql-lex-database))  
  (sql-fn-get-val lexicon 
		  :semi_setup_1))
  
(defmethod semi-setup-2 ((lexicon psql-lex-database))  
  (sql-fn-get-val lexicon 
		  :semi_setup_2))
 
(defmethod semi-up-to-date-p ((lexicon psql-lex-database))  
  (string= "t"
	   (sql-fn-get-val lexicon
			   :semi_up_to_date_p)))
  
(defmethod semi-out-of-date ((lexicon psql-lex-database))  
  (mapcar #'(lambda (x) (2-symb (car x))) 
	  (sql-fn-get-raw-records lexicon 
				  :semi_out_of_date)))

(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    
(defmethod get-value-set ((lexicon psql-lex-database) field)
  (mapcar #'car 
	  (sql-fn-get-raw-records lexicon
				  :value_set
				  :args (list (2-str field)))))
