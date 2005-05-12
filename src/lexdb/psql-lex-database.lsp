;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;; --- psql-lex-database methods
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (with-slots (lexical-entries) lexicon
  (let ((hashed (gethash orth lexical-entries)))
    (cond 
     (hashed
      (if (eq hashed :EMPTY)
	  (setf hashed nil)
	hashed))
     (t 
      (let ((value (lookup-word-no-cache lexicon orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth lexical-entries) 
	    (if value value :EMPTY)))
	value))))))

;; orthkey must be mapped to normalized form before entering PSQL universe
(defmethod lookup-word-no-cache ((lexicon psql-lex-database) orth)
  (declare (ignore cache))
  (if (connection lexicon)
      (let* ((table (sql-fn-get-records lexicon 
					:retrieve_entries_by_orthkey 
					:args (list (sql-like-text 
						     (normalize-orthkey orth))) 
					:fields (grammar-fields lexicon)))
	     (ids (lookup-word-aux2 lexicon table)))
	ids)))

(defmethod uncached-orthkeys ((lexicon psql-lex-database) list-orth)
  (with-slots (lexical-entries) lexicon
    (loop
	for orth in list-orth
	unless (gethash orth lexical-entries)
	collect orth)))

(defun prune-quotes (str)
  (let ((len (length str)))
    (subseq str 1 (1- len))))

(defmethod lookup-words-cache ((lexicon psql-lex-database) table uc-list-orth)
  (with-slots (psorts lexical-entries record-cache fields-map) lexicon
    (let ((name-field (second (assoc :id fields-map))))
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
		(make-psort-struct lexicon record columns)))
	    ;; cache orthkey id
	    (setf (gethash orthkey lexical-entries)
	      (add-w-empty id (gethash orthkey lexical-entries)))))))

(defmethod lookup-word-aux2 ((lexicon psql-lex-database) table)
  (with-slots (psorts record-cache fields-map) lexicon
    (let ((name-field (second (assoc :id fields-map))))
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
		(make-psort-struct lexicon rec cols)))
	  collect id))))

;

;;; (used to index for generator)
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((orth-raw-mapping (assoc :orth (fields-map lexicon)))
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

;

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

;

(defmethod retrieve-all-records ((lexicon psql-lex-database) &optional reqd-fields)
  (cond
   ((connection lexicon)
    (sql-fn-get-records lexicon
			:retrieve_all_entries
			:fields reqd-fields))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))    
    
(defmethod retrieve-raw-record ((lexicon psql-lex-database) id &key (cache t) reqd-fields)
  (with-slots (record-cache) lexicon
    (let ((hashed (gethash id record-cache)))
      (cond (hashed
	     (unless (eq hashed :EMPTY)
	       hashed))
	    (t
	     (let* ((record (retrieve-raw-record-no-cache lexicon id reqd-fields)))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-raw-record-no-cache ((lexicon psql-lex-database) id &optional reqd-fields)
  (cond 
   ((connection lexicon)
    (let* ((id-str (symb-2-str id))
	   (column-records (sql-fn-get-raw-records lexicon
						   :retrieve_entry
						   :fields reqd-fields
						   :args (list (sql-like-text id-str)))))
      (if (> (length column-records) 1)
	  (error (format nil "too many records returned"))
	(first column-records))))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))

(defmethod retrieve-head-record-str ((lexicon psql-lex-database) id &optional reqd-fields)
  (retrieve-head-record lexicon (str-2-symb id) reqd-fields))

(defmethod retrieve-head-record ((lexicon psql-lex-database) id &optional reqd-fields)
  (cond
   ((connection lexicon)
    (let* ((id-str (2-str id))
	   (table (sql-fn-get-records lexicon
					:retrieve_head_entry
					:fields reqd-fields
					:args (list (sql-like-text id-str)))))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))

(defmethod grammar-fields ((lexicon psql-lex-database))
  (unless (fields-map lexicon)
    (complain-no-fields-map lexicon)
    (error "operation aborted"))
  (remove-duplicates 
   (mapcar #'cadr 
	   (fields-map lexicon))
   :test #'equal))

;

(defmethod read-psort ((lexicon psql-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (with-slots (psorts) lexicon
    (let ((hashed (and (not new-instance) (gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed :EMPTY) hashed))
	    (t
	     (let ((entry (read-psort-aux lexicon id :cache cache)))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry :EMPTY)))
	       entry))))))

(defmethod read-psort-aux ((lexicon psql-lex-database) id &key (cache t))
  (with-slots (psorts) lexicon
    (let* ((cols (grammar-fields lexicon))
	   (raw-record 
	    (retrieve-raw-record 
	     lexicon id 
	     :reqd-fields cols
	     :cache cache)))
      (if raw-record 
	  (make-psort-struct lexicon raw-record cols)))))

(defmethod make-psort-struct ((lexicon psql-lex-database) raw-record cols)
  (apply #'make-lex-entry 
	 (make-strucargs lexicon raw-record cols)))

(defmethod make-strucargs ((lexicon psql-lex-database) raw-record cols)
  (let* 
      ((strucslots 
	(loop 
	    for (slot-key slot-field slot-path slot-type) in (fields-map lexicon)
	    for slot-value-list = (work-out-value slot-type 
						  (get-val slot-field raw-record cols)
						  :path (work-out-rawlst slot-path))
	    when slot-value-list
	    append (mapcar 
		    #'(lambda (x) (make-strucargs-aux slot-key x slot-path)) 
		    slot-value-list)))
       ;; groups slots with same key together in a list
       (strucargs 
	(loop
	    for unique-slot in (remove-duplicates (mapcar #'car strucslots))
	    append
	      (list unique-slot 
		    (let ((values 
			   (loop 
			       for (psort-slot psort-value) 
			       in strucslots
			       when (eql psort-slot unique-slot)
			       collect psort-value)))
		      (if (> (list-length values) 1)
			  values
			(car values)))))))
    (let ((orth-str (second (member :orth strucargs))))
      (if (and (listp orth-str) 
	       (cdr orth-str))
	  ;; infl-pos is only relevant for multi-word entries
	  (setf strucargs
	    (append 
	     (list :infl-pos (find-infl-pos nil orth-str nil))
	     strucargs))))
    strucargs))

(defmethod record-to-tdl ((lexicon psql-lex-database) record)
  (let ((cols (mapcar #'car record))
	(rec (mapcar #'cdr record)))
    (raw-record-to-tdl lexicon rec cols)))
  
(defmethod raw-record-to-tdl ((lexicon psql-lex-database) rec cols)
  (to-tdl (make-psort-struct lexicon rec cols)))
  
;; lexicon is open
(defmethod load-lex-from-files ((lexicon psql-lex-database) file-names syntax)
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
    (when (catch :sql-error
	    (format t "~%~%Please wait: recreating database cache for new filter")
	    (force-output)
	    (unless (set-filter-aux lexicon filter)
	      (format t "~%(LexDB filter unchanged)")
	      (return-from set-filter))
	    nil)
      (lkb-beep)
      (set-filter lexicon))
    (format t "~%(lexicon filter: ~a )" 
	    (get-filter lexicon))
    (format t "~%(active lexical entries: ~a )" 
	    (sql-fn-get-val lexicon :size_current_grammar))))

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
;;; cache
;;;

(defmethod cache-all-lex-records ((lexicon psql-lex-database))
  (let* ((table (retrieve-all-records lexicon 
				      (grammar-fields lexicon)))
	 (recs (recs table))
	 (cols (cols table)))
    (with-slots (record-cache) lexicon
      (clrhash record-cache)
      (mapc
       #'(lambda (rec)  
	   (setf (gethash (record-id rec cols lexicon) record-cache) 
	     rec))
       recs))))

(defmethod cache-all-lex-records-orth ((lexicon psql-lex-database))
  (let ((table (retrieve-all-records lexicon (grammar-fields lexicon))))
    (with-slots (recs cols) table
      (with-slots (record-cache lexical-entries) lexicon
	;; clear cache
	(clrhash lexical-entries)
	(clrhash record-cache)
	;; for each record...
	(mapc
	 #'(lambda (rec)
	     (let* ((id (record-id rec cols lexicon))
		    (orth (record-orth rec cols lexicon)))
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

(defmethod make-field-map-slot ((lexicon psql-lex-database))
  "stores the mapping of fields to lex-entry structure slots"
  (with-slots (fields-map fields) lexicon
    (setf fields (get-fields lexicon))
    (setf fields-map
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
	       (get-raw-records lexicon (format nil "SELECT slot,field,path,type FROM defn WHERE mode='~a';" (fields-tb lexicon))))
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :unifs))))
    (if (null fields-map)
	(complain-no-fields-map lexicon))
    fields-map))

(defmethod complain-no-fields-map ((lexicon psql-lex-database))
  (format t "~%WARNING: No fields-map definitions available in LexDB ~a (mode='~a').
 (Hint: If necessary 'Merge new entries' from LexDB menu)" 
	       (dbname lexicon) (fields-tb lexicon)))

(defmethod get-internal-table-defn ((lexicon psql-lex-database))
      (sql-fn-get-records lexicon 
			  :return_field_info2 
			  :args (list "public" "revision")))

(defmethod get-field-size-map ((lexicon psql-lex-database))
  (let* ((table (get-internal-table-defn lexicon))
	(recs (recs table))
	(cols (cols table)))
    (mapcar 
     #'(lambda (x) (field-size-elt x cols)) 
     recs)))

(defmethod lookup ((lexicon psql-lex-database) field-kw val-str)
  (let* ((sql-fn (if val-str
		    :lookup_general
		   :lookup_general_null))
	 (field-str (2-str field-kw))
	 (args (if val-str
		   (list field-str (sql-like-text val-str))
		 (list field-str)))
	 (records
	  (sql-fn-get-raw-records lexicon
				  sql-fn
				  :args args)))
    (mapcar #'car records)))
  
(defmethod complete ((lexicon psql-lex-database) field-kw val-str)
  (mapcar #'car 
	  (sql-fn-get-raw-records lexicon 
				  :complete 
				  :args (list (symb-2-str field-kw)
					      (sql-like-text val-str)))))

;; called from pg-interface
(defmethod new-entries ((lexicon psql-lex-database))
  (let ((records (sql-fn-get-raw-records lexicon 
				  :revision_new
				  :fields '(:userid :name :modstamp))))
    (cons (list "userid" "name" "modstamp") records)))

(defmethod current-timestamp ((lexicon psql-lex-database))
  (sql-fn-get-val lexicon 
		  :retrieve_current_timestamp))

;;;
;;; low-level
;;;

(defmethod set-lex-entry-from-record ((lexicon psql-lex-database) fv-pairs)
  (set-lex-entry lexicon
		 (make-instance 'psql-lex-entry :fv-pairs fv-pairs)))

;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :orthkey (lexicon-le-orthkey lexicon psql-le))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid (user lexicon))
  (set-lex-entry-aux lexicon psql-le))
  
(defmethod set-lex-entry-aux ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :modstamp "NOW")
  (let* ((symb-list (copy-list (fields lexicon)))
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
	(check-lex-entry (str-2-symb (retr-val psql-le :name))
			 lexicon)
      (error "Invalid lexical entry ~a -- see Lisp buffer output" (retr-val psql-le :name)))))

#+:mwe
(defmethod mwe-initialize-lex ((lexicon psql-lex-database))
  (mwe-initialize-userschema lexicon))

#+:mwe
(defmethod mwe-initialize-userschema ((lexicon psql-lex-database))
  (format t "~%(mwe initializing private schema)")
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

(defmethod close-lex ((lexicon psql-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (lexdb-version semi) lexicon
    (setf lexdb-version nil)
    (if (next-method-p) (call-next-method))))

(defvar *lexdb-temp-lexicon*)
(defmethod open-lex ((lexicon psql-lex-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (lexdb-version dbname host user connection) 
      lexicon
    (close-lex lexicon)    
    (format t "~%Connecting to lexical database ~a@~a:~a" 
	    dbname host (true-port lexicon))
    (force-output)
    (setf *lexdb-tmp-lexicon* lexicon)
    (cond
     ((connect lexicon)
      (format t "~%Connected as user ~a" user)
      (format t "~%Opening ~a" dbname)
      (check-server-version lexicon)
      (cond
       ((not (stringp lexdb-version))
	(error "Unable to determine LexDB version"))
       ((string> (compat-version lexdb-version)
		 *lexdb-major-version*)
	(error *lexdb-message-old-lkb* lexdb-version *lexdb-major-version*))
       ((string< (compat-version lexdb-version)
		 *lexdb-major-version*)
       (error *lexdb-message-old-lexdb* lexdb-version *lexdb-major-version*)))
      (make-field-map-slot lexicon)
      (initialize-userschema lexicon)
      (setf (name lexicon) name)
      lexicon)
     (t
      (format t "~%unable to connect to ~s:~%  ~a" dbname
	      (error-msg connection))
      nil))))

(defmethod check-server-version ((lexicon psql-lex-database))
  (cond
   ((string>= (lexdb-version lexicon) "3.34")
    (let ((texts (sql-fn-get-vals lexicon :check_server_version)))
      (when texts
	(format t "~%WARNING: ~a" (str-list-2-str texts :sep-c #\Newline)))))
   (t
    (let ((server-version (get-server-version lexicon)))
      (unless (string>= server-version "7.3")
	(format t "~%WARNING: ")
	(format t *lexdb-message-old-server* server-version "7.4.x"))))))

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
    (run-command lexicon command)))

(defmethod vacuum-public-revision ((lexicon psql-lex-database) &key verbose)
  (with-slots (dbname host port) lexicon
    (let ((l2 (make-instance 'psql-database
		:dbname dbname
		:host host
		:port port
		:user (sql-fn-get-val lexicon :db_owner)))
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
  (when (connection-ok lexicon)
    (setf (lexdb-version lexicon) 
      (get-db-version lexicon))
    (when (string>= (lexdb-version lexicon) "3.34")
      (get-ext-fns lexicon))
    t))	

(defmethod get-ext-fns ((lexicon psql-lex-database)) 
  (setf (ext-fns lexicon)
    (mapcar #'(lambda (x)
		(str-2-keyword (car x)))
	    (get-raw-records lexicon "SELECT * FROM ext_fns()"))))

;;;
;;;
;;;

(defmethod get-db-version ((lexicon psql-lex-database))
  (caar 
   (get-raw-records lexicon 
		    "SELECT val FROM public.meta WHERE var='db-version' LIMIT 1")))
    
(defmethod get-filter ((lexicon psql-lex-database))
  (sql-fn-get-val lexicon :filter))

;; obsolete
(defmethod next-version (id (lexicon psql-lex-database))
  (let* ((res (sql-fn-get-val lexicon 
			      :next_version 
			      :args (list (sql-like-text id)))))
    (str-2-num res 0)))

(defmethod build-lex ((lexicon psql-lex-database))
  (build-lex-aux lexicon)
  (cond
   ((null (semi lexicon))
    nil)
   ((semi-up-to-date-p lexicon)
    (format t "~%(loading SEM-I into memory)")
    (unless (mrs::semi-p 
	     (catch :sql-error
	       (mrs::populate-*semi*-from-psql)))
      (format t "~% (unable to retrieve database SEM-I)"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~%WARNING: no lexical entries indexed for generator")))
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
  (let ((size (sql-fn-get-val lexicon :size_current_grammar)))
    (if (string= "0" size)
	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
      (format t "~%(active lexical entries: ~a )" size)))
  (empty-cache lexicon))

;;;
;; semi
;;;

(defmethod index-new-lex-entries ((lexicon psql-lex-database))
  (let ((semi-out-of-date (semi-out-of-date lexicon)))
    (format t "~%(indexing ~a entries)" (length semi-out-of-date))
    (when semi-out-of-date
      (mrs::populate-*semi*-from-psql)
      (index-lexical-rules)
      (index-grammar-rules)
      (mapc 
       #'(lambda (x)
	   (update-semi-entry lexicon x))
       semi-out-of-date)
      (mrs::dump-*semi*-to-psql))))
  
(defmethod update-semi-entry ((lexicon psql-lex-database) lexid)
  (let* ((entry (read-psort lexicon lexid :cache nil))
	 (new-fs (and
		  (expand-psort-entry entry)
		  (lex-entry-full-fs entry))))
    (if (and new-fs 
	     (not (eq new-fs :fail)))
	(mrs::extract-lexical-relations entry)
      (format t "~%No feature structure for ~A~%" 
	      (lex-entry-id entry))))
    (forget-psort lexicon lexid))

;;;
;;;
;;;

(defmethod get-fields ((lexicon psql-lex-database))
  (mapcar 
   #'(lambda (x) (intern (string-upcase (car x)) :keyword))
   (sql-fn-get-raw-records lexicon :list_fields)))

(defmethod user-read-only-p ((lexicon psql-lex-database) user-str)
  (string= "t" 
	   (sql-fn-get-val lexicon :user_read_only_p 
				   :args (list user-str))))

(defmethod show-scratch ((lexicon psql-lex-database))
  (sql-fn-get-raw-records lexicon 
			  :retrieve_private_revisions
			  :fields (list :name :userid :modstamp)))

(defmethod scratch-records ((lexicon psql-lex-database))
  (sql-fn-get-raw-records lexicon :retrieve_private_revisions))

(defmethod merge-into-db ((lexicon psql-lex-database) rev-filename)  
  (run-command-stdin lexicon 
		     (format nil "~a;~%~a;" 
			     "DELETE FROM temp" 
			     "COPY temp FROM stdin") 
		     rev-filename)
  (let ((count-new
	 (str-2-num
	  (sql-fn-get-val lexicon :merge_into_db2))))
    (format t "~%(~a new revision entries)" count-new)
    (unless (equal 0 count-new)
      (vacuum-public-revision lexicon))
    count-new))

(defmethod merge-defn ((lexicon psql-lex-database) dfn-filename)  
  (when (catch :sql-error 
	  (run-command lexicon "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL;"))
    (run-command lexicon "DROP TABLE temp_defn")
    (run-command lexicon "CREATE TABLE temp_defn AS SELECT * FROM defn WHERE NULL ;"))
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
  (sql-fn-get-val lexicon :initialize_user_schema )
  #+:mwe
  (if *postgres-mwe-enable*
      (mwe-initialize-userschema lexicon)))

(defmethod semi-setup-pre ((lexicon psql-lex-database))  
  (reconnect lexicon)
  (sql-fn-get-val lexicon :semi_setup_pre))
  
(defmethod semi-setup-post ((lexicon psql-lex-database))  
  (reconnect lexicon)
  (sql-fn-get-val lexicon :semi_setup_post))
 
(defmethod semi-up-to-date-p ((lexicon psql-lex-database))  
  (string= "t"
	   (sql-fn-get-val lexicon :semi_up_to_date_p)))
  
;; returns record-ids
(defmethod semi-out-of-date ((lexicon psql-lex-database))
  (with-slots (record-cache) lexicon
    (let* ((cols (grammar-fields lexicon))
	   (table (sql-fn-get-records lexicon 
				      :semi_out_of_date
				      :fields cols))
	   (recs (recs table)))
      ;; cache records
      (mapcar
       #'(lambda (rec)
	   (let ((rec-id (record-id rec cols lexicon)))
	     (setf (gethash rec-id record-cache) rec)
	     rec-id))
	   recs))))

(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    
(defmethod get-value-set ((lexicon psql-lex-database) field)
  (mapcar #'car 
	  (sql-fn-get-raw-records lexicon
				  :value_set
				  :args (list (2-str field)))))

;; return sql code to call db function and return
;; appropriate fields
(defmethod sql-fn-string ((lexicon psql-lex-database) fn &key args fields)
  (with-slots (lexdb-version ext-fns) lexicon
    (when (not (member fn ext-fns))
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

(defmethod sql-fn-get-records ((lexicon psql-lex-database) fn &key args fields)
  (get-records lexicon
	       (sql-fn-string lexicon fn :args args :fields fields)))

;;;
;;; this approach is not right
;;;
(defmethod sql-fn-get-records-union ((lexicon psql-lex-database) fn &key list-args fields)
  (when list-args
    (let* ((first-arg (pop list-args))
	   (sql-str
	    (apply #'concatenate 'string
		   (cons (sql-fn-string lexicon fn 
					:args first-arg
					:fields (cons (sql-fn-arg first-arg) fields))
			 (mapcan #'(lambda (x) (list " UNION " 
						     (sql-fn-string lexicon fn
								    :args x
								    :fields (cons (sql-fn-arg x) fields))))
				 list-args)))))
      (get-records lexicon sql-str))))

(defmethod sql-fn-get-raw-records ((lexicon psql-lex-database) fn &key args fields)
  (get-raw-records lexicon
		   (sql-fn-string lexicon fn :args args :fields fields)))
  
(defmethod sql-fn-get-val ((lexicon psql-lex-database) fn &key args fields)
  (caar (sql-fn-get-raw-records lexicon fn :args args :fields fields)))
  
(defmethod sql-fn-get-vals ((lexicon psql-lex-database) fn &key args fields)
  (mapcar #'car
	  (sql-fn-get-raw-records lexicon fn :args args :fields fields)))
  
;;
;;
;;

(defmethod merge-into-lexicon ((lexicon psql-lex-database) filename)
  "connect as db owner and merge new data into lexdb"
  (with-slots (dbname host port) lexicon
    (unless dbname
      (error "please set :dbname"))
    (let ((conn-db-owner 
	   (make-instance 'psql-lex-database
	     :dbname dbname
	     :host host
	     :port port
	     :user (sql-fn-get-val lexicon :db_owner)))
	  (count-new 0))
      (connect conn-db-owner)
      (when
	  (catch :sql-error
	    (progn
	      (let* ((rev-filename (absolute-namestring "~a.rev" filename))
		     (dfn-filename (absolute-namestring "~a.dfn" filename)))
		(if (probe-file rev-filename)
		    (setf count-new (merge-into-db conn-db-owner rev-filename))
		  (format t "~%WARNING: no file ~a" rev-filename))
		(cond
		 ((probe-file dfn-filename)
		  (merge-defn conn-db-owner dfn-filename)
		  (make-field-map-slot lexicon))
		 (t
		  (format t "~%WARNING: no file ~a" dfn-filename)))
		nil)))
	(format t "~%Merge new entries aborted...")
	(lkb-beep))
      (if (equal count-new 0)
	  (empty-cache lexicon)
	(initialize-psql-lexicon))
      (disconnect conn-db-owner))))

(defmethod merge-into-lexicon-defn ((lexicon psql-lex-database) filename)
  "reconnect as db owner and merge new defn into lexdb"
  (with-slots (dbname host port) lexicon
    (unless dbname
      (error "please set :dbname"))
    (let ((conn-db-owner 
	   (make-instance 'psql-lex-database
	     :dbname dbname
	     :host host
	     :port port
	     :user (sql-fn-get-val lexicon :db_owner))))
      (connect conn-db-owner)
      (when
	  (catch :sql-error
	    (progn
	      (let ((dfn-filename 
		     (absolute-namestring "~a" filename)))
		(cond
		 ((probe-file dfn-filename)
		  (merge-defn conn-db-owner 
			      dfn-filename)
		  (make-field-map-slot lexicon))
		 (t
		  (format t "~%WARNING: no file ~a" dfn-filename)))
		nil
		)))
	(format t "Merge new .dfn entries aborted..."))
      (empty-cache lexicon)
      (disconnect conn-db-owner))))


(defmethod to-db-dump ((x lex-entry) (lexicon psql-lex-database))
  "provide line entry for lexicon db import file"
  (with-slots (fields-map fields) lexicon
    (let* ((s (copy-slots x fields-map))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lexicon))))
	   (field-vals (append
			(mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x fields-map)))
			 extraction-fields)
			(list
			 (cons :orthkey (orthkey x))
			 (cons :userid *lexdb-dump-user*)
			 (cons :modstamp "NOW")
			 (cons :lang *lexdb-dump-lang*)
			 (cons :country *lexdb-dump-country*)
			 (cons :confidence 1)
			 (cons :source *lexdb-dump-source*)
			 (cons :flags 1))))
	   (ordered-field-vals (ordered-symb-val-list fields field-vals))
	   (line 
	    (format nil "~a~%" 
		    (str-list-2-str
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
       ((null (cdr (assoc :unifs s)))
	line)
       (t
	(format *lexdb-dump-skip-stream* "~a" (to-tdl x))
	"")))))

(defmethod to-db ((x lex-entry) (lexicon psql-lex-database))
  "insert lex-entry into lexicon db (user scratch space)"
  (with-slots (fields-map) lexicon
    (let* ((s (copy-slots x fields-map))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lexicon))))
	   (extracted-fields
	    (mapcan 
	     #'(lambda (x) (list x (extract-field s x fields-map)))
	     extraction-fields))
	 
	   (psql-le
	    (apply #'make-instance-psql-lex-entry
		   (append extracted-fields
			   (list :country *lexdb-dump-country*
				 :lang *lexdb-dump-lang*
				 :source (extract-pure-source-from-source *lexdb-dump-source*)
				 :confidence 1
				 :flags 1
				 )))))
      (cond
       ((null (cdr (assoc :unifs s)))
	(set-lex-entry lexicon psql-le)
	(empty-cache lexicon))
       (t
       (format t "~%skipping super-rich entry:~%~a" (to-tdl x))
       nil)))))
  
;; WARNING: not suited to batch import!
;; import lexicon to LexDB
(defmethod export-to-db ((lexicon lex-database) (lexdb psql-lex-database))
  (mapc
   #'(lambda (x) 
       (to-db (read-psort lexicon x :recurse nil :new-instance t) 
	      lexdb))
   (collect-psort-ids lexicon :recurse nil))
  (build-lex-aux lexdb))

(defmethod record-id (raw-record cols (lexicon psql-lex-database))
  (str-2-symb (get-val :name raw-record cols)))  

(defmethod record-orth (raw-record cols (lexicon psql-lex-database))
  (get-val (second (assoc :orth (fields-map lexicon))) raw-record cols))

;;
;;
;;

(defmethod dump-psql-lexicon ((lexdb psql-lex-database) filebase &key tdl)
  (format t "~%(dumping LexDB)")
  (force-output)
  (dump-rev lexdb filebase)
  (dump-dfn lexdb filebase)
  (dump-fld lexdb filebase)
  (when tdl (dump-tdl filebase))
  t)

(defmethod dump-rev ((lexdb psql-lex-database) filebase)
  (run-command-stdout lexdb "SELECT * FROM dump_rev(); COPY temp TO stdout" 
		      (namestring (pathname (format nil "~a.rev" 
							      filebase))))
  t)

(defmethod dump-dfn ((lexdb psql-lex-database) filebase)
  (run-command-stdout lexdb "COPY defn TO stdout" 
		      (namestring (pathname (format nil "~a.dfn" 
							      filebase))))
  t)

(defmethod dump-fld ((lexdb psql-lex-database) filebase)
  (run-command-stdout lexdb "COPY fields TO stdout" 
		      (namestring (pathname (format nil "~a.fld" 
							      filebase))))
  t)

(defmethod dump-tdl ((lexdb psql-lex-database) filebase)
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (get-filter lexdb))))))
    (format t "~%(exporting filtered ~a LexDB to TDL file ~a)" (dbname lexdb) tdl-file)
    (force-output)
    (export-to-tdl-to-file lexdb tdl-file)))
