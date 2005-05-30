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
	      :user (sql-fn-get-val ,lexdb :db_owner))))
       (open-lex ,lexdb-lexdb)
       ,@body
       (disconnect ,lexdb-lexdb))))
  
(defmethod lookup-word ((lex psql-lex-database) orth &key (cache t))
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
      (let* ((table (sql-fn-get-records lex 
					:retrieve_entries_by_orthkey 
					:args (list (sql-like-text 
						     (normalize-orthkey orth))) 
					:fields (grammar-fields lex)))
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
	  (get-raw-records *lexdb* "select distinct key from rev_key_all")))

;(defmethod lex-words ((lex psql-lex-database))
;  (let* ((orth-raw-mapping (assoc :orth (dfn lex)))
;	 (orth-raw-value-mapping (fourth orth-raw-mapping))
;	 (raw-orth-field-str (2-str (second orth-raw-mapping)))
;	 (values 
;	  (get-value-set lex raw-orth-field-str)))
;    (mapcan 
;     #'(lambda (x) 
;	 (mapcar #'string-upcase
;		 (car (work-out-value orth-raw-value-mapping x))))
;     values)))

(defmethod rev-key-p ((lex psql-lex-database))
  (string= "t" (sql-fn-get-val lex :rev_key_p)))
  
(defmethod generate-lex-orthkeys-if-nec ((lex psql-lex-database))
  (generate-orthkeys-if-nec lex)
  (with-lexdb-user-lexdb (lex2 lex)
    (generate-orthkeys-if-nec lex2))
  (update-lex lex))

(defmethod generate-orthkeys-if-nec ((lex psql-lex-database))
  (unless (rev-key-p lex)
    (format t "~%(generating orthkeys for user ~a)" (user lex))
    (generate-orthkeys lex)))

(defmethod generate-orthkeys ((lex psql-lex-database) &key (table :rev))
  (sql-fn-get-val lex :register_modstamp)
  (run-command-stdin lex "COPY rev_key FROM stdin"
		     (make-string-input-stream 
		      (generate-orthkeys-COPY-str lex :table table)))
  (sql-fn-get-val lex :register_modstamp))

(defmethod generate-orthkeys-COPY-str ((lex psql-lex-database) &key (table :rev))
  (unless (dfn lex)
    (error "no dfn definitions available"))
  (let* ((orth-raw-mapping (assoc :orth (dfn lex)))
	 (raw-orth-field-str (2-str (second orth-raw-mapping)))
       
	 (numo-t (get-records lex
			      (format nil "SELECT name,userid,modstamp,~a FROM ~a"
				      raw-orth-field-str
				      table)))
	 (recs (recs numo-t)))
    (join-str-lines
     (mapcar #'to-psql-COPY-rec
	     (rev-to-rev-key lex recs)))))

(defmethod rev-to-rev-key ((lex psql-lex-database) recs)
  (let* ((orth-raw-mapping (assoc :orth (dfn lex)))
	 (orth-raw-value-mapping (fourth orth-raw-mapping)))
    (mapcan
     #'(lambda (z) 
	 (mapcar #'(lambda (x)
		     (list (first z) (second z) (third z) x))
		 (mapcar #'normalize-orthkey
			 (car (work-out-value orth-raw-value-mapping (fourth z))))))
     recs)))

(defparameter *newline-str* (string (code-char 10)))
(defun join-str-lines (lines)
  (if (null lines) ""
    (apply #'concatenate 'string
	   (cons (car lines)
		 (loop
		     for line in (cdr lines)
		     collect *newline-str*
		     collect line)))))
      
	 

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
	 for i from 0 to (1- (length str))
	 for char = (aref str i)
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
	 (sql-fn-get-raw-records lex :lex_id_set)))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

;

(defmethod retrieve-all-records ((lex psql-lex-database) &optional reqd-fields)
  (cond
   ((connection lex)
    (sql-fn-get-records lex
			:retrieve_all_entries
			:fields reqd-fields))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))    
    
(defmethod retrieve-raw-record ((lex psql-lex-database) id &key (cache t) reqd-fields)
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

(defmethod retrieve-raw-record-no-cache ((lex psql-lex-database) id &optional reqd-fields)
  (cond 
   ((connection lex)
    (let* ((id-str (symb-2-str id))
	   (column-records (sql-fn-get-raw-records lex
						   :retrieve_entry
						   :fields reqd-fields
						   :args (list (sql-like-text id-str)))))
      (if (> (length column-records) 1)
	  (error (format nil "too many records returned"))
	(first column-records))))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))

(defmethod retrieve-head-record-str ((lex psql-lex-database) id &optional reqd-fields)
  (retrieve-head-record lex (str-2-symb id) reqd-fields))

(defmethod retrieve-head-record ((lex psql-lex-database) id &optional reqd-fields)
  (cond
   ((connection lex)
    (let* ((id-str (2-str id))
	   (table (sql-fn-get-records lex
					:retrieve_entry
					:fields reqd-fields
					:args (list (sql-like-text id-str)))))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~%WARNING: no connection to psql-lex-database"))))

(defmethod grammar-fields ((lex psql-lex-database))
  (unless (dfn lex)
    (complain-no-dfn lex)
    (error "operation aborted"))
  (remove-duplicates 
   (mapcar #'cadr 
	   (dfn lex))
   :test #'equal))

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

(defmethod make-strucargs ((lex psql-lex-database) raw-record cols)
  (let* 
      ((strucslots 
	(loop 
	    for (slot-key slot-field slot-path slot-type) in (dfn lex)
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
	    (format t "~%~%Please wait: recreating database cache for new filter")
	    (force-output)
	    (unless (set-filter-aux lex filter)
	      (format t "~%(LexDB filter unchanged)")
	      (return-from set-filter))
	    nil)
      (lkb-beep)
      (set-filter lex))
    (format t "~%(lexdb filter: ~a )" 
	    (get-filter lex))
    (format t "~%(active lexical entries: ~a )" 
	    (sql-fn-get-val lex :size_lex))))

(defmethod set-filter-aux ((lex psql-lex-database) filter)
  (unless (or (null filter) 
	      (equal (get-filter lex) filter))
    (reconnect lex) ;; must reconnect to avoid server bug...
    (force-output)
    (sql-fn-get-val lex 
		    :update_lex 
		    :args (list filter))
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
	       (get-raw-records lex (format nil "SELECT slot,field,path,type FROM dfn WHERE mode='~a';" (fields-tb lex))))
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :unifs))))
    (if (null dfn)
	(complain-no-dfn lex))
    dfn))

(defmethod complain-no-dfn ((lex psql-lex-database))
  (format t "~%(no dfn entries in ~a)" (dbname lex)))

(defmethod get-internal-table-dfn ((lex psql-lex-database))
      (sql-fn-get-records lex 
			  :return_field_info 
			  :args (list "public" "rev")))

(defmethod get-field-size-map ((lex psql-lex-database))
  (let* ((table (get-internal-table-dfn lex))
	(recs (recs table))
	(cols (cols table)))
    (mapcar 
     #'(lambda (x) (field-size-elt x cols)) 
     recs)))

(defmethod lookup ((lex psql-lex-database) field-kw val-str)
  (let* ((sql-fn (if val-str
		    :lookup_general
		   :lookup_general_null))
	 (field-str (2-str field-kw))
	 (args (if val-str
		   (list field-str (sql-like-text val-str))
		 (list field-str)))
	 (records
	  (sql-fn-get-raw-records lex
				  sql-fn
				  :args args)))
    (mapcar #'car records)))
  
(defmethod complete ((lex psql-lex-database) field-kw val-str)
  (mapcar #'car 
	  (sql-fn-get-raw-records lex 
				  :complete 
				  :args (list (symb-2-str field-kw)
					      (sql-like-text val-str)))))

;; called from pg-interface
(defmethod new-entries ((lex psql-lex-database))
  (let ((records (sql-fn-get-raw-records lex 
				  :rev_new
				  :fields '(:userid :name :modstamp))))
    (cons (list "userid" "name" "modstamp") records)))

(defmethod current-timestamp ((lex psql-lex-database))
  (sql-fn-get-val lex 
		  :current_timestamp))

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
  (set-val psql-le :modstamp "NOW")
  (let* ((symb-list (copy-list (fields lex)))
	 (symb-list (remove :name symb-list))
	 (symb-list (remove-duplicates symb-list))
	 (symb-list (remove-if 
		     #'(lambda (x) (or (null x) 
				       (and (stringp x)
					    (string= x ""))))
		     symb-list
		     :key #'(lambda (x) (retr-val psql-le x))))) 
    (sql-fn-get-val lex :update_entry
		    :args (list (retr-val psql-le :name)
				symb-list
				(ordered-val-list symb-list psql-le)))
    (generate-orthkeys lex :table :tmp)
    (sql-fn-get-val lex :update_entry_2
		    :args (list (retr-val psql-le :name)))
    		 
    (unless
	(check-lex-entry (str-2-symb (retr-val psql-le :name))
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
  (with-slots (lexdb-version semi) lex
    (setf lexdb-version nil)
    (if (next-method-p) (call-next-method))))

(defmethod open-lex ((lex psql-lex-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (dbname host user connection) 
      lex
    (close-lex lex)    
    (format t "~%connecting to lexical database ~a@~a:~a" 
	    dbname host (true-port lex))
    (force-output)
    ;;(setf *lexdb-tmp-lexicon* lex)
    (setf (name lex) name)
    (or (open-lex-aux lex)
	(format t "~%unable to connect to ~s:~%  ~a" dbname
	      (error-msg connection)))))

(defmethod open-lex-aux ((lex psql-lex-database)) 
  (with-slots (dbname host user) 
      lex
    (when (connect lex)
      (format t "~%~tconnected as user ~a" user)
      (format t "~%~topening ~a" dbname)
      (check-psql-server-version lex)
      (check-lexdb-version lex)
      (make-field-map-slot lex)
      (initialize-userschema lex)
      ;;(generate-orthkeys-if-nec lex)
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
    (let ((texts (sql-fn-get-vals lex :check_psql_server_version)))
      (when texts
	(format t "~%WARNING: ~a" (str-list-2-str texts :sep-c #\Newline)))))

(defmethod initialize-lex ((lex psql-lex-database))
  (when (open-lex lex)
    (update-lex lex)))
  
(defmethod vacuum-lex ((lex psql-database) &key verbose)
  (let ((command
	 (if verbose
	     "vacuum full analyze verbose lex"
	   "vacuum full analyze lex")))
    (format t "~%~%Please wait: vacuuming private table")
    (force-output)
    (run-command lex command)))

(defmethod vacuum-public-rev ((lex psql-lex-database) &key verbose)
  (with-slots (dbname host port) lex
    (let ((l2 (make-instance 'psql-database
		:dbname dbname
		:host host
		:port port
		:user (sql-fn-get-val lex :db_owner)))
	  (command
	   (if verbose
	       "vacuum full analyze verbose public.rev"    
	     "vacuum full analyze public.rev")))
      (format t "~%~%Please wait: vacuuming public table")
      (force-output)
      (connect l2)
      (run-command l2 command)
      (disconnect l2))))

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
	    (get-raw-records lex "SELECT * FROM pub_fns()"))))

;;;
;;;
;;;

(defmethod get-db-version ((lex psql-lex-database))
  (caar 
   (get-raw-records lex 
		    "SELECT val FROM public.meta WHERE var='lexdb-version' LIMIT 1")))
    
(defmethod get-filter ((lex psql-lex-database))
  (sql-fn-get-val lex :filter))

(defmethod update-lex ((lex psql-lex-database))
  (update-lex-aux lex)
  (cond
   ((null (semi lex))
    nil)
   ((semi-up-to-date-p lex)
    (format t "~%(loading SEM-I into memory)")
    (unless (mrs::semi-p 
	     (catch :sql-error
	       (mrs::populate-*semi*-from-psql)
	       ))
      (format t "~% (unable to retrieve database SEM-I)"))
    (index-lexical-rules)
    (index-grammar-rules))
   (t
    (format t "~%WARNING: no lexical entries indexed for generator")))
  lex)

(defmethod update-lex-aux ((lex psql-lex-database))
  (reconnect lex) ;; work around server bug
  (cond 
   ((not (user-read-only-p lex (user lex)))
    (sql-fn-get-raw-records lex 
			    :update_lex 
			    :args (list (get-filter lex))))
   (t
    (format t "~%(user ~a has read-only privileges)" (user lex))))    
  (format t "~%(LexDB filter: ~a )" (get-filter lex))
  (let ((size (sql-fn-get-val lex :size_lex)))
    (if (string= "0" size)
	(format t "~%WARNING: 0 entries passed the LexDB filter" size)
      (format t "~%(active lexical entries: ~a )" size)))
  (empty-cache lex))

;;;
;; semi
;;;

(defmethod index-new-lex-entries ((lex psql-lex-database))
  (let ((semi-out-of-date (semi-out-of-date lex)))
    (format t "~%(indexing ~a entries)" (length semi-out-of-date))
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
      (format t "~%No feature structure for ~A~%" 
	      (lex-entry-id entry))))
    (forget-psort lex lexid))

;;;
;;;
;;;

(defmethod get-fields ((lex psql-lex-database))
  (mapcar 
   #'(lambda (x) (intern (string-upcase (car x)) :keyword))
   (sql-fn-get-raw-records lex :list_fld)))

(defmethod user-read-only-p ((lex psql-lex-database) user-str)
  (string= "t" 
	   (sql-fn-get-val lex :user_read_only_p 
				   :args (list user-str))))

(defmethod show-scratch ((lex psql-lex-database))
  (sql-fn-get-raw-records lex 
			  :rev
			  :fields (list :name :userid :modstamp)))

(defmethod scratch-records ((lex psql-lex-database))
  (sql-fn-get-raw-records lex :retrieve_private_revs))

(defmethod merge-into-db ((lex psql-lex-database) rev-filename)  
  (run-command lex "DELETE FROM tmp")
  (run-command lex "DELETE FROM tmp_key")
  (run-command-stdin-from-file lex "COPY tmp FROM stdin" rev-filename)
  (let ((rev-key-filename (concatenate 'string rev-filename "_key")))
    (if (probe-file rev-key-filename)
	(run-command-stdin-from-file lex "COPY tmp_key FROM stdin" rev-key-filename)
      (with-lexdb-user-lexdb (lex2 lex)
	(run-command lex2 "DELETE FROM rev_key"))))
  (let ((count-new
	 (str-2-num
	  (sql-fn-get-val lex :merge_public_rev_rev_key_from_tmp_tmp_key))))
    (format t "~%(~a new rev entries)" count-new)
    (unless (equal 0 count-new)
      (vacuum-public-rev lex))
    (with-lexdb-user-lexdb (lex2 lex)
      (make-field-map-slot lex2)
      (time (generate-orthkeys-if-nec lex2)))
    count-new))

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
    (format t "~%(~a new dfn entries)" count-new-dfn)
    count-new-dfn))

(defmethod initialize-userschema ((lex psql-lex-database))
  (sql-fn-get-val lex :initialize_user_schema )
  #+:mwe
  (if *postgres-mwe-enable*
      (mwe-initialize-userschema lex)))

(defmethod semi-setup-pre ((lex psql-lex-database))  
  (reconnect lex)
  (sql-fn-get-val lex :semi_setup_pre))
  
(defmethod semi-setup-post ((lex psql-lex-database))  
  (reconnect lex)
  (sql-fn-get-val lex :semi_setup_post))
 
(defmethod semi-up-to-date-p ((lex psql-lex-database))  
  (string= "t"
	   (sql-fn-get-val lex :semi_up_to_date_p)))
  
;; returns record-ids
(defmethod semi-out-of-date ((lex psql-lex-database))
  (with-slots (record-cache) lex
    (let* ((cols (grammar-fields lex))
	   (table (sql-fn-get-records lex 
				      :semi_out_of_date
				      :fields cols))
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
  (mapcar #'car 
	  (sql-fn-get-raw-records lex
				  :value_set
				  :args (list (2-str field)))))

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
  (with-lexdb-user-lexdb (lexdb2 lex)
    (let ((count-new 0))
      (let* ((rev-filename (absolute-namestring "~a.rev" filename))
	     (dfn-filename (absolute-namestring "~a.dfn" filename)))
	(if (probe-file rev-filename)
	    (setf count-new (merge-into-db lexdb2 rev-filename))
	  (format t "~%WARNING: no file ~a" rev-filename))
	(cond
	 ((probe-file dfn-filename)
	  (merge-dfn lexdb2 dfn-filename)
	  (make-field-map-slot lex))
	 (t
	  (format t "~%WARNING: no file ~a" dfn-filename)))
	nil)
      (if (equal count-new 0)
	  (empty-cache lex)
	(initialize-lexdb)))))

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
	     :user (sql-fn-get-val lex :db_owner))))
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
	(format t "Merge new .dfn entries aborted..."))
      (empty-cache lex)
      (disconnect conn-db-owner))))


(defmethod to-db-dump ((x lex-entry) (lex psql-lex-database))
  "provide line entry for lex db import file"
  (with-slots (dfn fields) lex
    (let* ((s (copy-slots x dfn))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lex))))
	   (field-vals (append
			(mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x dfn)))
			 extraction-fields)
			(list
			 ;;(cons :orthkey (orthkey x))
			 (cons :userid *lexdb-dump-user*)
			 (cons :modstamp "NOW")
			 (cons :lang *lexdb-dump-lang*)
			 (cons :country *lexdb-dump-country*)
			 (cons :confidence 1)
			 (cons :source *lexdb-dump-source*)
			 (cons :dead "f"))))
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

(defmethod to-db ((x lex-entry) (lex psql-lex-database))
  "insert lex-entry into lex db (user scratch space)"
  (with-slots (dfn) lex
    (let* ((s (copy-slots x dfn))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lex))))
	   (extracted-fields
	    (mapcan 
	     #'(lambda (x) (list x (extract-field s x dfn)))
	     extraction-fields))
	 
	   (psql-le
	    (apply #'make-instance-psql-lex-entry
		   (append extracted-fields
			   (list :country *lexdb-dump-country*
				 :lang *lexdb-dump-lang*
				 :source (extract-pure-source-from-source *lexdb-dump-source*)
				 :confidence 1
				 :dead "f"
				 )))))
      (cond
       ((null (cdr (assoc :unifs s)))
	(set-lex-entry lex psql-le)
	(empty-cache lex))
       (t
       (format t "~%skipping super-rich entry:~%~a" (to-tdl x))
       nil)))))
  
;; not suited to batch import!
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
  (sql-fn-get-val lex :dump_public_rev_rev_key_to_tmp_tmp_key)
  (run-command-stdout-to-file lex "COPY tmp TO stdout" 
			      (namestring (pathname (format nil "~a.rev" 
							    filebase))))
  (run-command-stdout-to-file lex "COPY tmp_key TO stdout" 
			      (namestring (pathname (format nil "~a.rev_key" 
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
    (format t "~%(exporting filtered ~a LexDB to TDL file ~a)" (dbname lexdb) tdl-file)
    (force-output)
    (export-to-tdl-to-file lexdb tdl-file)))

(defmethod commit-private-rev ((lex psql-lex-database)) 
  (with-lexdb-user-lexdb (lex2 lex)
    (sql-fn-get-val lex2 :commit_rev :args (list (user lex))))
  (sql-fn-get-records lex :clear_rev)
  (empty-cache lex))

(defmethod close-private-rev ((lex psql-lex-database))
  (sql-fn-get-records lex :clear_rev)
  (empty-cache lex)
  (reconnect lex) ;; work around server bug
  (sql-fn-get-records lex 
		      :update_lex 
		      :args (list (get-filter lex))))

