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

#+:null
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
    out))
  
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

;;erg=> explain analyze select * from rev_all natural join (select name,userid,modstamp from lex_key WHERE lex_key.key LIKE 'dog') as t1;
;;                                                                                                                                                             QUERY PLAN                                                                                                                                  
;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Merge Join  (cost=36604.69..39658.92 rows=1 width=909) (actual time=1490.495..1490.594 rows=2 loops=1)
;;   Merge Cond: (("outer".name = "inner".name) AND ("outer".userid = "inner".userid) AND ("outer".modstamp = "inner".modstamp))
;;   ->  Subquery Scan rev_all  (cost=36590.70..39409.95 rows=31325 width=909) (actual time=1240.844..1464.493 rows=7969 loops=1)
;;         ->  Unique  (cost=36590.70..39096.70 rows=31325 width=909) (actual time=1240.809..1363.590 rows=7969 loops=1)
;;               ->  Sort  (cost=36590.70..36669.01 rows=31325 width=909) (actual time=1240.800..1270.136 rows=7969 loops=1)
;;                     Sort Key: name, userid, modstamp, dead, "type", orthography, keyrel, altkey, alt2key, keytag, altkeytag, compkey, ocompkey, pronunciation, complete, semclasses, preferences, classifier, selectrest, jlink, comments, exemplars, usages, lang, country, dialect, domains, genres, register, confidence, source
;;                     ->  Append  (cost=0.00..1301.50 rows=31325 width=909) (actual time=0.052..805.735 rows=31245 loops=1)
;;                           ->  Subquery Scan "*SELECT* 1"  (cost=0.00..1289.90 rows=31245 width=612) (actual time=0.045..615.338 rows=31245 loops=1)
;;                                 ->  Seq Scan on rev  (cost=0.00..977.45 rows=31245 width=612) (actual time=0.016..120.485 rows=31245 loops=1)
;;                           ->  Subquery Scan "*SELECT* 2"  (cost=0.00..11.60 rows=80 width=909) (actual time=0.011..0.011 rows=0 loops=1)
;;                                 ->  Seq Scan on rev  (cost=0.00..10.80 rows=80 width=909) (actual time=0.003..0.003 rows=0 loops=1)
;;   ->  Sort  (cost=13.99..14.00 rows=3 width=31) (actual time=0.158..0.164 rows=2 loops=1)
;;         Sort Key: lex_key.name, lex_key.userid, lex_key.modstamp
;;         ->  Index Scan using lex_key_key on lex_key  (cost=0.00..13.97 rows=3 width=31) (actual time=0.070..0.084 rows=2 loops=1)
;;               Index Cond: ("key" = 'dog'::text)
;;               Filter: ("key" ~~ 'dog'::text)
;; Total runtime: 1494.072 ms

;;erg=> explain analyze select * from public.rev natural join (select name,userid,modstamp from lex_key WHERE lex_key.key LIKE 'dog') as t1 union select * from rev natural join (select name,userid,modstamp from lex_key WHERE lex_key.key LIKE 'dog') as t1 ;
;;                                                                                                                                                       QUERY PLAN                                                                                                                                        
;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Unique  (cost=50.72..50.88 rows=2 width=909) (actual time=0.564..0.603 rows=2 loops=1)
;;   ->  Sort  (cost=50.72..50.72 rows=2 width=909) (actual time=0.556..0.564 rows=2 loops=1)
;;         Sort Key: name, userid, modstamp, dead, "type", orthography, keyrel, altkey, alt2key, keytag, altkeytag, compkey, ocompkey, pronunciation, complete, semclasses, preferences, classifier, selectrest, jlink, comments, exemplars, usages, lang, country, dialect, domains, genres, register, confidence, source
;;         ->  Append  (cost=0.00..50.71 rows=2 width=909) (actual time=0.161..0.413 rows=2 loops=1)
;;               ->  Subquery Scan "*SELECT* 1"  (cost=0.00..23.50 rows=1 width=612) (actual time=0.154..0.281 rows=2 loops=1)
;;                     ->  Nested Loop  (cost=0.00..23.49 rows=1 width=612) (actual time=0.129..0.224 rows=2 loops=1)
;;                           Join Filter: (("inner".userid = "outer".userid) AND ("inner".modstamp = "outer".modstamp))
;;                           ->  Index Scan using lex_key_key on lex_key  (cost=0.00..13.97 rows=3 width=31) (actual time=0.055..0.072 rows=2 loops=1)
;;                                 Index Cond: ("key" = 'dog'::text)
;;                                 Filter: ("key" ~~ 'dog'::text)
;;                           ->  Index Scan using rev_name on rev  (cost=0.00..3.14 rows=2 width=612) (actual time=0.024..0.031 rows=1 loops=2)
;;                                 Index Cond: (rev.name = "outer".name)
;;               ->  Subquery Scan "*SELECT* 2"  (cost=13.99..27.21 rows=1 width=909) (actual time=0.110..0.110 rows=0 loops=1)
;;                     ->  Hash Join  (cost=13.99..27.20 rows=1 width=909) (actual time=0.102..0.102 rows=0 loops=1)
;;                           Hash Cond: (("outer".name = "inner".name) AND ("outer".userid = "inner".userid) AND ("outer".modstamp = "inner".modstamp))
;;                           ->  Seq Scan on rev  (cost=0.00..10.80 rows=80 width=909) (actual time=0.004..0.004 rows=0 loops=1)
;;                           ->  Hash  (cost=13.97..13.97 rows=3 width=31) (actual time=0.064..0.064 rows=0 loops=1)
;;                                 ->  Index Scan using lex_key_key on lex_key  (cost=0.00..13.97 rows=3 width=31) (actual time=0.028..0.043 rows=2 loops=1)
;;                                       Index Cond: ("key" = 'dog'::text)
;;                                       Filter: ("key" ~~ 'dog'::text)
;; Total runtime: 1.292 ms

;; (TIMINGS FOR POSTGRESQL 8.0)

;; orthkey must be mapped to normalized form before entering PSQL universe
(defmethod lookup-word-no-cache ((lex psql-lex-database) orth)
  (declare (ignore cache))
  (if (connection lex)
      (let* ((quoted-literal (psql-quote-literal (sql-like-text (normalize-orthkey orth))))
	     (table 
	      (get-records lex
			   (format nil "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a UNION SELECT rev.* FROM rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a) as foo" 
				   ;;
				   ;;
				   (fields-str lex (grammar-fields lex))
				   quoted-literal quoted-literal)))
	     (ids (lookup-word-aux2 lex table)))
	ids)))

(defmethod lookup-word-aux2 ((lex psql-lex-database) table)
  (with-slots (psorts record-cache dfn) lex
    (let ((name-field (second (assoc :ID dfn))))
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
	  (get-raw-records lex "select distinct key from lex_key")))

(defmethod put-normalized-lex-keys ((lex psql-lex-database) recs)
  (when recs
    (let ((conn (connection lex)))
      (with-lexdb-client-min-messages (lex "error")
	(run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
      (pq:exec conn "COPY lex_key FROM stdin")
      (loop
	  for rec in recs
	  do 
	    (with-lexdb-locale (pq:putline conn (to-psql-copy-rec2 rec))))
      (with-lexdb-locale (putline conn "\\."))
      (endcopy conn)
      (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)")
      )))

(defparameter *newline-str* (string (code-char 10)))
(defun to-psql-COPY-rec2 (lst &key (delim-char #\tab) (null "\\N"))
  (cond
   ((null lst)
    "")
   (t
    (apply #'concatenate 'string 
	   (append (list (psql-COPY-val (car lst) :delim-char delim-char :null null))
		   (loop
		       with delim = (string delim-char)
		       for x in (cdr lst)
		       collect delim
		       collect (psql-COPY-val x :delim-char delim-char :null null))
		   (list *newline-str*))))))
     
(defun normalize-orthkeys (recs)
  (loop
      for rec in recs
      do (setf (fourth rec) (normalize-orthkey! (fourth rec))))
  recs)

(defmethod get-unnormalized-lex-keys ((lex psql-lex-database))
  (recs
   (get-records lex "SELECT name,userid,modstamp,key FROM lex_key")))

(defmethod create-unnormalized-lex-keys ((lex psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
  (run-command lex "DELETE FROM lex_key") 
  (loop
      with i = 1
      while (> (run-command lex (format nil "insert into lex_key select * from (select name,userid,modstamp,split_part(~a,' ',~a) as key from lex_cache) as foo where key!=''" (orth-field lex) i)) 0)
      do (incf i)))

(defmethod regenerate-orthkeys ((lex psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
  (run-command lex "DELETE FROM lex_key")
  (generate-missing-orthkeys lex)
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)" :ignore-errors t)))

(defmethod create-unnormalized-missing-lex-keys3 ((lex psql-lex-database))
  (recs 
   (get-records lex 
		(format nil "select name,userid,modstamp,~a from lex_cache left join lex_key using (name,userid,modstamp) where lex_key.key is null" 
			(orth-field lex)))))
  
(defmethod generate-missing-orthkeys ((lex psql-lex-database))
  (put-normalized-lex-keys lex
			   (normalize-orthkeys (create-unnormalized-missing-lex-keys3 lex))))
  
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
	 (get-raw-records lex "SELECT DISTINCT name FROM lex_cache")))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

;

(defmethod retrieve-all-records ((lex psql-lex-database) &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((reqd-fields (fields-str lex reqd-fields))
	  (recs (get-records lex (format nil "SELECT ~a FROM lex" reqd-fields))))
      recs))
   (t
    (format t "~&(LexDB) WARNING: no connection to psql-lex-database"))))

;(defmethod retrieve-all-records ((lex psql-lex-database) &optional (reqd-fields '("*")))
;  (cond
;   ((connection lex)
;    (let* ((reqd-fields (fields-str lex reqd-fields))
;	  (public (get-records lex (format nil "SELECT ~a FROM public.rev" reqd-fields)))
;	  (private (get-records lex (format nil "SELECT ~a FROM rev" reqd-fields))))
;      (make-instance 'psql-database-table 
;	:recs (append (recs public)
;		      (recs private))
;	:cols (cols public))))
;   (t
;    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

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
		       (fields-str lex reqd-fields)
		       (psql-quote-literal name))))

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
				 (fields-str lex reqd-fields)
				 (psql-quote-literal id)
				 (psql-quote-literal name)
				 (psql-quote-literal modstamp)))))
      
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
    (when (member :|_tdl| (fields lex))
      (pushnew :|_tdl| g-fields))
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
		 (remove-duplicates (mapcar #'first (dfn lex))))))
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
    (let ((unifs (cdr (assoc :UNIFS strucargs)))
	  (id (cadr (assoc :ID strucargs)))
	  (orth (cadr (assoc :ORTH strucargs))))
      ;; if using :|_tdl| field (undecomposed TDL) the raw tdl contributes to lex entry
      (when (member :|_tdl| (fields lex))
	(setf unifs 
	  (append unifs (tdl-to-unifs (get-val :|_tdl| raw-record cols)))))
      ;; finally, build the list of arguments
      (list :UNIFS unifs
	    :ID id
	    :ORTH orth
	    :INFL-POS (and (> (length orth) 1)
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
	    (format t "~&(LexDB) recreating 'lex'")
	    (force-output)
	    (reconnect lex)
	    (unless (update-filter lex filter)
	      (format t "~&(LexDB) filter unchanged")
	      (return-from set-filter))
	    nil)
      (lkb-beep)
      (set-filter lex))
    (format t "~&(LexDB) filter: ~a" 
	    (get-filter lex))
    (format t "~&(LexDB) active entries in 'lex': ~a" 
	    (count-lex lex)
	    )))

(defmethod count-lex ((lex psql-lex-database))
  (let ((size-lex-pub (sql-get-num lex "SELECT count(*) FROM public.rev JOIN lex_cache USING (name,userid,modstamp)"))
	(size-lex-priv (sql-get-num lex "SELECT count(*) FROM rev JOIN lex_cache USING (name,userid,modstamp)")))
    (+ size-lex-pub size-lex-priv)))

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
		   (let* ((slot (str-2-keyword (string-upcase (first x))))
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
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :UNIFS))))
    (if (null dfn)
	(complain-no-dfn lex))
    dfn))

(defmethod complain-no-dfn ((lex psql-lex-database))
  (error "~&(LexDB) no dfn entries found in ~a !!!" (dbname lex)))

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
			      (psql-quote-literal val-str))))
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
	(ql-val (psql-quote-literal (format nil "~a%" val-str))))
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
  (caar (get-raw-records lex "SELECT current_timestamp")))

;;;
;;; low-level
;;;

(defmethod set-lex-entry-from-record ((lex psql-lex-database) fv-pairs)
  (set-lex-entry lex (make-instance 'psql-lex-entry :fv-pairs fv-pairs)))

;;; insert lex entry into db
(defmethod set-lex-entry ((lex psql-lex-database) (psql-le psql-lex-entry) &key (gen-key t))
  (set-val psql-le :|orthkey| (lexicon-le-orthkey lex psql-le))
  (set-val psql-le :|modstamp| "NOW")
  (set-val psql-le :|userid| (user lex))
  (set-lex-entry-aux lex psql-le :gen-key gen-key))
  
(defmethod set-lex-entry-aux ((lex psql-lex-database) (psql-le psql-lex-entry) &key (gen-key t) )
  (let* ((symb-list (copy-list (fields lex)))
	 (symb-list (remove-duplicates symb-list))
	 (symb-list (remove-if 
		     #'(lambda (x) (or (null x) 
				       (and (stringp x)
					    (string= x ""))))
		     symb-list
		     :key #'(lambda (x) (retr-val psql-le x))))
	 (name (retr-val psql-le :|name|))
	 (lexid (str-2-symb name)))
    (unless (string= name (symb-2-str lexid))
      (format t "(LexDB) WARNING: lex id ~a should be written ~a" 
	      name (symb-2-str (str-2-symb name)))
      (lkb-beep)
      (setf name (symb-2-str (str-2-symb name))))
    (update-entry lex symb-list psql-le)
  (let ((*empty-cache-clears-generator-lexicon* nil))
    (empty-cache lex))
    (when gen-key 
      (generate-missing-orthkeys lex))
    
    (mrs::delete-lexid-from-generator-indices lexid)
    (if (read-psort lex lexid) 
	(if (check-lex-entry lexid lex)
	    (progn
	      (add-lexid-to-generator-indices lex lexid)
	      t)
	  (error "Invalid lexical entry ~a -- see Lisp buffer output" lexid))
      t)))

;;;
;;; postgres interface
;;;

	     
;;;
;;; script file fn
;;;

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
      (unless
	  (sql-get-bool lex "SELECT user_is_db_owner_p()")
	(make-field-map-slot lex)
	(unless (sql-get-bool lex "SELECT (user IN (SELECT val FROM public.meta WHERE var='user'))")
	  (initialize-user-schema lex)
	  (run-command lex "INSERT INTO meta VALUES ('hack','lex-cache')"))
	(unless (sql-get-bool lex "SELECT ('lex-cache' IN (SELECT val FROM meta WHERE var='hack'))")
	  (format t "~%(LexDB) [automatically updating user schema for compatibility with current LKB]")
	  (build-lex lex)
	  (run-command lex "INSERT INTO meta VALUES ('hack','lex-cache')")
	  (when (string= "4.8" (compat-version (lexdb-version lex)))
	    (with-lexdb-user-lexdb (l lex)
	      (run-command l "UPDATE public.meta SET val='4.90' WHERE var='lexdb-version'")))))
      lex)))

(defmethod check-lexdb-version ((lex psql-lex-database))
  (with-slots (lexdb-version) 
      lex
    (when (and
	   (string= *lexdb-major-version* "4.9")
	   (string= (compat-version lexdb-version) "4.8"))
      (return-from check-lexdb-version)) ;; we ensure compatibility via "lex_cache" hack
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
    t))	

;;;
;;;
;;;

(defmethod get-db-version ((lex psql-lex-database))
  (caar 
   (get-raw-records lex 
		    "SELECT val FROM public.meta WHERE var='lexdb-version' LIMIT 1")))
    
(defmethod get-filter ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))  

(defun clear-psql-semi (&key (lex *lexdb*))
  (unless (typep lex 'psql-lex-database)
    (error "psql-lex-database expected"))
  (semi-drop-indices lex)
  (run-command lex "DELETE FROM semi_pred")
  (run-command lex "DELETE FROM semi_frame")
  (run-command lex "DELETE FROM semi_var")
  (run-command lex "DELETE FROM semi_extra")
  (run-command lex "DELETE FROM semi_mod"))

(defmethod update-lex ((lex psql-lex-database))
  (unless (quick-load lex)
    (update-lex-aux lex))
  (when (semi lex)
    (format t "~%(LexDB) WARNING: :SEMI argument to *lexdb-params* is now obsolete")
    (format t "~%(LexDB)          (please call index-for-generator instead)"))
  lex)

(defmethod count-rev-all ((lex psql-lex-database))
  (sql-get-num lex "SELECT (SELECT count(*) FROM public.rev) + (SELECT count(*) FROM rev)"))

(defmethod update-lex-aux ((lex psql-lex-database))
  (reconnect lex) ;; work around server bug
  (lexdb-time 
   ("ensuring 'lex' is up-to-date" "done ensuring 'lex' is up-to-date")
   (update-filter lex nil))
  (let ((size (count-lex lex))
	(rev-size (count-rev-all lex)))
    (format t "~&(LexDB) total 'rev' entries available: ~a" rev-size)
    (when (= 0 rev-size)
      (format t " !!! PLEASE LOAD REV ENTRIES !!!")
      (lkb-beep))
    (format t "~&(LexDB) active entries in 'lex': ~a" size)
    (when (= 0 size)
      (format t " !!! PLEASE SET FILTER !!!")
      (lkb-beep))
  (format t "~&(LexDB) filter: ~a " (get-filter lex))
  (when (string= "NULL" (string-upcase (get-filter lex)))
    (format t "!!! PLEASE SET FILTER !!!")
    (lkb-beep)))
  (empty-cache lex))

(defmethod filter ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))

(defmethod new-public-rev ((lex psql-lex-database))
  (get-raw-records lex "select name, userid, modstamp from public.rev where modstamp>(select val from meta where var='build_time') and userid != user"))

(defmethod update-filter ((lex psql-lex-database) new-filter)
  (let ((old-filter (filter lex)))
    (cond
     ((and new-filter
	   (not (string= new-filter old-filter)))  
      (run-command lex 
		   (format nil "CREATE OR REPLACE VIEW filt AS SELECT * FROM rev_all WHERE ~a" new-filter))
      (run-command lex 
		   (format nil "UPDATE meta SET val=~a WHERE var='filter'" 
			   (psql-quote-literal new-filter)))
      (build-lex lex)
      t)
     ((< 0 (run-command lex (format nil "INSERT INTO lex_cache select name, userid, modstamp, ~a from public.rev where modstamp>(select val from meta where var='build_time') and userid != user" (orth-field lex))))
      (generate-missing-orthkeys lex)
      (register-build-time lex)
      t)
     (t
      nil))))

(defmethod build-lex ((lex psql-lex-database))
  (let* (
	 (built-in-fields '(:|name| :|userid| :|modstamp| :|dead|))
	 (lex-fields (append built-in-fields (set-difference (grammar-fields lex) built-in-fields)))
	 (lex-fields-str (fields-str lex lex-fields))
	 )
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "DROP TABLE tmp_filt_cache CASCADE" :ignore-errors t))
    (run-command lex 
		 (format nil "CREATE TABLE tmp_filt_cache AS SELECT name,userid,modstamp,dead,~a FROM filt" (orth-field lex)))
    
    (run-command lex "CREATE INDEX tmp_filt_cache_name_modstamp ON tmp_filt_cache (name,modstamp)")
    ;; drop lex table, if it exists (eg. old lexdb)
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "DROP TABLE lex CASCADE" :ignore-errors t))
    ;; drop lex_cache table, if it exists
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "DROP TABLE lex_cache CASCADE" :ignore-errors t))
    (run-command lex 
		 (format nil 
			 "CREATE TABLE lex_cache AS SELECT name,userid,modstamp,~a FROM (tmp_filt_cache JOIN (SELECT name, max(modstamp) AS modstamp FROM tmp_filt_cache GROUP BY name) AS t1 USING (name,modstamp)) WHERE dead='0'" (orth-field lex)))
    (run-command lex "CREATE INDEX lex_cache_name_userid_modstamp ON lex_cache (name, userid, modstamp)")
    
    (run-command lex (format nil "CREATE OR REPLACE VIEW lex AS SELECT rev_all.* FROM lex_cache JOIN rev_all USING (name,userid,modstamp)" lex-fields-str))
    
    ;;
    (run-command lex "DROP INDEX tmp_filt_cache_name_modstamp")
    (run-command lex "DROP TABLE tmp_filt_cache")
    
    ;;
    (run-command lex "DROP INDEX lex_key_key")
    (run-command lex "DELETE FROM lex_key")
    (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)"))
  ;;
  (register-build-time lex)
  
  ;;
  (generate-missing-orthkeys lex)
  (vacuum lex)
  (empty-cache lex)
  t)

(defmethod register-user-schema ((lex psql-lex-database) user)
  (with-lexdb-user-lexdb (lex-o lex)
   (run-command lex-o 
		(format nil "INSERT INTO public.meta VALUES ('user',~a)"
			(psql-quote-literal user)))))

(defmethod initialize-user-schema ((lex psql-lex-database))
  (cond
   ((sql-get-bool lex "SELECT (user IN (SELECT val FROM public.meta WHERE var='user'))")
    (format t "~&(LexDB) user schema already registered!")
    nil)
   ((sql-get-bool lex "SELECT user_is_db_owner_p()")
    (error "~&(LexDB) db owner is not allowed to initialize user schema!")
    nil)
   (t
  ;;
  (let* ((user (sql-get-val lex "SELECT user"))
	 (qi-user (quote-ident lex user)))
    (run-command lex (format nil "CREATE SCHEMA ~a" qi-user))
    (run-command lex (format nil "GRANT USAGE ON SCHEMA ~a TO lexdb" qi-user))
    (register-user-schema lex user)
    
    ;;
    (run-command lex "CREATE TABLE meta AS SELECT * FROM public.meta WHERE NULL")
    (run-command lex "INSERT INTO meta VALUES ('filter','NULL')")
    (run-command lex "INSERT INTO meta VALUES ('mod_time','')")
    (run-command lex "INSERT INTO meta VALUES ('build_time','')")

    ;;
    (run-command lex "CREATE TABLE tmp AS SELECT * FROM public.rev WHERE NULL")
    (run-command lex "GRANT SELECT ON tmp TO lexdb")
    
    (run-command lex "CREATE TABLE rev AS SELECT * FROM public.rev WHERE NULL")
    (run-command lex "GRANT SELECT ON rev TO lexdb")
    (run-command lex "CREATE UNIQUE INDEX rev_name ON rev (name)")
    
     (create-view-rev-all lex)
   ;;
    (run-command lex 
		 (format nil "CREATE TABLE lex_cache AS SELECT name,userid,modstamp,~a FROM public.rev WHERE NULL" (orth-field lex)))
    (run-command lex "CREATE OR REPLACE VIEW lex AS SELECT rev_all.* FROM lex_cache JOIN rev_all USING (name,userid,modstamp)")
    (create-table-lex-key lex)
    (index-lex-key lex)
    
    ;;
    (register-mod-time lex)
    (register-build-time lex)

    ;;
    (create-view-filt lex)
    (create-view-head lex)
    
    ;;
    (create-tables-semi lex)
    (semi-create-indices lex))
  t)))



(defmethod create-tables-semi ((lex psql-lex-database))
  (run-command lex "
CREATE TABLE semi_pred (
lex_id text NOT NULL,
pred_id text NOT NULL,
frame_id int NOT NULL,
pred_txt text NOT NULL,
string_p boolean NOT NULL
);

CREATE TABLE semi_frame (
frame_id int NOT NULL,
slot text NOT NULL,
str text,
symb text,
var_id int,
type text
);

CREATE TABLE semi_var (
var_id int NOT NULL,
extra_id int NOT NULL
);

CREATE TABLE semi_extra (
extra_id int NOT NULL,
feat text NOT NULL,
val text NOT NULL
);

CREATE TABLE semi_mod (
name text,
userid text,
modstamp TIMESTAMP WITH TIME ZONE,
modstamp0 TIMESTAMP WITH TIME ZONE
);
	
CREATE OR REPLACE VIEW semi_obj AS
SELECT lex_id,pred_id, slot, str, type, feat, val FROM
semi_pred NATURAL JOIN
semi_frame NATURAL LEFT JOIN
semi_var NATURAL LEFT JOIN
semi_extra;
"))
  
(defmethod semi-create-indices ((lex psql-lex-database))
  (run-command lex "
CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
CREATE UNIQUE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
"))
  
(defmethod semi-drop-indices ((lex psql-lex-database))
  (run-command-coe lex "DROP INDEX semi_pred_lex_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_pred_pred_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_frame_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_var_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_extra_extra_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_mod_name_userid_modstamp CASCADE"))

#+:null
(defun rehash (hash &key (test))
  (loop
      with h = 
	(if test (make-hash-table :test test)
	  (make-hash-table))
      for key being each hash-key in hash
      for val being each hash-value in hash
      do
	(setf (gethash key h) val)
      finally
	(return h)))

(defmethod index-public-rev ((lex psql-lex-database))
  (run-command lex "
ALTER TABLE public.rev ADD PRIMARY KEY (name,userid,modstamp);
CREATE UNIQUE INDEX name_modstamp ON public.rev (name,modstamp); 
CREATE INDEX rev_name_modstamp ON public.rev (name, modstamp);
SELECT if_psql_server_version(\'7.4\', \'CREATE INDEX rev_name_pattern ON public.rev (name varchar_pattern_ops)\', \'CREATE INDEX rev_name_pattern ON public.rev (name)\');
CREATE INDEX rev_name
	ON public.rev (name varchar_ops); 
"))
  
(defmethod deindex-public-rev ((lex psql-lex-database))
  (run-command lex "DROP INDEX name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name_pattern" :ignore-errors t)
  (run-command lex "ALTER TABLE public.rev DROP CONSTRAINT rev_pkey" :ignore-errors t))
  
(defmethod create-view-head ((lex psql-lex-database))
  (run-command lex "CREATE VIEW head AS SELECT fil.* FROM (filt AS fil NATURAL JOIN (SELECT name, max(modstamp) AS modstamp FROM filt GROUP BY name) AS t1) WHERE dead='0'"))
  
(defmethod create-view-filt ((lex psql-lex-database))
  (run-command lex "CREATE VIEW filt AS SELECT * FROM rev_all WHERE NULL"))
  
(defmethod create-view-rev-all ((lex psql-lex-database))
  (run-command lex "CREATE VIEW rev_all AS SELECT * FROM public.rev UNION SELECT * FROM rev"))
  
(defmethod create-table-lex-key ((lex psql-lex-database))
  (run-command lex "CREATE TABLE lex_key (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		key text NOT NULL
		)"))
  
(defmethod index-lex-key ((lex psql-lex-database))
  (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)"))

(defmethod deindex-lex-key ((lex psql-lex-database))
  (run-command lex "DROP INDEX lex_key_key"))  

(defmethod register-build-time ((lex psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='build_time'"))

(defmethod register-mod-time ((lex psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='mod_time'"))

(defmethod mod-time-private ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='mod_time'"))

(defmethod mod-time-public ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM public.meta WHERE var='mod_time'"))

(defmethod build-time ((lex psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='build_time'"))

(defmethod merge-public-rev-from-tmp ((lex psql-lex-database))
  (sql-get-bool lex "SELECT assert_db_owner()")
  
  (run-command lex "CREATE INDEX tmp_name_userid_modstamp on public.tmp (name, userid, modstamp)")
  (let ((num-dups (sql-get-num lex "SELECT count(*) FROM (SELECT name,userid,modstamp, count(*) FROM public.tmp GROUP BY name,userid,modstamp HAVING count(*)>1) AS foo"))
	count-new)
    (unless (= 0 num-dups)
      (error "Entries to merge contain ~a duplicated instance(s) of <name,userid,modstamp>" num-dups))
    (run-command lex "DELETE FROM tmp WHERE (name,userid,modstamp) IN (SELECT name,userid,modstamp FROM rev)")
    (run-command lex "DROP INDEX tmp_name_userid_modstamp")
    (setf count-new (sql-get-num lex "SELECT count(*) FROM tmp"))
    (unless (= 0 count-new)
      (deindex-public-rev lex)
      (run-command lex "INSERT INTO public.rev SELECT * FROM tmp")
      (index-public-rev lex)
      (register-mod-time lex))))

(defmethod merge-dfn-from-tmp-dfn ((lex psql-lex-database))
  (sql-get-bool lex "SELECT assert_db_owner()")
  
  ;;
  (run-command lex "DELETE FROM dfn WHERE mode IN (SELECT DISTINCT mode FROM tmp_dfn)")
  (run-command lex "INSERT INTO dfn SELECT * FROM tmp_dfn"))
  
(defmethod dump-public-rev-to-tmp ((lex psql-lex-database))
  (run-command lex "SET TIME ZONE 00")
  (run-command lex "DELETE FROM tmp")
  (run-command lex "INSERT INTO tmp SELECT * FROM public.rev ORDER BY name,userid,modstamp"))

(defmethod update-entry ((lex psql-lex-database) symb-list psql-le)
  (let ((ql-name (psql-quote-literal (retr-val psql-le :|name|))))
    (run-command lex 
		 (format nil "DELETE FROM rev WHERE name=~a" ql-name))
    (run-command lex 
		 (format nil "INSERT INTO rev ~a VALUES ~a" 
			 (sql-list symb-list 
				   #'(lambda (x) (quote-ident lex x)))
			 (sql-list (ordered-val-list symb-list psql-le) 
				   #'(lambda (x) (psql-quote-literal x)))))
    (run-command lex 
		 (format nil "DELETE FROM lex_cache WHERE name=~a" ql-name))
    (run-command lex 
		 (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name = ~a" (orth-field lex) ql-name))
    (run-command lex 
		 (format nil "DELETE FROM lex_key WHERE name = ~a" ql-name))))
  
(defmethod semi-mod-time-private ((lex psql-lex-database) psql-le)
  (sql-get-val 
   lex 
   (format nil "SELECT modstamp FROM semi_mod WHERE ~a=~a" 
	   (sql-list '(:|name| :|userid| :|modstamp|) 
		     #'(lambda (x) (quote-ident lex x)))
	   (sql-list (ordered-val-list '(:|name| :|userid| :|modstamp|) psql-le) 
		     #'(lambda (x) (psql-quote-literal x))))))

;;;
;; semi
;;;

#+:null
(defun truncate-list! (l n)
  (cond
   ((< n 1)
    nil)
   ((> n (length l))
    l)
   (t
    (setf (cdr (nthcdr (1- n) l)) nil)
    l)))

(defvar mrs::*semantic-table*)
(defmethod index-new-lex-entries ((lex psql-lex-database))
  (let ((semi-out-of-date (semi-out-of-date lex)))
    (format t "~&(LexDB) indexing ~a entries" (length semi-out-of-date))
    (when semi-out-of-date
      (loop for x in semi-out-of-date
	  do (update-lisp-semi-entry lex x))
      #+:null ;; fix_me (avoid duplicate sdb rows)
      (mrs::update-psql-semi semi-out-of-date 
			     :lex lex
			     :semantic-table mrs::*semantic-table*))))
  
(defmethod update-lisp-semi-entry ((lex psql-lex-database) lexid)
  (mrs::delete-lexid-from-generator-indices lexid) ;!!
  (add-lexid-to-generator-indices lex lexid))

(defmethod add-lexid-to-generator-indices ((lex psql-lex-database) lexid)
  (let* ((entry (read-psort lex lexid :cache nil))
	 (new-fs (and
		  (expand-psort-entry entry)
		  (lex-entry-full-fs entry))))
    (if (and new-fs 
	     (not (eq new-fs :fail)))
	(mrs::extract-lexical-relations entry) ; <-- efficiency problem originates in here
      (format t "~&No feature structure for ~A~%" 
	      (lex-entry-id entry))))
    (forget-psort lex lexid))

;;;
;;;
;;;

(defmethod get-fields ((lex psql-lex-database))
  (mapcar 
   #'(lambda (x) (intern x :keyword))
   (list-fld lex)))

(defmethod show-scratch ((lex psql-lex-database))
  (get-raw-records lex "SELECT name,userid,modstamp FROM rev"))

(defmethod scratch-records ((lex psql-lex-database))
  (get-raw-records lex "SELECT * FROM rev"))

(defmethod merge-rev ((lex psql-lex-database) rev-filename)
  (let (count-new)
    ;; empty temporary tables
    (with-lexdb-user-lexdb (lex2 lex)
      (run-command lex2 "DELETE FROM tmp")
      ;; vacuum at start
      (vacuum lex2)
      ;;;
      ;; populate temporary tables
      ;;;
      (lexdb-time ("populating temporary tables" "done populating temporary tables")
		  (run-command-stdin-from-file lex "COPY tmp FROM stdin" rev-filename))
      ;;;
      ;; update main tables
      ;;;
      (lexdb-time ("updating 'rev' table" "done updating 'rev' table")
		  (merge-public-rev-from-tmp lex)    
		  (setf count-new (sql-get-val lex2 "SELECT count(*) from public.tmp")))
      
      (format t "~&(LexDB) ~a new 'rev' entries" count-new)
      (make-field-map-slot lex2)
      ;; vacuum at end
      (vacuum lex2)
      count-new)))

(defmethod merge-dfn ((lex psql-lex-database) dfn-filename)  
  (run-command lex "DELETE FROM tmp_dfn")
  (run-command-stdin-from-file lex "COPY tmp_dfn FROM stdin" dfn-filename)
  (merge-dfn-from-tmp-dfn lex))

(defmethod semi-up-to-date-p ((lex psql-lex-database))
 (not (semi-out-of-date lex)))

(defvar mrs::*empty-semantics-lexical-entries*)
(defmethod semi-out-of-date ((lex psql-lex-database))
   (mapcar #'(lambda (x) (str-2-symb (first x))) 
	   (get-raw-records
	    lex "SELECT name FROM lex_cache LEFT JOIN semi_mod USING (name,userid,modstamp) WHERE lex_cache.modstamp > COALESCE(semi_mod.modstamp0,'-infinity')")))

(defun compat-version (lexdb-version)
  (when (stringp lexdb-version)
    (subseq lexdb-version 0 3)))  
    
(defmethod get-value-set ((lex psql-lex-database) field)
  (let ((qi-field (quote-ident lex (2-str field))))
    (mapcar #'car
	    (get-raw-records lex
			     (format nil "SELECT DISTINCT ~a::text AS foo FROM rev_all WHERE ~a IS NOT NULL"
				     qi-field qi-field)))))

(defmethod fields-str ((lex psql-lex-database) fields)
  (concat-str
   (mapcar #'(lambda (x) (quote-ident lex x))
	   fields)
   :sep-c #\,))

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
					       "")))
			 str-list))))))))

(defun sql-list (l quote-fn)
  (format nil "(~a)"
    (concat-str
     (mapcar quote-fn l)
     :sep-c #\,)))

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
	(setf count-new (read-from-string (caar (get-raw-records lex "SELECT count(*) from public.tmp")))))
       (t
	(format t "~&(LexDB) WARNING:  cannot find file ~a" rev-filename)))
      (if (or (null count-new) (equal count-new 0))
	  (empty-cache lex)
	(update-lex-aux lex)))))

(defmethod vacuum ((lex psql-lex-database))
  (let (time)
    (format t "~&(LexDB) performing vacuum/analyze on database (as user ~a)..." (user lex))
    (force-output)
    (setf time (get-internal-real-time))
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "vacuum full analyze"))
    (format t "~&(LexDB) vacuum/analyze complete [~F sec]" 
	    (/ (- (get-internal-real-time) time) internal-time-units-per-second))))

(defmethod db-owner ((lex psql-lex-database))
  (let* ((uid (sql-get-val lex "SELECT datdba FROM pg_catalog.pg_database WHERE datname=current_database()"))
	 (uname (sql-get-val lex 
			     (concatenate 'string
			       "SELECT usename FROM pg_catalog.pg_user WHERE usesysid=" (psql-quote-literal uid)))))
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
	    (remove :|_tdl|
		    (remove-duplicates
		     (cons :|name| (grammar-fields lex)))))
	   ;; extract field values from tdl structure 
	   ;; and remove unifs as they are found
	   (extraction-field-vals (mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x dfn)))
			 extraction-fields))
	   ;; convert any remaining unifs into raw tdl fragment
	   (skip (unifs-to-tdl-body (cdr (assoc :UNIFS s))))
	   (skip (if (string= skip "") nil skip))
	   ;; necessary fields
	   (hard-coded-field-vals (list
				   (cons :|userid| *lexdb-dump-user*)
				   (cons :|modstamp| *lexdb-dump-timestamp*)
				   (cons :|dead| "f")
				   (cons :|_tdl| skip)))
	   ;; additional (useful) fields
	   ;; if not all fields occur in LexDB they will be silently ignored
	   (other-field-vals (list
			      (cons :|lang| *lexdb-dump-lang*)
			      (cons :|country| *lexdb-dump-country*)
			      (cons :|confidence| 1)
			      (cons :|source| *lexdb-dump-source*)))
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
	 (skip (cdr (assoc :|_tdl| field-vals)))
	 (name (cdr (assoc :|name| field-vals)))
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
     ((member :|_tdl| (fields lex))
      (format t "~&(LexDB) Unhandled TDL fragment in lexical entry ~a: ~%~t~a~%~%" name skip)
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
	line)
     ;; component(s) skipped and no :skip field in db
     (t
      (format t "~&~%(LexDB) Lex entry ~a skipped due to unhandled TDL fragment: ~%~t~a~%" name skip)
      (format skip-stream "~a" (to-tdl x))
      ""))))

;; keys must be updated after set of calls to this fn
(defmethod to-db ((x lex-entry) (lex psql-lex-database))
  "insert lex-entry into lex db (user scratch space)"
  (let* (;;ordered a-list of field values
	 (field-vals (get-field-vals x lex))
	 (skip (cdr (assoc :|_tdl| field-vals)))
	 (name (cdr (assoc :|name| field-vals)))
	 (ordered-field-vals (ordered-symb-val-list (fields lex) field-vals))
	 (psql-le (make-instance 'psql-lex-entry :fv-pairs ordered-field-vals)))	 
    (cond
     ;; no components of lex entry skipped
     ((null skip)
      (set-lex-entry lex psql-le :gen-key nil)
      (empty-cache lex))
     ;; component(s) skipped, but :skip field available in db
     ((member :|_tdl| (fields lex))
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
      (set-lex-entry lex psql-le :gen-key nil)
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
  (generate-missing-orthkeys lexdb)
  (update-lex-aux lexdb))

(defmethod record-id (raw-record cols (lex psql-lex-database))
  (str-2-symb (get-val :|name| raw-record cols)))  

(defmethod record-orth (raw-record cols (lex psql-lex-database))
  (get-val (second (assoc :ORTH (dfn lex))) raw-record cols))

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
  (dump-public-rev-to-tmp lex)
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
  (let* ((qi-user (quote-ident lex (user lex))))
    (with-lexdb-user-lexdb (lex-public lex)
      (format t "~%(LexDB) updating 'public.rev' with ~a new entries from private 'rev'"
	      (sql-get-num lex (format nil "SELECT count(*) FROM rev")))
      (sync-rev lex)
      (run-command lex-public (format nil "INSERT INTO public.rev SELECT * FROM ~a.rev" qi-user))
      (with-lexdb-user-lexdb (l lex)
	(register-mod-time l))
      (run-command lex "DELETE FROM rev"))
    (empty-cache lex))) ;;??

(defmethod lex-up-to-date-p ((lex psql-lex-database)) 
  (string> (build-time lex)
	   (mod-time-public lex)))

(defmethod sync-rev ((lex psql-lex-database)) 
  (unless (lex-up-to-date-p lex)
    (update-lex-aux lex))
  
  (run-command lex "UPDATE rev SET userid=user")
  (run-command lex "UPDATE rev SET modstamp='NOW'")
  (run-command lex "DELETE FROM lex_cache WHERE name IN (SELECT name FROM rev)")
  (run-command lex (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name IN (SELECT name FROM rev)" (orth-field lex)))
  (run-command lex "DELETE FROM lex_key WHERE name IN (SELECT name FROM rev)")
  (generate-missing-orthkeys lex))

(defmethod table-size ((lex psql-lex-database) table)
  (let ((table-str (format nil "~a" table)))
    (cond
     ((string= (string-downcase table-str) "rev-all")
      (count-rev-all lex))
     ((string= (string-downcase table-str) "lex")
      (count-lex lex))
     (t
      (sql-get-num lex 
		   (format nil "SELECT count(*) FROM ~a" table))))))

(defmethod table-head-count ((lex psql-lex-database) table)
  (sql-get-num lex 
	       (format nil "SELECT count(*) FROM ~a WHERE (name,modstamp) IN (SELECT name, max(modstamp) AS modstamp FROM ~a GROUP BY name)" table table)))


(defmethod clear-private-rev ((lex psql-lex-database))
  (unless (> (table-size lex :rev) 0)
    (format t "~%(LexDB) private 'rev' is already empty")
    (return-from clear-private-rev nil))
   (empty-cache lex)
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "DROP TABLE tmp_filt_cache CASCADE" :ignore-errors t))
   (run-command lex 
		(format nil "CREATE TABLE tmp_filt_cache AS SELECT name,userid,modstamp,dead,~a FROM public.rev WHERE name IN (SELECT name FROM rev) AND ~a" (orth-field lex) (get-filter lex))) ;; public.rev not rev_all
    (run-command lex "CREATE INDEX tmp_filt_cache_name_modstamp ON tmp_filt_cache (name,modstamp)")
   (run-command lex "DELETE FROM lex_key WHERE name IN (SELECT name FROM rev)")
   (run-command lex "DELETE FROM rev") ;; now safe to delete
   (lexdb-time 
    ("updating 'lex'" "done updating 'lex'")
    (run-command lex 
		 (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM (tmp_filt_cache JOIN (SELECT name, max(modstamp) AS modstamp FROM tmp_filt_cache GROUP BY name) AS t1 USING (name,modstamp)) WHERE dead='0'" (orth-field lex))))
    (run-command lex "DROP INDEX tmp_filt_cache_name_modstamp")
    (run-command lex "DROP TABLE tmp_filt_cache")
   (reconnect lex) ;; work around server bug
   (generate-missing-orthkeys lex))

 (defmethod orth-field ((lex psql-lex-database))
   (let ((orth-raw-mapping (assoc :ORTH (dfn lex))))
     (quote-ident lex (second orth-raw-mapping))))
