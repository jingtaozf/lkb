;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;; --- mu-psql-lex-database methods
;;;

(defmethod lookup-word-no-cache-SQL ((lex mu-psql-lex-database) quoted-literal fields)
  (format nil "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a UNION SELECT rev.* FROM rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a) as foo" fields quoted-literal quoted-literal))

#+:null
(defmethod create-unnormalized-lex-keys ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
  (run-command lex "DELETE FROM lex_key") 
  (loop
      with i = 1
      while (> (run-command lex (format nil "insert into lex_key select * from (select name,userid,modstamp,split_part(~a,' ',~a) as key from lex_cache) as foo where key!=''" (orth-field lex) i)) 0)
      do (incf i)))

(defmethod create-unnormalized-missing-lex-keys3-FSQL ((lex mu-psql-lex-database))
  "select name,userid,modstamp,~a from lex_cache left join lex_key using (name,userid,modstamp) where lex_key.key is null")
  
(defmethod collect-psort-ids-SQL ((lex mu-psql-lex-database))
  "SELECT DISTINCT name FROM lex_cache")

(defmethod retrieve-head-record-str ((lex mu-psql-lex-database) id &optional (reqd-fields '("*")))
  (retrieve-head-record lex (str-2-symb id) reqd-fields))

(defmethod retrieve-head-record ((lex mu-psql-lex-database) id &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((id-str (2-str id))
	   (table 
	    (retrieve-entry lex (sql-like-text id-str) :reqd-fields reqd-fields)))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to mu-psql-lex-database"))))

(defmethod retrieve-record-ium ((lex mu-psql-lex-database) id name modstamp &optional (reqd-fields '("*")))
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
    (format t "~&(LexDB) WARNING:  no connection to mu-psql-lex-database"))))



;;;
;;; db filter
;;;

(defmethod set-filter ((lex mu-psql-lex-database) &rest rest)  
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

;;; ??? should be: "SELECT count(*) FROM lex_cache"
(defmethod count-lex ((lex mu-psql-lex-database))
  (let ((size-lex-pub (sql-get-num lex "SELECT count(*) FROM public.rev JOIN lex_cache USING (name,userid,modstamp)"))
	(size-lex-priv (sql-get-num lex "SELECT count(*) FROM rev JOIN lex_cache USING (name,userid,modstamp)")))
    (+ size-lex-pub size-lex-priv)))

;;;
;;; cache
;;;

(defmethod lookup-rev-all ((lex mu-psql-lex-database) field-kw val-str &key (ret-flds "*"))
  (lookup lex field-kw val-str :ret-flds ret-flds :from "rev_all"))

;;

(defmethod list-fld ((lex mu-psql-lex-database))
  (mapcar #'car
	  (recs 
	   (get-field-info2 lex "public" "rev"))))

;; called from pg-interface
(defmethod new-entries ((lex mu-psql-lex-database))
  (let ((records (get-raw-records lex
				  (format nil "SELECT userid,name,modstamp FROM public.tmp"))))
    (cons (list "userid" "name" "modstamp") records)))

;;;
;;; low-level
;;;

;;;
;;; script file fn
;;;

;; fix_me: remove 'lex-cache' upgrade hack
(defmethod open-lex-aux ((lex mu-psql-lex-database)) 
  (with-slots (dbname host user) 
      lex
    (when (connect lex)
      (check-psql-server-version lex)
      (check-lexdb-version lex)
      (unless
	  (sql-get-bool lex "SELECT user_is_db_owner_p()")
	(make-field-map-slot lex)
	(unless (sql-get-bool lex "SELECT (user IN (SELECT val FROM public.meta WHERE var='user'))")
	  ;; user schema was not defined
	  (initialize-user-schema lex)
	  (run-command lex "INSERT INTO meta VALUES ('hack','lex-cache')"))
	(unless (sql-get-bool lex "SELECT ('lex-cache' IN (SELECT val FROM meta WHERE var='hack'))")
	  ;; upgrade an existing schema
	  (format t "~%(LexDB) [automatically updating user schema for compatibility with current LKB]")
	  (build-lex lex)
	  (run-command lex "INSERT INTO meta VALUES ('hack','lex-cache')")
	  (when (string= "4.8" (compat-version (lexdb-version lex)))
	    (with-lexdb-user-lexdb (l lex)
	      (run-command l "UPDATE public.meta SET val='4.90' WHERE var='lexdb-version'")))))
      lex)))

(defmethod get-filter ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))  

(defmethod count-rev-all ((lex mu-psql-lex-database))
  (sql-get-num lex "SELECT (SELECT count(*) FROM public.rev) + (SELECT count(*) FROM rev)"))

(defmethod update-lex-aux ((lex mu-psql-lex-database))
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

(defmethod filter ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))

(defmethod new-public-rev ((lex mu-psql-lex-database))
  (get-raw-records lex "select name, userid, modstamp from public.rev where modstamp>(select val from meta where var='build_time') and userid != user"))

(defmethod update-filter ((lex mu-psql-lex-database) new-filter)
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

(defmethod build-lex ((lex mu-psql-lex-database))
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

(defmethod register-user-schema ((lex mu-psql-lex-database) user)
  (with-lexdb-user-lexdb (lex-o lex)
   (run-command lex-o 
		(format nil "INSERT INTO public.meta VALUES ('user',~a)"
			(psql-quote-literal user)))))

(defmethod initialize-user-schema ((lex mu-psql-lex-database))
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
    ;(create-table-lex-key lex)
    ;(index-lex-key lex)
    
    ;;
    (register-mod-time lex)
    (register-build-time lex)

    ;;
    (create-view-filt lex)
    (create-view-head lex)
    
    ;;
    (create-skeletal-db-semi lex)
    (new-semi lex)
;    (semi-create-indices lex)
    )
  t)))


(defmethod index-public-rev ((lex mu-psql-lex-database))
  (run-command lex "
ALTER TABLE public.rev ADD PRIMARY KEY (name,userid,modstamp);
CREATE UNIQUE INDEX name_modstamp ON public.rev (name,modstamp); 
CREATE INDEX rev_name_modstamp ON public.rev (name, modstamp);
SELECT if_psql_server_version(\'7.4\', \'CREATE INDEX rev_name_pattern ON public.rev (name varchar_pattern_ops)\', \'CREATE INDEX rev_name_pattern ON public.rev (name)\');
CREATE INDEX rev_name
	ON public.rev (name varchar_ops); 
"))
  
(defmethod deindex-public-rev ((lex mu-psql-lex-database))
  (run-command lex "DROP INDEX name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name_pattern" :ignore-errors t)
  (run-command lex "ALTER TABLE public.rev DROP CONSTRAINT rev_pkey" :ignore-errors t))
  
(defmethod create-view-head ((lex mu-psql-lex-database))
  (run-command lex "CREATE VIEW head AS SELECT fil.* FROM (filt AS fil NATURAL JOIN (SELECT name, max(modstamp) AS modstamp FROM filt GROUP BY name) AS t1) WHERE dead='0'"))
  
(defmethod create-view-filt ((lex mu-psql-lex-database))
  (run-command lex "CREATE VIEW filt AS SELECT * FROM rev_all WHERE NULL"))
  
(defmethod create-view-rev-all ((lex mu-psql-lex-database))
  (run-command lex "CREATE VIEW rev_all AS SELECT * FROM public.rev UNION SELECT * FROM rev"))
  
(defmethod register-build-time ((lex mu-psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='build_time'"))

(defmethod register-mod-time ((lex mu-psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='mod_time'"))

(defmethod mod-time-private ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='mod_time'"))

(defmethod mod-time-public ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM public.meta WHERE var='mod_time'"))

(defmethod build-time ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='build_time'"))

(defmethod merge-public-rev-from-tmp ((lex mu-psql-lex-database))
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

(defmethod merge-dfn-from-tmp-dfn ((lex mu-psql-lex-database))
  (sql-get-bool lex "SELECT assert_db_owner()")
  
  ;;
  (run-command lex "DELETE FROM dfn WHERE mode IN (SELECT DISTINCT mode FROM tmp_dfn)")
  (run-command lex "INSERT INTO dfn SELECT * FROM tmp_dfn"))
  
(defmethod dump-public-rev-to-tmp ((lex mu-psql-lex-database))
  (run-command lex "SET TIME ZONE 00")
  (run-command lex "DELETE FROM tmp")
  (run-command lex "INSERT INTO tmp SELECT * FROM public.rev ORDER BY name,userid,modstamp"))

(defmethod update-entry ((lex mu-psql-lex-database) symb-list psql-le)
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
  
(defmethod semi-mod-time-private ((lex mu-psql-lex-database) psql-le)
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

(defmethod show-scratch ((lex mu-psql-lex-database))
  (get-raw-records lex "SELECT name,userid,modstamp FROM rev"))

(defmethod scratch-records ((lex mu-psql-lex-database))
  (get-raw-records lex "SELECT * FROM rev"))

(defmethod merge-rev ((lex mu-psql-lex-database) rev-filename)
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

(defmethod merge-dfn ((lex mu-psql-lex-database) dfn-filename)  
  (run-command lex "DELETE FROM tmp_dfn")
  (run-command-stdin-from-file lex "COPY tmp_dfn FROM stdin" dfn-filename)
  (merge-dfn-from-tmp-dfn lex))

(defmethod get-value-set ((lex mu-psql-lex-database) field)
  (let ((qi-field (quote-ident lex (2-str field))))
    (mapcar #'car
	    (get-raw-records lex
			     (format nil "SELECT DISTINCT ~a::text AS foo FROM rev_all WHERE ~a IS NOT NULL"
				     qi-field qi-field)))))

;;
;;
;;

(defmethod merge-into-lexdb ((lex mu-psql-lex-database) filename)
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

(defmethod merge-into-lexicon-dfn ((lex mu-psql-lex-database) filename)
  "reconnect as db owner and merge new dfn into lexdb"
  (with-slots (dbname host port) lex
    (unless dbname
      (error "please set :dbname"))
    (let ((conn-db-owner 
	   (make-instance 'mu-psql-lex-database
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

;;
;;
;;

(defmethod dump-lexdb ((lexdb mu-psql-lex-database) filebase &key tdl)
  (format t "~%(dumping LexDB)")
  (force-output)
  (dump-rev lexdb filebase)
  (dump-dfn lexdb filebase)
  (dump-fld lexdb filebase)
  (dump-meta lexdb filebase)
  (when tdl (dump-tdl lexdb filebase))
  t)

(defmethod dump-rev ((lex mu-psql-lex-database) filebase)
  (dump-public-rev-to-tmp lex)
  (run-command-stdout-to-file lex "COPY tmp TO stdout" 
			      (namestring (pathname (format nil "~a.rev" 
							    filebase))))
  t)

(defmethod dump-dfn ((lexdb mu-psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.dfn TO stdout" 
			      (namestring (pathname (format nil "~a.dfn" 
							    filebase))))
  t)

(defmethod dump-fld ((lexdb mu-psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.fld TO stdout" 
			      (namestring (pathname (format nil "~a.fld" 
							    filebase))))
  t)

(defmethod dump-meta ((lexdb mu-psql-lex-database) filebase)
  (run-command-stdout-to-file lexdb "COPY public.meta TO stdout" 
			      (namestring (pathname (format nil "~a.meta" 
							    filebase))))
  t)

(defmethod dump-tdl ((lexdb mu-psql-lex-database) filebase)
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (get-filter lexdb))))))
    (format t "~&(LexDB) exporting filtered ~a LexDB to TDL file ~a" (dbname lexdb) tdl-file)
    (force-output)
    (export-to-tdl-to-file lexdb tdl-file)))

(defmethod commit-private-rev ((lex mu-psql-lex-database)) 
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

(defmethod lex-up-to-date-p ((lex mu-psql-lex-database)) 
  (string> (build-time lex)
	   (mod-time-public lex)))

(defmethod sync-rev ((lex mu-psql-lex-database)) 
  (unless (lex-up-to-date-p lex)
    (update-lex-aux lex))
  
  (run-command lex "UPDATE rev SET userid=user")
  (run-command lex "UPDATE rev SET modstamp='NOW'")
  (run-command lex "DELETE FROM lex_cache WHERE name IN (SELECT name FROM rev)")
  (run-command lex (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name IN (SELECT name FROM rev)" (orth-field lex)))
  (run-command lex "DELETE FROM lex_key WHERE name IN (SELECT name FROM rev)")
  (generate-missing-orthkeys lex))

(defmethod table-head-count ((lex mu-psql-lex-database) table)
  (sql-get-num lex 
	       (format nil "SELECT count(*) FROM ~a WHERE (name,modstamp) IN (SELECT name, max(modstamp) AS modstamp FROM ~a GROUP BY name)" table table)))


(defmethod clear-private-rev ((lex mu-psql-lex-database))
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


(defmethod connect ((lex mu-psql-lex-database)) 
  (if (next-method-p) (call-next-method))
  (when (connection-ok lex)
    (setf (lexdb-version lex) 
      (get-db-version lex))
    t))	

(defmethod close-lex ((lex mu-psql-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (lexdb-version) lex
    (setf lexdb-version nil)
    (if (next-method-p) (call-next-method))))


(defmethod check-lexdb-version ((lex mu-psql-lex-database))
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

(defmethod check-psql-server-version ((lex mu-psql-lex-database))
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

;;

;;;
;;;
;;;

(defmethod get-db-version ((lex mu-psql-lex-database))
  (caar 
   (get-raw-records lex 
		    "SELECT val FROM public.meta WHERE var='lexdb-version' LIMIT 1")))

(defmethod populate-semi-mod ((lex mu-psql-lex-database))
  (run-command lex "INSERT INTO semi_mod (SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex_cache JOIN semi_pred ON name=lex_id)"))

(defmethod populate-semi-mod-EMPTY-SEM ((lex mu-psql-lex-database) not-indexed)
  (run-command lex
		    (format nil "INSERT INTO semi_mod SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex_cache WHERE name IN ~a" 
			    (format nil " (~a~{, ~a~})" 
				    (psql-quote-literal (car not-indexed))
				    (loop for lexid in (cdr not-indexed)
					collect (psql-quote-literal lexid))))))
  
(defmethod semi-out-of-date ((lex mu-psql-lex-database))
   (mapcar #'(lambda (x) (str-2-symb (first x))) 
	   (get-raw-records
	    lex "SELECT name FROM lex_cache LEFT JOIN semi_mod USING (name,userid,modstamp) WHERE lex_cache.modstamp > COALESCE(semi_mod.modstamp0,'-infinity')")))


(defmethod import-tdl-file ((lex mu-psql-lex-database) filename)
  (declare (ignore filename))
  (if (next-method-p) (call-next-method))
  (format t "~&(LexDB) private space: ~a entries" 
	  (length (show-scratch lex)))
  )

(defmethod semi-mod-unused ((lex mu-psql-lex-database))
  (get-raw-records 
   lex 
   "select name from lex_cache right join semi_mod using (name) where lex_cache is null"))
