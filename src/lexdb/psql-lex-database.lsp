;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;; MU-PSQL-LEX-DATABAASE methods
;; (see also psql-lex-database0.lsp for more-generic function)
;;

;; SQL: select FIELDS for entries matching KEY
(defmethod lookup-word-no-cache-SQL ((lex mu-psql-lex-database) key fields)
  (let ((qkey (psql-quote-literal key)))
    (format nil "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key = ~a UNION SELECT rev.* FROM rev JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key = ~a) as foo" 
	    fields qkey qkey)))

;; FSQL: select entries with no KEY
(defmethod create-unnormalized-missing-lex-keys3-FSQL ((lex mu-psql-lex-database))
  "select name,userid,modstamp,~a from lex_cache left join lex_key using (name,userid,modstamp) where lex_key.key is null")
;; SQL: select all NAMEs
(defmethod collect-psort-ids-SQL ((lex mu-psql-lex-database))
  "SELECT name FROM lex_cache")
;  "SELECT DISTINCT name FROM lex_cache")

;;!
;; DOT: select FIELDS for revision entry ID NAME MODSTAMP
(defmethod get-dot-rev-record ((lex mu-psql-lex-database) id name modstamp &optional (fields '("*")))
  (unless (connection lex)
    (format t "~&(LexDB) WARNING:  no connection to mu-psql-lex-database")
    (return-from get-dot-rev-record))
  (let* ((fstr (fields-str lex fields))
	 (qid (psql-quote-literal id))
	 (qname (psql-quote-literal name))
	 (qmodstamp (psql-quote-literal modstamp))
	 (table
	  (get-records 
	   lex (format nil
		       "SELECT ~a FROM rev_all WHERE (name,userid,modstamp) = (~a,~a,~a)"
		       fstr qid qname qmodstamp))))
     (dot (cols table) (car (recs table)))))

;;;
;;; FILTER
;;;

(defmethod apply-filter ((lex mu-psql-lex-database) &optional filter)
  ;; query for filter if not provided
  (unless filter
    (setf filter (ask-user-for-x 
		"(LexDB) Alter filter" 
		(cons "New filter:" (filter lex)))))
  (apply-filter-aux lex filter))

(defmethod apply-filter-aux ((lex mu-psql-lex-database) filter)
  (cond
   ;; lexicon should be rebuilt
   ((and filter
	 (not (string= filter (filter lex))))
    (set-meta-filter lex filter)
    (build-lex lex)
    t)
   ;; some entries need to be rebuilt
   ((< 0 (insert-lex-cache-missing lex))
    (generate-missing-orthkeys lex)
    (register-build-time lex)
    t)
   ;; nothing to be done
   (t
    nil)))

(defmethod filter ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='filter'"))

;;;

(defmethod count-lex ((lex mu-psql-lex-database))
  (sql-get-num lex "SELECT count(*) FROM lex_cache"))

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

(defmethod new-user-schema-if-nec ((lex mu-psql-lex-database))
  (unless (sql-get-bool lex "SELECT (user IN (SELECT val FROM public.meta WHERE var='user'))")
    ;; user schema was not defined
    (new-user-schema lex)))

(defmethod count-rev-all ((lex mu-psql-lex-database))
  (sql-get-num lex "SELECT (SELECT count(*) FROM public.rev) + (SELECT count(*) FROM rev)"))

(defmethod new-public-rev ((lex mu-psql-lex-database))
  (get-raw-records lex "select name, userid, modstamp from public.rev where modstamp>(select val from meta where var='build_time') and userid != user"))

;; update lex_cache with new entries created by other users
(defmethod insert-lex-cache-missing ((lex mu-psql-lex-database))
  (run-command 
   lex 
   (format nil "INSERT INTO lex_cache select name, userid, modstamp, ~a from public.rev where modstamp>(select val from meta where var='build_time') and userid != user" 
	   (orth-field lex))))

;; set FILTER in META
(defmethod set-meta-filter ((lex mu-psql-lex-database) new-filter)
  (run-command lex 
	       (format nil "UPDATE meta SET val=~a WHERE var='filter'" 
		       (psql-quote-literal new-filter))))

;; check built/modstamps and rebuild lexicon if necessary
(defmethod build-lex-if-nec ((lex mu-psql-lex-database))
  (when (not (lex-up-to-date-p lex))
    (build-lex lex)))

;; rebuild lexicon
(defmethod build-lex ((lex mu-psql-lex-database))
  (lexdb-time 
   ("building 'lex'" "done building 'lex'")
   (build-lex2 lex)))

;; recreate LEX_CACHE and LEX_KEY (and LEX view)
(defmethod build-lex2 ((lex mu-psql-lex-database))
  (empty-cache lex)
  (new-lex-cache lex)
  (new-view-lex lex)
  (register-build-time lex)
  (new-lex-key-table lex)
  (vacuum lex)
  t)

;; recreate LEX view
(defmethod new-view-lex ((lex mu-psql-lex-database))
  (let* ((built-in-fields '(:|name| :|userid| :|modstamp| :|dead|))
	 (lex-fields (append built-in-fields (set-difference (grammar-fields lex) built-in-fields)))
	 (lex-fields-str (fields-str lex lex-fields)))
    (run-command lex (format nil "CREATE OR REPLACE VIEW lex AS SELECT rev_all.* FROM lex_cache JOIN rev_all USING (name,userid,modstamp)" lex-fields-str))))

;; recreate LEX_CACHE table
(defmethod new-lex-cache ((lex mu-psql-lex-database))
  (kill-lex-cache lex)
  (create-lex-cache lex))

(defmethod kill-lex-cache ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE lex_cache CASCADE" :ignore-errors t)))

;; create (and index) LEX_CACHE
(defmethod create-lex-cache ((lex mu-psql-lex-database))
  (new-tmp-filt-cache lex)
    (with-lexdb-client-min-messages (lex "error")
      (run-command 
       lex 
       (format 
	nil 
	"CREATE TABLE lex_cache AS SELECT name,userid,modstamp,~a FROM (tmp_filt_cache JOIN (SELECT name, max(modstamp) AS modstamp FROM tmp_filt_cache GROUP BY name) AS t1 USING (name,modstamp)) WHERE dead='0'" 
	(orth-field lex)))
      (kill-tmp-filt-cache lex)
      (run-command lex "ALTER TABLE lex_cache ADD PRIMARY KEY (name)")
      (run-command lex "CREATE INDEX lex_cache_name_userid_modstamp ON lex_cache (name, userid, modstamp)")))

;; (used in creation of LEX_CACHE)
(defmethod new-tmp-filt-cache ((lex mu-psql-lex-database))
  (kill-tmp-filt-cache lex)
  (create-tmp-filt-cache lex))

;; (used in creation of LEX_CACHE)
(defmethod kill-tmp-filt-cache ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE tmp_filt_cache CASCADE" :ignore-errors t)))

;; (used in creation of LEX_CACHE)
(defmethod create-tmp-filt-cache ((lex mu-psql-lex-database))
  (new-view-filt3 lex)
  (run-command 
   lex 
   (format nil 
	   "CREATE TABLE tmp_filt_cache AS SELECT name,userid,modstamp,dead,~a FROM filt" 
	   (orth-field lex)))
  (kill-view-filt lex)
  (run-command lex "CREATE INDEX tmp_filt_cache_name_modstamp ON tmp_filt_cache (name,modstamp)"))

(defmethod new-user-tmp ((lex mu-psql-lex-database))
  (kill-user-tmp lex)
  (create-user-tmp lex))

(defmethod kill-user-tmp ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE tmp CASCADE" :ignore-errors t)))

(defmethod create-user-tmp ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE TABLE tmp AS SELECT * FROM public.rev WHERE NULL")))

;;;
;;;

;; remove user's private space
(defmethod kill-user-schema ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex 
		 (format nil "DROP SCHEMA ~a CASCADE" (user lex))
		 :ignore-errors t)
    (with-lexdb-user-lexdb (lex2 lex)
      (run-command lex2 "DELETE FROM public.meta WHERE var='user' AND val='bmw20'" :ignore-errors t))))

;; create user's private space
(defmethod new-user-schema ((lex mu-psql-lex-database))
  (if (sql-get-bool lex "SELECT user_is_db_owner_p()")
      (error "~&(LexDB) no LexDB user scheme allowed for user '~S'" (user lex)))
  (kill-user-schema lex)
  (initialize-user-schema lex)
  (add-meta-user lex))

(defmethod add-meta-user ((lex mu-psql-lex-database))
  (with-lexdb-user-lexdb (lex-o lex)
    (run-command lex-o 
		 (format nil "INSERT INTO public.meta VALUES ('user',~a)"
			 (psql-quote-literal (user lex))))))

(defmethod create-user-schema ((lex mu-psql-lex-database))
  (let* ((qi-user (quote-ident lex (user lex))))
    (run-command lex (format nil "CREATE SCHEMA ~a" qi-user))
    (run-command lex (format nil "GRANT USAGE ON SCHEMA ~a TO lexdb" qi-user))))

(defparameter *lexdb-default-filter* "TRUE")

;; create user's META
(defmethod create-user-meta ((lex mu-psql-lex-database))
  (run-command lex "CREATE TABLE meta AS SELECT * FROM public.meta WHERE NULL")
  (run-command lex (format nil "INSERT INTO meta VALUES ('filter',~a)" 
			   (psql-quote-literal *lexdb-default-filter*)))
  (run-command lex "INSERT INTO meta VALUES ('mod_time','')")
  (run-command lex "INSERT INTO meta VALUES ('build_time','')"))
  
;(defmethod create-user-tmp ((lex mu-psql-lex-database))
;  (run-command lex "CREATE TABLE tmp AS SELECT * FROM public.rev WHERE NULL")
;  (run-command lex "GRANT SELECT ON tmp TO lexdb"))

;; create (and index) user's REV
(defmethod create-user-rev ((lex mu-psql-lex-database))
  (run-command lex "CREATE TABLE rev AS SELECT * FROM public.rev WHERE NULL")
  (run-command lex "GRANT SELECT ON rev TO lexdb")
  (index-user-rev lex))
  
(defmethod index-user-rev ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "ALTER TABLE rev ADD PRIMARY KEY (name);")))
  
(defmethod deindex-user-rev ((lex mu-psql-lex-database))
  (run-command lex "ALTER TABLE rev DROP CONSTRAINT rev_pkey" :ignore-errors t))
  
(defmethod index-public-rev ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "ALTER TABLE public.rev ADD PRIMARY KEY (name,userid,modstamp);")))
  
(defmethod deindex-public-rev ((lex mu-psql-lex-database))
  (run-command lex "ALTER TABLE public.rev DROP CONSTRAINT rev_pkey" :ignore-errors t))
  
;(defmethod create-user-lex-cache ((lex mu-psql-lex-database))
;  (run-command 
;   lex 
 ;  (format nil "CREATE TABLE lex_cache AS SELECT name,userid,modstamp,~a FROM public.rev WHERE NULL" (orth-field lex)))
;  (run-command 
;   lex 
;   "CREATE OR REPLACE VIEW lex AS SELECT rev_all.* FROM lex_cache JOIN rev_all USING (name,userid,modstamp)")
;  )

(defmethod create-view-rev-all ((lex mu-psql-lex-database))
  (run-command lex "CREATE VIEW rev_all AS SELECT * FROM public.rev UNION SELECT * FROM rev"))
  
(defmethod register-build-time ((lex mu-psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='build_time'"))

(defmethod register-mod-time ((lex mu-psql-lex-database))
  (run-command lex "UPDATE meta SET val=current_timestamp WHERE var='mod_time'"))

(defmethod initialize-user-schema ((lex mu-psql-lex-database))
  (if (sql-get-bool lex "SELECT user_is_db_owner_p()")
      (error "~&(LexDB)  no LexDB user scheme allowed for user '~S'" (user lex)))
  
  (create-user-schema lex)
  (create-user-meta lex)
					;  (create-user-tmp lex)
  (create-user-rev lex)
  (create-view-rev-all lex)
  (build-lex lex)
  ;(create-user-lex-cache lex)
  ;(new-lex-key-table lex)
  ;;
  (register-mod-time lex)
  (register-build-time lex)
  ;;
					;  (create-view-filt lex)
					;  (create-view-head lex)  
  ;;
  (create-skeletal-db-semi lex)
  (new-semi lex)
  t)


(defmethod mod-time ((lex mu-psql-lex-database))
  (let ((pub (mod-time-public lex))
	(priv (mod-time-private lex)))
    (if (string> pub priv)
	pub
      priv)))

(defmethod mod-time-private ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='mod_time'"))

(defmethod mod-time-public ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM public.meta WHERE var='mod_time'"))

(defmethod build-time ((lex mu-psql-lex-database))
  (sql-get-val lex "SELECT val FROM meta WHERE var='build_time'"))

(defmethod new-public-tmp ((lex mu-psql-lex-database))
  (kill-public-tmp lex)
  (create-public-tmp lex))

(defmethod kill-public-tmp ((lex mu-psql-lex-database))
  (run-command lex "DROP TABLE public.tmp"
	       :ignore-errors t))

(defmethod create-public-tmp ((lex mu-psql-lex-database))
  (run-command lex "CREATE TABLE public.tmp AS SELECT * FROM public.rev WHERE NULL"))

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

;(defmethod merge-dfn-from-tmp-dfn ((lex mu-psql-lex-database))
;  (sql-get-bool lex "SELECT assert_db_owner()")
;  (run-command lex "DELETE FROM dfn WHERE mode IN (SELECT DISTINCT mode FROM tmp_dfn)")
;  (run-command lex "INSERT INTO dfn SELECT * FROM tmp_dfn"))
  
(defmethod dump-public-rev-to-tmp ((lex mu-psql-lex-database))
  (run-command lex "SET TIME ZONE 00")
  (run-command lex "DELETE FROM tmp")
  (run-command lex "INSERT INTO tmp SELECT * FROM public.rev ORDER BY name,userid,modstamp"))

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
      (new-public-tmp lex)
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
      (kill-public-tmp lex)
      (format t "~&(LexDB) ~a new 'rev' entries" count-new)
					;(make-field-map-slot lex2)
      ;; vacuum at end
      (vacuum lex2)
      count-new)))

(defmethod replace-dfn ((lex mu-psql-lex-database) dfn-filename)  ;!!!
  (run-command lex "DELETE FROM dfn")
  (run-command-stdin-from-file lex "COPY dfn FROM stdin" dfn-filename)
  (get-dfn lex))

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
	;; replace DFN entries with those in file
	(replace-dfn lex2 dfn-filename))
       (t
	(format t "~&(LexDB) WARNING:  cannot find file ~a" dfn-filename)))
      ;; rev table
      (cond
       ((probe-file rev-filename)
	;; merge new REV entries from file
	(merge-rev lex2 rev-filename))
       (t
	(format t "~&(LexDB) WARNING:  cannot find file ~a" rev-filename)))
      (if (or (null count-new) (equal count-new 0))
	  (empty-cache lex)
	(build-lex-if-nec lex)))))

;(defmethod merge-into-lexicon-dfn ((lex mu-psql-lex-database) filename)
;  "reconnect as db owner and merge new dfn into lexdb"
;  (with-slots (dbname host port) lex
;    (unless dbname
;      (error "please set :dbname"))
;    (let ((conn-db-owner 
;	   (make-instance 'mu-psql-lex-database
;	     :dbname dbname
;	     :host host
;	     :port port
;	     :user (db-owner lex))))
;      (connect conn-db-owner)
;      (when
;	  (catch :sql-error
;	    (progn
;	      (let ((dfn-filename 
;		     (absolute-namestring "~a" filename)))
;		(cond
;		 ((probe-file dfn-filename)
;		  (merge-dfn conn-db-owner 
;			      dfn-filename)
;		  (make-field-map-slot lex))
;		 (t
;		  (format t "~%WARNING: no file ~a" dfn-filename)))
;		nil
;		)))
;	(format t "~&(LexDB) merge new dfn entries aborted ..."))
;      (empty-cache lex)
;      (disconnect conn-db-owner))))

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
  (new-user-tmp lex)
  (dump-public-rev-to-tmp lex)
  (run-command-stdout-to-file lex "COPY tmp TO stdout" 
			      (namestring (pathname (format nil "~a.rev" 
							    filebase))))
  (kill-user-tmp lex)
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
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (filter lexdb))))))
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
	   (mod-time lex)))

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
		(format nil "CREATE TABLE tmp_filt_cache AS SELECT name,userid,modstamp,dead,~a FROM public.rev WHERE name IN (SELECT name FROM rev) AND ~a" (orth-field lex) (filter lex))) ;; public.rev not rev_all
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
	  (length (show-scratch lex))))

(defmethod semi-mod-unused ((lex mu-psql-lex-database))
  (get-raw-records 
   lex 
   "select name from lex_cache right join semi_mod using (name) where lex_cache is null"))

(defmethod create-lex-key-indices ((lex mu-psql-lex-database))
  ;(with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)" :ignore-errors t)
    (run-command lex "CREATE INDEX lex_key_name_userid_modstamp ON lex_key (name, userid, modstamp)" :ignore-errors t)
  ;  )
  )

(defmethod drop-lex-key-indices ((lex mu-psql-lex-database))
  ;(with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t)
    (run-command lex "DROP INDEX lex_key_name_userid_modstamp" :ignore-errors t)
  ;  )
  )

(defmethod create-lex-key-table ((lex mu-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE TABLE lex_key (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		key text NOT NULL
		)")))

;; recreate FILT view
(defmethod new-view-filt3 ((lex mu-psql-lex-database))
  (run-command lex 
	       (format nil "CREATE OR REPLACE VIEW filt AS SELECT * FROM rev_all WHERE ~a" 
		       (filter lex))))

(defmethod kill-view-filt ((lex mu-psql-lex-database))
  (run-command lex 
	       (format nil "DROP VIEW filt")))


(defmethod new-view-head ((lex mu-psql-lex-database))
  (new-view-filt3 lex)
  (run-command lex "CREATE OR REPlACE VIEW head AS SELECT fil.* FROM (filt AS fil NATURAL JOIN (SELECT name, max(modstamp) AS modstamp FROM filt GROUP BY name) AS t1) WHERE dead='0'"))

(defmethod kill-view-head ((lex mu-psql-lex-database))
  (run-command lex 
	       (format nil "DROP VIEW head"))
  (kill-view-filt lex))


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
    (new-view-head lex)
    (run-command lex 
		 (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name = ~a" (orth-field lex) ql-name))
    (kill-view-head lex)
    (run-command lex 
		 (format nil "DELETE FROM lex_key WHERE name = ~a" ql-name))))
  

(defmethod sync-rev ((lex mu-psql-lex-database)) 
  (unless (lex-up-to-date-p lex)
    (build-lex-if-nec lex))
  
  (run-command lex "UPDATE rev SET userid=user")
  (run-command lex "UPDATE rev SET modstamp='NOW'")
  (run-command lex "DELETE FROM lex_cache WHERE name IN (SELECT name FROM rev)")
  (new-view-head lex)
  (run-command lex (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name IN (SELECT name FROM rev)" (orth-field lex)))
  (kill-view-head lex)
  (run-command lex "DELETE FROM lex_key WHERE name IN (SELECT name FROM rev)")
  (generate-missing-orthkeys lex))

(defmethod retrieve-entry2 ((lex mu-psql-lex-database) name &key (reqd-fields '("*")))
  (let ((qname (psql-quote-literal name)))
    (get-records lex
		 (format nil
			 "SELECT ~a FROM (SELECT rev.* FROM public.rev as rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a UNION SELECT rev.* FROM rev JOIN lex_cache USING (name,userid,modstamp) WHERE lex_cache.name = ~a) as foo"
			 (fields-str lex reqd-fields)
			 qname qname))))

(defmethod get-internal-table-dfn ((lex mu-psql-lex-database))
  (get-field-info lex "public" "rev"))  

(defmethod lookup ((lex mu-psql-lex-database) field-kw val-str &key (ret-flds "*") (from "lex"))
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

(defmethod delete-record ((lex mu-psql-lex-database) record)
  (setf (cdr (assoc :|dead| record)) "t") ;; mark entry as dead
  (set-lex-entry-from-record lex record)) ;; commit change

;; this belongs here due to use of low-level get-raw-records
(defmethod create-unnormalized-missing-lex-keys ((lex mu-psql-lex-database))
  (loop
      for rec in
	(get-raw-records lex
			 (format nil (create-unnormalized-missing-lex-keys3-FSQL lex)
				 (orth-field lex)))
      for orth-list = (string-2-str-list (fourth rec))
      if (= 1 (length orth-list))
      collect rec
      else
      append
      (loop for word in orth-list
	  collect (list (first rec) (second rec) (third rec) word))))

