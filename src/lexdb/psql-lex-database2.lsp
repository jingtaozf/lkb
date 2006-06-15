;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;; --- su-psql-lex-database methods
;;;

;; orthkey must be mapped to normalized form before entering PSQL universe
(defmethod lookup-word-no-cache ((lex su-psql-lex-database) orth)
  (declare (ignore cache))
  (if (connection lex)
      (let* ((quoted-literal (psql-quote-literal (sql-like-text (normalize-orthkey orth))))
	     (table 
	      (get-records 
	       lex
	       (format nil "SELECT ~a FROM (SELECT lex.* FROM lex JOIN lex_key USING (name,userid,modstamp) WHERE lex_key.key LIKE ~a) as foo" 
		       (fields-str lex (grammar-fields lex))
		       quoted-literal)))
	     (ids (lookup-word-aux2 lex table)))
	ids)))

(defmethod create-unnormalized-lex-keys ((lex su-psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
  (run-command lex "DELETE FROM lex_key") 
  (loop
      with i = 1
      while (> (run-command lex (format nil "insert into lex_key select * from (select name,userid,modstamp,split_part(~a,' ',~a) as key from lex) as foo where key!=''" (orth-field lex) i)) 0)
      do (incf i)))

(defmethod create-unnormalized-missing-lex-keys3 ((lex su-psql-lex-database))
  (loop
      for rec in
	(get-raw-records lex 
			 (format nil "select name,userid,modstamp,~a from lex left join lex_key using (name,userid,modstamp) where lex_key.key is null" 
				 (orth-field lex)))
      for orth-list = (string-2-str-list (fourth rec))
      if (= 1 (length orth-list))
      collect rec
      else
      append 
      (loop for word in orth-list
	  collect (list (first rec) (second rec) (third rec) word))))
  
(defmethod collect-psort-ids-aux ((lex su-psql-lex-database))
  (let ((query-res 
	 (get-raw-records lex "SELECT DISTINCT name FROM lex")))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

;

(defmethod retrieve-head-record-str ((lex su-psql-lex-database) id &optional (reqd-fields '("*")))
  (retrieve-head-record lex (str-2-symb id) reqd-fields))

(defmethod retrieve-head-record ((lex su-psql-lex-database) id &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((id-str (2-str id))
	   (table 
	    (retrieve-entry lex (sql-like-text id-str) :reqd-fields reqd-fields)))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to su-psql-lex-database"))))

(defmethod retrieve-record-ium ((lex su-psql-lex-database) id name modstamp &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((table
	    (get-records lex
			 (format nil
				 "SELECT ~a FROM lex WHERE (name,userid,modstamp) = (~a,~a,~a)"
				 (fields-str lex reqd-fields)
				 (psql-quote-literal id)
				 (psql-quote-literal name)
				 (psql-quote-literal modstamp)))))
      
      (if (> (length (recs table)) 1)
	  (error (format nil "too many records returned"))
	(dot (cols table) (first (recs table))))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to su-psql-lex-database"))))



;;;
;;; db filter
;;;

;;; ??? should be: "SELECT count(*) FROM lex_cache"
(defmethod count-lex ((lex su-psql-lex-database))
  (sql-get-num lex "SELECT count(*) FROM lex"))

;;;
;;; cache
;;;

;(defmethod lookup-rev-all ((lex su-psql-lex-database) field-kw val-str &key (ret-flds "*"))
;  (lookup lex field-kw val-str :ret-flds ret-flds :from "rev_all"))

;;

(defmethod list-fld ((lex su-psql-lex-database))
  (mapcar #'car
	  (recs 
	   (get-field-info2 lex "public" "lex"))))

;;;
;;; low-level
;;;

;;;
;;; script file fn
;;;

;; fix_me: remove 'lex-cache' upgrade hack
(defmethod open-lex-aux ((lex su-psql-lex-database)) 
  (with-slots (dbname host user) 
      lex
    (when (connect lex)
      (make-field-map-slot lex)
      lex)))

(defmethod update-lex-aux ((lex su-psql-lex-database))
  (reconnect lex) ;; work around server bug
  (let ((size (count-lex lex)))
    (format t "~&(LexDB) total 'lex' entries available: ~a" size)
    (when (= 0 size)
      (format t " !!! PLEASE LOAD LEX ENTRIES !!!")
      (lkb-beep)))
  (empty-cache lex))

(defmethod build-lex ((lex su-psql-lex-database))
  (generate-missing-orthkeys lex)
  (vacuum lex)
  (empty-cache lex))

(defmethod index-lex ((lex su-psql-lex-database))
  (run-command lex "
ALTER TABLE lex ADD PRIMARY KEY (name,userid,modstamp);
CREATE UNIQUE INDEX name_modstamp ON lex (name,modstamp); 
CREATE INDEX rev_name_modstamp ON lex (name, modstamp);
CREATE INDEX rev_name ON public.rev (name varchar_ops); 
"))
  
(defmethod deindex-lex ((lex su-psql-lex-database))
  (run-command lex "DROP INDEX name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name_modstamp" :ignore-errors t)
  (run-command lex "DROP INDEX rev_name" :ignore-errors t)
  (run-command lex "ALTER TABLE lex DROP CONSTRAINT rev_pkey" :ignore-errors t))
  
(defmethod get-value-set ((lex su-psql-lex-database) field)
  (let ((qi-field (quote-ident lex (2-str field))))
    (mapcar 
     #'car
     (get-raw-records 
      lex
      (format 
       nil 
       "SELECT DISTINCT ~a::text AS foo FROM lex WHERE ~a IS NOT NULL"
       qi-field qi-field)))))

;;
;;
;;

;; common?
#+:null
(defmethod to-db-dump-rev ((x lex-entry) (lex su-psql-lex-database) &key (skip-stream t))
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

;;
;;
;;

(defmethod dump-tdl ((lexdb su-psql-lex-database) filebase)
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (get-filter lexdb))))))
    (format t "~&(LexDB) exporting filtered ~a LexDB to TDL file ~a" (dbname lexdb) tdl-file)
    (force-output)
    (export-to-tdl-to-file lexdb tdl-file)))

(defmethod connect ((lex su-psql-lex-database)) 
  (if (next-method-p) (call-next-method))
  (when (connection-ok lex)
    t))	

(defmethod semi-out-of-date ((lex su-psql-lex-database))
   (mapcar #'(lambda (x) (str-2-symb (first x))) 
	   (get-raw-records
	    lex "SELECT name FROM lex LEFT JOIN semi_mod USING (name,userid,modstamp) WHERE lex.modstamp > COALESCE(semi_mod.modstamp0,'-infinity')")))

(defmethod dump-generator-indices-to-psql ((lex su-psql-lex-database))
  (mrs::sdb-to-psql 
   lex (mrs::populate-sdb :semantic-table mrs::*semantic-table*))
  (run-command lex "INSERT INTO semi_mod (SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex JOIN semi_pred ON name=lex_id)")
  (let ((not-indexed mrs::*empty-semantics-lexical-entries*))
    (when not-indexed
      (run-command lex
		   (format nil "INSERT INTO semi_mod SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex WHERE name IN ~a" 
			   (format nil " (~a~{, ~a~})" 
				   (psql-quote-literal (car not-indexed))
				   (loop for lexid in (cdr not-indexed)
				       collect (psql-quote-literal lexid)))))))
  ;; semi_mod indexes should be created after this call
  (run-command lex "SET ENABLE_HASHJOIN TO false")
  )

(defmethod update-entry ((lex su-psql-lex-database) symb-list psql-le)
  (let ((ql-name (psql-quote-literal (retr-val psql-le :|name|))))
    (run-command lex 
		 (format nil "DELETE FROM lex WHERE name=~a" ql-name))
    (run-command lex 
		 (format nil "INSERT INTO lex ~a VALUES ~a" 
			 (sql-list symb-list 
				   #'(lambda (x) (quote-ident lex x)))
			 (sql-list (ordered-val-list symb-list psql-le) 
				   #'(lambda (x) (psql-quote-literal x)))))
;    (run-command lex 
;		 (format nil "DELETE FROM lex_cache WHERE name=~a" ql-name))
;    (run-command lex 
;		 (format nil "INSERT INTO lex_cache SELECT name,userid,modstamp,~a FROM head WHERE name = ~a" (orth-field lex) ql-name))
    (run-command lex 
		 (format nil "DELETE FROM lex_key WHERE name = ~a" ql-name))))
  
(defmethod import-tdl-file ((lex su-psql-lex-database) filename)
  (declare (ignore filename))
  (if (next-method-p) (call-next-method))
  (format t "~&(LexDB) number of 'lex' entries: ~a" (table-size lex "lex")))

(defmethod semi-mod-unused ((lex su-psql-lex-database))
  (get-raw-records 
   lex 
   "select name from lex right join semi_mod using (name) where lex is null"))
