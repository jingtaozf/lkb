;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;;
;; --- su-psql-lex-database methods
;;;

(defmethod lookup-word-no-cache-SQL ((lex su-psql-lex-database) key fields)
  (format nil "SELECT ~a FROM (SELECT lex.* FROM lex JOIN lex_key USING (name) WHERE lex_key.key = ~a) as foo" 
	  fields 
	  (psql-quote-literal key)))

(defmethod create-unnormalized-missing-lex-keys3-FSQL ((lex su-psql-lex-database))
  "select name,~a from lex left join lex_key using (name) where lex_key.key is null")

(defmethod collect-psort-ids-SQL ((lex su-psql-lex-database))
  "SELECT DISTINCT name FROM lex")

(defmethod count-lex ((lex su-psql-lex-database))
  (sql-get-num lex "SELECT count(*) FROM lex"))

;;;
;;; cache
;;;

(defmethod list-fld ((lex su-psql-lex-database))
  (mapcar #'car
	  (recs 
	   (get-field-info2 lex "public" "lex"))))

;;;
;;; script file fn
;;;

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

(defmethod dump-tdl ((lexdb su-psql-lex-database) filebase)
  (let ((tdl-file (namestring (pathname (format nil "~a.~a.tdl" filebase (filter lexdb))))))
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

(defmethod populate-semi-mod ((lex su-psql-lex-database))
  (run-command lex "INSERT INTO semi_mod (SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex JOIN semi_pred ON name=lex_id)"))

(defmethod populate-semi-mod-EMPTY-SEM ((lex su-psql-lex-database) not-indexed)
  (run-command lex
		    (format nil "INSERT INTO semi_mod SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex WHERE name IN ~a"
			    (format nil " (~a~{, ~a~})" 
				    (psql-quote-literal (car not-indexed))
				    (loop for lexid in (cdr not-indexed)
					collect (psql-quote-literal lexid))))))
  

(defmethod update-entry ((lex su-psql-lex-database) symb-list psql-le)
  (new-entry lex symb-list psql-le))
  
(defmethod new-entry ((lex su-psql-lex-database) symb-list psql-le)
  (kill-entry lex (retr-val psql-le :|name|))
  (insert-entry lex symb-list psql-le))

(defmethod kill-entry ((lex su-psql-lex-database) name)
  (let ((qname (psql-quote-literal name)))
    ;; delete from lex
    (run-command lex 
		 (format nil "DELETE FROM lex WHERE name=~a" qname))
    ;; delete from lex-key
    (run-command lex 
		 (format nil "DELETE FROM lex_key WHERE name = ~a" qname))
    ))
    
(defmethod insert-entry ((lex su-psql-lex-database) symb-list psql-le)
  (run-command lex 
	       (format nil "INSERT INTO lex ~a VALUES ~a" 
		       (sql-list symb-list 
				 #'(lambda (x) (quote-ident lex x)))
		       (sql-list (ordered-val-list symb-list psql-le) 
				 #'(lambda (x) (psql-quote-literal x))))))
  
(defmethod import-tdl-file ((lex su-psql-lex-database) filename)
  (declare (ignore filename))
  (if (next-method-p) (call-next-method))
  (format t "~&(LexDB) number of 'lex' entries: ~a" (table-size lex "lex")))

(defmethod semi-mod-unused ((lex su-psql-lex-database))
  (get-raw-records 
   lex 
   "select name from lex right join semi_mod using (name) where lex is null"))

(defmethod create-lex-key-indices ((lex su-psql-lex-database))
  ;(with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)" :ignore-errors t)
  ;  )
  )

(defmethod drop-lex-key-indices ((lex su-psql-lex-database))
  ;(with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t)
  ;  )
  )

(defmethod create-lex-key-table ((lex su-psql-lex-database))
  (run-command lex "CREATE TABLE lex_key (
		name TEXT NOT NULL,
		key text NOT NULL
		)"))


(defmethod retrieve-entry2 ((lex su-psql-lex-database) name &key (reqd-fields '("*")))
  (get-records lex
	       (format nil
		       "SELECT ~a FROM lex WHERE name LIKE ~a"
		       (fields-str lex reqd-fields)
		       (psql-quote-literal name))))

(defmethod get-internal-table-dfn ((lex su-psql-lex-database))
  (let ((res (get-field-info lex "public" "lex")))
    (or
     res
     (error "cannot find fields for table 'lex'"))))

;;!
;; DOT: select FIELDS for revision entry ID NAME MODSTAMP
(defmethod get-dot-rev-record ((lex su-psql-lex-database) id name modstamp &optional (fields '("*")))
  (declare (ignore name modstamp))
  (unless (connection lex)
    (format t "~&(LexDB) WARNING:  no connection to mu-psql-lex-database")
    (return-from get-dot-rev-record))
  (let* ((fstr (fields-str lex fields))
	 (qid (psql-quote-literal id))
	 (table
	  (get-records 
	   lex (format nil
		       "SELECT ~a FROM lex WHERE name = ~a"
		       fstr qid))))
    (dot (cols table) (car (recs table)))))

(defmethod lookup ((lex su-psql-lex-database) field-kw val-str &key (ret-flds "*") (from "lex"))
  (declare (ignore from))
  (cond
   (val-str
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM lex WHERE ~a ILIKE ~a"
			     ret-flds ;from
			     (quote-ident lex field-kw)
			      (psql-quote-literal val-str))))
   (t
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM lex WHERE ~a IS NULL"
			     ret-flds ;from
			     (quote-ident lex field-kw))))))

(defmethod scratch-records ((lex su-psql-lex-database))
  (error "functionality not available for SINGLE-USER LexDB "))

(defmethod delete-record ((lex su-psql-lex-database) record)
  (let* ((id (cdr (assoc :|name| record)))
	 (qid (psql-quote-literal id)))
    (run-command lex (format nil "DELETE FROM lex WHERE name=~a" qid))))

;; this belongs here due to use of low-level get-raw-records
(defmethod create-unnormalized-missing-lex-keys ((lex su-psql-lex-database))
  (loop
      for rec in
	(get-raw-records lex
			 (format nil (create-unnormalized-missing-lex-keys3-FSQL lex)
				 (orth-field lex)))
      for orth-list = (string-2-str-list (second rec)) ;!
      if (= 1 (length orth-list))
      collect rec
      else
      append
      (loop for word in orth-list
	  collect (list (first rec) word))))
