;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;; --- su-psql-lex-database methods
;;;

(defmethod lookup-word-no-cache-SQL ((lex su-psql-lex-database) quoted-literal fields)
  (format nil "SELECT ~a FROM (SELECT lex.* FROM lex JOIN lex_key USING (name) WHERE lex_key.key LIKE ~a) as foo" fields quoted-literal))

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
  (let ((ql-name (psql-quote-literal (retr-val psql-le :|name|))))
    (run-command lex 
		 (format nil "DELETE FROM lex WHERE name=~a" ql-name))
    (run-command lex 
		 (format nil "INSERT INTO lex ~a VALUES ~a" 
			 (sql-list symb-list 
				   #'(lambda (x) (quote-ident lex x)))
			 (sql-list (ordered-val-list symb-list psql-le) 
				   #'(lambda (x) (psql-quote-literal x)))))
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
