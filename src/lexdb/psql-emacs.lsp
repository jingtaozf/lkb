;;; Copyright (c) 2002-2003 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; Emacs-Postgres interface
;;;

(in-package :lkb)

(defun lexdb-fn (fn-name &rest rest)
  (if *psql-lexicon*
      (apply fn-name (cons *psql-lexicon* rest))))

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

(defun field-size-elt (x)
  (let ((attname (get-val :ATTNAME x))
	(typname (get-val :TYPNAME x))
	(atttypmod (str-2-num (get-val :ATTTYPMOD x)
			      0)))
    (list (str-2-keyword attname) typname (field-len typname atttypmod))))

(defun field-len (typname atttypmod)
  (cond
   ((and (string= typname "_varchar") (> atttypmod 5))
    (- atttypmod 4))
   (t
    50)))

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
