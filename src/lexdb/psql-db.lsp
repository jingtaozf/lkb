;;; Copyright (c) 2002-2004
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Postgres interface
;;;

(defun dump-psql-lexicon (filename &key tdl)
  (when
      (catch 'pg:sql-error
	(progn
	  (get-postgres-temp-filename)
	  (format t "~%(dumping LexDB)")
	  (force-output)
	  (let* ((revision-filename 
		  (namestring (pathname (format nil "~a.tsv" filename))))
		 (defn-filename 
		     (namestring (pathname (format nil "~a.dfn" filename))))
		 (fld-filename 
		     (namestring (pathname (format nil "~a.fld" filename))))
		 (tdl-filename 
		     (namestring (pathname (format nil "~a.~a.tdl" filename (get-filter *psql-lexicon*)))))
		 (pg-files 
		  (string-2-str-list-on-spc
		   (caar (sql-fn-get-raw-records *psql-lexicon* :dump_db))
		   :esc nil))
		 (pg-rev (first pg-files))
		 (pg-dfn (second pg-files))
		 (pg-fld (third pg-files)))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-rev
							 revision-filename))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-dfn
							 defn-filename))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-fld
							 fld-filename))
	    (when tdl
	      (format t "~%(exporting filtered LexDB to TDL file ~a)" tdl-filename)
	      (force-output)
	      (export-to-tdl-to-file *psql-lexicon* tdl-filename))
	    nil)))
    (format t "~%Dump aborted...")))

(defun dump-scratch (filename)
  (get-postgres-temp-filename)
  (setf filename (namestring (pathname filename)))
  (sql-fn-get-raw-records *psql-lexicon*
			  :dump_scratch_db
			  :args (list 
				 (namestring 
				  (pathname *postgres-temp-filename*))))
  (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
					       *postgres-temp-filename*
					       filename)))

(defun merge-into-psql-lexicon (lexicon filename)
  "reconnect as db owner and merge new data into lexdb"
  (with-slots (dbname host port) lexicon
    (unless dbname
      (error "please set :dbname"))
    (let ((conn-db-owner (make-instance 'psql-lex-database
			   :dbname dbname
			   :host host
			   :port port
			   :user (sql-fn-get-val lexicon
						 :db_owner)))
;			   :user (raw-get-val lexicon "SELECT db_owner()")))
	  (count-new 0))
      (connect conn-db-owner)
      ;(retrieve-fn-defns conn-db-owner)
      (when
	  (catch 'pg:sql-error
	    (progn
	      (get-postgres-temp-filename)
	      (let* ((rev-filename 
		      (absolute-namestring "~a.tsv" 
					   filename))
		     (dfn-filename 
		      (absolute-namestring "~a.dfn" 
					   filename)))
		(if (probe-file rev-filename)
		    (setf count-new 
		      (merge-into-db conn-db-owner 
				     rev-filename))
		  (format t "~%WARNING: no file ~a" rev-filename))
		(cond
		 ((probe-file dfn-filename)
		  (merge-defn conn-db-owner 
			      dfn-filename)
		  (make-field-map-slot lexicon))
		 (t
		  (format t "~%WARNING: no file ~a" dfn-filename)))
		nil
		)))
	(format t "Merge new entries aborted..."))
      (if (and 
	   (equal count-new 0))
	  (empty-cache lexicon)
	(initialize-psql-lexicon))
      (disconnect conn-db-owner))))

;;;
;;;
;;;

(defun absolute-namestring (format str)
  (namestring (pathname (format nil format str))))

(defun get-postgres-temp-filename nil
  (setf *postgres-temp-filename*
    (format nil "~a.~a" "/tmp/postgres-temp" (sys:user-name))))

