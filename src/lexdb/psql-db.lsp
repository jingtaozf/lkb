;;; Copyright (c) 2002-2004
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;
;;; Postgres interface
;;;

(defun dump-psql-lexicon (filename)
  (when
      (catch 'pg:sql-error
	(progn
	  (get-postgres-temp-filename)
	  (let* ((revision-filename 
		 (namestring (pathname (format nil "~a.csv" filename))))
		(defn-filename 
		    (namestring (pathname (format nil "~a.dfn" filename))))
		 (pg-files 
		  (string-2-str-list-on-spc
;		   (dump-db *psql-lexicon*)
		   (caar (sql-fn-get-raw-records *psql-lexicon* :dump_db))
		   :esc nil))
		 (pg-rev (first pg-files))
		 (pg-dfn (second pg-files)))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-rev
							 revision-filename))
	    (common-lisp-user::run-shell-command (format nil "cp ~a ~a"
							 pg-dfn
							 defn-filename))
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
		      (absolute-namestring "~a.csv" 
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

