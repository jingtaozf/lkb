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
		  (namestring (pathname (format nil "~a.rev" filename))))
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

(defmethod dump-dfn-fld ((lexicon psql-lex-database) filename)
  (unless (string>= (lexdb-version lexicon) "3.32")
    (error "operation requires LexDB version 3.32 or above"))
  (when
      (catch 'pg:sql-error
	(progn
	  (get-postgres-temp-filename)
	  (format t "~%(dumping LexDB .dfn .fld)")
	  (force-output)
	  (let* ((pg-files 
		  (string-2-str-list-on-spc
		   (caar (sql-fn-get-raw-records *psql-lexicon* :dump_db_dfn_fld))
		   :esc nil))
		 (pg-dfn (first pg-files))
		 (pg-fld (second pg-files)))
	    (common-lisp-user::run-shell-command 
	     (format nil "cp ~a ~a" 
		     pg-dfn 
		     (namestring (pathname (format nil "~a.dfn" filename)))))
	    (common-lisp-user::run-shell-command 
	     (format nil "cp ~a ~a" 
		     pg-fld 
		     (namestring (pathname (format nil "~a.fld" filename)))))
	    nil)))
    (format t "~%Dump aborted...")
    :dump_aborted))

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

;;;
;;;
;;;

(defun absolute-namestring (format str)
  (namestring (pathname (format nil format str))))

(defun get-postgres-temp-filename nil
  (setf *postgres-temp-filename*
    (format nil "~a.~a" "/tmp/postgres-temp" (sys:user-name))))

