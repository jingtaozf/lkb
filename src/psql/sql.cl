;;; -*- Mode: Common-Lisp; Package: PG; -*-

;; This software is Copyright (c) Marina Motion LLC, November 2001.
;; Marina Motion LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :pg)

(defparameter *DEFAULT-DATABASE* nil)

(defmacro WITH-DATABASE-2 (db-props &body body)
  `(let ((*default-database* (connect-db ,db-props)))
     (unwind-protect
	 (progn
	   (unless (eq :connection-ok (decode-connection-status (status *default-database*)))
	     (error "BAD CONNECTION TO ~s: ~a" (db *default-database*) (error-message *default-database*)))
	   ,@body)
	 (finish *default-database*))))

(defmacro WITH-DATABASE ((&key (host "localhost") (dbname "db") (user (sys:user-name)) (password "")) &body body)
  `(let ((*default-database* (connect-db
			      ,(format nil "host=~a dbname=~a user=~a password=~a"
				       host dbname user password))))
     (unwind-protect
	 (progn
	   (unless (eq :connection-ok (decode-connection-status (status *default-database*)))
	     (error "BAD CONNECTION TO ~s: ~a" (db *default-database*) (error-message *default-database*)))
	   ,@body)
       (finish *default-database*))))
	 
(defun SQL (sql-statement &key (db *default-database*) (column-names t) (row-count nil)) 
  (let ((result (exec db sql-statement)))
    (unwind-protect
	(let ((exec-status (decode-exec-status (result-status result))))
	  (unless (or (eq :pgres-command-ok exec-status)
		      (eq :pgres-tuples-ok exec-status))
	    (error "BAD COMMAND TO ~s: ~a" (db db) (result-error-message result)))
	  (let ((field-names (loop for field below (nfields result)
				 collect (field-name result field)))
		(result-table (loop for tuple below (ntuples result)
				  collect (loop for field below (nfields result)
					      collect (getvalue result tuple field)))))
	    (cond 
	     ((and (null row-count) column-names)
	      (values result-table field-names))
	     ((and (null row-count) (null column-names))
	      result-table)
	     ;; 
	     (row-count			;for update, insert, or delete
	      (cmd-tuples result))	;number of rows affected
	     
	     )
	    ))
      (clear result))))
