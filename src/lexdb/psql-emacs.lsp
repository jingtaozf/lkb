;;; Copyright (c) 2002 - 2005
;;;   Ben Waldron, Ann Copestake, Fabre Lambeau, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;
;;; Emacs-Postgres interface
;;;

(in-package :lkb)

(defconstant *lexdb-emacs-other-fns*
    '(initialize-lexdb
      lexdb-fn))

(defparameter *lexdb-emacs-lexdb-fns*
    '(complete
      connection
      dbname
      empty-cache
      fields
      get-field-size-map
      get-value-set
      id-to-tdl-str
      lookup
      lookup-rev-all
      new-entries
      record-to-tdl
      retrieve-head-record-str
      retrieve-record-ium
      set-lex-entry
      set-lex-entry-from-record
      scratch-records
      get-records
      sql-fn-get-records
      sql-fn-get-raw-records
))
 
(defun lexdb-fn (fn-name &rest rest)
  (unless (member fn-name *lexdb-emacs-lexdb-fns*)
    (error "~a not in *lexdb-emacs-lexdb-fns*" fn-name))
  (if *lexdb*
      (apply fn-name (cons *lexdb* rest))))

(defun field-size-elt (raw-rec cols)
  (let ((attname (get-val :ATTNAME raw-rec cols))
	(typname (get-val :TYPNAME raw-rec cols))
;	(typname (get-val :TYPENAME raw-rec cols))
	(atttypmod (str-2-num (get-val :ATTTYPMOD raw-rec cols)
			      0)))
    (list (str-2-keyword attname) typname (field-len typname atttypmod))))

(defun field-len (typname atttypmod)
  (cond
   ((and (string= typname "_varchar") (> atttypmod 5))
    (- atttypmod 4))
   (t
    50)))
