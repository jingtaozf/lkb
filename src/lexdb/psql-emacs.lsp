;;; Copyright (c) 2002-2003 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; Emacs-Postgres interface
;;;

(in-package :lkb)

(defconstant *lexdb-emacs-other-fns*
    '(initialize-psql-lexicon
      lexdb-fn))

(defconstant *lexdb-emacs-lexdb-fns*
    '(complete
      connection
      dbname
      empty-cache
      fields
      get-field-size-map
      id-to-tdl-str
      lookup
      new-entries
      record-to-tdl
      retrieve-head-record-str
      set-lex-entry-from-record
      scratch-records))
 
(defun lexdb-fn (fn-name &rest rest)
  (unless (member fn-name *lexdb-emacs-lexdb-fns*)
    (error "~a not in *lexdb-emacs-lexdb-fns*" fn-name))
  (if *psql-lexicon*
      (apply fn-name (cons *psql-lexicon* rest))))

(defun field-size-elt (raw-rec cols)
  (let ((attname (get-val :ATTNAME raw-rec cols))
	(typname (get-val :TYPENAME raw-rec cols))
	(atttypmod (str-2-num (get-val :ATTTYPMOD raw-rec cols)
			      0)))
    (list (str-2-keyword attname) typname (field-len typname atttypmod))))

(defun field-len (typname atttypmod)
  (cond
   ((and (string= typname "_varchar") (> atttypmod 5))
    (- atttypmod 4))
   (t
    50)))
