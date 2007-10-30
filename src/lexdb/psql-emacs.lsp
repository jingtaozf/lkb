;;; Copyright (c) 2002 - 2006
;;;   Ben Waldron, Ann Copestake, Fabre Lambeau, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;
;;; Emacs-Postgres interface
;;;

(in-package :lkb)

(defparameter *check-pg-interface-version* 2.21)

(define-constant *lexdb-emacs-other-fns*
    '(initialize-lexdb
      lexdb-fn
      ))

(defparameter *lexdb-emacs-lexdb-fns*
    '(complete
      check-pg-interface-version
      clear-private-rev
      commit-private-rev
      dump-lexdb2
      dump-tdl-lexdb2
      connection
      dbname
      delete-record
      empty-cache
      fields
      get-field-size-map
      get-value-set
      id-to-tdl-str
      lookup
      lookup-rev-all
      merge-lexdb2
      new-entries
      orth-field-kw
      record-to-tdl
      set-lex-entry
      set-lex-entry-from-record
      scratch-records
      get-records
      sql-fn-get-records
      sql-fn-get-raw-records
      
      get-dot-lex-record
      get-dot-rev-record
      grammar-fields
))
 
(defun lexdb-fn (fn-name &rest rest)
  (unless (member fn-name *lexdb-emacs-lexdb-fns*)
    (error "~a not in *lexdb-emacs-lexdb-fns*" fn-name))
  (if *lexdb*
      (apply fn-name (cons *lexdb* rest))))

(defun field-size-elt (raw-rec cols)
  (let ((attname (get-val :|attname| raw-rec cols))
	(typname (get-val :|typname| raw-rec cols))
	(atttypmod (str-2-num (get-val :|atttypmod| raw-rec cols)
			      0)))
    (list (str-2-keyword attname) typname (field-len typname atttypmod))))

(defun field-len (typname atttypmod)
  (cond
   ((and (string= typname "_varchar") (> atttypmod 5))
    (- atttypmod 4))
   (t
    50)))

(defmethod check-pg-interface-version ((lex psql-lex-database) version)
  (unless (= version *check-pg-interface-version*)
    (error "Emacs/LexDB interface version ~a is incompatible with running LKB/LexDB. Please install pg-interface.el version ~a" version *check-pg-interface-version*)))

(defvar *cle-handled-types* '(list number string symbol))

(defun eval-for-cle (x)
  (if (eval 
       (cons 'or 
	     (mapcar #'(lambda (y) (typep x y)) *cle-handled-types*)))
      x '!!!unhandled-type!!!))

(defmethod dump-lexdb2 ((lex psql-lex-database) filename)
  (unless (eq *lexdb* lex)
    (error "Lisp is confused: ~a should be eq to *lexdb* ..." lex))
  (command-dump-lexdb filename))

(defmethod dump-tdl-lexdb2 ((lex psql-lex-database) filename)
  (unless (eq *lexdb* lex)
    (error "Lisp is confused: ~a should be eq to *lexdb* ..." lex))
  (command-export-lexicon-to-tdl filename))

(defmethod merge-lexdb2 ((lex psql-lex-database) filename)
  (unless (eq *lexdb* lex)
    (error "Lisp is confused: ~a should be eq to *lexdb* ..." lex))
  (command-merge-into-lexdb filename))