;;; Copyright (c) 2002-2003 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; Emacs-Postgres interface
;;;

(in-package :lkb)

(defun lexdb-fn (fn-name &rest rest)
  (if *psql-lexicon*
      (apply fn-name (cons *psql-lexicon* rest))))

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