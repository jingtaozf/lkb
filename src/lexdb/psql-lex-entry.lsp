;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; --- psql-lex-entry methods
;;;

(defun make-instance-psql-lex-entry (&rest rest)
  (make-instance 'psql-lex-entry
    :fv-pairs (kwl2alist rest)))

(defmethod clear-vals ((le psql-lex-entry))
  (setf (slot-value le 'fv-pairs) 
    nil))

(defmethod retr-val ((le psql-lex-entry) f)
  (cdr 
   (assoc f (slot-value le 
			'fv-pairs))))

(defmethod set-val ((le psql-lex-entry) f v)
  (with-slots (fv-pairs) le
    (let ((fv-pair (assoc f fv-pairs)))
      (if fv-pair
	  (setf (cdr fv-pair) v)
	(push (cons f v) fv-pairs)))))

;; obsolete
;;; set version to next val
(defmethod set-version ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (set-val psql-le 
	   :version 
	   (next-version 
	    (retr-val psql-le :name) 
	    lexicon)))

;;; prepare val list for SQL INSERT INTO query
(defun ordered-val-list (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (loop 
      while symb-list
      collect (retr-val psql-le (pop symb-list))))


(defmethod get-raw-orth ((lexicon psql-lex-database) (le psql-lex-entry))
  (let* ((orth-raw-mapping (assoc :orth (fields-map lexicon)))
	 (orth-raw-value-mapping (fourth orth-raw-mapping))
	 (raw-orth-field (second orth-raw-mapping)))
    (car (work-out-value
	  orth-raw-value-mapping
	  (retr-val le raw-orth-field)))))

(defmethod lexicon-le-orthkey ((lexicon psql-lex-database) (le psql-lex-entry))
  (car (last (get-raw-orth lexicon le))))
