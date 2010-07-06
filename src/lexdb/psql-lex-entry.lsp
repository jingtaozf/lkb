;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;;
;;; --- psql-lex-entry methods
;;;

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

;;; prepare val list for SQL INSERT INTO query
(defun ordered-val-list (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (loop 
      while symb-list
      collect (retr-val psql-le (pop symb-list))))


(defmethod get-raw-orth ((lexicon psql-lex-database) (le psql-lex-entry))
  (let* ((orth-raw-mapping (assoc :orth (dfn lexicon)))
	 (orth-raw-value-mapping (fourth orth-raw-mapping))
	 (raw-orth-field (second orth-raw-mapping)))
    (car (work-out-value
	  orth-raw-value-mapping
	  (retr-val le raw-orth-field)))))

(defmethod lexicon-le-orthkey ((lexicon psql-lex-database) (le psql-lex-entry))
  (normalize-orthkey (car (last (get-raw-orth lexicon le)))))
