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

;;; set version to next val
(defmethod set-version ((psql-le psql-lex-entry) (lexicon psql-lex-database))
  (set-val psql-le 
	   :version 
	   (next-version 
	    (retr-val psql-le :name) 
	    lexicon)))

;;; prepare val list for SQL INSERT INTO query
(defun sql-val-list-str (symb-list psql-le)
  (if (null symb-list) (error (format nil "non-null list expected")))
  (let ((stream (make-string-output-stream)))
    (format stream "~a" (make-sql-val-str 
			 (retr-val psql-le (pop symb-list))))
    (loop 
	while symb-list
	do 
	  (format stream ",~a" (make-sql-val-str 
				(retr-val psql-le (pop symb-list)))))
    (get-output-stream-string stream)))

