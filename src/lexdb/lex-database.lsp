;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod export-to-csv ((lexicon lex-database) stream)
    (unless (connection *psql-lexicon*)
      (error "no connection to LexDB"))
    (mapc 
     #'(lambda (x) 
	 (format stream "~a" (to-csv (read-psort lexicon x 
						 :recurse nil
						 :cache nil
						 :new-instance t) 
				     *psql-lexicon*)))
     (collect-psort-ids lexicon :recurse nil)))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-csv lexicon ostream)))

