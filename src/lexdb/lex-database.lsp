;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;; dump non-LexDB lexicon to file (DB format)
(defmethod export-to-db-dump ((lexicon lex-database) stream)
    (unless (connection *psql-lexicon*)
      (error "no connection to LexDB"))
    (mapc 
     #'(lambda (x) 
	 (format stream "~a" 
		 (to-db-dump (read-psort lexicon x 
					   :recurse nil
					   :cache nil
					   :new-instance t) 
			       *psql-lexicon*)))
     (collect-psort-ids lexicon :recurse nil)))

(defmethod export-to-db-dump-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-db-dump lexicon ostream)))

;; dump lexicon to file (TDL format)
(defmethod export-to-tdl-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

(defmethod export-to-tdl ((lexicon lex-database) stream)
  (when (typep lexicon 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records lexicon)
    (format t "~%(caching complete)"))
  (mapc
   #'(lambda (id)
       (format stream "~a" (to-tdl (read-psort lexicon id :new-instance t)))
       (unexpand-psort lexicon id))
   (sort (collect-psort-ids lexicon) 
	 #'(lambda (x y) (string< (2-str x) (2-str y)))))
  (when (typep lexicon 'psql-lex-database)
    (format t "~%(emptying cache)")
    (empty-cache lexicon)))

;; place port lexicon to LexDB
(defmethod export-to-db ((lexicon lex-database) (lexdb psql-lex-database))
  (mapc
   #'(lambda (x) 
       (to-db (read-psort lexicon x :recurse nil :new-instance t) 
	      lexdb))
   (collect-psort-ids lexicon :recurse nil))
  (build-lex-aux lexdb))

