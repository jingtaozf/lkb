;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;
;; EXPORT
;;

;; dump non-LexDB lexicon to file (DB format)
(defmethod export-to-db-dump-rev ((lexicon lex-database) stream &key (skip-stream t))
  (unless (connection *lexdb*)
    (error "no connection to LexDB"))
  (mapc 
   #'(lambda (x) 
       (format stream "~a" 
	       (to-db-dump-rev (read-psort lexicon x 
				       :recurse nil
				       :cache nil
				       :new-instance t) 
			   *lexdb*
			   :skip-stream skip-stream)))
   (collect-psort-ids lexicon :recurse nil)))

;; dump lexicon to file (TDL format)
(defmethod export-to-tdl-to-file ((lexicon lex-database) filename &key lex-ids recurse)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream :lex-ids lex-ids :recurse recurse)))

(defmethod export-to-tdl ((lexicon lex-database) stream &key lex-ids recurse)
  (when (typep lexicon 'psql-lex-database)
    (format t "~&(LexDB) caching all lexical records")
    (cache-all-lex-records lexicon)
    (format t "~&(LexDB) caching complete"))
  (unless lex-ids (setf lex-ids (collect-psort-ids lexicon :recurse recurse)))
  (format t "~&[~a entries]" (length lex-ids))

  (loop
      for id in (sort (copy-list lex-ids)
		      #'(lambda (x y) (string< (2-str x) (2-str y))))
      do
	(format stream "~a" (to-tdl (read-psort lexicon id :new-instance t)))
	(forget-psort lexicon id))
  
  (when (typep lexicon 'psql-lex-database)
    (format t "~&(LexDB) emptying cache")
    (empty-cache lexicon)))

(defmethod id-to-tdl-str ((lexicon lex-database) id)
  (id-to-tdl lexicon (str-2-symb id)))

(defmethod id-to-tdl ((lexicon lex-database) id)
  (to-tdl (read-psort lexicon id)))

;;
;;

