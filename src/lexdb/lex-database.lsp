(in-package :lkb)

(defmethod export-to-csv ((lexicon lex-database) stream)
  (let ((fields-map
	 (and *psql-lexicon* (fields-map *psql-lexicon*))))
    (unless fields-map
      (error "no fields map: plase connect to a LexDB"))
    (format t "~%Export fields map:~%~a~%" fields-map)
    (mapc 
     #'(lambda (x) 
	 (format stream "~a" (to-csv (read-psort lexicon x 
						 :recurse nil
						 :cache nil
						 :new-instance t
						 ) fields-map)))
     (collect-psort-ids lexicon :recurse nil))))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-csv lexicon ostream)))

;;;
;;; export to DB
;;;

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x 
				    :recurse nil
				    :new-instance t) output-lexicon))
   (collect-psort-ids lexicon :recurse nil))
  (build-lex-aux *psql-lexicon*))

(defmethod export-to-tdl ((lexicon lex-database) stream)
  #+:psql
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records *lexicon*)
    (format t "~%(caching complete)")
    )
  (mapc
   #'(lambda (id)
       (format stream "~a" (to-tdl (read-psort lexicon id
					       :new-instance t)))
       (unexpand-psort lexicon id))
   (collect-psort-ids lexicon))
  #+:psql
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(emptying cache)")
    (empty-cache *lexicon*)))

(defmethod export-to-tdl-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

