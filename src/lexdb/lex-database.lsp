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

