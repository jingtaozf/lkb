(in-package :lkb)

;;;
;;; --- sql-query methods and functions
;;;

(defmethod print-object ((object sql-query) stream)
  (with-slots 
      (sql-string records columns count-records mapped-recs) 
      object
    (let ((records-c (if (listp records)
			 (length records)
		       records))
	  (columns-c (if (listp columns)
			 (length columns)
		       columns)))
      (format
       stream
       "#[SQL-QUERY: ~a~%  [~a record~p; ~a column~p]]"
       sql-string
       records-c records-c 
       columns-c columns-c
       ))))

;;; returns _association list_
(defmethod make-column-map-record ((query sql-query))
    (loop 
        for element in (records query)
        collect (mapcar #'cons (columns query) element)))

