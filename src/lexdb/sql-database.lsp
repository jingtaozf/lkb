(in-package :lkb)

;;;
;;; --- sql-database methods
;;;

(defmethod close-lex ((lexicon sql-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (dbname host user password port) lexicon
    ;(setf dbname nil)
    ;(setf host nil)
    ;(setf user nil)
    ;(setf password nil)
    ;(setf port nil)
    )                                   ;:todo: unbind functions
  (if (next-method-p) (call-next-method)))

(defmethod true-port ((lexicon sql-database))
  (let* ((port (or
		(port lexicon)
		(car (excl.osi::command-output "echo $PGPORT")))))
    (if (equal port "")
	5432
      port)))
 
(defmethod get-records ((lexicon sql-database) sql-string)
  (make-column-map-record
   (get-raw-results lexicon sql-string)))

(defmethod get-raw-records ((lexicon sql-database) sql-string)
  (records (get-raw-results lexicon sql-string)))

(defmethod get-raw-results ((lexicon sql-database) sql-string)
   (run-query 
    lexicon 
    (make-instance 'sql-query :sql-string sql-string)))
