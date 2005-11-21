;;; Copyright (c) 2003 - 2005
;;;   Benjamin Waldron;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

(defun load-sdbt (sdbt dbname)
  (clear sdbt)
  (let* ((records
	  (lkb::get-raw-records dbname
				(format nil
					"SELECT * FROM semi_~a"
					(sdbt-name sdbt)))
	  ))
    (mapc #'(lambda (row) (sdbt-rows-hash 
			   (mapcar #'str-to-mixed2 row)
			   (sdbt-rows sdbt)))
	  records)
    (setf (sdbt-last sdbt) nil)))

(defun load-sdb (sdb dbname)
  (mapcar #'(lambda (x)
	      (load-sdbt x dbname))
	  (sdb-tables sdb)))

(defconstant *psql-semi-dump-base*
    (format nil "~a/semi.obj." 
	    (make-pathname :directory (namestring (lkb::lkb-tmp-dir)))))

#+:null
(defmethod dump-semi-to-psql ((semi semi) &key (lexicon lkb::*lexdb*))
  (populate-semi semi)
  (print-semi-db semi)
  (lkb::semi-setup-pre lexicon)
  (update-psql-semi-from-files :lex lexicon)
  (lkb::semi-setup-post lexicon))

#+:null
(defmethod semi-create-indices ((lex psql-lex-database))
  (run-command lex "
;
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
CREATE UNIQUE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
"))

#+:null
(defmethod semi-drop-indices ((lex psql-lex-database))
  (run-command-coe lex "DROP INDEX semi_pred_lex_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_pred_pred_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_frame_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_var_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_extra_extra_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_mod_name_userid_modstamp CASCADE"))

(defun sdb-to-psql (lex sdb)
  (lkb::clear-psql-semi lex)
  
  (lkb::run-command-stdin-from-hash-val-rows lex "semi_pred"
					(sdbt-rows (find 'PRED (sdb-tables sdb) 
					      :key #'mrs::sdbt-name)))
  (lkb::run-command-stdin-from-hash-val-rows lex "semi_frame"
					(sdbt-rows (find 'frame (sdb-tables sdb) 
					      :key #'mrs::sdbt-name)))
  (lkb::run-command-stdin-from-hash-val-rows lex "semi_var"
					(sdbt-rows (find 'var (sdb-tables sdb) 
					      :key #'mrs::sdbt-name)))
  (lkb::run-command-stdin-from-hash-val-rows lex "semi_extra"
					(sdbt-rows (find 'extra (sdb-tables sdb) 
					      :key #'mrs::sdbt-name)))
  
  (lkb::semi-create-indices lex)
  )

(defun dump-generator-indices-to-psql (&key (lex lkb::*lexdb*))
  (sdb-to-psql lex
	       (populate-sdb :semantic-table *semantic-table*))

  (lkb::run-command lex "INSERT INTO semi_mod (SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex_cache JOIN semi_pred ON name=lex_id)")
  (let ((not-indexed mrs::*empty-semantics-lexical-entries*))
    (when not-indexed
      (lkb::run-command lex
			(format nil "INSERT INTO semi_mod SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex_cache WHERE name IN ~a" 
				(format nil " (~a~{, ~a~})" 
					(lkb::psql-quote-literal (car not-indexed))
					(loop for lexid in (cdr not-indexed)
					    collect (lkb::psql-quote-literal lexid)))))))
  ;; semi_mod indexes should be created after this call
  (lkb::run-command lex "SET ENABLE_HASHJOIN TO false")
  )

#+:null
(defmethod put-normalized-lex-keys ((lex psql-lex-database) recs)
  (when recs
    (let ((conn (connection lex)))
      (with-lexdb-client-min-messages (lex "error")
	(run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
      (pq:exec conn "COPY lex_key FROM stdin")
      (loop
	  for rec in recs
	  do 
	    (with-lexdb-locale (pq:putline conn (to-psql-copy-rec2 rec))))
      (with-lexdb-locale (putline conn "\\."))
      (endcopy conn)
      (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)")
      )))

(defun update-psql-semi (lexids &key (lex lkb::*lexdb*)
				     (semantic-table *semantic-table*))
  (when (null lexids)
    (return-from update-psql-semi))
  (print-semi-db-partial lexids :semantic-table semantic-table)
  (let* ((use-temp-table (> (length lexids) 1000))
	 (where 
	  (cond
	   (use-temp-table
	    (lkb::run-command-ignore-errors lex "DROP TABLE scratch_update_psql_semi")
	    (lkb::copy-column-to-psql lex "scratch_update_psql_semi" lexids)
	    "(SELECT * FROM scratch_update_psql_semi)"
	    )
	   (t
	    (format nil " (~a~{, ~a~})" 
		    (lkb::psql-quote-literal (car lexids))
		    (loop for lexid in (cdr lexids)
			collect (lkb::psql-quote-literal lexid)))
	    ))))
    (lkb::run-command lex 
		      (format nil "DELETE FROM semi_pred WHERE lex_id IN ~a" where))
    ;; semi_mod info belongs in semi_pred?
    (lkb::run-command lex
		      (format nil "DELETE FROM semi_mod WHERE name IN ~a" where))
    ;; to_do: ensure duplicates are not added to semi tables!
    (update-psql-semi-from-files :lex lex)
    (lkb::run-command lex
		      (format nil "INSERT INTO semi_mod SELECT DISTINCT name,userid,modstamp,CURRENT_TIMESTAMP FROM lex_cache WHERE name IN ~a" where))
    (when use-temp-table
      (lkb::run-command-coe lex "DROP TABLE scratch_update_psql_semi"))
    ))
  
(defun update-psql-semi-from-files (&key (lex lkb::*lexdb*))
  (let* ((base (format nil "~asemi.obj" 
		       (make-pathname :directory (namestring (lkb::lkb-tmp-dir))))))
    (update-db-table-from-file "semi_pred" (format nil "~a.~a" base "pred") lex)
    (update-db-table-from-file "semi_frame" (format nil "~a.~a" base "frame") lex)
    (update-db-table-from-file "semi_var" (format nil "~a.~a" base "var") lex)
    (update-db-table-from-file "semi_extra" (format nil "~a.~a" base "extra") lex)))

(defun prune-semi (&key (lex lkb::*lexdb*))
  (loop 
      for x in (lkb::get-raw-records lex "select name from lex_cache right join semi_mod using (name) where lex_cache is null")
      do
	(lkb::run-command lex (format nil "DELETE FROM semi_mod WHERE name=~a" 
				      (lkb::psql-quote-literal (car x))))))

(defun load-generator-indices-from-psql (&key (lexdb lkb::*lexdb*))
  (let ((sdb (make-sdb)))
    (prune-semi)
    (load-sdb sdb lexdb)
    (populate-semantic-table sdb)
    (setf *empty-semantics-lexical-entries*
      (loop for x in (lkb::get-raw-records lkb::*lexdb* "select name from semi_mod left join semi_pred on name=lex_id where semi_pred.lex_id is null")
	  collect (lkb::str-2-symb (car x)))))
  t)

(defmethod populate-semantic-table ((sdb sdb))
  (let* ((pred-t (sdb-table sdb 'pred))
	 (pred-r (sdbt-rows pred-t)))
    (loop
	for lex-id being each hash-key in pred-r
	for record = (load-lex-id-db lex-id sdb)
	do
	  (add-semantics-record lex-id record)
	  ))
  (setf *sdb* nil)
  *semantic-table*)

#+:null
(defun prepare-cached-lexicon-index (&key (lexdb lkb::*lexdb*))
  (setf *sdb* (make-sdb))
  (load-sdb *sdb* lexdb)
  (populate-relation-index *sdb*)
  (make-semi))

#+:null
(defun populate-relation-index (sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
	 (pred-r (sdbt-rows pred-t)))
    (clrhash *relation-index*)
    (loop
	for rows being each hash-value in pred-r
	do
	  (loop
	    for row in rows
	      for id = (first row)
	      for rel = (second row)
	      do
		(index-simple-semantics-record rel id)
		;(setf (gethash rel *relation-index*) t)
		)))
    *relation-index*
    )

;;; -> lex-id
;;; semantics_record.id = lex-id
;;;                 .relations = frame-list
(defun load-lex-id-db (lex-id sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
       (pred-r (sdbt-rows pred-t))
       (rows (gethash lex-id pred-r)))
    (make-semantics-record :id lex-id
                         :relations (load-relations-db rows sdb))))

;;; -> lex-id
;;; semantics_record.id = lex-id
;;;                 .relations = frame-list
(defun load-lex-id-psql (lex-id db)
  (let* (
	 ;(pred-t (sdb-table sdb 'pred))
	 ;(pred-r (sdbt-rows pred-t))
	 (rows (getrows lex-id 'pred db)))
    (make-semantics-record :id lex-id
			   :relations (load-relations-psql rows db)))) 

;;; -> (lex-id pred frame-id)*
;;; rel-base*.pred = pred
;;;          .flist = role-list
(defun load-relations-db (rows sdb)
  (loop
      with leaf-hash = (sdb-leaf-hash sdb)
      for row in rows
      for string-p = (eq (fifth row) t) ;!
      for pred = (let* ((pred-raw 
			 (if string-p
			     (lkb::2-str (second row))
			   (second row)))
                      (pred-hash (gethash pred-raw leaf-hash)))
                 (or
                  pred-hash
                  (setf (gethash pred-raw leaf-hash) pred-raw)))
      for frame-id = (third row)
      for flist = (load-fvpairs-db frame-id sdb)
      for parameter-strings = (get-fvps-parameter-strings flist)
      collect
      (make-rel :pred pred
                :flist flist
		:parameter-strings parameter-strings
		)))

;;; -> frame-id
;;; (frame-id slot str symb var-id)
;;; fvpair*.feature = slot
;;;        .value   = slot-val
(defun load-fvpairs-db (frame-id sdb)
  (loop
      with frame-t = (sdb-table sdb 'frame)
      with frame-r = (sdbt-rows frame-t)
      with rows = (gethash frame-id frame-r)
      with leaf-hash = (sdb-leaf-hash sdb)
      for row in rows
      for slot = (second row)
      for str = (let* ((str-raw (third row))
                     (str-hash (gethash str-raw leaf-hash)))
                 (or
                  str-hash
                  (setf (gethash  str-raw leaf-hash) str-raw)))
      for symb = (fourth row)
      for var-id = (fifth row)
      for type = (let* ((type-raw (sixth row))
                     (type-hash (gethash type-raw leaf-hash)))
                 (or
                  type-hash
                  (setf (gethash type-raw leaf-hash) type-raw)))
      for slot-val = (cond
                    ((and str (null symb) (null var-id))
                     str)
                    ((and (null str) symb (null var-id))
                     symb)
                    ((and (null str) (null symb) var-id)
                     (make-var :type type
                               :extra (load-extra-list-db var-id sdb)
                               :id :dummy))
                    (t
                     (error "(str,symb,var-id)=(~a,~a,~a)"
                            str symb var-id)))
      collect
      (make-fvpair :feature slot
                   :value slot-val)))

;;; -> var-id
;;; (var-id extra-id)
;;; extrapair*.feature = feature
;;;           .value = value
(defun load-extra-list-db (var-id sdb)
  (loop
      with var-t = (sdb-table sdb 'var)
      with var-r = (sdbt-rows var-t)
      with rows = (gethash var-id var-r)
      for row in rows
      for extra-id = (second row)
      collect
      (load-extra-db extra-id sdb)))

;;; -> extra-id
;;; (extra-id feature value)
;;; extrapair.feature
;;;          .value
(defun load-extra-db (extra-id sdb)
  (let* ((extra-t (sdb-table sdb 'extra))
       (extra-r (sdbt-rows extra-t))
       (rows (gethash extra-id extra-r))
       (row (car rows))
       (feature (second row))
       (value (third row)))
    (unless (= 1 (length rows))
      (error "~a rows for extra-id=~a"
           (length rows) extra-id))
    (make-extrapair :feature feature
		    :value value)))

;;; -> (lex-id pred frame-id)*
;;; rel-base*.pred = pred
;;;          .flist = role-list
(defun load-relations-psql (rows db)
  (loop
      with leaf-hash = (sdb-leaf-hash *sdb*)
      for row in rows
      for string-p = (print (fifth row)) ;!
      for pred = (let* ((pred-raw 
			 (if string-p 
			     (lkb::2-str (second row)) ;!
			     (second row)))
			(pred-hash (gethash pred-raw leaf-hash)))
		   (or
		    pred-hash
		    (setf (gethash pred-raw leaf-hash) pred-raw)))
      for frame-id = (third row)
      for flist = (load-fvpairs-psql frame-id db)
      for parameter-strings = (get-fvps-parameter-strings flist)
      collect
	(make-rel :pred pred
		  :flist flist
		  :parameter-strings parameter-strings)))

;;; -> frame-id
;;; (frame-id slot str symb var-id)
;;; fvpair*.feature = slot
;;;        .value   = slot-val
(defun load-fvpairs-psql (frame-id db)
  (loop
      ;with frame-t = (sdb-table sdb 'frame)
      ;with frame-r = (sdbt-rows frame-t)
      with rows = (getrows frame-id 'frame db)
      with leaf-hash = (sdb-leaf-hash *sdb*)
      for row in rows
      for slot = (second row)
      for str = (let* ((str-raw (third row))
		       (str-hash (gethash str-raw leaf-hash)))
		   (or
		    str-hash
		    (setf (gethash  str-raw leaf-hash) str-raw)))
      for symb = (fourth row)
      for var-id = (fifth row)
      for type = (let* ((type-raw (sixth row))
		       (type-hash (gethash type-raw leaf-hash)))
		   (or
		    type-hash
		    (setf (gethash type-raw leaf-hash) type-raw)))
      for slot-val = (cond
		      ((and str (null symb) (null var-id))
		       str)
		      ((and (null str) symb (null var-id))
		       symb)
		      ((and (null str) (null symb) var-id)
		       (make-var :type type
				 :extra (load-extra-list-psql var-id db)
				 :id :dummy))
		      (t
		       (error "(str,symb,var-id)=(~a,~a,~a)"
			      str symb var-id)))
      collect
	(make-fvpair :feature slot
		     :value slot-val)))

;;; -> var-id
;;; (var-id extra-id)
;;; extrapair*.feature = feature
;;;           .value = value  
(defun load-extra-list-psql (var-id db)
  (loop 
      ;with var-t = (sdb-table sdb 'var)
      ;with var-r = (sdbt-rows var-t)
      with rows = (getrows var-id 'var db)
      for row in rows
      for extra-id = (second row)
      collect
	(load-extra-psql extra-id db)))

;;; -> extra-id
;;; (extra-id feature value)
;;; extrapair.feature
;;;          .value
(defun load-extra-psql (extra-id db)
  (let* (
	 ;(extra-t (sdb-table sdb 'extra))
	 ;(extra-r (sdbt-rows extra-t))
	 (rows (getrows extra-id 'extra db))
	 (row (car rows))
	 (feature (second row))
	 (value (third row)))
    (unless (= 1 (length rows))
      (error "~a rows for extra-id=~a" 
	     (length rows) extra-id))
    (make-extrapair :feature feature
		    :value value)))

(defun get-raw-rows (db table key val)
  (let ((rows 
	 (lkb::get-raw-records db
			       (format nil "SELECT * FROM ~a WHERE ~a=~a"
				       (lkb::quote-ident db table)
				       (lkb::quote-ident db key)
				       (lkb::quote-ident db (2-db-str val))))))
    (loop 
	for row in rows
	collect
	  (mapcar #'str-to-mixed2 row))))
  
(defun getrows (val table db)
  (let (
	(raw-rows
	 (case table
	   ('pred
	    (get-raw-rows db "semi_pred" "lex_id" val))
	   ('frame
	    (get-raw-rows db "semi_frame" "frame_id" val))
	   ('var
	    (get-raw-rows db "semi_var" "var_id" val))
	   ('extra
	    (get-raw-rows db "semi_extra" "extra_id" val))
	   (t
	    (error "unhandled table name")))))
    raw-rows))

(defun update-db-table-from-file (table-name file-name lex)
  (lkb::run-command-stdin-from-file lex
				    (format nil "COPY ~a FROM stdin;"
					    table-name
					    table-name) 
				    file-name))

(defun find-relpreds-from-lexid (lexid &key (semantic-table *semantic-table*))  
  (let ((sem-rec (gethash lexid semantic-table)))
    (unless sem-rec
      (return-from find-relpreds-from-lexid))
    (loop
	for rel in (semantics-record-relations sem-rec)
	collect (rel-pred rel))))

(defun delete-lexid-from-relation-index (lexid &key (relation-index *relation-index*) 
						     (semantic-table *semantic-table*))
  (loop
      for relpred in (find-relpreds-from-lexid lexid 
					       :semantic-table semantic-table)
      for val = (gethash relpred relation-index)
      do
	(cond
	 ((hash-table-p val)
	  (remhash lexid val)
	  (when (= 0 (hash-table-count val))
	    (remhash relpred relation-index)))
	 ((listp val)
	  (loop
	      for complex2 in val ; (FEAT . val3->(id1) val4->(id2 id3) ...)
	      for hash2 = (cdr complex2) ; val3->(id1) val4->(id2 id3)
	      with new-val
	      do
		(loop
		    for key2 being each hash-key in hash2 ; val4
		    for val2 being each hash-value in hash2 ; (id2 id3)
		    with new-val2
		    when (member lexid val2)
		    do 
		      (setf new-val2 (delete lexid val2)) ; 
		      (if new-val2
			  (setf (gethash key2 hash2) new-val2) ; (id3)
			(remhash key2 hash2))) ; val3->(id1)
		(when (= 0 (hash-table-count hash2))
		  (setf new-val (delete complex2 val))
		  (if new-val
		      (setf (gethash relpred relation-index) new-val)
		    (remhash relpred relation-index)))))
	 (t
	  (error "unexpected value in relation-index hash ~a : ~a"
		 relation-index val)))))

(defun delete-lexid-from-semantic-table (lexid &key (semantic-table *semantic-table*))
  (remhash lexid semantic-table))

(defun delete-lexid-from-generator-indices (lexid &key (relation-index *relation-index*) 
						       (semantic-table *semantic-table*))
  (cond
   ((gethash lexid semantic-table)
    (delete-lexid-from-relation-index lexid 
				      :relation-index relation-index
				      :semantic-table semantic-table)
    (delete-lexid-from-semantic-table lexid 
				      :semantic-table semantic-table)
    t)
   (t
    nil)))
  
