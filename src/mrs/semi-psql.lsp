;;; Copyright (c) 2003-2004
;;;   Ben Waldron;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

(defun load-sdbt (sdbt dbname)
  (clear sdbt)
  (let* ((sql-fn (case (sdbt-name sdbt)
		   ('pred :retrieve_semi_pred)
		   ('frame :retrieve_semi_frame)
		   ('var :retrieve_semi_var)
		   ('extra :retrieve_semi_extra)))
	 (records
	  (lkb::sql-fn-get-raw-records dbname
				      sql-fn)))
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
	    (make-pathname :directory (pathname-directory (lkb::lkb-tmp-dir)))))

(defun dump-*semi*-to-psql nil
  (dump-semi-to-psql *semi*))

(defmethod dump-semi-to-psql ((semi semi) &key (lexicon lkb::*psql-lexicon*))
  (populate-semi semi)
  (print-semi-db semi)
  (with-slots (lkb::host lkb::port lkb::user lkb::dbname) lexicon
  (let* ((base (format nil "~a/semi.obj" 
	    (make-pathname :directory (pathname-directory (lkb::lkb-tmp-dir))))))
    (lkb::semi-setup-pre lexicon)
    (load-db-table-from-file "semi_pred"
			     (format nil "~a.~a" base "pred")
			     lexicon)
    (load-db-table-from-file "semi_frame"
			     (format nil "~a.~a" base "frame")
			     lexicon)
    (load-db-table-from-file "semi_var"
			     (format nil "~a.~a" base "var")
			     lexicon)
    (load-db-table-from-file "semi_extra"
			     (format nil "~a.~a" base "extra")
			     lexicon)
    (lkb::semi-setup-post lexicon)
    semi
    )))

(defun populate-*semi*-from-psql nil
  (populate-semi-from-psql *semi*))

(defmethod populate-semi-from-psql ((semi semi) &key (psql-lexicon lkb::*psql-lexicon*))
  (close-semi semi)
  (let ((sdb (make-sdb)))
    (load-sdb sdb psql-lexicon)
    (populate-semantic-table sdb)
    (populate-semi semi))
  semi)

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
(defun prepare-cached-lexicon-index (&key (psql-lexicon lkb::*psql-lexicon*))
  (setf *sdb* (make-sdb))
  (load-sdb *sdb* psql-lexicon)
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
      for pred = (let* ((pred-raw (second row))
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
      for pred = (let* ((pred-raw (second row))
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
  (let ((rows (lkb::sql-fn-get-raw-records 
	       db
	       :get_semi_general
	       :args (list table 
			   key
			   (2-db-str val)))))
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

(defun load-db-table-from-file (table-name file-name lexicon)
  (lkb::run-command-stdin lexicon 
		     (format nil "DELETE FROM ~a; ~% COPY ~a FROM stdin;"
			     table-name
			     table-name) 
		     file-name))

  
