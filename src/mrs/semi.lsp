;;; Copyright (c) 2003-2004
;;;   Ben Waldron, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; AAC - April 2009 - I have drastically cut down this file to remove
;;; functions that appear to be unused in the current system.

(defvar *semi* nil)

(defstruct semi
  signature
  (roles (make-hash-table))
  (properties (make-hash-table))
  (predicates (make-hash-table :test #'equal))
  (lexicon (make-hash-table))
  (pred-names (make-hash-table :test #'equal))
  (lex-preds (make-hash-table :test #'equal))
  (pos-preds (make-hash-table :test #'equal)))

(defclass pred-name ()
  ((key :accessor key :initarg :key)
   (name :accessor name :initarg :name)
   (string-p :accessor string-p :initarg :string-p)
   (lex :accessor lex :initarg :lex)
   (pos :accessor pos :initarg :pos)
   (id :accessor id :initarg :id)))
  
(unless *semi* (setf *semi* (make-semi)))

(defmethod print-object ((object semi) stream)
  (let ((properties (hash-table-count (semi-properties object)))
	(roles (hash-table-count (semi-roles object)))
	(predicates (hash-table-count (semi-predicates object)))
	(lexicon (print (hash-table-count (semi-lexicon object)))))
      (format
       stream
       "#[SEM-I: ~a role~p; ~a predicate~p; ~a propert~a; ~a lexical item~p]"
       roles roles 
       predicates predicates 
       properties (if (= properties 1) "y" "ies")
       lexicon lexicon
       )))

;;; format = :db

(defstruct sdbt
  name
  (rows (make-hash-table :test #'equal))
  last
  (id-struc (make-hash-table :test #'equalp)))

(defmethod clear ((sdbt sdbt))
  (with-slots (rows last id-struc) sdbt
    (clrhash rows)
    (setf last nil))
    (clrhash (sdbt-id-struc sdbt))
  sdbt)

(defmethod print-object ((object sdbt) stream)
  (let ((keys-c (hash-table-count (sdbt-rows object)))
	(hashed-c (hash-table-count (sdbt-id-struc object))))
      (format
       stream
       "#[SDBT ~a: ~a key~p ~a hashed]"
       (sdbt-name object) 
       keys-c keys-c
       hashed-c)))

(defun next-counter (sdbt)
  (with-slots (last) sdbt
    (setf last (1+ last))))

(defun sdbt-rows-hash (row sdbt)
  (push row (gethash (car row) sdbt))
  sdbt)

(defun print-sdbt (sdbt &key (stream t))
  (format t "~%~a" (sdbt-rows sdbt))
  (maphash #'(lambda (key rows)
	       (declare (ignore key))
	       (mapc #'(lambda (row)
			 (princ (lkb::str-list-2-str row :sep-c #\tab) stream)
			 (terpri stream))
		     rows))
	   (sdbt-rows sdbt)))

(defstruct sdb
  (tables (list
	   (make-sdbt :name 'pred)
	   (make-sdbt :name 'frame :last 0)
	   (make-sdbt :name 'var :last 0)
	   (make-sdbt :name 'extra :last 0)))
  (leaf-hash (make-hash-table :test #'equal)))

(defmethod clear ((sdb sdb))
  (with-slots (tables leaf-hash) sdb
    (mapc #'clear tables)
    (clrhash leaf-hash)
    sdb))

(defun sdb-table (sdb table-name)
  (or
   (car (member table-name (sdb-tables sdb) :test #'eq :key #'sdbt-name))
   (error "unknown table name: ~a" table-name)))



(defun print-sdb (sdb &key (base lkb::*psorts-temp-file*))
  (setf base (pathname base))
  (unless base
    (error "please set lkb::*psorts-temp-file*"))
  (mapc
   #'(lambda (x)
       (with-open-file 
	   (stream
	    (make-pathname :name (format nil "~a.sem.~(~a~)" (pathname-name base) (sdbt-name x))
			   :host (pathname-host base)
			   :device (pathname-device base)
			   :directory (pathname-directory base))
	    :direction :output 
	    :if-exists :supersede)
	 (format t "~%writing table ~a..." (sdbt-name x))
	 (print-sdbt x :stream stream)))
   (sdb-tables sdb)))

(defvar *sdb* nil)


;;; called by UPDATE-PSQL-SEMI in semi-psql.lsp
(defun print-semi-db-partial (lexids &key (semantic-table *semantic-table*))
  (loop
      with sdb = (setf *sdb* (make-sdb))
      initially (format t "~%preparing semi-db tables...~%")
      for lexid in lexids
      for record = (gethash lexid semantic-table)
      do 
	(if record
	    (process-record-db record sdb)
	  (format t "~%Warning: ~a has no semantic record" lexid))
      finally
	(setf *sdb* sdb)
	(print-sdb sdb)))

;;; called by LKB::DUMP-GENERATOR-INDICES-TO-PSQL
(defun populate-sdb (&key (semantic-table *semantic-table*))
  (loop
      with sdb = (setf *sdb* (make-sdb))
      initially (format t "~%preparing semi-db tables...~%")
      for record being each hash-value in semantic-table
      do (process-record-db record sdb)
      finally
	(setf *sdb* sdb)
	(return sdb)))

;;; see above
(defun process-record-db (record sdb)
  (let* ((pred-t (sdb-table sdb 'pred))
	 (frame-t (sdb-table sdb 'frame))
	 (var-t (sdb-table sdb 'var))
	 (extra-t (sdb-table sdb 'extra))
	 
	 (pred-r (sdbt-rows pred-t))
	 (frame-r (sdbt-rows frame-t))
	 (var-r (sdbt-rows var-t))
	 (extra-r (sdbt-rows extra-t))
	 
	 ;(pred-h (sdbt-id-struc pred-t))
	 (frame-h (sdbt-id-struc frame-t))
	 (var-h (sdbt-id-struc var-t))
	 (extra-h (sdbt-id-struc extra-t))
	 )
    (loop
	with lex-id = (semantics-record-id record)
	with rels = (semantics-record-relations record)
	for rel in rels
	for pred = (rel-base-pred rel)
	for frame = (rel-base-flist rel)
	for frame-hashed = (gethash frame frame-h)
	for frame-id = (or frame-hashed (next-counter frame-t))
	for pred-row = (list (lkb::2-str lex-id)
			     (lkb::2-str pred)
			     (format nil "~a" frame-id)
			     (lkb::2-str pred)
			     (if (stringp pred) "t" "f"))
	do
	  (sdbt-rows-hash pred-row pred-r)
	unless frame-hashed
	do
	  (setf (gethash frame frame-h) frame-id)
	  (loop
	      for role in frame
	      for slot = (fvpair-feature role)
	      for slot-val = (fvpair-value role)
	      with frame-row
	      do
		(typecase slot-val
		  (string
		   (setf frame-row (list (format nil "~a" frame-id) (lkb::2-str slot) (lkb::2-str slot-val) nil nil nil))
		   (sdbt-rows-hash frame-row frame-r))
		  (symbol
		   (setf frame-row (list (format nil "~a" frame-id) (lkb::2-str slot) nil (lkb::2-str slot-val) nil nil))
		   (sdbt-rows-hash frame-row frame-r))
		  (var-base
		   (let* ((var slot-val)
			  (var-hashed (gethash var var-h))
			  (var-id (or var-hashed (next-counter var-t)))
			  (type (var-base-type var)))
		     (setf frame-row (list (format nil "~a" frame-id) (lkb::2-str slot) nil nil (format nil "~a" var-id) (lkb::2-str type)))
		     (sdbt-rows-hash  frame-row frame-r)
		     (unless var-hashed
		       (setf (gethash var var-h) var-id)
		       (loop
			   with extra-list = (var-base-extra var)
			   for extra in extra-list
			   for extra-hashed = (gethash extra extra-h)
			   for extra-id = (or extra-hashed (next-counter extra-t))
			   for var-row = (list (lkb::num-2-str var-id) (lkb::num-2-str extra-id))
			   for extra-feature = (extrapair-feature extra)
			   for extra-value = (extrapair-value extra)
			   for extra-row = (list (lkb::num-2-str extra-id) (lkb::2-str extra-feature) (lkb::2-str extra-value))
			   do
			     (sdbt-rows-hash  var-row var-r)
			   unless extra-hashed
			   do
			     (setf (gethash extra extra-h) extra-id)
			     (sdbt-rows-hash  extra-row extra-r)
			     ))))))))
    sdb)
		   
;;; aux fns

(defun lookup-preds (lex &key pos id (semi *semi*))
  (mapcar #'key
	  (loop
	      for pred-name in (gethash lex (semi-lex-preds semi))
	      if 
		(and
		 (or (null pos)
		     (string= (pos pred-name)
			      pos))
		 (or (null id)
		     (string= (id pred-name)
			      id)))
	      collect pred-name)))

(defun info-from-semi-by-pred (pred &key (semi *semi*))
   "depreciated: see more-info-from-semi-by-pred"
  (let* ((pred-name (gethash pred (semi-pred-names semi)))
	 (predicate (gethash pred (semi-predicates semi)))
	 
	 (args (mapcar
		#'rel-base-flist
		predicate)))
    (cons (string-p pred-name)
	  args)))

(defun get-info-from-semi (lex &key pos id (semi *semi*))
  "depreciated: see more-info-from-semi"
  (mapcar #'info-from-semi-by-pred (lookup-preds lex :pos pos :id id :semi semi)))

(defun more-info-from-semi-by-pred (pred &key (semi *semi*))
  "return stringness and arg-structure"
  (let* ((pred-name (gethash pred (semi-pred-names semi)))
	 (predicate (gethash pred (semi-predicates semi)))
	 
	 (args (mapcar
		#'rel-base-flist
		predicate)))
    (list
     (cons :stringness (string-p pred-name))
     (cons :args args))))

(defun get-more-info-from-semi (lex &key pos id (semi *semi*))
  "for each pred matching lex pos? id? return info on pred"
  (mapcar #'(lambda (x) 
	      (list
	       (cons :pred x)
	       (cons :info (more-info-from-semi-by-pred x))))
	  (lookup-preds lex :pos pos :id id :semi semi)))

;; work around compiler warnings in SMAF code...
(defun get-semi nil
  *semi*)

(defun get-meta-semi nil
  *meta-semi*)




