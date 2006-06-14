;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod close-lex ((lex psql-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots (lexdb-version semi dbname host user connection) lex
    (setf lexdb-version nil)
    (if (next-method-p) (call-next-method))))

(defmethod open-lex ((lex psql-lex-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (dbname host user connection) lex
    (close-lex lex)    
    (force-output)
    (setf (name lex) name)
    (or (open-lex-aux lex)
	(format t "~&unable to open connection to lexical database ~a(~a)@~a:~a (~a)" 
		dbname user host (true-port lex)
		(error-msg connection)))))

;;
;;
(defmethod check-lexdb-version ((lex psql-lex-database))
  (with-slots (lexdb-version) 
      lex
    (when (and
	   (string= *lexdb-major-version* "4.9")
	   (string= (compat-version lexdb-version) "4.8"))
      (return-from check-lexdb-version)) ;; we ensure compatibility via "lex_cache" hack
    (cond
     ((not (stringp lexdb-version))
      (error "Unable to determine LexDB version"))
     ((string> (compat-version lexdb-version)
	       *lexdb-major-version*)
      (error *lexdb-message-old-lkb* lexdb-version *lexdb-major-version*))
     ((string< (compat-version lexdb-version)
	       *lexdb-major-version*)
      (error *lexdb-message-old-lexdb* lexdb-version *lexdb-major-version*)))))

(defmethod check-psql-server-version ((lex psql-lex-database))
  (let* ((supported
	  (mapcar #'car
		  (get-raw-records lex "SELECT val FROM public.meta WHERE var='supported-psql-server'")))
	 (actual-full (caar (get-raw-records lex "SELECT split_part((SELECT version()),' ',2)")))
	 (actual-full-l (string-2-str-list actual-full :sep #\.))
	 (actual (concatenate 'string 
		   (nth 0 actual-full-l)
		   "."
		   (nth 1 actual-full-l))))
    (unless (member actual supported :test #'string=)
      (format t "~&(LexDB) WARNING: Unsupported PSQL server version ~a.~% Supported versions are: ~a)" actual supported))))

;;

(defmethod initialize-lex ((lex psql-lex-database))
  (when (open-lex lex)
    (with-slots (dbname user host) lex
        (format t "~&(LexDB) connected to LexDB ~a@~a:~a as database user ~a" 
		dbname host (true-port lex) user))
    (update-lex lex)))
  
(defmethod connect ((lex psql-lex-database)) 
  (if (next-method-p) (call-next-method))
  (when (connection-ok lex)
    (setf (lexdb-version lex) 
      (get-db-version lex))
    t))	

;;;
;;;
;;;

(defmethod get-db-version ((lex psql-lex-database))
  (caar 
   (get-raw-records lex 
		    "SELECT val FROM public.meta WHERE var='lexdb-version' LIMIT 1")))
    
;;
;;

(defmethod grammar-fields ((lex psql-lex-database))
  (unless (dfn lex)
    (complain-no-dfn lex)
    (error "operation aborted"))
  (let ((g-fields
	 (remove-duplicates 
	  (mapcar #'second (dfn lex)))))
    (when (member :|_tdl| (fields lex))
      (pushnew :|_tdl| g-fields))
    g-fields))

;; WORD -> IDS

(defmethod lookup-word ((lex psql-lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*))
  (with-slots (lexical-entries) lex
  (let ((hashed (gethash orth lexical-entries)))
    (cond 
     (hashed
      (if (eq hashed :EMPTY)
	  (setf hashed nil)
	hashed))
     (t 
      (let ((value (lookup-word-no-cache lex orth)))
	;;if caching, add entry to cache...
	(when cache
	  (setf (gethash orth lexical-entries) 
	    (if value value :EMPTY)))
	value))))))

(defmethod lookup-word-aux2 ((lex psql-lex-database) table)
  (with-slots (psorts record-cache dfn) lex
    (let ((name-field (second (assoc :ID dfn))))
      (loop
	  with cols = (cols table)
	  for rec in (recs table)
	  for id = (str-2-symb (get-val name-field rec cols))
	  do
	    ;; cache values
	    (unless (gethash id record-cache)
	      (setf (gethash id record-cache) 
		rec))
	    (unless (gethash id psorts)
	      (setf (gethash id psorts) 
		(make-psort-struct lex rec cols)))
	  collect id))))

;; ALL WORDS INDEXED

(defmethod lex-words ((lex psql-lex-database))
  (mapcar #'(lambda (x) (string-upcase (car x)))
	  (get-raw-records lex "select distinct key from lex_key")))

;; ORTH KEYS

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

(defmethod get-unnormalized-lex-keys ((lex psql-lex-database))
  (recs
   (get-records lex "SELECT name,userid,modstamp,key FROM lex_key")))

(defmethod regenerate-orthkeys ((lex psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))
  (run-command lex "DELETE FROM lex_key")
  (generate-missing-orthkeys lex)
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)" :ignore-errors t)))

(defmethod generate-missing-orthkeys ((lex psql-lex-database))
  (put-normalized-lex-keys lex
			   (normalize-orthkeys (create-unnormalized-missing-lex-keys3 lex))))

;;; ALL IDS

(defmethod collect-psort-ids ((lex psql-lex-database) &key (cache t) (recurse t))
  (declare (ignore recurse))
  (with-slots (cache-lex-list) 
      lex
    (let ((lex-list cache-lex-list))
      (when (null cache-lex-list)
	(setf lex-list 
	  (collect-psort-ids-aux lex))
	(if (null lex-list)
	    (setf lex-list :empty))
	(if cache 
	    (setf cache-lex-list lex-list)))
      (case lex-list
	(:empty nil)
	(otherwise lex-list)))))

;; ALL AVAILABLE RECORDS

(defmethod retrieve-all-records ((lex psql-lex-database) &optional (reqd-fields '("*")))
  (cond
   ((connection lex)
    (let* ((reqd-fields (fields-str lex reqd-fields))
	  (recs (get-records lex (format nil "SELECT ~a FROM lex" reqd-fields))))
      recs))
   (t
    (format t "~&(LexDB) WARNING: no connection to psql-lex-database"))))

;; RECORD BY ID

(defmethod retrieve-raw-record ((lex psql-lex-database) id &key (cache t) (reqd-fields '("*")))
  (with-slots (record-cache) lex
    (let ((hashed (gethash id record-cache)))
      (cond (hashed
	     (unless (eq hashed :EMPTY)
	       hashed))
	    (t
	     (let* ((record (retrieve-raw-record-no-cache lex id reqd-fields)))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-entry ((lex psql-lex-database) name &key (reqd-fields '("*")))
  (get-records lex
	       (format nil
		       "SELECT ~a FROM lex WHERE name LIKE ~a"
		       (fields-str lex reqd-fields)
		       (psql-quote-literal name))))

(defmethod retrieve-raw-record-no-cache ((lex psql-lex-database) id &optional (reqd-fields '("*")))
  (cond 
   ((connection lex)
    (let* ((id-str (symb-2-str id))
	   (column-records 
	    (recs (retrieve-entry lex (sql-like-text id-str) :reqd-fields reqd-fields))))
      (if (> (length column-records) 1)
	  (error (format nil " too many records returned"))
	(first column-records))))
   (t
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database"))))

;; ID -> ENTRY

(defmethod read-psort ((lex psql-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (with-slots (psorts) lex
    (let ((hashed (and (not new-instance) (gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed :EMPTY) hashed))
	    (t
	     (let ((entry (read-psort-aux lex id :cache cache)))
	       (when cache
		 (setf (gethash id psorts)
		   (or entry :EMPTY)))
	       entry))))))

(defmethod read-psort-aux ((lex psql-lex-database) id &key (cache t))
  (with-slots (psorts) lex
    (let* ((cols (grammar-fields lex))
	   (raw-record 
	    (retrieve-raw-record 
	     lex id 
	     :reqd-fields cols
	     :cache cache)))
      (if raw-record 
	  (make-psort-struct lex raw-record cols)))))

(defmethod make-psort-struct ((lex psql-lex-database) raw-record cols)
  (apply #'make-lex-entry 
	 (make-strucargs2 raw-record cols 
			  :dfn (dfn lex)
			  :fields (fields lex))
			  ))

;; RECORD -> TDL

(defmethod record-to-tdl ((lex psql-lex-database) record)
  (let ((cols (mapcar #'car record))
	(rec (mapcar #'cdr record)))
    (raw-record-to-tdl lex rec cols)))
  
(defmethod raw-record-to-tdl ((lex psql-lex-database) rec cols)
  (to-tdl (make-psort-struct lex rec cols)))

;;
;;
;;

;; lex is open
(defmethod load-lex-from-files ((lex psql-lex-database) file-names syntax)
  (setf *ordered-lex-list* nil) ;;fix_me
  (cond
   ((check-load-names file-names 'lexical)
    (let ((*lexicon-in* lex)) ;; *lexicon-in* is needed deep inside read-...-file-aux
      (dolist (file-name file-names)
	(ecase syntax
	  (:tdl (read-tdl-lex-file-aux-internal file-name))
	  (:path (read-lex-file-aux-internal file-name)))))
    t)
   (t
    (cerror "Continue" "Lexicon file not found")
    nil)))

;; CACHING

(defmethod cache-all-lex-records ((lex psql-lex-database))
  (let* ((table (retrieve-all-records lex 
				      (grammar-fields lex)))
	 (recs (recs table))
	 (cols (cols table)))
    (with-slots (record-cache) lex
      (clrhash record-cache)
      (mapc
       #'(lambda (rec)  
	   (setf (gethash (record-id rec cols lex) record-cache) 
	     rec))
       recs))))

(defmethod cache-all-lex-records-orth ((lex psql-lex-database))
  (let ((table (retrieve-all-records lex (grammar-fields lex))))
    (with-slots (recs cols) table
      (with-slots (record-cache lexical-entries) lex
	;; clear cache
	(clrhash lexical-entries)
	(clrhash record-cache)
	;; for each record...
	(mapc
	 #'(lambda (rec)
	     (let* ((id (record-id rec cols lex))
		    (orth (record-orth rec cols lex)))
	       ;; update cache for each component word...
	       (mapc
		#'(lambda (y)
		    (setf (gethash y lexical-entries) 
		      (cons id (gethash y lexical-entries))))
		(split-into-words orth))
	       ;; update record cache
	       (setf (gethash id record-cache)
		 rec)))
	 recs)))))

;;
;;

(defmethod make-field-map-slot ((lex psql-lex-database))
  "stores the mapping of fields to lex-entry structure slots"
  (with-slots (dfn fields) lex
    (setf fields (get-fields lex))
    (setf dfn
      (sort
       (mapcar 
	#'(lambda (x) 
	    (let* ((slot (str-2-keyword (string-upcase (first x))))
		   (field (str-2-keyword (second x)))
		   (path (third x))
		   (type2 (2-symb-or-list (fourth x)))
		   (type (if (listp type2) type2 (list type2))))
	      ;; correct any obsolete types
	      (setf (car type)
		(or (cdr (assoc (car type) *lexdb-fmtype-alt*))
		    (car type)))
	      (list slot field path type)))
	(get-raw-records lex (format nil "SELECT slot,field,path,type FROM dfn WHERE mode='~a' OR mode = ''" (fields-tb lex))))
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :UNIFS))))
    (if (null dfn)
	(complain-no-dfn lex))
    dfn))

(defmethod complain-no-dfn ((lex psql-lex-database))
  (error "~&(LexDB) no dfn entries found in ~a !!!" (dbname lex)))

(defmethod get-internal-table-dfn ((lex psql-lex-database))
  (get-field-info lex "public" "rev"))  

(defmethod get-field-size-map ((lex psql-lex-database))
  (let* ((table (get-internal-table-dfn lex))
	(recs (recs table))
	(cols (cols table)))
    (mapcar 
     #'(lambda (x) (field-size-elt x cols)) 
     recs)))

;; used by Emacs interface

(defmethod lookup ((lex psql-lex-database) field-kw val-str &key (ret-flds "*") (from "lex"))
  (cond
   (val-str
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM ~a WHERE ~a ILIKE ~a"
			     ret-flds from
			     (quote-ident lex field-kw)
			      (psql-quote-literal val-str))))
   (t
    (get-raw-records lex 
		     (format nil "SELECT ~a FROM ~a WHERE ~a IS NULL"
			     ret-flds from
			     (quote-ident lex field-kw))))))

(defmethod complete ((lex psql-lex-database) field-kw val-str)
  (let ((qi-field (quote-ident lex (symb-2-str field-kw)))
	(ql-val (psql-quote-literal (format nil "~a%" val-str))))
    (mapcar #'car
	    (get-raw-records lex
			     (format nil
				     "SELECT DISTINCT ~a AS field FROM lex WHERE ~a ILIKE ~a"
				     qi-field qi-field ql-val)))))

(defmethod current-timestamp ((lex psql-lex-database))
  (caar (get-raw-records lex "SELECT current_timestamp")))

;;;
;;; postgres interface
;;;

	     
(defmethod set-lex-entry-from-record ((lex psql-lex-database) fv-pairs)
  (set-lex-entry lex (make-instance 'psql-lex-entry :fv-pairs fv-pairs)))

;;; insert lex entry into db
(defmethod set-lex-entry ((lex psql-lex-database) (psql-le psql-lex-entry) &key (gen-key t))
  (set-val psql-le :|orthkey| (lexicon-le-orthkey lex psql-le))
  (set-val psql-le :|modstamp| "NOW")
  (set-val psql-le :|userid| (user lex))
  (set-lex-entry-aux lex psql-le :gen-key gen-key))
  
(defmethod set-lex-entry-aux ((lex psql-lex-database) (psql-le psql-lex-entry) &key (gen-key t) )
  (let* ((symb-list (copy-list (fields lex)))
	 (symb-list (remove-duplicates symb-list))
	 (symb-list (remove-if 
		     #'(lambda (x) (or (null x) 
				       (and (stringp x)
					    (string= x ""))))
		     symb-list
		     :key #'(lambda (x) (retr-val psql-le x))))
	 (name (retr-val psql-le :|name|))
	 (lexid (str-2-symb name)))
    (unless (string= name (symb-2-str lexid))
      (format t "(LexDB) WARNING: lex id ~a should be written ~a" 
	      name (symb-2-str (str-2-symb name)))
      (lkb-beep)
      (setf name (symb-2-str (str-2-symb name))))
    (update-entry lex symb-list psql-le)
  (let ((*empty-cache-clears-generator-lexicon* nil))
    (empty-cache lex))
    (when gen-key 
      (generate-missing-orthkeys lex))
    
    (mrs::delete-lexid-from-generator-indices lexid)
    (if (read-psort lex lexid) 
	(if (check-lex-entry lexid lex)
	    (progn
	      (add-lexid-to-generator-indices lex lexid)
	      t)
	  (error "Invalid lexical entry ~a -- see Lisp buffer output" lexid))
      t)))

;;
;; DATABASE SEMI
;;

(defmethod create-tables-semi ((lex psql-lex-database))
  (run-command lex "
CREATE TABLE semi_pred (
lex_id text NOT NULL,
pred_id text NOT NULL,
frame_id int NOT NULL,
pred_txt text NOT NULL,
string_p boolean NOT NULL
);

CREATE TABLE semi_frame (
frame_id int NOT NULL,
slot text NOT NULL,
str text,
symb text,
var_id int,
type text
);

CREATE TABLE semi_var (
var_id int NOT NULL,
extra_id int NOT NULL
);

CREATE TABLE semi_extra (
extra_id int NOT NULL,
feat text NOT NULL,
val text NOT NULL
);

CREATE TABLE semi_mod (
name text,
userid text,
modstamp TIMESTAMP WITH TIME ZONE,
modstamp0 TIMESTAMP WITH TIME ZONE
);
	
CREATE OR REPLACE VIEW semi_obj AS
SELECT lex_id,pred_id, slot, str, type, feat, val FROM
semi_pred NATURAL JOIN
semi_frame NATURAL LEFT JOIN
semi_var NATURAL LEFT JOIN
semi_extra;
"))
  
(defmethod semi-create-indices ((lex psql-lex-database))
  (run-command lex "
CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
CREATE UNIQUE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
"))
  
(defmethod semi-drop-indices ((lex psql-lex-database))
  (run-command-coe lex "DROP INDEX semi_pred_lex_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_pred_pred_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_frame_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_frame_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_var_var_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_extra_extra_id CASCADE")
  (run-command-coe lex "DROP INDEX semi_mod_name_userid_modstamp CASCADE"))

(defmethod index-new-lex-entries ((lex psql-lex-database))
  (let ((semi-out-of-date (semi-out-of-date lex)))
    (format t "~&(LexDB) indexing ~a entries" (length semi-out-of-date))
    (when semi-out-of-date
      (loop for x in semi-out-of-date
	  do (update-lisp-semi-entry lex x))
      #+:null ;; fix_me (avoid duplicate sdb rows)
      (mrs::update-psql-semi semi-out-of-date 
			     :lex lex
			     :semantic-table mrs::*semantic-table*))))
  
(defmethod update-lisp-semi-entry ((lex psql-lex-database) lexid)
  (mrs::delete-lexid-from-generator-indices lexid) ;!!
  (add-lexid-to-generator-indices lex lexid))

(defmethod add-lexid-to-generator-indices ((lex psql-lex-database) lexid)
  (let* ((entry (read-psort lex lexid :cache nil))
	 (new-fs (and
		  entry
		  (expand-psort-entry entry)
		  (lex-entry-full-fs entry))))
    (cond
     ((null entry)
      (format t "~&WARNING: No lexical entry named ~a" lexid))
     ((and new-fs 
	   (not (eq new-fs :fail)))
      (mrs::extract-lexical-relations entry)) ; <-- efficiency problem originates in here
     (t
      (format t "~&WARNING: No feature structure for ~a~%" lexid))))
    (forget-psort lex lexid))

(defmethod semi-up-to-date-p ((lex psql-lex-database))
 (not (semi-out-of-date lex)))

(defmethod semi-out-of-date ((lex psql-lex-database))
   (mapcar #'(lambda (x) (str-2-symb (first x))) 
	   (get-raw-records
	    lex "SELECT name FROM lex_cache LEFT JOIN semi_mod USING (name,userid,modstamp) WHERE lex_cache.modstamp > COALESCE(semi_mod.modstamp0,'-infinity')")))

;; LEX KEY TABLE

(defmethod create-table-lex-key ((lex psql-lex-database))
  (run-command lex "CREATE TABLE lex_key (
		name TEXT NOT NULL,
		userid TEXT DEFAULT user NOT NULL,
		modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
		key text NOT NULL
		)"))

(defmethod index-lex-key ((lex psql-lex-database))
  (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)"))

(defmethod deindex-lex-key ((lex psql-lex-database))
  (run-command lex "DROP INDEX lex_key_key"))  

;;

;;;
;;;
;;;

(defmethod get-fields ((lex psql-lex-database))
  (mapcar 
   #'(lambda (x) (intern x :keyword))
   (list-fld lex)))

(defmethod vacuum ((lex psql-lex-database))
  (let (time)
    (format t "~&(LexDB) performing vacuum/analyze on database (as user ~a)..." (user lex))
    (force-output)
    (setf time (get-internal-real-time))
    (with-lexdb-client-min-messages (lex "error")
      (run-command lex "vacuum full analyze"))
    (format t "~&(LexDB) vacuum/analyze complete [~F sec]" 
	    (/ (- (get-internal-real-time) time) internal-time-units-per-second))))

(defmethod db-owner ((lex psql-lex-database))
  (let* ((uid (sql-get-val lex "SELECT datdba FROM pg_catalog.pg_database WHERE datname=current_database()"))
	 (uname (sql-get-val lex 
			     (concatenate 'string
			       "SELECT usename FROM pg_catalog.pg_user WHERE usesysid=" (psql-quote-literal uid)))))
    uname))

(defmethod get-field-vals ((x lex-entry) (lex psql-lex-database))
  (with-slots (dfn) lex
    (let* (;; copy slots as we will destructively delete during processing
	   (s (copy-slots x dfn))
	   ;; perhaps should warn about duplicates?
	   (extraction-fields 
	    (remove :|_tdl|
		    (remove-duplicates
		     (cons :|name| (grammar-fields lex)))))
	   ;; extract field values from tdl structure 
	   ;; and remove unifs as they are found
	   (extraction-field-vals (mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x dfn)))
			 extraction-fields))
	   ;; convert any remaining unifs into raw tdl fragment
	   (skip (unifs-to-tdl-body (cdr (assoc :UNIFS s))))
	   (skip (if (string= skip "") nil skip))
	   ;; necessary fields
	   (hard-coded-field-vals (list
				   (cons :|userid| *lexdb-dump-user*)
				   (cons :|modstamp| *lexdb-dump-timestamp*)
				   (cons :|dead| "f")
				   (cons :|_tdl| skip)))
	   ;; additional (useful) fields
	   ;; if not all fields occur in LexDB they will be silently ignored
	   (other-field-vals (list
			      (cons :|lang| *lexdb-dump-lang*)
			      (cons :|country| *lexdb-dump-country*)
			      (cons :|confidence| 1)
			      (cons :|source| *lexdb-dump-source*)))
	   ;; combine all field values
	   (field-vals (append extraction-field-vals
			       hard-coded-field-vals
			       other-field-vals)))
      ;; construct ordered a-list of field values
      ;; fields not in LexDB silently ignored
      field-vals)))
  

;; keys must be updated after set of calls to this fn
(defmethod to-db ((x lex-entry) (lex psql-lex-database))
  "insert lex-entry into lex db (user scratch space)"
  (let* (;;ordered a-list of field values
	 (field-vals (get-field-vals x lex))
	 (skip (cdr (assoc :|_tdl| field-vals)))
	 (name (cdr (assoc :|name| field-vals)))
	 (ordered-field-vals (ordered-symb-val-list (fields lex) field-vals))
	 (psql-le (make-instance 'psql-lex-entry :fv-pairs ordered-field-vals)))	 
    (cond
     ;; no components of lex entry skipped
     ((null skip)
      (set-lex-entry lex psql-le :gen-key nil)
      (empty-cache lex))
     ;; component(s) skipped, but :skip field available in db
     ((member :|_tdl| (fields lex))
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
      (set-lex-entry lex psql-le :gen-key nil)
      (empty-cache lex))
     ;; component(s) skipped and no :skip field in db
     (t
      (format t "~&~%(LexDB) Lex entry ~a skipped due to unhandled TDL fragment: ~%~t~a~%" name skip)
      nil))))

;; NOTE: not suited to batch import
;; import lex to LexDB
(defmethod export-to-db ((lex lex-database) (lexdb psql-lex-database))
  (mapc
   #'(lambda (x) 
       (to-db (read-psort lex x :recurse nil :new-instance t) 
	      lexdb))
   (collect-psort-ids lex :recurse nil))
  (generate-missing-orthkeys lexdb)
  (update-lex-aux lexdb))

(defmethod record-id (raw-record cols (lex psql-lex-database))
  (str-2-symb (get-val :|name| raw-record cols)))  

(defmethod record-orth (raw-record cols (lex psql-lex-database))
  (get-val (second (assoc :ORTH (dfn lex))) raw-record cols))

(defmethod table-size ((lex psql-lex-database) table)
  (let ((table-str (format nil "~a" table)))
    (cond
     ((string= (string-downcase table-str) "rev-all")
      (count-rev-all lex))
     ((string= (string-downcase table-str) "lex")
      (count-lex lex))
     (t
      (sql-get-num lex 
		   (format nil "SELECT count(*) FROM ~a" table))))))

 (defmethod orth-field ((lex psql-lex-database))
   (let ((orth-raw-mapping (assoc :ORTH (dfn lex))))
     (quote-ident lex (second orth-raw-mapping))))
