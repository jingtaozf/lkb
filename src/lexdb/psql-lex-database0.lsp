;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmacro with-dropped-lex-key-indices ((lex) &body body)
  `(progn
     (drop-lex-key-indices ,lex)
     ,@body
     (create-lex-key-indices ,lex)))

;(defmethod close-lex ((lex psql-lex-database) &key in-isolation delete)
;  (declare (ignore in-isolation delete))
;  (with-slots (semi dbname host user connection) lex
;    (if (next-method-p) (call-next-method))))


;; close lex if nec
;; set name
;; connect (why is this not in open for psql-database?
(defmethod open-lex ((lex psql-lex-database) &key name parameters)
  (declare (ignore parameters)) 
  (with-slots (dbname host user connection) lex
    (close-lex lex)    ;; is this necessary?
    (force-output) ;; move to close-lex?
    (setf (name lex) name) ;; set lex name to lexdb name
    (cond
     ((connect lex)
      (setf (name lex) (format nil "LexDB:~a" dbname)))
     (t
      (format t "~&unable to open connection to lexical database ~a(~a)@~a:~a (~a)" 
	      dbname user host (true-port lex)
	      (error-msg connection))))))

;; check that server version + lexdb version are acceptable
;; unless user is lexdb owner... (should use sepratae mechanism!)
;; let user know we have connection
;; check everything is up to date
(defmethod open-lex :after ((lex mu-psql-lex-database) &key name parameters)
  (declare (ignore name parameters))
  (with-slots (dbname user host) lex
    (when (connection lex)
      (check-psql-server-version lex)
      (check-lexdb-version lex)
      ;; if not db owner, check user schema is up-to-date
      (unless
	  (sql-get-bool lex "SELECT user_is_db_owner_p()")
	(format t "~&(LexDB) connection opened to ~a LexDB ~a@~a:~a (database user ~a)" 
		(string-downcase (string (psql-lex-database-type lex)))
		dbname host (true-port lex) user)
	(get-fields lex) ;; initialize on request?
	(get-dfn lex) ;; initialize on request?
	(new-user-schema-if-nec lex) ;; create private space if nec
	(build-lex-if-nec lex) ;; update lexicon if necessary
	(let ((size (count-lex lex)) ;; size of lexicon -> property?
	      (rev-size (count-rev-all lex))) ;; size of repos -> property?
	  
	  (format t "~&(LexDB) total revision entries available: ~a" rev-size)
	  ;; warn user if repos empty
	  (when (= 0 rev-size)
	    (format t " !!! PLEASE LOAD REV ENTRIES !!!")
	    (lkb-beep))
	  
	  (format t "~&(LexDB) active entries in lexicon: ~a" size)
	  ;; warn user if lexicon empty
	  (when (= 0 size)
	    (format t " !!! PLEASE SET FILTER !!!")
	    (lkb-beep))
	  (format t "~&(LexDB) filter: ~a " (filter lex))
	  ;; warn user if filter unset
	  (when (string= "NULL" (string-upcase (filter lex)))
	    (format t "!!! PLEASE SET FILTER !!!")
	    (lkb-beep))))
      (empty-cache lex)))) ;; build-lex-if-nec / close-lex should take care of this

;; let user knwo we have connection
(defmethod open-lex :after ((lex su-psql-lex-database) &key name parameters)
  (declare (ignore name parameters))
  (when (connection lex)
    (with-slots (dbname user host) lex
      (format t "~&(LexDB) connected to ~a LexDB ~a@~a:~a (database user ~a)" 
	      (string-downcase (string (psql-lex-database-type lex)))
	      dbname host (true-port lex) user))
    (get-fields lex) ;; -> cached property?
    (get-dfn lex) ;; -> cached property?
    (let ((size (count-lex lex))) ;; -> property?
      (format t "~&(LexDB) total 'lex' entries available: ~a" size)
      ;; warn user if lexicon empty
      (when (= 0 size)
	(format t " !!! PLEASE LOAD LEX ENTRIES !!!")
	(lkb-beep)))
    (empty-cache lex))) ;; close-lex should take care of this

;;
;;

;; field names in dfn table
(defmethod grammar-fields ((lex psql-lex-database)) ;; -> cached property!
  (unless (dfn lex)
    (complain-no-dfn lex)
    (error "operation aborted"))
  (let ((g-fields
	 (remove-duplicates 
	  (mapcar #'second (dfn lex)))))
    (when (member :|_tdl| (fields lex)) ;; should go in DFN if used?
      (pushnew :|_tdl| g-fields))
    g-fields))

;; WORD -> IDS

;; cache in lexical-entries
(defmethod lookup-word ((lex psql-lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*)) ;; remove/rename global flag?
  ;; CASE no cache
  (unless cache
    (return-from lookup-word 
      (lookup-word-no-cache lex orth)))
  ;; CASE cache
  (with-slots (lexical-entries) lex ;; lexical entries stores cached entries
    (let* ((cached-raw (gethash orth lexical-entries))
	   (cached (if (eq cached-raw :EMPTY) nil cached-raw))
	   (value (if (not cached) (lookup-word-no-cache lex orth))))
      (cond
       (cached cached)
       (t
	;; update cache, and return value
	(setf (gethash orth lexical-entries) 
	  (or value :EMPTY))
	value)))))

;; orthkey must be mapped to normalized form before entering PSQL universe
(defmethod lookup-word-no-cache ((lex psql-lex-database) orth)
  ;(if (connection lex)
  (let* ((key (normalize-orthkey orth))
	 (fields (fields-str lex (grammar-fields lex))) ;;-> cached property?
	 ;; retrieve all grammar fields formmatching entries!
	 (table (get-records lex (lookup-word-no-cache-SQL lex key fields)))
	 ;; return simply the ids (but cache the rest)
	 (ids (cache-records-and-return-ids lex table)))
    ids))
;)

;; ALWAYS (!) cache retrieved records (and psorts!)
;; ??? allow caching to be disabled???
(defmethod cache-records-and-return-ids ((lex psql-lex-database) table)
  (with-slots (psorts record-cache dfn) lex
    (let ((name-field (second (assoc :ID dfn)))) ;; -> cached property?
      (loop
	  with cols = (cols table)
	  for rec in (recs table)
	  for id = (str-2-symb (get-val name-field rec cols)) ;; -> get-val-symb ?
	  do
	    ;; cache record
	    (unless (gethash id record-cache)
	      (setf (gethash id record-cache) 
		rec))
	    ;; cache psort (!)
	    (unless (gethash id psorts) ;; caching of psorts is not necssary!
	      (setf (gethash id psorts) 
		(make-psort-struct lex rec cols)))
	    
	  collect id)))) ;; return ids

;(defmethod lookup-word ((lex psql-lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*)) ;; remove/rename global flag?
;  (with-slots (lexical-entries) lex
;  (let ((hashed (gethash orth lexical-entries))) ;; lexical entries stores cached entries
;    (cond 
;     (hashed
;      (if (eq hashed :EMPTY) ;; :EMPTY means not in lexicon
;	  (setf hashed nil)
;	hashed))
;     (t 
;      (let ((value (lookup-word-no-cache lex orth)))
;	;;if caching, add entry to cache...
;	(when cache
;	  (setf (gethash orth lexical-entries) 
;	    (if value value :EMPTY)))
;	value))))))

;; ALL WORDS INDEXED

;; returns all KEYS (= indexed words) [UPCASED!]
(defmethod lex-words ((lex psql-lex-database))
  (loop 
      for raw-record in (get-raw-records lex "select distinct key from lex_key")
      for key = (car raw-record)
      collect (string-upcase key)))

;(defmethod lex-words ((lex psql-lex-database))
;  (mapcar #'(lambda (x) (string-upcase (car x)))
;	  (get-raw-records lex "select distinct key from lex_key")))


;; ORTH KEYS

;; write normalized keys to database
(defmethod put-normalized-lex-keys ((lex psql-lex-database) recs)
  ;; nothing to do if no recs
  (when recs
    (with-slots (connection) lex
      ;; ensure indices are dropped in table
      (with-dropped-lex-key-indices (lex)
	;; prepare to listen on STDIN
	(pq:exec connection "COPY lex_key FROM stdin")
	(loop
	    for rec in recs
		       ;; send each indivdual record
	    do 
	      (with-lexdb-locale 
		  (pq:putline connection (to-psql-copy-rec2 rec))))
	;; end of data
	(with-lexdb-locale 
	    (putline connection "\\."))
	;; stop listening on STDIN
	(endcopy connection)))))

;; retrieve contents of lex-keys
(defmethod get-unnormalized-lex-keys ((lex psql-lex-database))
  (recs
   (get-records lex "SELECT name,userid,modstamp,key FROM lex_key")))

;; NEW table LEX-KEY
(defmethod new-lex-key-table ((lex psql-lex-database))
  (empty-cache lex)
  (kill-lex-key-table lex)
  (create-lex-key-table lex)
  (create-lex-key-indices lex)
  (regenerate-orthkeys lex))

;; KILL table LEX-KEY
(defmethod kill-lex-key-table ((lex psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE lex_key CASCADE" :ignore-errors t)))

;; kill all lex-key entries, then regenerate them
(defmethod regenerate-orthkeys ((lex psql-lex-database))
  (lexdb-time ("regenerating lex_key entries" "done regenerating lex_key entries")
	      (with-dropped-lex-key-indices (lex)
		(run-command lex "DELETE FROM lex_key"))
	      (generate-missing-orthkeys lex)))
  
(defmethod generate-missing-orthkeys ((lex psql-lex-database))
  (put-normalized-lex-keys 
   lex
   (normalize-orthkeys lex
			(create-unnormalized-missing-lex-keys lex))))

(defmethod normalize-orthkeys ((lex mu-psql-lex-database) recs)
  (normalize-orthkeys-ith recs 3))

(defmethod normalize-orthkeys ((lex su-psql-lex-database) recs)
  (normalize-orthkeys-ith recs 1))

(defun normalize-orthkeys-ith (recs i)
  (loop
      for rec in recs
      do (setf (nth i rec) (normalize-orthkey! (nth i rec))))
  recs)

(defmethod create-unnormalized-missing-lex-keys ((lex psql-lex-database))
  (loop
      for rec in
	(get-raw-records lex 
			 (format nil (create-unnormalized-missing-lex-keys3-FSQL lex)
				 (orth-field lex)))
      for orth-list = (string-2-str-list (fourth rec))
      if (= 1 (length orth-list))
      collect rec
      else
      append 
      (loop for word in orth-list
	  collect (list (first rec) (second rec) (third rec) word))))
 
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
	     (let* ((record (car (recs (retrieve-raw-record-no-cache lex id reqd-fields)))))
	       (when cache
		 (setf (gethash id record-cache)
		   (or record :EMPTY)))
	       record))))))

(defmethod retrieve-raw-record-no-cache ((lex psql-lex-database) id &optional (reqd-fields '("*")))
  (unless (connection lex)
    (format t "~&(LexDB) WARNING:  no connection to psql-lex-database")
    (return-from retrieve-raw-record-no-cache))
  (retrieve-entry2 lex (2-str id) :reqd-fields reqd-fields))

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

(defun convert-obsolete-dfn-type-if-nec (l-type)
  (let* ((type (car l-type))
	 (x (cdr (assoc type *lexdb-fmtype-alt*))))
    (when x
      (format t "~%;;; Warning: (LexDB) DFN type '~S' is obsolete" type)
      (format t "~%;;;                  (please use '~S' instead)" x)
      (setf (car l-type) x))))

#+:null
(defmethod make-field-map-slot ((lex psql-lex-database))
  (get-fields lex)
  (get-dfn lex))

(defmethod get-dfn ((lex psql-lex-database))
  (with-slots (dfn) lex
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
	      (convert-obsolete-dfn-type-if-nec type)
	      (list slot field path type)))
	(get-raw-records lex (format nil "SELECT slot,field,path,type FROM dfn")))
;	(get-raw-records lex (format nil "SELECT slot,field,path,type FROM dfn WHERE mode='~a' OR mode = ''" (fields-tb lex))))
       #'(lambda (x y) (declare (ignore y)) (eq (car x) :UNIFS))))
    (if (null dfn)
	(complain-no-dfn lex))
    dfn))

(defmethod complain-no-dfn ((lex psql-lex-database))
  (error "~&(LexDB) no DFN entries" (dbname lex)))

(defmethod get-field-size-map ((lex psql-lex-database))
  (let* ((table (get-internal-table-dfn lex))
	(recs (recs table))
	 (cols (cols table)))
    (mapcar 
     #'(lambda (x) (field-size-elt x cols)) 
     recs)))

;; used by Emacs interface

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
    (empty-cache lex)
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

(defmethod new-semi ((lex psql-lex-database))
  (kill-semi lex)
  (create-semi-tables lex)
  (create-semi-indices lex))

(defmethod kill-semi ((lex psql-lex-database))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_pred CASCADE" :ignore-errors t))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_frame CASCADE" :ignore-errors t))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_var CASCADE" :ignore-errors t))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_extra CASCADE" :ignore-errors t))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_mod CASCADE" :ignore-errors t))
  (with-lexdb-client-min-messages (lex "error")
    (run-command lex "DROP TABLE semi_obj CASCADE" :ignore-errors t)))

(defmethod semi-p ((lex psql-lex-database))
  (not (typep (get-records lex "select count(*) from semi_pred where null" :ignore-errors t) 'sql-error)))

(defmethod create-skeletal-db-semi ((lex psql-lex-database))
  (create-semi-tables lex)
  (create-semi-indices lex))

(defmethod create-semi-tables ((lex psql-lex-database))
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
  
(defmethod create-semi-indices ((lex psql-lex-database))
  (run-command lex "
CREATE INDEX semi_pred_lex_id ON semi_pred (lex_id);
CREATE INDEX semi_pred_pred_id ON semi_pred (pred_id);
CREATE INDEX semi_frame_frame_id ON semi_frame (frame_id);
CREATE INDEX semi_frame_var_id ON semi_frame (var_id);
CREATE INDEX semi_var_var_id ON semi_var (var_id);
CREATE INDEX semi_extra_extra_id ON semi_extra (extra_id);
CREATE UNIQUE INDEX semi_mod_name_userid_modstamp ON semi_mod (name,userid,modstamp);
"))
  
(defmethod drop-semi-indices ((lex psql-lex-database))
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

;;; LEX KEY TABLE
;
;(defmethod index-lex-key ((lex psql-lex-database))
;  (run-command lex "CREATE INDEX lex_key_key ON lex_key (key)"))
;
;(defmethod deindex-lex-key ((lex psql-lex-database))
;  (run-command lex "DROP INDEX lex_key_key" :ignore-errors t))

;;

;;;
;;;
;;;

(defmethod get-fields ((lex psql-lex-database))
  (setf (fields lex)
    (mapcar 
     #'(lambda (x) (intern x :keyword))
     (list-fld lex))))

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
  (build-lex-if-nec lexdb))

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
  (quote-ident lex (orth-field-kw lex)))

(defmethod orth-field-kw ((lex psql-lex-database))
  (second (assoc :ORTH (dfn lex))))

;(defmethod update-lex ((lex psql-lex-database))
;  (update-lex-aux lex)
;  lex)

(defmethod to-db-dump-rev ((x lex-entry) (lex psql-lex-database) &key (skip-stream t))
  "provide line entry for lex db import file"
  (let* (;;ordered a-list of field values
	 (field-vals (get-field-vals x lex))
	 (skip (cdr (assoc :|_tdl| field-vals)))
	 (name (cdr (assoc :|name| field-vals)))
	 (ordered-field-vals (ordered-symb-val-list (fields lex) field-vals))
	 ;; construct CVS copy line
	 (line 
	  (format nil "~a~%" 
		  (str-list-2-line
		   (mapcar
		    #'(lambda (x)
			(let ((val (cdr x)))
			  (if val
			      (2-str val)
			    nil)))
		    ordered-field-vals)
		   :sep-c #\tab
		   :null-str "\\N"))))
    (cond
     ;; no components of lex entry skipped
     ((null skip)
      line)
     ;; component(s) skipped, but :skip field available in db
     ((member :|_tdl| (fields lex))
      (format t "~&(LexDB) Unhandled TDL fragment in lexical entry ~a: ~%~t~a~%~%" name skip)
      (format t "~&;; (LexDB) Unhandled TDL fragment in ~a placed in _tdl field as unstructured text" name)
	line)
     ;; component(s) skipped and no :skip field in db
     (t
      (format t "~&~%(LexDB) Lex entry ~a skipped due to unhandled TDL fragment: ~%~t~a~%" name skip)
      (format skip-stream "~a" (to-tdl x))
      ""))))


;; todo: this is very inefficient
(defmethod import-tdl-file ((lex psql-lex-database) filename)
  (catch 'abort 
    (unless lex
      (error "~%lex is NULL"))
    (let ((lexicon (load-scratch-lex :filename filename)))
      (query-for-meta-fields)
      (reconnect lex);; work around server bug
      (time 
       (export-to-db lexicon lex))
      (close-lex lexicon)
      ))
  ;(if (next-method-p) (call-next-method))
  )

(defmethod dump-generator-indices-to-psql ((lex psql-lex-database))
  (lexdb-time 
   ("dumping object SEMI to LexDB" "done dumping object SEMI to LexDB")
   (mrs::sdb-to-psql 
    lex (mrs::populate-sdb :semantic-table mrs::*semantic-table*))
   (populate-semi-mod lex)
   
   (let ((not-indexed mrs::*empty-semantics-lexical-entries*))
     (when not-indexed
       (populate-semi-mod-EMPTY-SEM lex not-indexed)
       ))
   ;; semi_mod indexes should be created after this call
   (run-command lex "SET ENABLE_HASHJOIN TO false")
   ))

(defmethod collect-psort-ids-aux ((lex psql-lex-database))
  (let ((query-res 
	 (get-raw-records lex (collect-psort-ids-SQL lex))))
    (mapcar 
     #'(lambda (x) 
	 (str-2-symb (car x)))
     query-res)))

;;!
;; DOT: select FIELDS for lexicon entry ID
(defmethod get-dot-lex-record ((lex psql-lex-database) id &optional (fields '("*")))
  (let ((table (retrieve-raw-record-no-cache lex id fields)))
    (dot (cols table) (car (recs table)))))
