;;; Copyright (c) 2002 
;;;   Ann Copestake, Fabre Lambeau, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; modifications by bmw (jun-03)
;;; - for compliance with other lexical sources, lex-words() is expected to
;;;     return "ad" and "hoc" separately, rather than just "ad hoc"; (done)
;;; - lookup-word now returns all entries containing given word. prev implementation used orthkey and so returned only entries whose last 'word' was word.
;;; - loading of multi-word lex entries implemented correctly
;;; - minor bug fixes
;;; - clear-lex methods implemented fully
;;; - clear-lex integrated with linking mechanism betw main and sub lexicons
;;;
;;; current issues:
;;; - versioning mechanism: - timestamped entries plus active flag
;;;                         - reset active on each change
;;; - profile/optimize/efficiency
;;; - export to TDL format
;;; - support insertion of lexical entries from LKB
;;;
;;; minor modifications by oe (27-mar-02):
;;;
;;;  - provide initialize and de-initialize procedures;
;;;  - rework DB access layer to cache connection in `psql-database' object.
;;;
;;; ToDo
;;;  - use SQL maximize to honour versioning mechanism in DB (or better view);
;;;  - look into Postgres optimization (factor of four or more);
;;;  - find out why we spend an extra 45 seconds or more over the CDB version
;;;    when looking up some 9000 stems;
;;;  - decide on whether to keep using the `definition' table or move that
;;;    information into the globals;
;;;  - add export to TDL file support; probably use `definition' table;
;;;  - generalize import code, maybe use `definition' table;
;;;  - add insertion of additional lexical entries support from LKB; maybe use
;;;    emacs(1) forms for input (and the emacs(1) -- lisp interface, such that
;;;    no extra installation overhead is incurred, e.g. for a web server to
;;;    talk to PostGres);
;;;  - work out interface for PET (not really LKB-related);
;;;  - investigate (declare (ignore ...)) statements (i put them in to avoid
;;;    compiler warnings);
;;;  - rework connection handling to re-open on demand (rather than error());
;;;  - support for generator indexing;
;;;  - integrate irregular spellings into lexical DB;
;;;  - versioning of databases, delivery, et al.
;;;

(in-package :lkb)

(defvar *psql-lexicon* nil)

;;;
;;; move code into 'constructor'?
;;;

					;: set up connection
					;: make-field-map-slot
					;: replace existing *psql-lexicon*
					;: clear lexical entries of *lexicon*
					;: link to lexicon
(defun initialize-psql-lexicon (&key (db "lingo")
                                     (host "localhost")
                                     (user "guest")
                                     (password "guest")
                                     (table "erg")
                                     (definition (format nil "~ad" table)))
  
  (let ((lexicon (make-instance 'psql-lex-database 
                    :dbname db :host host
                    :user user :password password
                    :lex-tb table :fields-tb definition))
	 )
    (cond
     ((eq (connect lexicon) :connection-ok)
      (make-field-map-slot lexicon)
      (when *psql-lexicon* (clear-lex *psql-lexicon*))
      (setf *psql-lexicon* lexicon)
      (clrhash (slot-value *lexicon* 'lexical-entries))
      (link *psql-lexicon* *lexicon*)
      )
     (t
      (error 
       "unable to connect to ~s: ~a"
       (pg:db connection) (pg:error-message connection))))))

;;;
;;; obsolete (use clear-lex instead)
;;;

;(defun close-psql-lexicon (&optional (lexicon *psql-lexicon*))
;  (when (eq (type-of lexicon) 'psql-lex-database)
;    ;;
;    ;; shut down connection to PostGreSQL server
;    ;;
;    (let ((connection (connection lexicon)))
;      (when connection 
;        (pg:finish connection)
;        (setf (connection lexicon) nil)))
;    ;; bmw:
;    ;; unlink from list of secondary lexica
;    ;;
;    (unlink lexicon *lexicon*)
;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to a Postgres database
;;;

(defclass sql-database ()
  ((dbname :initform nil :accessor dbname	:initarg :dbname)
   (host :initform "localhost" :accessor host :initarg :host)
   (user :initform (sys:user-name) :accessor user :initarg :user)
   (password :initform "" :accessor password :initarg :password)))

(defclass external-lex-database (lex-database)
  (
   ;; flat-table containing the lexical database
   (lexicon-table :initform nil :accessor lex-tb :initarg :lex-tb)
   ;; table for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping-table 
    :initform nil :accessor fields-tb :initarg :fields-tb)
   ;; a-list for mapping the lexicon-table fields to the psort-or-lex structure
   (slot-to-fields-mapping :initform nil :accessor fields-map)))

;;;
;;;
;;;

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)))

(defmethod connect ((lexicon psql-database))
  (with-slots (host dbname user password connection) lexicon
    (let (
	  (properties (format 
		       nil 
		       "host='~a' dbname='~a' user='~a' password='~a'"
		       (sql-escape-string host)
		       (sql-escape-string dbname)
		       (sql-escape-string user)
		       (sql-escape-string password)))
	  )
      (setf (connection lexicon) (pg:connect-db properties))
      (pg:decode-connection-status (pg:status connection))
      )
    )
  )

(defmethod disconnect ((lexicon psql-database))
  (with-slots (connection) lexicon
  ;:close connection cleanly
    (when connection 
      (pg:finish connection)
      (setf connection nil))
    )
  )

;;;
;;;
;;;

(defclass psql-lex-database (psql-database external-lex-database)
  ())

;;;
;;;
;;;

(defclass sql-query ()
  ((sql-string :accessor sql-string :initarg :sql-string)
   (records :accessor records)
   (columns :accessor columns)
   (count-records :accessor count-recs)
   (mapped-recs :accessor mapped-recs)))


;;;
;; --- String, Sql-query and Generic Sql-database methods and functions ---
;;;

(defun sql-escape-string (string)
  (if (and string (stringp string))
      (loop
          with padding = 128
          with length = (+ (length string) padding)
          with result = (make-array length
                                    :element-type 'character
                                    :adjustable nil :fill-pointer 0)
          for c across string
          when (char= c #\') do
            (vector-push #\\ result)
            (vector-push c result)
            (when (zerop (decf padding))              (setf padding 42)
              (incf length padding)
              (setf result (adjust-array result length)))
          else do
            (vector-push c result)
          finally
            (return result))
    string))

(defun symbol-to-str-format (expr)
  (unless (symbolp expr)
    (error "symbol expected"))
  (string-downcase (string expr)))

(defun str-to-keyword-format (expr)
  (unless (stringp expr)
    (error "string exected"))
  (intern (string-upcase expr) :keyword))

(defun str-to-symbol-format (expr)
  (unless (stringp expr)
    (error "string exected"))
  (intern (string-upcase expr) :lkb))

(defun orth-string-to-str-list (string)
  ;;
  ;; break orthography string returned from DB at (one or more) spaces
  ;;
  (unless (stringp string)
    (error "string exected"))

  (loop
      with result = nil
      with word = (make-array 42
                              :element-type 'character
                              :adjustable t :fill-pointer 0)
      with stream = (make-string-input-stream string)
      for c = (read-char stream nil nil)
      while c
      when (and (eql c #\space) (not (zerop (length word)))) do
        (push (copy-seq word) result)
        (setf (fill-pointer word) 0)
      when (not (eql c #\space)) do
        (vector-push-extend c word)
      finally
        (when (not (zerop (length word))) (push word result))
        (return (nreverse result))))

;;; returns (list of) values of appropriate type
(defun work-out-value (key type value)
  (cond ((equal type "symbol") 
	 (unless (equal value "")
	   (list (str-to-symbol-format value))))
	((equal type "string")
	 (unless (equal value "")
	   (list value)))
	((equal type "string-list")
	 (list (orth-string-to-str-list value))
	 )
	((equal type "string-fs")
	 (expand-string-list-to-fs-list (orth-string-to-str-list value))
	 )
	((equal type "list") (read-from-string value))
	(t (error "unhandled type during database access"))))

(defmethod run-query ((database psql-database) (query sql-query))
  (let ((connection (connection database)))
    (unless connection
      (error "database ~s has no active connection." database))
    (multiple-value-bind (recs cols recs-count)
        (pg:sql (sql-string query) :db connection)
      (setf (records query) recs 
            (columns query) cols
            (count-recs query) recs-count))
    query))

(defmethod make-column-map-record ((query sql-query))
  (setf (mapped-recs query) 
    (loop 
        for element in (records query)
        collect (mapcar #'list (columns query) element)))
  query)

;;;
;; --- external-lex-database methods
;;;

(defmethod make-field-map-slot ((lexicon psql-lex-database))
  ;; stores the mapping of fields to lex-or-psort structure slots
  (setf (fields-map lexicon)
    (mapcar #'(lambda (x) 
                (cons (str-to-keyword-format (car x)) (cdr x)))
            (records (run-query lexicon 
                                (make-instance 'sql-query
                                  :sql-string (format 
                                               nil 
                                               "select * from ~a;"
                                               (fields-tb lexicon))))))))

(defmethod make-requested-fields ((lexicon external-lex-database))
  ;; constructs the argument string to sql SELECT with all necessary fields
  (let* ((fields (remove-duplicates (mapcar #'cadr (fields-map lexicon))
                                    :test #'equal))
         (fields-str (pop fields)))
    (loop 
        for element in fields
        do (setf fields-str (concatenate 'string fields-str ", " element)))
    fields-str))

;;; eg. ("w1" "w2") -> ((FIRST "w1") (REST FIRST "w2") (REST REST *NULL*)) 
(defun expand-string-list-to-fs-list (string-list)
  (cond
   ((equal string-list nil) 
    (list (list '*NULL*)))
   (t
    (cons (list 'FIRST (first string-list)) 
	  (mapcar #'(lambda (x) (cons 'REST x))
	  (expand-string-list-to-fs-list (cdr string-list)))))))   

;;; create slot entry
(defun make-psort-struct-aux (slot-key slot-value slot-path)
  (cond
   ;: nil path => no unification
   ((equal slot-path "")
    (list slot-key slot-value))
   ;: atomic value => simple case
   ((atom slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path :typed-feature-list 
			   (work-out-value 
			    slot-key "list" slot-path))
	   :rhs (make-u-value :type slot-value))))
   ;: list. eg. (rest first "word") => (... rest first) has val "word"  
   ((listp slot-value)
    (list slot-key
	  (make-unification
	   :lhs (make-path :typed-feature-list 
			   
			   (append
			   (work-out-value slot-key "list" slot-path)
			   (reverse (cdr (reverse slot-value)))
			   )
			   )
	   :rhs (make-u-value :type (car (last slot-value))))))
  (T (error))
  ))

(defmethod make-psort-struct ((lexicon psql-lex-database) (query sql-query))
  (let* ((query-res (first (mapped-recs query)))
         (strucslots 
          (loop 
              for (slot-key slot-field slot-path slot-type) 
              in (fields-map lexicon)
              for slot-value-list = 
		(work-out-value 
                                slot-key slot-type 
                                (second (assoc slot-field query-res 
                                               :test #'equal)))
		
                               ;; if empty third argument (ie. path), 
                               ;; then add (:key "field")
	      when slot-value-list
              append 
	
		(mapcar #'(lambda (x) (make-psort-struct-aux slot-key x slot-path)) slot-value-list)

		))
	 
         ;; groups slots with same key together in a list
         (strucargs 
          (loop
              for unique-slot in (remove-duplicates (mapcar #'car strucslots))
              append
		(list unique-slot 
                           (let ((values (loop 
                                             for (psort-slot psort-value) 
                                             in strucslots
                                             when (eql psort-slot unique-slot)
                                             collect psort-value)))
                             ;;
                             ;; _fix_me_
                             ;; pretty bad fix to avoid getting lists where 
                             ;; they are not needed
                             ;;
                             (if (> (list-length values) 1)
                               values
                               (car values))))
		)))
    (apply #'make-lex-or-psort strucargs)))

;;;
;;;   lexicon database manipulation 
;;;

(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (if (connection lexicon)
  (let* ((orthography (sql-escape-string orth))
	 (orthfield "orthography")
	 (orthstr (string-downcase orthography)) ;:this is necessary
;fix-me: this code is inefficient
         (sql-str (format 
                   nil 
                   "select ~a from ~a where (~a like '~a' OR ~a like '~a %' OR ~a like '% ~a %' OR ~a like '% ~a');"
                   (second (assoc :id (fields-map lexicon)))
                   (lex-tb lexicon)
                   orthfield orthstr
                   orthfield orthstr
                   orthfield orthstr		   
                   orthfield orthstr		   
		   ))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
					;bmw
    (mapcar #'str-to-symbol-format (mapcar #'car (records query-res)))))
  ;: else warning message?
  )

; to be used to control whether the table exists
(defmethod table-existing-p ((lexicon psql-lex-database) tablename)
  (let* ((sql-str (format 
                   nil 
                   "select * from pg_tables where tablename='~a';" 
                   tablename))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (when (records query-res) tablename)))

;;; really necessary?
;;; (used to index for generator)
;;; for compliance with other lexical sources, lex-words() is expected to
;;; return "ad" and "hoc" separately, rather than just "ad hoc"; (done)
;;; fix_me: inefficient implementation
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((sql-str (format 
                   nil 
                   "select distinct ~a from ~a;" 
                   (second (assoc :orth (fields-map lexicon)))
                   (lex-tb lexicon)))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
 ;bmw
    (sort 
     (remove-duplicates
      (mapcan #'(lambda (x) (split-into-words (string-upcase (car x))))
	      (records query-res))
      :test #'equal)
     #'string-lessp
     )))

(defmethod collect-psort-ids ((lexicon psql-lex-database))
  (let* ((sql-str (format 
                   nil 
                   "select distinct ~a from ~a;" 
                   (second (assoc :id (fields-map lexicon)))
                   (lex-tb lexicon)))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'(lambda (x) (str-to-symbol-format (car x)))
            (records query-res))))

(defmethod read-psort ((lexicon psql-lex-database) id &key (cache nil))
  (declare (ignore cache))
  (let* ((sql-str (format nil "select ~a from ~a where ~a='~a';" 
                          (make-requested-fields lexicon)
                          (lex-tb lexicon)
                          (second (assoc :id (fields-map lexicon)))
                          (symbol-to-str-format id)))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (when (records query-res)
      (make-psort-struct lexicon (make-column-map-record query-res)))))

;; IN PROGRESS
;; rewrite the set-lexical-entry and store-psort, 
;; with sql statement "insert into", if needed.

;(defun add-lex-from-file (orth sense-id fs-or-type defs)
;  (let* ((lex-id (if orth (make-lex-id orth sense-id) sense-id))
;         (orth-string (if (and orth *sense-unif-fn*)
;                          (format nil "~A" orth) 
;			(extract-orth-from-unifs fs-or-type)))
;         (infl-pos (if (and (listp orth-string) (cdr orth-string))
;		       ;; infl-pos is only relevant for multi-word entries
;                       (find-infl-pos fs-or-type orth-string sense-id))))
;    ;; adapted for the case where the orthography is only specified
;    ;; in the FS; extract-orth-from-unifs must be defined on a
;    ;; per-grammar basis
;    (set-lexical-entry *lexicon* orth-string lex-id 
;		       (make-lex-or-psort
;			:orth orth-string
;			:infl-pos infl-pos                  
;			:sense-id sense-id 
;			:id lex-id
;			:unifs fs-or-type 
;			:def-unifs defs))))


;(defmethod set-lexical-entry (name constraint)

(defmethod set-lexical-entry ((lexicon lex-database) orth-string lex-id new-entry)
(error (format nil "METHOD UNDER CONSTRUCTION"))
;  (unless lex-id
;    (setf lex-id (if orth (make-lex-id orth sense-id) sense-id))
;    )
  
;  (unless orth-string
;    (setf orth-string (if (and orth *sense-unif-fn*)
;                          (format nil "~A" orth) 
;			(extract-orth-from-unifs fs-or-type)))
;    )
  
  (if orth-string (print (format t "WARNING: ignoring explicit orth-string")))
  (if lex-id (print (format t "WARNING: ignoring explicit lex-id")))
  (if (lex-or-psort-orth new-entry) (print (format t "WARNING: ignoring :orth")))
  (if (lex-or-psort-infl-pos new-entry) (print (format t "WARNING: ignoring :infl-pos")))
  (if (lex-or-psort-sense-id new-entry) (print (format t "WARNING: ignoring :sense-id")))
  (if (lex-or-psort-id new-entry) (print (format t "WARNING: ignoring :id")))
  (if (lex-or-psort-def-unifs new-entry) (print (format t "WARNING: ignoring :def-unifs")))
  
    (let* ((separator *export-separator*)
           (type (extract-type-from-unifications constraint))
           (temp (extract-stem-from-unifications constraint))
           (stem (cdr temp))
           (count (car temp))
           (keyrel (extract-key-from-unifications constraint))      
           (keytag (extract-tag-from-unifications constraint))
           (altkey (extract-altkey-from-unifications constraint))
           (alt2key (extract-alt2key-from-unifications constraint))
           (compkey (extract-comp-from-unifications constraint))
           (ocompkey (extract-ocomp-from-unifications constraint))
           (total (+ count 1 
                     (if keyrel 1 0) (if keytag 1 0) (if altkey 1 0)
                     (if alt2key 1 0) (if compkey 1 0) (if ocompkey 1 0))))
      (cond 
       ((= total (length constraint))
        ;;process the lexID and type fields,
        (format 
         nil
         "~d~a~(~a~)~a~(~a~)"
         (incf *export-counter*) 
         separator name separator type)

	;;along with the first word of the stem
	
        (format 
         nil
         "~a~a"
         separator (first stem))
        ;;print the rest of the stem to the file
        (loop
            for word in (rest stem)
            do (format stream " ~a" word))
        ;;print the other fields
        (format
         stream
         "~a~a~a~a~(~a~)~(~a~)~a~(~a~)~a~(~a~)~a~(~a~)~a~(~a~)~
          ~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a"
				
	 separator (first (last stem))
         ;separator (car stem) ;bmw
         separator  ;;pronunciation
         separator (or keyrel "")
         separator (or altkey "")
         separator (or alt2key "")
         separator (or keytag "")
         separator (or compkey "")
         separator (or ocompkey "")
         separator ;;complete
         separator ;;semclasses
         separator ;;preferences
         separator ;;classifier
         separator ;;selectrest
         separator ;;jlink
         separator ;;comments
         separator ;;exemplars
         separator ;;usages
         separator "EN" ;;lang
         separator "US" ;;country
         separator ;;dialect
         separator ;;domains
         separator ;;genres
         separator ;;register
         separator ;;confidence
         separator "danf" ;;user
         separator "03/27/02" ;;moddate
         separator 0 ;;version
         separator "LinGO" ;;source
         separator 1 ;;flags: 1 = not deleted
         )
        (format stream "~%")
        t)
       (t
        (format t"~%skipping super-rich entry: `~a'~%"  name)
        nil))))

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))

;bmw start

(defmethod clear-lex ((lexicon sql-database) &optional no-delete)
  (declare (ignore no-delete))
  (setf (dbname lexicon) nil)
  (setf (host lexicon) nil)
  (setf (user lexicon) nil)
  (setf (password lexicon) nil)
  )

(defmethod clear-lex ((lexicon external-lex-database) &optional no-delete)
  (declare (ignore no-delete))
  (setf (lexicon-table lexicon) nil)
  (setf (slot-to-fields-mapping-table lexicon) nil)
  (setf (slot-to-fields-mapping lexicon) nil)
  )

(defmethod clear-lex ((lexicon psql-database) &optional no-delete)
  (declare (ignore no-delete))
  (disconnect lexicon)
  )

;bmw end

(defmethod read-cached-lex ((lexicon psql-lex-database) filenames)
  (declare (ignore filenames))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))

(defmethod unexpand-psort ((lexicon psql-lex-database) id)
  (declare (ignore id))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))

;;;
;;; TDL output (work in progress)
;;;

(defmethod output-tdl ((lexicon psql-lex-database) syntax &optional file-name local-p)
(error (format nil "METHOD UNDER CONSTRUCTION"))
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (let ((count 0))
	;: do we need to sort?
        (loop for lex-name in (collect-psort-ids lexicon)
             do
             (if (> count 100)
               (progn (clear-expanded-lex lexicon)
                      (setf count 0))
               (incf count))
             (let ((entry (get-psort-entry lex-name)))
               (if entry
                   (case syntax
                     (:tdl (output-instance-as-tdl lex-name entry
                                                   ostream local-p))
                     (:lilfes 
                      (when local-p
                        (error "Local only output not supported with LiLFeS"))
                      (output-instance-as-lilfes 
                       lex-name entry
                       ostream))
                     (t (error "Unsupported syntax specifier ~A"
                               syntax)))
                 (format t "~%Warning ~A not found" lex-name))))))))

					;bmw-start

(defmethod get-tdl-str ((lexicon psql-lex-database) name &optional local-p)
(error (format nil "METHOD UNDER CONSTRUCTION"))
  (let* (
	(lex-struct (read-psort lexicon name))
	(fs (if local-p (lex-or-psort-local-fs lex-struct)
	      (tdfs-indef (lex-or-psort-full-fs lex-struct))))
	 (stream (make-string-output-stream))
	 )
    (format stream "~%:begin :instance.~%")
    (format stream "~%~A :=" (string-downcase name))
    (display-dag1 fs 'tdl stream)
    (format stream ".")
    (format stream "~%:end :instance.~%")
    
    (get-output-stream-string stream)
    ))

;(defun output-instance-as-tdl (name lex-struct stream local-p)
;  (format stream "~a" (get-tdl-str name localp))
;  )

					;bmw-end


