;;; Copyright (c) 2002 
;;;   Ann Copestake, Fabre Lambeau;
;;;   see `licence.txt' for conditions.

;;;
;;; minor modifications by oe (27-mar-02):
;;;
;;;  - provide initialize and de-initialize procedures;
;;;  - rework DB access layer to cache connection in `psql-database' object.
;;;
;;; ToDo
;;;   - look into Postgres optimization (factor of four or more);
;;;   - find out why we spend an extra 45 seconds or more over the CDB version
;;;     when looking up some 9000 stems;
;;;   - fix creation of psorts: fill in all fields, debug proper names;
;;;   - fix root node treatment, so they have a life outside of the database;
;;;   - decide on whether to keep using the `definition' table or move that
;;;     information into the globals;
;;;   - add export to TDL file support;
;;;   - add insertion of additional lexical entries support from LKB;
;;;   - work out interface for PET (not really LKB-related);
;;;   - investigate (declare (ignore ...)) statements (i put them in to avoid
;;;     compiler warnings);
;;;   - rework connection handling to re-open on demand (rather than error()).
;;;

(in-package :lkb)

;;;
;;; for now, assume there can be at most one (P)SQL-based lexicon at a time:
;;; whenever a request to (re-)initialize is performed, make sure to shut down
;;; an open (P)SQL lexical database (if available) first.
;;;

(defvar *psql-lexicon* nil)

(defun initialize-psql-lexicon (&key (db "lingo")
                                     (host "localhost")
                                     (user "guest")
                                     (password "guest")
                                     (table "erg")
                                     (definition "ergd"))
  
  (let* ((lexicon (make-instance 'psql-lex-database 
                    :dbname db :host host
                    :user user :password password
                    :lex-tb table :fields-tb definition))
         (properties (format 
                      nil 
                      "host='~a' dbname='~a' user='~a' password='~a'"
                      (sql-escape-string host)
                      (sql-escape-string db)
                      (sql-escape-string user)
                      (sql-escape-string password)))
         (connection (pg:connect-db properties))
         (status (pg:decode-connection-status (pg:status connection))))
    (cond
     ((eq status :connection-ok)
      (setf (connection lexicon) connection)
      (make-field-map-slot lexicon)
      (when *psql-lexicon* (close-psql-lexicon *psql-lexicon*))
      (setf *psql-lexicon* lexicon)
      (clrhash (slot-value *lexicon* 'lexical-entries))
      (setf (extra-lexicons *lexicon*) 
        (nconc (extra-lexicons *lexicon*) (list lexicon))))
     (t
      (error 
       "unable to connect to ~s: ~a" 
       (pg:db lexicon) (pg:error-message lexicon))))))

(defun close-psql-lexicon (&optional (lexicon *psql-lexicon*))
  (when (eq (type-of lexicon) 'psql-lex-database)
    ;;
    ;; shut down connection to PostGreSQL server
    ;;
    (let ((connection (connection lexicon)))
      (when connection 
        (pg:finish connection)
        (setf (connection lexicon) nil)))
    ;;
    ;; attempt to unlink from list of secondary lexica
    ;;
    (if (eq (extra-lexicons *lexicon*) lexicon)
      (setf (extra-lexicons *lexicon*) (rest (extra-lexicons *lexicon*)))
      (loop
          for foo on (extra-lexicons *lexicon*)
          when (eq (first (rest foo)) lexicon) do
            (setf (rest foo) (rest (rest foo)))))))
      

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

(defclass psql-database (sql-database)
  ((connection :initform nil :accessor connection :initarg connection)))

(defclass psql-lex-database (psql-database external-lex-database)
  ())

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
            (when (zerop (decf padding))
              (setf padding 42)
              (incf length padding)
              (setf result (adjust-array result length)))
          else do
            (vector-push c result)
          finally
            (return result))
    string))

(defun symbol-to-str-format (expr)
  (string-downcase (string expr))) 
(defun str-to-keyword-format (expr)
  (intern (string-upcase expr) :keyword))
(defun str-to-symbol-format (expr)
  (intern (string-upcase expr) :lkb))

(defun orth-string-to-str-list (string)
  ;;
  ;; break orthography string returned from DB at (one or more) spaces
  ;;
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


(defun work-out-value (key type value)
  (cond ((equal type "symbol") 
	 (unless (equal value "")
	   (str-to-symbol-format value)))
	((equal type "string")
	 (unless (equal value "")
	   (if (equal key :orth)
	     (orth-string-to-str-list value)
             value)))
	((equal type "list") (read-from-string value))
	(t value)))

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

(defmethod make-psort-struct ((lexicon psql-lex-database) (query sql-query))
  (let* ((query-res (first (mapped-recs query)))
         (strucslots 
          (loop 
              for (slot-key slot-field slot-path slot-type) 
              in (fields-map lexicon)
              for slot-value = (work-out-value 
                                slot-key slot-type 
                                (second (assoc slot-field query-res 
                                               :test #'equal)))
                               ;; if empty third argument (ie. path), 
                               ;; then add (:key "field")
	      when slot-value
              collect (if (equal slot-path "")
                        (list slot-key slot-value) 
                        (list slot-key
                              (make-unification
                               :lhs (make-path :typed-feature-list 
                                               (work-out-value 
                                                slot-key "list" slot-path))
                               :rhs (make-u-value :type slot-value))))))
         ;; groups slots with same key together in a list
         (strucargs 
          (loop
              for unique-slot in (remove-duplicates (mapcar #'car strucslots))
              append (list unique-slot 
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
                               (car values)))))))
    (apply #'make-lex-or-psort strucargs)))

;;;
;;;   lexicon database manipulation 
;;;


(defmethod lookup-word ((lexicon psql-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (let* ((orthography (sql-escape-string orth))
	 (orthfield "orthkey")
	 (orthstr (symbol-to-str-format orthography))
         (sql-str (format 
                   nil 
                   "select ~a from ~a where ~a='~a';"
                   (second (assoc :id (fields-map lexicon)))
                   (lex-tb lexicon)
                   orthfield orthstr))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'str-to-symbol-format (mapcar #'car (records query-res)))))

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
;;; _fix_me_
;;; for compliance with other lexical sources, lex-words() is expected to
;;; return "ad" and "hoc" separately, rather than just "ad hoc"; would need 
;;; to break orthography strings from DB into components and then uniq(1).
;;;                                                        (4-oct-02; oe)
(defmethod lex-words ((lexicon psql-lex-database))
  (let* ((sql-str (format 
                   nil 
                   "select distinct ~a from ~a;" 
                   (second (assoc :orth (fields-map lexicon)))
                   (lex-tb lexicon)))
         (query-res (run-query 
                     lexicon 
                     (make-instance 'sql-query :sql-string sql-str))))
    (mapcar #'(lambda (x) (string-upcase (car x)))
            (records query-res))))

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

;; TODO
;; rewrite the set-lexical-entry and store-psort, 
;; with sql statement "insert into", if needed.

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))

(defmethod clear-lex ((lexicon psql-lex-database) &optional no-delete)
  (declare (ignore no-delete)))

(defmethod read-cached-lex ((lexicon psql-lex-database) filenames)
  (declare (ignore filenames))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))

(defmethod unexpand-psort ((lexicon psql-lex-database) id)
  (declare (ignore id))
  (error "read-cached-lex(): invalid method on PostGreSQL lexicon"))
