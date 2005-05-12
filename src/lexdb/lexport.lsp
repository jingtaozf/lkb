;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; export lexicon in various formats

;;; bmw (oct-03)
;;; - 'mixed' type to handle mix of string/symbol values in field mapping

;;; bmw (aug-03)
;;; - merge-tdl-into-psql-lexicon
;;; - db scratch space
;;; - csv export creates multi entries from implicit info in tdl

;;; bmw (jul-03)
;;; - tdl export now handles coindexation, displays difference lists nicely
;;; - generalize field extraction
;;; - export to tdl, previous export mechanisms reworked
;;; - defaults, fixed bugs in export-lexicon-to-file

;;; bmw (jun-03)
;;; - direct export to postgres

(in-package :lkb)

;;;
;;; export to .rev file
;;;

(defun export-lexicon (&rest rest)
  (apply 'export-lexicon-to-file rest))

(defun export-lexicon-to-file (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-file-aux rest)))

(defun export-lexicon-to-file-aux (&key 
				(dir (postgres-user-temp-dir))
				name 
				(lexicon *lexicon*)
				use-defaults
				(recurse t))
  "export to db dump file format"
  (when (and name recurse)
    (format t "ignoring :recurse (ommit :name to enable :recurse)")
    (setf recurse nil))
  (when (typep lexicon 'psql-lex-database)
    (error "use 'merge' command to export LexDB"))
  (let* ((file (namestring
	       (pathname
		(format nil "~a/~a" dir 
			(or name (name lexicon) "unknown")))))
	(*lexdb-dump-source* *lexdb-dump-source*)
	;(*lexdb-dump-timestamp* *lexdb-dump-timestamp*)
	(rev-file (format nil "~a.rev" file))
	(skip-file (format nil "~a.skip" file)))
    (unless use-defaults
      ;; extra data in db entries
      (setf *lexdb-dump-source* (get-current-source))
      ;(setf *lexdb-dump-timestamp* (extract-date-from-source *lexdb-dump-source*))
      
      (query-for-username)
;      (query-for-modstamp-username)
      (query-for-meta-fields)
      ;(get-export-version)
      )
    
    (format t "~%~%Please wait: exporting lexicon ~a to REV file ~a" (name lexicon) rev-file)
    (format t "~%   (skip file: ~a)" skip-file)
    
      (with-open-file (*lexdb-dump-skip-stream* 
		       skip-file
		       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
	(unless (typep *psql-lexicon* 'psql-lex-database)
	  (error "please initialize *psql-lexicon*"))
	(dump-dfn *psql-lexicon* file)
	(dump-fld *psql-lexicon* file)
	(export-to-db-dump-to-file lexicon rev-file)))
  (format t "~%export complete")
  (when recurse
    (mapcar 
     #'(lambda (x) 
	 (export-lexicon :lexicon x :dir dir :recurse t :use-defaults t))
     (extra-lexicons lexicon))))

;;;
;;; export to .tdl file
;;;

(defun export-lexicon-to-tdl (&key (dir (postgres-user-temp-dir))
				   file 
				   (lexicon *lexicon*))
  (when (null file)
    (setf file (namestring 
                (pathname 
                 (format nil "~a/~a.tdl" dir 
                         (or (name lexicon) "unknown"))))))
  (format t "~%(export filename: ~a)" file)
  (export-to-tdl-to-file lexicon file))

;;;
;;; DB standard io
;;;

(defun get-new-filename (filename)
  (loop
      until (not (probe-file filename))
      do
	(setf filename (format nil "~aX" filename))
      finally
	(return filename)))

(defun load-scratch-lex (&key filename)
  (let ((lexicon (make-instance 'cdb-lex-database)))
    (unless
        (open-lex lexicon 
                  :parameters (list (make-nice-temp-file-pathname ".tx")
                                    (make-nice-temp-file-pathname ".tx-index")))
      (return-from load-scratch-lex))
    (load-lex-from-files lexicon (list filename) :tdl)
    lexicon))

(defun close-scratch-lex nil
  (let* ((lexicon *psql-lexicon*)
	(filter (get-filter lexicon)))
    (sql-fn-get-records lexicon 
			:clear_scratch)
    (empty-cache *lexicon*)
    (reconnect lexicon) ;; work around server bug
    (sql-fn-get-records lexicon 
			:initialize_current_grammar 
			:args (list filter))))

(defun commit-scratch-lex nil
  (sql-fn-get-val *psql-lexicon* 
		  :commit_scratch)
  (empty-cache *psql-lexicon*))

(defun load-tdl-from-scratch (filename)
  (let ((psql-lexicon *psql-lexicon*))
    (catch 'abort 
      (unless psql-lexicon
	(error "~%psql-lexicon is NULL"))
      (let ((lexicon (load-scratch-lex :filename filename)))
	(query-for-meta-fields)
	(reconnect psql-lexicon);; work around server bug
	(time 
         (export-to-db lexicon psql-lexicon))
	(close-lex lexicon)
	(format t "~%(private space: ~a entries)" 
		(length (show-scratch psql-lexicon)))))))

;;;
;;; get meta-level fields
;;;

(defun ask-user-for-x (head promptDcons)
  (car (ask-for-strings-movable head 
			   (list promptDcons))))

;; obsolete
;(defun get-export-version nil
;  (let ((old-val *lexdb-dump-version*)
;	(new-val))
;    (loop
;	until (integerp new-val)
;	do
;	  (let ((version-str (ask-user-for-x 
;			      "Export Lexicon" 
;			      (cons "Version?" (num-2-str old-val)))))
;	    (if (null version-str)
;		(throw 'abort 'version))
;	    (setf new-val
;	      (multiple-value-bind (a b)
;		  (parse-integer version-str
;				 :junk-allowed t)
;		(and (= b (length version-str))
;		     a)))))
;    (setf *lexdb-dump-version* new-val)))
  
(defun extract-date-from-source (source)
  (if (not (stringp source))
      (format t "WARNING: unable to determine modstamp for grammar")
    (let* ((start (position #\( source :test #'equal))
	   (end (position #\) source :test #'equal))
	   (date (and start end (< (1+ start) end)
		      (subseq source (1+ start) end))))
      (if date
	  date
	(format t "WARNING: unable to determine modstamp for grammar")))))
      
(defun get-current-source nil
  (let ((version (or (and (find-package :lkb)
			  (find-symbol "*GRAMMAR-VERSION*" :lkb))
		     (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))))
    (if (and version (boundp version))
	(symbol-value version)
      (format t "WARNING: no *GRAMMAR-VERSION* defined!"))))

(defun query-for-meta-fields nil
  (setf *lexdb-dump-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" (or (extract-pure-source-from-source *lexdb-dump-source*) ""))))
  (unless *lexdb-dump-source* (throw 'abort 'source))
  (setf *lexdb-dump-lang* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Language code?" (or *lexdb-dump-lang* "EN"))))
  (unless *lexdb-dump-lang* (throw 'abort 'lang))
  (setf *lexdb-dump-country* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Country code?" (or *lexdb-dump-country* "UK"))))
  (unless *lexdb-dump-country* (throw 'abort 'country))) 

;(defun query-for-modstamp-username nil
(defun query-for-username nil
;  (setf *lexdb-dump-timestamp* 
;    (ask-user-for-x 
;     "Export Lexicon" 
;     (cons "Modstamp?" (or *lexdb-dump-timestamp* "1990-01-01"))))
;  (unless *lexdb-dump-timestamp* (throw 'abort 'modstamp))
  (setf *lexdb-dump-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" (or *lexdb-dump-user* (sys:user-name)))))
  (unless *lexdb-dump-user* (throw 'abort 'user)))
