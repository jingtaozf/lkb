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
;;; export to .tsv file
;;;

(defun export-lexicon (&rest rest)
  (apply 'export-lexicon-to-file rest))

(defun export-lexicon-to-file (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-file-aux rest)))

(defun export-lexicon-to-file-aux (&key 
				(dir (postgres-user-temp-dir))
				file 
				(separator *postgres-export-separator*)
				(lexicon *lexicon*)
				use-defaults
				(recurse t))
  "export to db dump file format"
  (when (and file recurse)
    (format t "ignoring :recurse (ommit :file to enable :recurse)")
    (setf recurse nil))
  (when (null file)
    (setf file (format nil "~a/~a" dir 
                       (or (name lexicon) "unknown"))))
  (when (typep lexicon 'psql-lex-database)
    (error "use 'merge' command to export LexDB"))
  (setf file (namestring (pathname file)))
  (setf *postgres-export-separator* separator)
  (unless use-defaults
    ;; extra data in db entries
    (setf *postgres-current-source* (get-current-source))
    (setf *postgres-export-timestamp* (extract-date-from-source *postgres-current-source*))

    (query-for-modstamp-username)
    (query-for-meta-fields)
    (get-export-version))

  (let ((csv-file (format nil "~a.tsv" file))
	(skip-file (format nil "~a.skip" file))
	(multi-file (format nil "~a.multi.tsv" file)))
    (format t "~%~%Please wait: exporting lexicon ~a to CSV file ~a" (name lexicon) csv-file)
    (format t "~%   (skip file: ~a)" skip-file)
    
    (if *postgres-export-multi-separately*
	;; set multi stream
	(with-open-file (*postgres-export-multi-stream* 
			 multi-file
			 :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-open-file (*postgres-export-skip-stream* 
			   skip-file
			   :direction :output 
			   :if-exists :supersede :if-does-not-exist :create)
	    (format t "~%   (multi file: ~a)" multi-file)
	    (export-to-csv-to-file lexicon csv-file)))	  
      ;; no multi stream
      (with-open-file (*postgres-export-skip-stream* 
		       skip-file
		       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
	(export-to-csv-to-file lexicon csv-file))))
  (format t "~%export complete")
  (when recurse
    (mapcar #'(lambda (x) (export-lexicon :lexicon x 
					  :dir dir
					  :separator separator
					  :recurse t
					  :use-defaults t))
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
  (let ((lexicon *psql-lexicon*))
    (sql-fn-get-records lexicon 
			:clear_scratch)
    (reconnect lexicon) ;; work around server bug
    (sql-fn-get-records lexicon 
			:initialize_current_grammar 
			:args (list (get-filter *psql-lexicon*)))))

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

(defun get-export-version nil
  (let ((old-val *postgres-export-version*)
	(new-val))
    (loop
	until (integerp new-val)
	do
	  (let ((version-str (ask-user-for-x 
			      "Export Lexicon" 
			      (cons "Version?" (num-2-str old-val)))))
	    (if (null version-str)
		(throw 'abort 'version))
	    (setf new-val
	      (multiple-value-bind (a b)
		  (parse-integer version-str
				 :junk-allowed t)
		(and (= b (length version-str))
		     a)))))
    (setf *postgres-export-version* new-val)))
  
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
  (setf *postgres-current-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" (or (extract-pure-source-from-source *postgres-current-source*) ""))))
  (unless *postgres-current-source* (throw 'abort 'source))
  (setf *postgres-current-lang* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Language code?" (or *postgres-current-lang* "EN"))))
  (unless *postgres-current-lang* (throw 'abort 'lang))
  (setf *postgres-current-country* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Country code?" (or *postgres-current-country* "UK"))))
  (unless *postgres-current-country* (throw 'abort 'country))) 

(defun query-for-modstamp-username nil
  (setf *postgres-export-timestamp* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Modstamp?" (or *postgres-export-timestamp* "1990-01-01"))))
  (unless *postgres-export-timestamp* (throw 'abort 'modstamp))
  (setf *postgres-current-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" (or *postgres-current-user* (sys:user-name)))))
  (unless *postgres-current-user* (throw 'abort 'user)))