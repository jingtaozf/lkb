;;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

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

(defun export-lexicon-to-file (&key 
				(dir (postgres-user-temp-dir))
				name 
				(lexicon *lexicon*)
				use-defaults
				(recurse t))
  "export to db dump file format"
  (when (and name recurse)
    (format t "ignoring :recurse keyword to export-lexicon-to-file (ommit :name to enable :recurse)")
    (setf recurse nil))
  (when (equal lexicon *lexdb*)
    (error "You should use instead the \"Merge new entries\" command found under the LexDB menu if you want to create dump files for your current LexDB."))
  (unless (typep *lexdb* 'psql-lex-database)
    (error "Please open a connection to the LexDB. Eg. (initialize-lexdb :dbname \"MY_LEXDB\")"))
  (setf *lexdb-dump-source* (extract-pure-source-from-source (get-current-source)))
  (unless *lexdb-dump-user*
    (setf *lexdb-dump-user* (sys-user-name)))
  #-:sbcl
  (unless use-defaults
    (query-for-username)
    (query-for-meta-fields))
  (let* ((file-base (namestring
		     (pathname
		      (format nil "~a/~a" dir *lexdb-dump-source*))))
	 rev-file)
    (when (typep *lexdb* 'mu-psql-lex-database)
      (dump-dfn *lexdb* file-base)
      (dump-fld *lexdb* file-base)
      (dump-meta *lexdb* file-base))
    (setf rev-file (namestring (pathname (format nil "~a.rev" file-base))))
    (format t "~&(LexDB) exporting lexical entries to dump file ~a" rev-file)
    (with-open-file (rev-stream 
		     rev-file
		     :direction :output 
		     :if-exists :supersede 
		     :if-does-not-exist :create
		     )
      (export-lexicon-to-file-aux lexicon rev-stream file-base)
      (when recurse
	(mapcar #'(lambda (x) 
		    (export-lexicon-to-file-aux x rev-stream file-base))
		(extra-lexicons lexicon)))
      ))
  (lkb-beep))

(defun export-lexicon-to-file-aux (lexicon rev-stream file-base)
  (let ((*lexdb-dump-timestamp* (current-timestamp *lexdb*))
	(*lexdb-dump-source* (format nil "~a.~a" *lexdb-dump-source* (name lexicon)))
	(skip-file (namestring (pathname (format nil "~a.~a.skip" file-base (name lexicon))))))
    (setf *lexdb-dump-timestamp* *lexdb-dump-timestamp*)
    (format t "~&(LexDB)   skip file: ~a" skip-file)
    (with-open-file (skip-stream 
		     skip-file
		     :direction :output 
		     :if-exists :supersede 
		     :if-does-not-exist :create
		     )
      (export-to-db-dump-rev lexicon rev-stream :skip-stream skip-stream))))

;;;
;;; export to .tdl file
;;;

;;
;; dump lexdb to a number of source files
;; each named LEXDB_NAME.(SOURCE.)tdl
;; recursive behaviour is default
;; dump by source is disabled by default
;;
(defun export-lexicon-to-tdl (&key (dir (postgres-user-temp-dir))
				   file 
				   (lexicon *lexicon*)
				   (recurse t)
				   (by-source nil))
  ;; dumps of form LEXDB_NAME.SOURCE.tdl
  (when (and (typep lexicon 'psql-lex-database) by-source)
    (loop
	with filter = (filter lexicon)
	for r in 
	  (get-raw-records *lexdb* 
			   "select source from public.rev group by source")
	for source = (car r) ;; all sources in lexdb
	for file = (namestring ;; filename
			(pathname 
			 (format nil "~a/~a.~a.tdl" dir 
				 (or (name lexicon) "unknown")
				 source
				 )))
	do
	  (format t "~&(LexDB) export filename: ~a" file) ;; user feedback
	  (command-set-filter-lexdb ;; filter by source
	   (format nil 
		   "~a and source=~a"
		   filter
		   (psql-quote-literal source)))
	  (export-to-tdl-to-file lexicon file) ;; create dump file
	finally 
	  (command-set-filter-lexdb filter) ;; return filter to original value
	  ))
  ;; dumps of form LEXDB_NAME.tdl
  (when (not (and (typep lexicon 'psql-lex-database) by-source))
    (when (null file) ;; ie. no filename supplied
      (setf file (namestring 
		  (pathname 
		   (format nil "~a/~a.tdl" dir 
			   (or (name lexicon) "unknown"))))))
    (format t "~&(LexDB) export filename: ~a" file) ;; user feedback
    (export-to-tdl-to-file lexicon file)) ;; create dump file
  ;; recursive behaviour (when enabled)
  (when recurse
    (loop for sublex in (extra-lexicons lexicon) ;; for each sublexicon
	do
	  (export-lexicon-to-tdl ;; recurse 
	   :dir dir :lexicon sublex :recurse recurse
	   :by-source by-source))))

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

;;;
;;; get meta-level fields
;;;

;; NOTE: returns NIL if we don't have CLIM
(defun ask-user-for-x (head promptDcons)
  #+:clim
  (car (ask-for-strings-movable head 
				(list promptDcons)))
  #-:clim NIL
  )

(defun extract-date-from-source (source)
  (if (not (stringp source))
      (format t "(LexDB) WARNING:  unable to determine modstamp for grammar")
    (let* ((start (position #\( source :test #'equal))
	   (end (position #\) source :test #'equal))
	   (date (and start end (< (1+ start) end)
		      (subseq source (1+ start) end))))
      (if date
	  date
	(format t "(LexDB) WARNING:  unable to determine modstamp for grammar")))))
      
(defun get-current-source nil
  (let ((version (or (and (find-package :lkb)
			  (find-symbol "*GRAMMAR-VERSION*" :lkb))
		     (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))))
    (if (and version (boundp version))
	(symbol-value version)
      (format t "WARNING: no *GRAMMAR-VERSION* defined!"))))

#-:clim
(defun query-for-meta-fields nil
  (format t "~&Please set the following manually: *lexdb-dump-source* *lexdb-dump-lang* *lexdb-dump-country*"))
  

#+:clim
(defun query-for-meta-fields nil
  (setf *lexdb-dump-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" (or *lexdb-dump-source* ""))))
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

(defun query-for-username nil
  (setf *lexdb-dump-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" (or *lexdb-dump-user* (sys-user-name)))))
  (unless *lexdb-dump-user* (throw 'abort 'user)))

(defun sys-user-name ()
  (or #+(and :allegro-version>= (version>= 5 0)) 
      (sys:user-name)
      #+(and :allegro (not (and :allegro-version>= (version>= 5 0))))
      (system:getenv "USER")
      #+(and :mcl :powerpc) 
      (ccl:process-name ccl:*current-process*)
      #+:lucid 
      (lcl:environment-variable "USER")
      #+:sbcl
      (sb-ext:posix-getenv "USER")
      "nobody"))