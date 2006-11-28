;;; Copyright (c) 2001 -- 2006
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; LexDB menu commands
;;;

(defun command-merge-into-lexdb (&rest rest)
  (assert-mu-psql-lex-database *lexdb*)
  (let ((filename (get-filename rest :ending ".rev" :existing t)))
    (when filename
      (format t "~&(LexDB) merging files ~a.* into lexical database ~a ..." 
	      filename (dbname *lexdb*))
      (force-output)
      (time (merge-into-lexdb *lexdb* filename))
      (initialize-lexdb)
      (lkb-beep))))

(defun command-dump-lexdb (&rest rest)
  (assert-mu-psql-lex-database *lexdb*)
  (let ((filename (get-filename rest :ending ".rev" :existing nil)))
    (when filename
      (format t "~&(LexDB) dumping lexical database ~a to files ~a.* ..." 
	      (dbname *lexdb*) filename)
      (force-output)
      (time (dump-lexdb *lexdb* filename :tdl *lexdb-dump-tdl*))
      (lkb-beep))))
  
(defun command-export-lexicon-to-tdl (&rest rest)
  (let ((filename (get-filename rest :ending ".tdl" :existing nil)))
    (when filename
      (setf filename (format nil "~a.tdl" filename))
      (format t "~&(LexDB) exporting lexicon to TDL file...")
      (force-output)
      (time 
       (export-lexicon-to-tdl :file filename))
      (lkb-beep))))
  
(defun command-set-filter-lexdb (&rest rest)
  (assert-mu-psql-lex-database *lexdb*)
  (time
   (apply 'apply-filter *lexdb* rest))
  (lkb-beep))

(defun command-clear-private-rev nil
  (assert-mu-psql-lex-database *lexdb*)
  (let* ((lex *lexdb*)
	 (rev-size (table-size lex :rev)))
    (cond 
     ((= rev-size 0)
      (format t "~&(LexDB) contents of scratch (0 entries): NIL")
      (lkb-beep))
     (t
      (when (y-or-n-p-general 
	     (format nil "Confirm CLEAR all ~a entries from private 'rev'?" rev-size))
	(lexdb-time ("clearing private 'rev'" "done clearing private 'rev'")
		    (clear-private-rev lex))
	(lkb-beep))))))

(defun command-commit-private-rev nil
  (assert-mu-psql-lex-database *lexdb*)
  (let* ((lex *lexdb*)
	 (rev-size (table-size lex :rev)))
    (cond
     ((= rev-size 0)
      (format t "~&(LexDB) contents of scratch (0 entries): NIL")
      (lkb-beep))
     (t
      (when (y-or-n-p-general 
	     (format nil "Confirm COMMIT ~a entries from private 'rev' to 'public.rev'?"
		     (table-head-count lex :rev)))
	(lexdb-time ("committing private 'rev' to 'public.rev'" 
		     "done committing private 'rev' to 'public.rev'")
       (commit-private-rev lex))
      (lkb-beep))))))

(defun command-show-private-rev nil
  (assert-mu-psql-lex-database *lexdb*)
  (let ((scratch
	 (mapcar #'(lambda (x) (car x)) 
		 (show-scratch *lexdb*))))
    (format t "~&(LexDB) contents of scratch (~a entries): ~a"
	    (length scratch) scratch)
    (lkb-beep)))

(defun command-index-new-lex-entries nil
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
  (format t "~&(LexDB) indexing new lexical entries for generator...")
  (force-output)
  (time
   (index-new-lex-entries *lexicon*))
  (lkb-beep))

(defun command-load-tdl-to-scratch (&rest rest)
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
  (let ((filename (get-filename rest :ending ".tdl" :existing t)))
    (when filename
      (setf filename (format nil "~a.tdl" filename))
      (format t "~&(LexDB) importing TDL entries...")
      (import-tdl-file *lexdb* filename)
      (lkb-beep))))

(defun get-filename (rest &key (ending "") existing)
  (let* ((len-ending (length ending))
         (prompt (format nil "~a file?" ending))
         (filename (namestring (cond
		    ((= (length rest) 0)
		     (if existing
			 (ask-user-for-existing-pathname prompt)
		       (ask-user-for-new-pathname prompt)))
		    ((= (length rest) 1)
		     (first rest))
		    (t
		     (error "too many arguments")))))
	 (len-main (- (length filename) len-ending)))
    (cond
     ((and
       (> len-main 0)
       (equal (subseq filename len-main) ending))
      (subseq filename 0 len-main))
     (t
      filename))))

;;;
;;; dump small (tdl) lexicon of entries collected by tsdb
;;;

(defun dump-small-lexicon (&key file items-file-list)
  ;; using unwind-protect because rebinding globals via let statement 
  ;; does not work with itsdb
  (unwind-protect 
      (progn
	;; all we need are lexical ids
	(setf *abort-parse-after-lexical-lookup* t)
	;; empty *lex-ids-used*
	(setf *lex-ids-used* nil)
	;; construct filename if it is not supplied
	(unless file
	  (setf file (dump-small-lexicon-get-filename)))
	(cond
	 ;; if we have items files, process them and extract lexical ids
	 (items-file-list
	  (loop
	      for items-file0 in items-file-list
	      for items-file = (pathname items-file0)
	      for out-file = (merge-pathnames (make-pathname :type "tmp")
					      items-file)
	      do
		(format t "~&; calling (parse-sentences ~S ~S)" items-file out-file)
		(parse-sentences items-file out-file)))
	 (t
	  ;; if user provided no items files,
	  ;; tell the user to process test suite(s) manually
	  (format t "; [entering DUMP-SMALL-LEXICON]")
	  (format t "~%; Please now process test suites from which the")
	  (format t "~%; small the lexicon should be constructed.")
	  (format t "~%; Note that full parsing functionality is")
	  (format t "~%; temporarily disabled. Parsing will abort as soon")
	  (format t "~%; soon as lexical ids are collected.")
	  (format t "~%; Finish by entering ':cont' at the prompt below.")
	  (format t "~%~%")
	  ;; wait for the user to process some test suites
	  (lkb-beep)
	  (break)))
	;; attempt to dump small lexicon to filename
	(handler-case
	    (progn
	      (dump-small-lexicon-aux file))
	  (error (e) 
	    (format t "~%; Error: ~a" e)
	    (format t "~%; Error: unable to complete dump-small-lexicon")))
	(setf *abort-parse-after-lexical-lookup* nil))
    ;; reset to normal parse mode
    (setf *abort-parse-after-lexical-lookup* nil)
    (format t "~%; [exiting DUMP-SMALL-LEXICON]")))
    
(defun dump-small-lexicon-get-filename nil
  (cond
   ((typep *lexicon* 'cdb-lex-database)
    (let ((template (pathname (namestring (first (slot-value *lexicon* 'source-files))))))
      (make-pathname :directory (pathname-directory template)
		     :name (format nil "~a-small" (pathname-name template))
		     :type "tdl")))
   (t
    (make-pathname :directory (pathname-directory (lkb-tmp-dir))
		   :name "lexicon-small"
		   :type "tdl"))))
  
(defun dump-small-lexicon-aux (&optional file)
  (unless *lex-ids-used* (error "*lex-ids-used* is empty"))

  (format t "~&(LexDB) dumping small tdl lexicon [~a entries] to file: ~a" (length *lex-ids-used*) file)
  (export-to-tdl-to-file *lexicon* file :lex-ids *lex-ids-used*))

(defun assert-mu-psql-lex-database (lex)
  (cond
   ((typep lex 'su-psql-lex-database)
    (error "Action not available in SINGLE USER mode"))
   ((typep lex 'mu-psql-lex-database))
   (t
    (error "LexDB is not open"))))