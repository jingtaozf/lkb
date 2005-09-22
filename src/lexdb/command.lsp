;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; LexDB menu commands
;;;

(defun command-merge-into-lexdb (&rest rest)
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
  (let ((filename (get-filename rest :ending ".rev" :existing t)))
    (when filename
      (format t "~&(LexDB) merging files ~a.* into lexical database ~a ..." 
	      filename (dbname *lexdb*))
      (force-output)
      (time (merge-into-lexdb *lexdb* filename))
      (lkb-beep))))

(defun command-dump-lexdb (&rest rest)
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
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
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
  (time
   (apply 'set-filter *lexdb* rest))
  (lkb-beep))

(defun command-clear-private-rev nil
  (let* ((lex *lexdb*)
	 (rev-size (table-size lex :rev)))
    (unless (and
	     (typep lex 'psql-lex-database)
	     (connection lex))
      (error "please initialize-LexDB"))
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
  (let* ((lex *lexdb*)
	 (rev-size (table-size lex :rev)))
  (unless (and
	   (typep lex 'psql-lex-database)
	   (connection lex))
    (error "please initialize-LexDB"))
  (cond
     ((= rev-size 0)
      (format t "~&(LexDB) contents of scratch (0 entries): NIL")
      (lkb-beep))
     (t
      (when (y-or-n-p-general 
	     (format nil "Confirm COMMIT private 'rev' to 'public.rev'?"))
	(lexdb-time ("committing private 'rev' to 'public.rev'" 
		     "done committing private 'rev' to 'public.rev'")
       (commit-private-rev lex))
      (lkb-beep))))))

(defun command-show-private-rev nil
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
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

;(defun command-vacuum nil
;  (unless (and
;	   (typep *lexdb* 'psql-lex-database)
;	   (connection *lexdb*))
;    (error "please initialize-LexDB"))
;  (time
;   (vacuum *lexdb*))
;  (lkb-beep))

(defun command-load-tdl-to-scratch (&rest rest)
  (unless (and
	   (typep *lexdb* 'psql-lex-database)
	   (connection *lexdb*))
    (error "please initialize-LexDB"))
  (let ((filename (get-filename rest :ending ".tdl" :existing t)))
    (when filename
      (setf filename (format nil "~a.tdl" filename))
      (format t "~&(LexDB) importing TDL entries...")
      (load-tdl-to-private-rev filename)
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

(defmethod dump-small-lexicon (&key file)
  (unless *lex-ids-used* (error "*lex-ids-used* is empty"))
  (unless file
    (setf file
      (cond
       ((typep *lexicon* 'cdb-lex-database)
	(let ((template (pathname (namestring (first (slot-value *lexicon* 'source-files))))))
	  (make-pathname :directory (pathname-directory template)
			 :name (format nil "~a-small" (pathname-name template))
			 :type "tdl")))
       (t
	(make-pathname :directory (pathname-directory (lkb-tmp-dir))
		       :name "lexicon-small"
		       :type "tdl")))))
  (format t "~&(LexDB) dumping small tdl lexicon [~a entries] to file: ~a" (length *lex-ids-used*) file)
  (export-to-tdl-to-file *lexicon* file :lex-ids *lex-ids-used*))
