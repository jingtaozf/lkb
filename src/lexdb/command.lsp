;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;; LexDB menu commands
;;;

(defun command-merge-into-psql-lexicon (&rest rest)
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((filename (get-filename rest :ending ".csv" :existing t)))
    (when filename
      (format t 
	      "~%~%Please wait: merging files ~a.* into lexical database ~a" 
	      filename 
	      (dbname *psql-lexicon*))
      (force-output)
      (time (merge-into-psql-lexicon *psql-lexicon* filename))
      (lkb-beep))))

(defun command-dump-psql-lexicon (&rest rest)
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((filename (get-filename rest :ending ".csv" :existing nil)))
    (when filename
      (format t 
	      "~%~%Please wait: dumping lexical database ~a to files ~a.*" 
	      (dbname *psql-lexicon*) 
	      filename)
      (force-output)
      (time (dump-psql-lexicon filename))
      (lkb-beep))))
  
(defun command-export-lexicon-to-tdl (&rest rest)
  (let ((filename (get-filename rest :ending ".tdl" :existing nil)))
    (when filename
      (setf filename (format nil "~a.tdl" filename))
      (format t "~%~%Please wait: exporting lexicon to TDL file")
      (force-output)
      (time 
       (export-lexicon-to-tdl :file filename))
      (format t "~%Export complete")
      (lkb-beep))))
  
(defun command-set-filter-psql-lexicon (&rest rest)
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (time
   (apply 'set-filter-psql-lexicon rest))
  (lkb-beep))

(defun command-clear-scratch nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((count-priv (length (show-scratch *psql-lexicon*))))
    (format t "~%~%Please wait: clearing ~a entries from private space" count-priv)
    (force-output)
    (when (> count-priv 0)
      (time
       (close-scratch-lex)))
    (lkb-beep)))

(defun command-commit-scratch nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((count-priv (length (show-scratch *psql-lexicon*))))
    (format t "~%~%Please wait: moving ~a private entries to public space"
	    count-priv)
    (force-output)
    (when (> count-priv 0)
      (time
       (commit-scratch-lex)))
    (lkb-beep)))

(defun command-show-scratch nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((scratch
	 (mapcar 
	  #'(lambda (x) (cdr (first x))) 
	  (show-scratch *psql-lexicon*))))
    (format t "~%~%Contents of scratch (~a entries): ~a"
	    (length scratch)
	    scratch)
    (lkb-beep)))

(defun command-index-new-lex-entries nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (format t "~%~%Please wait: indexing new lexical entries for generator")
  (force-output)
  (time
   (index-new-lex-entries *lexicon*))
  (lkb-beep))

(defun command-vacuum-current-grammar nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (time
   (vacuum-current-grammar *psql-lexicon*)))

(defun command-vacuum-public-revision nil
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (time
   (vacuum-public-revision *psql-lexicon*))
  (lkb-beep))

(defun command-load-tdl-to-scratch (&rest rest)
  (unless (and
	   (typep *psql-lexicon* 'psql-lex-database)
	   (connection *psql-lexicon*))
    (error "please initialize PSQL lexicon"))
  (let ((filename (get-filename rest :ending ".tdl" :existing t)))
    (when filename
      (setf filename (format nil "~a.tdl" filename))
      (format t "~%~%Please wait: importing TDL entries")
      (load-tdl-from-scratch filename)
      (lkb-beep))))

(defun get-filename (rest &key (ending "") existing)
  (let* ((len-ending (length ending))
         (prompt (format nil "~a file?" ending))
         (filename
	 (cond
	  ((= (length rest) 0)
           (if existing
               (ask-user-for-existing-pathname prompt)
             (ask-user-for-new-pathname prompt)))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (cond
     ((and
       (> (- (length filename) len-ending) 
          0)
       (equal (subseq filename 
                      (- (length filename) 
                         len-ending)) 
              ending))
      (subseq filename 
              0 
              (- (length filename)
                 len-ending)))
     (t
      filename))))