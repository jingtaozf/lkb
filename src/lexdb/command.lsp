(in-package :lkb)

;;; LexDB menu commands
;;;

(defun command-merge-into-psql-lexicon (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-existing-pathname "CSV file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (if (and
	 (> (- (length filename) 4) 
	    0)
	 (equal (subseq filename 
			(- (length filename) 
			   4)) 
		".csv"))
	(setf filename (subseq filename 
			       0 
			       (- (length filename) 4))))
    (when filename
      (format t 
	      "~%Please wait: merging files ~a.* into lexical database ~a" 
	      filename 
	      (dbname *psql-lexicon*))
      (force-output)
      (time (merge-into-psql-lexicon *psql-lexicon* filename))
      (lkb-beep))))

(defun command-dump-psql-lexicon (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-new-pathname "CSV file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (if (and
	 (> (- (length filename) 
	       4) 
	    0)
	 (equal (subseq filename (- (length filename) 
				    4)) 
		".csv"))
	(setf filename 
	  (subseq filename 
		  0 
		  (- (length filename) 
		     4))))
    (when filename
      (format t 
	      "~%Please wait: dumping lexical database ~a to files ~a.*" 
	      (dbname *psql-lexicon*) 
	      filename)
      (force-output)
      (time (dump-psql-lexicon filename))
      (lkb-beep))))
  
(defun command-export-lexicon-to-tdl (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-new-pathname "TDL file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (when filename
      (time (export-lexicon-to-tdl :file filename))
      (lkb-beep))))
  
(defun command-set-filter-psql-lexicon (&rest rest)
  (time
   (apply 'set-filter-psql-lexicon rest))
  (lkb-beep))

(defun command-clear-scratch nil
  (let ((count-priv (length (show-scratch *psql-lexicon*))))
    (format t "~%Please wait: clearing ~a entries from private space" count-priv)
    (force-output)
    (when (> count-priv 0)
      (time
       (close-scratch-lex)))
    (lkb-beep)))

(defun command-commit-scratch nil
  (let ((count-priv (length (show-scratch *psql-lexicon*))))
    (format t "~%Please wait: moving ~a private entries to public space"
	    count-priv)
    (force-output)
    (when (> count-priv 0)
      (time
       (commit-scratch-lex)))
    (lkb-beep)))

(defun command-show-scratch nil
  (let ((scratch
	 (mapcar 
	  #'(lambda (x) (cdr (first x))) 
	  (show-scratch *psql-lexicon*))))
    (format t "~%Contents of scratch (~a entries): ~a"
	    (length scratch)
	    scratch)
    (lkb-beep)))

(defun command-index-new-lex-entries nil
  (format t "~%Please wait: indexing new lexical entries for generator")
  (force-output)
  (time
   (index-new-lex-entries *lexicon*))
  (lkb-beep))

(defun command-vacuum-current-grammar nil
  (time
   (vacuum-current-grammar *lexicon*)))

(defun command-vacuum-public-revision nil
  (time
   (vacuum-public-revision *lexicon*))
  (lkb-beep))

(defun command-load-tdl-to-scratch (&rest rest)
  (let ((filename
	 (cond
	  ((= (length rest) 0)
	   (ask-user-for-existing-pathname "TDL file?"))
	  ((= (length rest) 1)
	   (first rest))
	  (t
	   (error "too many arguments")))))
    (load-tdl-from-scratch filename)
    (lkb-beep)))
  