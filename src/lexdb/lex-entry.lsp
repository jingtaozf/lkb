;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod orthkey ((x lex-entry))
  (car (last (slot-value x 'orth))))
  
(defmethod to-csv ((x lex-entry) lexicon)
  "provide line entry for lexicon db import file"
  (with-slots (fields-map fields) lexicon
    (let* ((s (copy-slots x fields-map))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lexicon))))
	   (field-vals (append
			(mapcar 
			 #'(lambda (x) 
			     (cons x
				   (extract-field s x fields-map)))
			 extraction-fields)
			(list
			 (cons :orthkey (orthkey x))
			 (cons :userid *postgres-current-user*)
			 (cons :version (num-2-str *postgres-export-version*))
			 (cons :modstamp *postgres-export-timestamp*)
			 (cons :lang *postgres-current-lang*)
			 (cons :country *postgres-current-country*)
			 (cons :confidence 1)
			 (cons :source *postgres-current-source*)
			 (cons :flags 1))))
	   (ordered-field-vals (ordered-symb-val-list fields field-vals))
	   (line 
	    (format nil "~a~%" 
		    (str-list-2-str
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
       ((null (cdr (assoc :unifs s)))
	line)
       (t
	(format *postgres-export-skip-stream* "~a" (to-tdl x))
	"")))))

(defmethod to-db ((x lex-entry) (lexicon psql-lex-database))
  "insert lex-entry into lexicon db (user scratch space)"
  (with-slots (fields-map) lexicon
    (let* ((s (copy-slots x fields-map))
	   (extraction-fields (remove-duplicates
			       (cons :name (grammar-fields lexicon))))
	   (extracted-fields
	    (mapcan 
	     #'(lambda (x) (list x (extract-field s x fields-map)))
	     extraction-fields))
	 
	   (psql-le
	    (apply #'make-instance-psql-lex-entry
		   (append extracted-fields
			   (list :country *postgres-current-country*
				 :lang *postgres-current-lang*
				 :source (extract-pure-source-from-source *postgres-current-source*)
				 :confidence 1
				 :flags 1
				 )))))
      (cond
       ((null (cdr (assoc :unifs s)))
	(set-lex-entry lexicon psql-le)
	(empty-cache lexicon))
       (t
       (format t "~%skipping super-rich entry:~%~a" (to-tdl x))
       nil)))))
  
(defmethod copy-slots ((x lex-entry) fields-map)
  "copy slots for use in destructive operations"
  (let* ((slot-names
	  (remove-duplicates
	   (mapcar 
	    #'first
	    fields-map)))
	 (slots
	  (mapcar 
	   #'(lambda (s)
	       (cons
		s
		(copy-tree 
		 (slot-value 
		  x
		  (un-keyword s)))))
	   slot-names)))
    slots))

(defmethod to-tdl ((x lex-entry))
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str (lex-entry-id x))
   (to-tdl-body x)))
	  
(defmethod to-tdl ((x null)) nil)

(defmethod to-tdl-body ((x lex-entry))
  (p-2-tdl (pack-unifs (lex-entry-unifs x))))
	  
(defun extract-pure-source-from-source (source)
  (let* ((end (position #\( source :test #'equal))
	 (pure-source (and end (< 1 end)
			   (subseq source 0 end))))
    (if pure-source
	(string-trim '(#\Space) pure-source)
      source)))

