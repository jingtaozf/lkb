;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod orthkey ((x lex-entry))
  (normalize-orthkey (car (last (slot-value x 'orth)))))
  
(defmethod copy-slots ((x lex-entry) dfn)
  "copy slots for use in destructive operations"
  (let* ((slot-names
	  (remove-duplicates
	   (mapcar 
	    #'first
	    dfn)))
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
