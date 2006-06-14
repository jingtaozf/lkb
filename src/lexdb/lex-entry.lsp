;;; Copyright (c) 2001 -- 2005
;;;   Ben Waldron, John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;
;; some new methods not defined in lex.lsp
;;

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
  (unifs-to-tdl-body (lex-entry-unifs x)))
