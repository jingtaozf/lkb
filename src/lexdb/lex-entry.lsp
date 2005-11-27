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
  (unifs-to-tdl-body (lex-entry-unifs x)))

;; map unifs to tdl (fragment) text
(defmethod unifs-to-tdl-body (unifs)
  ;; return empy string if no unifs
  (if (null unifs) 
      (return-from unifs-to-tdl-body ""))
  (if (member nil unifs 
	      :key (lambda (x) (path-typed-feature-list (unification-lhs x))))
      ;; TDL = type & ...
      (p-2-tdl (pack-unifs unifs))
    ;; TDL = ...
    (p-2-tdl-aux 0 (pack-unifs unifs))))
	  
;; map tdl (fragment) text to unifs
(defun tdl-to-unifs (tdl-fragment)
  ;; fix_me: unless the string does contain a non-empty tdl fragment we will throw an error later
  (unless (and (stringp tdl-fragment)
	       (> (length tdl-fragment) 0))
    (return-from tdl-to-unifs))
  ;; assume fragment non-empty...
  (let ((*readtable* (make-tdl-break-table)))
    (read-tdl-lex-avm-def (make-string-input-stream 
			   (concatenate 'string tdl-fragment "."))
			  nil)))

(defun extract-pure-source-from-source (source)
  (let* ((end (position #\( source :test #'equal))
	 (pure-source (and end (< 1 end)
			   (subseq source 0 end))))
    (if pure-source
	(string-trim '(#\Space) pure-source)
      source)))

(defun pprint-dag (&rest rest)
  (format t "~&~%~a"
	  (apply #'pprint-dag-aux rest))
  rest)

(defun pprint-dag-aux (x &key (depth 0) root (max-depth 100))
  (when (and max-depth (> depth max-depth))
    (return-from pprint-dag-aux "..."))
;  (setf x (mrs::path-value x root))
  (setf x (unify-paths-dag-at-end-of1 x root))
  (setf x (deref-dag x))
  (cond
   ((dag-arcs x) 
    (format nil "[~(~s%~s~) ~a]"
	    (dag-type x) (dag-new-type x)
	    (concatenate-strings
	     (loop
		 for (node . val) in (dag-arcs x)
		 collect (format nil "~%~a~a ~a" 
				 (make-string depth :initial-element #\space)
				 node (pprint-dag-aux val :depth (+ 2 depth)))))))
;   ((stringp (dag-type x))
;    (format nil "\"~a\"" (dag-type x)))
   (t
    (format nil "~s%~s" (dag-type x) (dag-new-type x)))
   ))
