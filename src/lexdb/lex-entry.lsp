;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod orthkey ((x lex-entry))
  (car (last (slot-value x 'orth))))
  
(defmethod to-csv ((x lex-entry) fields-map)
  "provide line entry for lexicon db import file"
  (let* ((s (copy-slots x fields-map))

	 (name (extract-field s :name fields-map))
	 (f1 (extract-field s :f1 fields-map))
	 (f2 (extract-field s :f2 fields-map))
	 (f3 (extract-field s :f3 fields-map))      
	 (f6 (extract-field s :f6 fields-map))
	 (f4 (extract-field s :f4 fields-map))
	 (f7 (extract-field s :f7 fields-map))
	 (f5 (extract-field s :f5 fields-map))
	 (f8 (extract-field s :f8 fields-map))
	 (f9 (extract-field s :f9 fields-map))
	 (orthkey (orthkey x))
	 (pronunciation (extract-field s :pronunciation fields-map))
;	 (orth-list (string-2-str-list-on-spc f2))
	 (multi-base-name (and 
			   *postgres-export-multi-separately* 
			   (multi-p :name name :type f1)))
	 (line 
	  (format nil "~a~%"
		  (csv-line
		   name
		   *postgres-current-user* ;;userid
		   (num-2-str *postgres-export-version*) ;;version
		   *postgres-export-timestamp* ;;modstamp
		   f1
		   f2 
;		   (get-orthkey orth-list)
		   orthkey
		   pronunciation
		   f3
		   f4
		   f5
		   f6
		   f7
		   f8
		   f9
		   "" ;;complete
		   "" ;;semclasses
		   "" ;;preferences
		   "" ;;classifier
		   "" ;;selectrest
		   "" ;;jlink
		   "" ;;comments
		   "" ;;exemplars
		   "" ;;usages
		   *postgres-current-lang* ;;lang
		   *postgres-current-country* ;;country
		   "" ;;dialect
		   "" ;;domains
		   "" ;;genres
		   "" ;;register
		   "1";;confidence
		   *postgres-current-source* ;;source
		   "1" ;;flags: 1 = not deleted
		   ))))
    (cond 
     ((null (cdr (assoc :unifs s)))
      (if multi-base-name
	  (to-multi-csv-line :name name
			     :base-name multi-base-name
			     :particle f8
			     :type f1
			     :keyrel f3)
      line))
     (t
      (format *postgres-export-skip-stream* "~a" (to-tdl x))
      ""))))

(defmethod to-db ((x lex-entry) (lexicon psql-lex-database))
  "insert lex-entry into lexicon db (user scratch space)"
  (let* ((fields-map (fields-map lexicon))

	 (s (copy-slots x fields-map))

	 (name (extract-field s :name fields-map))
	 (f1 (extract-field s :f1 fields-map))
	 (f2 (extract-field s :f2 fields-map))
	 (f3 (extract-field s :f3 fields-map))      
	 (f6 (extract-field s :f6 fields-map))
	 (f4 (extract-field s :f4 fields-map))
	 (f7 (extract-field s :f7 fields-map))
	 (f5 (extract-field s :f5 fields-map))
	 (f8 (extract-field s :f8 fields-map))
	 (f9 (extract-field s :f9 fields-map))	 
	 (pronunciation (extract-field s :pronunciation fields-map))
	 
;	 (orth-list (string-2-str-list-on-spc f2))
	 (psql-le
	  (make-instance-psql-lex-entry
	   :name name
	   :f1 f1
;	   :f2 orth-list	;list
	   :f2 f2	;list
;	   :orthkey (get-orthkey orth-list)
	   :f3 f3
	   :f4 f4
	   :f5 f5
	   :f6 f6
	   :f7 f7
	   :f8 f8
	   :f9 f9
           :pronunciation pronunciation
	   :country *postgres-current-country*
	   :lang *postgres-current-lang*
	   :source (extract-pure-source-from-source *postgres-current-source*)
	   :confidence 1
	   :flags 1
	   )))
    (cond
     ((null (cdr (assoc :unifs s)))
      (set-lex-entry lexicon psql-le)
       (empty-cache lexicon))
     (t
       (format t "~%skipping super-rich entry:~%~a" (to-tdl x))
      nil))))

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
	  
(defmethod to-tdl-body ((x lex-entry))
  (p-2-tdl (pack-unifs (lex-entry-unifs x))))
	  
(defun extract-pure-source-from-source (source)
  (let* ((end (position #\( source :test #'equal))
	 (pure-source (and end (< 1 end)
			   (subseq source 0 end))))
    (if pure-source
	(string-trim '(#\Space) pure-source)
      source)))

