;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defmethod to-csv ((x lex-entry) fields-map)
  "provide line entry for lexicon db import file"
  (let* ((s (copy-slots x fields-map))

	 (name (extract-field s :name fields-map))
	 (keyrel (extract-field s :keyrel fields-map))      
	 (keytag (extract-field s :keytag fields-map))
	 (altkey (extract-field s :altkey fields-map))
	 (altkeytag (extract-field s :altkeytag fields-map))
	 (alt2key (extract-field s :alt2key fields-map))
	 (compkey (extract-field s :compkey fields-map))
	 (ocompkey (extract-field s :ocompkey fields-map))
	 (type (extract-field s :type fields-map))
	 (orthography (extract-field s :orthography fields-map))
	 (pronunciation (extract-field s :pronunciation fields-map))

	 (orth-list (string-2-str-list-on-spc orthography))
	 (multi-base-name (and 
			   *postgres-export-multi-separately* 
			   (multi-p :name name :type type)))
	 (line 
	  (format nil "~a~%"
		  (csv-line
		   name
		   *postgres-current-user* ;;userid
		   (num-2-str *postgres-export-version*) ;;version
		   *postgres-export-timestamp* ;;modstamp
		   type
		   orthography 
		   (get-orthkey orth-list)
		   pronunciation
		   keyrel
		   altkey
		   alt2key
		   keytag
		   altkeytag
		   compkey
		   ocompkey
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
			     :particle compkey
			     :type type
			     :keyrel keyrel)
      line))
     (t
      (format *postgres-export-skip-stream* "~a" (to-tdl x))
      ""))))

(defmethod to-db ((x lex-entry) (lexicon psql-lex-database))
  "insert lex-entry into lexicon db (user scratch space)"
  (let* ((fields-map (fields-map lexicon))

	 (s (copy-slots x fields-map))

	 (name (extract-field s :name fields-map))
	 (keyrel (extract-field s :keyrel fields-map))      
	 (keytag (extract-field s :keytag fields-map))
	 (altkey (extract-field s :altkey fields-map))
	 (altkeytag (extract-field s :altkeytag fields-map))
	 (alt2key (extract-field s :alt2key fields-map))
	 (compkey (extract-field s :compkey fields-map))
	 (ocompkey (extract-field s :ocompkey fields-map))	 
	 (type (extract-field s :type fields-map))
	 (orthography (extract-field s :orthography fields-map))
	 (pronunciation (extract-field s :pronunciation fields-map))
	 
	 (orth-list (string-2-str-list-on-spc orthography))
	 (psql-le
	  (make-instance-psql-lex-entry
	   :name name
	   :type type
	   :orthography orth-list	;list
	   :orthkey (get-orthkey orth-list)
           :pronunciation pronunciation
	   :keyrel keyrel
	   :altkey altkey
	   :alt2key alt2key
	   :keytag keytag
	   :altkeytag altkeytag
	   :compkey compkey
	   :ocompkey ocompkey
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

