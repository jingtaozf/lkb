(in-package :lkb)

;;;
;;; --- external-lex-database methods
;;;

(defmethod make-requested-fields ((lexicon external-lex-database))
  ;; constructs the argument string to sql SELECT with all necessary fields
  (let* ((fields 
	  (remove-duplicates 
	   (mapcar #'cadr 
		   (fields-map lexicon))
	   :test #'equal))
         (fields-str 
	  (symb-2-str 
	   (pop fields))))
    (loop 
        for element in fields
        do 
	  (setf fields-str 
	    (concatenate 'string 
	      fields-str 
	      ", " 
	      (symb-2-str element))))
    fields-str))

;; this is to avoid being annoyed when word not in the database.
(defmethod collect-expanded-lex-ids ((lexicon external-lex-database))
  ;(error "collect-expanded-lex-ids(): invalid method on PostGreSQL lexicon")
  )

(defmethod close-lex ((lexicon external-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation delete))
  (with-slots 
      (lex-tb fields-map fields-tb record-cache) 
      lexicon
    (setf lex-tb nil) ;; unused
    (setf fields-map nil)
    (setf fields-tb nil)
    ))

(defmethod empty-cache ((lexicon external-lex-database) &key recurse)
  (declare (ignore recurse))
  (with-slots (record-cache) lexicon
    (clrhash record-cache)
    ))

