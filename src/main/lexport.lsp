;;; Copyright (c) 2001 -- 2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; export TDL lexicon for (Access or) Postgres import.
;;;

;;; bmw (jul-03)
;;; - defaults, fixed bugs in export-lexicon-to-file

;;; bmw (jun-03)
;;; - direct export to postgres

(in-package :lkb)

(defvar *export-lexicon-p* nil)
(defvar *export-to* nil)

(defvar *export-output-file* "/tmp/lexicon.txt")
(defvar *export-skip-file* "/tmp/lexicon.skip")
(defvar *export-separator* #\,)
(defvar *export-counter* 0)
(defvar *export-timestamp* nil)

(defvar *export-output-lexicon* nil)
(defvar common-lisp-user::*grammar-version*)


;;; export prev loaded grammar to (psql) db or to file
(defun export-lexicon (&key
		       (to)
		       (output-lexicon *export-output-lexicon*)
		       (output-file *export-output-file*)
		       (skip  *export-skip-file*)
		       (separator *export-separator*))
  (cond
   ((equal to 'file)
    (export-lexicon-to-file 
     :output-file output-file
     :skip  skip
     :separator separator))
   (t
    (export-lexicon-to-db
     :output-lexicon output-lexicon)
    (reload-script-file) ;: fix_me
    )))
  
(defun export-lexicon-to-file (&key (output-file *export-output-file*) 
                            (skip *export-skip-file*)
                            (separator *export-separator*))
  (setf *export-to* 'file)
  (setf *export-counter* 0)
  (setf *export-separator* separator)
  (setf *export-output-file* output-file)
  (setf *export-skip-file* skip)
  (setf *current-source* common-lisp-user::*grammar-version*)
  (setf *export-timestamp* (extract-date-from-source *current-source*))
  (unless *current-lang* 
    (setf *current-lang* (ask-user-for-x "Export Lexicon" 
					 (cons "Language code?" "EN"))))
  (unless *current-country* 
    (setf *current-country* (ask-user-for-x "Export Lexicon" 
					    (cons "Country code?" "US"))))
  (setf *current-user* (ask-user-for-x "Export Lexicon" 
				       (cons "Username?" "guest")))
  (with-open-file (stream *export-output-file* :direction :output 
                   :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (stream *export-skip-file* :direction :output 
                   :if-exists :supersede :if-does-not-exist :create))
  (let ((*export-lexicon-p* t))
    (reload-lex-files :allp nil))) ;: hack: fix_me

(defun ask-user-for-x (head promptDcons)
  (car (ask-for-strings-movable head 
			   (list promptDcons))))

 ;;; implemented for postgres
(defun export-lexicon-to-db (&key (output-lexicon *export-output-lexicon*))
  (setf *export-to* 'db)
  (setf *current-source* common-lisp-user::*grammar-version*)
  (setf *export-timestamp* (extract-date-from-source *current-source*))
  (unless *current-lang* 
    (setf *current-lang* (ask-user-for-x "Export Lexicon" 
					 (cons "Language code?" "EN"))))
  (unless *current-country* 
    (setf *current-country* (ask-user-for-x "Export Lexicon" 
					    (cons "Country code?" "US"))))
  (setf *current-user* (ask-user-for-x "Export Lexicon" 
				       (cons "Username?" "guest")))
  (setf *export-output-lexicon* output-lexicon)
  (let ((*export-lexicon-p* t))
    (reload-lex-files :allp nil)
    ;(db-commit *export-output-lexicon*)
    (setf *export-timestamp* nil)
    ))					;: hack: fix_me

;;; a switch
(defun export-lexical-entry (name constraint)
  (cond
   ((equal *export-to* 'file) 
    (export-lexical-entry-to-file name constraint))
   (t
    (export-lexical-entry-to-db name constraint))))

;bmw
(defun export-lexical-entry-to-db (name constraint)
  (unless (and *export-output-lexicon* (connection *export-output-lexicon*))
    (setf *export-output-lexicon* (initialize-psql-lexicon))
    ;(db-begin *export-output-lexicon*)
    )
  (format *trace-output* "~%~a" name)
  (set-lexical-entry *export-output-lexicon* nil name constraint))

;oe
(defun export-lexical-entry-to-file (name constraint)
  (with-open-file (stream *export-output-file*
                   :direction :output 
                   :if-does-not-exist :create :if-exists :append)
    (let* ((separator (format nil "~a" *export-separator*))
           (type (extract-type-from-unifications constraint))
           (temp (extract-stem-from-unifications constraint))
           (stem (cdr temp))
           (count (car temp))
           (keyrel (extract-key-from-unifications constraint))      
           (keytag (extract-tag-from-unifications constraint))
           (altkey (extract-altkey-from-unifications constraint))
           (alt2key (extract-alt2key-from-unifications constraint))
           (compkey (extract-comp-from-unifications constraint))
           (ocompkey (extract-ocomp-from-unifications constraint))
           (total (+ count 1 
                     (if keyrel 1 0) (if keytag 1 0) (if altkey 1 0)
                     (if alt2key 1 0) (if compkey 1 0) (if ocompkey 1 0))))
      (cond 
       ((= total (length constraint))
	(format stream "~a~%"
	  (concatenate 'string
	    (string-downcase name) 
	    separator (string-downcase type) 
	    separator (string-downcase (str-list-2-str stem)) ;:todo: comma in word?
	    separator (string-downcase (first stem))
	    separator  ;;pronunciation
	    separator (string-downcase (or keyrel ""))
	    separator (string-downcase (or altkey ""))
	    separator (string-downcase (or alt2key ""))
	    separator (string-downcase (or keytag ""))
	    separator (string-downcase (or compkey ""))
	    separator (string-downcase (or ocompkey ""))
	    separator ;;complete
	    separator ;;semclasses
	    separator ;;preferences
	    separator ;;classifier
	    separator ;;selectrest
	    separator ;;comments
	    separator ;;exemplars
	    separator ;;usages
	    separator (or *current-lang* "") ;;lang
	    separator (or *current-country* "US") ;;country
	    separator ;;dialect
	    separator ;;domains
	    separator ;;genres
	    separator ;;register
	    separator "1";;confidence
	    separator "0" ;;version
	    separator (or *current-source* "?") ;;source
	    separator "1" ;;flags: 1 = not deleted
	    separator *current-user* ;;userid
	    separator (num-2-str *export-counter*) ;;id
	    separator (or *export-timestamp* "") ;;modstamp
	    )
        t))
       (t
        (format t"~%skipping super-rich entry: `~a'~%"  name)
        nil))))
  (incf *export-counter*))

(defun extract-date-from-source (source)
  (subseq source (1+ (position #\( source :test #'equal)) (position #\) source :test #'equal)))
				
(defun skip-lexical-entry (istream position)
  (with-open-file (ostream *export-skip-file* 
                   :direction :output
                   :if-exists :append :if-does-not-exist :create)
    (let ((end (file-position istream)))
      (file-position istream (- position 1))
      (loop
          for c = (read-char istream)
          while (<= (file-position istream) end)
          do 
            (format ostream "~a" c))
      (format ostream "~%~%"))))


(defun extract-key-from-unification (unification)
  (when (unification-p unification)
    (let ((lhs (unification-lhs unification)))
      (when (path-p lhs)
        (path-typed-feature-list lhs)))))

(defun extract-value-from-unification (unification)
  (when (unification-p unification)
    (let ((rhs (unification-rhs unification)))
      (when (u-value-p rhs)
        (u-value-type rhs)))))


;;type
(defun extract-type-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint nil))


;;keyrel
(defun extract-key-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS KEY)))

;;keytag
(defun extract-tag-from-unifications (constraint)
  (extract-value-by-path-from-unifications 
   constraint '(SYNSEM LOCAL KEYS KEY CARG)))

;;altkey
(defun extract-altkey-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS ALTKEY)))

;;altkey
(defun extract-alt2key-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LOCAL KEYS ALT2KEY)))

;;compkey
(defun extract-comp-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LKEYS --COMPKEY)))


;;ocompkey
(defun extract-ocomp-from-unifications (constraint)
  (extract-value-by-path-from-unifications constraint 
                                           '(SYNSEM LKEYS --OCOMPKEY)))


;;orthography
(defun extract-stem-from-unifications (constraint)
  (let ((stem nil)
        (count 1))
    (loop
        for path = nil then (cons 'rest path)
        for value = (extract-value-by-path-from-unifications
                     constraint (cons 'stem (append path '(first))))
        while value do 
          (incf count)
          (push value stem))
    (cons count (reverse stem))))


(defun extract-value-by-path-from-unifications (constraint path)
  (let* ((unification (find path constraint
                            :key #'extract-key-from-unification 
                            :test #'equal)))
    (when (unification-p unification)
      (extract-value-from-unification unification))))

