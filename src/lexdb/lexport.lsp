;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;   see `licence.txt' for conditions.

;;;
;;; export lexicon in various formats
;;;   (high level functions)

;;; bmw (oct-03)
;;; - 'mixed' type to handle mix of string/symbol values in field mapping

;;; bmw (aug-03)
;;; - merge-tdl-into-psql-lexicon
;;; - db scratch space
;;; - csv export creates multi entries from implicit info in tdl

;;; bmw (jul-03)
;;; - tdl export now handles coindexation, displays difference lists nicely
;;; - generalize field extraction
;;; - export to tdl, previous export mechanisms reworked
;;; - defaults, fixed bugs in export-lexicon-to-file

;;; bmw (jun-03)
;;; - direct export to postgres

(in-package :lkb)

(defvar *postgres-export-output-lexicon* nil)
(defvar *postgres-export-skip-stream* t)
(defvar *postgres-export-separator* #\,)

(defvar *postgres-export-version* 0)
(defvar *postgres-export-timestamp* nil) ;;; see lexport.lsp
(defvar *postgres-current-source* "?")
(defvar *postgres-current-user* nil)
(defvar *postgres-current-lang* nil)
(defvar *postgres-current-country* nil)

(defvar *postgres-export-multi-separately* nil)
(defvar *postgres-export-multi-stream* t)

(defvar *postgres-debug-stream*)

 ;;;
;;; export to .csv file
;;;

(defun export-lexicon (&rest rest)
  (apply 'export-lexicon-to-file rest))

(defun export-lexicon-to-file (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-file2 rest)))

(defun export-lexicon-to-file2 (&key 
				(dir (postgres-user-temp-dir))
				file 
				(separator *postgres-export-separator*)
				(lexicon *lexicon*)
				use-defaults
				(recurse t))
  (when (and file recurse)
    (format t "ignoring :recurse (ommit :file to enable :recurse)")
    (setf recurse nil))
  (unless file
    (if (name lexicon)
	(setf file (format nil "~a/~a" dir (name lexicon)))
      (setf file (format nil "~a/unknown" dir))))
  (when (typep lexicon 'psql-lex-database)
    (error "use Merge command to export LExDB"))
  (setf file (namestring (pathname file)))
  (setf *postgres-export-separator* separator)
  (unless use-defaults
    ;; extra data in db entries
    (setf *postgres-current-source* (get-current-source))
    (setf *postgres-export-timestamp* (extract-date-from-source *postgres-current-source*))
    (setf *postgres-current-source* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Source?" (or (extract-pure-source-from-source *postgres-current-source*) ""))))
    (unless *postgres-current-source* (throw 'abort 'source))
    
    (setf *postgres-export-timestamp* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Modstamp?" (or *postgres-export-timestamp* "1990-01-01"))))
    (unless *postgres-export-timestamp* (throw 'abort 'modstamp))
    
    (setf *postgres-current-lang* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Language code?" (or *postgres-current-lang* "EN"))))
    (unless *postgres-current-lang* (throw 'abort 'lang))
    
    (setf *postgres-current-country* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Country code?" (or *postgres-current-country* "UK"))))
    (unless *postgres-current-country* (throw 'abort 'country))
    
    (setf *postgres-current-user* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Username?" (or *postgres-current-user* "danf"))))
    (unless *postgres-current-user* (throw 'abort 'user))
  
    (get-export-version))

  (let ((csv-file (format nil "~a.csv" file))
	(skip-file (format nil "~a.skip" file))
	(multi-file (format nil "~a.multi.csv" file)))
    (format t "~%Exporting lexicon ~a to CSV file ~a" (name lexicon) csv-file)
    (format t "~%   (skip file: ~a)" skip-file)
    
    (if *postgres-export-multi-separately*
	;; set multi stream
	(with-open-file (*postgres-export-multi-stream* 
			 multi-file
			 :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-open-file (*postgres-export-skip-stream* 
			   skip-file
			   :direction :output 
			   :if-exists :supersede :if-does-not-exist :create)
	    (format t "~%   (multi file: ~a)" multi-file)
	    (export-to-csv-to-file lexicon csv-file)))	  
      ;; no multi stream
      (with-open-file (*postgres-export-skip-stream* 
		       skip-file
		       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
	(export-to-csv-to-file lexicon csv-file))))
  (format t "~%Export complete")
  (when recurse
    (mapcar #'(lambda (x) (export-lexicon :lexicon x 
					  :dir dir
					  :separator separator
					  :recurse t
					  :use-defaults t))
	    (extra-lexicons lexicon))))


(defmethod export-to-csv ((lexicon lex-database) stream)
  (let ((fields-map
	 (and *psql-lexicon* (fields-map *psql-lexicon*))))
    (unless fields-map
      (error "no fields map: plase connect to a LexDB"))
    (format t "~%Export fields map:~%~a~%" fields-map)
    (mapc 
     #'(lambda (x) 
	 (format stream "~a" (to-csv (read-psort lexicon x 
						 :recurse nil
						 :cache nil
						 :new-instance t
						 ) fields-map)))
     (collect-psort-ids lexicon :recurse nil))))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-csv lexicon ostream)))

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
		   ""  ;;pronunciation
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

(defun csv-line (&rest str-list)
  (str-list-2-str str-list
		  :sep-c *postgres-export-separator*
		  :null-str "?"))

(defmethod to-multi-csv-line (&key name base-name particle type keyrel)
  (let ((separator (string *postgres-export-separator*)))
    (format *postgres-export-multi-stream* "~a~%"
	    (concatenate 'string
	      name
	      separator base-name
	      separator particle
	      separator type
	      separator keyrel))
    ""))

;;;
;;; export to DB
;;;

(defun export-lexicon-to-db (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-db2 rest)))

(defun export-lexicon-to-db2 (&key (output-lexicon *postgres-export-output-lexicon*)
				  (lexicon *lexicon*))
  "export to postgres db lexicon"
  (unless
      (and output-lexicon (connection output-lexicon))
    (setf output-lexicon (initialize-psql-lexicon)))

  (setf *postgres-export-output-lexicon* output-lexicon)
  
  (setf *postgres-current-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" *postgres-current-source*)))
  (unless *postgres-current-source* (throw 'abort 'source))
  
  (setf *postgres-export-timestamp* "NOW")
  
    (setf *postgres-current-lang* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Language code?" (or *postgres-current-lang* "EN"))))
    (unless *postgres-current-lang* (throw 'abort 'lang))
    
    (setf *postgres-current-country* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Country code?" (or *postgres-current-country* "UK"))))
    (unless *postgres-current-country* (throw 'abort 'country))
    
  (format t 
	  "~%Exporting lexical entries to ~a" 
	  (dbname output-lexicon))  
  (export-to-db lexicon output-lexicon)
  (format t "~%Export complete"))

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x 
				    :recurse nil
				    :new-instance t) output-lexicon))
   (collect-psort-ids lexicon :recurse nil))
  (build-current-grammar *psql-lexicon*))

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
	 
	 (orth-list (string-2-str-list-on-spc orthography))
	 (psql-le
	  (make-instance-psql-lex-entry
	   :name name
	   :type type
	   :orthography orth-list	;list
	   :orthkey (get-orthkey orth-list)
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

;;;
;;; export to .tdl file
;;;

(defun export-lexicon-to-tdl (&key (dir (postgres-user-temp-dir))
				   file 
				   (lexicon *lexicon*))
    (unless file
    (if (name lexicon)
	(setf file (namestring (pathname (format nil "~a/~a" dir (name lexicon)))))
      (setf file (namestring (pathname (format nil "~a/unknown" dir))))))
  
  (let ((tdl-file (format nil "~a.tdl" file)))
    (if (equal (subseq file (- (length file) 4)) ".tdl")
	(setf tdl-file file))
    (format t "~%Exporting lexicon to TDL file ~a" tdl-file)
    (export-to-tdl-to-file lexicon tdl-file))
  (format t "~%Export complete~%"))

;;;
;;; get meta-level fields
;;;

(defun ask-user-for-x (head promptDcons)
  (car (ask-for-strings-movable head 
			   (list promptDcons))))

(defun get-export-version nil
  (let ((old-val *postgres-export-version*)
	(new-val))
    (loop
	until (integerp new-val)
	do
	  (let ((version-str (ask-user-for-x 
			      "Export Lexicon" 
			      (cons "Version?" (num-2-str old-val)))))
	    (if (null version-str)
		(throw 'abort 'version))
	    (setf new-val
	      (multiple-value-bind (a b)
		  (parse-integer version-str
				 :junk-allowed t)
		(and (= b (length version-str))
		     a)))))
    (setf *postgres-export-version* new-val)))
  
(defun extract-date-from-source (source)
  (if (not (stringp source))
      (format t "WARNING: unable to determine modstamp for grammar")
    (let* ((start (position #\( source :test #'equal))
	   (end (position #\) source :test #'equal))
	   (date (and start end (< (1+ start) end)
		      (subseq source (1+ start) end))))
      (if date
	  date
	(format t "WARNING: unable to determine modstamp for grammar")))))
      
(defun extract-pure-source-from-source (source)
  (let* ((end (position #\( source :test #'equal))
	 (pure-source (and end (< 1 end)
			   (subseq source 0 end))))
    (if pure-source
	(string-trim '(#\Space) pure-source)
      source)))

(defun get-current-source nil
  (let ((version (or (and (find-package :lkb)
			  (find-symbol "*GRAMMAR-VERSION*" :lkb))
		     (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))))
    (if (and version (boundp version))
	(symbol-value version)
      (format t "WARNING: no *GRAMMAR-VERSION* defined!"))))
    
;;;
;;; DB standard io
;;;

(defun get-new-filename (filename)
  (loop
      until (not (probe-file filename))
      do
	(setf filename (format nil "~aX" filename))
      finally
	(return filename)))

(defun merge-tdl-into-psql-lexicon (file-in)
  (setf file-in (namestring (pathname file-in)))
  (let ((tmp-lex (make-instance 'cdb-lex-database)))
    (unless
        (open-lex tmp-lex
                  :parameters (list (make-nice-temp-file-pathname ".tx")
                                    (make-nice-temp-file-pathname ".tx-index")))
      (format t "~%Operation aborted")
      (return-from merge-tdl-into-psql-lexicon))
    (unless (probe-file file-in)
      (error "~%file not found (~a)" file-in))
    (load-lex-from-files tmp-lex (list file-in) :tdl)
    (export-lexicon-to-db :lexicon tmp-lex :output-lexicon *psql-lexicon*)
    (close-lex tmp-lex))
  t)

;; assumes *lexicon* eq *psql-lexicon*
(defun load-scratch-lex (&key filename)
  (let ((lexicon (make-instance 'cdb-lex-database)))
    (unless
        (open-lex lexicon 
                  :parameters (list (make-nice-temp-file-pathname ".tx")
                                    (make-nice-temp-file-pathname ".tx-index")))
      (return-from load-scratch-lex))
    (load-lex-from-files lexicon (list filename) :tdl)
    lexicon))

(defun close-scratch-lex nil
  (fn-get-val *psql-lexicon* ''clear-scratch)
  (build-current-grammar *psql-lexicon*))

(defun commit-scratch-lex nil
  (fn-get-val *psql-lexicon* ''commit-scratch)
  (empty-cache *psql-lexicon*))

;;;
;;; low-level stuff
;;;

;;; insert lex entry into db
(defmethod set-lex-entry ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-val psql-le :modstamp "NOW")
  (set-val psql-le :userid (user lexicon))
  (set-lex-entry-aux lexicon psql-le)
  )
  
(defmethod set-lex-entry-aux ((lexicon psql-lex-database) (psql-le psql-lex-entry))
  (set-version psql-le lexicon) 
  (if *postgres-export-timestamp* (set-val psql-le :modstamp *postgres-export-timestamp*))
  (let* ((symb-list '(:type :orthography :orthkey :keyrel :altkey :alt2key :keytag :altkeytag :compkey :ocompkey :comments :exemplars :lang :country :dialect :domains :genres :register :confidence :version :source :flags :modstamp :userid))
	 (symb-list (remove-if #'(lambda (x) (or (null x) 
						 (and (stringp x)
						      (string= x ""))))
			       symb-list
			       :key #'(lambda (x) (retr-val psql-le x))))) 
    (run-query lexicon 
	       (make-instance 'sql-query
		 :sql-string (format nil
				     (fn lexicon 
					 'update-entry 
					 (retr-val psql-le :name) 
					 (sql-select-list-str symb-list) 
					 (sql-val-list-str symb-list psql-le)))))
    (unless
	(check-lex-entry (str-2-symb (retr-val psql-le :name)))
      (error "Invalid lexical entry -- see Lisp buffer output"))
    ))

;;;
;;; top-level commands
;;;

(defun command-load-tdl-to-scratch nil
  (catch 'abort 
    (unless *psql-lexicon*
      (error "~%no *psql-lexicon*!"))
    (let ((filename (ask-user-for-existing-pathname "TDL file?")))
      (when filename
	(let ((lexicon (load-scratch-lex :filename filename)))
	  (setf *postgres-current-source* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Source?" (or (extract-pure-source-from-source *postgres-current-source*) ""))))
	  (unless *postgres-current-source* (throw 'abort 'source))
	  (setf *postgres-current-lang* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Language code?" (or *postgres-current-lang* "EN"))))
	  (unless *postgres-current-lang* (throw 'abort 'lang))
	  (setf *postgres-current-country* 
	    (ask-user-for-x 
	     "Export Lexicon" 
	     (cons "Country code?" (or *postgres-current-country* "UK"))))
	  (unless *postgres-current-country* (throw 'abort 'country))
	  (export-to-db lexicon *psql-lexicon*)
	  (close-lex lexicon)
	  (lkb-beep))))))

