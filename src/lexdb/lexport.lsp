;;; Copyright (c) 2001 -- 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;   see `licence.txt' for conditions.

;;;
;;; export lexicon in various formats
;;;

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

(defvar *current-lex-id*) 

(defvar *postgres-temp-filename* nil)

(defvar *postgres-export-skip-stream* t)
(defvar *postgres-export-separator* #\,)

(defvar *postgres-export-version* 0)
(defvar *postgres-export-timestamp* nil)
(defvar *postgres-current-source*)
(defvar *postgres-current-user*)
(defvar *postgres-current-lang*)
(defvar *postgres-current-country*)

(defvar *postgres-export-output-lexicon* nil)
;(defvar common-lisp-user::*grammar-version*)
;(defvar *grammar-version*)
(defvar *postgres-export-default-fields-map*
    '((:UNIFS "alt2key" "(synsem local keys alt2key)" "symbol")
      (:UNIFS "altkey" "(synsem local keys altkey)" "symbol")
      (:UNIFS "altkeytag" "(synsem local keys altkey carg)" "string")
      (:UNIFS "compkey" "(synsem lkeys --compkey)" "symbol")
      (:UNIFS "keyrel" "(synsem local keys key)" "symbol")
      (:UNIFS "keytag" "(synsem local keys key carg)" "string")
      (:UNIFS "ocompkey" "(synsem lkeys --ocompkey)" "symbol")
      (:UNIFS "orthography" "(stem)" "string-fs") 
      (:UNIFS "type" "nil" "symbol")
      (:ID "name" "" "symbol") 
      (:ORTH "orthography" "" "string-list") 
      ;;(:SENSE-ID "name" "" "symbol")
      ))

(defvar *postgres-export-multi-separately* nil)
(defvar *postgres-export-multi-stream* t)

(defvar *postgres-user-temp-dir* (make-pathname :directory (pathname-directory (lkb-tmp-dir))))

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
				(dir *postgres-user-temp-dir*)
;;				(file (format nil "~a/lexicon" *postgres-user-temp-dir*)) 
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
      (format t 
	      "~%WARNING: using default fields map *postgres-export-default-fields-map*")
      (setf fields-map *postgres-export-default-fields-map*))
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
  (let* (
	 (unifs-in (copy-tree (slot-value x 'unifs)))
	 (keyrel (extract-field x :keyrel fields-map))      
	 (keytag (extract-field x :keytag fields-map))
	 (altkey (extract-field x :altkey fields-map))
	 (altkeytag (extract-field x :altkeytag fields-map))
	 (alt2key (extract-field x :alt2key fields-map))
	 (compkey (extract-field x :compkey fields-map))
	 (ocompkey (extract-field x :ocompkey fields-map))
	 
	 (type (extract-field x :type fields-map))
	 (orth-list (extract-field2 x :orth nil 'list))
	 (orthography (extract-field x :orthography fields-map))
	 (name (extract-field x :name fields-map))
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
		   (or *postgres-current-source* "?") ;;source
		   "1" ;;flags: 1 = not deleted
		   ))))
    (cond 
     ((null (lex-entry-unifs x))
      (if multi-base-name
	  (to-multi-csv-line :name name
			     :base-name multi-base-name
			     :particle compkey
			     :type type
			     :keyrel keyrel)
      line))
     (t
      (setf (slot-value x 'unifs) unifs-in)
      (format *postgres-export-skip-stream* "~a" (to-tdl x))
      ;;     (format *postgres-export-skip-stream* "~%skipping super-rich entry: ~a"  line)
      ""))))

(defun csv-line (&rest str-list)
  (str-list-2-str str-list
		  *postgres-export-separator*))

(defun encode-mixed-as-str (val)
  (cond
   ((null val)
    "")
   ((symbolp val)
    (let ((val-str (symb-2-str val)))
      (if (and (> (length val-str) 0)
	       (eq (aref val-str 0) #\"))
	  (format nil "\\~a" val-str)
	val-str)))
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type: ~a" val))))

(defun encode-string-as-str (val)
  (cond
   ((null val)
    "")
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type: ~a" val))))

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

(defmethod multi-p (&key name type)
  (cond
   ((equal (subseq type 0 10) "idiomatic-")
    (multi-idiom-base-name name))
   ((equal (subseq-from-end type 12) "_particle_le")
    (multi-vpc-base-name name))
   (t
    nil)))

(defun remove-sense-id-substr (name)
  (if (and (find #\_ name)
	   (numberp
	    (2-symb 
	     (subseq name (1+ (position #\_ name :from-end t))))))
      (subseq name 0 (position #\_ name :from-end t))
    name))

(defun multi-idiom-base-name (name-full)
  (let (( name (remove-sense-id-substr name-full)))
    (cond
     ((equal (subseq name 0 2) "i_")
      (subseq name 2))
     (t
      (format t "WARNING: cannot generate base name for idiom ~a~%" name-full)
      (format nil "UNKNOWN_BASE_~a" name)))))

(defun multi-vpc-base-name (name-full)
  (let ((name (remove-sense-id-substr name-full)))
    (cond
     ((and
       (not (equal (subseq name 0 1) "_"))
       (position #\_ name))
    (subseq name 0 (position #\_ name)))
     (t
      (format t "WARNING: cannot generate base name for vpc ~a~%" name-full)
      (format nil "UNKNOWN_BASE_~a" name)))))

;;;
;;; export to DB
;;;

(defun export-lexicon-to-db (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-db2 rest)))

 ;;; implemented for postgres
(defun export-lexicon-to-db2 (&key (output-lexicon *postgres-export-output-lexicon*)
				  (lexicon *lexicon*))
  (unless
      (and output-lexicon (connection output-lexicon))
    (setf output-lexicon (initialize-psql-lexicon)))

  (setf *postgres-export-output-lexicon* output-lexicon)
  
  (setf *postgres-current-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" *postgres-current-source*)))
  (unless *postgres-current-source* (throw 'abort 'source))
  
  (setf *postgres-export-timestamp* "NOW") ;; fixme?
  
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
  (let* ((fields-map (fields-map lexicon))

	 (keyrel (extract-field x :keyrel fields-map))      
	 (keytag (extract-field x :keytag fields-map))
	 (altkey (extract-field x :altkey fields-map))
	 (altkeytag (extract-field x :altkeytag fields-map))
	 (alt2key (extract-field x :alt2key fields-map))
	 (compkey (extract-field x :compkey fields-map))
	 (ocompkey (extract-field x :ocompkey fields-map))
	 
	 (orth-list (extract-field2 x :orth nil "list"))
	 (name (extract-field x :name fields-map))
	 (count (+ 2 (length orth-list)))
	 (total (+ count
		   (if (string> keyrel "") 1 0) 
		   (if (string> keytag "") 1 0) 
		   (if (string> altkey "") 1 0)
		   (if (string> altkeytag "") 1 0)
		   (if (string> alt2key "") 1 0) 
		   (if (string> compkey "") 1 0) 
		   (if (string> ocompkey "") 1 0)))
	 
	 (type (extract-field x :type fields-map))
	 
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
	   )
	  ))
    (cond
     ((= total (length (lex-entry-unifs x)))
      (set-lex-entry lexicon psql-le)
      (empty-cache lexicon))
     (t
      (format t "~%skipping super-rich entry: `~a'~%"  name)
      nil))))

;;;
;;; export to .tdl file
;;;

(defun export-lexicon-to-tdl (&key (dir *postgres-user-temp-dir*)
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
;;; tdl export (packed)
;;;

(defun tdl-val-str (symb)
  (cond
   ((null symb) "")
   ((numberp symb) (num-2-str symb))
   ((stringp symb) (format nil "~S" symb))
   (t (string-downcase (string symb)))))
    
(defun pack-unifs (unifs)
  (pack 
   (unifs-2-list unifs)))

(defun unifs-2-list (unifs)
  (let ((c 0)
	(coindex nil)
	(coindex-map)
	(match))
    (mapcan 
     #'(lambda (unif)
	 (with-slots (rhs lhs) unif
	   (cond
	    ((typep rhs 'U-VALUE)
	     (list (append (path-typed-feature-list lhs)
			   (u-value-type rhs))))
	    ((typep rhs 'PATH)
	     (setf match (assoc lhs coindex-map :test #'equalp))
	     (cond 
	      (match
	       (setf coindex (str-2-symb (format nil "#~a" (cdr match))))
	       (list (append (path-typed-feature-list rhs)
			     coindex)))
	      (t
	       (incf c)
	       (push (cons lhs c) coindex-map)
	       (setf coindex (str-2-symb (format nil "#~a" c)))
	       (list
		(append (path-typed-feature-list lhs)
			coindex)
		(append (path-typed-feature-list rhs)
			coindex))))))))
     unifs)))

(defun unif-2-lists (unif)
  (with-slots (rhs lhs) unif
    (cond
     ((typep rhs 'U-VALUE)
      (list (append (path-typed-feature-list lhs)
		    (u-value-type rhs))))
     ((typep rhs 'PATH)
      (list
       (append (path-typed-feature-list lhs)
	       '\#1)
       (append (path-typed-feature-list rhs)
	       '\#1))))))

(defun packed-extract-nonterminal (path packed)
  (packed-extract-aux path packed :terminal nil))

(defun packed-extract-terminal (path packed)
  (packed-extract-aux path packed :terminal t))

(defun packed-extract-aux (path packed &key terminal)
  (cond
   (path
    (packed-extract-nonterminal
     (cdr path)
     (cdr (car (member (car path) packed :key #'(lambda (x) (and (car x))))))))
   (terminal
    (mapcan #'(lambda (x) (and (not (cdr x)) (list x))) packed))
   (t
    (mapcan #'(lambda (x) (and (cdr x) (list x))) packed))))


(defun pack (l2)
  (loop
      for x in l2
      with p
      do
	(if (atom x)
	    (push (list x) p)
	  (if (assoc (car x) p)
	      (push (cdr x) (cdr (assoc (car x) p)))
	    (push (cons (car x) (list (cdr x))) p)))
      finally 
	(return
	  (mapcar
	   (lambda (x)
	     (if (atom x)
		 x
	       (cons (car x) (pack (cdr x)))))
	     (sort p #'pack-order)
	     ))))

;; list components ordered according to their printed representation
;; non-list components come first (non-deterministic ordering)
(defun pack-order (x y)
  (let ((a (pack-order-str x))
	(b (pack-order-str y)))
      (string< a b)))

(defun pack-order-str (x)
  (cond
   ((cdr x)
    (string (car x)))
   (t
    "")))

(defmethod export-to-tdl ((lexicon lex-database) stream)
  #+:psql
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(caching all lexical records)")
    (cache-all-lex-records *lexicon*)
    (format t "~%(caching complete)")
    )
  (mapc
   #'(lambda (id)
       (format stream "~a" (to-tdl (read-psort lexicon id
					       :new-instance t)))
       (unexpand-psort lexicon id))
   (collect-psort-ids lexicon))
  #+:psql
  (when (typep *lexicon* 'psql-lex-database)
    (format t "~%(emptying cache)")
    (empty-cache *lexicon*))
  )

(defmethod export-to-tdl-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

(defmethod to-tdl ((x lex-entry))
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str (lex-entry-id x))
   (to-tdl-body x)))
	  
(defmethod to-tdl-body ((x lex-entry))
  (p-2-tdl (pack-unifs (lex-entry-unifs x))))
	  
;; copy of p-2-tdl-2 w/o root
(defun p-2-tdl (branches)
  (unless branches
    (error "non-null value expected"))
  (let* ((a-branch-flag (not (cdr (first branches))))
	 (a-branches)
	 (len)
	 (i 0))
    (when a-branch-flag
      (do ()
	  ((or (null branches) (cdr (first branches))))
	(push (pop branches) a-branches)))
    (setf len (length branches))
    
     (cond
      ((and a-branch-flag (= len 0))
       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
			       a-branches)
		       " & "))
      (a-branch-flag
       (format nil "~a &~%~a ~a"  
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")
	       (make-string i :initial-element #\ )
	       (p-2-tdl-aux (+ i 3) branches)))
      ((= len 1)
       (format nil "~a" (p-2-tdl-2 i (first branches))))
      (t
       (format nil "~a" 
	       (p-2-tdl-aux i branches))))))

(defun p-2-tdl-2 (i p)
  (unless p
    (error "internal"))
  (let* ((root (car p))
	 (branches (cdr p))
	 (a-branch-flag (not (cdr (first branches))))
	 (a-branches)
	 (len))
  (setf i (+ i 3 (length (string root))))
    (when a-branch-flag
      (do ()
	  ((or (null branches) (cdr (first branches))))
	(push (pop branches) a-branches)))
    (setf len (length branches))
    
    (setf len (length branches))
     (cond
      ((and a-branch-flag (= len 0))
       (format nil "~a ~a" (string root)
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")))
      (a-branch-flag
       (format nil "~a ~a & ~a" 
	       (string root) 
	       (str-list-2-str-by-str (mapcar #'(lambda (x) (tdl-val-str (car x)))
				       a-branches)
			       " & ")	       
	       (p-2-tdl-aux i branches)))
      ((= len 1)
       (format nil "~a.~a" (string root) (p-2-tdl-2 i (first branches))))
      (t
       (format nil "~a ~a" 
	       (string root) 
	       (p-2-tdl-aux i branches))))))

(defun p-2-tdl-aux (i branches)
  (let ((res))
    (cond
     ((and (setf res (get-tdl-list branches))
	   (every #'(lambda (x) (= (length x) 1)) res))
      (format nil "< ~a >"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     ((and (setf res (get-tdl-diff-list branches))
	   (every #'(lambda (x) (= (length x) 1)) res))
      (format nil "<! ~a !>"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     (t
      (format nil "[ ~a ]"
	      (str-list-2-str-by-str
	       (mapcar (lambda (x) (p-2-tdl-2 i x)) branches)
	       (format nil ",~%~a" (make-string i :initial-element #\ ))))))))

(defun p-2-tdl-2-in-list (i x)
  (if (> (length x) 1)
      (format nil "[ ~a ]" (p-2-tdl-2 i x))
    (tdl-val-str (car x))))


(defun tdl-list-start-p (branches)
    (and
     (= (length branches) 2)
     (find (CAR *LIST-HEAD*) branches :key 'car)
     (find (CAR *LIST-TAIL*) branches :key 'car)
     *empty-list-type*))

(defun tdl-diff-list-start-p (branches)
  (let ((blast))
    (and
     (= (length branches) 2)
     (find *diff-list-list* branches :key 'car)
     (setf blast (find *diff-list-last* branches :key 'car))
     (= (length blast) 2)
     (coindex-p (car (second blast)))
     (car (second blast)))))

(defun get-tdl-list (branches)
  (let* ((bfirst (find (CAR *LIST-HEAD*) branches :key 'car))
	 (brest (find (CAR *LIST-TAIL*) branches :key 'car))
	 (res))
    (when (tdl-list-start-p branches)
      (setf res (get-tdl-list-aux *empty-list-type* (cdr brest)))
      (when (car res)
	(cons (cdr bfirst)
	      (cdr res))))))

(defun get-tdl-diff-list (branches)
  (let* ((blist (find *diff-list-list* branches :key 'car))
	 (end-symb (tdl-diff-list-start-p branches))
	 (res))
    (when end-symb
      (setf res (get-tdl-list-aux end-symb (cdr blist)))
      (when (car res)
	(cdr res)))))

(defun get-tdl-list-aux (end-symb branches)
  (let* ((vfirst (cdr (find (CAR *LIST-HEAD*) branches :key 'car)))
	 (vrest (cdr (find (CAR *LIST-TAIL*) branches :key 'car)))
	 (res))
    (cond
     ((eq (caar branches) end-symb)
      (cons end-symb nil))
     ((null vrest)
      nil)
     ((eq (caar vrest) end-symb)
      (cons end-symb (cons vfirst nil)))
     ((car (setf res (get-tdl-list-aux end-symb vrest)))
      (cons end-symb (cons vfirst (cdr res)))))))

(defun coindex-p (x)
  (and
   (symbolp x)
   (eq (char (symb-2-str x) 0) #\#)))

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
;;; extract grammatical fields
;;;

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

(defun extract-field (x field-kw &optional fields-map)
  (if (stringp field-kw)
      (setf field-kw (str-2-keyword field-kw)))
  (unless (typep x 'lex-entry)
    (error "unexpected type: ~a" x))
  (let* ((fields-map (or fields-map (fields-map *psql-lexicon*)))
	 (mapping (find field-kw fields-map :key #'second :test 'equal)))
    (if mapping
        (extract-field2 x (first mapping) (third mapping) (fourth mapping))
      "")))
	 
(defun extract-field2 (x key2 path2 type)
  (unless (typep x 'lex-entry)
    (error "unexpected type: ~a" x))
  (let* ((key (un-keyword key2))
	 (path (get-path path2)))
    (extract-value-by-path x key path type)))

(defun extract-atom-by-path (x key path)
  (case key
    ('unifs
     (extract-atom-by-path-from-unifs x path))
    (t
     (if path
	 (error "non-unifs key must have null path")
       (slot-value x key)))))
     
(defun extract-raw-list (x key path)
  (if (eq key 'unifs) (error "unifs use different type. eg. fs-list"))
  (if path (error "type requires null path"))
  (let ((val (slot-value x key)))
    (unless (listp val)
      (error "raw list expected"))
    val))
    

(defun extract-atom-by-path-from-unifs (x path)
  (let* ((constraint (slot-value x 'unifs))
	 (unification (find path constraint
                            :key #'extract-key-from-unification 
                            :test #'equal)))
    (when (unification-p unification)
       (setf (slot-value x 'unifs)
	(remove unification constraint))
     (extract-value-from-unification unification)
      )))

;; see also work-out-value
(defun extract-value-by-path (x key path type)
  (case type
    ('mixed
     (encode-mixed-as-str 
      (extract-atom-by-path x key path)))
    ('string
     (encode-string-as-str
      (extract-atom-by-path x key path)))
    ('symbol
     (symb-2-str
      (extract-atom-by-path x key path)))
    ('string-list
     (str-list-2-str
      (extract-raw-list x key path)))
    ('list
     (extract-raw-list x key path))
    ('string-fs
     (str-list-2-str (extract-fs-list x key path)))
    ('string-diff-fs
     (str-list-2-str (extract-fs-diff-list x key path)))
    (T
     (typecase type
       (list
	(case (first type)
	  ('mixed-fs
	   (mixed-list-2-str 
	    (extract-fs-list-complex x key path 
				     :e-path (cdr type))))
	  ('mixed-diff-fs
	   (mixed-list-2-str 
	    (extract-fs-diff-list-complex x key path 
					  :e-path (cdr type))))
	  (t
	   (error "unhandled (list) type: ~a" (first type)))))
       (T 
	(error "unhandled type"))))))
    
(defun extract-fs-list (x key path)
  (extract-fs-list-complex x key path))

(defun extract-fs-list-complex (x key path &key e-path)
  (let ((res (extract-fs-list-complex-aux 
	       (copy-list (slot-value x key)) 
	       path
	       nil
	       :e-path e-path)))
    (cond
     ((listp res)
      (setf (slot-value x key) (cdr res))
      (car res))
     (t
      nil))))

(defun extract-fs-list-complex-aux (unifs path o-list &key e-path)
  (let* (
	 (end-match (find path
			  unifs
			  :key #'extract-key-from-unification
			  :test #'equal))
	 (first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (val (extract-value-from-unification first-match)))
    (cond
     ((and end-match 
	   (eq (extract-value-from-unification end-match)
	       '*NULL*))
      (setf unifs (remove end-match unifs))
      (cons (reverse o-list) unifs))
     ((null val)
      :fail)
     (t
      (setf unifs (remove first-match unifs))
      (extract-fs-list-complex-aux unifs 
			   (append path (list 'REST))
			   (cons val o-list)
			   :e-path e-path)))))

(defun extract-fs-diff-list (x key path)
  (extract-fs-diff-list-complex x key path))
  
(defun extract-fs-diff-list-complex (x key path &key e-path)
  (let* ((unifs (copy-list (slot-value x key)))
	 (last-match 
	  (find (append path (list 'LAST))
		 unifs
		 :key #'extract-key-from-unification
		 :test #'equal))
	 (last-path
	  (and last-match
	       (path-typed-feature-list
		(unification-rhs last-match))))
	 (res 
	  (and last-path
	       (extract-fs-diff-list-complex-aux 
		(remove last-match unifs)
		(append path (list 'LIST))
		nil
		:last last-path
		:e-path e-path))))
    (cond
     ((null last-path)
      nil)
     ((not (listp res))
      nil)
     ((listp res)
      (setf (slot-value x key) (cdr res))
      (car res)))))

(defun extract-fs-diff-list-complex-aux (unifs path o-list &key last e-path)
  (let* ((first-match (find (append path (list 'first) e-path)
			    unifs
			    :key #'extract-key-from-unification
			    :test #'equal))
	 (val (extract-value-from-unification first-match)))
   (cond
    ((equal path
	    last)
     (cons (reverse o-list) unifs))
     ((null val)
      :fail)
     (t
      (setf unifs (remove first-match unifs))
      (extract-fs-diff-list-complex-aux 
       unifs 
       (append path (list 'REST))
       (cons val o-list)
       :last last
       :e-path e-path)))))

(defun get-path (path-str)
  (cond
   ((null path-str)
    nil)
   ((listp path-str)
    path-str)
   ((equal "" path-str)
    nil)
   ((stringp path-str)
    (work-out-value 'list path-str))
   (t
    (error "unhandled value: ~a" path-str))))

(defun get-orthkey (orth-list)
  (string-downcase 
   (or (car (last orth-list))
       "")))

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

(defun dag-diff-list-2-list (dag)
  (let* ((last-dag (dag-path-val (list *diff-list-last*) dag))
	 (list-dag (dag-path-val (list *diff-list-list*) dag)))
    (loop
	with rest-dag
	while (not (eq list-dag
		       last-dag))
	do
	  (setf rest-dag (dag-path-val '(rest) list-dag))
	  (when (null rest-dag)
	    (format t "~%WARNING: invalid difference list ~a in ~a" out-list *current-lex-id*)
	    (loop-finish))
	collect (dag-path-val '(first) list-dag)
	into out-list
	do
	  (setf list-dag rest-dag)
	finally
	  (return out-list)
	  )))

(defun dag-list-2-list (dag)
  (let* ((list-dag dag))
    (loop
	with rest-dag
	while (not (equal (dag-type list-dag)
			  *empty-list-type*))
	do
	  (setf rest-dag (dag-path-val *list-tail* list-dag))
	  (when (null rest-dag)
	    (format t "~%WARNING: invalid list ~a in ~a" out-list dag)
	    (loop-finish))
	collect (dag-path-val *list-head* list-dag)
	into out-list
	do
	  (setf list-dag rest-dag)
	finally
	  (return out-list)
	  )))