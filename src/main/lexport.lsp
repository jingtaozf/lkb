;;; Copyright (c) 2001 -- 2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
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

(defvar *export-lexicon-p* nil)
(defvar *export-to* nil)

(defvar *postgres-temp-filename* nil)
(defvar *export-file* (make-pathname :name "lexicon"
				     :directory (pathname-directory (lkb-tmp-dir))))
(defvar *export-skip-stream* t)
(defvar *export-separator* #\,)
(defvar *export-version* 0)
(defvar *export-timestamp* nil)

(defvar *export-output-lexicon* nil)
(defvar common-lisp-user::*grammar-version*)
(defvar *grammar-version*)
(defvar *export-default-fields-map*
    '((:ID "name" "" "symbol") 
      (:ORTH "orthography" "" "string-list") 
      ;;(:SENSE-ID "name" "" "symbol")
      (:UNIFS "alt2key" "(synsem local keys alt2key)" "symbol")
      (:UNIFS "altkey" "(synsem local keys altkey)" "symbol")
      (:UNIFS "altkeytag" "(synsem local keys altkey carg)" "string")
      (:UNIFS "compkey" "(synsem lkeys --compkey)" "symbol")
      (:UNIFS "keyrel" "(synsem local keys key)" "symbol")
      (:UNIFS "keytag" "(synsem local keys key carg)" "string")
      (:UNIFS "ocompkey" "(synsem lkeys --ocompkey)" "symbol")
      ;;(:UNIFS "orthography" "(stem)" "string-fs") 
      (:UNIFS "type" "nil" "symbol")))

(defvar *export-multi-separately* nil)
(defvar *export-multi-stream* t)

(defvar *user-temp-dir* (make-pathname :directory (pathname-directory (lkb-tmp-dir))))

(defun get-postgres-temp-filename nil
  (setf *postgres-temp-filename*
    (format nil "~a.~a" "/tmp/postgres-temp" (sys:user-name))))

(defun export-lexicon (&rest rest)
  (apply 'export-lexicon-to-file rest))

(defun export-lexicon-to-file (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-file2 rest)))

(defun export-lexicon-to-file2 (&key (file *export-file*) 
				    (separator *export-separator*)
				     (lexicon *lexicon*))
  (setf file (namestring (pathname file)))
  
  (setf *export-file* file)
  (setf *export-separator* separator)

  ;;; extra data in db entries
  (setf *current-source* (get-current-source))
  (setf *export-timestamp* (extract-date-from-source *current-source*))
  
  (setf *current-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" (or (extract-pure-source-from-source *current-source*) ""))))
  (unless *current-source* (throw 'abort 'source))
  
  (setf *export-timestamp* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Modstamp?" (or *export-timestamp* "1990-01-01"))))
  (unless *export-timestamp* (throw 'abort 'modstamp))
  
  (setf *current-lang* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Language code?" (or *current-lang* "EN"))))
  (unless *current-lang* (throw 'abort 'lang))
  
  (setf *current-country* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Country code?" (or *current-country* "UK"))))
  (unless *current-country* (throw 'abort 'country))
  
  (setf *current-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" (or *current-user* "danf"))))
  (unless *current-user* (throw 'abort 'user))
  
  (get-export-version) 

  (let ((csv-file (format nil "~a.csv" file))
	(skip-file (format nil "~a.skip" file))
	(multi-file (format nil "~a.multi.csv" file)))
    (format t "~%Exporting lexicon to CSV file ~a" csv-file)
    (format t "~%   (skip file: ~a)" skip-file)
    
    (if *export-multi-separately*
	;; set multi stream
	(with-open-file (*export-multi-stream* 
			 multi-file
			 :direction :output 
			 :if-exists :supersede :if-does-not-exist :create)
	  (with-open-file (*export-skip-stream* 
			   skip-file
			   :direction :output 
			   :if-exists :supersede :if-does-not-exist :create)
	    (format t "~%   (multi file: ~a)" multi-file)
	    (export-to-csv-to-file lexicon csv-file)))	  
      ;; no multi stream
      (with-open-file (*export-skip-stream* 
		       skip-file
		       :direction :output 
		       :if-exists :supersede :if-does-not-exist :create)
	(export-to-csv-to-file lexicon csv-file))))
  (format t "~%Export complete"))

(defun get-export-version nil
  (let ((old-val *export-version*)
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
    (setf *export-version* new-val)))
  
(defun ask-user-for-x (head promptDcons)
  (car (ask-for-strings-movable head 
			   (list promptDcons))))

(defun export-lexicon-to-db (&rest rest)
  (catch 'abort 
    (apply 'export-lexicon-to-db2 rest)))

 ;;; implemented for postgres
(defun export-lexicon-to-db2 (&key (output-lexicon *export-output-lexicon*)
				  (lexicon *lexicon*))
  (unless
      (and output-lexicon (connection output-lexicon))
    (setf output-lexicon (initialize-psql-lexicon)))

  (setf *export-output-lexicon* output-lexicon)
  
  (setf *current-source* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Source?" *current-source*)))
  (unless *current-source* (throw 'abort 'source))
  
  (setf *export-timestamp* "NOW") ;; fixme?
  
    (setf *current-lang* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Language code?" (or *current-lang* "EN"))))
    (unless *current-lang* (throw 'abort 'lang))
    
    (setf *current-country* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Country code?" (or *current-country* "UK"))))
    (unless *current-country* (throw 'abort 'country))
    
  (format t 
	  "~%Exporting lexical entries to ~a" 
	  (dbname output-lexicon))  
  (export-to-db lexicon output-lexicon)
  (format t "~%Export complete"))
  
(defun export-lexicon-to-tdl (&key (file *export-file*)
				   (lexicon *lexicon*))
  (setf file (namestring (pathname file)))
  (let ((tdl-file (format nil "~a.tdl" file)))
    (if (equal (subseq file (- (length file) 4)) ".tdl")
	(setf tdl-file file))
    (format t "~%Exporting lexicon to TDL file ~a" tdl-file)
    (export-to-tdl-to-file lexicon tdl-file))
  (format t "~%Export complete~%"))

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
    (error "unexpected type"))
  (let* ((fields-map (or fields-map (fields-map *psql-lexicon*)))
	 (mapping (find field-kw fields-map :key #'second :test 'equal)))
    (if mapping
        (extract-field2 x (first mapping) (third mapping) (fourth mapping))
      "")))
	 
(defun extract-field2 (x key2 path2 type2)
  (unless (typep x 'lex-entry)
    (error "unexpected type"))
  (let* ((key (un-keyword key2))
	 (path (get-path path2))
	 (type (2-symb type2))
	 (x-key (slot-value x key)))
    (extract-value-by-path x-key path type)))

(defun extract-value-by-path (x path type)
  (extracted-val-2-str
   (cond
    ((and (listp x) (every #'(lambda (y) (typep y 'unification)) x))
     (extract-value-by-path-from-unifications x path))
    ((null path)
     x)
    (t
     (error "unhandled input")))
   :type type))

(defun extracted-val-2-str (val &key (type 'STRING))
  (cond
   ((eq type 'MIXED)
    (encode-mixed-as-str val))
   ((eq type 'STRING)
    (encode-string-as-str val))
   ((eq type 'SYMBOL)
    (symb-2-str val))
   ((eq type 'STRING-LIST)
    (str-list-2-str val))
   ((eq type 'STRING-FS)
    (error "unhandled type"))
   ((eq type 'STRING-DIFF-FS)
    (error "unhandled type"))
   ((eq type 'LIST)
    (if (listp val)
	val
      (list val)))
   (t
    (error "unhandled type"))))


(defun get-path (path-str)
  (cond
   ((null path-str)
    nil)
   ((listp path-str)
    path-str)
   ((equal "" path-str)
    nil)
   ((stringp path-str)
    (work-out-value "list" path-str))
   (t
    (error "unhandled value"))))

(defun un-keyword (keyword-symb)
  (str-2-symb (symb-2-str keyword-symb)))

(defun extract-value-by-path-from-unifications (constraint path)
  (let* ((unification (find path constraint
                            :key #'extract-key-from-unification 
                            :test #'equal)))
    (when (unification-p unification)
      (extract-value-from-unification unification))))

(defun tdl-val-str (symb)
  (cond
   ((null symb) "")
   ((numberp symb) (num-2-str symb))
   ((stringp symb) (format nil "~S" symb))
   (t (string-downcase (string symb)))))
    
;;;
;;; tdl export (packed)
;;;

(defun pack-unifs (unifs)
  (pack 
   (unifs-2-list unifs)))

(defun unifs-2-list (unifs)
  (let ((c 0)
	(coindex nil))
    (mapcan 
     #'(lambda (unif)
	 (with-slots (rhs lhs) unif
	   (cond
	    ((typep rhs 'U-VALUE)
	     (list (append (path-typed-feature-list lhs)
			   (u-value-type rhs))))
	    ((typep rhs 'PATH)
	     (incf c)
	     (setf coindex (str-2-symb (format nil "#~a" c)))
	     ;(setf coindex (str-2-symb (format nil "\\#~a" c)))
	     (list
	      (append (path-typed-feature-list lhs)
		      coindex)
		      ;'\#1)
	      (append (path-typed-feature-list rhs)
		      coindex))))))
		      ;'\#1))))))
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
  (mapc
   #'(lambda (x) (format stream "~a" (to-tdl (read-psort lexicon x))))
   (collect-psort-ids lexicon)))

(defmethod export-to-tdl-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

(defmethod to-tdl ((x lex-entry))
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str (lex-entry-id x))
   (p-2-tdl (pack-unifs (lex-entry-unifs x)))))
	  
;; copy of p-2-tdl w/o root
(defun p-2-tdl (branches)
  (unless branches
    (error "non-null value expected"))
  (let* ((a-branch-flag (not (cdr (first branches))))
	 (a-branch)
	 (len)
	 (i 0)
	 )
    (when a-branch-flag
      (setf a-branch (pop branches)))
    (setf len (length branches))
    
     (cond
      ((and a-branch-flag (= len 0))
       (format nil "~a" (tdl-val-str (car a-branch))))
      (a-branch-flag
       (format nil "~a &~%~a ~a"  
	       (tdl-val-str (car a-branch))
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
	 (a-branch)
	 (len))
  (setf i (+ i 3 (length (string root))))
    (when a-branch-flag
      (setf a-branch (pop branches)))
    (setf len (length branches))
     (cond
      ((and a-branch-flag (= len 0))
       (format nil "~a ~a" (string root) (tdl-val-str (car a-branch))))
      (a-branch-flag
       (format nil "~a ~a & ~a" 
	       (string root) 
	       (tdl-val-str (car a-branch))
	       ;(make-string i :initial-element #\ )
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
	      (str-list-2-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     ((and (setf res (get-tdl-diff-list branches))
	   (every #'(lambda (x) (= (length x) 1)) res))
      (format nil "<! ~a !>"
	      (str-list-2-str
	       (mapcar (lambda (x) (p-2-tdl-2-in-list i (car x))) res)
	       ", ")))
     (t
      (format nil "[ ~a ]"
	      (str-list-2-str
	       (mapcar (lambda (x) (p-2-tdl-2 i x)) branches)
	       ;(concatenate 'string ",~%" (make-string i :initial-element #\ ))))))))
	       (format nil ",~%~a" (make-string i :initial-element #\ ))))))))

(defun p-2-tdl-2-in-list (i x)
  (if (> (length x) 1)
      (format nil "[ ~a ]" (p-2-tdl-2 i x))
    (tdl-val-str (car x))))


(defun tdl-list-start-p (branches)
    (and
     (= (length branches) 2)
     (find 'FIRST branches :key 'car)
     (find 'REST branches :key 'car)
     '*NULL*))

(defun tdl-diff-list-start-p (branches)
  (let ((blast))
    (and
     (= (length branches) 2)
     (find 'LIST branches :key 'car)
     (setf blast (find 'LAST branches :key 'car))
     (= (length blast) 2)
     (coindex-p (car (second blast)))
     (car (second blast)))))

(defun get-tdl-list (branches)
  (let* ((bfirst (find 'FIRST branches :key 'car))
	 (brest (find 'REST branches :key 'car))
	 (res))
    (when (tdl-list-start-p branches)
      (setf res (get-tdl-list-aux '*NULL* (cdr brest)))
      (when (car res)
	(cons (cdr bfirst)
	      (cdr res))))))

(defun get-tdl-diff-list (branches)
  (let* ((blist (find 'LIST branches :key 'car))
	 (end-symb (tdl-diff-list-start-p branches))
	 (res))
    (when end-symb
      (setf res (get-tdl-list-aux end-symb (cdr blist)))
      (when (car res)
	(cdr res)))))

(defun get-tdl-list-aux (end-symb branches)
  (let* ((vfirst (cdr (find 'FIRST branches :key 'car)))
	 (vrest (cdr (find 'REST branches :key 'car)))
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

(defmethod export-to-csv ((lexicon lex-database) stream)
  (let ((fields-map
	 (and *psql-lexicon* (fields-map *psql-lexicon*))))
    (unless fields-map
      (format t 
	      "~%WARNING: using default fields map *export-default-fields-map*")
      (setf fields-map *export-default-fields-map*))
    (format t "~%Export fields map:~%~a~%" fields-map)
    (mapc 
     #'(lambda (x) 
	 (format stream "~a" (to-csv (read-psort lexicon x :recurse nil) fields-map)))
     (collect-psort-ids lexicon :recurse nil))))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (setf filename (namestring (pathname filename)))
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-csv lexicon ostream)))

(defmethod to-csv ((x lex-entry) fields-map)
  (let* (
	 ;;(separator (string *export-separator*))

	 (keyrel (extract-field x :keyrel fields-map))      
	 (keytag (extract-field x :keytag fields-map))
	 (altkey (extract-field x :altkey fields-map))
	 (altkeytag (extract-field x :altkeytag fields-map))
	 (alt2key (extract-field x :alt2key fields-map))
	 (compkey (extract-field x :compkey fields-map))
	 (ocompkey (extract-field x :ocompkey fields-map))
	 
	 (type (extract-field x :type fields-map))
	 (orth-list (extract-field2 x :orth nil "list"))
	 (orthography (extract-field x :orthography fields-map))
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
	 
	 (multi-base-name (and *export-multi-separately* (multi-p :name name :type type)))
	 (line 
	  (format nil "~a~%"
		  (csv-line
		   name
		   *current-user* ;;userid
		   (num-2-str *export-version*) ;;version
		   *export-timestamp* ;;modstamp
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
		   *current-lang* ;;lang
		   *current-country* ;;country
		   "" ;;dialect
		   "" ;;domains
		   "" ;;genres
		   "" ;;register
		   "1";;confidence
		   (or *current-source* "?") ;;source
		   "1" ;;flags: 1 = not deleted
		   ))))
    (cond 
     ((= total (length (lex-entry-unifs x)))
      (if multi-base-name
	  (to-multi-csv-line :name name
			     :base-name multi-base-name
			     :particle compkey
			     :type type
			     :keyrel keyrel)
      line))
     (t
      (format *export-skip-stream* "~%skipping super-rich entry: ~a"  line)
      ""))))

(defun csv-line (&rest str-list)
  (str-list-2-str
   (mapcar #'csv-escape str-list)
   ","))

(defun csv-escape (str &optional (sep-char *export-separator*))
  (let ((l))
    (do ((i (1- (length str)) (1- i)))
	((< i 0))
      (push (aref str i) l)
      (if (eq (aref str i) sep-char)
	  (push #\\ l)))
    (concatenate 'string l)))
	  
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
    (error "unhandled type"))))

(defun encode-string-as-str (val)
  (cond
   ((null val)
    "")
   ((stringp val)
    (format nil "\"~a\"" val))
   (t
    (error "unhandled type"))))

(defmethod to-multi-csv-line (&key name base-name particle type keyrel)
  (let ((separator (string *export-separator*)))
    (format *export-multi-stream* "~a~%"
	    (concatenate 'string
	      name
	      separator base-name
	      separator particle
	      separator type
	      separator keyrel))
    ""))

(defun subseq-from-end (seq rev-end &optional (rev-start 0))
  (let* ((len (length seq))
	 (start (- len rev-end))
	 (end (- len rev-start)))
    (subseq seq start end)))

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

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x :recurse nil) output-lexicon))
   (collect-psort-ids lexicon :recurse nil))
  (build-current-grammar *psql-lexicon*)
  ;;(fn-get-records output-lexicon ''initialize-current-grammar (get-filter output-lexicon))
  )

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
	   :country *current-country*
	   :lang *current-lang*
	   :source (extract-pure-source-from-source *current-source*)
	   :confidence 1
	   )
	  ))
    (cond
     ((= total (length (lex-entry-unifs x)))
      (set-lex-entry lexicon psql-le)
      (empty-cache lexicon))
     (t
      (format t "~%skipping super-rich entry: `~a'~%"  name)
      nil))))

(defun get-orthkey (orth-list)
  (car (last orth-list)))
(defun get-current-source nil
  (cond
   ((boundp '*grammar-version*)
    *grammar-version*)
   ((boundp 'common-lisp-user::*grammar-version*)
    common-lisp-user::*grammar-version*)
   (t
    (format t "WARNING: no *GRAMMAR-VERSION* defined!"))))

(defun merge-tdl-into-psql-lexicon2 (file-in)
  (setf file-in (namestring (pathname file-in)))
  (let ((tmp-lex (create-empty-cdb-lex))
	(file-out (get-new-filename (make-pathname :name "lexicon"
                     :directory (pathname-directory (lkb-tmp-dir))))))
    (unless (probe-file file-in)
      (error "~%file not found (~a)" file-in))
    (load-lex tmp-lex :filename file-in)
    (export-lexicon-to-file :lexicon tmp-lex :file file-out)
    (merge-into-psql-lexicon (namestring (probe-file (format nil "~a.csv" file-out))))))

(defun merge-tdl-into-psql-lexicon (file-in)
  (setf file-in (namestring (pathname file-in)))
  (let ((tmp-lex (create-empty-cdb-lex)))
  (unless (probe-file file-in)
    (error "~%file not found (~a)" file-in))
  (load-lex tmp-lex :filename file-in)
  (export-lexicon-to-db :lexicon tmp-lex :output-lexicon *psql-lexicon*)
  (clear-lex tmp-lex)))

;; assumes *lexicon* eq *psql-lexicon*
(defun load-scratch-lex (&key filename)
  (let ((lexicon (create-empty-cdb-lex)))
    (load-lex lexicon :filename filename)
    lexicon))

(defun build-current-grammar (lexicon)
  (fn-get-records  lexicon ''build-current-grammar)
  (empty-cache lexicon))
  

(defun clear-scratch-lex nil
  (fn-get-val *psql-lexicon* ''clear-scratch)
  (build-current-grammar *psql-lexicon*)
  ;;  (fn-get-records  *psql-lexicon* ''build-current-grammar)
;;  (empty-cache *psql-lexicon*)
  )

(defun clear-scratch2-lex nil
  (format t "~%clearing scratch")
  (time (fn-get-records *psql-lexicon* ''clear-scratch2))
  (format t "~%clustering current_grammar")
  (time (fn-get-records *psql-lexicon* ''cluster-current-grammar))
  (empty-cache *psql-lexicon*)
  )

(defun commit-scratch-lex nil
  (fn-get-val *psql-lexicon* ''commit-scratch)
  (empty-cache *psql-lexicon*)
  )

