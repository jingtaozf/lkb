;;; Copyright (c) 2001 -- 2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; export lexicon in various formats
;;;

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

(defvar *export-file* "/tmp/lexicon")
(defvar *export-skip-stream* t)
(defvar *export-separator* #\,)
;;(defvar *export-counter* 0)
(defvar *export-version* "0")
(defvar *export-timestamp* nil)

(defvar *export-output-lexicon* nil)
(defvar common-lisp-user::*grammar-version*)


;;; export prev loaded grammar to (psql) db or to file
(defun export-lexicon nil
  (export-lexicon-to-file))
  
(defun export-lexicon-to-file (&key (file *export-file*) 
                            (separator *export-separator*))
  (setf *export-file* file)
  (setf *export-separator* separator)

  ;;; extra data reqd in db
  (setf *current-source* common-lisp-user::*grammar-version*)
  (setf *export-timestamp* (extract-date-from-source *current-source*))
  (unless *current-lang* 
    (setf *current-lang* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Language code?" "EN"))))
  (unless *current-country* 
    (setf *current-country* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Country code?" "US"))))
  (setf *current-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" "guest")))
  (setf *export-version* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Version?" "0")))
  (setf *export-version* 
    (symb-2-str (str-2-num *export-version*)))
  
  (let ((csv-file (format nil "~a.csv" file))
	(skip-file (format nil "~a.skip" file)))
    (format *trace-output* "~%Exporting lexicon to CSV file ~a" csv-file)
    (with-open-file (*export-skip-stream* 
		     skip-file
		     :direction :output 
		     :if-exists :supersede :if-does-not-exist :create)
      (export-to-csv-to-file *lexicon* csv-file))))
    
(defun ask-user-for-x (head promptDcons)
  (car (ask-for-strings-movable head 
			   (list promptDcons))))

 ;;; implemented for postgres
(defun export-lexicon-to-db (&key (output-lexicon *export-output-lexicon*))
  (unless
      (and output-lexicon (connection output-lexicon))
    (setf output-lexicon (initialize-psql-lexicon)))
  
  (setf *export-output-lexicon* output-lexicon)
  (setf *current-source* common-lisp-user::*grammar-version*)
  (setf *export-timestamp* (extract-date-from-source *current-source*))
  (unless *current-lang* 
    (setf *current-lang* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Language code?" "EN"))))
  (unless *current-country* 
    (setf *current-country* 
      (ask-user-for-x 
       "Export Lexicon" 
       (cons "Country code?" "US"))))
  (setf *current-user* 
    (ask-user-for-x 
     "Export Lexicon" 
     (cons "Username?" "guest")))
  
  (format *trace-output* 
	  "~%Exporting lexicon to DB ~a" 
	  (dbname output-lexicon))  
  (export-to-db *lexicon* output-lexicon))
  
(defun export-lexicon-to-tdl (&key (file *export-file*) )
  (let ((tdl-file (format nil "~a.tdl" file)))
    (format *trace-output* "~%Exporting lexicon to TDL file ~a" tdl-file)
    (export-to-tdl-to-file *lexicon* tdl-file)))

(defun extract-date-from-source (source)
  (subseq source (1+ (position #\( source :test #'equal)) (position #\) source :test #'equal)))
				
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

(defun extract-field (x field-str &optional field-map)
  (unless (typep x 'lex-or-psort)
    (error "unexpected type"))
  (let* ((field-map (or field-map (fields-map *psql-lexicon*)))
	 (mapping (find field-str field-map :key #'second :test 'equal)))
    (extract-field2 x (first mapping) (third mapping) (fourth mapping))))
	 

(defun extract-field2 (x key2 path2 type2)
  (unless (typep x 'lex-or-psort)
    (error "unexpected type"))
  (let* ((key (un-keyword key2))
	 (path (get-path path2))
	 (type (str-2-symb type2))
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
   ((eq type 'STRING)
    (symb-2-str val))
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
    (work-out-value nil "list" path-str))
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

;;;
;;; tdl export (unpacked)
;;;

(defmethod to-unpacked-tdl ((unif unification))
  (let ((lhs (unification-lhs unif))
	(rhs (unification-rhs unif)))
  (format nil "~a ~a"
    (str-list-2-str (path-typed-feature-list lhs) ".")
    (tdl-val-str (u-value-type rhs)))))

(defun unifs-to-unpacked-tdl (unifs)
  (format 
   nil "~a &~%  [ ~a ]."
   (str-list-2-str 
    (mapcar 'to-unpacked-tdl
	    (remove-if (lambda (x) (path-typed-feature-list (unification-lhs x))) 
		       unifs))
    "&~%    ")
   (str-list-2-str 
    (mapcar 'to-unpacked-tdl
	    (remove-if-not (lambda (x) (path-typed-feature-list (unification-lhs x))) 
			   unifs))
    ",~%    ")))

(defmethod to-unpacked-tdl ((x lex-or-psort))
  (format 
   nil "~a := ~a~%~%"
   (tdl-val-str (lex-or-psort-id x))
   (unifs-to-unpacked-tdl (lex-or-psort-unifs x))))
	  
(defmethod export-to-unpacked-tdl ((lexicon lex-database) stream)
  (mapc
   #'(lambda (x) (format stream "~a" (to-unpacked-tdl (read-psort lexicon x))))
   (collect-psort-ids lexicon)))

(defmethod export-to-unpacked-tdl-to-file ((lexicon lex-database) filename)
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-unpacked-tdl lexicon ostream)))
 
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
	     (setf coindex (str-2-symb (format nil "\\#~a" c)))
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
    ;(cond
     ;((and (eq (car x) 'LIST) (eq (car y) 'LAST))
     ; t)
     ;(t
      (string< a b)
      ;))
    ))

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
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-tdl lexicon ostream)))

(defmethod to-tdl ((x lex-or-psort))
  (format 
   nil "~%~a := ~a.~%"
   (tdl-val-str (lex-or-psort-id x))
   (p-2-tdl (pack-unifs (lex-or-psort-unifs x)))))
	  
;; copy of p-2-tdl w/o root
(defun p-2-tdl (branches)
  (unless branches
    (error "internal"))
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
	 (len)
	 )
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
	       (concatenate 'string ",~%" (make-string i :initial-element #\ ))))))))

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
  (mapc
   #'(lambda (x) (format stream "~a" (to-csv (read-psort lexicon x :recurse nil))))
   (collect-psort-ids lexicon :recurse nil)))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-csv lexicon ostream)))

(defmethod to-csv ((x lex-or-psort) 
		   &key 
		   (field-map
		    '((:ID "name" "" "symbol") 
		      (:ORTH "orthography" "" "string-list") 
					;(:SENSE-ID "name" "" "symbol")
		      (:UNIFS "alt2key" "(synsem local keys alt2key)" "symbol")
		      (:UNIFS "altkey" "(synsem local keys altkey)" "symbol")
		      (:UNIFS "compkey" "(synsem lkeys --compkey)" "symbol")
		      (:UNIFS "keyrel" "(synsem local keys key)" "symbol")
		      (:UNIFS "keytag" "(synsem local keys key carg)" "string")
		      (:UNIFS "ocompkey" "(synsem lkeys --ocompkey)" "symbol")
					;(:UNIFS "orthography" "(stem)" "string-fs") 
		      (:UNIFS "type" "nil" "symbol"))))
  (let* ((separator (string *export-separator*))

	 (keyrel (extract-field x "keyrel" field-map))      
	 (keytag (extract-field x "keytag" field-map))
	 (altkey (extract-field x "altkey" field-map))
	 (alt2key (extract-field x "alt2key" field-map))
	 (compkey (extract-field x "compkey" field-map))
	 (ocompkey (extract-field x "ocompkey" field-map))
	 
	 (orth-list (extract-field2 x :orth nil "list"))
	 (name (extract-field x "name" field-map))
	 (count (+ 2 (length orth-list)))
	 (total (+ count
		   (if (string> keyrel "") 1 0) 
		   (if (string> keytag "") 1 0) 
		   (if (string> altkey "") 1 0)
		   (if (string> alt2key "") 1 0) 
		   (if (string> compkey "") 1 0) 
		   (if (string> ocompkey "") 1 0))))
    (cond 
     ((= total (length (lex-or-psort-unifs x)))
      (format nil "~a~%"
	      (concatenate 'string
		name
		separator (extract-field x "type" field-map)
		separator (extract-field x "orthography" field-map) ;:todo: comma in word?
		separator (first orth-list)
		separator  ;;pronunciation
		separator keyrel
		separator altkey
		separator alt2key
		separator keytag
		separator compkey
		separator ocompkey
		separator ;;complete
		separator ;;semclasses
		separator ;;preferences
		separator ;;classifier
		separator ;;selectrest
		separator ;;jlink
		separator ;;comments
		separator ;;exemplars
		separator ;;usages
		separator *current-lang* ;;lang
		separator *current-country* ;;country
		separator ;;dialect
		separator ;;domains
		separator ;;genres
		separator ;;register
		separator "1";;confidence
		separator (symb-2-str *export-version*) ;;version
		separator (or *current-source* "?") ;;source
		separator "1" ;;flags: 1 = not deleted
		separator *current-user* ;;userid
		separator *export-timestamp* ;;modstamp
		)))
     (t
      (format *export-skip-stream* "~%skipping super-rich entry: `~a'"  name)
      ""))))

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x :recurse nil) output-lexicon))
   (collect-psort-ids lexicon :recurse nil))
  (fn-get-records output-lexicon ''initialize-current-grammar))

(defmethod to-db ((x lex-or-psort) (lexicon psql-lex-database))  
  (let* ((field-map (fields-map lexicon))

	 (keyrel (extract-field x "keyrel" field-map))      
	 (keytag (extract-field x "keytag" field-map))
	 (altkey (extract-field x "altkey" field-map))
	 (alt2key (extract-field x "alt2key" field-map))
	 (compkey (extract-field x "compkey" field-map))
	 (ocompkey (extract-field x "ocompkey" field-map))
	 
	 (orth-list (extract-field2 x :orth nil "list"))
	 (name (extract-field x "name" field-map))
	 (count (+ 2 (length orth-list)))
	 (total (+ count
		   (if (string> keyrel "") 1 0) 
		   (if (string> keytag "") 1 0) 
		   (if (string> altkey "") 1 0)
		   (if (string> alt2key "") 1 0) 
		   (if (string> compkey "") 1 0) 
		   (if (string> ocompkey "") 1 0)))
	 
	 (type (extract-field x "type" field-map))
	 
	 (psql-le
	  (make-instance 'psql-lex-entry
	    :name name
	    :type type
	    :orthography orth-list		;list
	    :orthkey (first orth-list)
	    :keyrel keyrel
	    :altkey altkey
	    :alt2key alt2key
	    :keytag keytag
	    :compkey compkey
	    :ocompkey ocompkey
	    :country *current-country*
	    :lang *current-lang*
	    :source *current-source*
	    :flags 1)))
    (cond
     ((= total (length (lex-or-psort-unifs x)))
      (set-lex-entry lexicon psql-le))
     (t
      (format *trace-output* "~%skipping super-rich entry: `~a'~%"  name)
      nil))))
