;;; Copyright (c) 2001 -- 2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;;
;;; export TDL lexicon for (Access or) Postgres import.
;;;

;;; bmw (jul-03)
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
(defvar *export-counter* 0)
(defvar *export-version* 0)
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
   (mapcar 
    (lambda (x) (append (path-typed-feature-list (unification-lhs x))
			(u-value-type (unification-rhs x))))
    unifs))

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
  (let ((a 
	 (if (cdr x) 
	     (string (car x))
	   ""))
	(b
	 (if (cdr y) 
	     (string (car y))
	   "")))
    (string< a b)))

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
  (let ((res (get-tdl-list branches)))
    (cond
     ((car res)
      (format nil "< ~a >"
	      (str-list-2-str
	       (mapcar 'tdl-val-str (cdr res))
	       ", ")))
     (t
      (format nil "[ ~a ]"
	      (str-list-2-str
	       (mapcar (lambda (x) (p-2-tdl-2 i x)) branches)
	       (concatenate 'string ",~%" (make-string i :initial-element #\ ))))))))

;;; test whether worth trying to represent struct as tdl list
(defun poss-tdl-listp (branches)
  (and
   (= (length branches) 2)
   (equal (car (first branches)) 'FIRST)
   (not (cddr (first branches)))
   (equal (car (second branches)) 'REST)
   (or (cddr (second branches))
       (equal (caadr (second branches)) '*NULL*))))

;;; obtain tdl list, if poss
;;; -> ( flag . tdl-list) 
(defun get-tdl-list (branches)
  (let ((rest-branch (cdr (second branches)))
	(first-val (car (second (first branches))))
	(res))
    (when 
	(and
	 branches
	 (poss-tdl-listp branches))
      (cond 
       ((equal rest-branch '((*NULL*)))
	(cons t (list first-val)))
       (t
	(setf res (get-tdl-list rest-branch))
	(when
	    (car res)
	  (cons t (cons first-val (cdr res)))))))))

(defmethod export-to-cvs ((lexicon lex-database) stream)
  (mapc
   #'(lambda (x) (format stream "~a" (to-cvs (read-psort lexicon x :recurse nil))))
   (collect-psort-ids lexicon :recurse nil)))

(defmethod export-to-csv-to-file ((lexicon lex-database) filename)
  (with-open-file 
      (ostream filename :direction :output :if-exists :supersede)
    (export-to-cvs lexicon ostream)))

(defmethod to-cvs ((x lex-or-psort))
  (let* ((name (lex-or-psort-id x))
	 (unifs (lex-or-psort-unifs x))
	 (separator (string *export-separator*))
	 (type (extract-type-from-unifications unifs))
	 (temp (extract-stem-from-unifications unifs))
	 (stem (cdr temp))
	 (count (car temp))
	 (keyrel (extract-key-from-unifications unifs))      
	 (keytag (extract-tag-from-unifications unifs))
	 (altkey (extract-altkey-from-unifications unifs))
	 (alt2key (extract-alt2key-from-unifications unifs))
	 (compkey (extract-comp-from-unifications unifs))
	 (ocompkey (extract-ocomp-from-unifications unifs))
	 (total (+ count 1 
		   (if keyrel 1 0) (if keytag 1 0) (if altkey 1 0)
		   (if alt2key 1 0) (if compkey 1 0) (if ocompkey 1 0))))
    (cond 
     ((= total (length unifs))
      (format nil "~a~%"
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
		separator ;;jlink
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
		separator *export-version* ;;version
		separator (or *current-source* "?") ;;source
		separator "1" ;;flags: 1 = not deleted
		separator *current-user* ;;userid
		separator (or *export-timestamp* "") ;;modstamp
		)))
     (t
      (format *export-skip-stream* "~%skipping super-rich entry: `~a'"  name)
      ""))))

(defmethod export-to-db ((lexicon lex-database) output-lexicon)
  (mapc
   #'(lambda (x) (to-db (read-psort lexicon x :recurse nil) output-lexicon))
   (collect-psort-ids lexicon :recurse nil)))

(defmethod to-db ((x lex-or-psort) output-lexicon)
  (let* ((name (lex-or-psort-id x))
	 (unifs (lex-or-psort-unifs x)))
    (set-lexical-entry output-lexicon nil name unifs)))
