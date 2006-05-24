;;; Copyright (c) 1991--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; bmw (aug-03)
;;; - fixed code broken by *lexicon*-related changes

;;; aac (aug-03)
;;; - moved old flat file stuff to slex.lsp - added to cvs
;;;   but not loaded by lkb.system
;;; - removed `other' entries from *lexicon*
;;;   (code for handling them is either here or in 
;;;   specific files)

(in-package :lkb)
	   
;;; Lexical entries, storage etc

;;; Lexical entries are indexed by orthography (string)
;;; They are also all indexed by an identifier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *lex-database-default-extra-mode* :union)

(defvar mrs::*initial-semantics-path*)

(defvar *lexicon-in*)
(defvar *verbose-lex-lookup-word* nil)
(defvar *lexdb*)
(defvar *postgres-mwe-enable*)
(defvar *lexicon-lexical-entries-cache-p* t)

(defclass lex-database () 
  ((lexical-entries :initform (make-hash-table :test #'equal))
      ;;; cache results of lookup-word
      ;;; :empty records cases where lookup-word returned nil
      ;;; (to distinguish from cases where value not cached)
   (psorts :initform (make-hash-table :test #'eq))
      ;;; cache results of read-psort
      ;;; :empty records cases where read-psort returned nil
      ;;; (to distinguish from cases where value not cached)
   (extra-lexicons :initform nil :accessor extra-lexicons)
   (extra-mode :initform *lex-database-default-extra-mode* :accessor extra-mode)
   (part-of :initform nil :accessor part-of)
   (invalid-p :initform t :accessor invalid-p)
   (cache-lex-list :initform nil :accessor cache-lex-list)
   (name :initform nil :accessor name)
   ))

(defmethod link ((sub-lexicon lex-database) (lexicon lex-database))
  (if (eq sub-lexicon lexicon)
      (error "cannot link a lexicon to itself!"))
  (with-slots (extra-lexicons) lexicon
    (with-slots (part-of) sub-lexicon
      (unless 
	  (and
	   (member sub-lexicon extra-lexicons)
	   (member lexicon part-of))
	(push lexicon part-of)
	(push sub-lexicon extra-lexicons))))
  lexicon)

(defmethod unlink ((sub-lexicon lex-database) (lexicon lex-database))
  (with-slots (extra-lexicons) lexicon
    (with-slots (part-of) sub-lexicon
      (setf part-of (remove lexicon part-of))
      (setf extra-lexicons (remove sub-lexicon extra-lexicons))))
  lexicon)

;;;
;;; given the orthography (string in all upper case), look up all lexical
;;; entries that _contain_ that orthography; return list of identifiers.
;;;
(defgeneric lookup-word (lexicon orth &key (cache)))

;;;
;;; for a given lexicon, return all words, i.e. strings of orthography that
;;; occur as part of the spelling of a lexical item; returns "ad" and "hoc" as
;;; two separate `words' but not "ad hoc".
;;; 
(defgeneric lex-words (lexicon))

(defgeneric lexicon-loaded-p (lexicon))

(defgeneric read-cached-lex (lexicon filenames))

(defgeneric store-cached-lex (lexicon))

(defgeneric set-lexical-entry (lexicon orth id new-entry &key cache))

(defgeneric close-lex (lexicon &key in-isolation delete))

(defgeneric collect-expanded-lex-ids (lexicon))

(defgeneric store-psort (lexicon id entry &optional orth))

(defgeneric read-psort (lexicon id &key cache recurse new-instance))

(defgeneric unexpand-psort (lexicon id))

;;;
;;; for a given lexicon, return list of psort identifiers (as strings), i.e.
;;; identifiers of lexical items plus start symbols and node labels (for the
;;; time being, these are stored in the same namespace).
;;;
(defgeneric collect-psort-ids (lexicon &key cache recurse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This isn't getting used correctly, since it's not preserved in the
;; lexical cache.  But it is only used for outputting source
;; files in other syntaxes and batch checking, where 
;; it is acceptable to have to read in a fresh lexicon

(defvar *ordered-lex-list* nil)

(defparameter *batch-mode* nil
   "set when indexing to prevent errors in expanding a lexical entry being
   signalled as continuable errors, rather than written
   to a file.")

(defstruct (psort)
   id 
   unifs
   def-unifs
   full-fs
   language)

(defstruct (lex-entry (:include psort)) 
   orth
   infl-pos ; for a multi-word entry only - a number
            ; indicating the element that can be inflected
            ; or NIL - no inflection
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Database-independent lexical access functions
;;;

(defun get-lex-entry-from-id (id &key (cache t))
  ;;; now lexical entries only
  (let ((entry (get-unexpanded-psort-entry id :cache cache)))
    ;; if we haven't previously expanded then
    ;; destructively modify entry
    (when entry
      (cond ((eql (lex-entry-full-fs entry) :fail) nil)
	    ((lex-entry-full-fs entry) entry)
	    ((expand-psort-entry entry) entry)
	    (t (setf (lex-entry-full-fs entry) :fail)
	       nil)))))

;; (bmw) copied from above
(defun get-expanded-lex-entry (entry)
  (when entry
    (cond ((eql (lex-entry-full-fs entry) :fail) nil)
	  ((lex-entry-full-fs entry) entry)
	  ((expand-psort-entry entry) entry)
	  (t (setf (lex-entry-full-fs entry) :fail)
	     nil))))
                   
(defun get-unexpanded-psort-entry (id &key (cache t))
  ;; for multi words, where we don't want the full-fs to be created
  ;; until we're sure we've got all the bits
  (let ((entry (read-psort *lexicon* id :cache cache)))
    entry))

(defun clear-expanded-lex nil
  (empty-cache *lexicon*))

(defun clear-non-parents nil
  (format t "~%Removing cached lexical entries")
  (clear-expanded-lex))

(defun get-psort-value (psort) 
  (let ((psort-entry (get-lex-entry-from-id psort)))
    (if psort-entry 
	(lex-entry-full-fs psort-entry)
      (let ((other-entry (get-other-entry psort)))
	(if other-entry
	    (psort-full-fs other-entry)
	  (let ((gr-entry (get-grammar-rule-entry psort)))
	    (if gr-entry
		(rule-full-fs gr-entry)
	      (let ((lr-entry (get-lex-rule-entry psort)))
		(if lr-entry 
		    (rule-full-fs lr-entry)
		  (lex-expansion-error 
		   "Return nil" 
		   (format 
		    nil 
		    "~A is not a valid structure identifier" psort)))))))))))

(defun get-lex-entry (orth)
  (loop 
      for psort in (remove-duplicates (lookup-word *lexicon* orth))
      for entry = (get-lex-entry-from-id psort)
      when entry collect entry))

(defun get-unexpanded-lex-entry (orth)
  (loop 
      for psort in (remove-duplicates ;; is remove-duplicates necessary???
		    (lookup-word *lexicon* orth))
      for entry = (get-unexpanded-psort-entry psort)
      when entry collect entry))

(defun generate-unknown-word-entries (orth)
  (loop for type in *unknown-word-types*
      nconc
        (let* 
            ((*safe-not-to-copy-p* nil)
             (unifs 
               (cons 
                (make-unification 
                 :lhs
                 (create-path-from-feature-list nil)
                 :rhs 
                 (make-u-value :type type))
                (make-unknown-word-sense-unifications orth)))
             (tdfs (let* ((fs (process-unifications unifs))
                          (wffs (if fs (create-wffs fs))))
                     (if wffs 
                         (construct-tdfs wffs nil t)))))
          (if tdfs
              (list (cons (gensym "unknown") tdfs))))))
              

(defun add-lex-from-file (orth sense-id fs-or-type defs)
  (let* ((lex-id (if orth (make-lex-id orth sense-id) sense-id))
         (orth-string (if (and orth *sense-unif-fn*)
                          (format nil "~A" orth) 
			(extract-orth-from-unifs fs-or-type)
			))
         (infl-pos (if (and (listp orth-string) (cdr orth-string))
		       ;; infl-pos is only relevant for multi-word entries
                       (find-infl-pos fs-or-type orth-string sense-id))))
    ;; adapted for the case where the orthography is only specified
    ;; in the FS; extract-orth-from-unifs must be defined on a
    ;; per-grammar basis

    (set-lexical-entry *lexicon-in* orth-string lex-id 
		       (make-lex-entry
			:orth orth-string
			:infl-pos infl-pos                  
			:id lex-id
			:unifs fs-or-type 
			:def-unifs defs))))

(defun extract-orth-from-unifs (unifs)
  ;; returns a list of strings
  ;; doesn't do much error checking
  (let ((matching-unifs 
         (loop for unif in unifs
              nconc
              (if (unification-p unif)
		  (let ((unif-lhs (unification-lhs unif)))
		    (if (general-path-match unif-lhs *orth-path*)
			(list unif)))))))
    (when matching-unifs
      (let ((n (length *orth-path*))
            (orth-strings nil))
        (cond ((null matching-unifs) nil)
              ((and (not (cdr matching-unifs))
                    (general-path-match 
                     (unification-lhs (car matching-unifs))
                     *orth-path* t))
               (list (u-value-type (unification-rhs (car matching-unifs)))))
              (t 
               (loop
                 (let ((exact-match
                        (dolist (unif matching-unifs)
                           (when
                             (let* ((path (unification-lhs unif))
                                    (feats (path-typed-feature-list path)))
                               (if
                                   (or (and (path-p path)
                                            (eq
                                             (nth n feats)
                                             (car *list-head*)))
                                       (and (typed-path-p path)
                                            (let 
                                                ((nth-el (nth n feats)))
                                              (and nth-el
                                                   (eq (type-feature-pair-feature 
                                                        nth-el)
                                                       (car *list-head*))))))
                                   (u-value-p (unification-rhs unif))))
                             (return unif)))))
                   (unless exact-match (return))
                   (push (u-value-type (unification-rhs exact-match))
                         orth-strings)
                   (incf n)))
               (nreverse orth-strings)))))))
      
(defun general-path-match (unif-path path &optional exactp)      
  (or
   (and (typed-path-p unif-path)
        (typed-path-matches 
         (path-typed-feature-list unif-path) path exactp))
   (and (path-p unif-path)
        (path-matches 
         (path-typed-feature-list unif-path) path exactp))))

(defun path-matches (tfplist flist exactp)
  (cond ((and (null flist) (null tfplist)) t)
        ((null flist) (not exactp))
        ((null tfplist) nil)
        ((eq (car tfplist) (car flist))
         (path-matches (cdr tfplist) (cdr flist) exactp))
        (t nil)))

(defun typed-path-matches (tfplist flist exactp)
  (cond ((and (null flist) (null tfplist)) t)
        ((null flist) (not exactp))
        ((null tfplist) nil)
        ((eq (type-feature-pair-feature (car tfplist)) (car flist))
         (typed-path-matches (cdr tfplist) (cdr flist) exactp))
        (t nil)))  


(defun extract-orth-from-fs (tdfs)
  ;;; returns a single string, possibly concatenating the words
  (let ((fs (tdfs-indef tdfs))
        (current-orth-path *orth-path*)
        (alt-orth-path *alt-orth-path*)
        (orth-strings nil))
    (let ((simple-value (get-value-at-end-of fs current-orth-path)))
      (if (and simple-value 
               (stringp simple-value))
          simple-value
        (let ((current-orth 
               (or
                (get-value-at-end-of fs 
                                     (append current-orth-path *list-head*))
                (let ((alt-value
                       (get-value-at-end-of fs 
                                            (append alt-orth-path *list-head*))))
                  (when alt-value
                    (setf current-orth-path alt-orth-path))
                  alt-value))))
          (loop 
            (when (or (null current-orth) 
                      (not (stringp current-orth)))
              (return))
            (setf current-orth-path (append current-orth-path *list-tail*))
            (push current-orth orth-strings)
            (push " " orth-strings)
            (setf current-orth 
              (get-value-at-end-of fs 
                                   (append current-orth-path *list-head*))))
          (apply #'concatenate 'string (nreverse (cdr orth-strings))))))))



(defun make-lex-id (orth sense-id)
   (intern (format nil "~A_~A" orth sense-id)))

;;; When expanding a lexical entry we want to eventually produce a
;;; tdfs which then has the non-persistent defaults incorporated.  As
;;; an interim stage, we want a fs with all defaults which has not yet
;;; been linked.  We also have the unifications formed from the
;;; orthography etc to unify in to the non-default fs.

(defun lex-expansion-error (string1 string2)
  (if *batch-mode* 
      (format t "~%~A" string2)
    (cerror string1 string2)))

(defun lex-entry-local-fs (entry)
  (expand-psort-entry entry t))

(defun expand-psort-entry (entry &optional local-p)
  (let* ((*safe-not-to-copy-p* nil)
	 (orth (lex-entry-orth entry))
         (lex-id (lex-entry-id entry))
         (language (lex-entry-language entry))
         (fs (append (lex-entry-unifs entry)
		     (when (and orth *sense-unif-fn*)
		       (apply *sense-unif-fn* 
			      (list orth 
				    (format nil "~A" lex-id) language))))))
    (process-unif-list lex-id fs (lex-entry-def-unifs entry) entry
		       *description-persistence* local-p)))

(defun make-non-lex-psort-entry (name constraint default)
  ;;; called for category templates, roots, idioms etc
  ;;; expansion happens at load time
  (let* ((*safe-not-to-copy-p* nil)
	 (entry (make-psort :id name 
				 :unifs constraint 
				 :def-unifs default)))
    (process-unif-list (psort-id entry) 
		       (psort-unifs entry)
		       (psort-def-unifs entry) 
		       entry
		       *description-persistence*)
    entry))

(defun process-unif-list (lex-id indef-list default-specs entry persistence
			  &optional local-p)
  (let* ((fs (process-unifications indef-list))
	 (indef (if fs (create-wffs fs))))
    (if local-p
	fs
      (if indef
	  (let* ((default-fss
		     (loop for default-spec in default-specs
			 collect
			   (make-equivalent-persistence-defaults 
			    indef (car default-spec) (cdr default-spec) lex-id)))
		 (local-tdfs (construct-tdfs indef default-fss t)))
	    (let ((interim-fs local-tdfs))
	      (setf (tdfs-tail interim-fs)
		(yadu-general-merge-tails
		 (tdfs-tail interim-fs)
		 (tdfs-tail (ltype-tdfs (get-type-entry (type-of-fs indef))))
		 indef))
	      (let ((incorp-fs 
		     (if persistence
			 (make-indefeasible interim-fs (list persistence))
		       interim-fs)))
		;; Cut off useless pointers, to help garbage collection
		(compress-dag (tdfs-indef incorp-fs))
		(setf (psort-full-fs entry)
		  incorp-fs))))
	(progn
	  (if fs
	      (format t "~%Structure for ~A could not be made well formed~%" lex-id)
	    (format t "~%Structure for ~A could not be created~%" lex-id))
	  nil)))))

;; Check to see if compiled files match originals

(defun up-to-date-p (in-files out-files) ;; move to clex.lsp
  (when (every #'probe-file out-files)
    (let ((in-date (apply #'max (mapcar #'file-write-date in-files)))
	  (out-date (apply #'min (mapcar #'file-write-date out-files))))
      (> out-date in-date))))


;;; Code for `other' entries

;;; utility function

(defun get-other-entry (fs-id)
  (or (get-root-entry fs-id)
      (get-display-template-entry fs-id)
      (get-idiom-entry fs-id)
      (get-temporary-entry fs-id)))

;;; temporary storage of structures created by unification check

(defparameter *temporary-entries* nil)

(defun store-temporary-psort-entry (name fs)
  (push (cons name
	      (make-psort :id name :full-fs fs))
	*temporary-entries*))

(defun get-temporary-entry (id)
  (cdr (assoc id *temporary-entries*)))

;;; root stuff goes here, for want of anywhere better

(defparameter *root-entries* nil)

(defun clear-root-entries nil
  (setf *root-entries* nil))

(defun add-root-entry (id non-def defs)
  (push (cons id
	      (make-non-lex-psort-entry id non-def defs))
	*root-entries*))

(defun get-root-entry (id)
  (cdr (assoc id *root-entries*)))

;;; end code for `other' entries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  General lexicon methods
;;;

(defmethod lookup-word :around ((lexicon lex-database) orth &key (cache *lexicon-lexical-entries-cache-p*))
  (let* ((value (if (next-method-p) (call-next-method)))
	   (mode (extra-mode lexicon))
	   (extra 
	    ;:if no vals in lexicon (or union mode) ...
	    (when (or (null value) (eq mode :union))
	      (loop
		  with result = nil
		  for lexicon in (extra-lexicons lexicon)		
		  for value = (and lexicon 
				   (lookup-word lexicon orth :cache cache))
		  ;:shadow mode returns first set of vals found
		  when (and value 
			    (eq mode :shadow)) 
		  do
		    (return value)
		  else when value 
		  do
		    (setf result 
		      (append result value))
		  finally 
		    (return result))))
	 (value (append value extra)))
     value))

(defmethod lex-words :around ((lexicon lex-database))
  (let* ((words (if (next-method-p) (call-next-method)))
	 (extra 
	    (loop
		for lexicon in (extra-lexicons lexicon)
		for extra-words = (and lexicon (lex-words lexicon))
		collect extra-words))
	 (words (cons words extra)))
    (remove-duplicates (mapcan #'(lambda (x) x) words) :test #'equal)))

(defmethod collect-psort-ids :around ((lexicon lex-database) &key (cache t) (recurse t))
  (let* ((ids (if (next-method-p) (call-next-method)))
	 (extra 
	  (if recurse
	      (loop
		  for lexicon in (extra-lexicons lexicon)
		  for extra-ids = (and lexicon (collect-psort-ids lexicon :cache cache :recurse recurse))
		  collect extra-ids)))
	 (ids (cons ids extra)))
    (remove-duplicates 
     (apply #'append (mapcar #'(lambda (x) x) ids)) 
     :test #'equal)))

(defmethod set-lexical-entry ((lexicon lex-database) orth id new-entry &key (cache *lexicon-lexical-entries-cache-p*))
  (store-psort lexicon id new-entry orth)
  (when cache
    (with-slots (lexical-entries) lexicon
      (dolist (orth-el orth)
	(pushnew id (gethash (string-upcase orth-el) lexical-entries))))))


;;; todo: cache
(defmethod read-psort :around ((lexicon lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (cond ((and (next-method-p) 
	      (call-next-method)))
	(recurse 
	 (some #'(lambda (lex) 
		   (read-psort lex id :cache cache :new-instance new-instance))
	       (extra-lexicons lexicon)))))

(defmethod close-lex :around ((lexicon lex-database) &key in-isolation delete)
  (declare (ignore delete))
  (with-slots (invalid-p extra-mode extra-lexicons part-of) lexicon
    (empty-cache lexicon)
    (setf invalid-p nil)
    (setf extra-mode *lex-database-default-extra-mode*)
    (if (next-method-p) (call-next-method))
    (unless in-isolation
      ;;unlink from sub-lexicons
      (mapcar 
       #'(lambda (lex) 
	   (unlink lex lexicon)
	   ;;clear sub-lexicon
	   (if (null (part-of lex))
	       (close-lex lex))) 
       extra-lexicons)  
      ;;unlink from super-lexicons
      (mapcar #'(lambda (lex) (unlink lexicon lex)) part-of))
    lexicon))

(defvar *empty-cache-clears-generator-lexicon* t) ;;fix_me (hack)
(defmethod empty-cache :around ((lexicon lex-database) &key (recurse))
  (with-slots (lexical-entries psorts cache-lex-list) lexicon
    ;;
    ;; _fix_me_
    ;; this gets invoked from clear-expanded-lex(): ditching precious generator
    ;; indices seems hardly in the scope of the caller.         (3-nov-05; oe)
    ;;
    #-:logon
    (when (and (fboundp 'clear-generator-lexicon)
	       *empty-cache-clears-generator-lexicon*)
      (funcall 'clear-generator-lexicon))
    (if (typep lexicon 'external-lex-database)
	(call-next-method))
    (clrhash lexical-entries)
    (clrhash psorts)
    (setf cache-lex-list nil)
    (when (fboundp 'clear-lexicon-indices)
      (funcall 'clear-lexicon-indices))
    (when recurse 
      (mapcar #'(lambda (x) (empty-cache x :recurse recurse))
	      (extra-lexicons lexicon)))
    lexicon))

;;; End of general methods

(defmethod collect-expanded-lex-ids ((lexicon lex-database))
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (when (and value
			    (not (eq value :empty))
			    (lex-entry-full-fs value))
		   (push id ids)))
	     (slot-value lexicon 'psorts))
    ids))

;(defmethod unexpand-psort ((lexicon lex-database) id)
;  "remove cached entry from lexicon providing a value only (eg. first lexicon with non-:empty in cache)"
;  (let* ((id-lexicon (lexicon-for-id lexicon id))
;	 (psorts 
;	  (cond
;	   (id-lexicon
;	    (slot-value id-lexicon 'psorts))
;	   (t 
;	    (return-from unexpand-psort)))))
;    (remhash id psorts)
;    psorts))

(defmethod lexicon-for-id ((lexicon lex-database) id)
  (if (read-psort lexicon id :recurse nil) 
      lexicon
    (some #'(lambda (x) (lexicon-for-id x id)) (extra-lexicons lexicon))))

(defmethod forget-psort ((lexicon lex-database) id)
  "remove cached entry (can be :empty) from all lexicons"
  (remhash id (slot-value lexicon 'psorts))
  (and (next-method-p) 
	      (call-next-method))
  (some #'(lambda (x) (forget-psort x id)) (extra-lexicons lexicon)))

;;--

(defun get-keyword-val (keyword list)
  (second (member keyword list)))

;; this can't go in psqllex
(defun lexdb-enabled-p nil
  (when *lexdb-params*
    (cond
     ((member :psql *features*) t)
     (t
      (format t "~%WARNING: ignoring *lexdb-params* (distribution is not :psql-enabled)")
      nil))))

;; this can't go in psqllex
(defun mwe-lexicon-enabled-p nil
  (and
   (lexdb-enabled-p)
   *postgres-mwe-enable*))

;;;
;;; format conversion
;;;

(defun str-2-symb (str)
  (unless (stringp str)
    (error "string exected"))
  (intern (string-upcase str) :lkb))

(defun str-2-keyword (str)
  (unless (stringp str)
    (error "string exected"))
  (intern str :keyword))

;(defun str-2-keyword (str)
;  (unless (stringp str)
;    (error "string exected"))
;  (intern (string-upcase str) :keyword))

;; currently 'str-2-lisp-object' ...
(defun str-2-list (str)
  (with-package (:lkb)
    (let ((item (read-from-string str)))
      (cond
       ((listp item)
	item)
       (t
	(error "list expected"))))))

;; use (parse-integer X :junk-allowed t) for integers
(defun str-2-num (str &optional default)
  (with-package (:lkb)
    (let ((item (read-from-string str)))
      (cond
       ((numberp item)
	item)
       ((eq default t)
	(error "number expected"))
       (t default)))))

(defun str-2-numstr (str &optional default)
  (let ((num (str-2-num str)))
    (cond
     ((numberp num) 
      (format nil "~a" num))
     ((eq default t)
      (error "number expected"))
     (t default))))

(defun explode-to-chars (string)
  (loop
      for i from 0 to (1- (length string))
      collect (aref string i)))
  
(defun escape-char (esc-char string)
  (implode-from-chars
   (loop
       for i from 0 to (1- (length string))
       for c = (aref string i)
       if (or (eq c esc-char)
	      (eq c #\\))
       collect #\\
       collect c)))

#+:psql
(defun escape-sql-copy-string (esc-char string)
  (implode-from-chars
   (loop
       for i from 0 to (1- (length string))
       for c = (aref string i)
       if (eq c #\Backspace) collect #\\ and collect #\b
       else if (eq c #\Page) collect #\\ and collect #\f
       else if  (eq c #\Return) collect #\\ and collect #\r
       else if (eq c #\Newline) collect #\\ and collect #\n
       else if (eq c #\Tab) collect #\\ and collect #\t
       else if (eq c #\vt) collect #\\ and collect #\v
       else if (eq c #\\) collect #\\ and collect #\\
       else if (eq c esc-char) collect #\\ and collect c
       else collect c)))
  
(defun replace-char (string &key from to)
  (unless (and
	   (characterp from)
	   (characterp to))
    (error ":from and :to must be characters"))
  (implode-from-chars
   (loop
       for i from 0 to (1- (length string))
       for c = (aref string i)
       if (eq c from)
       collect to
       else
       collect c)))
  
(defun implode-from-chars (char-list)
  (loop
      with res = (make-string (length char-list))
      for c in char-list
      for i upfrom 0
      do
	(setf (schar res i) c)
      finally
	(return res)))

;; map string list to CVS copy line
(defun str-list-2-line (str-list &key (sep-c #\Tab) (null-str "\\N"))
  (unless (listp str-list)
    (error "list argument expected"))
  (let ((sep (string sep-c)))
    (cond
     ;; return empty line if empty string list
     ((null str-list) "")
     ;; build line
     ;; sep-c separates values
     ;; values are null-str or escape-sql-copy-string'd vals
     (t (apply 'concatenate
	       (cons
		'string
		(cons
		 (escape-sql-copy-string sep-c (pop str-list))
		 (mapcan #'(lambda (x) 
			     (list sep
				   (if x 
				       (escape-sql-copy-string sep-c x)
				     null-str)
				   ))
			 str-list))))))))
  
(defun str-list-2-str (str-list &key (sep-c #\Space)
				     (null-str ":null:")
				     (esc t))
  (unless (listp str-list)
    (error "list expected"))
  (let ((sep (string sep-c)))
    (cond
     ((null str-list) "")
     (t (apply 'concatenate
	       (cons
		'string
		(cons
		 (if esc
		     (escape-char sep-c (pop str-list))
		   (pop str-list))
		 (mapcan #'(lambda (x) (list sep
					     (if x 
						 (if esc
						     (escape-char sep-c x)
						   x)
					       null-str)
					     ))
			 str-list))))))))
  
(defun str-list-2-str-by-str (str-list &optional (separator " "))
  (unless (listp str-list)
    (error "list expected"))
  (cond
   ((null str-list) "")
   (t (apply 'concatenate
	     (cons
	      'string
	      (cons
	       (pop str-list)
	       (mapcan #'(lambda (x) (list separator 
					   x))
		       str-list)))))))
  
(defun symb-2-str (symb)
  (unless (symbolp symb)
    (error "symbol expected"))
  (cond
   ((null symb) "")
   (t (string-downcase (string symb)))))

(defun num-2-str (num)
  (if (null num)
      (return-from num-2-str))
  (unless (numberp num)
    (error "number expected"))
  (format nil "~a" num))
  
(defun char-2-symb (c)
  (unless (characterp c)
    (error "character expected"))
  (str-2-symb (string c)))

(defun char-2-num (c)
  (unless (characterp c)
    (error "character expected"))
  (str-2-num (string c)))

(defun 2-symb (x)
  (cond
   ((symbolp x) x)
   ((stringp x) (str-2-symb x))
   (t (error "unhandled type"))))

(defun 2-str (x)
  (cond
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   ((pathnamep x) (namestring x))
   (t (error "unhandled type"))))

(defun null-or-2-str (x)
  (cond
   ((null x) nil)
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   (t (error "unhandled type"))))

;;;
;;; misc
;;;

(defun make-nice-temp-file-pathname (filename)
  (make-pathname :name filename
		 :host (pathname-host (lkb-tmp-dir))
		 :device (pathname-device (lkb-tmp-dir))
		 :directory (pathname-directory (lkb-tmp-dir))))

(defun get-grammar-version nil
  (let ((grammar-version (or (and (find-package :lkb)
				  (find-symbol "*GRAMMAR-VERSION*" :lkb))
			     (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))))
    (if (and grammar-version (boundp grammar-version))
            (remove-if-not #'alphanumericp (symbol-value grammar-version))
            "UNKNOWN_VERSION")))


(defun lex (str) (car (member str (extra-lexicons *lexicon*) :key #'name :test #'string=)))

(defun recomp (x)
  (compile-file x)
  (load x))


;;;
;;; initial code to support a notion of orthographic variants; see comments on
;;; *duplicate-lex-ids* in `mrs/generate.lisp'.  quite possibly, this would be
;;; easier to do in the DB natively, i.e. comparing relevant field values, but
;;; this version at least i can run without a DB (or with).     (8-aug-04; oe)
;;;

;;;
;;; a global repository of orthographic variants, relating sets of lexical ids
;;; that are equivalent modulo their STEM and ONSET values.
;;;
(defparameter *orthographic-variants* nil)

;;;
;;; mostly for efficiency reasons: extract a hash key per lexical entry, such
;;; that we need only compare entries with identical keys.  an invalid path for
;;; a non-Matrix grammar should result in loss of efficiency but not desaster.
;;;
(defparameter *key-path* '(SYNSEM LOCAL CAT HEAD KEYS KEY))

;;;
;;; _fix_me_
;;; give some thought to memory management, in the sense that we will end up
;;; with the full (expanded) lexicon in core, unless the caching in the lexicon
;;; itself did something clever to limit cache growth.  the latter, i guess,
;;; might be a good thing to aim for, if not available already. (8-aug-04; oe)
;;;
(defun find-orthographic-variants (&key file)
  (let ((stream (if (stringp file) 
                  (open file :direction :output :if-exists :supersede)
                  t))
        (map (make-hash-table :test #'equal))
        classes)

    ;;
    ;; first, populate .map. with pairs of lexical entries and restricted AVMs,
    ;; indexed by the type at *key-path* to reduce the number of comparisons.
    ;;
    (loop
        with *packing-restrictor* = (cons 'onset (last *orth-path*))
        for id in (collect-psort-ids *lexicon*)
        for entry = (get-lex-entry-from-id id)
        for dag = (tdfs-indef (lex-entry-full-fs entry))
        for key = (let ((dag (existing-dag-at-end-of dag *key-path*)))
                    (when dag (dag-type dag)))
        for copy = (copy-dag-partially dag)
        do
          (push (cons entry copy) (gethash key map)))

    ;;
    ;; next, compare all pairs (of pairs) in each .map. bucket against each
    ;; other, though making sure to not do all pairs twice; records classes of
    ;; two or more lexical entries that compare equivalent (in their restricted
    ;; AVMs, i.e. sans orthography).
    ;;
    (loop
        for bucket being each hash-value in map
        do
          (loop
              with matches
              for entry = (pop bucket)
              for dag1 = (rest entry)
              for class = (list (first entry))
              while entry
              do
                (loop
                    for (key . dag2) in bucket
                    when (dag-equal-p dag1 dag2)
                    unless (member key matches :test #'eq)
                    do
                      (push key matches)
                      (push key class))
                (when (rest class) (push class classes))))

    (setf *orthographic-variants* (make-hash-table))
    
    ;;
    ;; finally, do two things: set the global variable *orthographic-variants*
    ;; and output a textual representation of orthographic variants for later
    ;; re-use.  within each class sort lexicographically (for lack of a better
    ;; notion of precedence), but respect an original list of suppressed items
    ;; in *duplicate-lex-ids*, in case the grammar still supplies one.
    ;;
    (loop
        for class in classes
        do
          (loop
              with entries = (sort class #'string-lessp :key #'lex-entry-id)
              for entry in entries
              for id = (lex-entry-id entry)
              when (member id *duplicate-lex-ids* :test #'eq)
              collect id into passive
              else collect id into active
              finally
                (format
                 stream
                 "~@[~{~(~a~)~^ ~}~]~@[ : ~{~(~a~)~^ ~}~]~%"
                 active passive)
                (loop
                    for entry in entries
                    for id = (lex-entry-id entry)
                    for variants = (loop
                                       for foo in entries
                                       for bar = (lex-entry-id foo)
                                       unless (eq bar id) collect bar)
                    do (setf (gethash id *orthographic-variants*) variants))))
    
    (when (and (stringp file) (open-stream-p stream) (close stream)))
    
    classes))

(defun orthographic-variants-p (le1 le2)
  (when (hash-table-p *orthographic-variants*)
    (let ((le1 (intern le1 :lkb))
          (le2 (intern le2 :lkb)))
      (or (eq le1 le2)
          (member le1 (gethash le2 *orthographic-variants*) :test #'eq)
          (member le2 (gethash le1 *orthographic-variants*) :test #'eq)))))

