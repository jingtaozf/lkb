;;; Copyright (c) 1991--2003 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.

;;; bmw (aug-03)
;;; - lookup-word now caches for each lexicon separately

;;; bmw (aug-03)
;;; - fixed code broken by *lexicon*-related changes

;;; aac (aug-03)
;;; - moved old flat file stuff to slex.lsp - added to cvs
;;;   but not loaded by lkb.system
;;; - removed `other' entries from *lexicon*
;;;   (code for handling them is either here or in 
;;;   specific files)

;;; bmw (jun-03)
;;; - lex-words looks in sublexicons too
;;; - (partial) caching of null values
;;; - link/unlink replace (setf extra-lexicons)
;;;

(in-package :lkb)

;;; Lexical entries, storage etc

;;; Lexical entries are indexed by orthography (string)
;;; They are also all indexed by an identifier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *lexicon-in*)
(defvar *verbose-lex-lookup-word* nil)
(defvar *psql-lexicon*)

(defclass lex-database () 
  ((lexical-entries :initform (make-hash-table :test #'equal))
   (psorts :initform (make-hash-table :test #'eq))
   (extra-lexicons :initform nil :reader extra-lexicons) ;: use link/unlink to write
   (extra-mode :initform :union :accessor extra-mode)
   (part-of :initform nil :accessor part-of)))

;: obsolete: use link instead
(defmethod (setf extra-lexicons) (lexicon-list (lexicon lex-database))
  (format *trace-output* "WARNING: using obsolete method (setf extra-lexicons)")
  (mapcar #'(lambda (lex)
	      (unless (member lex lexicon-list)
		(unlink lex lexicon)))
	  (extra-lexicons lexicon))
  (mapcar #'(lambda (lex)
	      (unless (member lex (extra-lexicons lexicon))
		(link lex lexicon)))
	  lexicon-list))

(defmethod clear-cache ((lexicon lex-database))
  (clrhash (slot-value lexicon 'lexical-entries)))

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

(defgeneric set-lexical-entry (lexicon orth id new-entry))

(defgeneric clear-lex (lexicon &rest rest))

(defgeneric collect-expanded-lex-ids (lexicon))

(defgeneric store-psort (lexicon id entry &optional orth))

(defgeneric read-psort (lexicon id &key (cache) (recurse t)))

(defgeneric unexpand-psort (lexicon id))

;;;
;;; for a given lexicon, return list of psort identifiers (as strings), i.e.
;;; identifiers of lexical items plus start symbols and node labels (for the
;;; time being, these are stored in the same namespace).
;;;
(defgeneric collect-psort-ids (lexicon &key (recurse t)))

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

(defun get-lex-entry-from-id (id)
  ;;; now lexical entries only
  (let ((entry (get-unexpanded-psort-entry id)))
    ;; if we haven't previously expanded then
    ;; destructively modify entry
    (when entry
      (cond ((eql (lex-entry-full-fs entry) :fail) nil)
	    ((lex-entry-full-fs entry) entry)
	    ((expand-psort-entry entry) entry)
	    (t (setf (lex-entry-full-fs entry) :fail)
	       nil)))))
                   
(defun get-unexpanded-psort-entry (id)
  ;; for multi words, where we don't want the full-fs to be created
  ;; until we're sure we've got all the bits
  (let ((entry (read-psort *lexicon* id)))
    entry))

(defun clear-expanded-lex nil
  (dolist (id (collect-expanded-lex-ids *lexicon*))
    (unexpand-psort *lexicon* id)))

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

(defun get-type-from-lex-id (id)
  ;;; Utility function for MPhillies
  ;;; just returns nil if it doesn't find anything suitable
  ;;; since they may perhaps want to call it on things
  ;;; which may not be valid
  (let ((entry (get-lex-entry-from-id id)))
    (if entry
        (let ((tdfs (lex-entry-full-fs entry)))
          (if (tdfs-p tdfs)
              (indef-type-of-tdfs tdfs))))))

(defun get-lex-entry (orth)
  (loop 
      for psort in (remove-duplicates (lookup-word *lexicon* orth))
      for entry = (get-lex-entry-from-id psort)
      when entry collect entry))

(defun get-unexpanded-lex-entry (orth)
  (loop 
      for psort in (remove-duplicates (lookup-word *lexicon* orth))
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
    ;(set-lexical-entry *lexicon* orth-string lex-id 
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
		 (tdfs-tail (type-tdfs (get-type-entry (type-of-fs indef))))
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
	      (format t "~%Structure for ~A could not be made well formed" lex-id)
	    (format t "~%Structure for ~A could not be created" lex-id))
	  nil)))))

;; Check to see if compiled files match originals

(defun up-to-date-p (in-files out-files)
  (when (every #'probe-file out-files)
    (let ((in-date (apply #'max (mapcar #'file-write-date in-files)))
	  (out-date (apply #'min (mapcar #'file-write-date out-files))))
      (> out-date in-date))))


;;; Code for `other' entries

;;; utility function

(defun get-other-entry (fs-id)
  (or (get-root-entry fs-id)
      (get-display-template-entry fs-id)
;      (get-idiom-entry fs-id)
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

(defmethod lookup-word :around ((lexicon lex-database) orth &key (cache t))
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
		      ;(nconc result value))
		  finally 
		    (return result))))
	   (value (append value extra)))
	   ;(value (nconc value extra)))
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

(defmethod collect-psort-ids :around ((lexicon lex-database) &key (recurse t))
  (let* ((ids (if (next-method-p) (call-next-method)))
	 (extra 
	  (if recurse
	      (loop
		  for lexicon in (extra-lexicons lexicon)
		  for extra-ids = (and lexicon (collect-psort-ids lexicon :recurse recurse))
		  collect extra-ids)))
	 (ids (cons ids extra)))
    (remove-duplicates (mapcan #'(lambda (x) x) ids) :test #'equal)))

(defmethod set-lexical-entry ((lexicon lex-database) orth id new-entry)
  (store-psort lexicon id new-entry orth)
  (with-slots (lexical-entries) lexicon
    (dolist (orth-el orth)
      (pushnew id (gethash (string-upcase orth-el) lexical-entries)))))


;;; todo: cache
(defmethod read-psort :around ((lexicon lex-database) id &key (cache t) (recurse t))
  (cond ((and (next-method-p) (call-next-method)))
	(recurse (some #'(lambda (lex) (read-psort lex id :cache cache))
		       (extra-lexicons lexicon)))))

(defmethod clear-lex :around ((lexicon lex-database) &rest rest)
  (let ((in-isolation (get-keyword-val :in-isolation rest)))
;  (when (fboundp 'clear-generator-lexicon)
;    (funcall 'clear-generator-lexicon))
;  (clrhash (slot-value lexicon 'lexical-entries))
;  (clrhash (slot-value lexicon 'psorts))
;  (when (fboundp 'clear-lexicon-indices)
;    (funcall 'clear-lexicon-indices))
    (empty-cache lexicon)
    (call-next-method)
    (unless in-isolation
      ;;unlink from sub-lexicons
      (mapcar 
       #'(lambda (lex) 
	   (unlink lex lexicon)
	   ;;clear sub-lexicon
	   (if (null (part-of lex))
	       (apply #'clear-lex (cons lex rest)))) 
       (extra-lexicons lexicon))  
      ;;unlink from super-lexicons
      (mapcar #'(lambda (lex) (unlink lexicon lex)) (part-of lexicon)))
    lexicon))

(defmethod empty-cache ((lexicon lex-database))
  (when (fboundp 'clear-generator-lexicon)
    (funcall 'clear-generator-lexicon))
  (clrhash (slot-value lexicon 'lexical-entries))
  (clrhash (slot-value lexicon 'psorts))
  (when (fboundp 'clear-lexicon-indices)
    (funcall 'clear-lexicon-indices))
    lexicon)

;;; End of general methods

;;; Utility function - called from leaf.lsp and clex.lsp

;;; dunno what this is - looks like oe code

(defun lexicon-to-xml (&key (stream t) file)
  (loop
      with stream = (if file
                      (open file
                            :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
                      stream)
      with *batch-mode* = t
      for id in (collect-psort-ids *lexicon*)
      for entry = (read-psort *lexicon* id)
      for tdfs = (and entry (lex-entry-local-fs entry))
      for type = (and tdfs (indef-type-of-tdfs tdfs))
      for stem = (and entry (lex-entry-orth entry))
      when (and id type stem) do
        (format
         stream
         "<instance name=\"~(~a~)\" type=\"~(~a~)\" ~
           stem=\"~(~{~a~^ ~}~)\" status=\"lexicon\"/>~%"
         id type stem)
      finally (when file (close stream))))

;; moved from clex.lsp (it belongs here)

(defmethod collect-expanded-lex-ids ((lexicon lex-database))
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (when (and value
			    (lex-entry-full-fs value))
		   (push id ids)))
	     (slot-value lexicon 'psorts))
    ids))

(defmethod unexpand-psort ((lexicon lex-database) id)
  (setf (gethash id (slot-value lexicon 'psorts)) nil))

;;--

(defun get-keyword-val (keyword list)
  (second (member keyword list)))
