;;; Copyright (c) 1991--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; User defined functions - from old globals file

(in-package :lkb)

(defun preprocess-sentence-string (str)
  ;; default, and very basic, embedded preprocessor
  ;; treat all punctuation as spaces, except:
  ;; - in *bracketing-p* mode in which case ( and ) are special
  ;; - for PAGE compatability treat #\' as #\space
  ;;     except at end of word treat as if #\space #\s
  ;; split into words on spaces
  ;; in *characterize-p* mode keep track of character position pointers
  #+(or :preprocessor :xml)
  (declare (special *x-preprocessor* *preprocessor* *sppp-stream*))

  #+:xml
  (when *sppp-stream*
    (return-from preprocess-sentence-string (sppp str)))

  #+:preprocessor
  (when *x-preprocessor*
    (return-from preprocess-sentence-string 
      (x-preprocess str :format :saf)))

  #+:preprocessor
  (when *preprocessor*
    (return-from preprocess-sentence-string 
      (preprocess str :format :lkb :verbose nil)))
  
  (let ((in-word nil)
        (chars (coerce str 'list))
        (result-chars nil)
	(cwords nil) (from 0) (i 0))
    (flet ((push-word ()
	     (if result-chars
		 (push (if *characterize-p*
			   (make-chared-word :word (coerce (reverse result-chars) 'string)
					     :cfrom from :cto i)
			 (coerce (reverse result-chars) 'string))
		       cwords))
	     (setf result-chars nil)
	     (setf in-word nil)))
      (do* ((next-char (car chars) (car remainder))
	    (remainder (cdr chars) (cdr remainder)))
	  ((null next-char) nil)
	(cond ((eql next-char #\')
	       (cond 
		((not in-word) 
		 (if (or (null remainder) (eql (car remainder) #\space))
		     nil
		   (progn
		     (push next-char result-chars)
		     (unless in-word (setf from i))
		     (setf in-word t))))
		((or (null remainder) (eql (car remainder) #\space))
		 (push-word)
		 (push #\s result-chars) (incf i)
		 (push-word))
		(t
		 (push-word))))
	      ((and *bracketing-p*
		    (or (eql next-char #\() 
			(eql next-char #\))))
	       (push-word)
	       (push (string next-char) cwords) (decf i))
	      ((or (char= next-char #\Space)
		   (not (alphanumeric-or-extended-p next-char)) )
	       (push-word))
	      (t 
	       (unless in-word (setf from i))
	       (setf in-word t)
	       (push next-char result-chars)))
	(incf i))
      (when result-chars
	(push-word)))
    (reverse cwords)))

(defun make-sense-unifications (sense-string id language)
   ;; < orth : hd > = sense
   ;; < lang > = language
  (declare (ignore sense-string id language))
   nil)

;;; if  make-sense-unifications is redefined to do something
;;; *sense-unif-fn* should be set 
;;; (defparameter *sense-unif-fn* #'make-sense-unifications)


;;;   (when sense-string
;;;    (list 
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(orth hd))
;;;          :rhs (make-u-value :type sense-string))
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(lang))
;;;          :rhs (make-u-value :type language))
;;;        (make-unification :lhs
;;;           (create-path-from-feature-list '(fs-id))
;;;           :rhs (make-u-value :type id))
;;;       )))


(defun make-unknown-word-sense-unifications (word-string)
  (when word-string
    (list 
       (make-unification :lhs
          (create-path-from-feature-list '(orth list first))
          :rhs (make-u-value :type word-string))
       (make-unification :lhs
          (create-path-from-feature-list '(sem key pred))
          :rhs (make-u-value :type 
                             (concatenate 'string word-string "_rel"))))))


(defun instantiate-generic-lexical-entry (gle surface)
  (declare (ignore gle surface)))


(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :type orth-value))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
    #|
    ;; not a good idea for difference lists                             
    (push (make-unification :lhs
                     (create-path-from-feature-list tmp-orth-path)
                            :rhs 
                     (make-u-value :type *empty-list-type*))
           unifs)
           |#
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (when indef
          (make-tdfs :indef indef))))))

#|
;;; following is for non-list valued ORTHs
(defun make-orth-tdfs (orth)
  (let ((indef (process-unifications 
                (list 
                 (make-unification :lhs
                                   (create-path-from-feature-list 
                                    *orth-path*)                    
                                   :rhs
                                   (make-u-value 
                                    :type orth))))))
    (when indef
      (setf indef (create-wffs indef))
      (when indef
        (make-tdfs :indef indef)))))
|#
  

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
   (sort (remove 'needs-affix (top-level-features-of rule-fs))
         #'(lambda (x y)
             (let ((x-num (if (numberp x) x
                            (parse-integer (string x) :junk-allowed t)))
                   (y-num (if (numberp y) y
                            (parse-integer (string y) :junk-allowed t))))
               (if (and (numberp x-num) (numberp y-num))
                 (< x-num y-num)
                 (not (numberp x-num)))))))


(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
  (unless (eql *morph-option* :default)
    (error "~%This code assumes that the LKB morphophonology analyser is in use,so will only work if *morph-option* is set to :default - please 
redefine the function spelling-change-rule-p as appropriate"))
  (in-morph-rule-set-p rule))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = false
;;; in the rule
  (let ((affix (get-dag-value (tdfs-indef (rule-full-fs rule)) 'productive)))
    (and affix (bool-value-false affix))))

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
  (declare (ignore type-name))
  nil)

(defun find-infl-pos (unifs orths sense-id)
  ; default inflection position for multi-word entries is rightmost
  (declare (ignore unifs sense-id))
  (length orths))

;;; Assign priorities to parser tasks

(defun rule-priority (rule)
  (declare (ignore rule))
  1)

(defun lex-priority (mrec)
  (declare (ignore mrec))
  1)

;;; Assign priorities to generator tasks

(defun gen-rule-priority (rule)
  (declare (ignore rule))
  1)

(defun gen-lex-priority (mrec)
  (declare (ignore mrec))
  1)

;;; functions which returns a directory etc for LKB temporary files.


(defparameter *psorts-temp-file* nil  
   "a temporary file for the lexicon")

(defparameter *psorts-temp-index-file* nil
  "a file to index the lexicon")

(defparameter *leaf-temp-file* nil
  "a temporary file for leaf types")

(defun lkb-tmp-dir nil 
  ;;; This should be a function, rather than a global, because we 
  ;;; need something that will work for an individual user
  ;;; e.g. by calling user-homedir-pathname
  ;;; unfortunately this doesn't do what we want for MCL
  ;;; so this hardwires the pathname
  (or
   #+(and :allegro :mswindows)
   (let ((tmp
          (if (system:getenv "TMP")
	    (or (let ((path (concatenate 'string (system:getenv "TMP") "\\")))
		  (ignore-errors (parse-namestring path)))
		(ignore-errors (parse-namestring (system:getenv "TMP"))))
            (when (system:getenv "TEMP")
	      (or (let ((path 
			 (concatenate 'string (system:getenv "TEMP") "\\")))
		    (ignore-errors (parse-namestring path)))
		  (ignore-errors 
		   (parse-namestring (system:getenv "TEMP"))))))))
		  
     (when (and (pathnamep tmp) (ignore-errors (directory tmp))) tmp))
  (let ((pathname (user-homedir-pathname))
        (tmp-dir #-:mcl '("tmp")
                 #+:mcl '("Documents" "tmp")))
    (make-pathname
     :host (pathname-host pathname) :device (pathname-device pathname)
     :directory (append (pathname-directory pathname) tmp-dir)
     :name (pathname-name pathname) :type (pathname-type pathname)
     :version (pathname-version pathname)))))


(defun set-temporary-lexicon-filenames nil
  ;;; grammars can redefine this to use different names
  (setf *psorts-temp-file* 
    (make-pathname :name "templex"
                   :host (pathname-host (lkb-tmp-dir))
                   :device (pathname-device (lkb-tmp-dir))
                   :directory (pathname-directory (lkb-tmp-dir))))
  (setf *psorts-temp-index-file* 
    (make-pathname :name "templex.idx"
                   :host (pathname-host (lkb-tmp-dir))
                   :device (pathname-device (lkb-tmp-dir))
                   :directory (pathname-directory (lkb-tmp-dir))))
  (setf *leaf-temp-file* 
    (make-pathname :name "templeaf" 
                   :host (pathname-host (lkb-tmp-dir))
                   :device (pathname-device (lkb-tmp-dir))
                   :directory (pathname-directory (lkb-tmp-dir)))))

  


;;; Generator function

(defun intersective-modifier-dag-p (dag)
   "is this dag a possible intersective modifier?"
   (let ((val
          (existing-dag-at-end-of dag 
                                  '(synsem local cat head mod first local))))
      (and val
           (subtype-or-equal (type-of-fs val) 'intersective_mod))))


;;; The following function needs to be called if there is a disrepancy between
;;; the checkpaths construction and application

(defun check-path-convert (check-paths)
  (let ((new-paths nil)
        (combined-paths nil))
    ;; Remove [ ARGS.REST...FIRST ] prefix from paths
    (dolist (thing check-paths)
      (let ((path (car thing))
	    (count (cdr thing)))
	(if (eql (car path) 'ARGS)
	    (let ((rest (cdr path)))
	      (if (eql (car rest) 'FIRST)
		  (push (cons (cdr rest) count)
			new-paths)
		(if (and (eql (car rest) 'REST)
			 (eql (cadr rest) 'FIRST))
		    (push (cons (cddr rest) count) new-paths)
		  (if (and (eql (car rest) 'REST)
			   (eql (cadr rest) 'REST)
			   (eql (caddr rest) 'FIRST))
		      (push (cons (cdddr rest) count) new-paths)
		    (error "Unexpected path ~A" path)))))
	  (push (cons path count) new-paths))))
    ;; Combine the weights for any paths that became identical after removing
    ;; the prefix
    (dolist (np new-paths)
      (let ((existing (assoc (car np) combined-paths :test #'equal)))
	(if existing 
	    (setf (cdr existing) (+ (cdr existing) (cdr np)))
	  (push np combined-paths))))
    (sort combined-paths #'> :key #'cdr)))

(defun bool-value-true (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type 'true))))
  
(defun bool-value-false (fs)
  (and fs
       (let ((fs-type (type-of-fs fs)))
         (eql fs-type 'false))))

;;;
;;; the following two functions allow customizing SEM-I creation: given a set
;;; of FSs corresponding to semantic variables plus the FS of a lexical entry,
;;; return a list of boolean flags indicating optionality of each argument.
;;; furthermore, given a lexical entry, compute an a-list containing derived
;;; (surface) forms.
;;;
(defun determine-argument-optionality (sign arguments)
  (declare (ignore sign))
  (loop repeat (length arguments) collect nil))

(defun determine-derived-forms (le)
  (declare (ignore le)))

;;;
;;; the following two functions allow customization of how edges are displayed
;;; in the LUI chart browser (not the traditional LKB chart window).  for each
;;; edge, two properties are relevant: (a) its `name' and (b) its `label'; both
;;; should be strings, where name should be a relatively short, yet contentful
;;; identifier used as the primary representation of edges in chart cell, and
;;; label can be a longer string shown in the pop-up area on mouse-over.
;;;
(defun lui-chart-edge-name (edge)
  (format 
   nil 
   "~a[~a]"
   (tree-node-text-string (find-category-abb (edge-dag edge))) 
   (edge-id edge)))


(defun lui-chart-edge-label (edge)
  (format
   nil
   "~a"
   (typecase (edge-rule edge)
     (string (first (edge-lex-ids edge)))
     (symbol (edge-rule edge))
     (rule (rule-id (edge-rule edge)))
     (t :unknown))))

