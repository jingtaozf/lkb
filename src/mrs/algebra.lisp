;;; Copyright (c) 2004--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Checking that a grammar obeys the algebra for composition
;;; and extracting the components so that algebra MRSs can be displayed
;;; on parse trees.

;;; sement structures and print code are in basemrs.lisp

(in-package :mrs)

;;; FIX
;;; variable naming - we would like this to be constant over a parse
;;; this implies calling the algebra construction code in such a way
;;; that variables are recognised as equivalent between phrases.
;;; The best way to do this is probably to do something INSTLOC-like
;;; but ignore for now.
;;; There's also an issue about the interaction between this and the standard
;;; MRS extraction code, again ignored for now.

(defun extract-sement (parse-fs edge-record)
  (declare (ignore edge-record))
  (initialise-algebra)
  (let ((sement 
	 (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
    sement))

(defun extract-and-check-sement (parse-fs edge-record)
  (initialise-algebra)
  (let ((sement 
	 (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
    (check-algebra sement edge-record)))

(defun initialise-algebra nil
  (unless (and *hook-path*
	       *psoa-liszt-path*
	       *psoa-rh-cons-path*)
    (error "Grammar parameters not set for algebra use"))
  (setf *variable-generator* (init-variable-generator))
  (setf *all-nodes* nil)
  (setf *named-nodes* nil))

(defun extract-algebra-from-fs (fs)
  ;;; components are
  ;;; a) hook
  ;;; b) slots
  ;;; c) rels list
  ;;; d) hcons
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    (if (is-valid-fs sem-fs)
        (construct-algebra-sement sem-fs fs))))


(defun construct-algebra-sement (sem-fs full-fs)
  (let* ((hook-fs (path-value sem-fs *hook-path*))
         (liszt-fs (path-value sem-fs *psoa-liszt-path*))
         (h-cons-fs (path-value sem-fs *psoa-rh-cons-path*)))
    (unless (and hook-fs liszt-fs h-cons-fs)
      (error "Missing algebra components"))
    (let* ((hook (construct-algebra-hook hook-fs nil))
	   (rels (nreverse (construct-liszt 
			    liszt-fs nil *variable-generator*)))
	  ;;; construct-liszt in mrsoutput
	   (hcons (nreverse (construct-h-cons 
			     h-cons-fs nil *variable-generator*)))
	         	  ;;; construct-h-cons in mrsoutput
	   (slots (construct-algebra-slots full-fs)))
      ;;; let* because slots construction assumes other variables
      ;;; are already found to allow pruning of unused `slots'
      (make-sement :hook hook :slots slots
		   :liszt rels :h-cons hcons))))

(defun construct-algebra-hook (fs in-slot-p)
  (let ((index-fs
	 (path-value fs *index-path*))
	(ltop-fs 
	 (path-value fs *ltop-path*))
	(xarg-fs
	 (path-value fs *xarg-path*)))
    (unless (or in-slot-p
		(and index-fs (is-valid-fs index-fs)
		 ltop-fs (is-valid-fs ltop-fs)))
      (error "Defective hook feature structure"))
    (if (or (not in-slot-p)
	    (assoc index-fs *named-nodes*)
	    (assoc ltop-fs *named-nodes*)
	    (and xarg-fs (is-valid-fs xarg-fs)
		 (assoc xarg-fs *named-nodes*)))
	(make-hook
	 :index (create-variable index-fs *variable-generator*)
	 :ltop (create-variable ltop-fs *variable-generator*)
	 :xarg (if (and xarg-fs (is-valid-fs xarg-fs))
		   (create-variable xarg-fs *variable-generator*))))))


(defun construct-algebra-slots (fs)
  ;;; the code for the generator and the SEM-I that do fairly
  ;;; similar things don't quite do what's wanted.  
  ;;; This code walks over a feature structure, finding all
  ;;; paths in the syntax part of the FS that lead to a HOOK
  ;;; feature.  The HOOK is taken to be a slot, the path name
  ;;; is used to generate the slot name.
  ;;; 1. reentrancy - if we've seen the HOOK before, store 
  ;;; all paths that lead to it and decide on the best name
  ;;; when we're done (e.g., ignore -- paths)
  ;;; 2. don't worry about OPT etc.  The slot will be registered
  ;;; as a slot at the location where it exists.  The algebra
  ;;; checking code will have to ensure legitimacy of slot removal.
  ;;;
  ;;; C-CONT is an issue
  (let ((slot-alist
	 (lkb::collect-subdags-for-type fs 
					   *hook-type* 
					   '(lkb::arg-s lkb::kcmp)
					   '((lkb::args) (lkb::c-cont)
					     (lkb::sem)
					     (lkb::synsem lkb::local lkb::cont)))))
    (loop for slot in slot-alist
	for slot-hook = (construct-algebra-hook (car slot) t)
			;;; returns nil if slot hook isn't coindexed with
			;;; anything in rels or the main hook
			;;; ? do we allow slots just for coindexation
			;;; with other slots?  if so, this will 
			;;; incorrectly cause them to be deleted
	when slot-hook
	collect
	  (make-slot :hook slot-hook
		     :name (create-slot-name (cdr slot))))))

;;; for mrscomp grammar we want (lkb::sem) in the ignore-paths list

(defparameter *non-slot-features* '(lkb::SEM lkb::cont lkb::HOOK lkb::SYNSEM 
				    lkb::LOCAL lkb::cat lkb::val lkb::head))

(defun create-slot-name (paths)
  ;;; slot naming
  ;;; a slot gets named after the list of features on the trimmed path
  ;;; LIST features are interpreted so that we get
  ;;; COMPS1 etc
  ;;;
  ;;; in the case of multiple paths leading to the same 
  ;;; HOOK, we distinguish between uninteresting cases (e.g. features beginning
  ;;; with `--') and interesting cases, such as control where
  ;;; e.g. try shares its SPR hook with the COMP1 SPR1 hook (because
  ;;; the whole SPR is shared)
  ;;; given ((SPR FIRST SEM HOOK)(COMPS REST FIRST SPR FIRST SEM HOOK))
  ;;; we return SPR1/COMPS2.SPR1
  (let* ((first-feat (car lkb::*list-head*))
	 (rest-feat (car lkb::*list-tail*))
	 (names 
	  (loop for path in paths
	      collect
		(let* ((trimmed-path
			(remove-if #'(lambda (x) (member x *non-slot-features*)) path))
		       (path-with-counts
			(reverse
			 (trimmed-path-name trimmed-path 0 nil first-feat rest-feat))))
		  (string-left-trim '(#\.)
		   (apply #'concatenate 'string path-with-counts))))))
    (if (cdr names)
	(let ((sorted-names (sort names #'(lambda (x y)
					    (< (length x) (length y))))))
	  (format nil "~A/~{~A~}" (car sorted-names) (cdr sorted-names)))
      (car names))))

;;; FIX - GAP is a difference list

(defun trimmed-path-name (path count name-so-far first-feat rest-feat)
  (if (null path) name-so-far
    (progn 
      (cond ((eql (car path) first-feat)
	     (push (format nil "~A" (+ 1 count)) name-so-far)
	     (setf count 0))
	    ((eql (car path) rest-feat)
	     (setf count (+ 1 count)))
	    (t (let ((name (format nil ".~A" (car path))))
		 (push name name-so-far))))
      (trimmed-path-name (cdr path) count name-so-far first-feat rest-feat))))

;;; Checking that the algebra is obeyed

;;; Check an individual rule application

(defparameter *rule-algebra-table*
    '((head-complement-rule-0  1 nil)
      (head-complement-rule-1  1 (COMPS1))
      (head-complement-rule-2  1 (COMPS1 COMPS2) (2 3))
      (head-specifier-rule  2 (SPR1))
      (determiner-head-rule  1 (SPEC))
      (head-modifier-rule 2 (MOD))
      (modifier-head-rule 1 (MOD))
      (noun-modifier-rule 1 nil)
      (bare-pl-noun-rule 0 (DTR1))
      (passive-rule 1 nil) ; modifies slots
      (inversion-rule 1 nil)		; ditto
      (coord-rule 1 nil)		; wrong ...
      (head-gap-rule-1 1 nil) ; modifies slots
      (head-gap-rule-2 1 (COMP2))		; modifies slots
      (head-gap-rule-3 1 (COMP1))	; modifies slots
      (head-filler-rule 2 (GAP))))
      
;;;  rule-name   semhead slot

(defun check-algebra (sement edge-record)
  (setf *mrs-comparison-output-messages* nil)
  (let* ((*mrs-comparison-output-control* :save)
	 (rule (lkb::rule-id (lkb::edge-rule edge-record)))
	 (rule-record (assoc rule *rule-algebra-table* 
			     :test #'string-equal))) ; lose packages
    (if rule-record
	(let* ((dtrs (lkb::edge-children edge-record))
	       (edge-sements (loop for dtr in dtrs
				 when (lkb::edge-p dtr)
				 collect
				   (extract-sement
				    (lkb::edge-dag dtr) nil)))
	       (head (cadr rule-record))
	       (slots (caddr rule-record))
	       (order (cadddr rule-record))
	       (reconstructed-sement 
		(reconstruct-sement edge-sements
				    head slots order)))
	  (if reconstructed-sement
	      (compare-sements reconstructed-sement sement
			       (sement-equalities reconstructed-sement))
	    (mrs-comparison-output "Cannot reconstruct semet")))
      ;;; can't find rule
      (mrs-comparison-output "No rule ~A in rule table for algebra" rule)))
  *mrs-comparison-output-messages*)
	  

(defun reconstruct-sement (dtr-sements head slots order)
  (unless (and (integerp head) (>= head 0))
    (error "Invalid head ~A" head))
  (if (eql head 0)
      (mrs-comparison-output "Can't deal with C-CONT heads yet")
    (let ((head-dtr (elt dtr-sements (- head 1))))
      (unless head-dtr
	(error "No head ~A in dtr-sements" head))
      (cond ((= (length dtr-sements) 3)
	  ;;; allowing for quaternary etc really needs
	  ;;; recursive fn here
	     (if order
		 (let* ((first-dtr (elt dtr-sements (- (car order) 1)))
			(next-dtr (elt dtr-sements (- (cadr order) 1)))
		     ;;; worry about error checking later ...
			(first-slot (car slots))
			(next-slot (cadr slots))
			(new-sement
			 (binary-sement-construct head-dtr first-dtr
						  first-slot)))
		   (if new-sement
		       (binary-sement-construct new-sement next-dtr
						next-slot)))
	       (error "Ternary rule and no order")))
	    ((= (length dtr-sements) 2)
	     (let ((non-head-dtr (car (remove head-dtr dtr-sements)))) 
	       (binary-sement-construct
		head-dtr non-head-dtr (car slots))))
	    ((= (length dtr-sements) 1)
	     head-dtr)
	    (t (error "Rule must be max ternary"))))))

(defun binary-sement-construct (head-dtr non-head-dtr slot)
  (let ((slot-record (find slot (sement-slots head-dtr) 
			   :key #'slot-name :test #'string-equal)))
    (if slot-record
	(let* ((slot-hook (slot-hook slot-record))
	       (bindings (sement-hooks-equal-p 
			  slot-hook (sement-hook non-head-dtr) 
			  (sement-equalities head-dtr))))
	  (if bindings
	      (make-sement :hook (sement-hook head-dtr)
			   :slots (remove slot-record (sement-slots head-dtr))
			   :liszt
			    (append (sement-liszt head-dtr)
				    (sement-liszt non-head-dtr))
			   :h-cons 
			    (append (sement-h-cons head-dtr)
				    (sement-h-cons non-head-dtr))
			    :equalities bindings)
	    (mrs-comparison-output "Hook and slot incompatible")))
      (mrs-comparison-output "Slot not found"))))

(defun compare-sements (sement1 sement2 bindings)
    (if (setf bindings (sement-hooks-equal-p (sement-hook sement1)
					     (sement-hook sement2) bindings))
	(if (setf bindings (mrs-liszts-equal-p (sement-liszt sement1)
					       (sement-liszt sement2) 
					       t bindings))
	    (if (setf bindings
		  (hcons-equal-p (sement-h-cons sement1)
				 (sement-h-cons sement2)
				 bindings))
		bindings
	      (mrs-comparison-output "~%hcons difference ~A ~A"
		      (sement-h-cons sement1)
		      (sement-h-cons sement2)))
	  nil)				; message comes from mrs-liszt-equal-p
      nil))


(defun sement-hooks-equal-p (hook1 hook2 bindings)
  ;;; bindings may be nil when called
  (if (setf bindings (variables-equal 
		      (hook-index hook1)
		      (hook-index hook2) nil bindings))
      (if (setf bindings
	    (variables-equal
	     (hook-ltop hook1)
	     (hook-ltop hook2)
	     nil 
	     bindings))
	  (if (setf bindings
		(variables-equal 
		 (hook-xarg hook1)
		 (hook-xarg hook2)
		 nil
		 bindings))
	      bindings
	    (mrs-comparison-output "Mismatch in xarg"))
	(mrs-comparison-output "Mismatch in ltop"))
    (mrs-comparison-output "Mismatch in index")))


