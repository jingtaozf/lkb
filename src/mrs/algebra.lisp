;;; Copyright (c) 2004--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Checking that a grammar obeys the algebra for composition
;;; and extracting the components so that algebra MRSs can be displayed
;;; on parse trees.

;;; sement structures and print code are in basemrs.lisp

(in-package :mrs)

(defun algebra-available-p nil
  *rule-algebra-table*)

;;; FIX
;;; variable naming - we would like this to be constant over a parse
;;; this implies calling the algebra construction code in such a way
;;; that variables are recognised as equivalent between phrases.
;;; The best way to do this is probably to do something INSTLOC-like
;;; but ignore for now.
;;; There's also an issue about the interaction between this and the standard
;;; MRS extraction code, again ignored for now.

(defun extract-sement (parse-fs &optional start-num)
  (initialise-algebra start-num)
  (let ((sement 
	 (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
    sement))

(defun extract-and-check-sement (parse-fs edge-record)
  (initialise-algebra)
  (let ((sement 
	 (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
    (check-algebra sement edge-record)))

(defun initialise-algebra (&optional dtr-num)
  (unless (and *hook-path*
	       *psoa-liszt-path*
	       *psoa-rh-cons-path*)
    (error "Grammar parameters not set for algebra use"))
  (setf *variable-generator* (create-variable-generator dtr-num))
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
					*algebra-ignore-feats*
					*algebra-ignore-paths*)))
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

(defun create-slot-name (paths)
  ;;; slot naming
  ;;; a slot gets named after the list of features on the trimmed path
  ;;; list features are interpreted so that we get
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
			(remove-if #'(lambda (x)
				       (or (eql x lkb::*diff-list-list*)
					   (member x *non-slot-features*))) 
				   path))
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

;;; Check an individual rule application.
;;; This relies on *rule-algebra-table* which should be specified
;;; in the mrsglobals file for the grammar for now
;;;
;;; We reconstruct the daughter sements and then see if the
;;; mother sement we construct according to the algebra matches the
;;; sement we actually get from the feature structure.

(defun check-algebra (sement edge-record)
  (setf *mrs-comparison-output-messages* nil)
  (let ((reconstructed-sement nil)
	(count 0))
    ;;; count is used as a starting count for variable numbering -
    ;;; the first daughter variables start at 1000, next at 2000 and so
    ;;; on.  This prevents name clashes.
    (if (lkb::edge-p edge-record)
	(let* ((rule-struct (lkb::edge-rule edge-record))
	       (*mrs-comparison-output-control* :save)
	       ;;; comparison messages are stored up and output
	       ;;; in a window etc at the end
	       (rule (if (lkb::rule-p rule-struct) (lkb::rule-id rule-struct))) 
	       (rule-record (if rule (assoc rule *rule-algebra-table* 
					    :test #'string-equal)))) ; lose packages
	  (if rule
	      (let ((dtrs (if (lkb::edge-morph-history edge-record)
			      (list (lkb::edge-morph-history edge-record))
			    (lkb::edge-children edge-record))))
		(if rule-record    
		    (let* ((edge-sements (loop for dtr in dtrs
					     when (and (lkb::edge-p dtr)
						       (lkb::edge-dag dtr))
					     collect
					       (progn
						 (setf count (+ count 1000))
						 (extract-sement
						  (lkb::edge-dag dtr) count))))
			   (head (cadr rule-record))
			   (slots (caddr rule-record))
			   (order (cadddr rule-record)))
		      (when edge-sements
			(setf reconstructed-sement
			  (reconstruct-sement edge-sements
					      head slots order)))
		      (if reconstructed-sement
			  (compare-sements reconstructed-sement sement)
			(mrs-comparison-output "Cannot reconstruct sement")))
		  (if (cdr dtrs)
      ;;; can't find rule
		      (mrs-comparison-output 
		       "Warning: Non-unary rule ~A missing ~%from algebra table" rule)
		    (mrs-comparison-output 
		     "Unary rule ~A not in algebra table: ~%algebra OK unless C-CONT" rule))))
	    (mrs-comparison-output 
	     "Sement corresponds to lexical entry: no check done"))))
    (values *mrs-comparison-output-messages* reconstructed-sement)))
	  

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
	       (equalities (equate-sement-hooks 
			    slot-hook (sement-hook non-head-dtr)))
	       (head-eqs (cadr equalities))
	       (non-head-eqs (car equalities)))
	  (if equalities
	      (make-sement :hook (canonicalise-sement-hook
				  (sement-hook head-dtr)
				  head-eqs)
			   :slots 
			   (canonicalise-sement-slots
			    (remove slot-record (sement-slots head-dtr))
			    head-eqs)
			    :liszt
			    (append (canonicalise-sement-liszt
				     (sement-liszt head-dtr)
				     head-eqs)
				    (canonicalise-sement-liszt
				     (sement-liszt non-head-dtr)
				     non-head-eqs))
			    :h-cons 
			    (append (canonicalise-sement-hcons-list
				     (sement-h-cons head-dtr)
				     head-eqs)
				    (canonicalise-sement-hcons-list
				     (sement-h-cons non-head-dtr)
				     non-head-eqs)))
	    (mrs-comparison-output "Hook and slot incompatible")))
      (mrs-comparison-output "Slot not found"))))

(defun compare-sements (sement1 sement2)
  (let ((bindings nil))
    (if (setf bindings (sement-hooks-equal-p (sement-hook sement1)
					     (sement-hook sement2) bindings))
	(if (setf bindings (mrs-liszts-equal-p (sement-liszt sement1)
					       (sement-liszt sement2) 
					       nil bindings))
	    ;;; should be t here instead of nil
	    ;;; to make the check `syntactic' (i.e. fussy)
	    ;;; but FIX to make `u's into `x's is needed first
	    (if (setf bindings
		  (hcons-equal-p (sement-h-cons sement1)
				 (sement-h-cons sement2)
				 bindings))
		bindings
	      (mrs-comparison-output "~%hcons difference ~A ~A"
				     (sement-h-cons sement1)
				     (sement-h-cons sement2)))
	  nil)				; message comes from mrs-liszt-equal-p
      nil)))


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
	  (if (and (hook-xarg hook1) (hook-xarg hook2))
	      (if (setf bindings
		    (variables-equal 
		     (hook-xarg hook1)
		     (hook-xarg hook2)
		     nil
		     bindings))
		  bindings
		(mrs-comparison-output "Mismatch in xarg"))
	    bindings)
	(mrs-comparison-output "Mismatch in ltop"))
    (mrs-comparison-output "Mismatch in index")))

(defun equate-sement-hooks (head-hook non-head-hook)
  ;;; this checks for compatibility of variable types (via variables-equal)
  ;;; and returns two lists of equalities, which are later interpreted as 
  ;;; replacement instructions. first is for the non-head-dtr
  ;;; second (generally empty) for the head-dtr
  ;;; a) h1 x2 x3 maps to h4 x5 x6
  ;;;    (((4 . 1) (5 . 2) (6 . 3)) nil)
  ;;; b) h1 x2 x2 maps to h4 x5 x5
  ;;;    (((4 . 1) (5 . 2) (5 . 2)) nil)
  ;;; second 5 . 2 is redundant but not harmful
  ;;; c) h1 x2 x2 maps to h4 x5 x6
  ;;;    (((4 . 1) (5 . 2) (6 . 2)))
  ;;; d) h1 x2 x3 maps to h4 x5 x5
  ;;;    (((4 . 1) (5 . 2))  ((3 . 2)))
  ;;; note this could be
  ;;;    h1 x2 x3 maps to h1 x2 x2
  ;;;    giving
  ;;;    (((1 . 1) (2 . 2)) (3 . 2))
  ;;; this code uses the MRS comparison code but treats each variable
  ;;; as distinct.  The bindings 
  ;;; returned by the MRS code are lists of assoc lists
  ;;; to allow for alternatives - but here can assume just
  ;;; one possible binding
  ;;; Note the order of the variables in the call to variables-equal
  (let* ((index-bindings (variables-equal 
			  (hook-index non-head-hook)
			  (hook-index head-hook)
			  nil nil))
	 (ltop-bindings (variables-equal
			 (hook-ltop non-head-hook)
			 (hook-ltop head-hook)
			 nil nil))
	 (xarg-bindings (variables-equal 
			 (hook-xarg non-head-hook)
			 (hook-xarg head-hook)
			 nil nil)))
    (cond ((null index-bindings)     
	   (mrs-comparison-output "Mismatch in index variables"))
	  ((and (not (hook-xarg non-head-hook))
		(not (hook-xarg head-hook)))
	   (list (append (car index-bindings) 
			   (car ltop-bindings))
		   nil))
	  ((null xarg-bindings)
	   (mrs-comparison-output "Mismatch in xarg variables"))
	  ((null ltop-bindings)
	   (mrs-comparison-output "Mismatch in ltop variables"))
	  ((eql-var-id (hook-index non-head-hook) (hook-xarg non-head-hook))
	   ;;; case d - the more complicated one
	   ;;; take index as canonical
	   (list (append (car index-bindings) 
			 (car ltop-bindings))
		 (list (cons (get-var-num (hook-xarg head-hook))
			     (get-var-num (hook-index head-hook))))))
	  (t (list (append (car index-bindings) 
			   (car xarg-bindings) 
			   (car ltop-bindings))
		   nil)))))


;;; ******** Code to reset variables to canonical ids *********
;;; cf rmrs/comp.lisp 
;;; destrctive

(defun canonicalise-sement-hook (hook bindings)
  (canonicalise-sement-variable (hook-index hook) bindings)
  (when (hook-xarg hook)			   
    (canonicalise-sement-variable (hook-xarg hook) bindings))
  (canonicalise-sement-variable (hook-ltop hook) bindings)
  hook)

(defun canonicalise-sement-slots (slots bindings)
  (dolist (slot slots)
    (canonicalise-sement-hook (slot-hook slot) bindings))
  slots)

(defun canonicalise-sement-liszt (liszt bindings)
  (dolist (ep liszt)
    (canonicalise-sement-variable (rel-handel ep) bindings)
    (dolist (fvp (rel-flist ep))
      (let ((value (fvpair-value fvp)))
	(when (var-p value) 
	  (canonicalise-sement-variable value bindings)))))
  liszt)


(defun canonicalise-sement-variable (var bindings)
  (let* ((var-id (var-id var))
	 (replace-value (cdr (assoc var-id bindings))))
    (when replace-value
	(setf (var-id var) replace-value))))

(defun canonicalise-sement-hcons-list (hcons-list bindings)
  (dolist (hcons hcons-list)
    (canonicalise-sement-variable
     (hcons-scarg hcons) bindings)
    (canonicalise-sement-variable
     (hcons-outscpd hcons) bindings))
  hcons-list)
  
;;; Checking an entire parse
;;;
;;; The code is called on each edge in a chart
;;; this is designed to be called while going through some test suite
;;; lkb::*do-something-with-parse*

#|
(defparameter lkb::*do-something-with-parse* 'mrs::check-algebra-on-chart)
|#

(defun check-algebra-on-chart nil
  ;;; packing must not be used
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%~A~%" sentence)
    (loop
	for i from 1 to lkb::*chart-limit*
	while (check-algebra-chart-entry i (aref lkb::*chart* i 0) 
					 ostream))))
  

(defun check-algebra-chart-entry (vertex item stream)
  (if item 
    (progn
      (dolist
         (configuration
            (sort (copy-list (lkb::chart-entry-configurations item))
               #'(lambda (span1 span2)
                   (cond
                      ((eql (lkb::chart-configuration-begin span1)
                          (lkb::chart-configuration-begin span2))
                         (< (lkb::edge-id (lkb::chart-configuration-edge span1))
                            (lkb::edge-id (lkb::chart-configuration-edge span2))))
                      (t
                        (< (lkb::chart-configuration-begin span1)
                           (lkb::chart-configuration-begin span2)))))))
        (check-algebra-chart-item configuration stream))
      t)
    (aref lkb::*morphs* vertex)))

(defun check-algebra-chart-item (item stream)
  (let* ((edge (if (lkb::edge-p item) 
		   item 
		 (lkb::chart-configuration-edge item)))
	 (edge-id (if (lkb::edge-p edge) (lkb::edge-id edge)))
	 (parse-fs (if (and (lkb::edge-p edge) 
			      (lkb::edge-dag edge)
			      (lkb::tdfs-p (edge-dag edge)))
			 (lkb::edge-dag edge)))
	 (rule-struct (lkb::edge-rule edge)))
    (when (and parse-fs (lkb::rule-p rule-struct))
      (initialise-algebra)
      (let ((sement 
	     (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
	(if sement
	  (let ((messages
		 (check-algebra sement edge)))
	    (when messages
		(format stream "Edge ~A~%" edge-id)
		(dolist (message messages)
		  (format stream "~A~%" message stream))))
	  (format stream 
		  "~%Edge ~A: Sement structure could not be extracted" edge-id))))))
