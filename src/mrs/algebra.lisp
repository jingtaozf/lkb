;;; Copyright (c) 2004--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Checking that a grammar obeys the algebra for composition
;;; and extracting the components so that algebra MRSs can be displayed
;;; on parse trees.

;;; sement structures and print code are in basemrs.lisp

(in-package :mrs)

(defun algebra-available-p nil 
    (and *hook-path*
	 *psoa-liszt-path*
	 *psoa-rh-cons-path*))


;;; FIX
;;; variable naming - we would like this to be constant over a parse
;;; this implies calling the algebra construction code in such a way
;;; that variables are recognised as equivalent between phrases.
;;; This would be the case if we were using the single reconstructed FS from
;;; parse tree, but if we do that, then the RELS that appear on the diff list
;;; include things which shouldn't be on that node.  We might get round that, 
;;; but there will be other shared things too A possible solution is
;;; to exploit the *named-nodes* mechanism, store an alist of fs / variable
;;; name pairs associated with a window, and when we create a variable, 
;;; to look up that path in the reconstructed FS and use it to find the 
;;; variable.  I have added in appropriate arguments from the calling
;;; functions in lkb-acl-mrs.lisp and so on, but actually writing this
;;; code would be complicated.

;;; There's also an issue about the interaction between this and the standard
;;; MRS extraction code, again ignored for now.  In other words, we 
;;; need to make sure that there won't be a screw up if we call the MRS
;;; extraction code after having done this because of the *named-nodes*
;;; or whatever.

(defun extract-sement (parse-fs reconstructed-fs &optional start-num)
  (declare (ignore reconstructed-fs))
  (initialise-algebra start-num)
  (let ((sement 
	 (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
;;;    (mrs::output-algebra-sement1 sement 'mrs::simple-indexed t)
    sement))

(defun extract-rule-sement (rule-fs &optional start-num)
  (initialise-algebra start-num)
  (let ((sement 
	 (extract-algebra-from-rule-fs (lkb::tdfs-indef rule-fs))))
    sement))

(defstruct (reconstruction-result)
  match-p reconstructed-sement messages)

(defun extract-and-check-sement (parse-fs reconstructed-fs edge-record)
  (initialise-algebra)
  (let* ((sement 
	  (extract-algebra-from-fs (lkb::tdfs-indef parse-fs)
				   (lkb::tdfs-indef reconstructed-fs)))
	 (all-compares
	  (check-algebra sement edge-record))
	 (good-matches
	  (loop for res in all-compares
	      when (reconstruction-result-match-p res)
	      collect res))
	 (global-messages 
	  (if good-matches
	    (if (cdr good-matches)
	        (list "Multiple matches on algebra")
	      (list "Match on algebra"))
	    (if all-compares
		 '("No good matches on algebra")
	      (list "No results returned")))))
    (values global-messages (or good-matches all-compares)
				sement)))


(defun initialise-algebra (&optional dtr-num)
  (unless (and *hook-path*
	       *psoa-liszt-path*
	       *psoa-rh-cons-path*)
    (error "Grammar parameters not set for algebra use"))
  (setf *variable-generator* (create-variable-generator dtr-num))
  (setf *all-nodes* nil)
  (setf *named-nodes* nil))

(defun extract-algebra-from-fs (fs &optional reconstructed-fs)
  (declare (ignore reconstructed-fs))
  ;;; components are
  ;;; a) hook
  ;;; b) slots
  ;;; c) rels list
  ;;; d) hcons
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    (if (is-valid-fs sem-fs)
        (construct-algebra-sement sem-fs fs))))

(defun extract-algebra-from-rule-fs (fs)
  (let ((sem-fs (path-value fs *c-cont-path*)))
    (if (is-valid-fs sem-fs)
        (construct-algebra-sement sem-fs fs))))

(defun construct-algebra-sement (sem-fs full-fs)
  (let* ((hook-fs (path-value sem-fs *hook-path*))
         (liszt-fs (path-value sem-fs *psoa-liszt-path*))
         (h-cons-fs (path-value sem-fs *psoa-rh-cons-path*)))
    (unless hook-fs 
      (error "Missing hook"))
    (unless liszt-fs 
      (error "Missing rels"))
    (unless  h-cons-fs
      (error "Missing handle constraints"))
    (let* ((hook (construct-algebra-hook hook-fs nil))
	   (rels (nreverse (construct-liszt 
			    liszt-fs nil *variable-generator*)))
	  ;;; construct-liszt in mrsoutput
	   (hcons (nreverse (construct-h-cons 
			     h-cons-fs nil *variable-generator*)))
	         	  ;;; construct-h-cons in mrsoutput
	   (slots (construct-algebra-slots 
		   full-fs 
		   *algebra-ignore-paths*
		  *algebra-ignore-feats*)))
      ;;; let* because slots construction assumes other variables
      ;;; are already found to allow pruning of unused `slots'
      (make-sement :hook hook :slots slots
		   :liszt rels :h-cons hcons))))

(defun construct-algebra-hook (fs in-slot-p)
  ;;; this code is called when we originally create the hook,
  ;;; and also from inside the slot creation code (when in-slot-p 
  ;;; will be true).  The assoc
  ;;; of named-nodes is only relevant in the latter case - we are
  ;;; checking that the variables are coindexed with something since
  ;;; otherwise this is not an interesting slot.
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


(defun construct-algebra-slots (fs slot-ignore-paths 
					     slot-ignore-features)
  ;;; the code for the generator and the SEM-I that do fairly
  ;;; similar things don't quite do what's wanted.  
  ;;; This code walks over a feature structure, finding all
  ;;; paths in the syntax part of the FS that lead to a HOOK
  ;;; feature.  The HOOK is taken to be a slot, the path name
  ;;; is used to generate the slot name.
  ;;; 1. reentrancy - if we've seen the HOOK before, store 
  ;;; all paths that lead to it and decide on the name
  ;;; when we're done
  ;;; 2. don't worry about OPT etc.  The slot will be registered
  ;;; as a slot at the location where it exists.  The algebra
  ;;; checking code will allow slot removal.
  (let ((slot-alist
	 (lkb::collect-subdags-for-type fs *hook-type* 
					slot-ignore-features 
					slot-ignore-paths)))
    (loop for slot in slot-alist
	for slot-hook = (construct-algebra-hook (car slot) t)
			;;; returns nil if slot hook isn't coindexed with
			;;; anything in rels or the main hook
			;;; ?FIX do we allow slots just for coindexation
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
  ;;; HOOK, we distinguish between uninteresting cases
  ;;; and interesting cases, such as control where
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
;;;
;;; We reconstruct the daughter sements and then see if the
;;; mother sement we construct according to the algebra matches the
;;; sement we actually get from the feature structure.


(defun check-algebra (actual-sement edge-record)
  ;;; returns a list of reconstruction-results
  (unless (lkb::edge-p edge-record)
    (error "~%Edge expected"))
  (let* ((count 0)
    ;;; count is used as a starting count for variable numbering -
    ;;; the first daughter variables start at 1000, next at 2000 and so
    ;;; on.  This prevents name clashes
	 (rule-struct (lkb::edge-rule edge-record))
	 (*mrs-comparison-output-control* :save))
	       ;;; comparison messages are stored up and output
	       ;;; in a window etc at the end
    (if (and rule-struct (lkb::rule-p rule-struct))
	(let* ((dtrs (lkb::edge-children edge-record))
	       (edge-sements 
		(loop for dtr in dtrs
		    when (and (lkb::edge-p dtr) (lkb::edge-dag dtr))
		    collect
		      (progn (setf count (+ count 1000))
			     (extract-sement (lkb::edge-dag dtr) 
					     nil
					     count))))
	       (rule-sement (extract-rule-sement
			     (lkb::rule-full-fs rule-struct) (+ count 1000)))
	       (dtr-sements 
		(if edge-sements
		    (loop for poss-dtr in 
			  (if rule-sement
			      (cons rule-sement edge-sements)
			    edge-sements)
			when ;;; (sement-liszt poss-dtr)
			     ;;; possibly need to FIX this
			     
			    (or (sement-liszt poss-dtr)
				 (sement-slots poss-dtr)) 
			collect poss-dtr)))
	       ;; allow rule to be counted if it has a liszt
	       ;; discard things like `it' in `it rained'
	       (reconstructed-sements
		(reconstruct-sements actual-sement 
				     (or 
				      dtr-sements edge-sements))))
	  ;;; don't want to discard `it' if that's all we've got
	  ;;;
		;;; reconstruct-sements builds alternatives
		;;; and then tests them against the actual sement
	  (or reconstructed-sements
	      (list 
	       (make-reconstruction-result 
		:match-p nil :reconstructed-sement nil 
		:messages
		(list '(:message "Cannot reconstruct sement"))))))
      (list 
       (make-reconstruction-result 
	:match-p t
	:reconstructed-sement nil 
	:messages
	(list '(:message 
		"Sement corresponds to lexical entry: no check done")))))))

(defun reconstruct-sements (actual-sement dtr-sements)
  (loop for result in 	  
	(let ((*mrs-comparison-output-control* nil))
	  (cond ((cddr dtr-sements)
	 ;;; ternary and above rules (or binary plus c-cont)
		 (do-binary-sement-splits dtr-sements))
		((cdr dtr-sements)
		 (binary-combine-sements dtr-sements))
		(t (list (car dtr-sements)))))
	collect
	  (progn
;	    (format t "~%~%")
;	    (mrs::output-algebra-sement1 
;		   result 'mrs::simple-indexed t)
	    (setf *mrs-comparison-output-messages* nil)
	    (let ((match-p (compare-sements result actual-sement))) 
	      (make-reconstruction-result 
	       :match-p match-p
	       :reconstructed-sement result
	       :messages
	       *mrs-comparison-output-messages*)))))
	
(defun do-binary-sement-splits (dtr-sements)
  ;;; if there is more than one dtr (real, or c-cont)
  ;;; we have to try different tree structures here
  ;;; trying doing this in an entirely undirected fashion ...
  (if (cddr dtr-sements)
      (loop for poss in dtr-sements
	  append
	    (let ((rest-of-tree
		   (do-binary-sement-splits (remove poss dtr-sements))))
	      (let ((possibilities
		     (loop for res in rest-of-tree
			 append
			   (binary-combine-sements (list poss res)))))
		possibilities)))
    (binary-combine-sements dtr-sements)))
	    
(defun binary-combine-sements (dtr-sements)
  ;;; should be called only when there are two elements in dtr-sements 
  (loop for head-dtr in dtr-sements
      nconc
	(let* ((non-head-dtr (first (remove head-dtr dtr-sements)))
  ;;; when we're calling this, we don't know for sure which 
  ;;; slot is going to be filled.  So we try out all possibilities.
	       (slots (sement-slots head-dtr)))
	  (loop for slot-record in slots
	        for new-sement =
		   (make-sement-from-sements 
		    slot-record head-dtr non-head-dtr slots nil)
	      when new-sement
	      collect new-sement))))
	     
	  #|
	    (dolist (sement dtr-sements)
	      (format t "~%")
	      (mrs::output-algebra-sement1 sement 'mrs::simple-indexed t)))
	      |#

(defun make-sement-from-sements (slot-record head-dtr 
				 non-head-dtr slots interactive-p)
  (let* ((slot-hook (slot-hook slot-record))
	 (equalities (equate-sement-hooks 
		      slot-hook (sement-hook non-head-dtr)))
	 (head-eqs (cadr equalities))
	 (non-head-eqs (car equalities)))
    (when interactive-p
      (format t 
	      "~%Attempt to match ~A (~A) with hook ~A: ~A"
	      (make-readable-hook slot-hook)
	      (slot-name slot-record)
	      (make-readable-hook (sement-hook non-head-dtr))
	      (if equalities "succeeded" "failed")))
    (if equalities
	(make-sement 
	 :hook (canonicalise-sement-hook
		(copy-sement-hook
		 (sement-hook head-dtr)) head-eqs)
	 :slots 
	 (canonicalise-sement-slots
	  (copy-sement-slots
	   (remove slot-record slots)) head-eqs)
	 :liszt
	 (append (canonicalise-basemrs-liszt
		  (copy-liszt-completely
		   (sement-liszt head-dtr) nil) head-eqs)
		 (canonicalise-basemrs-liszt
		  (copy-liszt-completely
		   (sement-liszt non-head-dtr) nil) non-head-eqs))
	 :h-cons 
	 (append (canonicalise-basemrs-hcons-list
		  (copy-psoa-hcons 
		   (sement-h-cons head-dtr)) head-eqs)
		 (canonicalise-basemrs-hcons-list
		  (copy-psoa-hcons (sement-h-cons non-head-dtr))
		  non-head-eqs))))))

;;; Interactive application

(defparameter *slot-selection* nil)

(defun select-sement-slot-interactive (name sement)
  (setf  *slot-selection* (cons name sement)))

(defun select-sement-dtr-interactive (sement)
  ;;; only active if *slot-selection*
  (if *slot-selection*
    (let ((res (binary-combine-sements-interactive 
		(cdr *slot-selection*) (car *slot-selection*) sement)))
      (when res
	(lkb::show-mrs-sement-result-window res)))
    (lkb::show-message-window "No slot selected: 
(use pop up menu on slot in another sement)")))

(defun binary-combine-sements-interactive (head-dtr slot-name non-head-dtr)
  (let* ((slots (sement-slots head-dtr))
	 (slot-record (find slot-name slots :key #'slot-name :test #'equal)))
    (if slot-record
	(make-sement-from-sements slot-record head-dtr 
				  non-head-dtr slots t))))

(defun make-readable-hook (hook) 
  ;;; for debugging
  (let ((index (hook-index hook))
	(ltop (hook-ltop hook))
	(xarg (hook-xarg hook))) 
    (format nil "[~(~a~),~(~a~),~(~a~)]" 
	    (var-string ltop) 
	    (var-string index) 
	    (if xarg (var-string xarg)
	      "_"))))

;;; end interactive stuff

(defun compare-sements (sement1 sement2)
  ;;; this ignores slots, currently at least
  (let ((bindings nil)
	(*special-hack-for-message-types-p* 
	 *allow-sloppy-message-matching-p*))
    (if (setf bindings (sement-hooks-equal-p (sement-hook sement1)
					     (sement-hook sement2) bindings))
	(if (setf bindings (mrs-liszts-equal-p (sement-liszt sement1)
					       (sement-liszt sement2) 
					       nil bindings))
	    (if (setf bindings
		  (hcons-equal-p (sement-h-cons sement1)
				 (sement-h-cons sement2)
				 bindings))
		bindings
	      (mrs-comparison-output :hcons
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
		(mrs-comparison-output :message "Mismatch in xarg"))
	    bindings)
	(mrs-comparison-output :message "Mismatch in ltop"))
    (mrs-comparison-output :message "Mismatch in index")))

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
	   (mrs-comparison-output :message "Mismatch in index variables"))
	  ((and (not (hook-xarg non-head-hook))
		(not (hook-xarg head-hook)))
	   (list (append (car index-bindings) 
			   (car ltop-bindings))
		   nil))
	  ((null xarg-bindings)
	   (mrs-comparison-output  :message "Mismatch in xarg variables"))
	  ((null ltop-bindings)
	   (mrs-comparison-output  :message "Mismatch in ltop variables"))
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
;;; moved to basemrs.lisp


;;; Checking an entire parse
;;;
;;; The code is called on each edge in a chart
;;; this is designed to be called while going through some test suite
;;; lkb::*do-something-with-parse*

#|
(defparameter lkb::*do-something-with-parse* 'mrs::check-algebra-on-chart)
|#

(defparameter *algebra-problems* nil)

(defun check-algebra-on-chart nil
  ;;; packing must not be used
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (setf *algebra-problems* nil)
    (format ostream "~%~A" sentence)
    (loop
	for i from 0 to lkb::*chart-max*
	while (check-algebra-chart-entry (aref lkb::*chart* i 1) 
					 ostream))
    (unless *algebra-problems*
      (format ostream "~%   No algebra problems detected"))))
  

(defun check-algebra-chart-entry (item stream)
  (if item 
    (progn
      (dolist
         (configuration
            (sort (copy-list item)
               #'(lambda (span1 span2)
                   (cond
                      ((eql (lkb::chart-configuration-begin span1)
                          (lkb::chart-configuration-begin span2))
                         (< (lkb::edge-id (lkb::chart-configuration-edge span1))
                            (lkb::edge-id (lkb::chart-configuration-edge span2))))
                      (t
                        (< (lkb::chart-configuration-begin span1)
                           (lkb::chart-configuration-begin span2)))))))
        (check-algebra-chart-item configuration stream)))))

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
      (let ((rule-id (lkb::rule-id rule-struct))
	    (sement 
	     (extract-algebra-from-fs (lkb::tdfs-indef parse-fs))))
	(if sement
	  (let* ((all-compares
		  (check-algebra sement edge))
		 (good-matches
		  (loop for res in all-compares
		      when (reconstruction-result-match-p res)
		      collect res)))
	    (unless good-matches
	        (setf *algebra-problems* t)
		(format stream "~%   Problem on Edge ~A (rule ~A)" 
			edge-id rule-id)))
	  (progn 
	    (setf *algebra-problems* t)
	  (format stream 
		  "~%   Edge ~A: Sement structure could not be extracted" edge-id)))))))
