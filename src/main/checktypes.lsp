;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

;;; Checking the type hierarchy to see if it meets 
;;; the various constraints

;;; general purpose functions for creating new atoms ...

(defun next (template)
       (let ((instance nil)
             (number (+ (or (get template 'last-number)
                            0)
                        1)))
         (setf (get template 'last-number)
               number)
         (setf instance
               (intern
                 (concatenate 'string
                              (string template)
                              (princ-to-string number))))
         (push instance
               (get template 'children))
         (setf (get instance 'root-template)
               template)
         instance))

;;; and destroying them

(defun scratch (templates)
;;; see the function Next - scratch removes all info from the template symbols
;;; used by Next and in effect reinitialises the values.  It can take a single
;;; item or a list of templates to be reinitialised.
    (dolist (template (if (listp templates) templates (list templates)))
	    (remprop template 'last-number)
	    (dolist (child (get template 'children))
		    (setf (symbol-plist child)
			  nil))
	    			; a bit drastic - have to hope nothing
				; important is kept on p-list by anyone else
	    (remprop template 'children)))


;;; Functions to set up type hierarchy from an input file

(defparameter *leaf-type-addition* nil)

(defun add-type-from-file (name parents constraint default comment &optional daughters)
  ;;; YADU --- extra arg needed
  (if *leaf-type-addition*
      (add-leaf-type name parents constraint default comment daughters)
  (let ((existing-type (get-type-entry name))
        (real-parents nil)
        (template-parents nil))
      (when existing-type
         (format t "~%Type ~A redefined" name))
      (when (and *templates* (cdr parents))
        (for parent in parents
             do
             (if (member parent *templates* :test #'eq)
               (push parent template-parents)
               (push parent real-parents)))
        (unless real-parents
          (setf real-parents (list (car template-parents)))
          (setf template-parents (cdr template-parents))))
      (let ((new-type 
               (make-type :name name 
                  :parents (or real-parents parents) 
                  :template-parents template-parents
                  :daughters daughters
                  :comment comment
                  :constraint-spec constraint
                  :default-spec default
                  :enumerated-p (if daughters t))))
         (create-mark-field new-type)
         (when (null parents)
            (when (and *toptype* (not (eq *toptype* name)))
               (error "Two top types ~A and ~A have been defined" 
                  *toptype* name))
            (setf *toptype* name))
         (set-type-entry name
            new-type)))))

(defun amend-type-from-file (name parents constraint default comment)
  (let ((ok t)
        (existing-type (get-type-entry name)))    
    (if existing-type
	(if (type-parents-equal parents (type-parents existing-type)
                                (type-real-parents existing-type))
	    (progn 
	      (setf (type-constraint-spec existing-type) constraint)
	      (setf (type-default-spec existing-type) default)
	      (setf (type-comment existing-type) comment))
          (progn
            (format t "~%Warning - ~A ignored - patch cannot change type hierarchy"
                    name)
            (setf ok nil)))
      (progn
        (setf ok nil)
        (format t "~%Warning - ~A ignored - patch only works to redefine types"
                name)))
    ok))

(defun type-parents-equal (new-parents old-parents old-real-parents)
  (let ((test-parents (or old-real-parents old-parents))
        (actual-new-parents (for parent in new-parents
                                 filter
                                 (unless (member parent *templates*)
                                   parent))))
    (and (null (set-difference actual-new-parents test-parents))
         (null (set-difference test-parents actual-new-parents)))))

(defvar *type-names* nil)

(defun amend-type-information (new-type new-type-entry parents daughters)
  ;;
  ;; called from fix-mglb
  ;;
  ;; takes a new type-name and its entry (this should have parents and
  ;; daughters specified, plus any idiosyncratic information) a list of its
  ;; parent types and of its daughter types (it is assumed that these will not
  ;; add a cycle, and that they are valid types, so if this function is called
  ;; from code which gets new specs from a user, the calling function must
  ;; perform the checks). the fn does the following:
  ;; 1) sets the new type entry
  ;; 2) amends all the daughter types so that their parents 
  ;;    are correct (remove any redundant links to the old parents)
  ;; 3) amends all the parents in a similar way
  ;; 3') adds the ancestors and descendants to the type entry
  ;; 4) adds the new type to the descendants list of all its ancestors
  ;; 5) adds the new type to the ancestors list of all its descendants
  ;; 5') pushes the new type onto the list of ordered glbtypes
  ;;     (used for output)
  ;; 6) pushes the new type onto the list of *type-names*
  (let ((ancestors nil)
        (descendants nil))
    (create-mark-field new-type-entry)
    (set-type-entry new-type new-type-entry)   
    (dolist (dtr daughters)
      (let ((dtr-entry (get-type-entry dtr)))
	(setf (type-parents dtr-entry)
	  (cons new-type (set-difference (type-parents dtr-entry) 
					 parents :test #'eq)))
	(setf descendants (union descendants 
				 (type-descendants dtr-entry) :test #'eq))
	(push (get-type-entry dtr) descendants)))
    (dolist (parent parents)
      (let ((par-entry (get-type-entry parent)))
	(setf (type-daughters par-entry)
	  (cons new-type (set-difference (type-daughters par-entry) 
					 daughters :test #'eq)))
	(setf ancestors (union ancestors 
			       (type-ancestors par-entry) :test #'eq))
	(push (get-type-entry parent) ancestors)))
    (setf (type-descendants new-type-entry) descendants)
    (setf (type-ancestors new-type-entry) ancestors)
    (dolist (ancestor ancestors)
      (push new-type-entry (type-descendants ancestor)))
    (dolist (descendant descendants)
      (push new-type-entry (type-ancestors descendant)))
    (push new-type *ordered-glbtype-list*)
    (push new-type *type-names*)
    new-type-entry))
        
;;; Checking

(defparameter *partition* nil)

(defparameter *glbsets* nil)

(defparameter *hierarchy-only-p* nil)

(defparameter *display-glb-messages* nil
   "if set, informs user of glbtypes as they are created")


(defun check-type-table nil
   (scratch 'glbtype)
   (format t "~%Checking type hierarchy")
   (setf *type-names* (collect-type-names))
   (when *toptype*
     (when 
         (add-daughters-to-type-table)
       (when (check-for-cycles-etc *toptype*)
         (unmark-type-table)
         (format t "~%Checking for unique greatest lower bounds") 
	 (setq *partition* nil)
         (find-good-partitions *toptype*)
         (unmark-type-table)
         (for partition in *partition*
              do
              (check-for-unique-glbs partition)
                 ;;; partition is now a list of lists of types
                 (when *glbsets* 
                   (unmark-type-table)
                   (format t 
                           "~%Partition size ~A" 
                           (length partition))
                   (fix-mglbs)))
         (unmark-type-table)
         (if *hierarchy-only-p*
               (expand-local-only-constraints)
           (progn
             (format t "~%Expanding constraints")
           (when (expand-and-inherit-constraints)
             (format t "~%Making constraints well formed")
             (when (strongly-type-constraints)
               (optimise-check-unif-paths)
                ;;; YADU --- extra expansion stage
                ;;; earlier stages are unchanged
               (when (expand-type-hierarchy-defaults)
                 (format t "~%Type file checked successfully")
		 (gc-types)
                 (clear-type-cache)	; not for consistency, but for efficiency
                 t)))))))))


(defun patch-type-table nil
  ;;; added for the case where definitions are changed, but the hierarchy
  ;;; itself is unaltered
  (clear-types-for-patching-constraints)
  (for name in *type-names*
         do
         (let* ((type-entry (get-type-entry name)))
           (when (leaf-type-p type-entry)
             (setf (leaf-type-expanded-p type-entry) nil))
           (setf (type-constraint type-entry) nil)
           (setf (type-atomic-p type-entry) nil)
           (setf (type-tdfs type-entry) nil)
           (setf (type-appfeats type-entry) nil)
           (setf (type-constraint-mark type-entry) nil)          
           (setf (type-local-constraint type-entry) nil)))
  (unmark-type-table)  
  #+:allegro (when *gc-before-reload* (gc t))
                ;; try and force it to reclaim space before we refill it
  (format t "~%Expanding constraints")
  (when (expand-and-inherit-constraints)
    (format t "~%Making constraints well formed")
    (when (strongly-type-constraints)
       ;;; YADU --- extra expansion stage
       ;;; earlier stages are unchanged
;      (format t "~%Expanding defaults") 
      (when (expand-type-hierarchy-defaults)
	(format t "~%Re-expanding rules")
	(expand-rules) ; in rules.lsp
	(format t "~%Type file checked successfully")
	t))))


;;; First we need to check that the type hierarchy itself is OK
;;; viewed as a graph without considering the constraints
;;; There are several relevant conditions:
;;; connectedness
;;; existance of top
;;; no cycles
;;; unique greatest lower bound
;;;
;;; ;;; no unary branches test is now removed
;;;
;;; we also check for redundant links - where a node is both
;;; an immediate and a non-immediate daughter of another

(defun add-daughters-to-type-table nil
   ;; checks for correctness of parent specs
   (let ((ok t))
      (for name in *type-names*
         do
         (let* ((type-entry (get-type-entry name))
                (parents (type-parents type-entry)))
           (setf (type-real-parents type-entry)
             (type-parents type-entry))
           ;;; type-parents gets reset by glb code
           (for parent in parents
                do
                (let ((parent-entry (get-type-entry parent)))
                  (cond 
                   (parent-entry 
                    (pushnew name 
                             (type-daughters parent-entry)
                             :test #'eq))
                   (t (setf ok nil)
                      (format t
                              "~%~A specified to have non-existent parent ~A"
                              name parent)))))))
      ok))

(defun check-for-cycles-etc (top)
  (let ((top-entry (get-type-entry top)))
      (mark-node-active top-entry)
      (when (mark-for-cycles top-entry)
        (unmark-type-table)
        (if (mark-for-redundancy top-entry)
            (scan-table)
          (progn (find-all-redundancies)
                 ;;; tell user about all the problems
                 nil)))))
   
;;; John's algorithm for marking a graph to check for cycles 
;;; 1. start from top - mark node as active and seen
;;; 2. go depth first - marking all nodes as active and seen 
;;; until you hit a leaf node.  Mark this as seen (but not active)
;;; go back up - when all leaf nodes of a parent have been marked
;;; remove its active mark etc
;;; 3. If you find a node marked as seen but not active then
;;; treat it as a leaf node
;;; 4. If at any point you find that you are about to mark an already 
;;; active node then you have a cycle

;;; marks.lsp contains the marking structures and functions

(defun mark-for-cycles (type-record)
    (let ((ok t))
      (unless (seen-node-p type-record) 
        (mark-node-seen type-record)
        (setf ok 
          (for daughter in 
               (type-daughters type-record)
               all-satisfy
               (let ((daughter-entry (get-type-entry daughter)))
                 (if (active-node-p daughter-entry)
                     (progn
                       (format t "~%Cycle involving ~A" 
                               daughter)
                       nil)
                   (progn
                     (mark-node-active daughter-entry)
                     (let ((inner-ok 
                            (mark-for-cycles daughter-entry)))
                       (unmark-node-active daughter-entry)
                       inner-ok)))))))
      ok))


;;; checking for redundant links
;;;
;;; mark all daughters of the node as active
;;; marking an already active node indicates a redundant link
;;; recurse on each daughter
;;; only unmark as active when all daughters have been checked
;;; the algorithm requires that some parts of the hierarchy 
;;; will be scanned more than once

(defun mark-for-redundancy (type-record)
  ;; assumes no cycles
  (mark-node-seen type-record)
  ;; this is here because it's used in the next phase
  ;; of checking
  (let ((ok t)
        (daughters (for d in (type-daughters type-record)
                         collect (get-type-entry d))))
    (for daughter in daughters
         do
         (if (active-node-p daughter)
             (progn 
               (setf ok nil)
               (format t "~%Redundancy involving ~A" 
                       (type-name daughter)))
           (mark-node-active daughter)))
    (when ok
      (setf ok 
        (for daughter in daughters
             all-satisfy
             (mark-for-redundancy daughter)))
      (for daughter in daughters
           do
           (unmark-node-active daughter)))
    ok))


(defun scan-table nil
   (let ((ok t))
   (maphash
      #'(lambda (name type-entry)
         ;; check for unconnected nodes
         (unless (seen-node-p type-entry)
            (setf ok nil)
            (format t
                        "~%~A not connected to top"
                        name))
          ;; unmark the node
         (clear-marks type-entry))
;;; removed unary branch stuff - can't imagine anyone wanting to
;;; even be warned 
      *types*)
   (when ok
     (set-up-descendants *toptype*)
     (add-ancestors-to-type-table *toptype*))
   ok))

(defun set-up-descendants (type)
  (let ((type-entry (get-type-entry type)))
    (or (type-descendants type-entry)
        (let ((daughters (type-daughters type-entry))
              (descendants nil))
          (dolist (daughter daughters)
	    (pushnew (get-type-entry daughter) descendants :test #'eq)
	    (dolist (descendant (set-up-descendants daughter))
	      (pushnew descendant descendants :test #'eq)))
          (setf (type-descendants type-entry) descendants)
          descendants))))


(defun add-ancestors-to-type-table (top-node)
  (declare (ignore top-node))
  (for type in *type-names*
       do
       (let ((type-entry (get-type-entry type)))
         (calculate-ancestors type-entry))))

(defun calculate-ancestors (type-entry)
  (or (type-ancestors type-entry)
      (let* ((parents (type-parents type-entry))
             (ancestors nil))
        (dolist (parent parents)
	  (pushnew (get-type-entry parent) ancestors :test #'eq)
	  (dolist (ancestor (calculate-ancestors (get-type-entry parent)))
	    (pushnew ancestor ancestors :test #'eq)))
        (setf (type-ancestors type-entry) ancestors)
        ancestors)))

(defun unmark-type-table nil
   (maphash 
      #'(lambda (node type-entry)
           (declare (ignore node))
           (clear-marks type-entry))
        *types*))


(defun find-good-partitions (type)
  ;; doing the glb stuff over the entire hierarchy is very slow when we have
  ;; lots of multiple inheritance.  This function attempts to find manageable
  ;; subchunks, returning a list of lists of nodes which should be mutually
  ;; independent 
  ;; AAC - Oct 12 1998 - faster version
  (let* ((type-entry (get-type-entry type))
         (daughters (type-daughters type-entry)))
    (unless (or (active-node-p type-entry)
                (seen-node-p type-entry))
      (mark-node-active type-entry)
      (when daughters
        (let ((descendants (type-descendants type-entry)))
          (dolist (daughter daughters)
	    (find-good-partitions daughter))
          (when 
	      (for descendant in descendants
                   all-satisfy
		   (or (seen-node-p descendant)
		       (and (or (null (cdr (type-parents descendant)))
				(subsetp (type-parents descendant)
					 (mapcar #'type-name descendants)
					 :test #'eq)))))
            (let ((partition-nodes
                   (for descendant in descendants
                        filter
			(when (not (seen-node-p descendant))
			  (mark-node-seen descendant)
			  descendant))))
	      (when partition-nodes
		(push partition-nodes *partition*)))))))))

;;; GLB stuff

(defparameter *interesting-types* nil)
 
(defstruct (glbset)
  top bottom ignore)

(defun check-for-unique-glbs (partition-nodes)
  (setf *glbsets* nil)
  (setf *interesting-types* nil)
  (let ((ancestors nil))
    (dolist (type partition-nodes)
      (when (cdr (type-parents type))                   
	(dolist (ancestor (type-ancestors type))
	  (when (member ancestor partition-nodes :test #'eq)
	    (pushnew ancestor ancestors :test #'eq)))))
    (setf *interesting-types* ancestors)
    (check-for-unique-glbs-from-top ancestors)))

(defun check-for-unique-glbs-from-top (possible-nodes)
  (mapl #'(lambda (remaining-nodes)
            (let ((x (car remaining-nodes)))
	      (mapc #'(lambda (y)
			(let ((problem-list (check-lbs-from-top x y)))
			  (when problem-list
			    (push (make-glbset :top (list x y) 
					       :bottom problem-list) 
				  *glbsets*))))
		    (cdr remaining-nodes))))
        possible-nodes))

;;; this is the bottleneck in glb computation, taking more than 90% of the
;;; cpu time. The functions looks a bit odd but that's because they're
;;; optimised

(defvar *wanted* nil)
     
(defun check-lbs-from-top (x y)
  (let ((xdescendants (type-descendants x))
        (ydescendants (type-descendants y)))
    (unless (or (member y xdescendants :test #'eq) 
                (member x ydescendants :test #'eq))
      (let ((*wanted* nil))
	(find-highest-intersections xdescendants ydescendants nil)
	(when (cdr *wanted*) *wanted*)))))


(defun find-highest-intersections (xs ys intersectp)
   ;; find set of highest intersections between 2 sets of types - intersectp
   ;; records whether any intersection has been detected. If there hasn't,
   ;; test first if the next type in xs is a member of ys. If there has,
   ;; then quickest strategy is to see whether type has been marked (i.e. is
   ;; a descendent of an intersect point) so need not even be tested for
   ;; membership of ys. (In former case the member test must come first
   ;; otherwise type lookup slows things down) At the end, collect highest
   ;; intersections and unmark the types in xs. A type can only have been
   ;; marked if intersectp is true, since intersectp is true for every type
   ;; in xs that is in ys (either it's true on entry, or it's set to true
   ;; after the member test in the second branch of the if test)
  (when xs
    (let ((type (car xs)))
      (when 
	  (if intersectp
	      ;; specialise order of tests depending on intersectp
	      (and (not (seen-node-p type))
		   (member type ys :test #'eq))
	    (and (member type ys :test #'eq)
		 (setq intersectp t)
		 (not (seen-node-p type))))
	(mark-node-seen type)
	(dolist (desc (type-descendants type))
	  (mark-node-active-and-seen desc)))
      (find-highest-intersections (cdr xs) ys intersectp)
      (cond ((null intersectp))
	    ((not (seen-node-p type)))
	    ((not (active-node-p type))
	     ;; seen but not active, so is a highest intersection
	     (push type *wanted*)
	     (clear-marks type))
	    (t (clear-marks type))))))


;;;; July 1996 new stuff to fix mglbs

;;; the glb checking code puts records of problems on the global *glbsets*
;;; We find start from the bottom, fixing a set which has no lower sets in
;;; the problem list The fix consists of adding a type with parents of the
;;; types which have mglbs and daughters equal to the mglbs and removing any
;;; redundant links.  We then make fixes to *glbsets*


(defun fix-mglbs nil
  ;; partition specific because of *interesting-types*
  (format t "~%Elements in *interesting-types* ~A" 
	  (length *interesting-types*))
  (loop 
    (let* ((minset (find-minimal-glbset *glbsets*))
	   ;; find candidate problem to fix 
	   (new-type-entry (fix-mglb minset)))
      ;; fix-mglb now also redoes the type hierarchy info
      (when *display-glb-messages*
	(format t "~%Fixing ~A with glbs ~A" 
		(mapcar #'type-name (glbset-top minset))
		(mapcar #'type-name (glbset-bottom minset))))
      (modify-glbsets new-type-entry *interesting-types*)
      ;; (push new-type *interesting-types*)
      ;; apparently glbtypes cannot be interesting - there are never
      ;; cases where we get multiple glbs between glbtypes (presumably
      ;; because glbtypes are always minimal)
      (unless *glbsets*			; have we fixed it yet?
	(format t "~%Checked")
	(return t)))))

(defun modify-glbsets (new-type-entry nodes)
  (let* ((ancestors (type-ancestors new-type-entry)))
    (dolist (glbrec *glbsets*)
      (let* ((pair (glbset-top glbrec)))
	(when (and (member (car pair) ancestors :test #'eq)
		   (member (cadr pair) ancestors :test #'eq))
	  (let ((new-probs (check-lbs-from-top (car pair) (cadr pair))))
	    (if new-probs
		(setf (glbset-bottom glbrec) new-probs)
	      (setf (glbset-ignore glbrec) t))))))
    (dolist (node nodes)
      (let ((problem-list (check-lbs-from-top new-type-entry node)))
	(when problem-list
	  (push (make-glbset :top (list new-type-entry node) 
			     :bottom problem-list) 
		*glbsets*))))
    (setf *glbsets* 
      (delete-if #'(lambda (x) (glbset-ignore x))
		 *glbsets*))))


;;; code to find best candidate to fix

(defun find-minimal-glbset (glbs)
  ;; find a set x which has no other top set y which is more specific - that
  ;; is there is no y which contains both an element e such that e is strictly
  ;; subsumed by a member of x, and an element f which is subsumed by or equal
  ;; to another member of x
  (let* ((candidate (car glbs))
         (newset
          (do ((rest (cdr glbs) (cdr rest)))
              ((null rest) nil)
            (when (glb-subsum-test candidate (car rest))
              (return rest)))))
    (if newset
	(find-minimal-glbset newset)
      candidate)))

(defun glb-subsum-test (glbset1 glbset2)
  ;; messy test
  (let ((tset1 (glbset-top glbset1))
        (tset2 (glbset-top glbset2))
        (ok1 nil)
        (ok2 nil))
    ;; first check to see whether any element in tset2 is strictly more
    ;; specific than any element in tset1 - if so, return these elements as a
    ;; cons
    (dolist (x tset2)
      (when
	  (dolist (y tset1)
	    (when (member y (type-ancestors x) :test #'eq)
	      (setf ok1 (cons x y))
	      (return ok1)))
        (return ok1)))
    ;; now check to see whether there is some other element pair which either
    ;; match, or where tset2 is strictly more specific
    (and ok1
	 (dolist (x1 (remove (car ok1) tset2 :test #'eq))
	   (when
	       (dolist (y1 (remove (cdr ok1) tset1 :test #'eq))
		 (when (or (eq x1 y1) 
			   (member y1 (type-ancestors x1) :test #'eq))
		   (setf ok2 t)
		   (return ok2)))
	     (return ok2))))))

; code to fix the local glb problem

(defun fix-mglb (glbset)
  ;; takes a mglbset which is at least as specific as any of the other problem
  ;; sets, and fixes this locally by creating a new type which has as
  ;; (multiple) parents all the elements in the top set and has as daughters
  ;; all the types in the bottom set
  ;;
  ;; if there are now some redundant links - i.e. there were originally direct
  ;; links between members of the top set and bottom set - these are removed
  (let* ((daughters (mapcar #'type-name (glbset-bottom glbset)))
         (parents (mapcar #'type-name (glbset-top glbset)))
         (new-type (make-glb-name daughters))
         (new-type-entry (make-type :name new-type 
				    :parents parents
                                    :daughters daughters
				    :glbp t)))
    (amend-type-information new-type new-type-entry parents daughters)))
   

(defun make-glb-name (dtrs)
  (declare (ignore dtrs))
  (next 'glbtype))

#|
  (let* ((true-dtrs (remove-duplicates 
                     (for dtr in dtrs
                         append
                         (let ((dtr-entry (get-type-entry dtr)))
                           (if (type-glbp dtr-entry)
                             (find-other-daughters dtr-entry)
                             (list dtr))))))                         
         (new-name-str
          (format nil "+~{~A+~}"
                  (mapcar #'abbrev-type-name true-dtrs)))
         (existing (find-symbol new-name-str)))
    (if existing (next existing) (intern new-name-str))))
|#

(defun abbrev-type-name (dtr)
  (let ((strname (string dtr)))
    (if (> (length strname) 3)
      (subseq strname 0 3)
      strname)))

;;; Constraint stuff

(defun expand-and-inherit-constraints nil
   (let ((ok t))
      (unmark-type-table)
      ;; a. determine which types are atomic
      ;;    can't do unification really without this info
      ;; 
      ;; b. expand constraints by first forming fstructure from
      ;; any constraint spec and then unifying with the
      ;; template-parents' constraints (if any) and then
      ;; the parents' constraints, expanding these if necessary
      ;; marking types when the constraint has been expanded
      ;;
      (determine-atomic-types)
      (for node in *type-names*
         do
         (let ((type-entry (get-type-entry node)))
           (unless (leaf-type-p type-entry)
            (unless 
               (expand-constraint node type-entry)
               (setf ok nil)))))
      (when ok 
         (check-feature-table))))
         

(defun determine-atomic-types nil 
  (dolist (node *type-names*)
    (let ((type-entry (get-type-entry node)))
      (let ((constraint-spec (type-constraint-spec type-entry)))
	(unless (leaf-type-p type-entry)
	  (setf (type-atomic-p type-entry)
	    (not 
	     (or constraint-spec
		 (some #'type-constraint-spec
		       (type-ancestors type-entry))
		 (some #'(lambda (daughter)
			   (or (type-constraint-spec daughter)
			       (some #'type-constraint-spec
				     (type-ancestors daughter))))
		       (type-descendants type-entry))))))))))


(defun expand-constraint (node type-entry)
   (cond
      ((seen-node-p type-entry) (type-constraint type-entry))
      (t
         (mark-node-seen type-entry)
         (let* ((*unify-debug-cycles* t) ; turn on cyclic dag warning messages
                (constraint-spec (type-constraint-spec type-entry))
                (local-constraint 
                  (if constraint-spec (process-unifications constraint-spec))))
            (cond ((and constraint-spec (null local-constraint))
                     (format t "~%Type ~A has an invalid constraint specification" node)
                        nil)
                  (t    
                     (when local-constraint
                        (unless 
                           (or (eq (type-of-fs local-constraint) *toptype*)
                              (eq (type-of-fs local-constraint) node))
                           (format t 
                                   "~%Warning: setting constraint of ~A to have ~A as type"
                              node node))
                        (setq local-constraint 
                          (destructively-retype-dag local-constraint node))
                        (setf (type-local-constraint type-entry) 
                          local-constraint)
                        (let ((local-appfeats 
                                 (top-level-features-of local-constraint)))
                           (for feature in local-appfeats
                              do 
                              (add-maximal-type feature node))))
                     ; no need to do inheritance when checking
                     ; for maximal introduction
                     (let ((full-constraint 
                              (inherit-constraints node type-entry local-constraint)))
                        (cond 
                           (full-constraint
                              (setf (type-constraint type-entry) full-constraint)
                              (setf (type-appfeats type-entry)
                                 (top-level-features-of full-constraint))
                              full-constraint)
                           (t (format t "~%Type ~A's constraint ~
                                 specification clashes with its parents'" node) 
                              nil)))))))))


(defun inherit-constraints (node type-entry local-constraint)
  (if (type-atomic-p type-entry)
      (create-atomic-dag node)
    (let ((supers 
	   (mapcar #'(lambda (parent)
		       (expand-constraint parent (get-type-entry parent)))
		   (append (type-template-parents type-entry)
			   (type-parents type-entry)))))
      (with-unification-context (nil)
        (let ((result
	 (reduce #'(lambda (x y) (when (and x y) 
				   (unify-dags x (retype-dag y *toptype*))))
		 supers
		 :initial-value (or local-constraint 
				    (create-typed-dag node)))))
          (if result (copy-dag result)))))))

(defun add-maximal-type (feature type)
   ;; a feature may only be introduced at one
   ;; point in the type hierarchy
   ;; Set up a hash table associating maximal types
   ;; with features
   ;; When a feature is first found in a constraint spec
   ;; initially set its entry to that type
   ;; Subsequently if it is found on another type there are
   ;; three possibilities
   ;; the new type may be a subtype of one of the first - do nothing
   ;; one or more of the old types may be a subtype of the new type -
   ;; replace the old with the new
   ;; there may be no subtype relationship - this may be because
   ;; there is an as yet unencountered type which is a supertype of
   ;; both or it may be an error
   ;; - add the new type to the old in a list - then replace
   ;; as appropriate if the supertype comes along
   ;; if there is already a list and another type is encountered
   ;; which is not a supertype of all the types then add it to the 
   ;; list 
   ;; at the end a final pass checks for problems and converts lists
   ;; to atoms   
   (let ((max-types (maximal-type-of feature)))
      (if (null max-types) 
         (set-feature-entry feature (list type))
         (unless (some #'(lambda (old-type) (subtype-p type old-type))
               max-types)
            (set-feature-entry feature 
               (remove-duplicates 
                  (cons type
                     (substitute-if type 
                        #'(lambda (old-type)
                           (subtype-p old-type type))
                        max-types))))))))


;;; Do strong typing and well-formedness check

(defparameter *well-formed-trace* nil)

(defun strongly-type-constraints nil
  ;; c. check for well-formedness ...
  (let ((ok t))
    (unmark-type-table)
    (for type-name in *type-names*
	 do
	 (unless (leaf-type-p (get-type-entry type-name))
	   (unless 
               (progn (setf *well-formed-trace* nil)
                      (nth-value 1 (wf-constraint-of type-name)))
	     ;; i.e. just looks at boolean ok/not-ok
	     (setf ok nil))))
    (unmark-type-table)
    ;; !!! can't create cyclic dags so don't check for them
    ok))

(defun wf-constraint-of (type-name)
  ;; may need to be copied completely before use
  ;; (print (list '> 'wf-constraint-of type-name))
  (let ((type-entry (get-type-entry type-name))
	(ok t))
    (unless (seen-node-p type-entry)
        (when (member type-name *well-formed-trace*)
          (error "~%~A is used in expanding its own constraint 
                    expansion sequence: ~A"  type-name
                 *well-formed-trace*))
        (push type-name *well-formed-trace*)
        (when (type-appfeats type-entry)
          (let ((new-dag (type-constraint type-entry)))
            ;; !!! outside here must stay within current generation
            (let ((*unify-generation* *unify-generation*)
                  (*within-unification-context-p* t))
              ;; establish new unification generation now, and also at
              ;; end (the usual place)
              (invalidate-marks)
              (prog1
                  (if (really-make-features-well-formed new-dag nil type-name)
                      (let ((res (copy-dag new-dag)))
                        (if res
                            (setf (type-constraint type-entry) res)
                          (progn
                            (format t "~%Warning: cycle in ~A" type-name)
                            (setq ok nil))))
                    (setq ok nil))
                (invalidate-marks)))))
        (mark-node-seen type-entry))
    ;; (print (list '< 'wf-constraint-of type-name))
    (values (type-constraint type-entry) ok)))                   
                  

;;; Make appfeats order equivalent so that display is consistent. Mostly
;;; will have same features as parent and be ordered the same already. If not,
;;; take current list, scan parent order, and delete types found in that from
;;; list, moving them to the front of the list

(defun canonicalise-feature-order nil
   (format t "~%Computing display ordering") 
   (unmark-type-table)
   (inherit-display-ordering *toptype* nil))


(defun inherit-display-ordering (type parent-feature-order)
   (let* ((type-record (get-type-entry type))
          (already-ordered-p t)
          (ordered-features 
             (if (every #'eq (type-appfeats type-record) parent-feature-order)
                (type-appfeats type-record)
                (let ((parent-ordered nil)
                      (appfeats (cons nil (type-appfeats type-record))))
                   (setq already-ordered-p nil)
                   (dolist
                      (parent-feat parent-feature-order
                         (nreconc parent-ordered (cdr appfeats)))
                      (block found
                         (do ((app-prev-tail appfeats (cdr app-prev-tail))
                              (app-tail (cdr appfeats) (cdr app-tail)))
                             ((null app-tail))
                             (when (eq (car app-tail) parent-feat)
                                (setf (cdr app-prev-tail) (cdr app-tail))
                                (setf (cdr app-tail) parent-ordered)
                                (setq parent-ordered app-tail)
                                (return-from found nil))))))))
          (sorted-ordered-features 
           (if (and *feature-ordering* ordered-features)
               (fix-feature-ordering ordered-features
                                     *feature-ordering*)
             ordered-features)))
      (setf (type-appfeats type-record) sorted-ordered-features)
      ;; don't process children if this type 
      ;; has been visited previously and its
      ;; feature ordering wasn't changed this time around
      ;(print (list type already-ordered-p (seen-node-p type-record)))
      (unless (and already-ordered-p (seen-node-p type-record))
         (mark-node-seen type-record)
         (unless (type-enumerated-p type-record)
            (for daughter in (type-daughters type-record)
               do
               (inherit-display-ordering daughter sorted-ordered-features))))))

(defun fix-feature-ordering (feats feature-order)
  (sort feats
               #'(lambda (x y)
                   (let ((x-list (member x feature-order)))
                     (if x-list
                         (member y x-list))))))

; should be stable-sort, but seems to be buggy in ACL

;;; Stuff below here is new for YADU
;;; Checking defaults
;;; The type checking for the hard information is defined to behave exactly
;;; as before.  The defaults are added in as a final stage
;;;
;;; 1. A default constraint must be internally consistent 
;;; 2. A default constraint must be strictly more specific than the
;;;    indefeasible constraint
;;; 3. A default constraint must inherit all defaults from its supertypes
;;;    i.e. subsequent unification with the constraints
;;;    on its supertypes has no further effect

;;; Note that there is no requirement that a default fs be well-formed
;;; However the local default constraint is actually constructed by
;;; creating a wffs from the default specifications and using unify-wffs
;;; to unify it with the indefeasible fs

(defun expand-type-hierarchy-defaults nil
   (let ((ok t))
      (unmark-type-table)
      ;; expand constraints to tdfs by 
      ;; 1) creating the default fss grouped by persistence
      ;;;   and making them well formed 
      ;; 2) unifying-wffsing them with the indefeasible fs
      ;; 3) creating a tdfs
      ;; 4) yaduing the tdfs with the supertypes tdfs
      ;;  expanding these if necessary and marking types when 
      ;;  the constraint has been expanded
      (for node in *type-names*
         do
         (let ((type-entry (get-type-entry node)))
           (unless (leaf-type-p type-entry)
             (unless 
                 (expand-default-constraint node type-entry)
               (setf ok nil)))))
      ok))

(defun expand-default-constraint (node type-entry)
   (cond ((seen-node-p type-entry) (type-tdfs type-entry))
      (t
         (mark-node-seen type-entry)
         (let* ((indef (type-constraint type-entry))
                (full-tdfs nil)
                (default-specs (type-default-spec type-entry))
                (default-fss
                    (for default-spec in default-specs
                         collect
                         (make-equivalent-persistence-defaults indef 
                               (car default-spec) (cdr default-spec) node))))               
               (setf full-tdfs 
                     (inherit-default-constraints node type-entry 
                       (construct-tdfs 
                        indef
                        default-fss)))
               (setf (type-tdfs type-entry) full-tdfs)
               full-tdfs))))

(defun make-equivalent-persistence-defaults (indef persistence default-spec node)
   (let*
      ((local-fs 
            (if default-spec 
               (process-unifications default-spec)))
         (local-default
            (if local-fs
               (create-wffs local-fs))))
      (with-unification-context (local-default)
         (let
            ((new-default
               (if local-default
                  (if 
                     (unify-wffs local-default indef)
                     local-default))))
            (when (and default-spec (not new-default))
               (format t
                       "~%Type ~A has inconsistent defaults for ~
                        persistence ~A: ignoring those defaults" 
                  node persistence))
            (when new-default 
               (unless
                  (setq new-default (copy-dag new-default))
                  (format t 
                          "~%Defeasible FS contains cycles in type ~A ~
                           persistence ~A: ignoring those defaults"
                     node persistence)))
            (unless new-default
               (setf new-default indef))
            (cons persistence new-default)))))


;; Check for defaults that aren't at the top level

(defun collect-tails (node dag)
  (let* ((type (type-of-fs dag))
	 (type-entry (unless (type-spec-atomic-p type)
		       (get-type-entry type))))
    (when type-entry
      (let ((tdfs (type-tdfs type-entry)))
	(when (and tdfs
		   (tdfs-tail tdfs))
	  (format t "~%Default constraint on ~A ignored in ~A"
		  (type-name type-entry) node)))
      (dolist (arc (dag-arcs dag))
	(collect-tails node (dag-arc-value arc))))))

(defun inherit-default-constraints (node type-entry local-tdfs)
  (collect-tails node (tdfs-indef local-tdfs))
  (let ((current-tail (tdfs-tail local-tdfs)))
    (for parent in (type-parents type-entry)
         do
         (let ((parent-tdfs (expand-default-constraint parent
                                 (get-type-entry parent))))
           (unless parent-tdfs
             (format t "~%Cannot make tdfs for ~A" parent))
           (when parent-tdfs
             (setf current-tail
                   (yadu-general-merge-tails (tdfs-tail parent-tdfs)
                             current-tail (tdfs-indef local-tdfs))))))
      (setf (tdfs-tail local-tdfs) current-tail)
      local-tdfs))


