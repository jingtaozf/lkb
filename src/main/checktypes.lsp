;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; April 1997 - combined glb check and YADU functionality
;;;            - removed expand-type-table (redundant since
;;;              checking is so quick

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

(defun add-type-from-file (name parents constraint default comment &optional daughters)
  ;;; YADU --- extra arg needed
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
            new-type))))

(defun amend-type-from-file (name parents constraint default comment)
   (declare (ignore parents))
   (let ((existing-type (get-type-entry name)))    
      (if existing-type
        (progn 
          (setf (type-constraint-spec existing-type) constraint)
          (setf (type-default-spec existing-type) default)
          (setf (type-comment existing-type) comment))
        (format t "~%Warning - ~A ignored - patch only works to redefine types"
                name))))

(defvar *type-names* nil)

;;; Due to a bug in Procyon Common Lisp doing a gethash on a 
;;; hash table inside a maphash on the same hash table can 
;;; go wrong.  To work around this the calls to maphash in
;;; this file have been replaced with for loop on a list
;;; of type names

(defun amend-type-information (new-type new-type-entry parents daughters)
;;;
;;; called from fix-mglb
;;;
  ;;; takes a new type-name and its entry 
  ;;; (this should have parents and daughters specified, plus any idiosyncratic
  ;;; information)
  ;;; a list of its parent types and of its daughter types
  ;;; (it is assumed that these will not add a cycle, and that they are valid
  ;;; types, so if this function is called from code which gets new specs
  ;;; from a user, the calling function must perform the checks)
  ;;; the fn does the following
  ;;; 1) sets the new type entry
  ;;; 2) amends all the daughter types so that their parents 
  ;;;    are correct (remove any redundant links to the old parents)
  ;;; 3) amends all the parents in a similar way
  ;;; 3') adds the ancestors and descendants to the type entry
  ;;; 4) adds the new type to the descendants list of all its ancestors
  ;;; 5) adds the new type to the ancestors list of all its descendants
  ;;; 6) pushes the new type onto the list of *type-names*
  (let ((ancestors nil)
        (descendants nil))
    (create-mark-field new-type-entry)
    (set-type-entry new-type new-type-entry)   
    (for dtr in daughters
         do
         (let ((dtr-entry (get-type-entry dtr)))
           (setf (type-parents dtr-entry)
                 (cons new-type (set-difference (type-parents dtr-entry) 
                                                parents :test #'eq)))
           (setf descendants (union descendants 
                                    (type-descendants dtr-entry) :test #'eq))
           (push dtr descendants)))
    (for parent in parents
         do
         (let ((par-entry (get-type-entry parent)))
           (setf (type-daughters par-entry)
                 (cons new-type (set-difference (type-daughters par-entry) 
                                              daughters :test #'eq)))
           (setf ancestors (union ancestors 
                                  (type-ancestors par-entry) :test #'eq))
           (push parent ancestors)))
    (setf (type-descendants new-type-entry) descendants)
    (setf (type-ancestors new-type-entry) ancestors)
    (for ancestor in ancestors
         do
         (push new-type (type-descendants (get-type-entry ancestor))))
    (for descendant in descendants
         do
         (push new-type (type-ancestors (get-type-entry descendant))))
    (push new-type *type-names*)))
        
;;; Checking

(defparameter *partition* nil)

(defparameter *glbsets* nil)

(defun check-type-table nil
   (scratch 'glbtype)
   (format t "~%Checking type hierarchy")
   (setf *type-names* (collect-type-names))
   (prog1
      (when *toptype*
        (when 
          (add-daughters-to-type-table)
          (when (check-for-cycles-etc *toptype*)
            (unmark-type-table)
            (format t "~%Checking for unique greatest lower bounds") 
            (setf *partition* nil)
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
            (format t "~%Expanding constraints")
            (when (expand-and-inherit-constraints)
              (format t "~%Making constraints well formed")
              (when (strongly-type-constraints)
                (format t "~%Optimising unification check paths") 
                (optimise-check-unif-paths)
                ;;; YADU --- extra expansion stage
                ;;; earlier stages are unchanged
                (format t "~%Expanding defaults") 
                (when (expand-type-hierarchy-defaults)
                  (format t "~%Type file checked successfully")
                  t))))))
     (clear-type-cache))) ; not for consistency, but for efficiency

(defun patch-type-table nil
  ;;; added for the case where definitions are changed, but the hierarchy
  ;;; itself is unaltered
  (clear-types-for-patching-constraints)
  (for name in *type-names*
         do
         (let* ((type-entry (get-type-entry name)))
           (setf (type-constraint type-entry) nil)
           (setf (type-atomic-p  type-entry) nil)
           (setf (type-tdfs type-entry) nil)
           (setf (type-appfeats type-entry) nil)
           (setf (type-local-constraint type-entry) nil)))
  (unmark-type-table)  
   (format t "~%Expanding constraints")
   (when (expand-and-inherit-constraints)
     (format t "~%Making constraints well formed")
     (when (strongly-type-constraints)
       ;;; YADU --- extra expansion stage
       ;;; earlier stages are unchanged
       (format t "~%Expanding defaults") 
       (when (expand-type-hierarchy-defaults)
         (format t "~%Type file checked successfully")
         t))))


;;; First we need to check that the type hierarchy itself is OK
;;; viewed as a graph without considering the constraints
;;; There are several relevant conditions:
;;; connectedness
;;; existance of top
;;; no cycles
;;; no unary branches
;;; unique greatest lower bound
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
               (for parent in parents
                  do
                  (let ((parent-entry (get-type-entry parent)))
                     (cond (parent-entry 
                           (pushnew name 
                                    (type-daughters parent-entry)
                                    :test #'eq))
                        (t (setf ok nil)
                           (format t
                              "~%~A specified to have non-existent parent ~A"
                              name parent)))))))
      (when ok
        (set-up-descendants *toptype*)
        (add-ancestors-to-type-table *toptype*))
      ok))

(defun check-for-cycles-etc (top)
   (let ((top-entry (get-type-entry top)))
      (mark-node-active top-entry)
      (when (mark-for-cycles top-entry)
         (scan-table))))
   
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

;;; extension to check for redundant links
;;;
;;; mark all daughters of the node as active
;;; marking an already active node either indicates a cycle or
;;; a redundant link
;;; only unmark as active when all daughters have been checked

;;; marks.lsp contains the marking structures and functions

(defun mark-for-cycles (type-record)
   (let ((ok t)
         (daughters (mapcar #'get-type-entry 
               (type-daughters type-record))))
      (unless (seen-node-p type-record) 
         (mark-node-seen type-record)
         (for daughter in daughters
            do
            (if (active-node-p daughter)
               (progn 
                  (setf ok nil)
                  (format t "~%Cycle or redundancy involving ~A" 
                     (type-name daughter)))
               (mark-node-active daughter)))
         (when ok
            (setf ok 
               (for daughter in daughters
                  all-satisfy
                  (mark-for-cycles daughter)))
            (for daughter in daughters
               do
               (unmark-node-active daughter))))
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
          (clear-marks type-entry) 
          ;; check for unary branches 
         (when (eql (length (type-daughters type-entry)) 1)
          ;;  (setf ok nil)
          ;; unary branches aren't going to make the rest of the 
          ;; checking fail so let it continue
          ;; Actually we don't really care about unary branches at all
          ;; so unless variable is set - keep quiet
          (when *warn-of-unary-branches*
            (format t "~%Warning: ~A specified to have single daughter ~A"
                        name
                        (car (type-daughters type-entry))))))
               *types*)
            ok))

#|
(defun set-up-descendants (type)
  (let ((type-entry (get-type-entry type)))
    (setf (type-descendants type-entry)
          (get-descendants type-entry))
    (for daughter in (type-daughters type-entry)
         do 
         (set-up-descendants daughter))))
|#

;;; The original version of this runs in quadratic time, but it ought
;;; to be linear.

(defun set-up-descendants (type)
  (let* ((type-entry (get-type-entry type))
	 (daughters (type-daughters type-entry)))
    (when daughters
      (setf (type-descendants type-entry)
	(union daughters
	       (reduce #'union
		       (mapcar #'(lambda (daughter)
				   (set-up-descendants daughter))
			       daughters)))))))

(defun add-ancestors-to-type-table (top-node)
  (for type in (retrieve-descendants top-node)
       do
       (let ((type-entry (get-type-entry type)))
         (setf (type-ancestors type-entry)
               (get-ancestors type-entry)))))

(defun unmark-type-table nil
   (maphash 
      #'(lambda (node type-entry)
           (declare (ignore node))
           (clear-marks type-entry))
        *types*))

(defun find-good-partitions (type)
  ;;; doing the glb stuff over the entire hierarchy is very slow
  ;;; when we have lots of multiple inheritance.  This function attempts
  ;;; to find manageable subchunks, returning a list of lists of nodes which
  ;;; should be mutually independent
  (let* ((type-entry (get-type-entry type))
         (daughters (type-daughters type-entry)))
    (unless (seen-node-p type-entry)
      (when daughters
        (let*
          ((ancestors (type-ancestors type-entry))
           (descendants (type-descendants type-entry))
           (lineage (cons type (append ancestors descendants))))
          (for daughter in daughters
               do 
               (find-good-partitions daughter))
          (when 
            (for descendant in descendants
                 all-satisfy
                 (let ((desc-entry (get-type-entry descendant)))
                         (or (seen-node-p desc-entry)
                             (subsetp (type-ancestors desc-entry)
                                      lineage :test #'eq))))
            (let ((partition-nodes
                         (for descendant in descendants
                              filter
                              (let ((desc-entry (get-type-entry descendant)))
                                (if (not (seen-node-p desc-entry))
                                  (progn
                                    (mark-node-seen desc-entry)
                                    descendant))))))
              (push partition-nodes *partition*))))))))


;;; GLB stuff


(defparameter *interesting-types* nil)
 
(defstruct (glbset)
  top bottom ignore)

(defun check-for-unique-glbs (partition-nodes)
  (setf *glbsets* nil)
  (setf *interesting-types* nil)
  (let ((ancestors nil))
    (for type in partition-nodes
         do
         (let ((type-entry (get-type-entry type)))
           (when (cdr (type-parents type-entry))                   
             (for ancestor in (retrieve-ancestors type)
                  do
                  (when (member ancestor partition-nodes :test #'eq)
                    (pushnew ancestor ancestors :test #'eq))))))
    (setf *interesting-types* ancestors)
    (check-for-unique-glbs-from-top ancestors)))

(defun check-for-unique-glbs-from-top (possible-nodes)
  (mapl #'(lambda (remaining-nodes)
            (let ((x (car remaining-nodes)))
               (mapc #'(lambda (y)
                         (let ((problem-list
                                (check-lbs-from-top x y)))                           
                           (when problem-list
                             (push (make-glbset :top (list x y) 
                                                :bottom problem-list) 
                                   *glbsets*))))
                     (cdr remaining-nodes))))
         possible-nodes))



(defvar *wanted* nil)
     
(defun check-lbs-from-top (x y)
  (let ((xdescendants (retrieve-descendants x))
        (ydescendants (retrieve-descendants y)))
    (unless (or (member y xdescendants :test #'eq) 
                (member x ydescendants :test #'eq))
      (let ((*wanted* nil))
         (find-highest-intersections xdescendants ydescendants)
         (collect-highest-intersects xdescendants)
         (if (cdr *wanted*) *wanted*)))))

(defun find-highest-intersections (xs ys)
   (for type in xs
      do
      (when (member type ys :test #'eq)
         (let ((type-entry (get-type-entry type)))
            (unless (seen-node-p type-entry)
               (mark-node-seen type-entry)
               (for desc in (type-descendants type-entry)
                  do 
                  (mark-node-active-and-seen (get-type-entry desc))))))))

#|

(defun find-highest-intersections (type)
  (let ((type-entry (get-type-entry type)))
    (unless (seen-node-p type-entry)
      (if (member type *others*)
        (progn (mark-node-seen type-entry)
               ; a seen but not active node is highest so far
               (for desc in (retrieve-descendants type)
                    do 
                    (mark-node-active-and-seen (get-type-entry desc))))
        (for daughter in (type-daughters type-entry)
             do
             (find-highest-intersections daughter))))))

|#

(defun collect-highest-intersects (descendants)
  (for node in descendants
       do
       (let ((node-entry (get-type-entry node)))         
         (when (and (seen-node-p node-entry)
                    (not (active-node-p node-entry)))
           (push node *wanted*))
         (clear-marks node-entry))))
  
           

#|

(defun check-lbs-from-top (x y)
  (let ((xdescendants (retrieve-descendants x))
        (ydescendants (retrieve-descendants y)))
    (unless (or (member y xdescendants)
                (member x ydescendants))
      (let ((z (intersection xdescendants ydescendants)))
        (if (cdr z)
          (let ((problem-list (remove-descendants-glb z)))
            (if (cdr problem-list)
              problem-list)))))))

(defun remove-descendants-glb (int-list)
   (do* ((done nil (cons initial done))
         (initial (car int-list) (car (set-difference new-int-list done)))
         (new-int-list (set-difference int-list (retrieve-descendants initial))
            (set-difference new-int-list (retrieve-descendants initial))))
      ((null (set-difference new-int-list (cons initial done)))
            new-int-list)))

|#          

;;;; July 1996 new stuff to fix mglbs

;;; the glb checking code puts records of problems on the global *glbsets*
;;; We find start from the bottom, fixing 
;;; a set which has no lower sets in the problem list
;;; The fix consists of adding a type with parents of the
;;; types which have mglbs and daughters equal to the mglbs
;;; and removing any redundant links.  We then make 
;;; fixes to *glbsets*



(defun fix-mglbs nil
  ;;; partition specific because of *interesting-types*
  (format t "~%Elements in *interesting-types* ~A" (length *interesting-types*))
  (loop 
      (let* ((minset (find-minimal-glbset *glbsets*))
        ; find candidate problem to fix 
             (new-type (fix-mglb minset)))
          ;;; fix-mglb now also redoes the type hierarchy info
        (when *display-glb-messages*
          (format t "~%Fixing ~A with glbs ~A" (glbset-top minset)
                  (glbset-bottom minset)))
        (modify-glbsets new-type *interesting-types*)
;          (push new-type *interesting-types*)
; apparently glbtypes cannot be interesting - there are never
; cases where we get multiple glbs between glbtypes (presumably
; because glbtypes are always minimal)
        (unless *glbsets* ; have we fixed it yet?
          (format t "~%Checked")
          (return t)))))

(defun modify-glbsets (new-type nodes)
   (let* ((ancestors (retrieve-ancestors new-type)))
     (for glbrec in *glbsets*
          do
         (let* ((pair (glbset-top glbrec)))
           (if
             (and (member (car pair) ancestors :test #'eq)
                  (member (cadr pair) ancestors :test #'eq))
             (let ((new-probs
                    (check-lbs-from-top (car pair) (cadr pair))))
               (if new-probs
                 (setf (glbset-bottom glbrec) new-probs)
                 (setf (glbset-ignore glbrec) t))))))
     (for node in nodes
         do
         (let ((problem-list
                (check-lbs-from-top new-type node)))
           (when problem-list
             (push (make-glbset :top (list new-type node) 
                                :bottom problem-list) 
                   *glbsets*))))
     (setf *glbsets* 
          (delete-if #'(lambda (x) (glbset-ignore x))
                     *glbsets*))))


;;; code to find best candidate to fix

(defun find-minimal-glbset (glbs)
  ;;; find a set x which has no other top set y which is 
  ;;; more specific - that is there is no y which
  ;;; contains both an element e such that e is strictly subsumed by
  ;;; a member of x, and an element f which is subsumed by or equal to
  ;;; another member of x
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
  ;;; messy test
  (let ((tset1 (glbset-top glbset1))
        (tset2 (glbset-top glbset2))
        (ok1 nil)
        (ok2 nil))
    ;;; first check to see whether any element in tset2 is strictly more
    ;;; specific than any element in tset1 - if so, return these elements
    ;;; as a cons
    (dolist (x tset2)
      (when
         (dolist (y tset1)
           (when (member y (retrieve-ancestors x) :test #'eq)
             (setf ok1 (cons x y))
             (return ok1)))
        (return ok1)))
    ;;; now check to see whether there is some other element pair which
    ;;; either match, or where tset2 is strictly more specific
    (and ok1
      (dolist (x1 (remove (car ok1) tset2 :test #'eq))
        (when
          (dolist (y1 (remove (cdr ok1) tset1 :test #'eq))
            (when (or (eq x1 y1) 
                      (member y1 (retrieve-ancestors x1) :test #'eq))
              (setf ok2 t)
              (return ok2)))
          (return ok2))))))

; code to fix the local glb problem

(defun fix-mglb (glbset)
  ;;; takes a mglbset which is at least as specific as any of the other
  ;;; problem sets, and fixes this locally by creating a new type
  ;;; which has as (multiple) parents all the elements in the top set
  ;;; and has as daughters all the types in the bottom set
  ;;;
  ;;; if there are now some redundant links - i.e. there were originally
  ;;; direct links between members of the top set and bottom set
  ;;; - these are removed
  (let* ((daughters (glbset-bottom glbset))
         (parents (glbset-top glbset))
         (new-type (make-glb-name daughters))
         (new-type-entry (make-type :name new-type :parents parents
                                    :daughters daughters
                                     :glbp t)))
    (amend-type-information new-type new-type-entry parents daughters)
    new-type))

    

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
            (unless 
               (expand-constraint node type-entry)
               (setf ok nil))))
      (when ok 
         (check-feature-table))))
         

(defun determine-atomic-types nil 
   (for node in *type-names*
      do
      (let ((type-entry (get-type-entry node)))
         (let ((constraint-spec (type-constraint-spec type-entry)))
            (setf (type-atomic-p type-entry)
               (not 
                  (or constraint-spec
                     (some #'constraint-spec-of 
                        (type-ancestors type-entry))
                     (some #'(lambda (daughter)
                           (or (constraint-spec-of daughter)
                              (some #'constraint-spec-of 
                                 (type-ancestors (get-type-entry daughter)))))
                        (type-descendants type-entry)))))))))


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
                        (setq local-constraint (retype-dag local-constraint node))
                        (setf (type-local-constraint type-entry) local-constraint)
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
      (reduce
         #'(lambda (x y) (if (and x y) (unify-dags x y)))
         (mapcar #'(lambda (parent)
                     (let ((constraint
                            (expand-constraint parent (get-type-entry parent))))
                        (if constraint
                           ;; hack to allow for templates
                           (retype-dag constraint *toptype*))))
                 (append (type-template-parents type-entry)
                         (type-parents type-entry)))
         :initial-value 
         (or local-constraint (create-typed-dag node)))))


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

(defun strongly-type-constraints nil
   ;; c. check for well-formedness ...
   (let ((ok t))
      (unmark-type-table)
      (for type-name in *type-names*
         do
         (unless 
               (nth-value 1 (wf-constraint-of type-name))
           ;;; i.e. just looks at boolean ok/not-ok
;               (format t "~%Type ~A has an invalid constraint specification" type-name)
                  (setf ok nil)))
         (unmark-type-table)
; !!! can't create cyclic dags
;         (for type in *type-names* 
;            do 
;            (let ((constraint (constraint-of type)))
;               (if constraint
;                  (check-fs-for-cycles constraint type))))
         ok))

(defun wf-constraint-of (type-name) 
   ;; may need to be copied completely before use
   ;; (print (list '> 'wf-constraint-of type-name))
   (let ((type-entry (get-type-entry type-name))
         (ok t))
      (unless (seen-node-p type-entry)
         (when (type-appfeats type-entry)
            (let ((new-dag (type-constraint type-entry)))
               ;; !!! outside here must stay within current generation
               (let ((*unify-generation* *unify-generation*)
                     (*within-unification-context-p* t))
                  ;; establish new unification generation now, and also at end
                  ;; (the usual place)
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
                                (return-from found nil)))))))))
      (setf (type-appfeats type-record) ordered-features)
      ;; don't process children if this type has been visited previously and its
      ;; feature ordering wasn't changed this time around
      ;(print (list type already-ordered-p (seen-node-p type-record)))
      (unless (and already-ordered-p (seen-node-p type-record))
         (mark-node-seen type-record)
         (unless (type-enumerated-p type-record)
            (for daughter in (type-daughters type-record)
               do
               (inherit-display-ordering daughter ordered-features))))))


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
            (unless 
               (expand-default-constraint node type-entry)
               (setf ok nil))))
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
               (cerror
                  "Ignore them"
                  "Type ~A has inconsistent defaults for persistence ~A" 
                  node persistence))
            (when new-default 
               (unless
                  (setq new-default (copy-dag new-default))
                  (cerror "Ignore it"
                     "Defeasible FS contains cycles in type ~A persistence ~A"
                     node persistence)))
            (unless new-default
               (setf new-default indef))
            (cons persistence new-default)))))
            
(defun inherit-default-constraints (node type-entry local-tdfs)
  (declare (ignore node))
  (let ((current-tail (tdfs-tail local-tdfs)))
    (for parent in (type-parents type-entry)
         do
         (let ((parent-tdfs (expand-default-constraint parent
                                 (get-type-entry parent))))
           (unless parent-tdfs
             (cerror "Ignore it" "Cannot make tdfs for ~A" parent))
           (when parent-tdfs
             (setf current-tail
                   (combine-yadu-tails (tdfs-tail parent-tdfs)
                             current-tail (tdfs-indef local-tdfs))))))
      (setf (tdfs-tail local-tdfs) current-tail)
      local-tdfs))

(defun combine-yadu-tails (tail1 tail2 indef)
  (let ((ct1 (for element in tail1
                 collect
                 (copy-tail-element-completely
                    element)))
        (ct2 (for element in tail2
                 collect
                 (copy-tail-element-completely
                    element))))
    (cond ((and ct1 ct2)
           (merge-tails ct1 ct2 indef))
          (ct1)
          (t ct2))))
