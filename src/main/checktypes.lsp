;;; Copyright Ann Copestake, John Carroll 1991-2000 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :lkb)

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

(defvar *type-redefinitions* nil)

(defun add-type-from-file (name parents constraint default comment &optional daughters)
  ;;; YADU --- extra arg needed
  (if *leaf-type-addition*
      (add-leaf-type name parents constraint default comment daughters)
  (let ((existing-type (get-type-entry name)))
      (when existing-type
        (format t "~%WARNING: Type `~A' redefined." name)
        (push name *type-redefinitions*))
      (let ((new-type 
               (make-type :name name 
                  :parents parents 
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
	(if (type-parents-equal parents (type-parents existing-type))
	    (progn 
	      (setf (type-constraint-spec existing-type) constraint)
	      (setf (type-default-spec existing-type) default)
	      (setf (type-comment existing-type) comment))
          (progn
            (unless (member name *type-redefinitions*)
              (format t "~%Warning - ~A ignored - patch cannot change type hierarchy"
                      name)
              (setf ok nil))))
      (progn
        (setf ok nil)
        (format t "~%Warning - ~A ignored - patch only works to redefine types"
                name)))
    ok))

(defun type-parents-equal (new-parents old-parents)
    (and (null (set-difference new-parents old-parents))
         (null (set-difference old-parents new-parents))))


;;;

(defvar *type-names* nil)

(defvar *partitions* nil)

(defparameter *hierarchy-only-p* nil)

(defparameter *display-glb-messages* nil
   "if set, informs user of glbtypes as they are created")


(defun check-type-table nil
   (scratch 'glbtype)
   (format t "~%Checking type hierarchy")
   (setq *type-names* (sort (collect-type-names) #'string-lessp))
   (when *toptype*
     (when 
       (add-daughters-to-type-table)
       (when (check-for-cycles-etc *toptype*)
         (unmark-type-table)
         (set-up-ancestors-and-descendants)
         (format t "~%Checking for unique greatest lower bounds") 
         (let ((*partitions* nil) (glbp nil))
            (find-good-partitions *toptype*)
            (unmark-type-table)
            ;; (format t "~%Constructing glb types") (force-output t)
            (dolist
               (partition
                  ;; sort so behaviour reproducible if grammar has not changed
                  (sort *partitions* #'> :key #'length))
               (setq glbp (compute-and-add-glbtypes partition glbp)))
            ;; (unless glbp (format t "~%No glb types needed"))
            )
         (set-up-ancestors-and-descendants)
         (if *hierarchy-only-p*
           (expand-local-only-constraints)
           (progn
             (format t "~%Expanding constraints")
             (when (expand-and-inherit-constraints)
               (format t "~%Making constraints well formed")
               (when (strongly-type-constraints)
                 (optimise-check-unif-paths)
                 ;; YADU --- extra expansion stage
                 ;; earlier stages are unchanged
                 (format t "~%Expanding defaults")
                 (when (expand-type-hierarchy-defaults)
                   (format t "~%Type file checked successfully")
                   (gc-types)
                   (clear-type-cache) ; not for consistency, but for efficiency
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
  #+:allegro (when *gc-before-reload* (excl:gc t))
  ;; try and force it to reclaim space before we refill it
  (format t "~%Expanding constraints")
  (when (expand-and-inherit-constraints)
    (format t "~%Making constraints well formed")
    (when (strongly-type-constraints)
       ;;; YADU --- extra expansion stage
       ;;; earlier stages are unchanged
      (format t "~%Expanding defaults") 
      (when (expand-type-hierarchy-defaults)
	(format t "~%Re-expanding rules")
	(expand-rules) ; in rules.lsp
	(format t "~%Type file checked successfully")
	t))))


(defun unmark-type-table nil
   (maphash 
      #'(lambda (node type-entry)
          (declare (ignore node))
          (clear-marks type-entry))
      *types*))


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
   ok))


;;; Set up descendants and ancestors - done before glb computation
;;; because needed for partition finding, and after as well since so much
;;; changes during glb computation that it's not worth trying to keep
;;; descendants and ancestors up-to-date during it

(defun set-up-ancestors-and-descendants ()
   (maphash 
      #'(lambda (node type-entry)
          (declare (ignore node))
          (setf (type-ancestors type-entry) nil)
          (setf (type-descendants type-entry) nil))
      *types*)
   (maphash 
      #'(lambda (node type-entry)
          (declare (ignore node))
          (set-up-ancestors type-entry))
      *types*)
   (set-up-descendants (get-type-entry *toptype*)))

(defun set-up-ancestors (type-entry)
  (or (type-ancestors type-entry)
      (let ((parents (type-parents type-entry))
            (ancestors nil))
        (when parents
           (setq ancestors
              (cons (get-type-entry (car parents))
                 (set-up-ancestors (get-type-entry (car parents)))))
           (dolist (parent (cdr parents))
	      (pushnew (get-type-entry parent) ancestors :test #'eq)
	      (dolist (ancestor (set-up-ancestors (get-type-entry parent)))
	         (pushnew ancestor ancestors :test #'eq))))
        (setf (type-ancestors type-entry) ancestors)
        ancestors)))

(defun set-up-descendants (type-entry)
  (or (type-descendants type-entry)
      (let ((daughters (type-daughters type-entry))
            (descendants nil))
         (when daughters
            (setq descendants
               (cons (get-type-entry (car daughters))
                  (set-up-descendants (get-type-entry (car daughters)))))
            (dolist (daughter (cdr daughters))
	       (pushnew (get-type-entry daughter) descendants :test #'eq)
	       (dolist (descendant (set-up-descendants (get-type-entry daughter)))
	          (pushnew descendant descendants :test #'eq))))
         (setf (type-descendants type-entry) descendants)
         descendants)))


;;; Compute partitions of the hierarchy, returning a list of lists of nodes
;;; which are mutually independent. Additionally filter out from partitions
;;; sets of types which are in tree-shaped configurations. Shortens the type bit
;;; representations and reduces the number of comparisons performed from
;;; ntypes^2 to (a^2 + b^2 + ...) where a,b,... are sizes of partitions

(defun find-good-partitions (type)
  ;; AAC - Oct 12 1998 - faster version
  (let* ((type-entry (get-type-entry type))
         (daughters (type-daughters type-entry)))
    (when (and (not (active-node-p type-entry))
               (not (seen-node-p type-entry)))
      (mark-node-active type-entry)
      (when daughters
        (dolist (daughter daughters)
	   (find-good-partitions daughter))
        (let* ((descendants (type-descendants type-entry))
               (desc-names (mapcar #'type-name descendants)))
          (when 
	      (for descendant in descendants
                   all-satisfy
		   (or (seen-node-p descendant)
		       (null (cdr (type-parents descendant)))
                       (subsetp
                          (type-parents descendant) desc-names :test #'eq)))
            (let ((partition-nodes
                   (for descendant in descendants
                        filter
			(when (not (seen-node-p descendant))
			  (mark-node-seen descendant)
			  descendant))))
	      (when partition-nodes
		 (push (partition-non-tree-config-types type-entry partition-nodes)
                    *partitions*)))))))))

(defun partition-non-tree-config-types (top others)
   (let ((partition-types (cons top others))
         (filtered nil))
       (dolist (type partition-types)
          (when (cdr (type-parents type)) ; multiple parents
 	     (dolist (ancestor (type-ancestors type))
	        (when (member ancestor partition-types :test #'eq)
	           (pushnew type filtered :test #'eq)
                   (pushnew ancestor filtered :test #'eq)))))
       (cons top (delete top filtered :test #'eq))))


;;; Glb type computation. Assigns a (temporary) bit representation for
;;; each type in heirarchy, and uses it to efficiently check if each pair
;;; of types has a glb and add it if not

#|
;;; Type bit code operations assuming simple bit vector representation.
;;; Useful example code - DO NOT DELETE!

(progn
(defparameter +bit-code-temp+ nil)
(defparameter +bit-code-zero+ nil)

(defun make-bit-code (length)
   (make-array length :element-type 'bit :initial-element 0))

(defun bit-code-equal (c1 c2)
   (equal c1 c2))

(defun bit-code-and-zero-p (c1 c2 c3)
   ;; c3 <- c1 AND c2 (destructive)
   ;; also return boolean indicating whether c3 is all zero
   (unless (= (length c1) (length +bit-code-zero+))
      (setq +bit-code-zero+
         (make-array (length c1) :element-type 'bit :initial-element 0)))
   (bit-and c1 c2 c3)
   (equal c3 +bit-code-zero+))

(defun bit-code-ior (c1 c2 c3)
   (bit-ior c1 c2 c3))

(defun bit-code-subsume-p (c1 c2)
   ;; does code c1 subsume c2? i.e. for every bit not set in c1, is the
   ;; corresponding bit in c2 also unset?
   (unless (= (length c1) (length +bit-code-temp+) (length +bit-code-zero+))
      (setq +bit-code-temp+
         (make-array (length c1) :element-type 'bit))
      (setq +bit-code-zero+
         (make-array (length c1) :element-type 'bit :initial-element 0)))
   (equal (bit-andc1 c1 c2 +bit-code-temp+) +bit-code-zero+))

(defun set-bit-code (c n)
   (setf (sbit c n) 1))

(defun bit-code-position-1 (c)
   ;; index of first bit that is set in c
   (position 1 c))
)
|#

;;; The actual type bit code representation is different since not all lisp
;;; systems have all of these operations running at a decent speed (especially
;;; the equal and position functions). Instead, each code is represented by a
;;; simple vector of fixnums, with all operations on codes being performed
;;; 30 or so bits at a time. (A related approach is described by Henry
;;; Baker "Efficient implementation of bit-vector operations in Common Lisp",
;;; ACM Lisp Pointers, 3(2-4).) The scheme as implemented here is completely
;;; portable and should run well in all reasonable Lisp systems

(progn
(defconstant +fixnum-len+
   (min (1+ (integer-length most-positive-fixnum))
        (1+ (integer-length most-negative-fixnum))))

(defun make-bit-code (length)
   (make-array (ceiling length +fixnum-len+) :element-type t :initial-element 0))

(defun bit-code-equal (c1 c2)
   ;; need the array decl up here since if it's in the locally then acl
   ;; seems to overlook it. The arrayp tests should still be OK and not
   ;; optimised away since we're not at the highest speed setting just yet
   (declare (type simple-vector c1 c2))
   (when (or (not (arrayp c1)) (not (arrayp c2))) (error "not an array"))
   (locally
      (declare (optimize (speed 3) (safety 0)))
      (dotimes (n (length c1) t)
         (declare (fixnum n))
         (unless (= (the fixnum (svref c1 n)) (the fixnum (svref c2 n))) (return nil)))))

(defun bit-code-and-zero-p (c1 c2 c3)
   (declare (type simple-vector c1 c2 c3))
   (when (or (not (arrayp c1)) (not (arrayp c2)) (not (arrayp c3)))
      (error "not an array"))
   (locally
      (declare (optimize (speed 3) (safety 0)))
      (let ((acc 0))
         (declare (fixnum acc))
         (dotimes (n (length c1) (zerop acc))
            (declare (fixnum n))
            (let ((x (logand (the fixnum (svref c1 n)) (the fixnum (svref c2 n)))))
               (declare (fixnum x))
               (setf (svref c3 n) x)
               (setq acc (logior x acc)))))))

(defun bit-code-ior (c1 c2 c3)
   (declare (type simple-vector c1 c2 c3))
   (when (or (not (arrayp c1)) (not (arrayp c2)) (not (arrayp c3)))
      (error "not an array"))
   (locally
      (declare (optimize (speed 3) (safety 0)))
      (dotimes (n (length c1) c3)
         (declare (fixnum n))
         (setf (svref c3 n)
            (logior (the fixnum (svref c1 n)) (the fixnum (svref c2 n)))))))

(defun bit-code-subsume-p (c1 c2)
   (declare (type simple-vector c1 c2))
   (when (or (not (arrayp c1)) (not (arrayp c2))) (error "not an array"))
   (locally
      (declare (optimize (speed 3) (safety 0)))
      (dotimes (n (length c1) t)
         (declare (fixnum n))
         (unless (zerop (logand (lognot (the fixnum (svref c1 n))) (the fixnum (svref c2 n))))
            (return nil)))))

(defun set-bit-code (c n)
   (multiple-value-bind (e1 e2)
      (truncate n +fixnum-len+)
      (setf (svref c e1)
      	 (logior (svref c e1)
            (if (zerop e2) most-negative-fixnum (ash 1 (- (1- +fixnum-len+) e2)))))))

(defun bit-code-position-1 (c)
   (declare (type simple-vector c))
   (unless (arrayp c) (error "not an array"))
   (locally
      (declare (optimize (speed 3) (safety 0)))
      (dotimes (n (length c) nil)
         (declare (fixnum n))
         (let ((e (svref c n)))
            (declare (fixnum e))
            (unless (zerop e)
               (return
                  (+ (* n +fixnum-len+)
                     (if (minusp e) 0 (- +fixnum-len+ (integer-length e))))))))))
)


;;; Entry point for glb computation: compute-and-add-glbtypes. Don't need to
;;; consider any types that are at the fringe of the hierarchy and have only
;;; a single parent
;;;
;;; Type codes can be looked up efficiently by hashing them on the index of
;;; their first non-zero bit. Thanks to Ulrich Callmeier for the code
;;; on which much of this is based

(defvar *bit-coded-type-table*)

(defmacro get-bit-coded-type (bit-coded-type-table code)
   `(svref ,bit-coded-type-table (bit-code-position-1 ,code)))

(defun lookup-type-from-bits (code)
   ;; hash code and check for equal one in bucket
   (dolist (type (get-bit-coded-type *bit-coded-type-table* code) nil)
      (when (bit-code-equal (type-bit-code type) code)
         (return type))))


(defun external-single-parent-type-p (type-entry)
   (and (null (type-daughters type-entry))
        (null (cdr (type-parents type-entry)))))

(defun compute-and-add-glbtypes (types glbp)
   (let*
      ((internal-types
          (cons (car types)
             (remove-if #'external-single-parent-type-p (cdr types))))
       (ntypes (length internal-types)))
      (if (> ntypes 3)
         (let ((*bit-coded-type-table* (make-array ntypes :initial-element nil)))
            (assign-type-bit-codes internal-types ntypes)
            (let ((glbtypes
                    (compute-glbtypes-from-bit-codes internal-types ntypes)))
               (when glbtypes
                  ;; (format t "~A~A" (if glbp "+" " ") (length glbtypes))
                  ;; (force-output t)
                  (insert-glbtypes-into-hierarchy internal-types glbtypes))
               (dolist (type (append glbtypes internal-types))
                  (setf (type-bit-code type) nil))
               (or glbp glbtypes)))
         glbp)))


(defun assign-type-bit-codes (types ntypes)
   ;; assign a bit code to types (of length the number of types), the first
   ;; element being the common ancestor of all of them. Code for each type
   ;; is the OR of all its descendants with one additional bit set
   (let ((n ntypes))
      (labels
         ((assign-type-bit-codes1 (type)
            (let ((code (type-bit-code type)))
               (unless code
                  (setq code (make-bit-code ntypes))
                  (setf (type-bit-code type) code)
                  (dolist (d-name (type-daughters type))
                     (let ((d (get-type-entry d-name)))
                        (when (member d types :test #'eq)
                           (setq code
                              (bit-code-ior code (assign-type-bit-codes1 d) code)))))
                  (decf n)
                  (set-bit-code code n)
                  (push type
                     (get-bit-coded-type *bit-coded-type-table* code)))
               code)))
         (assign-type-bit-codes1 (car types)))))


(defun compute-glbtypes-from-bit-codes (types ntypes)
   ;; for every pair of types check if they have any common subtypes (is the
   ;; AND of the two types' codes non-zero?), and if so, if they already have
   ;; a glb type (is there a type with a code equal to the AND of the codes?).
   ;; If not, a glb type is created with this code. Process iterates with new
   ;; types until no more are constructed 
   (let* ((temp (make-bit-code ntypes))
          (new nil) (glbtypes nil))
      (loop
         (unless (cdr types) (return glbtypes))
         (do* ((t1 types (cdr t1)))
              ((null t1))
            (do* ((t2 (cdr t1) (cdr t2))
                  (code-zero-p nil))
                 ((null t2))
               (setq code-zero-p
                  (bit-code-and-zero-p
                     (type-bit-code (car t1)) (type-bit-code (car t2)) temp))
               (when (and (not code-zero-p)
                          (not (lookup-type-from-bits temp)))
                  (let* ((name (make-glb-name nil))
                         (new-type-entry (make-type :name name :glbp t)))
                     (when *display-glb-messages*
	                (format t "~%Fixing ~A and ~A with ~A" 
		           (car t1) (car t2) new-type-entry))
                     (setf (type-bit-code new-type-entry) temp)
                     (push new-type-entry
                        (get-bit-coded-type *bit-coded-type-table* temp))
                     (push new-type-entry glbtypes)
                     (push new-type-entry new)
                     (setq temp (make-bit-code ntypes))))))
         (setq types new new nil))))


(defun insert-glbtypes-into-hierarchy (types glbtypes
                                       &aux (all-types (append glbtypes types)))
   ;; work out the parents and daughters of each glb type and insert it into
   ;; the standard linked type node representation of the hierarchy
   (dolist (glbtype-entry glbtypes)
      (let ((parents nil) (daughters nil))
         (dolist (entry all-types)
            (unless (eq entry glbtype-entry)
               (cond
                  ((bit-code-subsume-p (type-bit-code glbtype-entry) (type-bit-code entry))
                     ;; entry is a descendent of glbtype - try and add it to the current
                     ;; highest disjoint set of descendants. If it subsumes any elements
                     ;; of the set, replace one of them and delete rest. If it's subsumed
                     ;; by any, then don't consider this entry further
                     (do ((tail daughters (cdr tail))
                          (replacedp nil))
                         ((null tail)
                          (setq daughters (delete nil daughters))
                          (unless replacedp (push entry daughters)))
                        (cond
                           ((bit-code-subsume-p
                               (type-bit-code entry) (type-bit-code (car tail)))
                              (setf (car tail) (if replacedp nil entry))
                              (setq replacedp t))
                           ((and (not replacedp)
                               (bit-code-subsume-p
                                  (type-bit-code (car tail)) (type-bit-code entry)))
                              (return)))))
                  ((bit-code-subsume-p (type-bit-code entry) (type-bit-code glbtype-entry))
                     ;; entry is an ancestor of glbtype - try and add it to lowest
                     ;; disjoint set of ancestors
                     (do ((tail parents (cdr tail))
                          (replacedp nil))
                         ((null tail)
                          (setq parents (delete nil parents))
                          (unless replacedp (push entry parents)))
                        (cond
                           ((bit-code-subsume-p
                               (type-bit-code (car tail)) (type-bit-code entry))
                              (setf (car tail) (if replacedp nil entry))
                              (setq replacedp t))
                           ((and (not replacedp)
                               (bit-code-subsume-p
                                  (type-bit-code entry) (type-bit-code (car tail))))
                              (return))))))))
         (insert-new-type-into-hieriarchy
            (type-name glbtype-entry) glbtype-entry parents daughters))))

         
(defun insert-new-type-into-hieriarchy (new-type new-type-entry parents daughters)
   ;; ancestors and descendants are updated later in a single pass
   (create-mark-field new-type-entry)
   (set-type-entry new-type new-type-entry)   
   (setf (type-daughters new-type-entry) (mapcar #'type-name daughters))
   (setf (type-parents new-type-entry) (mapcar #'type-name parents))
   (dolist (daughter daughters)
      (setf (type-parents daughter)
	    (cons new-type (set-difference (type-parents daughter) 
				           parents :test #'eq))))
   (dolist (parent parents)
      (setf (type-daughters parent)
	    (cons new-type (set-difference (type-daughters parent) 
                                           daughters :test #'eq))))
   (push new-type *ordered-glbtype-list*)
   (push new-type *type-names*)
   new-type-entry)


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

(defun abbrev-type-name (dtr)
  (let ((strname (string dtr)))
    (if (> (length strname) 3)
      (subseq strname 0 3)
      strname)))
|#


;;; Constraint stuff

(defun expand-and-inherit-constraints nil
   (let ((ok t))
      (unmark-type-table)
      ;; a. determine which types are atomic
      ;;    can't do unification really without this info
      ;; 
      ;; b. expand constraints by first forming fstructure from
      ;; any constraint spec and then unifying with
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
      ((seen-node-p type-entry) (type-inherited-constraint type-entry))
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
                              (setf (type-inherited-constraint type-entry) full-constraint)
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
                   (type-parents type-entry))))
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
  (let ((ok t)
        (*unify-debug-cycles* t))       ; turn on cyclic dag warning messages
    (unmark-type-table)
    (for type-name in *type-names*
	 do
	 (unless (leaf-type-p (get-type-entry type-name))
	   (unless 
               (progn (setf *well-formed-trace* nil)
                      (wf-constraint-of type-name))
	     (setf ok nil))))
    (setf *well-formed-trace* nil)
    (unmark-type-table)
    ;; !!! can't create cyclic dags so don't check for them
    ok))

(defun wf-constraint-of (type-name)
  ;; may need to be copied completely before use
  ;; (print (list '> 'wf-constraint-of type-name))
  (let ((type-entry (get-type-entry type-name)))
    (unless (seen-node-p type-entry)
        (when (member type-name *well-formed-trace*)
          (error "~%~A is used in expanding its own constraint 
                    expansion sequence: ~A"  type-name
                 *well-formed-trace*))
        (push type-name *well-formed-trace*)
        (if (type-appfeats type-entry)
          (let ((new-dag (type-inherited-constraint type-entry)))
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
                          (format t "~%Warning: cycle in well-formed constraint for ~A" type-name)))
                    (format t "~%Warning: cannot make constraint for ~A well-formed" type-name))
                (invalidate-marks)
                )))
          (setf (type-constraint type-entry)
            (type-inherited-constraint type-entry)))
        (mark-node-seen type-entry))
    ;; (print (list '< 'wf-constraint-of type-name))
    (when (type-constraint type-entry)
      (setf (type-inherited-constraint type-entry) nil))
    ;;; strong typing has worked, so save some space - otherwise leave 
    ;;; the old structure around for debugging
    (type-constraint type-entry)))                   

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


