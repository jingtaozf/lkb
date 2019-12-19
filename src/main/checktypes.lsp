;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package :lkb)

;;; Checking the type hierarchy to see if it meets 
;;; the various constraints

;;; general purpose functions for creating symbols associated with a 'template' ...

(defun next (template)
   (let*
      ((num (incf (get template 'last-number 0)))
       (instance
         (intern
           (concatenate 'string (symbol-name template) (princ-to-string num)))))
      (declare (number num))
      (push instance (get template 'children))
      (setf (get instance 'root-template) template)
      instance))

;;; ... and destroying them

(defun scratch (templates)
    ;; see the function Next - scratch removes all info from the template symbols
    ;; used by Next and in effect reinitialises the values.  It can take a single
    ;; item or a list of templates to be reinitialised.
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

(defun add-type-from-file (name parents constraint default comment 
                           &optional daughters)
  ;;; YADU --- extra arg needed
  (if *leaf-type-addition*
    (add-leaf-type name parents constraint default comment daughters)
    (let ((existing-type (get-type-entry name)))
      (when existing-type
        (format t "~%WARNING: Type `~A' redefined" name)
        (push name *type-redefinitions*))
      (let ((new-type 
               (make-ltype :name name 
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
         (set-type-entry name new-type)))))

(defun amend-type-from-file (name parents constraint default comment)
  (let ((ok t)
        (existing-type (get-type-entry name)))    
    (if existing-type
	(if (type-parents-equal parents (ltype-parents existing-type))
	    (progn 
	      (setf (ltype-constraint-spec existing-type) constraint)
	      (setf (ltype-default-spec existing-type) default)
	      (setf (ltype-comment existing-type) comment))
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

;;; ERB (2004-08-10) Allow users to add info to a type that's
;;; already been defined.  

(defun add-info-to-type-from-file (name parents constraint default comment)
  (let ((existing-type (get-type-entry name)))
    (unless existing-type
      (cerror "Cancel load" "Cannot add information to type ~a as it is not yet defined" name))
    (let* ((existing-parents (ltype-parents existing-type))
	   (existing-constraint (ltype-constraint-spec existing-type))
	   (existing-default (ltype-default-spec existing-type))
	   (existing-comment (ltype-comment existing-type))
	   (redundant-parents (when (listp existing-parents)
				(loop for parent in parents
				      if (member parent existing-parents)
				      collect parent))))
      (unless (null redundant-parents)
	(cerror "Cancel load" "Cannot add redundant parents ~a to type ~a" redundant-parents name))
      (let ((new-parents (append existing-parents parents))
	    (new-constraint (append existing-constraint constraint))
	    (new-default (append existing-default default))
	    (new-comment (if existing-comment
			     (concatenate 'string existing-comment (string #\newline) comment)
			   comment)))
	(setf (ltype-parents existing-type) new-parents)
	(setf (ltype-constraint-spec existing-type) new-constraint)
	(setf (ltype-default-spec existing-type) new-default)
	(setf (ltype-comment existing-type) new-comment)))))



(defun type-parents-equal (new-parents old-parents)
    (and (null (set-difference new-parents old-parents))
         (null (set-difference old-parents new-parents))))


;;;

(defvar *type-names* nil)

(defvar *partitions* nil)

(defparameter *hierarchy-only-p* nil)

(defparameter *display-glb-messages* nil
   "if set, informs user of glbtypes as they are created")

(defparameter *display-glb-summary* nil
   "if set, summary statistics are displayed about glbtype creation")


(defun check-type-table nil
  (macrolet
    ((report-glb-time (&body forms)
       `(let ((.start-time. (get-internal-run-time))
             #+:sbcl (.start-bytes. (sb-ext:get-bytes-consed)))
          ,@forms
          (when *display-glb-summary*
            (format t "~%Glb computation took ~,3F seconds of run time"
              (/ (- (get-internal-run-time) .start-time.) internal-time-units-per-second))
            #+:sbcl
            (let ((allocated (- (sb-ext:get-bytes-consed) .start-bytes.)))
              (when (> allocated 0) (format t ", ~:D bytes allocated" allocated)))
            (force-output)))))
    (scratch 'glbtype)
    (format t "~%Checking type hierarchy")
    (force-output) 
    (setq *type-names* (sort (collect-type-names) #'string-lessp))
    (when (and *toptype* (add-daughters-to-type-table) (check-for-cycles-etc *toptype*))
      (unmark-type-table)
      (set-up-ancestors-and-descendants)
      (format t "~%Checking for unique greatest lower bounds")
      (force-output) 
      (report-glb-time ; this block is suitable place for execution profiling
        (let ((*partitions* nil) (nglbs 0))
          (find-good-partitions *toptype*)
          (unmark-type-table)
          ;; A partition of 5 types is the smallest to potentially need new glb types. Sort
          ;; partitions so behaviour is likely to be the same if grammar has not changed
          (setq *partitions*
            (sort (remove-if #'(lambda (p) (< (length p) 5)) *partitions*)
               #'> :key #'length))
          (when (and *display-glb-summary* *partitions*)
            (format t "~%Identified ~A non-trivial type partition~:p" (length *partitions*)))
          (dolist (partition *partitions*)
            (incf nglbs (compute-and-add-glbtypes partition)))
          (if (zerop nglbs)
            (format t "~%No glb types needed") (format t "~%Created ~A glb type~:p" nglbs))
          (force-output)))
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
                t))))))))


(defun patch-type-table nil
  ;;; added for the case where definitions are changed, but the hierarchy
  ;;; itself is unaltered
  (clear-types-for-patching-constraints)
  (loop for name in *type-names*
         do
         (let* ((type-entry (get-type-entry name)))
           (when (leaf-type-p type-entry)
             (setf (leaf-type-expanded-p type-entry) nil))
           (setf (ltype-constraint type-entry) nil)
           (setf (ltype-atomic-p type-entry) nil)
           (setf (ltype-tdfs type-entry) nil)
           (setf (ltype-appfeats type-entry) nil)
           (setf (ltype-constraint-mark type-entry) nil)          
           (setf (ltype-local-constraint type-entry) nil)))
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
   ;; for each parent link, add the corresponding daughter link - also do some consistency
   ;; checks on parent specs
   (loop for name in *type-names*
      with ok = t
      do
      (loop for (parent . tail) on (ltype-parents (get-type-entry name)) 
         do
         (if (member parent tail :test #'eq)
            (format t "~%~A has repeated parent ~A - repeat ignored"
               name parent)
            (let ((parent-entry (get-type-entry parent)))
               (cond 
                  (parent-entry 
                     (push name (ltype-daughters parent-entry)))
               (t
                  (setf ok nil)
                  (format t "~%~A specified to have non-existent parent ~A"
                     name parent))))))
      finally (return ok)))

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
  (or (seen-node-p type-record) 
      (progn
        (mark-node-seen type-record)
        (not
         (dolist (daughter (ltype-daughters type-record))
           (unless 
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
                       inner-ok))))
             (return t)))))))


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
        (daughters (loop for d in (ltype-daughters type-record)
                         collect (get-type-entry d))))
    (loop for daughter in daughters
         do
         (if (active-node-p daughter)
             (progn 
               (setf ok nil)
               (format t "~%Redundancy involving ~A" 
                       (ltype-name daughter)))
           (mark-node-active daughter)))
    (when ok
      (setf ok 
        (not
         (dolist (daughter daughters)
           (unless
               (mark-for-redundancy daughter)
             (return t)))))
      (loop for daughter in daughters
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


;;; Set up descendants and ancestors - done before glb computation because needed for
;;; partition finding, and after as well since it would be tricky to keep them updated
;;; while processing each partition separately and ignoring authored types that are
;;; 'inactive' wrt glbs

(defmacro loop-across-bit-vector-1-bits ((x bv . keys) &body body)
  ;; similar to dotimes, except that x is set to the index of successive 1 bits of bv
  (let ((s (gensym)) (v (gensym)))
    `(loop
       with ,v of-type simple-bit-vector = ,bv
       with ,s of-type fixnum = ,(getf keys :start 0)
       for ,x = (position 1 ,v :start ,s
                  ,@(if (getf keys :end) `(:end ,(getf keys :end))))
       while ,x
       do
       (locally (declare (fixnum ,x)) (setq ,s (1+ ,x)) ,@body))))


(defun set-up-ancestors-and-descendants ()
  ;; JAC 18-Jan-18: compute a temporary bit vector representation of descendants then
  ;; use this to construct ancestors and descendants lists. Could be done more efficiently
  ;; (e.g. by iterating over types, recording daughter links and computing transitive
  ;; closure), but instead we recurse down daughters lists in order to check hierarchy
  ;; connectivity
  (let*
    ((ntypes (hash-table-count *types*))
     (index-to-type (make-array ntypes))
     (type-index-table (make-hash-table :test #'eq :size ntypes)))
    (labels
      ((compute-descendant-bit-codes (entry)
         (cond
           ((ltype-descendants entry))
           ((null (ltype-daughters entry))
             ;; don't create a bit vector, just return the type's (integer) index
             (setf (ltype-descendants entry) (gethash entry type-index-table)))
           (t
             (setf (ltype-descendants entry) t)
             (loop
               with desc = (make-array ntypes :element-type 'bit :initial-element 0)
               for daughter in (ltype-daughters entry)
               for dcode = (compute-descendant-bit-codes (get-type-entry daughter))
               do
               (cond
                 ((eq dcode t)
                    (error "Inconsistency - circularity involving type ~A" (ltype-name entry)))
                 ((numberp dcode)
                    (setf (sbit desc dcode) 1))
                 (t (setq desc (bit-ior desc dcode t))))
               finally
               (setf (sbit desc (gethash entry type-index-table)) 1)
               (return (setf (ltype-descendants entry) desc)))))))
      (loop
        for entry being each hash-value in *types*
        for n from 0
        do
        (setf (ltype-ancestors entry) nil)
        (setf (ltype-descendants entry) nil)
        (setf (gethash entry type-index-table) n)
        (setf (svref index-to-type n) entry))
      (compute-descendant-bit-codes (get-type-entry *toptype*))
      (loop
        for entry being each hash-value in *types*
        do
        (let ((desc (ltype-descendants entry)))
          (unless desc
            (error
"Inconsistency - type ~A unreachable in a top-down traversal of the type hierarchy"
                (ltype-name entry)))
          (setf (ltype-descendants entry) nil) ; descendants slot now becomes a list
          (when (bit-vector-p desc)
            (loop-across-bit-vector-1-bits (n desc)
              (let ((d (svref index-to-type n)))
                (unless (eq d entry)
                  (push entry (ltype-ancestors d))
                  (push d (ltype-descendants entry)))))))))))


;;; Compute partitions of the hierarchy, returning a list of lists of
;;; nodes which are mutually independent. Shortens the type bit
;;; representations and reduces the number of comparisons performed from
;;; ntypes^2 to (a^2 + b^2 + ...) where a,b,... are sizes of partitions.
;;; The key test is whether for each descendant of a type x, each of that
;;; descendant's parents are one of x's descendants - i.e. its parents
;;; remain within the partition's 'envelope'

(defun find-good-partitions (type)
  ;; AAC - Oct 12 1998 - faster version
  #+:baseglb
  (push ; put all types in the same partition
     (mapcar #'(lambda (x) (get-type-entry x)) (cons type (remove type *type-names*)))
     *partitions*)
  #-:baseglb
  (let* ((type-entry (get-type-entry type))
         (daughters (ltype-daughters type-entry)))
     (when (and (not (active-node-p type-entry))
                (not (seen-node-p type-entry)))
        (mark-node-active type-entry)
        (when daughters
           (dolist (daughter daughters)
	      (find-good-partitions daughter))
           (let ((descendants (ltype-descendants type-entry))
                 (desc-names-non-leaf (make-hash-table :test #'eq)))
              (loop
                 for d in descendants
                 when (ltype-daughters d) ; leaf types are irrelevant
                 do (setf (gethash (ltype-name d) desc-names-non-leaf) t))
              (when
                 (loop
                    for descendant in descendants
                    always
                    (or (seen-node-p descendant)
                        (null (cdr (ltype-parents descendant)))
                        (loop for p in (ltype-parents descendant)
                           always
                           (gethash p desc-names-non-leaf))))
                 (let ((partition-nodes
                       (loop for descendant in descendants
                             when (not (seen-node-p descendant))
			     collect (progn (mark-node-seen descendant)
			                    descendant))))
                    (when partition-nodes
		       (push (cons type-entry partition-nodes) *partitions*)))))))))


;;; Glb type computation. Assigns a (temporary) bit representation for
;;; each type in heirarchy, and uses it to efficiently check if each pair
;;; of types has a glb and add it if not.

;;; There are 4 read/compile-time switches (3 features and 1 constant):
;;;   bitseqbc   straightforward native bit vector implementation of bit codes
;;;   wholebc    bit codes processed as whole units without variable start/end indices
;;;   baseglb    baseline glb algorithm, as implemented by Ulrich Callmeier in PET
;;;   +bc-nsum+  number of summary words
;;; By default the three features are off and +bc-nsum+ is 3, which gives the most
;;; efficient implementation.

;;; The code below implements the type bit operations using a straightforward native
;;; bit vector representation. This approach is relatively slow in practice, but the
;;; code is included as a reference implementation and to help understand the more
;;; complex but much faster approach below.

#+:bitseqbc
(locally
#-:tdebug (declare (optimize (speed 3) (safety 0)))

(defun make-bit-code (nbits)
   (declare (fixnum nbits))
   (make-array nbits :element-type 'bit :initial-element 0))

(defmacro bit-code-start (c) 0)
(defsetf bit-code-start (c) (value)
  (error "Attempt to set bit-code start when option not available"))

(defmacro bit-code-end (c) `(1- (length ,c)))
(defsetf bit-code-end (c) (value)
  (error "Attempt to set bit-code end when option not available"))

(defmacro bit-code-first (c) 0)
(defsetf bit-code-first (c) (value)
  (error "Attempt to set bit-code first when option not available"))

(defun clear-bit-code (c)
   (declare (simple-bit-vector c))
   (fill c 0))

(defun copy-bit-code (c)
   (declare (simple-bit-vector c))
   (copy-seq c))

(defun finalise-bit-code (c) c)

(defun set-bit-code (c n)
   (declare (simple-bit-vector c) (fixnum n))
   (setf (sbit c n) 1))

(defun bit-code-ior (c1 c2 c3)
   (declare (simple-bit-vector c1 c2 c3))
   (bit-ior c1 c2 c3))

(defmacro bit-code-zero/one-1-bit-p (c)
   ;; does c have only zero or one bits set?
   `(locally
       #-:tdebug (declare (optimize (speed 3) (safety 0)))
       (let ((pos1 (position 1 (the simple-bit-vector ,c))))
          (and pos1 (not (find 1 (the simple-bit-vector ,c) :start (1+ pos1)))))))

(defun bit-code-and-zero/one-p (c1 c2 c3)
   (declare (simple-bit-vector c1 c2 c3))
   ;; c3 <- c1 AND c2 (destructive)
   ;; and return a boolean indicating whether c3 would be all zero (and optionally
   ;; furthermore whether only one bit in c3 is set), although if returning true
   ;; then the contents of c3 are not used
   (not (find 1 (bit-and c1 c2 c3))))

(defun bit-code-position-1 (c)
   (declare (simple-bit-vector c))
   ;; (zero-based) index of first bit that is set in c, or undefined if no 1 bits
   (position 1 c))

(defun bit-code-equal-p (c1 c2)
   (declare (simple-bit-vector c1 c2))
   (equal c1 c2))

(defun bit-code-two-1-bits-p (c)
   (declare (simple-bit-vector c))
   ;; does c have at most 2 bits set?
   (let* ((pos1 (position 1 c))
          (pos2 (and pos1 (position 1 c :start (1+ pos1)))))
      (and pos2 (not (find 1 c :start (1+ pos2))))))

(let ((temp (make-array 0 :element-type 'bit)))
   (defun bit-code-subsume-p (c1 c2)
      (declare (simple-bit-vector c1 c2 temp))
      ;; does code c1 subsume c2? i.e. for every bit not set in c1, is the corresponding
      ;; bit in c2 also unset?
      (unless (= (length temp) (length c1))
         (setq temp
            (make-array (length c1) :element-type 'bit)))
      (not (find 1 (bit-andc1 c1 c2 temp))))
)

(defmacro loop-across-bit-code-1-bits ((x bc) &body body)
   `(loop-across-bit-vector-1-bits (,x ,bc) ,@body))

(defun print-bit-code (c) (print c))
)


;;; The preceding bit code representation of types as bit vectors will not be efficient
;;; unless logical operations on them are performed 32 or 64 bits at a time. Although one
;;; can usually trust the Common Lisp system to do this effectively, the algorithms will
;;; also not be efficient unless logical operations used as predicates may exit early,
;;; and unless logical operations can be applied to sub-sequences of bits between
;;; indices determined at runtime. In Common Lisp there is no portable way of referencing
;;; segments of bit vectors except by moving subsequences of bits to separate vectors or
;;; creating new arrays that are displaced to the originals - but this entails allocating
;;; some memory every time. The best practical approach is to represent each type code by
;;; a vector of fixnums, and to program the operations on codes ourselves 64 or so bits at
;;; a time. The scheme as implemented here is completely portable and should run efficiently
;;; in any Common Lisp system. (A related but non-portable approach is described by Henry
;;; Baker "Efficient implementation of bit-vector operations in Common Lisp", ACM Lisp
;;; Pointers, 3(2-4).)
;;;
;;; JAC 15-Aug-17: added one or more "summary words" for each type, held in the first
;;; +bc-nsum+ element(s) of the type's vector - each successive bit in these summary
;;; words is the logical OR of each successive 64 (or so) bits of the type bit code.
;;; Given two types, if the result of ANDing these summary words is all zeros, then
;;; it's guaranteed that the AND of the full representations will also be all zeros.
;;; Similarly, if the result of ANDing the complement of one set of summary words with
;;; another is non-zero, then the former type cannot subsume the latter. See
;;; finalise-bit-code, bit-code-and-zero/one-p and bit-code-subsume-p below.
;;;
;;; Pre-dating this implementation, in 2011 Glenn Slayden <glenn@agree-grammar.com>
;;; independently devised and implemented summary bits for sparse type bit codes -
;;; see code posted at http://agree-grammar.com/src/miew/bitarr.cs . The design and
;;; purpose of that implementation is very similar to the one here.
;;;
;;; JAC 8-Oct-17: added two further initial words holding the index of the first
;;; and last non-zero words in the type bit code. These are used to skip initial and final
;;; sequences of zero bits in the AND, subsumption and equal operations. The start index
;;; is also used to sort and index lists of types, and both the start and end indexes are
;;; used as pre-filters and early termination tests for the computations involving AND
;;; and subsumption. These start and end indices are very effective due to the sparseness
;;; of the type bit codes, and the fact that since the hierarchy is tree-like it is
;;; possible to assign bit vector indices that are close to types that are close in the
;;; hierarchy. NB outside the start-end region of the bit code there may be non-zero
;;; words left over from previous operations - these must not be accessed.
;;;
;;; Pre-dating this implementation, in PET, Ulrich Callmeier experimented with a method of
;;; "zoning" bit codes so operations such as the subtype test are restricted to just the
;;; region containing 1 bits; he partially implemented this but then removed the code on
;;; 05/30/03 - see http://pet.opendfki.de/changeset/99
;;;
;;; JAC 13-Mar-18: changed bit codes so they don't explicitly store leading or trailing
;;; words that are all zero - this can be thought of as a 'compressed' state. With this
;;; approach (depending on the grammar), bit codes occupy as little as 4% of the storage
;;; they required previously. Now, 'start' and 'end' are logical indices which do not
;;; necessarily key into the vector directly, and there is a further field, 'first',
;;; holding the actual vector index of the first non-zero word (i.e. in the case of
;;; compressed bit codes its value is always +bc-nsum+ + 3, and for uncompressed bit
;;; codes its value is the same as 'start').
;;;
;;; The memory layout of these bit code representations is:
;;; #(<summary words> <logical start of non-zero words> <logical end of non-zero words>
;;;   <actual index of first non-zero word> <type bit code words>)
;;;
;;; For example, the following are valid uncompressed and compressed representations
;;; respectively of the same data (assuming 1 summary word):
;;; #(#x0180000000000000 7 8 7 0 0 0 #x0000000000000001 #x0000000000000002 0 0) 
;;; #(#x0180000000000000 7 8 4 #x0000000000000001 #x0000000000000002) 
;;;
;;; We use simple general vectors of fixnums; an alternative would be specialized arrays
;;; with element-type (unsigned-byte 64), but that would prevent us storing (fixnum)
;;; vector indices in the same array, thus entailing an extra level of indirection for
;;; accessing the bit codes. For the bit code elements we use only positive fixnums, since
;;; using negative as well yields only one more bit, at the cost of complicating the bit
;;; setting and testing operations.
;;;
;;; NB The summary words are consistent at the points where the AND and subsumption
;;; operations are applied. DO NOT attempt to use them in the bit code equal, OR
;;; or position-1 functions.

#-:bitseqbc
(locally
#-:tdebug (declare (optimize (speed 3) (safety 0)))

(defconstant +bc-word-len+ (integer-length most-positive-fixnum)) ; not using negative fixnums

(defconstant +bc-nsum+ #-:baseglb 3 #+:baseglb 0) ; number of summary words (may be zero)

(defconstant +bc-sef+ #-:wholebc 3 #+:wholebc 0) ; start/end/first slots, when applicable

(defun make-bit-code (nbits)
   (declare (fixnum nbits))
   (let
      ((c (make-array
             (+ +bc-nsum+                      ; summary words
                +bc-sef+                       ; start, end, first
                (ceiling nbits +bc-word-len+)) ; type bit code words
             :element-type t)))
      (clear-bit-code c)
      c))

(defmacro bit-code-start (c)
   `(the fixnum
      (locally #-:tdebug (declare (optimize (speed 3) (safety 0)))
         #-:wholebc
         (svref (the simple-vector ,c) +bc-nsum+)
         #+:wholebc
         (+ +bc-nsum+ +bc-sef+))))

(defsetf bit-code-start (c) (value)
   #-:wholebc `(setf (svref ,c +bc-nsum+) ,value)
   #+:wholebc (error "Attempt to set bit-code start when option turned off"))

(defmacro bit-code-end (c)
   `(the fixnum
      (locally #-:tdebug (declare (optimize (speed 3) (safety 0)))
         #-:wholebc
         (svref (the simple-vector ,c) (+ +bc-nsum+ 1))
         #+:wholebc
         (1- (length (the simple-vector ,c))))))

(defsetf bit-code-end (c) (value)
   #-:wholebc `(setf (svref ,c (+ +bc-nsum+ 1)) ,value)
   #+:wholebc (error "Attempt to set bit-code end when option turned off"))

(defmacro bit-code-first (c)
   `(the fixnum
      (locally #-:tdebug (declare (optimize (speed 3) (safety 0)))
         #-:wholebc
         (svref (the simple-vector ,c) (+ +bc-nsum+ 2))
         #+:wholebc
         (+ +bc-nsum+ +bc-sef+))))

(defsetf bit-code-first (c) (value)
   #-:wholebc `(setf (svref ,c (+ +bc-nsum+ 2)) ,value)
   #+:wholebc (error "Attempt to set bit-code first when option turned off"))

(defun clear-bit-code (c)
   (declare (simple-vector c))
   #+:wholebc (fill c 0)
   #+(and (not :wholebc) :tdebug) (fill c 'x) ; provoke error for accesses outside start-end
   #-:wholebc (setf (bit-code-start c) (1- (length c))) ; counter-intuitive, but correct
   #-:wholebc (setf (bit-code-end c) (1- (+ +bc-nsum+ 3)))
   #-:wholebc (setf (bit-code-first c) (bit-code-start c)))

(defun copy-bit-code (c) ; returns compressed result
   (declare (simple-vector c))
   (let*
      ((n-code-words (- (1+ (bit-code-end c)) (bit-code-start c)))
       (new
         (make-array (+ +bc-nsum+ +bc-sef+ n-code-words) :element-type t)))
      (replace new c :end1 (+ +bc-nsum+ +bc-sef+)) ; copy across summary words, start, end
      #-:wholebc (setf (bit-code-first new) (+ +bc-nsum+ 3))
      (replace new c
         :start1 (+ +bc-nsum+ +bc-sef+)
         :start2 (bit-code-first c) :end2 (+ (bit-code-first c) n-code-words))
      new))

(defun finalise-bit-code (c)
   (declare (simple-vector c))
   (when (> +bc-nsum+ 0) ; summary words to be computed?
      (loop
         initially (fill c 0 :end +bc-nsum+)
         for n of-type fixnum from (bit-code-start c) to (bit-code-end c)
         for nc of-type fixnum from (bit-code-first c)
         unless (zerop (the fixnum (svref c nc)))
         do
         (multiple-value-bind (sw-elt sw-bit)
               (truncate n +bc-word-len+)
            (declare (fixnum sw-elt sw-bit))
            (setq sw-elt (rem sw-elt +bc-nsum+))
            (setf (svref c sw-elt)
               (logior (the fixnum (svref c sw-elt)) (the fixnum (ash 1 sw-bit)))))))
   c)

(defun set-bit-code (c n) ; assumes c is uncompressed
   (declare (simple-vector c) (fixnum n))
   (multiple-value-bind (e1 e2)
         (truncate n +bc-word-len+)
      (declare (fixnum e1 e2))
      (incf e1 (+ +bc-nsum+ +bc-sef+)) ; convert from zero-based integer to bit code index
      ;; if e1 is outside current start-end interval then zero-fill between it and interval
      #-:wholebc
      (when (< e1 (bit-code-start c))
         (fill c 0 :start e1 :end (bit-code-start c))
         (setf (bit-code-start c) e1)
         (setf (bit-code-first c) e1))
      #-:wholebc
      (when (> e1 (bit-code-end c))
         (fill c 0 :start (1+ (bit-code-end c)) :end (1+ e1))
         (setf (bit-code-end c) e1))
      (setf (svref c e1)
         (logior (the fixnum (svref c e1)) (the fixnum (ash 1 e2))))))

(defun bit-code-ior (c1 c2 c3) ; assumes c3 (= result) is uncompressed and large enough
   (declare (simple-vector c1 c2 c3))
   ;; NB not used as a predicate so summary words are irrelevant
   (loop
      with start of-type fixnum = (min (bit-code-start c1) (bit-code-start c2))
      with end of-type fixnum = (max (bit-code-end c1) (bit-code-end c2))
      with nc1 of-type fixnum = (bit-code-first c1)
      with nc2 of-type fixnum = (bit-code-first c2)
      for n of-type fixnum from start to end
      for c1e of-type fixnum =
        (if (<= (bit-code-start c1) n (bit-code-end c1)) (prog1 (svref c1 nc1) (incf nc1)) 0)
      for c2e of-type fixnum =
        (if (<= (bit-code-start c2) n (bit-code-end c2)) (prog1 (svref c2 nc2) (incf nc2)) 0)
      do
      (setf (svref c3 n) (logior c1e c2e))
      finally
      #-:wholebc (setf (bit-code-start c3) start)
      #-:wholebc (setf (bit-code-end c3) end)
      #-:wholebc (setf (bit-code-first c3) start)
      (return c3)))

(defmacro bit-code-zero/one-1-bit-p (c &optional start end)
   ;; NB c might not have been through finalise-bit-code so we can't use its summary words
   `(locally
       #-:tdebug (declare (optimize (speed 3) (safety 0)))
       #-:wholebc
       (and
          ,(if (and start end)
              `(= ,start ,end) `(= (bit-code-start ,c) (bit-code-end ,c)))
          (let ((e (svref ,c (bit-code-first ,c))))
             (declare (fixnum e))
             ;; the expression e & (e-1) returns e without its least significant 1 bit
             (zerop (logand e (the fixnum (1- e))))))
       #+:wholebc
       (let ((s
               ,(if (and start end)
                   `(and (= ,start ,end) ,start)
                   `(loop for n of-type fixnum from (bit-code-start ,c) to (bit-code-end ,c)
                       with nz = nil ; index of first non-zero word
                       do
                       (unless (zerop (the fixnum (svref ,c n))) (if nz (return nil) (setq nz n)))
                       finally (return nz)))))
          (and s
             (let ((e (svref ,c s)))
                (declare (fixnum e))
                (zerop (logand e (the fixnum (1- e)))))))))

(defmacro bit-code-and-zero/one-p (c1 c2 c3) ; assumes c3 (= result) is uncompressed
   `(locally
       #-:tdebug (declare (optimize (speed 3) (safety 0)))
       (or
          ;; first check the bit code summary words
          ,(if (> +bc-nsum+ 0)
              `(and
                  ,@(loop for i from 0 below +bc-nsum+
                       collect
                       `(zerop (logand (the fixnum (svref ,c1 ,i))
                                       (the fixnum (svref ,c2 ,i)))))))
          (bit-code-and-zero/one-p-1 ,c1 ,c2 ,c3))))

(defun bit-code-and-zero/one-p-1 (c1 c2 c3) ; assumes c3 is uncompressed and large enough
   (declare (simple-vector c1 c2 c3))
   (loop
      with first-poss-nz of-type fixnum = (max (bit-code-start c1) (bit-code-start c2))
      with last-poss-nz of-type fixnum = (min (bit-code-end c1) (bit-code-end c2))
      with o1 of-type fixnum = (- (bit-code-start c1) (bit-code-first c1)) ; no. of omitted leading 0 words
      with o2 of-type fixnum = (- (bit-code-start c2) (bit-code-first c2))
      for n of-type fixnum from first-poss-nz to last-poss-nz
      unless (zerop (logand (the fixnum (svref c1 (- n o1))) (the fixnum (svref c2 (- n o2)))))
      return
      (loop
         with first-actual-nz of-type fixnum = n
         with last-actual-nz of-type fixnum = n
         for m of-type fixnum from first-actual-nz to last-poss-nz
         for res of-type fixnum =
            (logand (the fixnum (svref c1 (- m o1))) (the fixnum (svref c2 (- m o2))))
         do
         (setf (svref c3 m) res)
         (setq last-actual-nz (if (zerop res) last-actual-nz m))
         finally
         #-:wholebc (setf (bit-code-first c3) first-actual-nz)
         (if #-:baseglb (bit-code-zero/one-1-bit-p c3 first-actual-nz last-actual-nz)
             #+:baseglb nil
            (return t)
            (progn
               #+:wholebc (fill c3 0 :end first-actual-nz)
               #+:wholebc (fill c3 0 :start (1+ last-actual-nz))
               #-:wholebc (setf (bit-code-start c3) first-actual-nz)
               #-:wholebc (setf (bit-code-end c3) last-actual-nz)
               (return nil))))
      finally (return t)))

(defun bit-code-position-1 (c)
   (declare (simple-vector c))
   ;; NB c may not have been through finalise-bit-code so we can't use its summary words
   (let*
      ((start #-:wholebc (bit-code-start c)
              #+:wholebc
              (loop for n of-type fixnum from (bit-code-start c) to (bit-code-end c)
                    while (zerop (the fixnum (svref c n)))
                    finally (return n)))
       (e (svref c #-:wholebc (bit-code-first c) #+:wholebc start)))
      (declare (fixnum e start))
      (the fixnum
         (+ (the fixnum (* (- start (+ +bc-nsum+ +bc-sef+)) ; convert from index to 0-based integer
                           +bc-word-len+))
            ;; the expression e & -e returns the least significant 1 bit in e
            (1- (integer-length (logand e (the fixnum (- e)))))))))

(defmacro bit-code-equal-p (c1 c2)
   ;; NB c2 hasn't been through finalise-bit-code so we can't use summary words
   `(locally
       #-:tdebug (declare (optimize (speed 3) (safety 0)))
       (and #-:wholebc (= (bit-code-start ,c1) (bit-code-start ,c2))
            #-:wholebc (= (bit-code-end ,c1) (bit-code-end ,c2))
            (bit-code-equal-p-1 ,c1 ,c2))))

(defun bit-code-equal-p-1 (c1 c2)
   (declare (simple-vector c1 c2))
   (loop
      for n of-type fixnum from (bit-code-start c1) to (bit-code-end c1)
      for nc1 of-type fixnum from (bit-code-first c1)
      for nc2 of-type fixnum from (bit-code-first c2)
      always (= (the fixnum (svref c1 nc1)) (the fixnum (svref c2 nc2)))))

(defun bit-code-two-1-bits-p (c)
   (declare (simple-vector c))
   ;; only called twice on each glb type, so not worth filtering with summary words
   (loop for n of-type fixnum from (bit-code-start c) to (bit-code-end c)
      for nc of-type fixnum from (bit-code-first c)
      with sum of-type fixnum = 0
      do (incf sum (logcount (the fixnum (svref c nc))))
      always (<= sum 2)))

(defmacro bit-code-subsume-p (c1 c2)
   `(locally
       #-:tdebug (declare (optimize (speed 3) (safety 0)))
       (and
          ;; first check the bit code summary words
          ,@(loop for i from 0 below +bc-nsum+
              collect
              `(zerop (logandc1 (the fixnum (svref ,c1 ,i))
                                (the fixnum (svref ,c2 ,i)))))
          ;; and also check start/end index compatibility
          #-:wholebc
          (and (<= (bit-code-start ,c1) (bit-code-start ,c2))
               (>= (bit-code-end ,c1) (bit-code-end ,c2)))
          (bit-code-subsume-p-1 ,c1 ,c2))))

(defun bit-code-subsume-p-1 (c1 c2)
   (declare (simple-vector c1 c2))
   (loop
      for n of-type fixnum from (bit-code-start c2) to (bit-code-end c2) ; NB c1 bounds are irrelevant
      for nc1 of-type fixnum from (+ (bit-code-first c1) (- (bit-code-start c2) (bit-code-start c1)))
      for nc2 of-type fixnum from (bit-code-first c2)
      always
      (zerop (logandc1 (the fixnum (svref c1 nc1)) (the fixnum (svref c2 nc2))))))

(defmacro loop-across-bit-code-1-bits ((x bc) &body body)
  (let ((n (gensym)) (nc (gensym)) (e (gensym)) (i (gensym)) (c (gensym)) (m (gensym)))
    `(loop
       with ,c of-type simple-vector = ,bc
       for ,n of-type fixnum from (bit-code-start ,c) to (bit-code-end ,c)
       for ,nc of-type fixnum from (bit-code-first ,c)
       for ,e of-type fixnum = (svref ,c ,nc)
       unless (zerop ,e)
       do
       (loop for ,i of-type fixnum from 0 below +bc-word-len+
         with ,m of-type fixnum = (* (the fixnum (- ,n (+ +bc-nsum+ +bc-sef+))) ; convert to 0-based integer
                             +bc-word-len+)
         when (logbitp ,i ,e)
         do
         (let ((,x (+ ,m ,i)))
           (declare (fixnum ,x))
           ,@body)))))

(defun print-bit-code (c)
   (flet
      ((list-hex (n m)
         (format nil "(~{~A~^ ~})"
            (loop for i from n to m
               for e = (svref c i)
               with n-hex-digits = (ceiling +bc-word-len+ 4) ; allow for either 32/64-bit system
               collect
               (format nil "#x~V,'0X" n-hex-digits e)))))
      (format t "~%#<bit-code sum=~A start=~A end=~A ints=~A> "
         (list-hex 0 (1- +bc-nsum+))
         (bit-code-start c) (bit-code-end c)
         (list-hex (bit-code-first c)
            (+ (bit-code-first c) (- (bit-code-end c) (bit-code-start c)))))
      c))
)


;;; GLB computation over a partition of the type hierarchy. Main steps:
;;; 
;;; * Assign a bit code to each type
;;; * Check pairs of types and create a GLB type if they have no unique common subtype
;;; * Insert GLB types into hierarchy, recomputing parent/daughter links
;;;
;;; The overall approach to finding GLBs draws on insights in Ait-Kaci et al.'s 1989
;;; paper 'Efficient Implementation of Lattice Operations'.
;;; The basic algorithms used here for assigning bit codes to types, computing
;;; corresponding GLB types, and reconstructing the graph representation of the type
;;; hierarchy were originally published in Ulrich Callmeier's 2001 dissertation
;;; 'Efficient Parsing with Large-Scale Unification Grammars'
;;; http://www.coli.uni-saarland.de/~uc/thesis/thesis.ps (pp 28-30). However, there
;;; are many improvements here which greatly reduce processing time for large and
;;; complex type hierarchies.

(defun compute-and-add-glbtypes (types)
   ;; A type satisfying no-split-no-join-type-p or internal-tree-type-p doesn't need a bit
   ;; code since it is not 'active' with respect to GLB computation: it cannot be a GLB,
   ;; cannot be a parent/daughter of a new GLB type, and cannot cause a new GLB type to
   ;; be created. A type satisfying no-split-type-p can be a GLB, can be a daughter of
   ;; a new GLB type, but cannot cause a new GLB type to be created.
   (labels
      ((no-split-no-join-type-p (type) ; no more than 1 incoming and 1 outgoing arc?
          #+:baseglb nil
          #-:baseglb
          (and (null (cdr (ltype-parents type))) (null (cdr (ltype-daughters type)))))
       (internal-tree-type-p (type)
          #+:baseglb 
          (and (null (ltype-daughters type)) ; leaf type (no children, exactly one parent)?
               (null (cdr (ltype-parents type))))
          #-:baseglb
          (and (null (cdr (ltype-parents type))) ; below root of a tree-shaped part of hierarchy?
             (loop for d in (ltype-daughters type)
                always
                (internal-tree-type-p (get-type-entry d)))))
       (no-split-type-p (type) ; no more than 1 outgoing arc that's non-tree shaped?
          #+:baseglb nil
          #-:baseglb
          (loop for d in (ltype-daughters type)
             count (not (internal-tree-type-p (get-type-entry d))) into sum
             always (< sum 2))))
      (let*
         ((active-types ; top (first) type of the partition is not 'active'
             (remove-if
                #'(lambda (x) (or (no-split-no-join-type-p x) (internal-tree-type-p x)))
                (cdr types)))
          (nactive (length active-types))
          (split-types (remove-if #'no-split-type-p active-types))
          (nsplits (length split-types)))
         ;; minimum to get a new GLB is 4 active types (excluding top type of the partition),
         ;; with at least 2 splits (and 2 joins as well, but these might also be splits)
         (if (and (>= nactive 4) (>= nsplits 2))
            (let ((code-table (make-array nactive :initial-element nil)))
               (when *display-glb-summary*
                  (format t "~%Partition ~A of ~A types (~A active, ~A splits)"
                     (ltype-name (car types)) (length types) nactive nsplits)
                  (force-output))
               (setq active-types
                  (assign-type-bit-codes types active-types nactive code-table))
               (let ((glbtypes
                       (compute-glbtypes-from-bit-codes split-types nactive code-table)))
                  (when (and *display-glb-summary* glbtypes)
                     (format t " -> ~A glb~:p" (length glbtypes))
                     (force-output))
                  (when glbtypes
                     (insert-glbtypes-into-hierarchy active-types glbtypes))
                  (dolist (type-list (list glbtypes active-types))
                     (dolist (type type-list) (setf (ltype-bit-code type) nil)))
                  (length glbtypes)))
            0))))


(defmacro get-type-bit-code (code code-table)
   ;; type bit codes hashed on the position of their first non-zero bit
   `(svref ,code-table (bit-code-position-1 ,code)))

(defun assign-type-bit-codes (types active-types nactive code-table)
   ;; Assign a bit code to each active type. The code for each type is the OR of all
   ;; its descendants with one additional bit set. Traversal of type hierarchy is
   ;; depth-first, assigning type bit positions so that parent has the smallest -
   ;; this parent position is used subsequently to index the type.
   ;; Process only the types list, and pass through any type not in active-types.
   ;; Avoid copying bit codes until a destructive operation needs to be performed.
   ;; Returns a re-ordered active-types list, reflecting the bit position used for
   ;; each type.
   (let ((status (make-hash-table :test #'eq :size nactive))
         (n nactive)
         (temp (make-bit-code nactive))
         (active-ordered nil))
      (labels
         ((length-<= (x y)
            (if (and x y) (length-<= (cdr x) (cdr y)) (null x)))
          (assign-type-bit-codes1 (type)
            (if (gethash type status)
               (or (ltype-bit-code type)
                  (let*
                     ((ds
                        (mapcar #'(lambda (dn) (get-type-entry dn)) (ltype-daughters type)))
                      (subs
                        ;; allocate bit positions to largest descendant sub-graphs first
                        (loop for d in (sort ds #'length-<= :key #'ltype-descendants)
                           when (assign-type-bit-codes1 d)
                           collect it))
                      (code
                        (bit-code-ior-set-n
                           subs (if (eq (gethash type status) :active) (decf n)) temp)))
                     (when (eq (gethash type status) :active)
                        (setq code (finalise-bit-code code))
                        (setf (ltype-bit-code type) code)
                        (push code (get-type-bit-code code code-table))
                        (push type active-ordered))
                     code)))))
         (loop for type in types do (setf (gethash type status) t))
         (loop for type in active-types do (setf (gethash type status) :active))
         (assign-type-bit-codes1 (car types))
         active-ordered)))

(defun bit-code-ior-set-n (subs n temp)
   (if (or (cdr subs) n)
      (progn
         (clear-bit-code temp)
         (dolist (sub subs) (bit-code-ior sub temp temp))
         (when n (set-bit-code temp n))
         (copy-bit-code temp))
      (car subs)))


(defun sort-types-increasing-start-decreasing-end (types)
   ;; primary sort on increasing bit code start, secondary sort on decreasing end
   (sort types
      #'(lambda (c1 c2)
          (or (< (bit-code-start c1) (bit-code-start c2))
              (and (= (bit-code-start c1) (bit-code-start c2))
                   (> (bit-code-end c1) (bit-code-end c2)))))
      :key #'ltype-bit-code))

(defun compute-glbtypes-from-bit-codes (types ntypes code-table)
   ;; For every pair of types check if they have any common subtypes (is the AND of the
   ;; two types' codes non-zero?), and if so, if they already have a glb type (is there
   ;; a type with a code equal to the AND of the codes?). If not, a glb type is created
   ;; with this code. Process iterates with new types until no more are constructed 
   (flet
      ((bit-code-exists-p (code code-table)
         (loop for c in (get-type-bit-code code code-table)
            thereis (bit-code-equal-p c code))))
      (let ((temp (make-bit-code ntypes))
            (new nil) (glbtypes nil))
         (loop
            (unless (cdr types) (return glbtypes))
            ;; The sorting and iteration structure below is optimal in the sense that only
            ;; pairs of types with overlapping descendant bit spans are considered
            (loop
               for t1 on (sort-types-increasing-start-decreasing-end types)
               for t1c = (ltype-bit-code (car t1))
               do
               (loop
                  for t2 on (cdr t1)
                  for t2c = (ltype-bit-code (car t2))
                  until (> (bit-code-start t2c) (bit-code-end t1c)) ; will also hold for rest of t2
                  do
                  (when (and (not (bit-code-and-zero/one-p t1c t2c temp))
                             (not (bit-code-exists-p temp code-table)))
                     (let* ((code (finalise-bit-code (copy-bit-code temp)))
                            (name (next 'glbtype))
                            (glb (make-ltype :name name :glbp t)))
                        (create-mark-field glb)
                        (set-type-entry name glb)   
                        (push name *ordered-glbtype-list*)
                        (push name *type-names*)
                        (when *display-glb-messages*
                           (format t "~%Fixing ~A and ~A with ~A" (car t1) (car t2) glb))
                        (setf (ltype-bit-code glb) code)
                        (push code (get-type-bit-code code code-table))
                        (push glb glbtypes)
                        (unless #-:baseglb (bit-code-two-1-bits-p code) #+:baseglb nil
                           ;; glb has >2 active descendants so it can contribute to a new glb
                           (push glb new))))))
            (setq types new new nil)))))


(defun insert-glbtypes-into-hierarchy (auth glbs)
  ;; Compute the parents and daughters of each existing (authored) active type and each
  ;; new glb type
  (setq glbs
    ;; sort to allow more efficient computation of reachability between pairs of glbs
    (sort-types-increasing-start-decreasing-end (copy-list glbs)))
  (let* ((nauth (length auth))
         (ntypes (+ nauth (length glbs)))
         (int-to-type (make-array ntypes :initial-contents (append auth glbs)))
         (auth-names (make-hash-table :test #'eq :size nauth))
         (path-table (make-array ntypes :initial-element nil)))
    (declare (list auth glbs) (fixnum nauth ntypes) (simple-vector int-to-type path-table))
    ;;
    ;; fill adjacency matrix
    (insert-glbtypes-descendant-relations nauth ntypes int-to-type path-table)
    ;;
    ;; delete this partition's active types from parent lists and from daughter lists (after
    ;; saving previous contents), then read out parent/daughter links from adjacency matrix
    ;; and record them
    (loop for i from 0 below nauth
      do
      (setf (gethash (ltype-name (svref int-to-type i)) auth-names) t))
    (loop for ty in auth
      do
      (setf (ltype-parents ty)
        (remove-if #'(lambda (pn) (gethash pn auth-names)) (ltype-parents ty))))
    (loop for pj from 0 below ntypes
      for p = (svref int-to-type pj)
      for dns = (ltype-daughters p)
      when (svref path-table pj)
      do
      (when (< pj nauth)
        (setf (ltype-daughters p)
          (remove-if #'(lambda (dn) (gethash dn auth-names)) (ltype-daughters p))))
      (loop-across-bit-vector-1-bits (di (svref path-table pj)) ; M(pj,di)=1
        (when (= di pj)
          (error "Inconsistency - glb processing created circularity involving type ~A"
            (ltype-name p)))
        (let ((d (svref int-to-type di)))
          ;; for auth types, don't add a link to another auth type when it wasn't
          ;; previously linked - avoids creating redundant links bypassing inactive types
          (when (or (>= pj nauth) (>= di nauth) (member (ltype-name d) dns :test #'eq))
            (push (ltype-name d) (ltype-daughters p))
            (push (ltype-name p) (ltype-parents d))))))))

#-:baseglb
(defun insert-glbtypes-descendant-relations (nauth ntypes int-to-type path-table)
  (let* ((glb-index-max
           (reduce #'max int-to-type :key #'(lambda (x) (bit-code-end (ltype-bit-code x)))
             :initial-value 0))
         (glb-index
           (make-array (1+ glb-index-max) :initial-element ntypes)))
    (declare (fixnum nauth ntypes) (simple-vector int-to-type path-table glb-index))
    ;;
    ;; initialise adjacency / reachability matrix, omitting row vector if type has no active
    ;; descendants
    (loop for i from 0 below ntypes
      unless (bit-code-zero/one-1-bit-p (ltype-bit-code (svref int-to-type i)))
      do
      (setf (svref path-table i) (make-array ntypes :element-type 'bit :initial-element 0)))
    ;;
    ;; glb-index is used to improve efficiency of auth -> glb reachability computation: element
    ;; i holds index of first glb whose start is at least i
    (loop
      for n from nauth below ntypes
      for s = (bit-code-start (ltype-bit-code (svref int-to-type n)))
      with sprev = -1
      do
      (when (> s sprev)
        (loop for j from (1+ sprev) to s do (setf (svref glb-index j) n))
        (setq sprev s)))
    ;;
    ;; add reachability relations for auth/glb -> auth [0.057]
    (loop for ai from 0 below ntypes
      when (svref path-table ai) ; any active descendants?
      do
      (loop-across-bit-code-1-bits (dj (ltype-bit-code (svref int-to-type ai)))
        (unless (= dj ai)
          (setf (sbit (svref path-table ai) dj) 1))))
    ;;
    ;; add reachability relations for auth (splits only) -> glb - only considers glb types
    ;; satisfying auth-start <= glb-start <= auth-end [0.516]
    (loop for i from 0 below nauth
      for ti = (svref int-to-type i)
      for ci = (ltype-bit-code ti)
      when (cdr (ltype-daughters ti)) ; a split type?
      do
      (loop for j of-type fixnum from (svref glb-index (bit-code-start ci)) below ntypes 
        for cj = (ltype-bit-code (svref int-to-type j))
        until (> (bit-code-start cj) (bit-code-end ci)) ; will also hold for rest of j
        do
        (when (bit-code-subsume-p ci cj)
          (setf (sbit (svref path-table i) j) 1))))
    ;;
    ;; add reachability relations between pairs of glbs - separated into the cases where glb1
    ;; potentially subsumes glb2, and vice-versa. The first inner loop considers only glb2s
    ;; satisfying glb1-start <= glb2-start <= glb1-end. The second considers only glb2s
    ;; satisfying glb2-start = glb1-start and glb2-end >= glb1-end (note that the situation
    ;; glb2-start < glb1-start is impossible due to the initial sorting of glbs) [1.348]
    (loop for i from nauth below ntypes
      for ci = (ltype-bit-code (svref int-to-type i))
      do
      (unless (bit-code-two-1-bits-p ci) ; can't actually subsume other glbs if <=2 bits set
        (loop for j from (1+ i) below ntypes
          for cj = (ltype-bit-code (svref int-to-type j))
          until (> (bit-code-start cj) (bit-code-end ci)) ; will also hold for rest of j
          do
          (when (bit-code-subsume-p ci cj)
            (setf (sbit (svref path-table i) j) 1))))
      (loop for j from (1+ i) below ntypes
        for cj = (ltype-bit-code (svref int-to-type j))
        until (or (/= (bit-code-start cj) (bit-code-start ci))
                  (< (bit-code-end cj) (bit-code-end ci))) ; will also hold for rest of j
        do
        (when (bit-code-subsume-p cj ci)
          (setf (sbit (svref path-table j) i) 1))))
    ;;
    ;; transitive closure to fill in remaining reachability relations - at this point
    ;; all such relations are correct except that the non-split auth type -> glb type
    ;; entries (which occupy one quadrant of the reachability matrix) are missing. We thus
    ;; need to scan only the auth -> auth quadrant, which shares the same rows [0.014]
    (insert-glbtypes-transitive-closure path-table nauth)
    ;;
    ;; transitive reduction to convert reachability relations to adjacency [0.104]
    (insert-glbtypes-transitive-reduction path-table)))

#+:baseglb
(defun insert-glbtypes-descendant-relations (nauth ntypes int-to-type path-table)
    (declare (fixnum nauth ntypes) (simple-vector int-to-type path-table))
    ;;
    ;; initialise adjacency / reachability matrix
    (loop for i from 0 below ntypes
      do
      (setf (svref path-table i) (make-array ntypes :element-type 'bit :initial-element 0)))
    ;;
    ;; add reachability relations for each authored and glb type
    (loop for i from 0 below ntypes
      for ci = (ltype-bit-code (svref int-to-type i))
      do
      (loop-across-bit-code-1-bits (j ci) ; -> auths
        (unless (= j i)
          (setf (sbit (svref path-table i) j) 1)))
      (loop for j of-type fixnum from nauth below ntypes ; -> glbs
        for cj = (ltype-bit-code (svref int-to-type j))
        unless (= j i)
        do
        (when (bit-code-subsume-p ci cj)
          (setf (sbit (svref path-table i) j) 1))))
    ;;
    ;; transitive reduction to convert reachability relations to adjacency
    (insert-glbtypes-transitive-reduction path-table))


(macrolet
  ((create-matrix-function (fn bit-vector-op)
    `(defun ,fn (table &optional (max (length table)))
       ;; process rows and columns 0-max, which might be less than the full table size. NB the
       ;; 'when' tests below deal with rows that were omitted since they would have been all-zero
       (declare (simple-vector table) (fixnum max))
       (loop for i of-type fixnum from 1 below max
         when (svref table i)
         do
         (loop-across-bit-vector-1-bits (j (svref table i) :end i) ; i>j, M(i,j)=1
           (when (svref table j)
             (setf (svref table i) ; row(i) = row(i) <op> row(j)
               (,bit-vector-op (the simple-bit-vector (svref table i))
                 (the simple-bit-vector (svref table j)) t)))))
       (loop for i of-type fixnum from 0 below (1- max)
         when (svref table i)
         do
         (loop-across-bit-vector-1-bits (j (svref table i) :start (1+ i) :end max) ; i<j, M(i,j)=1
           (when (svref table j)
             (setf (svref table i) ; row(i) = row(i) <op> row(j)
               (,bit-vector-op (the simple-bit-vector (svref table i))
                 (the simple-bit-vector (svref table j)) t))))))))
  ;; These transitive closure and reduction functions are based on the following paper:
  ;; Henry Warren (1975) A modification of Warshall's algorithm for the transitive closure of
  ;; binary relations. CACM 18(4), 218-220. Warshall's algorithm iterates across matrix columns,
  ;; whereas Warren's iterates over rows. The latter property makes it possible to skip
  ;; within-row subsequences of 0-bits, and moreover long runs of zeros can be skipped a word
  ;; at a time. Since the matrix is stored row-wise, Warren's algorithm is much more CPU 
  ;; cache-friendly. Also, this implementation allows rows that would have been all-zero to
  ;; be omitted. Transitive reduction is an adaptation of the transitive closure algorithm.
  ;;
  (create-matrix-function insert-glbtypes-transitive-closure bit-ior)
  (create-matrix-function insert-glbtypes-transitive-reduction bit-andc2))


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
      (loop for node in *type-names*
         do
         (let ((type-entry (get-type-entry node)))
           (unless (leaf-type-p type-entry)
            (unless 
               (expand-constraint node type-entry)
               (setf ok nil)))))
      (when ok 
         (check-feature-table))))
         

(defun determine-atomic-types nil 
  (let ((inherit-constraint
          (make-hash-table :test #'eq :size (length *type-names*))))
    (dolist (tn *type-names*)
      (let ((ty (get-type-entry tn)))
        (setf (gethash ty inherit-constraint) ; has any ancestor of this type got a constraint?
          (some #'ltype-constraint-spec (ltype-ancestors ty)))))
    (dolist (node *type-names*)
      (let ((ty (get-type-entry node)))
        (unless (leaf-type-p ty)
          (setf (ltype-atomic-p ty)
            (not 
              (or (ltype-constraint-spec ty)
                  (gethash ty inherit-constraint)
                  (some #'(lambda (desc)
                            (or (ltype-constraint-spec desc)
                                (gethash desc inherit-constraint)))
                    (ltype-descendants ty))))))))))


(defun expand-constraint (node type-entry)
   (cond
      ((seen-node-p type-entry) (ltype-inherited-constraint type-entry))
      (t
         (mark-node-seen type-entry)
         (let* ((*unify-debug-cycles* t) ; turn on cyclic dag warning messages
                (constraint-spec (ltype-constraint-spec type-entry))
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
                        (setf (ltype-local-constraint type-entry) 
                          local-constraint)
                        (let ((local-appfeats 
                                 (top-level-features-of local-constraint)))
                           (loop for feature in local-appfeats
                              do 
                              (add-maximal-type feature node))))
                     ; no need to do inheritance when checking
                     ; for maximal introduction
                     (let ((full-constraint 
                              (inherit-constraints node type-entry local-constraint)))
                        (cond 
                           (full-constraint
                              (setf (ltype-inherited-constraint type-entry) full-constraint)
                              (setf (ltype-appfeats type-entry)
                                 (top-level-features-of full-constraint))
                              full-constraint)
                           (t (format t "~%Type ~A's constraint ~
                                 specification clashes with its parents'" node) 
                              nil)))))))))


(defun inherit-constraints (node type-entry local-constraint)
  (if (ltype-atomic-p type-entry)
      (create-typed-dag node)
    (let ((supers 
	   (mapcar #'(lambda (parent)
		       (expand-constraint parent (get-type-entry parent)))
                   (ltype-parents type-entry))))
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
    (loop for type-name in *type-names*
	 do
	 (unless (leaf-type-p (get-type-entry type-name))
	   (unless 
               (progn (setf *well-formed-trace* nil)
                      (wf-constraint-of type-name))
	     (setf ok nil))))
    (setf *well-formed-trace* nil)
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
        (if (ltype-appfeats type-entry)
          (let ((new-dag (ltype-inherited-constraint type-entry)))
            ;; !!! outside here must stay within current generation
            ;; *unify-generation* is declared global, so save and reset it instead of rebinding 
            (let ((old-unify-generation *unify-generation*)
                  (*within-unification-context-p* t))
              ;; establish new unification generation now, and also at
              ;; end (the usual place)
              (invalidate-marks)
              (prog1
                (if (really-make-features-well-formed new-dag nil type-name)
                    (let ((res (copy-dag new-dag)))
                      (if res
                        (setf (ltype-constraint type-entry) res)
                        (format t "~%Warning: cycle in well-formed constraint for ~A" type-name)))
                    ;; warning msg is excessive
                    ;; (format t "~%Warning: cannot make constraint for ~A well-formed" type-name)
                    nil)
                (invalidate-marks)
                (setq *unify-generation* old-unify-generation))))
          (setf (ltype-constraint type-entry)
            (ltype-inherited-constraint type-entry)))
        (mark-node-seen type-entry))
    ;; (print (list '< 'wf-constraint-of type-name))
    (when (ltype-constraint type-entry)
      (setf (ltype-inherited-constraint type-entry) nil))
    ;;; strong typing has worked, so save some space - otherwise leave 
    ;;; the old structure around for debugging
    (ltype-constraint type-entry)))                   

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
             (if (every #'eq (ltype-appfeats type-record) parent-feature-order)
                (ltype-appfeats type-record)
                (let ((parent-ordered nil)
                      (appfeats (cons nil (ltype-appfeats type-record))))
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
      (setf (ltype-appfeats type-record) sorted-ordered-features)
      ;; don't process children if this type 
      ;; has been visited previously and its
      ;; feature ordering wasn't changed this time around
      ;(print (list type already-ordered-p (seen-node-p type-record)))
      (unless (and already-ordered-p (seen-node-p type-record))
         (mark-node-seen type-record)
         (unless (ltype-enumerated-p type-record)
            (loop for daughter in (ltype-daughters type-record)
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
      (loop for node in *type-names*
         do
         (let ((type-entry (get-type-entry node)))
           (unless (leaf-type-p type-entry)
             (unless 
                 (expand-default-constraint node type-entry)
               (setf ok nil)))))
      ok))

(defun expand-default-constraint (node type-entry)
   (cond ((seen-node-p type-entry) (ltype-tdfs type-entry))
      (t
         (mark-node-seen type-entry)
         (let* ((indef (ltype-constraint type-entry))
                (full-tdfs nil)
                (default-specs (ltype-default-spec type-entry))
                (default-fss
                    (loop for default-spec in default-specs
                         collect
                         (make-equivalent-persistence-defaults indef 
                               (car default-spec) (cdr default-spec) node))))               
               (setf full-tdfs 
                     (inherit-default-constraints node type-entry 
                       (construct-tdfs 
                        indef
                        default-fss)))
               (setf (ltype-tdfs type-entry) full-tdfs)
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
	 (type-entry (get-type-entry type)))
    (when type-entry
      (let ((tdfs (ltype-tdfs type-entry)))
	(when (and tdfs
		   (tdfs-tail tdfs))
	  (format t "~%Default constraint on ~A ignored in ~A"
		  (ltype-name type-entry) node)))
      (dolist (arc (dag-arcs dag))
	(collect-tails node (dag-arc-value arc))))))

(defun inherit-default-constraints (node type-entry local-tdfs)
;;;  (collect-tails node (tdfs-indef local-tdfs))
  (declare (ignore node))
  (let ((current-tail (tdfs-tail local-tdfs)))
    (loop for parent in (ltype-parents type-entry)
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


