;;; Copyright Ann Copestake 1994/1995 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK
;;; and several other places ...

(in-package :cl-user)

(defparameter *yadu-debug* nil)

;;; YADU

;;; YADU data structures

;;; TDFS has two components 
;;; indefeasible / tail

(defstruct tdfs 
           indef tail def
           (shrunk nil) ; local specification of shrunk nodes
           (not-shrunk nil)) ; non-shrunk nodes, overriding globally shrunk paths
        
;;; indef is a normal tfs
;;; the result of TDFS1 YADU TDFS2 is computed from indef and tail
;;; it may be cached in def

;;; tails are sets of tail elements


(defstruct tail-element
           path-rep spec persistence)


;;; path-rep assumes we have atomic fs tails - this simplifies
;;; life because we don't need to worry about feature structure copying
;;; because for some calculations a different data structure is more efficient
;;; In Lascarides and Copestake (1995)
;;; spec is a type, but we might want to use other notions of
;;; specificity
;;; persistence indicates whether defaults are purely lexical etc

(defstruct yadu-pv
   path value)

;;; path is a list of features
;;; value may be a type or a list of types

(defstruct yadu-pp
   paths)

;;; paths is an unordered pair of two reentrant paths

;;; When we're not using defaults, we don't want to do any 
;;; unnecessary copying

(defun make-nondefault-tdfs (indef)
  (make-tdfs :indef indef :tail nil))

;;; Copying functions

(defun copy-tdfs-elements (old-tdfs)
  ;; nothing destructive now happens to tail-elements
   (let ((indef (copy-dag (tdfs-indef old-tdfs))))
      (when indef
         (make-tdfs :indef indef
                    :tail 
                    (for element in (tdfs-tail old-tdfs)
                         collect
                         element)))))

(defun copy-tdfs-completely (old-tdfs)
  ;; nothing destructive now happens to tail-elements
   (let ((indef (copy-dag-completely (tdfs-indef old-tdfs))))
     (when indef
       (make-tdfs :indef indef
		  :tail (copy-list (tdfs-tail old-tdfs))))))

;;; Utility

(defun indef-type-of-tdfs (tdfs)
   (type-of-fs (tdfs-indef tdfs)))


(defun tdfs-at-end-of (path tdfs)
   (if path
       (make-tdfs :indef
                  (existing-dag-at-end-of-with-error (tdfs-indef tdfs) path)
                  :tail 
                  (remove-path-from-tail-elements path (tdfs-tail tdfs)))
      tdfs))

(defun remove-path-from-tail-elements (path tail)
  (for tail-element in tail
       collect
       (remove-path-from-tail path tail-element)))


(defun yadu-general-merge-tails (tail1 tail2 indef)
  (cond ((and tail1 tail2)
         (merge-tails tail1 tail2 indef))
        (tail1 
         (filter-tail tail1 indef))
        (tail2
         (filter-tail tail2 indef))
        (t nil)))


;;; YADU
;;; given two TDFS TDFS1 = I1/T1 and TDFS2 = I2/T2
;;; we calculate TDFS1 YADU TDFS2 by:
;;; I12 =  unify-wffs (I1 I2)
;;; T12 =  merge-tails (T1 T2 I12)

;;; When we want to calculate what wins
;;; D1 = generalise-set (carp-unify (...(carp-unify I1 spec1) ...)
;;;                                    specn)
;;; where spec1 to specn is the partition of T1
;;; generalise-set is defined in terms of generalise
;;; carp-unify is defined below

; (defparameter *unif-count* 0)

(defun yadu (tdfs1 tdfs2)
   (yadu1 (tdfs-indef tdfs1) (tdfs-indef tdfs2) (tdfs-tail tdfs1)
      (tdfs-tail tdfs2)))

(defun yadu1 (indef1 indef2 tail1 tail2)
   (let ((indef12 (unify-wffs indef1 indef2)))
      (if indef12
          (make-tdfs :indef indef12
                     :tail (yadu-general-merge-tails tail1 tail2 indef12)))))

(defun yadu-winner (tdfs)
  (let ((indef (tdfs-indef tdfs))
         (tail (tdfs-tail tdfs)))
    (if tail
        (or (tdfs-def tdfs) ; result cached
            (let* ((partition (partition-tail tail nil))
                   (def-fs
                     (generalise-set 
                      (yadu-unify (list indef) partition))))
              (setf (tdfs-def tdfs) def-fs)
              def-fs))
      indef)))


(defun generalise-set (fs-set)
;;;  (format t "~% test generalise")
;;;  (let ((num 0))
;;;  (for fs in fs-set
;;;       do 
;;;       (incf num)
;;;       (display-fs fs (format nil "~A" num))))
   (reduce #'generalise-dags fs-set))



;;; Operations on tails

;;; Specificity ordering of elements

(defun more-specific-p (el1 el2)
   (let ((spec1 (tail-element-spec el1))
         (spec2 (tail-element-spec el2)))
      (if (eq spec1 'lex) 
         (not (eq spec2 'lex))
         (if (eq spec2 'lex)
            nil
            (subtype-p spec1 spec2)))))


;;; Partitioning
;;;
;;; There is no point in attempting to maintain a partitioned tail
;;; because elements which are currently in the same partition can be
;;; split up when other elements are added to the tail
;;;
;;; defn of a specificity partition
;;; T = mu_1 union mu_2 union mu_m
;;; 
;;; mu_1 is all the elements in the tail st there are no more-specific-p 
;;; elements
;;;
;;; mu_i is all the elements in the tail st there is a more-specific-p element
;;; in mu_(i-1) and there is no more-specific-p element in the tail
;;; excluding mu_1 to mu_(i-1)

;;; algorithm
;;; remove all the most specific elements from the tail and put them in mu1
;;; take remainder of tail, remove all most specific elements and put them in 
;;; mu2
;;; repeat until no more tail


(defun partition-tail (unpartitioned partitioned)
   (if unpartitioned
      (let ((mu-next nil)
         (next-unpartitioned nil))
      (for tail-element in unpartitioned
         do
         (if 
            (every #'(lambda (rem-element) 
                        (not (more-specific-p rem-element tail-element)))
                     unpartitioned)
             (push (tail-element-path-rep tail-element) mu-next)
             (push tail-element next-unpartitioned)))
       (partition-tail next-unpartitioned (cons mu-next partitioned)))
    (nreverse partitioned)))



;;; Merging

;;; T1 merge-tails T2 = T1 union T2 \ Bot12
;;; where Bot12 are all the tail elements with values incompatible 
;;; with I1 unify I2 or no more specific than it

;;; Algorithm -
;;; union T1 and T2 - using a FS equality test - dag-equal-p 
;;; remove incompatible elements - not unify-dags 
;;; remove subsumed elements - dag-subsumes-p tail-fs indef-fs

;;; dag-equal-p and dag-subsumes-p are in gen.lsp
;;; unify-dags is in dag.lsp - this needs to be made more efficient
;;; by doing the pre-unification check
;;; and think about constraints - should unify-wffs be used instead?

(defun merge-tails (tail1 tail2 indef-fs)
   (remove-if 
      #'(lambda (tail-element)
          (let ((tail-atfs (tail-element-path-rep tail-element)))
             (or (atomic-dag-subsumes-p tail-atfs indef-fs)
                 (not
                    (atomic-unifiable-dags-p tail-atfs indef-fs)))))
      (union-tails tail1 tail2)))

(defun filter-tail (tail indef-fs)
  ;;; for the case where only one TDFS has a tail, we still want to remove
  ;;; stuff that's incompatible with the indefeasible structure
  (for tail-element in tail
       filter
       (let ((tail-atfs (tail-element-path-rep tail-element)))
         (if (atomic-unifiable-dags-p tail-atfs indef-fs)
             tail-element))))

(defun atomic-dag-subsumes-p (atomic-fs fs)
  (if (yadu-pv-p atomic-fs)
      (let* ((path (yadu-pv-path atomic-fs))
             (value (yadu-pv-value atomic-fs))
             (fs-at-end-of (existing-dag-at-end-of fs 
                                      (path-typed-feature-list path))))
        (and fs-at-end-of
             (subsume-types value
                            (type-of-fs fs-at-end-of))))
    (let* ((paths (yadu-pp-paths atomic-fs))
           (fs1 (existing-dag-at-end-of fs
                                        (path-typed-feature-list
                                         (car paths)))))
      (and fs1
           (for path in (cdr paths)
                all-satisfy
                (eq fs1
                    (existing-dag-at-end-of fs 
                                            (path-typed-feature-list path))))))))

(defun atomic-unifiable-dags-p (atomic-fs indef-fs)
  (with-unification-context (nil)
    (if (yadu-pv-p atomic-fs)
        (unify-paths (yadu-pv-path atomic-fs)
                     indef-fs
                     (make-u-value 
                      :types (yadu-pv-value atomic-fs))
                     nil)
      (let* ((paths (yadu-pp-paths atomic-fs))
             (initial-path (car paths))
             (ok nil))
        (dolist (path2 (cdr paths))
          (setf ok
            (unify-paths initial-path       
                         indef-fs
                         path2
                         indef-fs))
          (unless ok
            (return)))
        ok))))





(defun union-tails (tail1 tail2)
   (when (> (length tail2) (length tail1))
      (rotatef tail1 tail2))  ; swaps the values
   (let ((new-elements
            (for tail-element in tail2
               filter
               (let 
                  ((existing-element-list
                        (member tail-element tail1 
                           :test #'equal-tail-elements)))
                  (if existing-element-list
                     (let ((pers2 (tail-element-persistence tail-element))
                           (pers1 (tail-element-persistence 
                                 (car existing-element-list))))
                        (unless (equal pers1 pers2)
                           (setf (tail-element-persistence 
                                 (car existing-element-list))
                              (merge-persistence pers1 pers2)))
                        nil)
                     tail-element)))))
      (append new-elements tail1)))  

(defun merge-persistence (p1 p2)
   (sort (union p1 p2) 
      #'string<))
                  

(defun equal-tail-elements (el1 el2)
   (and (equalp (tail-element-spec el1) (tail-element-spec el2))
      (let ((pr1 (tail-element-path-rep el1))
            (pr2 (tail-element-path-rep el2)))
         (or (and (yadu-pp-p pr1) (yadu-pp-p pr2)
               (or (equalp (yadu-pp-paths pr1) (yadu-pp-paths pr2))
                  (equalp (yadu-pp-paths pr1) 
                     (reverse (yadu-pp-paths pr2)))))
            (and (yadu-pv-p pr1) (yadu-pv-p pr2)
               (equalp (yadu-pv-path pr1) (yadu-pv-path pr2))
               (equalp (yadu-pv-value pr1) (yadu-pv-value pr2)))))))


;;; Adding and removing features from tails for rule application

(defun add-path-to-tail (path tail-element)
  (let* ((p-rep (tail-element-path-rep tail-element))
         (new-element (make-tail-element 
                       :spec (tail-element-spec tail-element)
                       :persistence 
                       (tail-element-persistence tail-element)
                       :path-rep
                       (if (yadu-pp-p p-rep)
                           (make-yadu-pp :paths
                                         (for tail-path in (yadu-pp-paths p-rep)
                                              collect
                                              (path-append path tail-path)))
                         (if (yadu-pv-p p-rep)
                             (make-yadu-pv :path 
                                           (path-append path (yadu-pv-path p-rep))
                                           :value (yadu-pv-value p-rep))
                           (unless (null p-rep)
                             (error "Incorrect value in path rep")))))))
    new-element))

(defun remove-path-from-tail (path tail-element)
    (let* ((p-rep (tail-element-path-rep tail-element))
         (new-element (make-tail-element 
                       :spec (tail-element-spec tail-element)
                       :persistence 
                       (tail-element-persistence tail-element)
                       :path-rep
                       (if (yadu-pp-p p-rep)
                           (make-yadu-pp :paths
                                         (for tail-path in (yadu-pp-paths p-rep)
                                              collect
                                              (path-delete path tail-path)))
                         (if (yadu-pv-p p-rep)
                             (make-yadu-pv :path 
                                           (path-delete path (yadu-pv-path p-rep))
                                           :value (yadu-pv-value p-rep))
                           (unless (null p-rep)
                             (error "Incorrect value in path rep")))))))
      new-element))
  

;;; YADU 
  


(defun yadu-unify (fixed-fss partition &optional second-call-p)
  (when *yadu-debug* 
    (format t "~%No of fixed fs ~A next tail length ~A second-call-p ~A"
    (length fixed-fss) (length (car partition)) second-call-p))
   (if partition
      (yadu-unify
         (for fixed-fs in fixed-fss
            append
            (carp-unify fixed-fs (car partition) second-call-p))
         (cdr partition) t)
      fixed-fss))


;;; We need to find the maximal results
;;; I'm attempting to make this efficient for the case where most of the
;;; defaults are mutually compatible
;;;
;;; Algorithm
;;; for n members of def-fs-set
;;; unify in all n
;;; success -> return
;;; failure ->
;;;        generate all combinations of length n-1
;;;        unify in all combinations
;;;  A      all succeed -> return them
;;;         otherwise 1) keep successful combinations
;;;                   2) generate all combinations of length n-2 that are
;;;                     not subsets of a successful combination
;;;                   3) unify in all combinations
;;;                   4) recurse from A

;;; tweak - keep a record of failures and don't try any combination which is a
;;; superset of the failures

;;; check for individual consistency of defs with the fixed-fs (since this
;;; isn't actually the indefeasible fs but only the less defeasible
;;; information there may be stuff which is incompatible with it in the
;;; tails).

(defparameter *success* nil)
(defparameter *temp-bit-vector* nil)

(defmacro successes (state)
  `(first ,state))

(defmacro failures (state)
  `(second ,state))

(defmacro defaults (state)
  `(third state))

(defun make-state (defaults)
  (list nil nil 
	;; We represent a set of constraints as a bit vector, so we need to
	;; assign each constraint a bit position
	(loop for def in defaults
	    and pos upfrom 0
	    collect (cons def pos))))

(defun carp-unify (fixed-fs def-fs-set check-ind-defs-p)
  ;; def-fs-set is using path representation 
  ;; check-ind-defs-p is true if the fixed-fs might contain some defeasible
  ;; material
  (when check-ind-defs-p
    (setf def-fs-set 
      (for fs in def-fs-set
           filter
           (when (atomic-unifiable-dags-p fs fixed-fs)
	     fs))))
  ;; we improve matters by chucking out structures which were incompatible
  ;; with the fixed-fs when check-ind-defs-p is true - if it isn't, we're
  ;; doing the first round and the tails are guaranteed to be compatible
  ;; because the tail elements have already been filtered
  (let* ((*success* nil)
	 (*temp-bit-vector* (make-array (list (length def-fs-set))
					:element-type 'bit))
	 (state (make-state def-fs-set))
	 (combo (make-array (list (length def-fs-set))
			    :element-type 'bit 
			    :initial-element 1))
	 (all-ok (unify-in fixed-fs combo state)))
    (if all-ok
	(list all-ok)
      (or
       (search-combinations fixed-fs -1 combo state)
       (list fixed-fs)))))

;; Quick subset test for sets encoded as bit vectors.  The Allegro version
;; calls the runtime system directly, avoiding a little bit of type-checking
;; overhead.

#-allegro
(defmacro fast-subsetp (l1 l2)
  `(progn
     (bit-and ,l1 ,l2 *temp-bit-vector*)  
     (equal ,l1 *temp-bit-vector*)))

#+allegro
(defmacro fast-subsetp (l1 l2)
  `(progn
     (excl::.primcall 'sys::rs-bitop
		      ,l1
		      ,l2
		      *temp-bit-vector*
		      boole-and)
     (excl::.primcall 'sys::rs-bit-equal 
		      ,l1 
		      *temp-bit-vector*)))

(defun search-combinations (fixed-fs start combo state)
  (unify-combinations fixed-fs start combo state)
  *success*)
  
(defun unify-combinations (fixed-fs start combo state) 
  (unless (= start (1- (length combo)))
    (let ((v (copy-seq combo)))
      (loop for pos from (1- (length combo)) downto (1+ start)
	  do
	    (setf (sbit v pos) 0))
      ;; Every subset of combo that we generate from this point will be a
      ;; superset of v, so if we know v will fail there's no point in
      ;; continuing.
      (unless (unify-in fixed-fs v state)
	(loop for pos from (1- (length combo)) downto (1+ start)
	    do
	      (setf (sbit combo pos) 0)
	      ;; Every superset of combo has already been considered.
	      ;; If one succeeded, then combo can't be maximal.
	      (unless (dolist (old-comb (successes state))
			(when (fast-subsetp combo old-comb)
			  (return t)))
		(let ((unif-result (unify-in fixed-fs combo state)))
		  (cond (unif-result
			 (push unif-result *success*)
			 (push combo (successes state)))
			(t (unify-combinations fixed-fs pos combo state))))
		(setf (sbit combo pos) 1)))))))

;; Try unifying a set of constraints (represented as a bit vector) into a
;; feature structure.  If successful, return the resulting feature structure.
;; Otherwise, add the set of conflicting constraints to *failure-list* and
;; return the fatal constraint.

(defun unify-in (indef-fs combo state)
  ;; *** FIX - need to check for well-formedness ***
  ;; We might have already verified that a subset of combo is inconsistent.
  ;; If so, we know combo can't be consistent.
  (unless (dolist (old-comb (failures state))
	    (when (fast-subsetp old-comb combo)
	      (return t)))
    (let ((added (make-array (length (defaults state))
			     :element-type 'bit 
			     :initial-element 0)))
      (with-unification-context (nil)
	(dolist (def (defaults state))
	  (let ((res (unless (zerop (sbit combo (cdr def)))
		       ;; Skip if not in set
		       (setf (sbit added (cdr def)) 1)
		       (if (yadu-pv-p (car def))
			   (unify-paths (yadu-pv-path (car def))
					indef-fs
					(make-u-value
					 :types (yadu-pv-value (car def)))
					nil)
			 (let* ((paths (yadu-pp-paths (car def)))
				(initial-path (car paths)))
			   (dolist (path2 (cdr paths) t)
			     (unless (unify-paths initial-path       
						  indef-fs
						  path2
						  indef-fs)
			       (return))))))))
	    (cond ((and res (not (cyclic-dag-p indef-fs)))
		   ;; So far so good, now try the next constraint
		   (setq indef-fs (fix-dag indef-fs)))
		  (t 
		   ;; Failed: add set so far to *failure-list*
		   (push added (failures state))
		   (setf (defaults state) 
		     (cons def (delete def (defaults state) :test #'eq)))
		   (return-from unify-in nil)))))
	;; Unifications all successful, now try to copy
	(copy-dag indef-fs)))))

;;; incorporating all the defaults of a given persistence
;;; non-destructively
;;; given a tdfs 
;;; 1) the tail is split into things to be incorporated and 
;;;    things that are to remain default
;;; 2) the default fs is calculated for the things that are to be
;;;    incorporated
;;; 3) this is unified into the indefeasible structure which is
;;;    made well-formed
;;; 4) a new def fs is calculated from the other tail elements and the new
;;;    indefeasible structure
;;;
;;; If there is any interaction between the persistent and
;;; non-persistent defaults this may give non-intuitive results, because
;;; non-persistent defaults that were being masked by persistent ones
;;; could show up in the indefeasible structure.

(defun make-indefeasible (tdfs persistence)
  (if (tdfs-tail tdfs)
      (multiple-value-bind (persistent non-persistent)
	  (split-tail persistence (tdfs-tail tdfs))
	(let* ((indef (tdfs-indef tdfs))
               (partition (partition-tail non-persistent nil))
               (non-persistent-def 
                (generalise-set (yadu-unify (list indef) partition)))
	       (result (unify-dags indef non-persistent-def)))
	  (unless result
	    (error "Default is inconsistent with indef"))
	  (let ((new-indef (create-wffs result)))
	    (unless new-indef
	      (cerror "The result of incorporating the persistent defaults
                     cannot be made well-formed" "Ignore defaults")
	      (setf new-indef indef))
	    (make-tdfs :indef new-indef
		       :tail persistent))))
    tdfs))


(defun split-tail (persistence tail)
   (if (eql persistence 'all) 
      (values nil tail)
      (let ((persistent nil)
            (non-persistent nil))
         (for tail-element in tail
            do
            (if (intersection (tail-element-persistence tail-element) 
                  persistence)
               (progn (push tail-element non-persistent)
                  (when (set-difference 
                        (tail-element-persistence tail-element)
                        persistence)
                     (push tail-element persistent)))
               (push tail-element persistent))) 
         (values persistent non-persistent))))


;;; Initialising structures with tails 

(defun construct-tdfs (indef default-list &optional lexp)
   ;;; basically this just involves constructing the tails
   ;;; default list is a list of cons of
   ;;; persistence atoms and tfs
   ;;; The default fs are mutually consistent with eachother and with the 
   ;;; indefeasible structure
   ;;; If lexp is true, these defaults arise from the lexicon and therefore
   ;;; should be given maximal specificity
   (let ((tails
            (if default-list
               (let ((tail-sets 
                        (for def in default-list
                           collect
                           (extract-yadu-tails (car def) (cdr def) indef
                              lexp))))
                  (if (cdr tail-sets)
                     (reduce #'(lambda (x y)
                           (union-tails x y))
                        tail-sets)
                     (car tail-sets)))
               nil)))
      (make-tdfs :indef indef 
         :tail tails)))


;;; for example, if indef is 
;;; [ t1 ]
;;; and def is
;;; [ t1
;;;   F = [ t2
;;;         G = t3 
;;;         H = <1> t4 ]
;;;   I = <1> ]
;;;
;;; We will get in the tail:
;;; #S(tail-element :fs [F = t2] :spec t1)
;;; #S(tail-element :fs [F G = t3] :spec t1)
;;; #S(tail-element :fs [F H = t4] :spec t1)
;;; #S(tail-element :fs [I = t4] :spec t1)
;;; #S(tail-element :fs [F H = I] :spec t)
;;; but #S(tail-element :fs [t1] :spec t1)
;;; will be omitted because it's not more specific than I

(defvar *atfs* nil)

(defun extract-yadu-tails (persistence def-dag indef &optional lexp)
   (invalidate-visit-marks)
   (let ((specificity (if lexp 'lex (type-of-fs def-dag))))
      (setf *atfs* nil)
      (yadu-convert-dag-to-atfs def-dag nil specificity indef persistence)
      *atfs*))



(defun yadu-convert-dag-to-atfs (dag-instance path-so-far spec indef-dag
      persistence)
   (let* ((real-dag (follow-pointers dag-instance))
         (real-indef (if indef-dag (follow-pointers indef-dag)))
         (current-path 
            (create-path-from-feature-list (reverse path-so-far))))      
      (for stored-path in (dag-visit real-dag)
         do
         (unless (and real-indef 
               (member stored-path (dag-visit real-indef)))
            (push 
             (make-tail-element :spec spec 
                  :path-rep 
                  (make-yadu-pp :paths 
                     (list current-path stored-path))
                  :persistence (list persistence))
               *atfs*)))
      (push current-path (dag-visit real-dag))
      (when real-indef
         (push current-path (dag-visit real-indef)))
      (if (is-atomic real-dag) 
         (let ((val (type-of-fs real-dag)))
            (unless 
               (and real-indef (is-atomic real-indef)
                  (equalp (type-of-fs real-indef) val))
               (push 
                (make-tail-element :spec spec 
                     :path-rep 
                     (make-yadu-pv :path current-path :value val)
                     :persistence (list persistence))
                  *atfs*)))
         (progn
            (let ((type (dag-type real-dag)))
               (unless 
                  (and real-indef 
                     (eql (dag-type real-indef) type))
                  (push 
                     (make-tail-element :spec spec 
                        :path-rep 
                        (make-yadu-pv :path current-path :value (list type))
                        :persistence (list persistence))
                        *atfs*)))
            (for label in (top-level-features-of real-dag)
               do
               (yadu-convert-dag-to-atfs (get-dag-value real-dag label)
                  (cons label path-so-far) spec
                  (if indef-dag (get-dag-value indef-dag label))
                  persistence))))))

       



   





