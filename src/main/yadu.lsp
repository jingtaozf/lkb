;;; Copyright Ann Copestake 1994/1995 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK
;;; and several other places ...

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
           fs path-rep spec persistence)
        
;;; In Lascarides and Copestake (1995), the fs is atomic, but this might
;;; not be true when we impose well-formedness conditions
;;; also - spec is a type there, but we might want to use other notions of
;;; specificity
;;; persistence indicates whether defaults are purely lexical etc

;;; path-rep is an additional slot for when we do have atomic fs tails
;;; because for some calculations a different data structure is more efficient

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

(defun copy-tdfs-elements (old-tdfs) ; *** JAC new function
   (let ((indef (copy-dag (tdfs-indef old-tdfs))))
      (when indef
         (make-tdfs :indef indef
              :tail 
              (for element in (tdfs-tail old-tdfs)
                 collect
                 (copy-tail-element-completely ; *** ???
                    element))))))

(defun copy-tdfs-completely (old-tdfs)
   (make-tdfs :indef (copy-dag-completely (tdfs-indef old-tdfs))
              :tail 
              (for element in (tdfs-tail old-tdfs)
                 collect
                 (copy-tail-element-completely
                    element))))
        
(defun copy-tdfs-really-completely (old-tdfs)
   ;;; needed for lex rule application
   (make-tdfs :indef (copy-dag-completely (tdfs-indef old-tdfs))
              :tail 
              (for element in (tdfs-tail old-tdfs)
                 collect
                 (copy-tail-element-completely
                    element))))
        
(defun copy-tail-element-completely (t-element)
   ;;; not actually completely because paths are shared with the old
   ;;; structure, but I think this is OK
   (make-tail-element 
      :fs 
      (copy-dag-completely 
         (tail-element-fs t-element))
      :path-rep 
      (let ((p-rep (tail-element-path-rep t-element)))
         (if (yadu-pp-p p-rep)
            (make-yadu-pp :paths
               (copy-list (yadu-pp-paths p-rep)))
            (if (yadu-pv-p p-rep)
               (make-yadu-pv :path (yadu-pv-path p-rep)
                  :value (copy-list (yadu-pv-value p-rep)))
               (unless (null p-rep)
                  (error "Incorrect value in path rep")))))
      :persistence (copy-list (tail-element-persistence t-element))
      :spec (tail-element-spec t-element)
      ; was copy-list, but this gives error with atoms in MCL
      ))

;;; Utility

(defun indef-type-of-tdfs (tdfs)
   (type-of-fs (tdfs-indef tdfs)))


(defun tdfs-at-end-of (feat tdfs)
   (if feat
      (let ((path (list feat)))
         (make-tdfs :indef
                    (existing-dag-at-end-of-with-error (tdfs-indef tdfs) path)
            :tail 
            (remove-feat-from-tail-elements feat (tdfs-tail tdfs))))
      tdfs))

(defun remove-feat-from-tail-elements (feat tail)
   (when tail
      (let ((path (create-path-from-feature-list (list feat))))
         (for tail-element in tail
            filter
            (remove-feat-from-tail feat path tail-element)))))


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

(defun yadu-features (tdfs1-feat tdfs1 tdfs2-feat tdfs2)
   (yadu1
      (if (listp tdfs1-feat)
         (existing-dag-at-end-of-with-error (tdfs-indef tdfs1) tdfs1-feat)
         (get-dag-value (tdfs-indef tdfs1) tdfs1-feat))
      (if (listp tdfs2-feat)
         (existing-dag-at-end-of-with-error (tdfs-indef tdfs2) tdfs2-feat)
         (get-dag-value (tdfs-indef tdfs2) tdfs2-feat))
      (if tdfs1-feat
         (remove-feat-from-tail-elements tdfs1-feat (tdfs-tail tdfs1))
         (tdfs-tail tdfs1))
      (if tdfs2-feat
         (remove-feat-from-tail-elements tdfs2-feat (tdfs-tail tdfs2))
         (tdfs-tail tdfs2))))


(defun yadu1 (indef1 indef2 tail1 tail2)
   (let ((indef12 (unify-wffs indef1 indef2)))
      (if indef12
          (if (and tail1 tail2)
              (let* ((tail12 (merge-tails tail1 tail2
                                          indef12))
                     (result (make-tdfs :indef indef12
                                        :tail tail12)))
                  (break "inside yadu defaults")
                  result)
              (make-tdfs :indef indef12
                         :tail (or tail1 tail2))))))

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
;;;    (format t "~% test generalise")
;;;    (for fs in fs-set
;;;       do 
;;;       (display-dag1 fs 'simple t))
   (reduce #'generalise fs-set))



;;; Operations on tails

;;; Specificity ordering of elements

(defun more-specific-p (el1 el2)
   (let ((spec1 (tail-element-spec el1))
         (spec2 (tail-element-spec el2)))
      (if (eql spec1 'lex) 
         (not (eql spec2 'lex))
         (if (eql spec2 'lex)
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
             (push (tail-element-fs tail-element) mu-next)
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
   ;;; change to delete-if eventually?
   (remove-if 
      #'(lambda (tail-element)
           (let ((tail-fs (tail-element-fs tail-element)))
              (or (dag-subsumes-p tail-fs indef-fs)
                 (not
                    (unifiable-dags-p tail-fs indef-fs)))))
        (union-tails tail1 tail2)))
  
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
      (append new-elements tail1)))  ; make nconc?

(defun merge-persistence (p1 p2)
   (sort (union (copy-list p1) (copy-list p2)) 
      #'string<))
                  

(defun equal-tail-elements (el1 el2)
   (and (equalp (tail-element-spec el1) (tail-element-spec el2))
      ;;;  stuff below SHOULD be more efficient than
      ;;;      (dag-equal-p (tail-element-fs el1) (tail-element-fs el2))))
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

(defun add-feat-to-tail (feature path tail-element)
   (let ((new-dag (create-dag)))
      (unify-paths path new-dag (make-path) (tail-element-fs tail-element))
      (setf (tail-element-fs tail-element) new-dag)
      (let ((p-rep (tail-element-path-rep tail-element)))
         (if (yadu-pp-p p-rep)
            (for tail-path in (yadu-pp-paths p-rep)
               do
               (push-feature feature tail-path))
            (if (yadu-pv-p p-rep)
               (push-feature feature (yadu-pv-path p-rep))
               (unless (null p-rep)
                  (error "Incorrect value in path rep")))))
      tail-element))

(defun remove-feat-from-tail (feature path tail-element)
  (declare (ignore feature))
   (let ((new-dag 
            (existing-dag-at-end-of 
               (tail-element-fs tail-element) 
               (path-typed-feature-list path))))
      (when new-dag
         (setf (tail-element-fs tail-element) new-dag)
         (let ((p-rep (tail-element-path-rep tail-element)))
            (if (yadu-pp-p p-rep)
               (for tail-path in (yadu-pp-paths p-rep)
                  do
                  (pop-feature tail-path))
               (if (yadu-pv-p p-rep)
                  (pop-feature  (yadu-pv-path p-rep))
                  (unless (null p-rep)
                     (error "Incorrect value in path rep"))))
            tail-element))))
  

;;; YADU 
  


(defun yadu-unify (fixed-fss partition &optional second-call-p)
   (if partition
      (yadu-unify
         (for fixed-fs in fixed-fss
            append
            (carp-unify fixed-fs (car partition) second-call-p))
         (cdr partition) t)
      fixed-fss))


;;; We need to find the maximal results
;;; I'm attempting to make this efficient for the case where
;;; most of the defaults are mutually compatible
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

;;; tweak - keep a record of failures and don't try any combination 
;;; which is a superset of the failures

;;; check for individual consistency of defs with the
;;; fixed-fs (since this isn't actually the indefeasible fs but only the
;;; less defeasible information there may be stuff which is incompatible with 
;;; it in the tails).  

(defparameter *failure-list* nil)



(defun carp-unify (fixed-fs def-fs-set check-ind-defs-p)
   ; check-ind-defs-p is true if the fixed-fs might contain
   ; some defeasible material
   (setf *failure-list* nil)
   (let* ((possible-defs 
            (if check-ind-defs-p
               (remove-if-not 
                  #'(lambda (def-fs)
                  (unify-dags (copy-dag-completely fixed-fs)
                       (copy-dag-completely def-fs)))
                 def-fs-set)
              def-fs-set))
        (all-ok (unify-in (copy-dag-completely fixed-fs) possible-defs nil)))
      (if all-ok (list all-ok)
         (or
            (unify-combinations fixed-fs possible-defs nil nil 
               (- (length possible-defs) 1))
            (list fixed-fs)))))


(defun unify-combinations (fixed-fs def-fs-list existing-combs successful comb-length)
   (if (eql comb-length 0)
      successful
      (let ((success t)
            (combinations (generate-combinations def-fs-list comb-length)))
         (for combination in combinations
            do
            (cond ((some #'(lambda (old-comb) 
                        (subsetp combination old-comb))
                     existing-combs) nil) ;;; successful but non-maximal
               ((some #'(lambda (old-comb) 
                        (subsetp old-comb combination))
                     *failure-list*)
                  (setf success nil)) ;;; bound to fail
               (t
                  (let* ((dag-copy (copy-dag-completely fixed-fs))
                        (unif-result 
                           (unify-in dag-copy combination nil)))
                     (if unif-result
                        (progn
                           (push unif-result successful)
                           (push combination existing-combs))
                        (setf success nil))))))
         (if success
            successful
            (unify-combinations fixed-fs def-fs-list existing-combs successful 
               (- comb-length 1))))))


(defun generate-combinations (elements clength)
   (cond ((or (> clength (length elements)) (< clength 1))
         (error "Incorrect length input"))
      ((eql (length elements) clength) (list elements))
      ((eql clength 1) (for element in elements collect (list element)))
      (t 
         (append (generate-combinations (cdr elements) clength)
            (for comb in (generate-combinations (cdr elements) (- clength 1))
               collect
               (cons (car elements) comb))))))
   


(defun unify-in (indef-fs def-fs-list added)
   (if def-fs-list
      (let ((def-copy (copy-dag-completely (car def-fs-list))))
;         (incf *unif-count*)
         (if (unify-dags indef-fs def-copy)
            ; order is significant since unify-dags is destructive
            ; modifying the first arg to give the result
            ;
            ; check-dag-for-cycles is needed to block incorrect LAST values
            ; being inherited
            ;           
            ; should this be unify-wffs?
            ; or maybe something that does limited 
            ; wellformedness checking?
            (unify-in indef-fs (cdr def-fs-list)
               (cons (car def-fs-list) added))
            (progn
               (push (cons (car def-fs-list) added) *failure-list*) 
               nil)))
      indef-fs))



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
      (multiple-value-bind
         (persistent non-persistent)
         (split-tail persistence (tdfs-tail tdfs))
         (let* ((indef (copy-dag-completely (tdfs-indef tdfs)))
               (partition (partition-tail non-persistent nil))
               (non-persistent-def 
                  (generalise-set (yadu-unify (list indef) partition))))
            (unless (unify-dags indef non-persistent-def)
               (error "Default is inconsistent with indef"))
            (let ((new-indef (create-wffs indef)))
               (unless new-indef
                  (cerror "The result of incorporating the persistent defaults
                     cannot be made well-formed" "Ignore defaults")
                     (setf new-indef (copy-dag-completely (tdfs-indef tdfs))))
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
                     (push (copy-tail-element-completely
                           tail-element) persistent)))
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
               (make-tail-element :spec spec :fs
                  (make-path-path-fs current-path stored-path) 
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
                  (make-tail-element :spec spec :fs
                     (make-path-value-fs current-path val) 
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
                     (make-tail-element :spec spec :fs
                        (make-path-value-fs current-path (list type))
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

(defun make-path-value-fs (path value)
   (let ((new-dag (create-typed-dag *toptype*)))
      (unify-paths path new-dag
         (make-u-value :types value) new-dag)
      new-dag))

(defun make-path-path-fs (path1 path2)
   (let ((new-dag (create-typed-dag *toptype*)))
      (unify-paths path1 new-dag path2 new-dag)
      new-dag))
       



   





