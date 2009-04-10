;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; TO DO (most urgent first)
;;; FIX1 - Prune so unmatched determiners aren't possible
;;; FIX2 - add weightings and test on QA
;;; FIX3 - expand notion of arg compatibility

#| Robust comparison of RMRSs

There are two somewhat different situations to consider:
1) the RMRSs come from different sentences and we're trying to see
how close they are e.g. question answering
2) the RMRSs come from the same text

The same code is used for both situations.

The idea is to record the RMRS results with comparison records,
using comp-status slots.  This enables display and scoring of 
similarities.

The backbone of the comparison is the eps, especially the
ones from real words, especially the ones from open class words.

It should be possible to reuse this as MRS code.  In fact it could replace
the MRS equalities code, provided there are jumps out when structures are
found non-equal, but leave for now.

AAC Jan 1 2004: Originally I intended to do this incrementally, but
the best way of doing this now looks like a multi-pass strategy.

The general idea is to leave the more complicated binding until the basic
relation match has been done and for the indeterminacy in comparison
to be localised to matches for individual relations.  The proper bindings
will then potentially filter the matches.

For instance, consider matching:

A: the big dog sleeps and the big cat snores

and

B: the big dog snores and the big cat sleeps

We have on a first pass:

the1 . the1, the1 . the6, the6 . the1, the6 . the6
big2 . big2, big2 . big7, big7 . big2, big7 . big7
dog3 . dog3
sleep4 . sleep9
and5 . and5
cat8 . cat8
snore9 . snore4

The labels are paired and the characteristic variables.

Obviously these relation pairings are not all mutually compatible in that
one thing can only be paired once, but because repeated relations are less
common that unrepeated ones, it's more economical to store things
this way and work out the global compatibilities later

In the second pass, the variable bindings are used to rule out members
of sets, and to record cases where the arguments differ.  
In the case above, we want to rule out the `wrong' pairings
for `the' (and possibly `big') and reduce the overall score (somehow) 
because snore and sleep don't have matching ARGs.  

|#


(defstruct (comp-rmrs (:include rmrs)))

;;; Originally I intended this to have extra slots compared to
;;; a normal rmrs but this doesn't seem required now.  However
;;; it differes from a normal rmrs because
;;; a comp-rmrs rels list is a list of comparison-set structures. 

;;; For the same source case, this list is ordered via characterisation -
;;; for the non-same-source case, there will just be one list for now,
;;; but the code is general so that if we produce some idea of typing
;;; mutually distinct pred classes we can use that to order the 
;;; comparison sets.

(defstruct comparison-set
  cfrom cto real-preds constant-preds gram-preds)
;;; real-preds and constant-preds are sorted, gram-preds aren't necessarily

;;; Matching involves taking advantage of sortal order
;;; wherever possible.  This means making assumptions
;;; about compatibility between different classes of predicates.
;;; This will probably have to be parameterisable depending on the
;;; grammar and the application, since we might have `external'
;;; predicate hierarchies (or no hierarchies at all)


;;; *********************************************
;;; Storing results 
;;;
;;; Structure definitions in basermrs.lisp since used outside this file
;;;
;;;
#|

(defstruct rmrs-comparison-record
  matched-top matched-rels matched-args matched-ings matched-hcons bindings
  qeq-pairs)

(defstruct match-rel-record
  rel1
  rel2
  pred-comp-status
  var-comp-status
  arg-comp-status
  label-pair ;; rel labels
  anchor-pair ;; rel anchors (for version without ING) 
  cvar-pair ;; characteristic vars (non-handle)
  hvar-pair ;; non arg vars (handle)
  )

;;; comp-status records the class of compatibility
;;; between the relation
;;; :equal
;;; :sub1 - rel1 is more specific tham rel2
;;; :sub2 - rel2 is more specific tham rel1
;;; :comp - rel1 and rel2 are compatible but not equal
;;;         and not in a subsumes relationship

(defstruct match-top-record
  label1
  label2)

(defstruct match-arg-record
  arg1
  arg2
  comp-status
  arg-type) ; want to record if this is a carg

(defstruct match-ing-record
  ing1
  ing2)

(defstruct match-hcons-record
  hcons1
  hcons2)

|#

;;; *************************************************************  
;;;
;;; Code
;;;

;;; Main entry point - same-source-p is t if we want
;;; to use the character position information 
;;; FIX2? - possibly need to put in a quick and dirty check for the
;;; weighted match case for e.g. QA, since otherwise this
;;; might be too expensive computationally
;;; Addition, sept 2008 - same-source-p is :approx
;;; if the original source is the same, but character positions are
;;; not present.  In this case an approximation is used.

(defun compare-rmrs (rmrs1 rmrs2 same-source-p)
  ;;; returns a list of comparison records
  (unless (and (rmrs-p rmrs1) (rmrs-p rmrs2))
    (error "Arguments to compare-rmrs are not valid RMRSs"))
  (if (and (rmrs-liszt rmrs1) (rmrs-liszt rmrs2))
      (let* ((new-rmrs1 (sort-rmrs (convert-to-comparison-rmrs rmrs1)
				   same-source-p))
	     (new-rmrs2 (sort-rmrs (convert-to-comparison-rmrs rmrs2)
				   same-source-p))
	     (first-pass
	      (compare-rmrs-liszts (rmrs-liszt new-rmrs1) 
				   (rmrs-liszt new-rmrs2)
				   (initial-comparison-record)
				   same-source-p))
	     (second-pass (prune-comparison-record first-pass))
	     (expanded
	      (expand-comparison-records second-pass new-rmrs1 new-rmrs2)))
	(dolist (comp expanded)
	  (add-top-label-compare comp new-rmrs1 new-rmrs2) 
	  (add-argument-compare comp new-rmrs1 new-rmrs2)
	  (add-ing-compare comp new-rmrs1 new-rmrs2)
	  (add-hcons-compare comp new-rmrs1 new-rmrs2))
	expanded)))

(defun initial-comparison-record nil
  (make-rmrs-comparison-record 
   :matched-rels nil))

#|
***********************************************
Sorting
**********************************************

If the source is the same, this is assumed to have some reflection
on linear order of rels and the sorting reflects that linear order
(via characterization).  As a secondary sorting measure,
the real pred and constant value rels are sorted
as for the non-same source case.  There may be multiple
rels with the same character positions.

In the case where the source is not the same, and we're allowing
for subsumption, the rels are sorted
into three groups.  The first group contains the lexially governed cases
which can be put into a canonical order (alphabetical order on 
real predicate name).  The second group is the constant-valued rels which
have alphabetical order on CARGs.  The third group
are the grammar preds, which can't be put into any order lexically
in general because we have to allow for subsumption (although
if *non-hierarchical-gpreds* is t, we treat them as though there's no
subsumbption.  We may be able to order them by some notion of 
type, but ignore that for now.

In the case of two relations with the same sortal order,
these will go into the same comparison set.

Because this code allows for subsumption, we're in a different
position than with the old MRS equality checking as regards the notion
of a canonical order.  If this were fully general, i.e.,
if we allowed for any predicate to subsume any other,
we would have a big problem ... 

As it is, we make the assumption that we do an initial match on real 
preds where subsumption is simply as indicated by the lemma/pos/sense 
conventions.  Grampreds without CARGS are always treated as secondary.
Grampreds may be in a subsumption hierarchy with respect to 
eachother and real preds, but we aren't going to bother about grampreds
in RMRSs from different sources unless we have some matching 
realpreds.  So the code for doing the gram pred match
can be slower.

FIX? - we could have a speedier version for equality checking where
the canonical order is fully defined.

|#

(defun sort-rmrs (rmrs same-source-p)
  (let* ((liszt (rmrs-liszt rmrs))
	 (new-liszt 
	  (if (and same-source-p
		   (not (eql same-source-p :approx)))
	      (loop for relset in
		    (combine-similar-relations 
		     liszt nil
		     #'rmrs-rel-sort-same-source-eql)
		    ;;; combine-similar-relations in
		    ;;; mrs/mrscorpus.lisp
		  collect
		  (group-relations-by-class relset))
	    (list (group-relations-by-class liszt)))))
    (let ((sorted-liszt
	   (if (and same-source-p
		   (not (eql same-source-p :approx)))
	       (sort new-liszt 
		     #'rmrs-cset-same-source-lesser-p)
	     new-liszt)))
      (setf (rmrs-liszt rmrs)
	sorted-liszt)
      rmrs)))

;;; Sorting predicate functions

(defun rmrs-rel-sort-same-source-eql (rel1 rel2)
  (let ((cfrom1 (rel-cfrom rel1))
	(cfrom2 (rel-cfrom rel2))
	(cto1 (rel-cto rel1))
	(cto2 (rel-cto rel2)))
    (and (eql cfrom1 cfrom2)
	 (eql cto1 cto2))))

(defun rmrs-cset-same-source-lesser-p (cset1 cset2)
  (let ((cfrom1 (comparison-set-cfrom cset1))
	(cfrom2 (comparison-set-cfrom cset2))
	(cto1 (comparison-set-cto cset1))
	(cto2 (comparison-set-cto cset2)))
    (declare (ignore cto1 cto2))
    (< cfrom1 cfrom2)))

    #|
    (or (< cfrom1 cfrom2)
	(and (eql cfrom1 cfrom2) 
	(< cto1 cto2)))))
	|#

(defun rmrs-real-pred-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
  ;;; note that this ignores pos tags and senses deliberately
    (string-lessp (realpred-lemma pred1)
		  (realpred-lemma pred2))))

(defun rmrs-real-pred-approx-lesser-p (rel1 rel2)
  (let ((cfrom1 (rel-cfrom rel1))
	(cfrom2 (rel-cfrom rel2)))
    (< cfrom1 cfrom2)))
    
(defun rmrs-constant-pred-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (if (equal pred1 pred2)
	(let ((str1 (rel-parameter-strings rel1))
	      (str2 (rel-parameter-strings rel2)))
	  (string-lessp str1 str2))
      (string-lessp pred1 pred2))))

(defun rmrs-constant-pred-approx-lesser-p (rel1 rel2)
  (let ((cfrom1 (rel-cfrom rel1))
	(cfrom2 (rel-cfrom rel2)))
    (< cfrom1 cfrom2)))

      
(defun rmrs-grammar-pred-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (string-lessp pred1 pred2)))

;;;

(defun overlapping-rels (rel1 rel2)
  (let ((cfrom1 (rel-cfrom rel1))
	(cfrom2 (rel-cfrom rel2))
	(cto1 (rel-cto rel1))
	(cto2 (rel-cto rel2)))
    (not (or (> cfrom1 cto2)
	     (> cfrom2 cto1)))))
;;; 

(defparameter *non-hierarchical-gpreds-p* nil)

;;; Dividing relations into classes

(defun group-relations-by-class (rels)
  (let ((real-pred-rels nil)
	(constant-pred-rels nil)
	(gram-pred-rels nil)
	(cfrom (rel-cfrom (car rels)))
	(cto (rel-cto (car rels))))
    (dolist (rel rels)
      (let ((pred (rel-pred rel)))
	(cond ((realpred-p pred)
	       (push rel real-pred-rels))
	      ((rel-parameter-strings rel)
	       (push rel constant-pred-rels))
	      (t (push rel gram-pred-rels)))))
    (let ((combined-real-preds
	   (sort 
	    (combine-similar-relations real-pred-rels nil 
				      #'rmrs-real-pred-rel-eql)
	    #'rmrs-real-pred-lesser-p :key #'car))
	  (combined-constant-preds
	   (sort
	    (combine-similar-relations constant-pred-rels nil 
				      #'rmrs-constant-pred-rel-eql)
	    #'rmrs-constant-pred-lesser-p :key #'car))
	  (combined-grammar-preds
	   (if *non-hierarchical-gpreds-p*
	       (sort
		(combine-similar-relations gram-pred-rels nil 
					   #'rmrs-grammar-pred-rel-eql)
		 #'rmrs-grammar-pred-lesser-p :key #'car)
	     gram-pred-rels)))
      (make-comparison-set
       :cfrom cfrom
       :cto cto
       :real-preds combined-real-preds
       :constant-preds combined-constant-preds
       :gram-preds combined-grammar-preds))))

(defun rmrs-constant-pred-rel-eql (rel1 rel2)
  ;;; used for grouping similar relations
    ;;; needs to be kept reasonably consistent with above
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (and (equal pred1 pred2) 
	 (string-equal (rel-parameter-strings rel1)
		       (rel-parameter-strings rel2)))))

(defun rmrs-real-pred-rel-eql (rel1 rel2)
  ;;; used for grouping similar relations
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (compare-rmrs-real-preds pred1 pred2)))

(defun rmrs-grammar-pred-rel-eql (rel1 rel2)
  ;;; only used when grammar preds are not hierarchically organised
  (unless *non-hierarchical-gpreds-p*
    (error "Only used when grammar preds are not in a hierarchy"))
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (equal pred1 pred2)))


;;; **********************************************
;;; Conversion to comp-rmrs structures

;;; This code puts any CARG values into parameter-strings
;;; --- CARGS can subsequently be ignored

(defun convert-to-comparison-rmrs (rmrs)
    (set-carg-values rmrs)
    (make-comp-rmrs
     :top-h (rmrs-top-h rmrs)
     :liszt (rmrs-liszt rmrs)
     :h-cons (rmrs-h-cons rmrs)
     :rmrs-args (rmrs-rmrs-args rmrs)
     :in-groups (rmrs-in-groups rmrs)
     :cfrom (rmrs-cfrom rmrs)
     :cto (rmrs-cto rmrs)
     :origin (rmrs-origin rmrs)))

(defun set-carg-values (rmrs)
  (loop for ep in (rmrs-liszt rmrs)
	do
	(setf (rel-parameter-strings ep)
	      (get-carg-value
	       ep
	       (rmrs-rmrs-args rmrs)))))

(defun get-carg-value (rel rmrs-args)
  (let ((lbl (rel-handel rel)))
    (dolist (arg rmrs-args)
      (when (and (eql-var-id lbl 
			     (rmrs-arg-label arg))
		 (equal (rmrs-arg-arg-type arg)
			"CARG"))
	(return (rmrs-arg-val arg))))))

;;; ********************************************
;;; Code for inital pred comparison
;;;

(defun compare-rmrs-liszts (l1 l2 comp-record same-source-p)
  ;;; if these are from the same-source, then the list of comparison
  ;;; records is sorted by cfrom/cto, so we don't need
  ;;; to compare if we can establish that the first element is
  ;;; greater/lesser and can recurse accordingly.  Otherwise we're
  ;;; comparing everything with everything.
  (if (and l1 l2)
      (let ((first1 (car l1))
	    (first2 (car l2)))
	(if 
	    (and same-source-p
	         (rmrs-cset-same-source-lesser-p first2 first1))
	    (compare-rmrs-liszts l1 (cdr l2) comp-record same-source-p)
	  (if (and same-source-p
		   (rmrs-cset-same-source-lesser-p first1 first2))
	      (compare-rmrs-liszts (cdr l1) l2 comp-record same-source-p)
	    (let ((new-comp-record 
		   (compare-rmrs-rel-set 
		    (car l1) (car l2) comp-record same-source-p)))
	      (compare-rmrs-liszts (cdr l1) (cdr l2) 
				  new-comp-record same-source-p)))))
    comp-record))


(defun compare-rmrs-rel-set (s1 s2 comp-record same-source-p)
  ;;; 1 compare the real preds on an ordered basis
  ;;; (i.e., lexicographic order)
  ;;; 2 compare the constant preds on an ordered basis
  ;;;   (in both cases, store any left overs)
  ;;; 3 compare the grammar preds on an unordered basis
  ;;;   if *non-hierarchical-gpreds-p* is nil (the default)
  ;;;   or ordered if it's t
  ;;; 4 try any left over real and constant preds 
  ;;;   against any left over grammar preds on an unordered
  ;;;   basis
  ;;; except forget this last step for now, since it seems
  ;;; like to be very expensive
  ;;; FIX4
  ;;; same-source-p is :approx is interpreted as providing an
  ;;; ordering for the constant and real preds
  ;;; but as adding an overlap constraint on equality
  ;;; for grammar preds
  (let* ((real-preds1 (comparison-set-real-preds s1))
	 (real-preds2 (comparison-set-real-preds s2))
	 (const-preds1 (comparison-set-constant-preds s1))
	 (const-preds2 (comparison-set-constant-preds s2))
	 (gram-preds1 (comparison-set-gram-preds s1))
	 (gram-preds2 (comparison-set-gram-preds s2))
	 (r-comp-record
	  (compare-rmrs-ordered-rel-set 
	   real-preds1 real-preds2 comp-record
	   (if (eql same-source-p :approx)
	       #'rmrs-real-pred-approx-lesser-p
	     #'rmrs-real-pred-lesser-p) nil))
	 (c-comp-record
	  (compare-rmrs-ordered-rel-set 
	   const-preds1 const-preds2 r-comp-record 
	   (if (eql same-source-p :approx)
	       #'rmrs-constant-pred-approx-lesser-p 
	     #'rmrs-constant-pred-lesser-p) nil))
	 (g-comp-record
	  (if *non-hierarchical-gpreds-p*
	      (compare-rmrs-ordered-rel-set 
	       gram-preds1 gram-preds2 c-comp-record
		 #'rmrs-grammar-pred-lesser-p 
		 (eql same-source-p :approx))
	    (compare-rmrs-unordered-rel-set 
	     gram-preds1 gram-preds2 c-comp-record 
	     (eql same-source-p :approx)))))
	  g-comp-record))

(defun compare-rmrs-ordered-rel-set (l1 l2 comp-record lesser-p-fn 
					overlap-test-p)
  (if (and l1 l2)
      (let ((first1 (car l1))
	    (first2 (car l2)))
	(if (apply lesser-p-fn (list (car first2) (car first1)))
	    (compare-rmrs-ordered-rel-set l1 (cdr l2) 
					  comp-record lesser-p-fn 
					  overlap-test-p)
	  (if (apply lesser-p-fn (list (car first1) (car first2)))
	      (compare-rmrs-ordered-rel-set (cdr l1) l2 
					    comp-record lesser-p-fn
					    overlap-test-p)
	    (let ((new-comp-record
		   (compare-rmrs-unordered-rel-set 
		    first1 first2 comp-record overlap-test-p)))
	      (compare-rmrs-ordered-rel-set 
	       (cdr l1) (cdr l2) 
	       new-comp-record lesser-p-fn overlap-test-p)))))
    comp-record))

(defun compare-rmrs-unordered-rel-set (s1 s2 comp-record overlap-test-p)
  (let ((matches nil))
    (dolist (rel1 s1)
      (dolist (rel2 s2)
	(let ((match (compare-rmrs-rels rel1 rel2 overlap-test-p)))
	  (when match
	    (push match matches)))))
    (when matches
      (push matches
	    (rmrs-comparison-record-matched-rels comp-record)))
    comp-record))


(defun compare-rmrs-rels (rel1 rel2 overlap-test-p)
  (let ((pred-comparison (compare-rmrs-preds rel1 rel2 overlap-test-p)))
    (if pred-comparison
	(multiple-value-bind
	    (cvar-pair hvar-pair) 
	    (get-char-var-pair rel1 rel2)
	       ;;; don't let var match affect comp-status for now
	  (let
	       ((match-record
		 (make-match-rel-record :rel1 rel1
					:rel2 rel2
					:pred-comp-status pred-comparison
					:label-pair 
					(cons (var-id (rel-handel rel1))
					      (var-id (rel-handel rel2)))
					:anchor-pair 
					(if (and (var-p (rel-anchor rel1))
						 (var-p (rel-anchor rel2)))
					    (cons (var-id (rel-anchor rel1))
					      (var-id (rel-anchor rel2))))
					:cvar-pair cvar-pair
					:hvar-pair hvar-pair)))
	  match-record))
      nil)))

(defun get-char-var-pair (rel1 rel2)
  (let* ((var1 (retrieve-rmrs-ep-var rel1))
	 (var2 (retrieve-rmrs-ep-var rel2)))
    (if (and var1 var2)
	(let ((cvar-pair (cons (var-id var1)
			       (var-id var2))))
	  (if (is-handel-var var1)
	      (if (is-handel-var var2)
		  (values nil cvar-pair)
		nil)			; error state
	    (if (is-handel-var var2)
		nil			; error state
	      (values cvar-pair nil)))))))

(defun compare-rmrs-preds (rel1 rel2 overlap-test-p)
  ;;; FIX? - need to think about the parameter
  ;;; strings handling.  Note that rmrs-constant-pred-rel-eql
  ;;; needs to be consistent with this
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (cond ((and (realpred-p pred1) 
		(realpred-p pred2))
	   (compare-rmrs-real-preds pred1 pred2))
	  ((realpred-p pred1) (if (gpred-subsumes-real-p pred2 pred1)
				  :sub1))
	  ;;; 1 is more specific
	  ((realpred-p pred2) (if (gpred-subsumes-real-p pred1 pred2)
				  :sub2))
	  (overlap-test-p
	   ;;; only set for gpreds
	   (if 
	       (and (equal pred1 pred2)
		    (overlapping-rels rel1 rel2))
	       :equal nil))
	  ((and (equal pred1 pred2) 
		(string-equal (rel-parameter-strings rel1)
			      (rel-parameter-strings rel2)))
	 ;;; string equal returns t if args are both nil
	   :equal)
	  ((and (rel-parameter-strings rel1)
	       (rel-parameter-strings rel2)) nil)
	  ;;; this is a sl arbitrary decision that may need to be reconsidered
	  ((and (gpred-subsumes-gpred-p pred1 pred2) 
		(not (rel-parameter-strings rel1))) :sub2)
	  ;;; 2 is more specific
	  ((and (gpred-subsumes-gpred-p pred2 pred1)
		(not (rel-parameter-strings rel2))) :sub1)
	  ((and (gpred-compatible-gpred-p pred1 pred2)
		(not (or (rel-parameter-strings rel1)
			 (rel-parameter-strings rel2)))) :comp)
	  (t nil))))

(defun gpred-subsumes-real-p (gpred real-pred)
  ;;; returns t if the gpred subsumes the real-pred
  ;;; always nil for now
  (declare (ignore gpred real-pred))
  nil)

(defun gpred-subsumes-gpred-p (gpred1 gpred2)
  ;;; currently only called if not equal
  ;;; returns t if gpred1 subsumes gpred2
  ;;; for now, do this via the LKB type hierarchy
  ;;; if we're in the LKB - nil otherwise
  #+:lkb
  (let ((type1 (vsym gpred1))
	(type2 (vsym gpred2)))
    (and (is-valid-type type1)
	 (is-valid-type type2)
	 (equal-or-subtype type2 type1)))
  #-:lkb (declare (ignore gpred1 gpred2))
  #-:lkb nil)

(defun gpred-compatible-gpred-p (gpred1 gpred2)
  ;;; currently only called if not equal
  ;;; or subsuming
  ;;; returns t if gpred1 and gpred2 are compatible
  ;;; (equal, one subsumes the other, or have lb in a general
  ;;; hierarchy)
  ;;; for now, do this via the LKB type hierarchy
  ;;; if we're in the LKB - nil otherwise
  #+:lkb  
  (let ((type1 (vsym gpred1))
	(type2 (vsym gpred2)))
    (and (is-valid-type type1)
	 (is-valid-type type2)
	 (compatible-types type1 type2)))
  #-:lkb (declare (ignore gpred1 gpred2))
  #-:lkb nil)

(defun compare-rmrs-real-preds (pred1 pred2)
  ;;; real preds have two or three parts
  ;;; lemma, pos and (optionally) sense
  ;;; the lemma has to be equal for there to be
  ;;; any relationship 
  ;;; pos has the possibility of an underspecified value
  ;;; (this shouldn't occur with a specified sense)
  ;;;
  ;;; an unspecified sense always subsumes a specified sense
  ;;;
  ;;; currently don't allow anything other than the  
  ;;; specified/unspecified distinction for pos or sense
  (if (equal (realpred-lemma pred1)
	     (realpred-lemma pred2))
	   (cond ((or (equal (realpred-pos pred1)
			     (realpred-pos pred2))
		      (and
		       (equal (realpred-pos pred1) "j")
		       (equal (realpred-pos pred2) "a"))
		      (and
		       (equal (realpred-pos pred1) "a")
		       (equal (realpred-pos pred2) "j")))
		  (cond ((equal (realpred-sense pred1)
				(realpred-sense pred2)) 
			 :equal)
			((null (realpred-sense pred1))
			 :sub2) ;;; 2 is more specific
			((null (realpred-sense pred2))
			 :sub1) ;;; 1 is more specific
			(t nil)))
		 ((unknown-rmrs-pos pred1) 
		  (when (realpred-sense pred1)
			(error "~A should not have a sense specification with unknown pos" pred1))
		  :sub2)
	         ((unknown-rmrs-pos pred2) 
		  (when (realpred-sense pred2)
			(error "~A should not have a sense specification with unknown pos" pred2))
		  :sub1)
		 (t nil))
    nil))

(defun unknown-rmrs-pos (pred)
  (equal (realpred-pos pred)
	 "u"))



;;;; ******************************************************
;;; The second pass
;;; 
;;; *******************************************************

;;; Inequalities
;;;
;;; When two RMRSs are compared, it may be possible to
;;; find a match which involves equating two currently unequated
;;; variables.  For ERG output, we currently assume that this will never
;;; happen.  
;;; For RASP output, we guarantee that primary variables associated
;;; with nouns and with verbs will never be shared.
;;;
;;; e.g. dog_n_rel(x1), cat_n_rel(x2), x1 neq x2
;;;
;;; This could be modelled via binary inequalities, but this is
;;; extremely verbose and currently seems unnecessary
;;; Instead we have a list of distinguished variables
;;; and a list of undistinguished variables. 
;;; An undistinguished variable may be equated with a distinguished
;;; one or with another undistinguished variable, but two
;;; distinguished variables may never be equated.
;;; For the ERG, currently, all real variables are distinguished.
;;; All labels are always distinguished.

(defun distinguished-rel-type-p (rel)
  (let ((pred (rel-pred rel)))
    (or ;; (equal pred "named_rel")
     (rel-parameter-strings rel)
     (and (realpred-p pred)
	  (or (equal (realpred-pos pred) "p")
	      (equal (realpred-pos pred) "n")
	      (equal (realpred-pos pred) "a")
	      (equal (realpred-pos pred) "v"))))))

;;; Pruning 

(defun prune-comparison-record (comp-record)
  (let ((firm-bindings nil)
	(a-set nil)
	(b-set nil)
	(matches (rmrs-comparison-record-matched-rels
			    comp-record)))
    (dolist (match-set matches)
      	(let ((firm-p (not (cdr match-set))))      
	  (dolist (mrec match-set)
	    (let ((pred-comp-status 
		   (match-rel-record-pred-comp-status mrec)))
	      (when (distinguished-rel-type-p
		     (if (eql pred-comp-status :sub2)
			 (match-rel-record-rel2 mrec)
		       (match-rel-record-rel1 mrec)))
		(let ((label-pair (match-rel-record-label-pair mrec))
		      (anchor-pair (match-rel-record-anchor-pair mrec))
		      (cvar-pair (match-rel-record-cvar-pair mrec)))
		  (when firm-p
		    (push label-pair firm-bindings)
		    (when anchor-pair 
		      (push anchor-pair firm-bindings))
		    (push cvar-pair firm-bindings))
		  (push (car label-pair) a-set)
		  (push (cdr label-pair) b-set)
		  (when anchor-pair 
		    (push (car anchor-pair) a-set)
		    (push (cdr anchor-pair) b-set))
		  (push (car cvar-pair) a-set)
		  (push (cdr cvar-pair) b-set)))))))
    (let ((new-matches
	   (loop for match-set in matches
	       nconc
		 (prune-match-set match-set firm-bindings a-set b-set))))
      (setf (rmrs-comparison-record-matched-rels
	     comp-record)
	new-matches)
      comp-record)))
    
(defun prune-match-set (mrecs bindings a-set b-set)
  (declare (ignore a-set b-set))
  ;;; FIX1
  ;;;
  ;;; this doesn't look at arguments but gets rid of
  ;;; det etc pairings where there's a match failure 
  ;;; on the noun.  It could be adapted so it looked at arguments
  ;;; too - see whether this is necessary later.
  ;;; The rationale here is that things like determiners have
  ;;; no interesting existence apart from the nouns.
  (let ((new-mrecs
	 (loop for match-record in mrecs
	     unless
	       (let ((cvar-pair (match-rel-record-cvar-pair match-record)))
		 (and cvar-pair
		      (or ; (not (member (car cvar-pair) a-set))
			  ; (not (member (cdr cvar-pair) b-set))
			  (incompatible-with-bindings-p 
			   cvar-pair
			   bindings))))
	     collect match-record)))
    (if new-mrecs
	(list new-mrecs))))
	     
(defun incompatible-with-bindings-p (pair firm-bindings)
  ;;; all vars in firm bindings are
  ;;; assumed to be unique
    (let* ((a-var (car pair))
	   (b-var (cdr pair))
	   (bound-a (assoc a-var firm-bindings))
	   (bound-b (rassoc b-var firm-bindings)))
      (or (and bound-a
	       (not (eql (cdr bound-a) b-var)))
	  (and bound-b
	       (not (eql (car bound-b) a-var))))))


;;; ******************************************************
;;; Expansion and arguments
;;;
;;; FIX1 - this needs to be modified to prune in the same way as above
;;; so that we only get determiners matching with the particular noun
;;;
;;; see (lkb::test-eg 3)
;;;
;;; *****************************************************

(defun expand-comparison-records (comp-record rmrs1 rmrs2)
  (let* ((match-alternatives (rmrs-comparison-record-matched-rels
			      comp-record))
	 (flat-matches
	  (expand-comparison-records-aux match-alternatives)))
    (loop for option in flat-matches
	collect
	  (let*
	      ((bindings (collect-all-bindings option))
	       (qeq-pairs (collect-qeq-pairs rmrs1 rmrs2 bindings)))
	    (make-rmrs-comparison-record 
	     :matched-rels option
	     :bindings bindings
	     :qeq-pairs qeq-pairs)))))


(defun expand-comparison-records-aux (option-list)
  (if option-list
      (let ((mutually-compatible
	     (find-maximal-length-comp
	      (expand-multiple-comp-records (car option-list)))))     
	(if (cdr option-list)
	    (let ((res (expand-comparison-records-aux (cdr option-list))))
	      (loop for option-set in mutually-compatible
		  nconc
		    (loop for partial in res
			collect
			  (append option-set partial))))
	mutually-compatible))
    nil))

(defun expand-multiple-comp-records (opt-set)
  ;;; this comes into play when we have things like
  ;;; ((theA1 theB1) (theA1 theB2) (theA2 theB1) (theA2 theB2))
  ;;; where we want to return
  ;;; (((theA1 theB1) (theA2 theB2)) ((theA1 theB2) (theA2 theB1)))
  (if opt-set
      (let ((new (car opt-set))
	    (res 
	     (expand-multiple-comp-records (cdr opt-set))))
	(if res
	    (let ((new-res
		   (loop for poss in res
		       unless
			 (rel-already-used new poss)
		       collect
			 (cons new poss))))
	      (cons (list new) (append new-res res)))
	  (list (list new))))
    nil))

(defun find-maximal-length-comp (sets)
  (let ((max-length 0)
	(current-res nil))
    (dolist (set sets)
      (let ((l (length set)))
      (cond  ((> l max-length)
	      (setf max-length l)
	      (setf current-res (list set)))
	     ((= l max-length)
	      (push set current-res))
	     (t nil))))
    current-res))

(defun rel-already-used (mrec mrec-list)
  (let ((arel (match-rel-record-rel1 mrec))
	(brel (match-rel-record-rel2 mrec)))
    (dolist (used mrec-list)
      (when (or (eql (match-rel-record-rel1 used)
		     arel)
		(eql (match-rel-record-rel2 used)
		     brel))
	(return t)))))

(defun collect-all-bindings (mrecs)
  (let ((bindings nil))
    (dolist (mrec mrecs)
      (push (match-rel-record-label-pair mrec) bindings)
      (let ((anchor-pair (match-rel-record-anchor-pair mrec)))
	(when anchor-pair
	  (push anchor-pair bindings)))
      (let ((cvar-pair (match-rel-record-cvar-pair mrec)))
	(when cvar-pair
	  (push cvar-pair bindings)))
      (let ((hvar-pair (match-rel-record-hvar-pair mrec)))
	(when hvar-pair
	  (push hvar-pair bindings))))
    bindings))

(defun collect-qeq-pairs (rmrs1 rmrs2 bindings)
  ;;; this collects all the top parts of the qeqs, if the bottom parts
  ;;; are in the bindings.  Then, if a handle arg is found which
  ;;; matches a qeq-pair, accept it as valid, and add that pairing to
  ;;; the bindings.  qeqs themselves will then be accepted.
  ;;; The point is we only want to accept args as matching if they are
  ;;; properly grounded and qeqs if they correspond to handle args
  (let ((qeq-pairs nil))
    (dolist (hcons1 (rmrs-h-cons rmrs1))
      (let ((larg-binding (cdr (assoc (var-id (hcons-outscpd hcons1))
				      bindings)))
	    (harg1 (var-id (hcons-scarg hcons1))))
	(when larg-binding
	  (dolist (hcons2 (rmrs-h-cons rmrs2))
	    (when (eql (var-id (hcons-outscpd hcons2)) larg-binding)
	      (let ((harg2 (var-id (hcons-scarg hcons2))))
		(push (cons harg1 harg2)
		      qeq-pairs)
		(return t)))))))
    qeq-pairs))


;;; Checking top label
;;;

(defun add-top-label-compare (comp rmrs1 rmrs2)
  (let* ((bindings (rmrs-comparison-record-bindings comp))
	 (top1 (rmrs-top-h rmrs1))
	 (top2 (rmrs-top-h rmrs2))
	 (top1-match (assoc (var-id top1) bindings)))
    (when (and top1-match
	       (eql (cdr top1-match) (var-id top2)))
      (setf (rmrs-comparison-record-matched-top comp)
	(make-match-top-record 
		:label1 top1
		:label2 top2)))))
	     
    

;;; Checking argument bindings
;;;
;;;
#|
Arguments are checked after expansion to see whether they
are compatible with the set of binding for the particular
set of match record that has been produced.

Argument compatability status is recorded.

Unattached arguments are ignored
|#

(defun add-argument-compare (comp rmrs1 rmrs2)
  (let ((bindings (rmrs-comparison-record-bindings comp))
	(qeq-pairs (rmrs-comparison-record-qeq-pairs comp))
	(arg-matches nil))
;;;    (pprint bindings)
    (dolist (rmrs-arg1 (rmrs-rmrs-args rmrs1))
      (let* ((label1 (rmrs-arg-label rmrs-arg1)) 
	     (value1 (rmrs-arg-val rmrs-arg1))
	     (arg-type1 (rmrs-arg-arg-type rmrs-arg1))
	     (label-binding (cdr (assoc (var-id label1) 
					bindings))))
	(when label-binding
	  (dolist (rmrs-arg2 (rmrs-rmrs-args rmrs2))
	    (let ((label2 (rmrs-arg-label rmrs-arg2)))
	      (when (eql (var-id label2)
			 label-binding)
		(let ((value2 (rmrs-arg-val rmrs-arg2))
		      (arg-type2 (rmrs-arg-arg-type rmrs-arg2)))
		  (cond ((and (equal arg-type1 "CARG")
			      (equal arg-type2 "CARG"))
			 (when (equal value1 value2)
			   (push (make-match-arg-record 
				  :arg1 rmrs-arg1
				  :arg2 rmrs-arg2
				  :comp-status :equal
				  :arg-type :carg)
				 arg-matches))
			 (return))	; return anyway
			((or (not (var-p value1))
			     (not (var-p value2)))
			 nil)
			((and (is-handel-var value1)
			      (is-handel-var value2))
			 (let ((arg-binding (assoc (var-id value1) 
							bindings))
			       (qeq-binding (assoc (var-id value1) 
							qeq-pairs)))
			   (when (or (and arg-binding
					  (eql (var-id value2)
					       (cdr arg-binding)))
				     (and qeq-binding
					  (eql (var-id value2)
					       (cdr qeq-binding))))
			     (let ((comp-status 
				    (compatible-arg-types arg-type1
							  arg-type2)))
			       (when comp-status
				 (push (make-match-arg-record 
					:arg1 rmrs-arg1
					:arg2 rmrs-arg2
					:comp-status comp-status)
				     arg-matches)
				 (unless arg-binding
				   (push qeq-binding
					 (rmrs-comparison-record-bindings 
					  comp))))
			       (return))))) 
			((or (is-handel-var value1)
			      (is-handel-var value2))
			 nil)
			(t (let ((arg-binding (assoc (var-id value1) 
							  bindings)))
			     (when (and arg-binding
					(eql (var-id value2)
					     (cdr arg-binding)))
			       (let ((comp-status 
				      (compatible-arg-types arg-type1
							    arg-type2)))
				 (when comp-status
				   (push (make-match-arg-record 
					  :arg1 rmrs-arg1
					  :arg2 rmrs-arg2
					  :comp-status comp-status)
					 arg-matches))
				 (return)))))))))))))
    (setf (rmrs-comparison-record-matched-args comp)
      arg-matches)))

(defun compatible-arg-types (arg-type1 arg-type2)
  ;;; FIX3 - will need expansion
  (cond 
   ((and (string-equal arg-type1 "argn")
	 (string-equal arg-type2 "argn"))
    :comp)
   ((string-equal arg-type1 arg-type2) :equal)
   ((string-equal arg-type1 "argn") :sub2)
   ((string-equal arg-type2 "argn") :sub1)
   ((and (string-equal arg-type1 "arg2-4")
	 (or (string-equal arg-type2 "arg2")
	     (string-equal arg-type2 "arg3")
	     (string-equal arg-type2 "arg4")))
	 :sub1)
   ((and (string-equal arg-type2 "arg2-4")
	 (or (string-equal arg-type1 "arg2")
	     (string-equal arg-type1 "arg3")
	     (string-equal arg-type1 "arg4")))
	 :sub1)
   (t nil)))


;;; Checking in-groups
;;;
;;; Canonicalised in-groups - see notes in convert.lisp - mean
;;; that this is simply an equality comparison (cf hcons)

(defun add-ing-compare (comp rmrs1 rmrs2)
  (let ((bindings (rmrs-comparison-record-bindings comp))
	(ing-matches nil))
    (dolist (ing1 (rmrs-in-groups rmrs1))
      (let ((a-binding (cdr (assoc (var-id (in-group-label-a ing1)) 
				   bindings)))
	    (b-binding (cdr (assoc (var-id (in-group-label-b ing1)) 
				   bindings))))
	(when (and a-binding b-binding)
	  (dolist (ing2 (rmrs-in-groups rmrs2))
	    (when (and (eql (var-id (in-group-label-a ing2)) a-binding)
		       (eql (var-id (in-group-label-b ing2)) b-binding))
	      (push
	       (make-match-ing-record 
		:ing1 ing1
		:ing2 ing2)
	       ing-matches)
	      (return))))))
    (setf (rmrs-comparison-record-matched-ings comp)
      ing-matches)))

  
  
;;; Checking h-cons
;;;
;;; This is a syntactic check in the sense that two sets of
;;; h-cons might give rise to the same scopings but
;;; still be non-eql.  However, because the qeq construction
;;; has rules which are obeyed by the ERG and RASP, this doesn't concern
;;; us.  The hcons check is therefore trivial - exact match is a match
;;; anything else isn't

(defun add-hcons-compare (comp rmrs1 rmrs2)
  (let ((bindings (rmrs-comparison-record-bindings comp))
	(hcons-matches nil))
    (dolist (hcons1 (rmrs-h-cons rmrs1))
      (let ((harg-binding (cdr (assoc (var-id (hcons-scarg hcons1)) bindings)))
	    (larg-binding (cdr (assoc (var-id (hcons-outscpd hcons1)) bindings))))
	(when (and harg-binding larg-binding)
	  (dolist (hcons2 (rmrs-h-cons rmrs2))
	    (when (and (eql (var-id (hcons-scarg hcons2)) harg-binding)
		       (eql (var-id (hcons-outscpd hcons2)) larg-binding))
	      (push
	       (make-match-hcons-record 
		:hcons1 hcons1
		:hcons2 hcons2)
	       hcons-matches)
	      (return))))))
    (setf (rmrs-comparison-record-matched-hcons comp)
      hcons-matches)))



;;; ****** Comparison of RMRSs based on data stored in the fine system

#|

We assume that we have a test suite which has been parsed using the ERG
and by RASP.  What is stored in the fine system is the derivation in both 
cases


|#


;;; temporary - set these interactively soon

(defparameter *tsdb-directory1* "cue_phrases/cue_phrases/general/lingo")


(defparameter *tsdb-directory2* "cue_phrases/cue_phrases/general/rasp")


;;; Window showing sentences to compare

#+:lkb
(defun display-sentences-to-compare nil
  (mrs::clear-rule-record)
  ;;; temporary - set these interactively soon
  (let ((*rasp-rmrs-gram-file*
         (make-pathname 
          :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
          :name "gram14.1.rmrs"))
        (*rasp-rmrs-tag-file*
         (make-pathname :directory "/homes/aac10/lingo/lkb/src/rmrs/annlt-test/"
	:name "lex14.1.rmrs")))
    (mrs::read-rmrs-grammar *rasp-rmrs-gram-file*)
    (mrs::read-rmrs-tag-templates *rasp-rmrs-tag-file*))
  (let ((test-items   #+:tsdb (lkb::get-test-suite-sentences *tsdb-directory1*)
                      #-:tsdb nil))
    (if test-items
        (lkb::draw-active-list
         (loop for record in test-items
               collect
               (cons 
                (format nil "~a: ~a" (car record) (cdr record))
                record))
         "Test items"
         (list
          (cons "Compare RMRSs"
                #'(lambda (record)
                    (compare-rmrs-from-test-suite record)))))
      (format t "~%Test suite items cannot be retrieved")))) 


(defparameter *characterisation-hack* nil)

#+:lkb
(defun compare-rmrs-from-test-suite (record)
  #+:tsdb  
  (let* ((egnum (car record))
         (sentence (cdr record))
         (*characterisation-hack*
          (if lkb::*characterize-p*
              (let ((words (lkb::preprocess-sentence-string sentence)))
                (if (lkb::chared-word-p (car words))
                    words))))
         (rasp-rmrs 
          (lkb::get-tsdb-selected-rasp-rmrs egnum *tsdb-directory2*))
         (erg-rmrs
          (lkb::get-tsdb-selected-erg-rmrs egnum *tsdb-directory1*)))
    (when (and rasp-rmrs erg-rmrs)
      (dolist (comparison-record (mrs::compare-rmrs erg-rmrs rasp-rmrs t))
        ;; use string position
        (lkb::show-mrs-rmrs-compare-window erg-rmrs rasp-rmrs 
                                           comparison-record sentence))))
    #-:tsdb  
    (declare (ignore record))
    #-:tsdb 
    nil)

#+:lkb
(defun find-cfrom-hack (from)
  (let ((word (nth from *characterisation-hack*)))
    (if word
        (lkb::chared-word-cfrom word)
      -1)))

#+:lkb
(defun find-cto-hack (to)
  (let ((word (nth to *characterisation-hack*)))
    (if word
        (lkb::chared-word-cto word)
      -1)))

;;; end fine system stuff

;;; ****************************************************
;;;
;;; Comparison of RMRSs where the characterisation is missing or
;;; non-aligned.
;;;
;;; ****************************************************


#|

if cfrom cto isn't present or isn't consistent, we try an
approximation, which is to set cfrom and cto on the basis of the
real-preds these may not match precisely - we find the list of
realpreds that are in both rmrses, match them up, number these
successively and consistently, and number everything surrounding them
so that they have cfrom/cto pairs which are consistent with a location
that could be either side of a realpred.


for instance, if we have

the dog gpred1 likes the gpred1 cat

the gpred1 dog like s the cat

the renumbering will end up as

1.1the 3.3dog 2.6gpred1 5.5likes 7.7the 6.10gpred1 9.9cat

1.1the 0.4gpred1 3.3dog 5.5like 6.6s 7.7the 9.9cat
|#


#| 

(let* ((test-rmrs1 (read-single-rmrs-file "richard.rmrs"))
      (test-rmrs2 (read-single-rmrs-file "richard.rmrs"))
      (comp-recs (compare-rmrs test-rmrs1 test-rmrs2 :approx)))
  (dolist (comp-rec comp-recs)
    (lkb::show-mrs-rmrs-compare-window test-rmrs1 test-rmrs2
				       comp-rec "foo")))

|#

(defun compare-rmrs-no-char (rmrs1 rmrs2)
  (let ((*non-hierarchical-gpreds-p* t))
    ;;; don't try and match according to a hierarchy (cf syntactic-p
    ;;; option for mrsequal-p
    (multiple-value-bind (renumbered-rmrs1 renumbered-rmrs2)
	 (renumber-rmrses rmrs1 rmrs2)
	 (compare-rmrs renumbered-rmrs1 renumbered-rmrs2
		       :approx))))
	
    

(defun renumber-rmrses (rmrs1 rmrs2)
  (set-carg-values rmrs1)
  (set-carg-values rmrs2)
  (let* ((rmrs1-lemma-rels nil)
	 (rmrs2-lemma-rels nil))
    (loop for rel in (rmrs-liszt rmrs1)
	  do 
	  (setf (rel-cfrom rel) nil)
	  (setf (rel-cto rel) nil)
	  (let ((pred (rel-pred rel)))
	    (if (or (realpred-p pred)
		    (rel-parameter-strings rel))
		   (push rel rmrs1-lemma-rels))))
    (setf rmrs1-lemma-rels (nreverse rmrs1-lemma-rels))
    (loop for rel in (rmrs-liszt rmrs2)
	  do 
	  (setf (rel-cfrom rel) nil)
	  (setf (rel-cto rel) nil)
	  (let ((pred (rel-pred rel)))
	    (if (or (realpred-p pred)
		    (rel-parameter-strings rel))
		   (push rel rmrs2-lemma-rels))))
    (setf rmrs2-lemma-rels (nreverse rmrs2-lemma-rels))
    (let ((match-list
	   (match-lemma-preds rmrs1-lemma-rels 
			     rmrs2-lemma-rels)))
    ;;; produce a list of paired real-preds
      (loop for pair in match-list
	    and num from 1 by 2
	    do
	    (setf (rel-cfrom (car pair)) num)
	    (setf (rel-cfrom (cdr pair)) num)
	    (setf (rel-cto (car pair)) num)
	    (setf (rel-cto (cdr pair)) num))
      (reset-cfrom-tos (rmrs-liszt rmrs1))
      (reset-cfrom-tos (rmrs-liszt rmrs2))
      (values rmrs1 rmrs2))))


(defun reset-cfrom-tos (eps)
  (let ((cfrom 0) (cto 2))
    (loop for ep in eps
	  do
	  (cond ((rel-cfrom ep) 
		 (setf cfrom (- (rel-cfrom ep) 1))
		 (setf cto (+ (rel-cfrom ep) 3)))
		((or (realpred-p (rel-pred ep))
		     (rel-parameter-strings ep))
		 (setf (rel-cfrom ep) cfrom)
		 (setf (rel-cto ep) cfrom))
		(t (setf (rel-cfrom ep) cfrom)
		   (setf (rel-cto ep) cto))))))
	    
	    
(defun match-lemma-preds (eps1 eps2)
  (let ((matches nil))
    (loop for ep1 in eps1
	  and count1 from 0
	  do
	  (loop for ep2 in eps2
		and count2 from 0
	       do
	       (when (sloppy-match-ep ep1 ep2)
		 (push (cons count1 count2) matches))))
    (let* ((refined-list (prune-matches-for-compatability matches))
	   (best-list (select-best-matches (reverse refined-list) nil)))
      (loop for pairing in best-list
	    collect
	    (cons (nth (car pairing) eps1)
		  (nth (cdr pairing) eps2))))))
	  

(defun sloppy-match-ep (ep1 ep2)
  (let* ((pred1 (rel-pred ep1))
	 (pred2 (rel-pred ep2))
	 (lemma1 (or (rel-parameter-strings ep1)
		     (if (realpred-p (rel-pred ep1))
			 (realpred-lemma pred1))))
	 (lemma2 (or (rel-parameter-strings ep2)
		     (if (realpred-p (rel-pred ep2))
			 (realpred-lemma pred2)))))
    (if (and (stringp lemma1) (stringp lemma2))
	(string-equal lemma1 lemma2))))

#|

(select-best-matches 
(reverse (prune-matches-for-compatability 
'((1 . 2) (3 . 2) (4 . 4) (4 . 6) (5 . 4) (5 . 6) (6 . 5) (7 . 2) (6 . 7)))) 
nil)

returns

((6 . 7) (5 . 6) (4 . 4) (1 . 2))

|#

(defun prune-matches-for-compatability (match-list)
  (loop for pair1 in match-list
	collect
	(cons pair1
	      (loop for pair2 in match-list
		    unless (equal pair1 pair2)
		    when (pairing-compatible pair1 pair2)
		    collect pair2))))

(defun pairing-compatible (pair1 pair2)
  (let ((left (car pair1)) (right (cdr pair1)))
    (or (and (> (car pair2) left) 
	     (> (cdr pair2) right))
	(and (< (car pair2) left) 
	     (< (cdr pair2) right)))))

(defun select-best-matches (comp-lists ok-nodes)
  (if comp-lists
      (let ((max 0) (top nil))
	(loop for clist in comp-lists
	      when (> (length clist) max)
	      do (setf top clist))
	(let* ((best-node (car top))
	       (compatible-nodes (cdr top))
	       (possible-lists
		(loop for clist in comp-lists
		      unless (eq clist top)
		      when (member (car clist) compatible-nodes)
		      collect 
		      (cons (car clist)
			    (loop for pair in (cdr clist)
				  unless (not (pairing-compatible 
					       pair best-node))
				  collect pair)))))
    ;;; collect all lists compatible with top
    ;;; removing elements which were incompatible
	  (select-best-matches possible-lists
			       (cons best-node ok-nodes))))
    ok-nodes))

