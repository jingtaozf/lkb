;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

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
;;; real-preds and constant-preds are sorted, gram-preds aren't.

;;; Matching involves taking advantage of sortal order
;;; wherever possible.  This means making assumptions
;;; about compatibility between different classes of predicates.
;;; This will probably have to be parameterisable depending on the
;;; grammar and the application, since we might have `external'
;;; predicate hierarchies (or no hierarchies at all)


;;; *********************************************
;;; Storing results 
;;;

(defstruct rmrs-comparison-record
  matched-top matched-rels matched-args matched-ings matched-hcons bindings)

(defstruct match-rel-record
  rel1
  rel2
  pred-comp-status
  var-comp-status
  arg-comp-status
  label-pair ;; rel labels
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
  comp-status)

(defstruct match-ing-record
  ing1
  ing2)

(defstruct match-hcons-record
  hcons1
  hcons2)

  
;;; *************************************************************  
;;;
;;; Code
;;;

;;; Main entry point - same-source-p is t if we want
;;; to use the character position information 
;;; FIX? - possibly need to put in a quick and dirty check for the
;;; weighted match case for e.g. QA, since otherwise this
;;; might be too expensive computationally

(defun compare-rmrs (rmrs1 rmrs2 same-source-p input-string)
  (declare (ignore input-string))
  ;;; returns a list of comparison records
  (unless (and (rmrs-p rmrs1) (rmrs-p rmrs2))
    (error "Arguments to compare-rmrs are not valid RMRSs"))
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
	  (expand-comparison-records second-pass)))
    (dolist (comp expanded)
      (add-top-label-compare comp new-rmrs1 new-rmrs2) 
      (add-argument-compare comp new-rmrs1 new-rmrs2)
      (add-ing-compare comp new-rmrs1 new-rmrs2)
      (add-hcons-compare comp new-rmrs1 new-rmrs2))
    expanded))

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
because we have to allow for subsumption.
We may be able to order them by some notion of 
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

FIX - we could have a speedier version for equality checking where
the canonical order is fully defined.

|#

(defun sort-rmrs (rmrs same-source-p)
  (let* ((liszt (rmrs-liszt rmrs))
	 (new-liszt 
	  (if same-source-p
	      (loop for relset in
		    (combine-similar-relations liszt nil 
					       #'rmrs-rel-sort-same-source-eql)
		    ;;; combine-similar-relations in
		    ;;; mrs/mrscorpus.lisp
		  collect
		  (group-relations-by-class relset))
	    (list (group-relations-by-class liszt)))))
    (let ((sorted-liszt
	   (if same-source-p
	       (sort new-liszt 
		     #'rmrs-cset-same-source-lesser-p)
	     new-liszt)))
      (setf (rmrs-liszt rmrs)
	sorted-liszt)
      rmrs)))

;;; Sorting predicate functions

(defun rmrs-rel-sort-same-source-eql (rel1 rel2)
  (let ((cfrom1 (char-rel-cfrom rel1))
	(cfrom2 (char-rel-cfrom rel2))
	(cto1 (char-rel-cto rel1))
	(cto2 (char-rel-cto rel2)))
    (and (eql cfrom1 cfrom2)
	 (eql cto1 cto2))))

(defun rmrs-rel-sort-same-source-lesser-p (rel1 rel2)
  (let ((cfrom1 (char-rel-cfrom rel1))
	(cfrom2 (char-rel-cfrom rel2))
	(cto1 (char-rel-cto rel1))
	(cto2 (char-rel-cto rel2)))
    (or (< cfrom1 cfrom2)
	(and (eql cfrom1 cfrom2) 
	     (< cto1 cto2)))))

(defun rmrs-cset-same-source-lesser-p (cset1 cset2)
  (let ((cfrom1 (comparison-set-cfrom cset1))
	(cfrom2 (comparison-set-cfrom cset2))
	(cto1 (comparison-set-cto cset1))
	(cto2 (comparison-set-cto cset2)))
    (or (< cfrom1 cfrom2)
	(and (eql cfrom1 cfrom2) 
	     (< cto1 cto2)))))

(defun rmrs-constant-pred-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
    (if (equal pred1 pred2)
	(let ((str1 (rel-parameter-strings rel1))
	      (str2 (rel-parameter-strings rel2)))
	  (string-lessp str1 str2))
      (string-lessp pred1 pred2))))
      
(defun rmrs-real-pred-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
  ;;; note that this ignores pos tags and senses deliberately
    (string-lessp (realpred-lemma pred1)
		  (realpred-lemma pred2))))

;;; Dividing relations into classes

(defun group-relations-by-class (rels)
  (let ((real-pred-rels nil)
	(constant-pred-rels nil)
	(gram-pred-rels nil)
	(cfrom (char-rel-cfrom (car rels)))
	(cto (char-rel-cto (car rels))))
    (dolist (rel rels)
      (let ((pred (rel-pred rel)))
	(cond ((realpred-p pred)
	       (push rel real-pred-rels))
	      ((rel-parameter-strings rel)
	       (push rel constant-pred-rels))
	      (t (push rel gram-pred-rels)))))
    (let ((combined-real-preds
	   (combine-similar-relations real-pred-rels nil 
				      #'rmrs-real-pred-rel-eql))
	  (combined-constant-preds
	   (combine-similar-relations constant-pred-rels nil 
				      #'rmrs-constant-pred-rel-eql)))
      (make-comparison-set
       :cfrom cfrom
       :cto cto
       :real-preds (sort combined-real-preds
			 #'rmrs-real-pred-lesser-p :key #'car)
       :constant-preds (sort combined-constant-preds
			     #'rmrs-constant-pred-lesser-p :key #'car)
       :gram-preds gram-pred-rels))))

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


;;; **********************************************
;;; Conversion to comp-rmrs structures

;;; This code puts any CARG values into parameter-strings
;;; --- CARGS can subsequently be ignored

(defun convert-to-comparison-rmrs (rmrs)
    (loop for ep in (rmrs-liszt rmrs)
	do
	  (setf (rel-parameter-strings ep)
	    (get-carg-value
	     ep
	     (rmrs-rmrs-args rmrs))))
    (make-comp-rmrs
     :top-h (rmrs-top-h rmrs)
     :liszt (rmrs-liszt rmrs)
     :h-cons (rmrs-h-cons rmrs)
     :rmrs-args (rmrs-rmrs-args rmrs)
     :in-groups (rmrs-in-groups rmrs)
     :bindings nil ;;; should already be canonicalized??
     :cfrom (rmrs-cfrom rmrs)
     :cto (rmrs-cto rmrs)
     :origin (rmrs-origin rmrs)))

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
		    (car l1) (car l2) comp-record)))
	      (compare-rmrs-liszts (cdr l1) (cdr l2) 
				  new-comp-record same-source-p)))))
    comp-record))

(defun compare-rmrs-rel-set (s1 s2 comp-record)
  ;;; 1 compare the real preds on an ordered basis
  ;;; 2 compare the constant preds on an ordered basis
  ;;;   (in both cases, store any left overs)
  ;;; 3 compare the grammar preds on an unordered basis
  ;;; 4 try any left over real and constant preds 
  ;;;   against any left over grammar preds on an unordered
  ;;;   basis
  ;;; except forget this last step for now, since it seems
  ;;; like to be very expensive
  ;;; FIX!!!
  (let ((real-preds1 (comparison-set-real-preds s1))
	(real-preds2 (comparison-set-real-preds s2))
	(const-preds1 (comparison-set-constant-preds s1))
	(const-preds2 (comparison-set-constant-preds s2))
	(gram-preds1 (comparison-set-gram-preds s1))
	(gram-preds2 (comparison-set-gram-preds s2)))
    (let
	((r-comp-record
	  (compare-rmrs-ordered-rel-set 
	   real-preds1 real-preds2 comp-record #'rmrs-real-pred-lesser-p)))
      (let ((c-comp-record
	     (compare-rmrs-ordered-rel-set 
	      const-preds1 const-preds2 r-comp-record 
	      #'rmrs-constant-pred-lesser-p)))
	(let ((g-comp-record
		(compare-rmrs-unordered-rel-set 
		 gram-preds1 gram-preds2 c-comp-record)))
	  g-comp-record)))))

(defun compare-rmrs-ordered-rel-set (l1 l2 comp-record lesser-p-fn)
  (if (and l1 l2)
      (let ((first1 (car l1))
	    (first2 (car l2)))
	(if (apply lesser-p-fn (list (car first2) (car first1)))
	    (compare-rmrs-ordered-rel-set l1 (cdr l2) 
					  comp-record lesser-p-fn)
	  (if (apply lesser-p-fn (list (car first1) (car first2)))
	      (compare-rmrs-ordered-rel-set (cdr l1) l2 
					    comp-record lesser-p-fn)
	    (let ((new-comp-record
		   (compare-rmrs-unordered-rel-set 
		    first1 first2 comp-record)))
	      (compare-rmrs-ordered-rel-set (cdr l1) (cdr l2) 
					    new-comp-record lesser-p-fn)))))
    comp-record))

(defun compare-rmrs-unordered-rel-set (s1 s2 comp-record)
  (let ((matches nil))
    (dolist (rel1 s1)
      (dolist (rel2 s2)
	(let ((match (compare-rmrs-rels rel1 rel2)))
	  (when match
	    (push match matches)))))
    (when matches
      (push matches
	    (rmrs-comparison-record-matched-rels comp-record)))
    comp-record))


(defun compare-rmrs-rels (rel1 rel2)
  (let ((pred-comparison (compare-rmrs-preds rel1 rel2)))
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

(defun compare-rmrs-preds (rel1 rel2)
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
	  ((and (gpred-compatible-gred-p pred1 pred2)
		(not (or (rel-parameter-strings rel1)
			 (rel-parameter-strings rel2)))) :comp)
	  (t nil))))

(defun gpred-subsumes-real-p (gpred real-pred)
  ;;; returns t if the gpred subsumes the real-pred
  ;;; always nil for now
  (declare (ignore gpred real-pred))
  nil)

(defun gpred-subsumes-gpred-p (gpred1 gpred2)
  ;;; returns t if gpred1 subsumes gpred2
  ;;; for now, do this via the LKB type hierarchy
  (equal-or-subtype (vsym gpred2) (vsym gpred1)))

(defun gpred-compatible-gred-p (gpred1 gpred2)
  ;;; returns t if gpred1 and gpred2 are compatible
  ;;; (equal, one subsumes the other, or have lb in a general
  ;;; hierarchy)
  ;;; for now, do this via the LKB type hierarchy
  (compatible-types (vsym gpred1) (vsym gpred2)))

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
	   (cond ((equal (realpred-pos pred1)
		      (realpred-pos pred2))
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
    (or (equal pred "named_rel")
	(and (realpred-p pred)
	 (or (equal (realpred-pos pred) "n")
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
		      (cvar-pair (match-rel-record-cvar-pair mrec)))
		  (when firm-p
		    (push label-pair firm-bindings)
		    (push cvar-pair firm-bindings))
		  (push (car label-pair) a-set)
		  (push (cdr label-pair) b-set)
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
;;; FIX - this needs to be modified to prune in the same way as above
;;; so that we only get determiners matching with the particular noun
;;;
;;; *****************************************************

(defun expand-comparison-records (comp-record)
  (let* ((match-alternatives (rmrs-comparison-record-matched-rels
			      comp-record))
	 (flat-matches
	  (expand-comparison-records-aux match-alternatives)))
    (loop for option in flat-matches
	collect
	  (make-rmrs-comparison-record 
	   :matched-rels option
	   :bindings (collect-all-bindings option)))))


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
      (let ((cvar-pair (match-rel-record-cvar-pair mrec)))
	(when cvar-pair
	  (push cvar-pair bindings)))
      (let ((hvar-pair (match-rel-record-hvar-pair mrec)))
	(when hvar-pair
	  (push hvar-pair bindings))))
    bindings))

;;; Checking top label
;;;


(defun add-top-label-compare (comp rmrs1 rmrs2)
  (declare (ignore comp rmrs1 rmrs2))
  nil)

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
   (declare (ignore comp rmrs1 rmrs2))
  nil) 

#|
    (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
	do
	  (progn
	    (pushnew (rmrs-arg-label rmrs-arg) 
		     labels :test #'eql-var-id)
	    (let ((value (rmrs-arg-val rmrs-arg)))
	      (if (is-handel-var value)
		  (pushnew value holes :test #'eql-var-id)
		(if (var-p value) 
		    (ecase (rmrs-origin rmrs) 
		      (:erg
		       (pushnew value distinguished 
				:test #'eql-var-id))
		      (:rasp 
		       (pushnew value undistinguished 
				  :test #'eql-var-id))))))))

|#

;;; Checking in-groups
;;;
;;; Canonicalised in-groups
;;;

(defun add-ing-compare (comp rmrs1 rmrs2)
   (declare (ignore comp rmrs1 rmrs2))
  nil) 
  
  
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
	       hcons-matches))))))
    (setf (rmrs-comparison-record-matched-hcons comp)
      hcons-matches)
    hcons-matches))


