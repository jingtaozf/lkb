(in-package "MRS")

#| Robust comparison of RMRSs

There are two somewhat different situations to consider:
1) the RMRSs come from different sentences and we're trying to see
how close they are e.g. question answering
2) the RMRSs come from the same text

For now, attempt to use the same code for both situations.

The idea is to record the RMRS results on the elements,
using a comp-status slot.  This should enable
display and scoring of differences.

The backbone of the comparison is the eps, especially the
ones from real words, especially the ones from open class words.

Should be possible to reuse this as MRS code
|#

(defstruct (comp-rmrs (:include rmrs)) 
  labels holes distinguished undistinguished)

;;; the extra slots here are for caching the variables
;;; distinguished vs undistinguished is explained below

(defstruct rmrs-comparison-record
  matched-rels
  label-list
  var-list)

(defstruct match-rel-record
  rel1
  rel2
  comp-status)

;;; Main entry point - same-source-p is t if we want
;;; to use the character position information 

(defun compare-rmrs (rmrs1 rmrs2 same-source-p input-string)
  (declare (ignore input-string))
  ;;; returns a list of comparison records 
  (unless (and (rmrs-p rmrs1) (rmrs-p rmrs2))
    (error "Arguments to compare-rmrs are not valid RMRSs"))
  (let* ((new-rmrs1 (sort-rmrs (convert-to-comparison-rmrs rmrs1)
			  same-source-p))
	 (new-rmrs2 (sort-rmrs (convert-to-comparison-rmrs rmrs2)
			  same-source-p)))
    (compare-rmrs-aux new-rmrs1 new-rmrs2 
		      (initial-comparison-record)
		      same-source-p)))


(defun initial-comparison-record nil
  (make-rmrs-comparison-record 
   :matched-rels nil
   :label-list nil
   :var-list nil))

#|
sorting

If the source is the same, this is assumed to have some reflection
on linear order of rels and the sorting reflects that linear order
(via characterization).  As a secondary sorting measure,
the rels are sorted by name (see pred comparison notes, below)

In the case where the source is not the same, the rels are sorted
into a canonical order (alphabetical order on predicate name,
followed by alphabetical order on any CARGs)
In the case of two relations with the same sortal order,
these will go into the same comparison set - this may necessitate
backtracking.

Because this code allows for subsumption, we're in a different
position than with MRS equality checking as regards the notion
of a canonical order.  If this were fully general, i.e.,
if we allowed for any predicate to subsume any other,
we would have a big problem ... As it is, we make the assumption
that real-preds and grampreds are disjoint, that real-pred
subsumption is simply as indicated by the lemma/pos/sense 
conventions, and that grampreds are always treated as secondary.
grampreds may be in a subsumption hierarchy with respect to 
eachother, but we aren't going to bother about grampreds
in RMRSs from different sources, unless we have some matching 
realpreds.

|#


(defun sort-rmrs (rmrs same-source-p)
  (let* ((liszt (rmrs-liszt rmrs))
	 (new-liszt 
	  (combine-similar-relations liszt nil 
				     (if same-source-p
					 #'rmrs-rel-sort-same-source-eql
				       #'rmrs-rel-sort-eql))))
    ;;; combine-similar-relations is in mrs/mrscorpus.lisp
    (let ((sorted-liszt 
	   (sort new-liszt 
		 (if same-source-p
		     #'rmrs-rel-sort-same-source-lesser-p
		   #'rmrs-rel-sort-lesser-p)
		 :key #'car)))
      (setf (rmrs-liszt rmrs)
	sorted-liszt)
      rmrs)))


(defun rmrs-rel-sort-same-source-eql (rel1 rel2)
  (let ((cfrom1 (char-rel-cfrom rel1))
	(cfrom2 (char-rel-cfrom rel2))
	(cto1 (char-rel-cto rel1))
	(cto2 (char-rel-cto rel2)))
    (and (eql cfrom1 cfrom2)
	 (eql cto1 cto2)
	 (rmrs-rel-sort-eql rel1 rel2))))


(defun rmrs-rel-sort-eql (rel1 rel2)
  (compare-rmrs-preds rel1 rel2))
			   

(defun rmrs-rel-sort-same-source-lesser-p (rel1 rel2)
  (let ((cfrom1 (char-rel-cfrom rel1))
	(cfrom2 (char-rel-cfrom rel2))
	(cto1 (char-rel-cto rel1))
	(cto2 (char-rel-cto rel2)))
    (or (< cfrom1 cfrom2)
	(and (eql cfrom1 cfrom2) 
	    (or (< cto1 cto2)
		(and (eql cfrom1 cfrom2)
		    (rmrs-rel-sort-lesser-p rel1 rel2)))))))

(defun rmrs-rel-sort-lesser-p (rel1 rel2)
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
  ;;; returns t if 
  ;;; pred1 is alphabetically less than pred2
  ;;; or if they are equal non realpreds and the parameter 
  ;;;                                     string is less
  ;;; or if pred1 is a real-pred and pred2 isn't
    (if (realpred-p pred1)
	(if (not (realpred-p pred2))
	    t
	  (rmrs-real-pred-lesser-p pred1 pred2))
    ;;; else pred1 isn't a realpred
      (if (realpred-p pred2)
	  nil
	(if (equal pred1 pred2)
	    (let ((str1 (rel-parameter-strings rel1))
		  (str2 (rel-parameter-strings rel2)))
	      (if (and str1 str2)
		  (string-lessp str1 str2)
		  nil)) ;; bit arbitrary ...
	  nil))))) ;;; we don't want to order two grammar preds
                   ;;; because one might subsume the other

(defun rmrs-real-pred-lesser-p (pred1 pred2)
  ;;; note that this ignores pos tages and senses deliberately
  (string-lessp (realpred-lemma pred1)
		(realpred-lemma pred2)))


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

;;; Variables are extracted during the pass over the RMRS
;;; structure which converts it to a comp-rmrs
;;; This code also puts any CARG values into parameter-strings
;;; --- CARGS can subsequently be ignored

(defun convert-to-comparison-rmrs (rmrs)
  (let* ((holes (list (rmrs-top-h rmrs)))
	 (labels nil)
	 (distinguished nil)
	 (undistinguished nil))
    (loop for ep in (rmrs-liszt rmrs)
	do
	  (progn
	    (pushnew (rel-handel ep) labels 
		     :test #'eql-var-id)
	    (let ((ep-var (retrieve-rmrs-ep-var ep)))
	      (if (or (eql (rmrs-origin rmrs) :erg)
		      (distinguished-rel-type-p ep))
		  (pushnew ep-var distinguished
			   :test #'eql-var-id)
		(pushnew ep-var undistinguished
			 :test #'eql-var-id))
	      (setf (rel-parameter-strings ep)
		(get-carg-value
		 ep
		 (rmrs-rmrs-args rmrs))))))
      (dolist (qeq (rmrs-h-cons rmrs))
	(cond
	  ((equal (hcons-relation qeq) "qeq")
	   (pushnew (hcons-scarg qeq) holes 
		    :test #'eql-var-id)
	   (pushnew (hcons-outscpd qeq) labels 
		    :test #'eql-var-id))
	  (t (error "Unsupported hcons relation ~A"  (hcons-relation qeq)))))
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
	(make-comp-rmrs
	 :top-h (rmrs-top-h rmrs)
	 :liszt (rmrs-liszt rmrs)
	 :h-cons (rmrs-h-cons rmrs)
	 :rmrs-args (rmrs-rmrs-args rmrs)
	 :in-groups (rmrs-in-groups rmrs)
	 :bindings nil ;;; should already be canonicalized??
	 :cfrom (rmrs-cfrom rmrs)
	 :cto (rmrs-cto rmrs)
	 :origin (rmrs-origin rmrs)
	 :labels labels
	 :holes holes
	 :distinguished distinguished
	 :undistinguished undistinguished)))

(defun get-carg-value (rel rmrs-args)
  (let ((lbl (rel-handel rel)))
    (dolist (arg rmrs-args)
      (when (and (eql-var-id lbl 
			     (rmrs-arg-label arg))
		 (equal (rmrs-arg-arg-type arg)
			"CARG"))
	(return (rmrs-arg-val arg))))))
      

(defun distinguished-rel-type-p (rel)
  (let ((pred (rel-pred rel)))
    (or (equal pred "named_rel")
	(and (realpred-p pred)
	 (or (equal (realpred-pos pred) "n")
	     (equal (realpred-pos pred) "v"))))))


(defun compare-rmrs-aux (rmrs1 rmrs2 comp-record same-source-p)
  ;;; FIX needs much more!
  (compare-rmrs-liszts (rmrs-liszt rmrs1) (rmrs-liszt rmrs2) 
		       comp-record same-source-p))

(defun compare-rmrs-liszts (l1 l2 comp-record same-source-p)
  (if (and l1 l2)
      (let ((first1 (caar l1))
	    (first2 (caar l2)))
	(cond ((if same-source-p
		   (rmrs-rel-sort-same-source-lesser-p first2 first1)
		 (rmrs-rel-sort-lesser-p first2 first1))
	       (compare-rmrs-liszts l1 (cdr l2) comp-record same-source-p))
	      ((if same-source-p
		   (rmrs-rel-sort-same-source-lesser-p first1 first2)
		 (rmrs-rel-sort-lesser-p first1 first2))
	       (compare-rmrs-liszts (cdr l1) l2 comp-record same-source-p))
	      (t (let ((comp-records (compare-rmrs-rel-set 
				      (car l1) (car l2) comp-record)))
		   (loop for alternative in comp-records
		       nconc
			 (compare-rmrs-liszts (cdr l1) (cdr l2) 
					      alternative same-source-p))))))
    (list comp-record)))
  
(defun compare-rmrs-rel-set (s1 s2 comp-record)
  ;;; the full set of possibilities here is horrible
  ;;; since in principle if we have {a,b} and {c,d}
  ;;; we should allow for the possibility that if a matches c
  ;;; b might match d if we ignored the a/c match
  ;;; For now, see if we can get away without this since it
  ;;; seems very unlikely that the bindings will interact
  ;;;
  ;;; FIX - cheat altogether for now ... 
  ;;; very unlikely to have non-singleton sets with same-source-p
  ;;; anyway.
  (list (compare-rmrs-rels (car s1) (car s2) comp-record)))

(defun compare-rmrs-rels (rel1 rel2 comparison-record)
  (let ((pred-comparison (compare-rmrs-preds rel1 rel2)))
    (if pred-comparison
	(let ((var-match-record 
	       (let ((label-match (compare-rmrs-labels 
				   (rel-handel rel1)
				   (rel-handel rel2)
				   comparison-record)))
		 (if label-match
		     (let* ((var1 (retrieve-rmrs-ep-var rel1))
			    (var2 (retrieve-rmrs-ep-var rel2)))
		       (cond ((and var1 var2)
			      (compare-rmrs-vars var1 var2 label-match))
			     ((or var1 var2) nil) 
			     ;; this probably shouldn't happen		      
			     (t label-match)))
		   nil))))
	  (if var-match-record
	      (let ((match-record
		     (make-match-rel-record :rel1 rel1
					:rel2 rel2
					:comp-status pred-comparison)))
		(push match-record
		      (rmrs-comparison-record-matched-rels 
		       var-match-record))
		var-match-record)
	    ;;; else - variable match is impossible
	    comparison-record))
      ;;; else - predicates don't match
      comparison-record)))
      

     

(defun compare-rmrs-preds (rel1 rel2)
  ;;; code is used both for comparing rels from different
  ;;; sources and putting rels into equivalence classes
  ;;; initially, though in the latter case we treat as a boolean fn
  (let ((pred1 (rel-pred rel1))
	(pred2 (rel-pred rel2)))
  ;;; a gram pred can never to equal to a non-gram pred
    (cond ((and (realpred-p pred1) 
		(realpred-p pred2))
	   (compare-rmrs-real-preds pred1 pred2))
	  ((realpred-p pred1) nil)
	  ((realpred-p pred2) nil)
	  ((and (equal pred1 pred2) 
		(string-equal (rel-parameter-strings rel1)
			      (rel-parameter-strings rel2)))
	 ;;; string equal returns t if args are both nil
	   :equal)
	  (t nil))))

(defun compare-rmrs-real-preds (pred1 pred2)
  (if (equal (realpred-lemma pred1)
	     (realpred-lemma pred2))
	   (cond ((equal (realpred-pos pred1)
		      (realpred-pos pred2))
		  (cond ((equal (realpred-sense pred1)
				(realpred-sense pred2)) 
			 :equal)
			((null (realpred-sense pred1))
			 :sub1)
			((null (realpred-sense pred2))
			 :sub2)
			(t nil)))
		 ((unknown-rmrs-pos pred1) :sub1)
	         ((unknown-rmrs-pos pred2) :sub2)
		 (t nil))
    nil))

(defun unknown-rmrs-pos (pred)
  (equal (realpred-pos pred)
	 "u"))

(defun compare-rmrs-vars (var1 var2 comparison-record)
  (declare (ignore var1 var2))
  comparison-record)

(defun compare-rmrs-labels (var1 var2 comparison-record)
  (declare (ignore var1 var2))
  comparison-record)

#|

(defun compare-rmrs-vars (var1 var2 comparison-record)
  (let ((extra-comparison (compare-rmrs-extras (var-extra var1)
					       (var-extra var2))))
    (if extra-comparison
	(if (not-eq-vars (var-id var1) 
			 (var-id var2))
	    ))))

|#