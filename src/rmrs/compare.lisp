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

(defstruct (comp-rel (:include char-rel)) 
  comp-status)

(defstruct (comp-arg (:include rmrs-arg))
  comp-status)

(defstruct comparison-record
  label-list
  var-list)
  
#|
(setf *rasp-eg5*
  (nth 4
       (read-rmrs-file "xxx" :rasp)))

(setf *erg-eg5*
  (progn (let ((*mrs-output-p* t))
	   (do-parse-tty "Abrams handed Browne the cigarette")
	   (mrs-to-rmrs *mrs-debug*))))
|#

(defun compare-rmrs (rmrs1 rmrs2 same-source-p)
  (unless (and (rmrs-p rmrs1) (rmrs-p rmrs2))
    (error "Arguments to compare-rmrs are not valid RMRSs"))
  (compare-rmrs-aux (convert-to-comparison-rmrs 
		     (sort-rmrs rmrs1 same-source-p))
		    ;; turns the rmrs into comp-rel and comp-args
		    (convert-to-comparison-rmrs 
		     (sort-rmrs rmrs2 same-source-p))
		    (initial-comparison-record)))

(defun initial-comparison-record nil
  nil)

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
#|

(defun sort-rmrs (rmrs same-source-p)
  (let* ((liszt (rmrs-liszt rmrs))
	 (new-liszt 
	  (combine-similar-relations liszt nil FIX)))
    ;;; combine-similar-relations is in mrs/mrscorpus.lisp
  (if same-source-p rmrs
    (let ((sorted-liszt (sort (rmrs-liszt rmrs) #'< 
			      :key #'char-rel-cfrom)))
      (setf (rmrs-liszt rmrs)
	(merge-equal-elements sorted-liszt #'(lambda (x y)
					       (eql (char-rel-cfrom x)
						    (char-rel-cfrom y))))
      rmrs)
      ;;; else - FIX
    rmrs))))

|#

(defun rmrs-rel-sort-same-source-eql (rel1 rel2)
  (let ((cfrom1 (char-rel-cfrom rel1))
	(cfrom2 (char-rel-cfrom rel2)))
    (and (eql cfrom1 cfrom2)
	 (rmrs-rel-sort-eql rel1 rel2))))


(defun rmrs-rel-sort-eql (rel1 rel2)
)


  
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
	 (undistinguished nil)
	 (count 0)
	 (elpreds
	  (loop for ep in (rmrs-liszt rmrs)
	      collect
		(progn
		(incf count) ;;; temporary hack
		  (pushnew (rel-handel ep) labels 
			   :test #'eql-var-id)
		  (let ((ep-var (retrieve-rmrs-ep-var ep)))
		    (if (or (eql (rmrs-origin rmrs) :erg)
			    (distinguished-rel-type-p ep))
			(pushnew ep-var distinguished
				 :test #'eql-var-id)
		      (pushnew ep-var undistinguished
			       :test #'eql-var-id))
		    (make-comp-rel :handel (rel-handel ep)
				   :pred (rel-pred ep)
				   :flist (rel-flist ep)
				   :cfrom count
				   :parameter-strings
				   (get-carg-value
				    ep
				    (rmrs-rmrs-args rmrs))))))))
      (dolist (qeq (rmrs-h-cons rmrs))
	(cond
	  ((equal (hcons-relation qeq) "qeq")
	   (pushnew (hcons-scarg qeq) holes 
		    :test #'eql-var-id)
	   (pushnew (hcons-outscpd qeq) labels 
		    :test #'eql-var-id))
	  (t (error "Unsupported hcons relation ~A"  (hcons-relation qeq)))))
      (let ((new-rmrs-args
	     (loop for rmrs-arg in (rmrs-rmrs-args rmrs)
		 collect
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
				       :test #'eql-var-id)))))
		     (make-comp-arg
		      :arg-type (rmrs-arg-arg-type rmrs-arg)
		      :label (rmrs-arg-label rmrs-arg)
		      :val value))))))
	(make-comp-rmrs
	 :top-h (rmrs-top-h rmrs)
	 :liszt elpreds
	 :h-cons (rmrs-h-cons rmrs)
	 :rmrs-args new-rmrs-args
	 :in-groups (rmrs-in-groups rmrs)
	 :bindings nil ;;; should already be canonicalized??
	 :cfrom (rmrs-cfrom rmrs)
	 :cto (rmrs-cto rmrs)
	 :origin (rmrs-origin rmrs)
	 :labels labels
	 :holes holes
	 :distinguished distinguished
	 :undistinguished undistinguished))))

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


(defun compare-rmrs-aux (rmrs1 rmrs2 comp-record)
  ;;; FIX needs much more!
  (compare-rmrs-liszts (rmrs-liszt rmrs1) (rmrs-liszt rmrs2) comp-record))

(defun compare-rmrs-liszts (l1 l2 comp-record)
  ;;; these should eventually be lists of comparison sets
  ;;; but for now just lists of rels
  (if (and l1 l2)
      (let ((first1 (car l1))
	    (first2 (car l2)))
	(cond ((greater-sort-rels first1 first2)
	       (compare-rmrs-liszts first1 (cdr l2) comp-record))
	      ((greater-sort-rels first2 first1)
	       (compare-rmrs-liszts (cdr l1) first2 comp-record))
	      (t (let ((comp-records (compare-rmrs-rel-set 
				      first1 first2 comp-record)))
		   (loop for alternative in comp-records
		       do
			 (compare-rmrs-liszts (cdr l1) (cdr l2) 
					      alternative))))))
    (list comp-record)))
  
(defun compare-rmrs-rel-set (s1 s2 comp-record)
  ;;; FIX when these are sets
  (let ((match (compare-rmrs-rels s1 s2 comp-record)))
    (or match comp-record)))

(defun compare-rmrs-rels (rel1 rel2 comparison-record)
  (let ((pred-comparison (compare-rmrs-preds 
			  (rel-pred rel1)
			  (rel-pred rel2))))
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
	      (progn
		(setf (comp-rel-comp-status rel1) pred-comparison)
		(setf (comp-rel-comp-status rel2) pred-comparison)
		var-match-record)
	    ;;; else - variable match is impossible
	    nil))
      ;;; else - predicates don't match
      nil)))
      

     

(defun compare-rmrs-preds (pred1 pred2)
  ;;; a gram pred can never to equal to a non-gram pred
  (cond ((and (realpred-p pred1) 
	      (realpred-p pred2))
	 (compare-rmrs-real-preds pred1 pred2))
	((realpred-p pred1) nil)
	((realpred-p pred2) nil)
	((equal pred1 pred2) :equal)
	(t nil)))

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

#|

(defun compare-rmrs-vars (var1 var2 comparison-record)
  (let ((extra-comparison (compare-rmrs-extras (var-extra var1)
					       (var-extra var2))))
    (if extra-comparison
	(if (not-eq-vars (var-id var1) 
			 (var-id var2))
	    ))))

|#