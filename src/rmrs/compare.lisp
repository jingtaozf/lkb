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
       (read-rmrs-file "xxx")))

(setf *erg-eg5*
  (progn (let ((*mrs-output-p* t))
	   (do-parse-tty "Abrams handed Browne the cigarette")
	   (mrs-to-rmrs *mrs-debug*))))
|#

(defparameter *inequalities1* nil)

(defparameter *inequalities2* nil)



(defun compare-rmrs (rmrs1 rmrs2 same-source-p)
  )

(defun compare-rmrs-rels (rel1 rel2 comparison-record)
  (let ((pred-comparison (compare-rmrs-preds 
			  (rel-pred rel1)
			  (rel-pred rel2))))
    (if pred-comparison
      (progn
	(setf (comp-rel-comp-status rel1)
	  (translate-comparison1 pred-comparison))
	(setf (comp-rel-comp-status rel2)
	  (translate-comparison2 pred-comparison))
	comparison-record)
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

(defun compare-rmrs-vars (var1 var2 comparison-record)
  (let ((extra-comparison (compare-rmrs-extras (var-extra var1)
					       (var-extra var2))))
    (if extra-comparison
	(if (not-eq-vars (var-id var1) 
			 (var-id var2))
	    