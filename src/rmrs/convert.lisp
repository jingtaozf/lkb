;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; convert an MRS structure to an RMRS  

;;; settings for qa
;;; (defparameter *unknown-word-types* '(n_proper_le))
;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)

(defparameter *mrs-to-rmrs-conversion-warnings* nil)

(defun warn-rmrs-problem (str)
  #+:lkb  (push lkb::*parse-input* *mrs-to-rmrs-conversion-warnings*)
  (push str *mrs-to-rmrs-conversion-warnings*))

(defparameter *qa-count* 1)

#+:lkb
(defun batch-output-rmrs nil
  (let ((sentence lkb::*parse-input*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%<S id='~A'>" *qa-count*)
    (setf *qa-count* (+ 1 *qa-count*))
    (if sentence
        (format ostream
                "~%<string>~%~S~%</string>" sentence)
      (format ostream
              "~%<string></string>"))
    (format ostream
            "~%<tree></tree>")
    ;;; for rasp output compatibility
    (if *parse-record*
          (let* ((parse (car *parse-record*))
                 (mrs-struct (extract-mrs parse))
                 (rmrs-struct 
                    (mrs-to-rmrs mrs-struct)))
            (output-rmrs1 rmrs-struct 'xml ostream))
      (format ostream
              "~%<rmrs></rmrs>"))
    (format ostream "</S>~%")
    (finish-output ostream)))

#+:lkb
(defun batch-output-rmrs-only nil  
  (let ((ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (if *parse-record*
          (let* ((parse (car *parse-record*))
                 (mrs-struct (extract-mrs parse))
                 (rmrs-struct 
                    (mrs-to-rmrs mrs-struct)))
            (output-rmrs1 rmrs-struct 'xml ostream))
      (format ostream
              "~%<rmrs></rmrs>"))
    (finish-output ostream)))


;;; Full MRS to RMRS conversion

(defun mrs-to-rmrs (mrs)
  (initialize-rmrs-variables-plus)
  (let ((lzt (psoa-liszt mrs))
        (new-lzt nil)
        (new-args nil)
        (label-recs nil))
    (dolist (rel lzt)
          (multiple-value-bind (ep rmrs-args new-label-recs)
            (parsonify-rel rel label-recs)
            (push ep new-lzt)
            (setf new-args (append new-args rmrs-args))
            (setf label-recs new-label-recs)))
    (make-rmrs   :top-h (psoa-top-h mrs)
                 :h-cons (psoa-h-cons mrs)
                 :liszt (nreverse new-lzt)
                 :in-groups (construct-converted-in-groups label-recs) 
                 :rmrs-args new-args
		 :origin :erg)))



#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps plus rmrs-args
|#

(defparameter *rmrs-ignore-features* '("DIM"))

(defstruct ing-info
  label type new-label-type-pairs)

(defun parsonify-rel (rel label-recs)
  ;;; label-recs is a list of ing-info structures
  ;;; so that we can have a canonical notion of in-group
  (let* ((pred (rmrs-convert-pred (rel-pred rel)))
	 (rel-type (determine-ing-rel-type pred)) 
         (flist (rel-flist rel))
	 (main-arg (fvpair-value (car flist)))
         (converted-main-arg (if (var-p main-arg)
				 (rmrs-convert-variable main-arg)
			       (progn (warn-rmrs-problem 
				       (format nil "~A as main argument" main-arg))
				      main-arg)))
         (label (rel-handel rel))
	 (label-id (var-id label))
	 (label-match (dolist (label-rec label-recs)
			(when (eql label-id 
				   (var-id (ing-info-label label-rec)))
			  (return label-rec))))
         (new-label (if label-match ;; conjunction
                        (create-new-rmrs-var 
			 "h" *rmrs-variable-generator* nil)))
         (rmrs-args
          (if (cdr flist)
              (loop for fvpair in (cdr flist)
                  nconc
                    (let ((feat (fvpair-feature fvpair))
                          (val (fvpair-value fvpair)))
                      (unless (or 
                               (and (var-p val) 
                                    (equal (var-type val) "u"))
			       ;;; remove any optional arguments
                               (member (string feat) *rmrs-ignore-features*
                                       :test #'equal))
                        (list (make-rmrs-arg 
                               :arg-type (string feat)
                               :label (or new-label label)
                               :val (if (var-p val)
					(rmrs-convert-variable val)
				      val))))))))
         (ep 
          (make-char-rel
           :handel (or new-label label)
           :parameter-strings (rel-parameter-strings rel)
           :extra (rel-extra rel)
           :pred pred 
           :flist (list converted-main-arg)
	   :cfrom (if (char-rel-p rel)
		      (char-rel-cfrom rel))
	   :cto (if (char-rel-p rel)
		    (char-rel-cto rel)))))
    (values ep rmrs-args 
	    (record-ing-info label-recs 
			     label new-label rel-type label-match))))

(defun record-ing-info (label-recs label new-label rel-type
			      label-match)
  (if label-match
      (progn
	(push (cons new-label rel-type) 
	      (ing-info-new-label-type-pairs label-match))
	label-recs)
    (cons (make-ing-info :label label :type rel-type)
	  label-recs)))

(defun construct-converted-in-groups (label-recs)
  (let ((ings nil))
    (dolist (label-rec label-recs)
      (let ((matches (ing-info-new-label-type-pairs label-rec)))
	(when matches
	  (let* ((var-pairs (cons (cons (ing-info-label label-rec)
				       (ing-info-type label-rec))
				 matches))
		(sorted-pairs (sort var-pairs #'ing-rel-type-greater-p 
				    :key #'cdr))
		 (top-rank-var (caar sorted-pairs)))
	    (dolist (pair (cdr sorted-pairs))
	      (push
		  (make-in-group :label-a top-rank-var
				 :label-b (car pair))
		  ings))))))
    ings))



  
(defun rmrs-convert-pred (pred)
  ;;; the pred should obey the format:
  ;;; _lemma_pos_sense_rel
  ;;; or
  ;;; _lemma_pos_rel
  ;;; if the senses are not distinguished
  ;;;
  ;;; If there is no leading underscore, the pred
  ;;; is not decomposed
  (let* ((str (string-downcase (string pred))))
    ;;; parse the string - hacky ...
    (if (not (eql (elt str 0) #\_))
        str
      (let*
          ((uscore-pos2 (position #\_ str :start 1))
           (uscore-pos3 
            (if uscore-pos2
                (position #\_ str :start (+ 1 uscore-pos2))))
           (uscore-pos4
            (if uscore-pos3
                (position #\_ str :start (+ 1 uscore-pos3))))
           (remainder (cond (uscore-pos4
                             (subseq str uscore-pos4))
                            (uscore-pos3
                             (subseq str uscore-pos3))
                            (t nil))))
        (if (not (equal remainder "_rel"))
              ;;; we're missing the _rel
              ;;; nasty so just output what we've got
	    (progn (warn-rmrs-problem str)
		   str)
          (make-realpred :lemma (subseq str 1 uscore-pos2)
                         :pos (subseq str (+ 1 uscore-pos2) uscore-pos3)
                         :sense (if uscore-pos4
                                   (subseq str (+ 1 uscore-pos3) uscore-pos4))))))))





(defun rmrs-convert-variable (var)
  (make-var :type (if (member (var-type var) '("x" "e" "h" "u" "l")
                              :test #'equal)
                      (var-type var)
                    "u")
            :id (var-id var)))
	    
;;;	    :extra (rmrs-convert-var-extra (var-extra var))))

;;; conversion of extra values is going to be grammar specific
;;; and there's no guarantee that it can be done one-to-one
;;;
;;; for now, code below works for current ERG - rationalize 
;;; this when we've got a better idea of what's going on

#|

(defparameter *var-extra-conversion-table-simple*
'(
 (divisible . divisible)
 (e.aspect.perf . refdistinct)
 (e.aspect.progr . imr)
 (e.tense . tense)))

(defparameter *var-extra-conversion-table-complex*
'(
 ((png.gen fem) . (gender f))
 ((png.gen masc) . (gender m))
 ((png.gen andro) . (gender m-or-f))
 ((png.gen neut) . (gender n))
 ((png.pn 3sg) . (pers 3))
 ((png.pn 3sg) .  (num sg))
 ((png.pn 3pl) . (pers 3))
 ((png.pn 3pl) . (num pl))
 ((png.pn 2per) .  (pers 2))
 ((png.pn 1pl) . (pers 1))
 ((png.pn 1pl) . (num pl))
 ((png.pn 1sg) . (pers 1))
 ((png.pn 1sg) . (num sg))
 ))


(defun rmrs-convert-var-extra (extras)
  (let ((converted nil))
    (dolist (extra extras)
      (let* ((feat (extrapair-feature extra))
	     (val (extrapair-value extra))
	     (simple-transfer
	      (assoc feat *var-extra-conversion-table-simple*)))
	(if simple-transfer
	    (push (make-extrapair :feature 
				  (cdr simple-transfer)
				  :value val)
		  converted)
	  (if (and (member feat *var-extra-conversion-table-complex*
			   :key #'caar)
		   )))))))
			   
|#		  
 

;;; **************************************************
;;; ING handling
;;; **************************************************

#|

It is convenient to have a notion of a canonical set of IN-Gs
This can be regarded in the same way as the set of qeqs - although
different groups of qeqs might semantically result in the same thing,
if we assume consistent composition principles, we'll get the same set
of qeqs for comparable strings.  The same is true of IN-Gs - although
semantically they (probably) just correspond to conjunction, we
can compare much more efficiently if we assume rules about how they are 
constructed.  This also allow us to play games with underspecification
and possibly change the interpretation of IN-Gs, but this is ignored for
now.

The assumption is that whenever an IN-G is created, it corresponds to
a situation where one label can be considered `higher'.
Higher could be defined in several ways (and I think it's really arbitrary), 
but let's assume that the modifiee is always `higher' 
(this makes sense because it may have several modifiers)
and that a semantic head is always higher otherwise.				This is a bit complicated here in the context of conversion from the ERG
because we don't know what the composition rules are, so we have to
try and simulate on the basis of which label comes from a more `major' relation.
Code here allows record of this for debugging.  Main problem is working
out the hierarchy on grammar preds.  However, in practice we can 
limit this to the cases which occur in the RASP-RMRS code for now.

Errors won't be devastating anyway ...
|#

(defun determine-ing-rel-type (pred)
  (if (realpred-p pred)
      (realpred-pos pred)
    pred))

;;; FIX to treat grammar preds
;;; probably by reference to type hierarchy

(defparameter *ing-ranking*
    '(("n" "v" "j" "p" "prpstn_m_rel" "poss_rel")
      ("v" "p" "r")))
      

(defun ing-rel-type-greater-p (type1 type2)
  (let* ((outrank-rec (assoc type1 *ing-ranking* :test #'equal))
	 (outrank-p
	  (and outrank-rec
	       (member type2 (cdr outrank-rec) :test #'equal))))
;;;    (format t "~%Outranks ~A ~A: ~A" type1 type2 outrank-p)
    outrank-p))





