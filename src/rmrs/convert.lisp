;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; convert an MRS structure to an RMRS  

;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)

(defparameter *mrs-to-rmrs-conversion-warnings* nil)

(defun warn-rmrs-problem (str)
#+:lkb  (push lkb::*parse-input* *mrs-to-rmrs-conversion-warnings*)
  (push str *mrs-to-rmrs-conversion-warnings*))

#+:lkb
(defun batch-output-rmrs nil
  (let ((sentence lkb::*parse-input*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%<S>")
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
  (let ((lzt (psoa-liszt mrs))
        (new-lzt nil)
        (new-args nil)
        (new-in-groups nil)
        (labels nil))
    (dolist (rel lzt)
          (multiple-value-bind (ep rmrs-args in-group label-list)
            (parsonify-rel rel labels)
            (push ep new-lzt)
            (setf new-args (append new-args rmrs-args))
            (when in-group
                (push in-group new-in-groups))
            (setf labels label-list)))
    (make-rmrs   :top-h (psoa-top-h mrs)
                 :h-cons (psoa-h-cons mrs)
                 :liszt (nreverse new-lzt)
                 :in-groups (nreverse new-in-groups)
                 :rmrs-args new-args
		 :origin :erg)))


#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps plus rmrs-args
|#

(defparameter *rmrs-ignore-features* '("DIM"))

(defun parsonify-rel (rel labels)
  (let* ((pred (rmrs-convert-pred (rel-pred rel)))
         (flist (rel-flist rel))
	 (main-arg (fvpair-value (car flist)))
         (converted-main-arg (if (var-p main-arg)
				 (rmrs-convert-variable main-arg)
			       (progn (warn-rmrs-problem 
				       (format nil "~A as main argument" main-arg))
				      main-arg)))
         (label (rel-handel rel))
         (new-label (if (member (var-id label) labels) ;; conjunction
                        (create-new-rmrs-var 
			 :handle *rmrs-variable-generator* nil)
                        label))
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
                               :label new-label
                               :val (if (var-p val)
					(rmrs-convert-variable val)
				      val))))))))
         (ep 
          (make-char-rel
           :handel new-label
           :parameter-strings (rel-parameter-strings rel)
           :extra (rel-extra rel)
           :pred pred 
           :flist (list converted-main-arg)
	   :cfrom (if (char-rel-p rel)
		      (char-rel-cfrom rel))
	   :cto (if (char-rel-p rel)
		      (char-rel-cto rel))))
         (in-group (if (member (var-id label) labels)
                       (make-in-group :labels (list label new-label)))))
    (values ep rmrs-args in-group
            (cons (var-id new-label) labels))))

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
  (make-var :type (var-type var)
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
 
 




