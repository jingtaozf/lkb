;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; convert an MRS structure to an RMRS  

;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)
  
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
                 :rmrs-args new-args)))


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
                                    (eql (var-type val) 'lkb::non_expl))
                               (member (string feat) *rmrs-ignore-features*
                                       :test #'equal))
                        (list (make-rmrs-arg 
                               :arg-type (string feat)
                               :label new-label
                               :val val)))))))
         (ep 
          (make-rel
           :handel new-label
           :parameter-strings (rel-parameter-strings rel)
           :extra (rel-extra rel)
           :pred pred 
           :flist (list main-arg)))
         (in-group (if (member (var-id label) labels)
                       (make-in-group :labels (list label new-label)))))
    (values ep rmrs-args in-group
            (cons (var-id new-label) labels))))


(defparameter *do-not-convert-preds* '("_cop_id_rel"))

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
    (if (or (not (eql (elt str 0) #\_))
         (member str *do-not-convert-preds* :test #'string-equal))
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
            str
          (make-realpred :lemma (subseq str 1 uscore-pos2)
                         :pos (subseq str (+ 1 uscore-pos2) uscore-pos3)
                         :sense (if uscore-pos4
                                   (subseq str (+ 1 uscore-pos3) uscore-pos4))))))))


               

