(in-package :mrs)

;;; convert an MRS structure to an RMRS  

;;; simple version currently broken ...

;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)

(defun batch-output-simple-rmrs nil
  ;;; to be called from LKB batch processing
  (batch-output-rmrs-aux t))

(defun batch-output-rmrs nil
  ;;; to be called from LKB batch processing
  (batch-output-rmrs-aux nil))
  
(defun batch-output-rmrs-aux (simple-p)  
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
                  (if simple-p
                      (simple-mrs-to-rmrs mrs-struct)
                    (mrs-to-rmrs mrs-struct))))
            (setf *test-mrs* mrs-struct)
            (setf *test-rmrs* rmrs-struct)
            (output-rmrs1 rmrs-struct 'xml ostream))
      (format ostream
              "~%<rmrs></rmrs>"))
    (format ostream "</S>~%")
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
            (setf new-args (append rmrs-args new-args))
            (if in-group
                (setf new-in-groups (cons in-group new-in-groups)))
            (setf labels label-list)))
    (make-rmrs   :h-cons (psoa-h-cons mrs)
                 :liszt new-lzt
                 :in-groups new-in-groups
                 :rmrs-args new-args)))


#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps plus rmrs-args
|#

(defparameter *rmrs-ignore-features* '("DIM"))

(defun parsonify-rel (rel labels)
  (let* ((reltype (rel-reltype rel))
         (pred (rmrs-convert-pred (rel-sort rel)))
         (flist (rel-flist rel))
         (main-arg (fvpair-value (car flist)))
         (label (rel-handel rel))
         (new-label (if (member (var-id label) labels) ;; conjunction
                        (create-new-rmrs-var *rmrs-variable-generator* :handle)
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
           :reltype reltype
           :sort pred 
           :flist (list main-arg)))
         (in-group (if (member (var-id label) labels)
                       (make-in-group :labels (list label new-label)))))
    (values ep rmrs-args in-group
            (cons (var-id new-label) labels))))


(defparameter *do-not-convert-preds* '("_cop_id_rel"))

(defun rmrs-convert-pred (pred)
  (let* ((pos-type (find-pred-pos-type pred))
        (str (string-downcase (string pred))))
    ;;; parse the string - hacky ...
    (if (or (not (eql (elt str 0) #\_))
         (member str *do-not-convert-preds* :test #'string-equal))
        str
      (let*
          ((uscore-pos (position #\_ str :start 1))
           (uscore-pos2 
            (if uscore-pos
                (position #\_ str :start (+ 1 uscore-pos))))
           (remainder (cond (uscore-pos2
                             (subseq str uscore-pos2))
                            (uscore-pos
                             (subseq str uscore-pos))
                            (t nil))))
        (if (not (equal remainder "_rel"))
              ;;; we're missing the _rel
              ;;; nasty so just output what we've got
            str
          (concatenate 'string
            (subseq str 0 uscore-pos)
            pos-type
            (strip-pos-type (subseq str uscore-pos) pos-type)))))))

(defun find-pred-pos-type (pred)
  (cond ((not (lkb::is-valid-type pred)) "")
        ((lkb::subtype-p pred 'LKB::basic_NOM_REL) "_N")
        ((lkb::subtype-p pred 'LKB::adj_rel) "_J")
        ((lkb::subtype-p pred 'LKB::adv_rel) "_R")
        ((lkb::subtype-p pred 'LKB::prep_rel) "")
        ((lkb::subtype-p pred 'LKB::event_rel) "_V")
        (t "")))

(defun strip-pos-type (str pos-type)
  (if (eql (elt str 2) #\_)
      (let ((candidate-pos (subseq str 0 2)))
        (if (string-equal candidate-pos pos-type)
            (subseq str 2)
          str))
    str))
               

;;; simple MRS (currently broken, due to adding the full MRS)



(defun simple-mrs-to-rmrs (mrs)
  (let ((lzt (psoa-liszt mrs)))
    (make-rmrs :eps
               (loop for rel in lzt
                     append
                     (simple-parsonify-rel rel)))))

#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps, in parsons style.
The conversion should only be done for relations which are of the correct 
type.
|#

(defun simple-parsonify-rel (rel)
  (let* ((reltype (rel-reltype rel))
         (pred (rel-sort rel))
         (flist (rel-flist rel))
         (main-arg (fvpair-value (car flist)))
         (parsons-eps
          (if (and (cdr flist)
                   (parsonifiable-type-p reltype))
              (loop for fvpair in (cdr flist)
                  collect
                    (let ((feat (fvpair-feature fvpair))
                     (val (fvpair-value fvpair)))
                      (make-ep :sort (string feat)
                               :flist 
                               (list main-arg val)))))))
    (cons
     (make-ep :sort pred 
              :flist (if parsons-eps 
                         (list main-arg) 
                       (loop for fvpair in flist
                           collect (fvpair-value fvpair))))
     parsons-eps)))


(defun parsonifiable-type-p (reltype)
  (declare (ignore reltype))
  ;;; needs to return t for rels with more than
  ;;; one arg which are to be split up
  t)

