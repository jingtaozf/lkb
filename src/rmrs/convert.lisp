(in-package :mrs)

;;; convert a (simple) MRS structure to an RMRS  

#|

the most complicated bit of this is the Parsonification -
this takes a relation of the normal form
[ rel
  ARG0 e
  ARG1 x
  ARG2 x1 ]

and converts it to

[ rel e ], [ ARG1 x ], [ ARG2 y ]

The conversion is controlled by the type of the relation - this
might not be done for PPs, for instance.  This code is somewhat
grammar-specific.

|#

;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)

(defun batch-output-rmrs nil
  ;;; to be called from LKB batch processing
  (let ((sentence lkb::*sentence*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%<sentence>")
    (format ostream
            "~%<s>~%~A~%</s>" sentence)
    (loop for parse in *parse-record*
        do
          (let* ((mrs-struct (extract-mrs parse))
                 (rmrs-struct (simple-mrs-to-rmrs mrs-struct)))
            (output-rmrs1 rmrs-struct 'xml ostream)))
    (format ostream "~%</sentence>")
    (finish-output ostream)))
  



(defun simple-mrs-to-rmrs (mrs)
  (let ((lzt (psoa-liszt mrs)))
    (make-rmrs :eps
               (loop for rel in lzt
                     append
                     (parsonify-rel rel)))))

#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps, in parsons style.
The conversion should only be done for relations which are of the correct 
type.
|#

(defun parsonify-rel (rel)
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




