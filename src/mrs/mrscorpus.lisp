;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  MRSCORPUS.LISP
;;;
;;;  Module: MRS
;;;  Version: 1.0
;;;  Last Modified: 
;;;  Author: aac
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We want to be able to:
;;; 1. store the parsing results to a file with MRS structures
;;;    Format is
;;;    sentence string
;;;    list of mrs-structures in machine readable form
;;;    commented out human readable mrs-structs
;;; 2. retrieve these results and check for differences when we parse
;;;    the same sentence again
;;; 3. run a corpus of sentences comparing the results with stored results
;;;    outputting the results only if different from those stored
;;; 
;;; 


(IN-PACKAGE "MRS")

(defparameter *mrs-results-table* (make-hash-table :test #'equalp))

(defun write-mrs-results (filename)
  (with-open-file
   (ostream filename :direction :output)
     (maphash #'(lambda (sentence mrs-struct)
                  (format ostream "~S~%" sentence)
                  (format ostream "~S~%" mrs-struct)
                  (output-mrs1 mrs-struct 'comment ostream))
              *mrs-results-table*)))
      



(defun retrieve-mrs-results (file-name)
  (with-open-file
   (istream file-name :direction :input)
   (clrhash *mrs-results-table*)
   (setf *mrs-results-check* t)
   (do* ((sentence (read istream nil 'eof) (read istream nil 'eof ))
         (mrsstruct (read istream nil 'eof) (read istream nil 'eof)))
        ((or (eql sentence 'eof) (eql mrsstruct 'eof)) nil)
        (when (and mrsstruct (psoa-p mrsstruct) (stringp sentence))
              (setf (gethash
                     (remove-trailing-periods sentence) *mrs-results-table*)
                    mrsstruct)))))
  


(defun compare-mrs-struct (sentence mrs-struct stream &optional (comment t))
  (if mrs-struct
      (let ((previous-result (gethash (remove-trailing-periods sentence)
                                      *mrs-results-table*)))
        (if previous-result
            (unless (mrs-equalp mrs-struct previous-result)
                    (format stream "~%~S" sentence)
                    (format stream "~%;;; DIFFERS")
                    (format stream "~%~S" mrs-struct)
                    (if comment
                        (output-mrs1 mrs-struct 'comment stream)))
          (progn
            (format stream "~%~S" sentence)
            (format stream "~%;;; NO PREVIOUS RESULT FOUND")
            (format stream "~%~S" mrs-struct)
            (if comment (output-mrs1 mrs-struct 'comment stream)))))
    (progn
      (format stream "~%~S" sentence)
      (format stream "~%;;; NO MRS CONSTRUCTED")
     (format stream "~%NIL") )))

        


;;; comparing two MRS structures

;;; To some extent, the notion of MRS equality depends on the
;;; function of the calling code.  For checking the result of
;;; parsing against a previous result, we may want a rather rigid notion,
;;; one which ensures that all feature values are instantiated to
;;; the same thing and that the same HCONS constraints are present.
;;; The equality code has to take care of variable numbering
;;; and of treating the LISZT etc as a set, but otherwise a fairly
;;; `syntactic' notion may be appropriate.
;;; 
;;; For generation, the situation is potentially rather different.
;;; Since we are assuming that the preliminary stages have taken care
;;; of ensuring that the predicates (i.e. relation sorts) are maximally
;;; specific, and (possibly) adding in any extra relations, we
;;; still require that the LISZT contain the same relations, with
;;; the same variables (modulo numbering).  However, the values
;;; in `EXTRA' which currently contain essential semantic information,
;;; should be checked for compatability, rather than being identical.
;;; The issue of HCONS is more complex: doing it properly would
;;; require working out all the implications of the constraint, 
;;; which as far as I can see at the moment, requires actually 
;;; doing the scoping.  The current hypothesis is that most of the time
;;; we will be able to get away with a more limited form of compatability
;;; checking: that is, making sure that there is no immediate 
;;; contradiction between the two sets of constraints.  

;;; Current verson of the code for generation totally
;;; ignores HCONS

;;; To facilitate checking, we want a canonical form for the 
;;; set valued features.
;;; We sort the liszt by rel type - if there are
;;; two identical rel types, we further sort by
;;; any value features that may be present
;;; If there are relations which are identical wrt
;;; both relation sort and all value features
;;; we group them together, so the sort returns a list
;;; of lists.  Checking for equality then requires
;;; we allow for the possibility of different binding
;;; sets.

;;; Unfortunately it is possible for two relations not to be
;;; orderable (e.g. two instances of a def_rel)
;;; so we have to return sets in this case, and check
;;; compatability pairwise

(defun sort-mrs-struct-liszt (liszt)
  (let ((new-liszt (combine-similar-relations liszt nil)))
   (sort new-liszt
         #'(lambda (relset1 relset2)
             (let ((rel1 (car relset1)) (rel2 (car relset2)))
               (or (string-lessp (rel-sort rel1) (rel-sort rel2))
                   (and (string-equal (rel-sort rel1) (rel-sort rel2))
                        (value-feats-lessp (rel-flist rel1)
                                           (rel-flist rel2)))))))))

(defun combine-similar-relations (liszt result-so-far)
  (if (null liszt)
      result-so-far
    (let ((test-rel (car liszt))
          (similar nil)
          (non-similar nil))
      (for rel in (cdr liszt)
           do
           (if (similar-relations-p test-rel rel)
               (push rel similar)
             (push rel non-similar)))
      (combine-similar-relations non-similar
                                 (push (cons test-rel similar)
                                      result-so-far))))) 
  
(defun similar-relations-p (rel1 rel2)
  (and (string-equal (rel-sort rel1) (rel-sort rel2))
       (let ((fv1 (rel-flist rel1))
             (fv2 (rel-flist rel2)))
         (if (eql (length fv1) (length fv2))
                         ;;; assumes canonical feature ordering
             (loop for fvpair1 in fv1
                 as fvpair2 in fv2
                 always 
                   (and (eql (fvpair-feature fvpair1)
                             (fvpair-feature fvpair2))
                        (or (not (member (fvpair-feature fvpair1)
                                         *value-feats*))
                            (equal (fvpair-value fvpair1)
                                   (fvpair-value fvpair2)))))))))
          

(defun value-feats-lessp (fv1 fv2)
  (let ((l1 (length fv1))
        (l2 (length fv2)))
    (if (eql l1 l2)
        (loop for fvpair1 in fv1
            as fvpair2 in fv2
            do 
              (let ((feat1 (fvpair-feature fvpair1))
                    (feat2 (fvpair-feature fvpair2)))
                (unless (eql feat1 feat2)
                  (return (string-lessp feat1 feat2)))
                (when (member (fvpair-feature fvpair1)
                              *value-feats*)
                  (let ((val1 (fvpair-value fvpair1))
                        (val2 (fvpair-value fvpair2)))
                  (unless (equal val1 val2)
                    (return (string-lessp val1 val2))))))
            finally 
              (error "~%Similar relations not grouped"))
      (< l1 l2))))

;;; need sort-mrs-hcons

;;; bindings is a list of assoc lists of variable numbers

(defun mrs-equalp (mrs1 mrs2 &optional syntactic-p noisy-p)
  (let ((bindings (variables-equal (psoa-handel mrs1)
                                   (psoa-handel mrs2) syntactic-p nil)))
    (if bindings
        ; hack for case where no handels
        (progn
          (unless (listp bindings)
            (setf bindings nil))
       (if (setf bindings (variables-equal (psoa-index mrs1)
                            (psoa-index mrs2) syntactic-p bindings))
           (if (setf bindings (mrs-liszts-equal-p (psoa-liszt mrs1)
                                   (psoa-liszt mrs2) 
                                   syntactic-p 
                                   noisy-p bindings))
               (if 
                   (or (not syntactic-p)
                       (setf bindings (hcons-equal-p (psoa-h-cons mrs1)
                                      (psoa-h-cons mrs2) bindings)))
                   bindings
                 (progn 
                   (when noisy-p
                     (format t "~%hcons difference ~A ~A"
                             (psoa-h-cons mrs1)
                             (psoa-h-cons mrs2)))
                   nil))
                 ;; difference in rels reported by
                 ;; mrs-liszts-equal-p
             nil)
         (progn 
           (when noisy-p
             (format t "~%index difference ~A ~A"
                     (psoa-index mrs1)
                     (psoa-index mrs2))
             nil))))
    (progn 
      (when noisy-p
        (format t "~%handel difference ~A ~A"
                (psoa-handel mrs1)
                (psoa-handel mrs2)))
      nil))))


(defun mrs-liszts-equal-p (orig-liszt1 orig-liszt2 syntactic-p 
                                       noisy-p bindings)
  (let ((liszt1 (sort-mrs-struct-liszt orig-liszt1))
        (liszt2 (sort-mrs-struct-liszt orig-liszt2)))
  (and (eql (length liszt1) (length liszt2))
       (if (loop for rel1 in liszt1
           as rel2 in liszt2
           always
             (if
                 (setf bindings (mrs-relation-set-equal-p rel1 rel2 syntactic-p noisy-p bindings))
                bindings 
               (progn
                 (when noisy-p
                   (format t "~%Relations differ ~A ~A" rel1 rel2))
                 nil)))
           bindings))))

(defun mrs-relation-set-equal-p (relset1 relset2 syntactic-p noisy-p bindings)
  (and (eql (length relset1) (length relset2))
       (for rel-alt1 in relset1
            append
            (for rel-alt2 in relset2
                 append
                 (let ((new-bindings (copy-tree bindings)))
                   (mrs-relations-equal-p rel-alt1 rel-alt2
                                          syntactic-p noisy-p new-bindings))))))

(defun mrs-relations-equal-p (rel1 rel2 syntactic-p noisy-p bindings)
  (if (eql (rel-sort rel1) (rel-sort rel2))
      (if (setf bindings 
            (if 
                (and (rel-handel rel1) (rel-handel rel2))
                (variables-equal 
                 (rel-handel rel1) 
                 (rel-handel rel2) syntactic-p bindings)
              bindings))
          (let ((fv1 (rel-flist rel1))
                (fv2 (rel-flist rel2)))
            (if (eql (length fv1) (length fv2))
                         ;;; assumes canonical feature ordering 
                (if 
                    (loop for fvpair1 in fv1
                        as fvpair2 in fv2
                        always 
                          (setf bindings
                            (mrs-fvpair-equal-p fvpair1 fvpair2 
                                                syntactic-p noisy-p bindings)))
                    bindings)
              (progn (when noisy-p
                       (format t "~%Feature numbers differ ~A ~A" 
                               fv1 fv2))
                     nil)))        
            (progn (when noisy-p
                     (format t "~%Handels differ ~A ~A" 
                             (rel-handel rel1) 
                             (rel-handel rel2))
                     nil))) 
        (progn (when noisy-p
                 (format t "~%Sorts differ ~A ~A" 
                         (rel-sort rel1) 
                         (rel-sort rel2))
                 nil))))





(defun mrs-fvpair-equal-p (fvpair1 fvpair2 syntactic-p noisy-p bindings)
  (if (eql (fvpair-feature fvpair1)
           (fvpair-feature fvpair2))
      (if (member (fvpair-feature fvpair1)
                  *value-feats*)
          (if (equal (fvpair-value fvpair1)
                     (fvpair-value fvpair2))
              bindings
            (progn
              (when noisy-p
                (format 
                 t "~%Values differ ~A ~A" 
                 (fvpair-value fvpair1)
                 (fvpair-value fvpair2)))
              nil))
        (if (setf bindings 
              (variables-equal
               (fvpair-value fvpair1)
               (fvpair-value fvpair2)
               syntactic-p bindings))
            bindings
          (progn 
            (when noisy-p
              (format 
               t "~%Variables differ ~A ~A" 
               (fvpair-value fvpair1)
               (fvpair-value fvpair2)))
            nil)))
    (progn (when noisy-p
             (format 
              t "~%Features differ ~A ~A" 
              (fvpair-feature fvpair1)
              (fvpair-feature fvpair2))))))


(defun variables-equal (var1 var2 syntactic-p bindings)
  (let ((new-bindings nil))
    (or (eq var1 var2)
        (and (var-p var1) (var-p var2)
             (if syntactic-p
                 (equal (var-type var1) (var-type var2))
               (compatible-types (var-type var1) (var-type var2)))
             (if syntactic-p
                 (equal-extra-vals
                  (var-extra var1) 
                  (var-extra var2))
               (progn
                 (setf new-bindings
                   (compatible-extra-vals 
                    (var-extra var1) 
                    (var-extra var2) bindings))
                 (or new-bindings (null bindings))))
             (bindings-equal (get-var-num var1)
                             (get-var-num var2) new-bindings)))))

(defun compatible-extra-vals (extra1 extra2 bindings)
  ;;; extra values are currently in a typed-path notation
  ;;; this version is for generation, where we assume we need
  ;;; compatibility, rather than identity
  (let ((ok t))
    (dolist (tp1 extra1)
      (if (and (fvpair-p tp1)
               (typed-path-p (fvpair-feature tp1)))
          (let ((flist1 (typed-path-typed-feature-list 
                         (fvpair-feature tp1))))
            (dolist (tp2 extra2)
              (if (and (fvpair-p tp2)
                       (typed-path-p (fvpair-feature tp2)))
                  (let ((flist2 (typed-path-typed-feature-list 
                                 (fvpair-feature tp2))))
                    (multiple-value-bind
                        (compatible-p comparable-p)
                        (typed-feature-list-compatible flist1
                                                flist2)
                      (unless compatible-p
                        (setf ok nil)
                        (return))
                      (when comparable-p
                        (unless (setf bindings (compatible-extra-values (fvpair-value tp1)
                                                  (fvpair-value tp2) bindings))
                          (setf ok nil)
                          (return)))))))))
      (when (not ok)
        (return)))
    (if ok
        bindings)))


(defun compatible-extra-values (val1 val2 bindings)
  ;;; needs to deal with lists of variables correctly
  ;;; currently just checks for first ones being compatible
  (if (and (listp val1) (var-p (car val1)))
      (if (and (listp val2) (var-p (car val2)))
          (variables-equal (car val1) (car val2) nil bindings)
        nil)
    (if (compatible-types val1 val2) bindings)))

  

(defun typed-feature-list-compatible (tp1 tp2)
  (if (or (null tp1) (null tp2))
      (values t t) ; compatible and equal features
    (let ((first-tvp1 (car tp1))
          (first-tvp2 (car tp2)))
      (if (compatible-types 
           (type-feature-pair-type first-tvp1)
           (type-feature-pair-type first-tvp2))
          (if (eq (type-feature-pair-feature first-tvp1)
                   (type-feature-pair-feature first-tvp2))
            (typed-feature-list-compatible (cdr tp1) (cdr tp2))
            (values t nil))             ; different features, so compatible
                                        ; but not comparable
        (values nil nil))))) ; same features, incompatible type
          
(defun equal-extra-vals (extra1 extra2)
  ;;; extra values are currently in a typed-path notation
  ;;; this version is for tsdb etc, so we're really looking for identity
  (and (eql (length extra1) (length extra2))
       (for tp1 in extra1
            all-satisfy
            (for tp2 in extra2
                 some-satisfy
                 (fvpairs-equal tp1 tp2)))))

(defun fvpairs-equal (tp1 tp2)
  (if (and (fvpair-p tp1) (fvpair-p tp2))
      (and 
       (typed-paths-equal (fvpair-feature tp1)
                         (fvpair-feature tp2))
       (equal (fvpair-value tp1)
              (fvpair-value tp2)))
    (equal tp1 tp2)))

(defun typed-paths-equal (tp1 tp2)
  (if (and (typed-path-p tp1) (typed-path-p tp2))
      (let ((tfl1 (typed-path-typed-feature-list tp1))
            (tfl2 (typed-path-typed-feature-list tp2)))
        (typed-feature-list-equal tfl1 tfl2))
    (equal tp1 tp2)))

(defun typed-feature-list-equal (tp1 tp2)
  (if (or (null tp1) (null tp2))
      t
    (let ((first-tvp1 (car tp1))
          (first-tvp2 (car tp2)))
      (if (equal 
           (type-feature-pair-type first-tvp1)
           (type-feature-pair-type first-tvp2))
          (if (eq (type-feature-pair-feature first-tvp1)
                  (type-feature-pair-feature first-tvp2))
              (typed-feature-list-equal (cdr tp1) (cdr tp2))
            nil)
        nil))))
        
(defun bindings-equal (val1 val2 bindings)
  ;;; this should only be called with null bindings 
  ;;; at the start of a process of checking equality
  ;;; After this - null bindings is a failure
  (if (null bindings)
      (list (list (cons val1 val2)))
    (for binding-possibility in bindings
         filter
         (let ((bound (assoc val1 binding-possibility)))
           (if bound                    ; val1 exists
               (if (eql (cdr bound) val2) ; if it's bound to val2 OK else fail
                   binding-possibility)
             (unless (rassoc val2 binding-possibility) ; check val2 hasn't got bound
               (push (cons val1 val2) binding-possibility) ; create new binding
               binding-possibility))))))


(defun hcons-equal-p (hc-list1 hc-list2 bindings)
  (and (eql (length hc-list1) (length hc-list2))
       (for hc1 in hc-list1
            all-satisfy
            (for hc2 in hc-list2
                 some-satisfy
                 (hcons-el-equal hc1 hc2 bindings)))))

(defun hcons-el-equal (hc1 hc2 bindings)
  (and (variables-equal (hcons-scarg hc1)
                        (hcons-scarg hc2) t bindings)
       (variables-equal (hcons-outscpd hc1)
                        (hcons-outscpd hc2) t bindings)))

(defun variable-set-equal (l1 l2 bindings)
  (or (and (null l1) (null l2))
      (and (eql (length l1) 
                (length l2))
           (for v1 in l1
                all-satisfy
                (for v2 in l2
                     some-satisfy
                     (variables-equal v1 v2 t bindings))))))

