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
;;; two identical rel types, we use handel number

(defun sort-mrs-struct-liszt (liszt)
   (let ((new-liszt (copy-list liszt)))
        (sort new-liszt
              #'(lambda (rel1 rel2)
                  (or (string-lessp (rel-sort rel1) (rel-sort rel2))
                      (and (string-equal (rel-sort rel1) (rel-sort rel2))
                           (< (get-var-num (rel-handel rel1))
                              (get-var-num (rel-handel rel2)))))))
        new-liszt))

;;; need sort-mrs-hcons


(defparameter *bindings* nil)

;;; *bindings* is an assoc list of variable numbers

(defun mrs-equalp (mrs1 mrs2 &optional syntactic-p)
  (setf *bindings* nil)
  (and (variables-equal (psoa-handel mrs1)
                        (psoa-handel mrs2) syntactic-p)
       (variables-equal (psoa-index mrs1)
                        (psoa-index mrs2) syntactic-p)
       (mrs-liszts-equal-p (psoa-liszt mrs1)
                           (psoa-liszt mrs2) syntactic-p)
       (or (not syntactic-p)
           (hcons-equal-p (psoa-h-cons mrs1)
                        (psoa-h-cons mrs2)))))


(defun mrs-liszts-equal-p (orig-liszt1 orig-liszt2 syntactic-p)
  (let ((liszt1 (sort-mrs-struct-liszt orig-liszt1))
        (liszt2 (sort-mrs-struct-liszt orig-liszt2)))
  (and (eql (length liszt1) (length liszt2))
       (loop for rel1 in liszt1
             as rel2 in liszt2
             always
             (and (eql (rel-sort rel1) (rel-sort rel2))
                  (variables-equal (rel-handel rel1) 
                                   (rel-handel rel2) syntactic-p)
                  (let ((fv1 (rel-flist rel1))
                        (fv2 (rel-flist rel2)))
                    (and (eql (length fv1) (length fv2))
                         ;;; assumes canonical feature ordering
                         (loop for fvpair1 in fv1
                               as fvpair2 in fv2
                             always 
                               (and (eql (fvpair-feature fvpair1)
                                         (fvpair-feature fvpair2))
                                    (if
                                        (member
                                         (fvpair-feature fvpair1)
                                         *value-feats*)
                                        (equal
                                         (fvpair-value fvpair1)
                                         (fvpair-value fvpair2))
                                      (variables-equal
                                       (fvpair-value fvpair1)
                                       (fvpair-value fvpair2)
                                       syntactic-p)))))))))))

(defun variables-equal (var1 var2 syntactic-p)
  (or (eq var1 var2)
      (and (var-p var1) (var-p var2)
           (equal (var-type var1) (var-type var2))
           (if syntactic-p
               (equal-extra-vals
                (var-extra var1) 
                (var-extra var2))
               (compatible-extra-vals 
                (var-extra var1) 
                (var-extra var2)))
           (bindings-equal (get-var-num var1)
                           (get-var-num var2)))))

(defun compatible-extra-vals (extra1 extra2)
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
                        (unless (compatible-types (fvpair-value tp1)
                                                  (fvpair-value tp2))
                          (setf ok nil)
                          (return)))))))))
      (when (not ok)
        (return)))
    ok))


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
        
(defun bindings-equal (val1 val2)
  (let ((bound (assoc val1 *bindings*)))
    (if bound                           ; val1 exists
        (eql (cdr bound) val2)          ; if it's bound to val2 OK else fail
      (unless (rassoc val2 *bindings*)  ; check val2 hasn't got bound
              (push (cons val1 val2) *bindings*) ; create new binding
              t))))


(defun hcons-equal-p (hc-list1 hc-list2)
  (and (eql (length hc-list1) (length hc-list2))
       (for hc1 in hc-list1
            all-satisfy
            (for hc2 in hc-list2
                 some-satisfy
                 (hcons-el-equal hc1 hc2)))))

(defun hcons-el-equal (hc1 hc2)
  (and (variables-equal (hcons-scarg hc1)
                        (hcons-scarg hc2) t)
       (variables-equal (hcons-outscpd hc1)
                        (hcons-outscpd hc2) t)
       (variable-set-equal (hcons-cands hc1)
                           (hcons-cands hc2))))

(defun variable-set-equal (l1 l2)
  (or (and (null l1) (null l2))
      (and (eql (length l1) 
                (length l2))
           (for v1 in l1
                all-satisfy
                (for v2 in l2
                     some-satisfy
                     (variables-equal v1 v2 t))))))

