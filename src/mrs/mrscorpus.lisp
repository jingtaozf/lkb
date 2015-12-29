;;; Copyright (c) 1998-2008 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.


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

;; Not all lisps (e.g., CMU CL 18a) support equalp hash tables, so in
;; the interests of portability, make this an equal hash table and map
;; all the keys to lower case (RPM, 27-Mar-2000)

(defparameter *mrs-results-table* (make-hash-table :test #'equal))

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
                     (string-downcase (remove-trailing-periods sentence))
		     *mrs-results-table*)
                    mrsstruct)))))
  

(defun remove-trailing-periods (sentence-string)
  (string-right-trim '(#\Space #\.) sentence-string))


(defun compare-mrs-struct (sentence mrs-struct stream &optional (comment t))
  (if mrs-struct
      (let ((previous-result (gethash 
			      (string-downcase 
			       (remove-trailing-periods sentence))
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


(defparameter *special-hack-for-message-types-p* nil)
;;; a temporary expedient (I hope) to allow the algebra checking to
;;; go through despite the differences in message types
;;; produced by the current ERG

(defun sort-mrs-struct-liszt (liszt)
  (let ((new-liszt 
         (combine-similar-relations liszt nil #'similar-relations-p)))
   (sort new-liszt
         #'(lambda (relset1 relset2)
             (let ((rel1 (car relset1)) (rel2 (car relset2)))
               (if *special-hack-for-message-types-p*
		   (cond ((subtype-p (rel-pred rel1) 'lkb::message_m_rel) 
			  t)
			 ((subtype-p (rel-pred rel2) 'lkb::message_m_rel) 
			  nil)
			 (t (or
			     (string-lessp (rel-pred rel1) (rel-pred rel2))
			     (and (string-equal (rel-pred rel1) (rel-pred rel2))
				  (value-feats-lessp (rel-flist rel1)
						     (rel-flist rel2))))))
		 (or
		  (string-lessp (rel-pred rel1) (rel-pred rel2))
		  (and (string-equal (rel-pred rel1) (rel-pred rel2))
		       (value-feats-lessp (rel-flist rel1)
					  (rel-flist rel2))))))))))
		 

(defun combine-similar-relations (liszt result-so-far test-fn)
  ;;; revised version maintains original order
  (if (null liszt)
      result-so-far
    (let ((test-rel (car liszt))
          (similar nil)
          (non-similar nil))
      (loop for rel in (cdr liszt)
           do
           (if (apply test-fn (list test-rel rel))
               (push rel similar)
             (push rel non-similar)))
      (combine-similar-relations (nreverse non-similar)
                                 (push (cons test-rel (nreverse similar))
				       result-so-far)
				 test-fn))))

(defun similar-relations-p (rel1 rel2)
  (and (or (string-equal (rel-pred rel1) (rel-pred rel2))
	   (and *special-hack-for-message-types-p*
	       (subtype-p (rel-pred rel1) 'lkb::message_m_rel)
	       (subtype-p (rel-pred rel2) 'lkb::message_m_rel)))
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

;;; make current code more compact and allow for 
;;; reporting of comparisons to windows etc

(defparameter *mrs-comparison-output-control*
    nil)
;;; values are nil, :noisy-p, :save

(defparameter *mrs-comparison-output-messages*
    nil)
;; used by algebra code

(defparameter *no-comptype-checkp*
    nil)

(defun mrs-comparison-output (&rest args)
  (cond ((eql *mrs-comparison-output-control* :noisy-p)
	 (do-comparison-message args 'mrs::simple-indexed t))
	((eql *mrs-comparison-output-control* :save)
	 (push args *mrs-comparison-output-messages*))
	(t nil))
  nil)

(defun do-comparison-message (args device stream)
  (def-print-operations device 0 stream)
  (format stream "~%")
  (if (eql (car args) :message)
      (apply #'format stream (rest args))
    (let ((real-args (rest args))
	  (display *mrs-display-structure*))
      (ecase (car args)
	(:value
	 (format stream "~%Values differ: ")
	 (dolist (value real-args)
	   (format stream " ")
	   (if (var-p value)
	       (mrs-output-var-fn display
				  (find-var-name value nil))
	     (mrs-output-atomic-fn display value))))
	(:feature
	 (format stream "~%Features differ: ~A ~A" (first real-args)
		 (second real-args)))
	(:hcons
	 (format stream "~%Hcons differ")
	 (dolist (h real-args)
	   (print-mrs-hcons h nil display)))
	(:index
	 (format stream "~%Indices differ")
	 (dolist (h real-args)
	   (format stream " ")
	   (mrs-output-var-fn 
	    display
	    (find-var-name h nil))))
	(:handle
	 (format stream "~%Labels differ")
	 (dolist (h real-args)
	   (format stream " ")
	   (mrs-output-var-fn
	    display
	    (find-var-name h nil))))
	(:relset 
	 (format stream "~%Relations differ")
	 (dolist (relset real-args)
	   (format stream "~%")
	   (dolist (rel relset)
	     (print-rel rel t nil display))))
	(:predicate (format stream "~%Predicates differ ~A ~A" 
			    (first real-args) (second real-args)))))))

;;; need sort-mrs-hcons

;;; bindings is a list of assoc lists of variable numbers

;;;
;;; by default, include variable properties in equality comparison
;;;
(defparameter *mrs-equalp-properties-p* t)

;;;
;;; for better parameterization, allow the equality test to ignore a set of
;;; (pseudo-)roles in each EP; make this list empty, by default, to preserve
;;; original LKB behavior, but in LOGON we need LNK and PSV on this list (the
;;; former, for now, while paul treats LNK as a plain role).
;;;                                                            (25-nov-04; oe)
(defparameter *mrs-equalp-ignored-roles* nil)

(defun mrs-equalp (mrs1 mrs2 &optional syntactic-p noisy-p (propertyp t))
  #+:debug
  (setf %mrs1 mrs1 %mrs2 mrs2)
  (let* ((*mrs-comparison-output-control* (if noisy-p :noisy-p nil))
	 (*mrs-equalp-properties-p* propertyp)
         (bindings (variables-equal (psoa-top-h mrs1)
                                    (psoa-top-h mrs2) syntactic-p nil)))
    (if (or bindings (null *rel-handel-path*))
	(if (setf bindings 
		  (variables-equal (psoa-index mrs1)
				   (psoa-index mrs2) syntactic-p bindings))
           (if (setf bindings (mrs-liszts-equal-p (psoa-liszt mrs1)
                                   (psoa-liszt mrs2) 
                                   syntactic-p 
                                   bindings))
               (if 
                   (or (not syntactic-p)
                       (setf bindings (hcons-equal-p (psoa-h-cons mrs1)
                                      (psoa-h-cons mrs2) bindings)))
                   bindings
		 (mrs-comparison-output :hcons
					(psoa-h-cons mrs1)
					(psoa-h-cons mrs2)))
                 ;; difference in rels reported by
                 ;; mrs-liszts-equal-p
             nil)
	 (mrs-comparison-output :index
				(psoa-index mrs1)
				(psoa-index mrs2)))
        (mrs-comparison-output :handle
                (psoa-top-h mrs1)
                (psoa-top-h mrs2)))))


(defun mrs-liszts-equal-p (orig-liszt1 orig-liszt2 syntactic-p 
                                       bindings)
  (let ((liszt1 (sort-mrs-struct-liszt orig-liszt1))
        (liszt2 (sort-mrs-struct-liszt orig-liszt2)))
    (unless (eql (length liszt1) (length liszt2))
      (mrs-comparison-output :message "Difference in RELS (bags of EPs)")
      (return-from mrs-liszts-equal-p))
    (if (loop for rel1 in liszt1
            as rel2 in liszt2
            always
              (if (setf bindings 
                    (mrs-relation-set-equal-p 
                     rel1 rel2 syntactic-p bindings))
                bindings 
		(mrs-comparison-output :relset
				       rel1 rel2)))
        bindings)))

(defun mrs-relation-set-equal-p (relset1 relset2 syntactic-p bindings)
  (and (eql (length relset1) (length relset2))
;;;       (let ((*mrs-comparison-output-control* nil))
	 (loop for rel-alt1 in relset1
	     append
	       (loop for rel-alt2 in relset2
		   append
		     (let ((new-bindings (copy-tree bindings)))
		       (mrs-relations-equal-p rel-alt1 rel-alt2
					      syntactic-p new-bindings))))))


(defun mrs-relations-equal-p (rel1 rel2 syntactic-p bindings)
  (if (or (equal (rel-pred rel1) (rel-pred rel2))
	  (and *special-hack-for-message-types-p*
	       (subtype-p (rel-pred rel1) 'lkb::message_m_rel)
	       (subtype-p (rel-pred rel2) 'lkb::message_m_rel)
	       (compatible-types (rel-pred rel1) (rel-pred rel2))))
      ;; then predicates ok
      (if (setf bindings 
            (if (or (rel-handel rel1) (rel-handel rel2))
                (variables-equal 
                 (rel-handel rel1) 
                 (rel-handel rel2) syntactic-p bindings)
              bindings))
	  ;; then handels ok
          (let ((fv1 (loop
                         for role in (rel-flist rel1)
                         unless (member
                                 (fvpair-feature role) 
                                 *mrs-equalp-ignored-roles*)
                         collect role))
                (fv2 (loop
                         for role in (rel-flist rel2)
                         unless (member
                                 (fvpair-feature role) 
                                 *mrs-equalp-ignored-roles*)
                         collect role)))
            (if (eql (length fv1) (length fv2))
                         ;;; assumes canonical feature ordering 
                (if 
                    (loop for fvpair1 in fv1
                        as fvpair2 in fv2
                        always 
                          (setf bindings
                            (mrs-fvpair-equal-p fvpair1 fvpair2 
                                                syntactic-p bindings)))
                    bindings)
	      (mrs-comparison-output :message "Feature numbers differ")))
	    ;;; else handels not ok
	    (mrs-comparison-output :handle
				   (rel-handel rel1) 
				   (rel-handel rel2)))
    ;; else predicates not ok
	(mrs-comparison-output :predicate
			       (rel-pred rel1) 
			       (rel-pred rel2))))





(defun mrs-fvpair-equal-p (fvpair1 fvpair2 syntactic-p bindings)
  (if (eql (fvpair-feature fvpair1)
           (fvpair-feature fvpair2))
      (if (member (fvpair-feature fvpair1)
                  *value-feats*)
          (if (equal (fvpair-value fvpair1)
                     (fvpair-value fvpair2))
              bindings
	    (mrs-comparison-output :value 
				   (fvpair-value fvpair1)
				   (fvpair-value fvpair2)))
        (if (setf bindings 
              (variables-equal
               (fvpair-value fvpair1)
               (fvpair-value fvpair2)
               syntactic-p bindings))
            bindings
              (mrs-comparison-output :value
				     (fvpair-value fvpair1)
				     (fvpair-value fvpair2))))
             (mrs-comparison-output :feature
              (fvpair-feature fvpair1)
              (fvpair-feature fvpair2))))


(defun variables-equal (var1 var2 syntactic-p bindings)
  (or (if (eq var1 var2) bindings)
      (and (var-p var1) (var-p var2)
           (if syntactic-p
             (equal (var-type var1) (var-type var2))
             (if (and (var-type var1) (var-type var2))
		 (compatible-var-types (var-type var1) (var-type var2))
               t))
           (or (null *mrs-equalp-properties-p*)
               (if (or syntactic-p *no-comptype-checkp*)
                 (equal-extra-vals (var-extra var1) (var-extra var2))
                 (compatible-extra-vals (var-extra var1) (var-extra var2))))
           (bindings-equal (get-var-num var1)
                           (get-var-num var2) bindings))))


(defun compatible-var-types (var-type1 var-type2)
  ;;; FIX - should allow for underspecified values
  ;;; we really must clean this up!
  (or 
   (equal var-type1 var-type2)
   (equal var-type1 "u")
   (equal var-type2 "u")
   (and (equal var-type1 "e") (equal var-type2 "i"))
   (and (equal var-type1 "x") (equal var-type2 "i"))
   (and (equal var-type1 "i") (equal var-type2 "e"))
   (and (equal var-type1 "i") (equal var-type2 "x"))))


(defun compatible-extra-vals (extra1 extra2)
  ;;; this version is for generation, where we assume we need
  ;;; compatibility, rather than identity
  (or (null extra1) (null extra2)
      (let ((ok t))
        (dolist (tp1 extra1)
          (if (extrapair-p tp1)
              (let ((f1 (extrapair-feature tp1)))
                (dolist (tp2 extra2)
                  (if (extrapair-p tp2)
                      (let ((f2 (extrapair-feature tp2)))
                        (when (eql f1 f2)
                          (unless (compatible-types 
                                   (extrapair-value tp1)
                                   (extrapair-value tp2))
                            (setf ok nil)
                            (return))))))))
          (when (not ok)
            (return)))
        ok)))

(defun equal-extra-vals (extra1 extra2)
  ;;
  ;; for better parameterization, allow *mrs-equalp-properties-p* to be a list
  ;; of property names that should be ignored: for simplicity, filter those out
  ;; first.                                                     (30-jun-04; oe)
  ;;
  (let ((extra1 (if (consp *mrs-equalp-properties-p*)
                  (loop
                      with filter = *mrs-equalp-properties-p*
                      for extra in extra1
                      unless (member (extrapair-feature extra) filter)
                      collect extra)
                  extra1))
        (extra2 (if (consp *mrs-equalp-properties-p*)
                  (loop
                      with filter = *mrs-equalp-properties-p*
                      for extra in extra2
                      unless (member (extrapair-feature extra) filter)
                      collect extra)
                  extra2)))
    (and (eql (length extra1) (length extra2))
         (every #'(lambda (tp1)
                    (some 
                     #'(lambda (tp2) 
                         (extrapairs-equal tp1 tp2))
                     extra2))
                extra1))))
       
(defun extrapairs-equal (tp1 tp2)
  (if (and (extrapair-p tp1) (extrapair-p tp2))
      (and 
       (eq (extrapair-feature tp1)
           (extrapair-feature tp2))
       (equal (extrapair-value tp1)
              (extrapair-value tp2)))
    (equal tp1 tp2)))
       
(defun bindings-equal (val1 val2 bindings)
  ;;; this should only be called with null bindings 
  ;;; at the start of a process of checking equality
  ;;; After this - null bindings is a failure
  ;;; but this is checked for in code before this is called
  (if (null bindings)
      (list (list (cons val1 val2)))
    (loop for binding-possibility in bindings
         nconc
         (let ((bound (assoc val1 binding-possibility)))
           (if bound                    ; val1 exists
               (if (eql (cdr bound) val2) ; if it's bound to val2 OK else fail
                   (list binding-possibility))
             (unless (rassoc val2 binding-possibility) ; check val2 hasn't got bound
               (push (cons val1 val2) binding-possibility) ; create new binding
               (list binding-possibility)))))))


(defun hcons-equal-p (hc-list1 hc-list2 bindings)
  (and (eql (length hc-list1) (length hc-list2))
       (every #'(lambda (hc1)
                  (some 
                   #'(lambda (hc2) 
                       (hcons-el-equal hc1 hc2 bindings))
                   hc-list2))
              hc-list1)))

(defun hcons-el-equal (hc1 hc2 bindings)
  (and (variables-equal (hcons-scarg hc1)
                        (hcons-scarg hc2) t bindings)
       (variables-equal (hcons-outscpd hc1)
                        (hcons-outscpd hc2) t bindings)))

(defun variable-set-equal (l1 l2 bindings)
  (or (and (null l1) (null l2))
      (and (eql (length l1) 
                (length l2))
           (every #'(lambda (v1)
                      (some 
                       #'(lambda (v2) 
                           (variables-equal v1 v2 t bindings))
                       l2))
                  l1))))


;;; for idioms

(defvar *unmatched-rels* nil)

(defun mrs-subset-p (subset-mrs superset-mrs syntactic-p)
  (setf *unmatched-rels* nil)
  (let* ((subset-liszt (psoa-liszt subset-mrs))
         (superset-liszt (psoa-liszt superset-mrs))
         (liszt1 (sort-mrs-struct-liszt subset-liszt))
        (liszt2 (sort-mrs-struct-liszt superset-liszt)))
    (values (mrs-liszts-subset-aux-p liszt1 liszt2 syntactic-p nil)
            *unmatched-rels*)))

(defun mrs-liszts-subset-aux-p (subset-liszt1 superset-liszt2 syntactic-p 
                                bindings)
  (or (null subset-liszt1)
      (and superset-liszt2
           (let ((binding-copy (copy-tree bindings))
                 (rel1 (car subset-liszt1))
                 (rel2 (car superset-liszt2)))
             (if
                 (mrs-relation-set-equal-p rel1 rel2 
                                           syntactic-p binding-copy)
                 (mrs-liszts-subset-aux-p (cdr subset-liszt1)
                                          (cdr superset-liszt2) 
                                          syntactic-p binding-copy)
               (progn (push rel2 *unmatched-rels*)
                      (mrs-liszts-subset-aux-p subset-liszt1
                                               (cdr superset-liszt2)
                                               syntactic-p bindings)))))))


;;; Code for SciBorg
;;; Utility function, takes a file of gold MRSs from TreeBanking
;;; and a corresponding directory containing PET output MRSs 
;;; each as a separate file.  Assumes file name is item_n
;;; 
;;; Checks the gold MRS against the PET MRSs, outputting
;;; a string if there is a perfect match.  This notion of matching 
;;; uses the `syntactic' version of mrsequal-p
;;;
;;; Also, converts MRS to RMRS and uses the RMRS matching code
;;; to find a `best' match according to the supplied scoring
;;; function.  Outputs the MRS corresponding to the best match.

(defun sciborg-mrs-match-results (goldfile petdir output-file)
  (sciborg-match-results goldfile petdir output-file :mrs))

(defun sciborg-rmrs-match-results (goldfile petdir output-file)
  (sciborg-match-results goldfile petdir output-file :rmrs))


(defun sciborg-match-results (resfile petdir output-file match-type)
  (unless (or (eql match-type :mrs) (eql match-type :rmrs))
    (error "sciborg-match-results must be called with match-type :mrs or :rmrs"))
  (with-open-file (ostream output-file 
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (with-open-file (istream resfile :direction :input)
      (loop 
	(let ((resline (read-line istream nil nil)))
	  (unless (and resline (not (equal resline "")))
	    (return))
	  (let ((item-number (get-item-number-from-res resline))
		(mrsstr (get-mrsstr-from-res resline)))
	    (when (and item-number mrsstr)
	      (with-input-from-string (istream mrsstr)
		(let* ((original-mrs (read-mrs istream))
		       (mrs (remove-unk-rel-cargs original-mrs))
		       (rmrs (mrs-to-rmrs mrs)))
		  (format ostream "~%Gold standard for ~A~%" item-number) 
		  (output-mrs1 mrs 'mrs-xml ostream)
		  ;;		(pprint rmrs)
	       (let ((petsubdir 
		      (concatenate 'string petdir 
				   "item_" (format nil "~A/*" item-number)))
		     (max-weight 0)
		     (matching-mrss nil)
		     (best-petfile-so-far nil)
		     (best-mrs-so-far nil))
		  ;;; /* so directory gives files
		 (unless (directory petsubdir)
		   (format ostream "~%No files found in ~A" petsubdir))
		 (loop for file in (directory petsubdir)
		       do
		       (let* ((possible-match-from-file
			       (read-single-mrs-xml-file file))
			      (possible-match 
			       (remove-unk-rel-cargs 
				possible-match-from-file)))
			 (when possible-match
			   (if (eql match-type :mrs)
			       (let ((*no-comptype-checkp* t))
			       (when (mrs-equalp possible-match mrs t)
				 ;; (mrs-equalp possible-match mrs nil t)
				 ;; gives some feedback
				 (format ostream "~%Item ~A perfect match ~A" 
					 item-number file)
				 (push possible-match matching-mrss)))
			       ;;; else - :rmrs
			   (let* ((possible-match-rmrs 
				   (mrs-to-rmrs possible-match))
				  (comparison-records 
				    (compare-rmrs-no-char 
				     ;; in rmrs/compare.lisp
				     rmrs
				     possible-match-rmrs)))
			     (when comparison-records
			       (let
				   ((score (qa-score 
					    (car comparison-records))))
				    ;;; FIX the scoring
				 (when (> score max-weight)
				   (setf max-weight score)
				   (setf best-petfile-so-far
				     (list file))
				   (setf best-mrs-so-far 
				     (list possible-match-from-file)))
				 (when (= score max-weight)
				   (push file best-petfile-so-far)
				   (push possible-match-from-file
					 best-mrs-so-far)))))))))
				   
		 (if (eql match-type :rmrs)
		     (progn
		       (format ostream "~%Best matches ~A" 
			       best-petfile-so-far)
		       (dolist (mrs best-mrs-so-far)
			 (output-mrs1 mrs 'mrs-xml ostream)))
		   (if matching-mrss
		       (progn
			 (format ostream "~%Perfect matches")
			 (dolist (match-mrs matching-mrss)
			   (output-mrs1 match-mrs 'mrs-xml ostream)))
		     (format ostream "~%No matches")))))))))))))


(defun single-mrs-check (gold-file other-file)
  ;;; 
  (let ((*no-comptype-checkp* t))
  (let* ((gold-mrs (read-single-mrs-xml-file gold-file))
	 (possible-match-from-file
	  (read-single-mrs-xml-file other-file)))
    (mrs-equalp (remove-unk-rel-cargs possible-match-from-file)
		(remove-unk-rel-cargs gold-mrs) nil t))))


#| 

(single-mrs-check "gold-10.mrs" "lkb1.mrs")

(sciborg-mrs-match-results "~aac10/sciborg/foraac/sciborg-b309733a/result-selected"
"/anfs/bigdisc/aac10/sciborg/goldoscar/sciborg-b309733a/" "gold-b309733a.mrs")

(sciborg-rmrs-match-results "~aac10/sciborg/foraac/sciborg-b309733a/result-selected"
"/anfs/bigdisc/aac10/sciborg/goldoscar/sciborg-b309733a-s/" "gold-b309733a-rmrs.mrs")

|#
         
;;; parse the fine system result - mrs is at the end between two @
;;; item is first thing, before first @

(defun get-item-number-from-res (resline)
  (let ((hashpos (position #\@ resline)))
    (when hashpos 
      (parse-integer (subseq resline 0 hashpos) :junk-allowed t))))
	
	    
(defun get-mrsstr-from-res (resline)
  (let* ((hashpos1 (position  #\@ resline :from-end t 
			      :end (- (length resline) 1))))
    (when hashpos1
      (subseq resline (+ hashpos1 1) (- (length resline) 1)))))

(defun remove-unk-rel-cargs (mrs)
  ;;; a hack necessitated by the fact that unknown relations 
  ;;; have been given CARGs, but these are not instantiated by the 
  ;;; treebanking machinery
  (dolist (rel (psoa-liszt mrs))
    (when (or (substring-match-p "_unk_" (string (rel-pred rel)))
	      (substring-match-p "oscarcompound" (string (rel-pred rel))))
      (setf (rel-flist rel)
	(loop
	    for role in (rel-flist rel)
	    unless (string-equal
		    (string (fvpair-feature role)) 
		    "CARG")
	    collect role))))
  mrs)

(defun substring-match-p (substr str)
  ;;; must be defined somewhere already!
  (let ((substr-lst (coerce substr 'list))
	(str-lst (coerce str 'list)))
    (sublist-match-p substr-lst str-lst)))

(defun sublist-match-p (sublst lst)
  (or (null sublst)
      (if lst
	  (or (all-match-p sublst lst)
	    (sublist-match-p sublst (cdr lst)))
	nil)))

(defun all-match-p (sublst lst)
  (or (null sublst)
      (and
       lst
       (char-equal (car sublst) (car lst))
       (all-match-p (cdr sublst) (cdr lst)))))


