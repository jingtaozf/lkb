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

(defparameter *mrs-results-check* nil)

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
;;; In theory this is trivial, except for the variable replacement
;;; issue.  We simply sort the liszt, by rel type - if there are
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

(defparameter *bindings* nil)

;;; *bindings* is an assoc list of variable numbers

(defun mrs-equalp (mrs1 mrs2)
  (setf *bindings* nil)
  (push (cons (get-var-num (psoa-handel mrs1))
              (get-var-num (psoa-handel mrs2))) *bindings*)
  (cond ((psoa-index mrs1)
         (when (psoa-index mrs2)
               (push (cons (get-var-num (psoa-index mrs1))
                           (get-var-num (psoa-index mrs1)))
                     *bindings*)
               (mrs-liszts-equal-p (psoa-liszt mrs1)
                                   (psoa-liszt mrs2))))
        ((psoa-index mrs2) nil)
        (t (mrs-liszts-equal-p (psoa-liszt mrs1)
                               (psoa-liszt mrs2)))))

(defun mrs-liszts-equal-p (orig-liszt1 orig-liszt2)
  (let ((liszt1 (sort-mrs-struct-liszt orig-liszt1))
        (liszt2 (sort-mrs-struct-liszt orig-liszt2)))
  (and (eql (length liszt1) (length liszt2))
       (loop for rel1 in liszt1
             as rel2 in liszt2
             always
             (and (eql (rel-sort rel1) (rel-sort rel2))
                  (bindings-equal (get-var-num (rel-handel rel1))
                                  (get-var-num (rel-handel rel2)))
                  (let ((fv1 (rel-flist rel1))
                        (fv2 (rel-flist rel2)))
                    (and (eql (length fv1) (length fv2))
                         ;;; assumes canonical feature ordering
                         (loop for fvpair1 in fv1
                               as fvpair2 in fv2
                               always (and (eql (fvpair-feature fvpair1)
                                                (fvpair-feature fvpair2))
                                           (or
                                            (and (member
                                                     (fvpair-feature fvpair1)
                                                     *value-feats*)
                                                    (equal
                                                     (fvpair-value fvpair1)
                                                     (fvpair-value fvpair2)))
                                             (bindings-equal
                                              (get-var-num
                                               (fvpair-value fvpair1))
                                              (get-var-num
                                                (fvpair-value fvpair2)))))))))))))

(defun bindings-equal (val1 val2)
  (let ((bound (assoc val1 *bindings*)))
    (if bound                           ; val1 exists
        (eql (cdr bound) val2)          ; if it's bound to val2 OK else fail
      (unless (rassoc val2 *bindings*)  ; check val2 hasn't got bound
              (push (cons val1 val2) *bindings*) ; create new binding
              t))))
