;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :mrs)

;;; see convert-const-parse-expression for one way of running this code

;;; basemrs.lisp has been modified to define the information structure part of
;;; the MRS and also defines print routines

;;; the following two functions are called from construct-mrs in mrsoutput.lisp
;;; Together with the globals in the mrsglobals file, they control
;;; how a feature structure is processed to extract an information structure

(defun construct-info-s (fs ilist)
 (if (is-valid-fs fs)
  (let ((label-list (fs-arcs fs)))
    (if label-list
        (let ((first-part (assoc (car *first-path*)
                                 label-list))
              (rest-part (assoc (car *rest-path*)
                                label-list)))
          (if (and first-part rest-part)
              (progn
                (push (create-info-struct
                       (cdr first-part))
                       ilist)
                (construct-info-s (cdr rest-part)
                                  ilist))
            ilist))
      ilist))))

(defun create-info-struct (fs)
  (if (is-valid-fs fs)
      (let* ((variable-fs (path-value fs *istruct-var-path*))
             (variable (get-existing-variable variable-fs))
             (focus-fs (path-value fs *istruct-focus-path*))
             (focus-type (if focus-fs (create-type (fs-type focus-fs)))))
        (make-info-struct :variable variable
                          :focus focus-type))))

        

;;; the function find-variable-info-s takes a variable and an MRS structure and
;;; returns the info value associated with the variable, if it is in the
;;; information-structure

(defun find-variable-info-s (var mrs)
  (let ((info-s (psoa-info-s mrs)))
    (if info-s
        (let ((value
               (find var info-s :key #'info-struct-variable)))
          (if value (info-struct-focus value))))))

;;; the output functions are a very simple form of output, essentially a cut
;;; down version of the gq code, which allows for a quantifier free fragment
;;; all variables are interpreted as constants

(defun output-const-mrs (mrs &key (stream t))
  (let* ((rel-list (psoa-liszt mrs)))
    (output-const-rels rel-list stream mrs)))

(defun output-const-rels (rel-list stream mrs)
  (if (rest rel-list) 
      (output-binary-const-conjunction 
           rel-list stream mrs)
    (output-const-rel (first rel-list) stream mrs)))
      
(defun output-binary-const-conjunction (rel-list stream mrs)
  (format stream "(and ")
  (output-const-rel (first rel-list) stream mrs)
  (if (rest (rest rel-list))
      (progn
        (output-binary-const-conjunction (rest rel-list) stream mrs)
        (format stream ")"))
    (progn
      (output-const-rel (first (rest rel-list)) stream mrs)
      (format stream ")"))))

(defun output-const-rel (rel stream mrs)
  (format stream " (~A" 
          (remove-right-sequence "_rel" (string-downcase (rel-sort rel))))
  (loop for feat-val in (rel-flist rel)
      do     
        (let* ((var (fvpair-value feat-val)))
          (if (var-p var)
              (format stream " ~A?~A"
                      (find-variable-info-s var mrs)
                      (remove-variable-junk 
                       (get-bound-var-value var)))
            (format stream " ~A" (remove-variable-junk var)))))
  (format stream ")"))

;;; the following functions are a modified version of conversion to normal form
;;; for DTP, which use the simplified output format

(defun convert-const-parse-expression nil
  (let* ((edges *parse-record*)
         (chosen-edge (select-parse edges))
         (gq-exp (output-selected-const-gq chosen-edge)))
    (if gq-exp
        (let ((fol-exp (convert-gq-to-fol gq-exp)))
          (if fol-exp
              (let ((inf-exps (convert-fol-to-inf fol-exp)))
                inf-exps))))))

(defun output-selected-const-gq (parse)
  (let* ((mrs-struct (extract-mrs parse)))
    (read-from-string
     (with-output-to-string (stream)
       (output-const-mrs mrs-struct :stream stream)))))
