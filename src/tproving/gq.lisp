;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; Output routines which produce a prefixed version of the GQ
;;; notation, with explicit binary `and', suitable for reading
;;; into gq-to-fol.lisp


(defun output-gq-mrs (mrs &key (stream t))
  (let* ((top-handel (get-true-var-num (psoa-top-h mrs)))
         (rel-list (psoa-liszt mrs)))
    (output-gq-rels top-handel rel-list stream nil)))

(defun output-gq-rels (top-handel rel-list stream handels-so-far)
  (when (member top-handel handels-so-far)
    (error "Circular structure passed to output-gq-mrs"))
  (let ((top-rels (loop for rel in rel-list
                      when (eql 
                            (get-true-var-num (rel-handel rel)) 
                            top-handel)
                      collect rel)))
    (when (null (list-length top-rels))
      ;;; list-length returns nil if top-rels is a cycle
      (error "Circular LISZT passed to output-gq-mrs"))
    (let ((new-handels-so-far (cons top-handel handels-so-far)))
      (if (rest top-rels) 
          (output-binary-conjunction 
           top-rels rel-list stream new-handels-so-far)
        (output-gq-rel (first top-rels) rel-list stream new-handels-so-far)))))
      
(defun output-binary-conjunction (top-rels rel-list stream handels-so-far)
  (format stream "(and ")
  (output-gq-rel (first top-rels) rel-list stream handels-so-far)
  (if (rest (rest top-rels))
      (output-binary-conjunction (rest top-rels) rel-list stream handels-so-far)
    (progn
      (output-gq-rel (first top-rels) rel-list stream handels-so-far)
      (format stream ")"))))

(defun output-gq-rel (rel rel-list stream handels-so-far)
  (format stream " (~A" 
          (remove-right-sequence "_rel" (string-downcase (rel-sort rel))))
  (loop for feat-val in (rel-flist rel)
      do     
        (let* ((var (fvpair-value feat-val)))
          (if (is-handel-var var)
              (output-gq-rels (get-true-var-num var) rel-list stream
                              handels-so-far)
            (if (var-p var)
                (format stream " ?~A" (remove-variable-junk 
                                      (get-bound-var-value var)))
              (format stream " ~A" (remove-variable-junk var))))))
  (format stream ")"))
