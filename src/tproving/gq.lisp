;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package "MRS")

;;; Output routines which produce a prefixed version of the GQ
;;; notation, with explicit binary `and', suitable for reading
;;; into gq-to-fol.lisp

;;; called from mrs/interface.lisp etc on an mrs and associated
;;; *canonical-bindings* from a scope resolution

(defun output-gq-mrs (mrs)
  (let* ((top-handel (get-true-var-num (psoa-top-h mrs)))
         (rel-list (psoa-liszt mrs)))
    (output-gq-rels top-handel rel-list nil)))

(defun output-gq-rels (top-handel rel-list handels-so-far)
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
           top-rels rel-list new-handels-so-far)
        (output-gq-rel (first top-rels) rel-list new-handels-so-far)))))
      
(defun output-binary-conjunction (top-rels rel-list handels-so-far)
  (cons 'and
	(list 
	 (output-gq-rel (first top-rels) rel-list handels-so-far)
	 (if (rest (rest top-rels))
	     (output-binary-conjunction (rest top-rels) 
					rel-list handels-so-far)
	   (output-gq-rel (second top-rels) rel-list handels-so-far)))))

(defun output-gq-rel (rel rel-list handels-so-far)
  (cons 
   (intern (remove-right-sequence "_REL" (string-upcase (rel-pred rel))))
   (loop for feat-val in (rel-flist rel)
       collect
	 (let* ((var (fvpair-value feat-val)))
	   (if (is-handel-var var)
	       (output-gq-rels (get-true-var-num var) rel-list
			       handels-so-far)
	     (if (var-p var)
		 (intern 
		  (format nil "?~A" 
			  (string-upcase
			  (remove-variable-junk 
				      (get-bound-var-value var)))))
	       (intern (string-upcase (remove-variable-junk var)))))))))
