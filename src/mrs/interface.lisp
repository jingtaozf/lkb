(in-package "MRS")

;;; LKB specific

(defun show-mrs nil
  (let ((*print-circle* nil))
    (dolist (mrs-struct (extract-mrs *parse-record*))
      (output-mrs mrs-struct 'simple)
      (when *mrs-to-vit*
          (mrs-to-vit-convert mrs-struct)))))


(defvar *mrs-record* nil)

#| 
(mrs::output-mrs *parse-record*)
|#

(defun output-mrs-after-parse (edges)
  (when (and (or *mrs-to-vit* *mrs-scoping*
               *mrs-output-p*))
    (setf *mrs-record*
      (extract-mrs edges))
    (if *mrs-to-vit*
        (for mrs in *mrs-record* 
             do
             (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
             (setf edges (cdr edges))
             (mrs-to-vit-convert mrs t))
      (if *mrs-scoping*
          (for mrs in *mrs-record*
             do             
             (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
             (setf edges (cdr edges))
             (check-mrs-struct mrs))
        (for mrs in *mrs-record*
             do              
             (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
             (setf edges (cdr edges))
             (output-mrs mrs 'simple))))))

        

#|
(in-package "CL-USER")


 (defparameter *do-something-with-parse* 'mrs::show-mrs)

|#
