(in-package "MRS")

;;; LKB specific


(defun time-scope nil 
  (format t "~%~A" user::*sentence*) 
  (for res in user::*parse-record* 
       do 
       (format t " ~A" (length (make-scoped-mrs (car (extract-mrs (list res) t)))))))

(defparameter *alex-munge* nil)

(defun show-mrs nil
  (let ((*print-circle* nil))
    (format t "~%********************")
    (format t "~%~{~A ~}" cl-user::*sentence*)
    (dolist (mrs-struct (extract-mrs *parse-record*))
      (treat-mrs mrs-struct nil))))

(defun show-mrs-full nil
  (let ((*print-circle* nil))
    (format t "~%********************")
    (format t "~%~{~A ~}" cl-user::*sentence*)
    (dolist (mrs-struct (extract-mrs *parse-record*))
      (when *alex-munge*
        (output-mrs mrs-struct 'simple))
      (treat-mrs mrs-struct nil))))

(defun treat-mrs (mrs-struct extra-param)
  (declare (ignore extra-param))
  (cond (*mrs-to-vit*
         (mrs-to-vit-convert mrs-struct))
        ((and *alex-munge* (fboundp 'alex-munge))
         (alex-munge mrs-struct))
        (*mrs-scoping*
         (check-mrs-struct mrs-struct))
        (t (output-mrs mrs-struct 'simple))))


(defvar *mrs-record* nil)

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defun output-mrs-after-parse (edges)
  (when (and (or *mrs-to-vit* *mrs-scoping*
               *mrs-output-p* *alex-munge*))
    (setf *mrs-record*
      (extract-mrs edges))
    (for mrs in *mrs-record* 
         do
         (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
         (setf edges (cdr edges))
         (treat-mrs mrs t))))

#|
(in-package "CL-USER")


(defparameter *do-something-with-parse* 'mrs::show-mrs)

(defparameter *do-something-with-parse* 'mrs::time-scope)

|#
