;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package "MRS")

(defvar *dtp-file* nil)

(defun initialize-dtp (file-name)
  (setf *restart-variable-generator* nil)
  (setf *dtp-file* file-name)
  (with-open-file (ostream file-name :direction :output 
                   :if-exists :supersede)
    (format ostream ":theory auto~%")))
  
(defun assert-dtp nil
  (let ((converted-expression (convert-parse-expression)))
    (with-open-file 
        (ostream *dtp-file* :direction :output 
         :if-exists :append)
      (loop for inf-exp in converted-expression
          do
            (format ostream "~%~A" inf-exp)))))

(defun convert-parse-expression nil
  (let* ((edges *parse-record*)
         (chosen-edge (select-parse edges))
         (gq-exp (output-selected-gq chosen-edge)))
    (if gq-exp
        (let ((fol-exp (convert-gq-to-fol gq-exp)))
          (if fol-exp
              (let ((inf-exps (convert-fol-to-inf fol-exp)))
                inf-exps))))))

#|
(defun query-dtp nil
  ;;; won't work because of package problems
  (dtp::dtp-load *dtp-file*)
  (let ((converted-expression (convert-parse-expression)))
    (loop for exp in converted-expression
        do
        (dtp::prove exp))))
|#

(defun output-selected-gq (parse)
  (let* ((mrs-struct (extract-mrs parse))
         (binding-sets (make-scoped-mrs mrs-struct)))
    (when binding-sets
      (let ((binding (select-binding binding-sets)))
        (setf *canonical-bindings* 
          (canonical-bindings binding))
        (let ((*print-circle* nil))
          (read-from-string
           (with-output-to-string (stream)
             (output-gq-mrs mrs-struct :stream stream))))))))


(defun select-parse (edges)
  (first edges))

(defun select-binding (binding-sets)
  (first binding-sets))


