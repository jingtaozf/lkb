(in-package :lkb)

;;; functions which output in path notation
;;; mostly moved from lexinput.lsp 


;;; called from outputsrc

(defun output-type-as-paths (type type-record ostream)
  (format ostream 
          "~%~(~A~) ~(~A~) " type (type-parents type-record))
  (when (type-comment type-record)
    (format ostream "~%~S" (type-comment type-record)))
  (if (type-enumerated-p type-record)
    (format ostream "~%(OR~{~( ~A~)~}).~%" (type-daughters type-record))
    (let ((local-constraint (type-local-constraint type-record)))
      (if (null local-constraint)
        (format ostream ".~%")
        (display-dag1 local-constraint
                      'path2 ostream)))))



;;; unification based

(defun output-unif (unif ostream active-p)
   (cond 
      ((unification-p unif)
         (output-unif-lhs ostream unif active-p)
         (output-path ostream (basic-unification-rhs unif) active-p))
      ((or (path-p unif) (typed-path-p unif))
       (output-path ostream unif active-p))
      (t (format ostream "~A" unif))))

(defun output-unif-lhs (ostream unif &optional active-p)
  (declare (ignore active-p))
   (format ostream "~%")
   (cond 
      ((unification-p unif) 
         (output-path ostream (basic-unification-lhs unif))
         (format ostream "    =     "))
      (t (error "Unrecognised item in lexical specification ~A" 
            unif))))
         

(defun display-fs-spec (structure ostream &optional active-p)
   (cond 
      ((fs-and-path-p structure) 
         (display-fs-spec (fs-and-path-fs structure) 
            ostream active-p)
         (output-path ostream (fs-and-path-path structure) active-p))
      (t (error "~%Unknown thing in unification specification ~A"
            structure))))  

            
(defun output-path (ostream path &optional active-p)
   (cond ((typed-path-p path)
          (let ((ordered-list (typed-path-typed-feature-list path)))
            (format ostream "<")
            (when ordered-list
              (output-type-feature-pair ostream (car ordered-list) active-p)
              (loop for tfp in (cdr ordered-list)
                   do
                   (format ostream ":" )
                   (output-type-feature-pair ostream tfp active-p)))
            (format ostream ">")))
         ((path-p path)
          (let ((ordered-list (path-typed-feature-list path)))
            (format ostream "<")
            (when ordered-list
              (format ostream " ~A " (car ordered-list))
              (loop for feat in (cdr ordered-list)
                   do
                   (format ostream ":" )
                   (format ostream " ~A " feat)))
            (format ostream ">")))
         (t
          (format ostream (if active-p "~/FB/~(~A~)~/FP/   " "~S") 
                    (u-value-type path)))))

      
(defun output-type-feature-pair (ostream tfp &optional active-p)
   (if active-p
      (format ostream " ~A " 
         (type-feature-pair-feature tfp))
      (format ostream "~( ~A ~A ~)" 
         (type-feature-pair-type tfp)
         (type-feature-pair-feature tfp))))



