;;;A little fn to display the parses feature structure

(defun show-parse-fs nil
          (loop for parse in *parse-record*
               do
               (let* ((fs (mrs::get-parse-fs parse))
                     (sem-struct
                        (mrs::path-value fs mrs::*initial-semantics-path*)))
                 (display-dag sem-struct 'simple))))




(defparameter *do-something-with-parse* 'show-parse-fs)



;;; LinGO big grammar specific functions

(defun do-parse nil
  (let* ((sentence
            (ask-for-strings-movable "Current Interaction"
               `(("Sentence" . ,(cons :typein-menu *last-parses*))) 400)))
    (when sentence
      (setf *sentence* (car sentence))
      (close-existing-chart-windows)
      (let ((str (string-trim '(#\space #\tab #\newline) (car sentence))))
        (setq *last-parses*
          (butlast
           (cons str (remove str *last-parses* :test #'equal))
           (max 0 (- (length *last-parses*) 25))))
                                        ; limit number of sentences retained

        (with-output-to-top ()
          (parse
           (split-into-words
            (preprocess-sentence-string str))))))))



            
(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
  (let* ((mother NIL)
         (daughter1 (get-value-at-end-of rule-fs '(DTRS FIRST)))
         (daughter2 (get-value-at-end-of rule-fs '(DTRS REST FIRST)))
         (daughter3 (get-value-at-end-of rule-fs '(DTRS REST REST FIRST)))
         (daughter4 (get-value-at-end-of rule-fs '(DTRS REST REST REST FIRST))))
    (declare (ignore mother))
    (unless (and daughter1 (not (eql daughter1 'no-way-through)))
      (cerror "Ignore it" "Rule without daughter"))
    (append (list nil '(DTRS FIRST))
            (if (and daughter2 (not (eql daughter2 'no-way-through)))
                (list '(DTRS REST FIRST)))
            (if (and daughter3 (not (eql daughter3 'no-way-through)))
                (if (and daughter2 (not (eql daughter2 'no-way-through)))
                    (list '(DTRS REST REST FIRST))))
            (if (and daughter4 (not (eql daughter4 'no-way-through)))
                (if (and daughter3 (not (eql daughter3 'no-way-through)))      
                (if (and daughter2 (not (eql daughter2 'no-way-through)))
                    (list '(DTRS REST REST REST FIRST))))
            ))))


  
  
(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
(let ((affix (existing-dag-at-end-of (tdfs-indef 
                             (rule-full-fs rule)) '(dtrs first needs-affix))))
  (and affix (equal (type-of-fs affix) '(true)))))



(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
  (declare (ignore rule))
  nil)
             

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. None of their descendents (if any) will be displayed either

(defun hide-in-type-hierarchy-p (type-name)
  (and (symbolp type-name)
       (search "GLBTYPE" (symbol-name type-name))))



;;;JTB - 06/28/01 changed :types (list orth-value) to :type orth-value 
;;;since that's what it's supposed to be?

(defun make-orth-tdfs (orth) 
  ;;; this version is for grammars where the
  ;;; orthography is encoded as a list (difference list or otherwise)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (loop for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :type orth-value))
                 unifs)
           (setq tmp-orth-path 
             (append tmp-orth-path *list-tail*))))
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (when indef
          (make-tdfs :indef indef))))))