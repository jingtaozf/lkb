;;; User defined functions - from old globals file

(in-package :cl-user)

(defun preprocess-sentence-string (str)
  ;; replace all punctuation by spaces
  ;; except
  ;; for PAGE compatability, replace #\' by #\space
  ;; except at end of word, when replace by #\space #\s
  (let ((in-word nil)
        (chars (coerce str 'list))
        (result-chars nil))
    (do* ((next-char (car chars) (car remainder))
          (remainder (cdr chars) (cdr remainder)))
         ((null next-char) nil)
         (cond ((eql next-char #\')
                (cond 
                 ((not in-word) 
                  (if (or (null remainder) (eql (car remainder) #\space))
                      nil
                    (progn
                      (push next-char result-chars)
                      (setf in-word t))))
                 ((or (null remainder) (eql (car remainder) #\space))
                  (setf in-word nil)
                  (push #\space result-chars)
                  (push #\s result-chars))
                 (t
                  (setf in-word nil)
                  (push #\space result-chars))))
               ((not (alphanumeric-or-extended-p next-char)) 
                (setf in-word nil)
                (push #\space result-chars))
               (t (setf in-word t) 
                (push next-char result-chars))))
    (string-trim '(#\space) (coerce (nreverse result-chars) 'string))))

(defun make-sense-unifications (sense-string id language)
   ;; < orth : hd > = sense
   ;; < lang > = language
  (declare (ignore sense-string id language))
   nil)

(defparameter *sense-unif-fn* #'make-sense-unifications)


;;;   (when sense-string
;;;    (list 
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(orth hd))
;;;          :rhs (make-u-value :types (list sense-string)))
;;;       (make-unification :lhs
;;;          (create-path-from-feature-list '(lang))
;;;          :rhs (make-u-value :types (list language)))
;;;        (make-unification :lhs
;;;           (create-path-from-feature-list '(fs-id))
;;;           :rhs (make-u-value :types (list id)))
;;;       )))


(defun make-orth-tdfs (orth)
  (let ((unifs nil)
        (tmp-orth-path *orth-path*))
    (for orth-value in (split-into-words orth)
         do
         (let ((opath (create-path-from-feature-list 
                       (append tmp-orth-path *list-head*))))
           (push (make-unification :lhs opath                    
                                   :rhs
                                   (make-u-value 
                                    :types (list orth-value)))
                 unifs)
           (setq tmp-orth-path (append tmp-orth-path *list-tail*))))
#|
         ;; not a good idea for difference lists                             
(push (make-unification :lhs
                            (create-path-from-feature-list tmp-orth-path)
                            :rhs 
                            (make-u-value :types (list *empty-list-type*)))
                            unifs)
|#
    (let ((indef (process-unifications unifs)))
      (when indef
        (setf indef (create-wffs indef))
        (make-tdfs :indef indef)))))

  

(defun establish-linear-precedence (rule-fs)
   ;;;    A function which will order the features of a rule
   ;;;    to give (mother daughter1 ... daughtern)
   ;;;    
   ;;;  Modification - this must always give a feature
   ;;;  position for the mother - it can be NIL if
   ;;; necessary
   (sort (remove 'needs-affix (top-level-features-of rule-fs))
         #'(lambda (x y)
             (let ((x-num (if (numberp x) x
                            (parse-integer (string x) :junk-allowed t)))
                   (y-num (if (numberp y) y
                            (parse-integer (string y) :junk-allowed t))))
               (if (and (numberp x-num) (numberp y-num))
                 (< x-num y-num)
                 (not (numberp x-num)))))))


(defun spelling-change-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which affects spelling and
;;; which should therefore only be applied by the morphology
;;; system.  
;;; this version tests for
;;; < NEEDS-AFFIX > = true
;;; in the rule
  (let ((affix (get-dag-value (rule-full-fs rule) 'needs-affix)))
    (and affix (equal (type-of-fs affix) '(true)))))

(defun redundancy-rule-p (rule)
;;; a function which is used to prevent the parser 
;;; trying to apply a rule which is only used
;;; as a redundancy rule 
;;; this version tests for 
;;; < PRODUCTIVE > = false
;;; in the rule
  (let ((affix (get-dag-value (rule-full-fs rule) 'productive)))
    (and affix (equal (type-of-fs affix) '(false)))))

;;; return true for types that shouldn't be displayed in type hierarchy
;;; window. Descendents (if any) will be displayed, i.e. non-displayed
;;; types are effectively spliced out

(defun hide-in-type-hierarchy-p (type-name)
  (declare (ignore type-name))
  nil)

(defun find-infl-pos (unifs orths sense-id)
  ; default inflection position for multi-word entries is rightmost
  (declare (ignore unifs sense-id))
  (length orths))

;;; Assign priorities to parser tasks

(defun rule-priority (rule)
  (declare (ignore rule))
  1)

