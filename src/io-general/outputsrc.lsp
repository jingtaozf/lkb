;;; General purpose routines for outputting
;;; types, lexicon etc, in various formats.
;;; Takes functions from the old tdloutput and lilout files

(in-package :cl-user)

#|
;;; all take optional second argument for file name
(output-types :lilfes "Macintosh HD:foo" t)
;;; output-types does local constraints unless second optional
;;; argumant is t when it does features defined for
;;; that type only
;;;
;;; both do all output types including glbtypes
;;; all the options sort the types so that no type
;;; is used before it is defined since PAGE and LILFES
;;; require this
;;; 


;;; by default, the expanded information is output
;;; optional argument allows for local constraints only
(output-lex :lilfes "Macintosh HD:foo1")
(output-grules :lilfes "Macintosh HD:foo2")
(output-lrules :tdl)

(output-lex-and-derived :lilfes "Macintosh HD:foo3")
(output-lex-and-derived :lilfes "~aac/lilfes/lingo/lex.lil" *lex-ids-used*)

(output-lex-and-derived :tdl "Macintosh HD:foo4")

(output-lex-and-derived :ebl "~aac/ebl.lex")
;;; options for syntax are :lilfes :tdl
;;; and :path
|#

(defvar *cached-type-order* nil)

(defun output-types (syntax &optional file-name sig-only-p)
  (unless (member syntax '(:tdl :path :lilfes))
    (error "Unsupported syntax specifier ~A" 
           syntax))
  (unless (or (eq syntax :lilfes) (not sig-only-p))
    (error "Syntax specifier ~A not currently supported for signatures"))
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (let ((type-order (or *cached-type-order*
               (sort-by-appearance-order
                (copy-list (append *ordered-type-list*
                                   *ordered-glbtype-list*)) sig-only-p))))
        (setf *cached-type-order* type-order)
        (for type-name in type-order
             do
             (let ((entry (get-type-entry type-name)))                  
               (ecase syntax
                 (:tdl (output-type-as-tdl type-name entry
                                           ostream))
                 (:path (output-type-as-paths type-name entry
                                              ostream))
                 (:lilfes (output-type-as-lilfes type-name entry
                                                 ostream sig-only-p)))))))))

;;; Neither of these lexical output functions
;;; will work from a cached lexicon

(defun output-lex (syntax &optional file-name local-p lex-ids)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (let ((count 0))
        (for lex-name in (or lex-ids (reverse *ordered-lex-list*))
             do
             (if (> count 100)
               (progn (clear-expanded-lex)
                      (setf count 0))
               (incf count))
             (let ((entry (get-psort-entry lex-name)))
               (if entry
                   (case syntax
                     (:tdl (output-instance-as-tdl lex-name entry
                                                   ostream local-p))
                     (:lilfes 
                      (when local-p
                        (error "Local only output not supported with LiLFeS"))
                      (output-instance-as-lilfes 
                       lex-name entry
                       ostream))
                     (t (error "Unsupported syntax specifier ~A"
                               syntax)))
                 (format t "~%Warning ~A not found" lex-name))))))))


(defun output-lex-and-derived (syntax &optional file-name lex-ids)
  ;;; lexicon and everything that can be derived from it
  ;;; via lexical rule.  Ordered by base form.
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (let ((count 0))
        (for lex-name in (or lex-ids (reverse *ordered-lex-list*))
             do            
             (if (> count 100)
               (progn (clear-expanded-lex)
                      (setf count 0))
               (incf count))
             (setf *number-of-applications* 0)
             (let* ((lex-entry (get-psort-entry lex-name))
                    (lex-entry-fs 
                     (if lex-entry
                       (lex-or-psort-full-fs (get-psort-entry lex-name))
                       (error "Entry for ~A not found" lex-name)))
                    (result-list
                     (cons (cons nil lex-entry-fs)
                           (try-all-lexical-rules 
                            (list (cons nil lex-entry-fs)))))
                    (idno 0))
               (for result-pair in result-list
                    do
                    (let* ((fs (cdr result-pair))
                           (orth (extract-orth-from-fs fs)))
                      (case syntax
                        (:tdl 
                         (output-derived-instance-as-tdl orth fs ostream 
                                                         lex-name idno))
                        (:lilfes 
                         (output-derived-instance-as-lilfes 
                          orth fs ostream lex-name idno))
                        (:ebl
                         (output-for-ebl orth fs ostream (car result-pair)
                                         lex-name lex-entry-fs))
                        (t (error "Unsupported syntax specifier ~A"
                                  syntax))))
                    (incf idno))))))))

(defparameter *infl-rules* '(plur_noun_infl_rule third_sg_fin_verb_infl_rule
                             past_verb_infl_rule psp_verb_infl_rule 
                             subjunctive_verb_infl_rule prp_verb_infl_rule
                             er_comp_adj_infl_rule est_super_adj_infl_rule
                             non_third_sg_fin_verb_infl_rule
                             sing_noun_infl_rule pos_adj_infl_rule
                             no-affix_infl_rule -ly_infl_rule))

                             

(defun output-for-ebl (orth fs ostream rule-list base-id base-fs)
  (declare (ignore fs))
  (let* ((type (type-of-fs (tdfs-indef base-fs)))
         (category (find-possibly-cached-cat type base-fs))
        (infl-rules nil)
        (other-rules nil))
    (for rule in rule-list 
         do
         (if (member rule *infl-rules*)
             (push rule infl-rules)
           (push rule other-rules)))
    (format ostream 
            "~%(~S ~S ~S ~S ~A)" 
            (split-into-words orth) 
            type
            (cons base-id infl-rules)
            other-rules
            category)))

(defvar *cat-type-cache* (make-hash-table))

(defun find-possibly-cached-cat (type fs)
  (let ((cached-cat (gethash type *cat-type-cache*)))
    (or cached-cat
        (let ((cat (find-category-abb fs)))
          (setf (gethash type *cat-type-cache*) cat)
          cat))))



(defun output-grules (syntax &optional file-name local-p)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
        (for rule-name in (reverse *ordered-rule-list*)
             do
             (let ((entry (get-grammar-rule-entry rule-name)))                  
               (case syntax
                 (:tdl (output-instance-as-tdl rule-name entry
                                               ostream local-p))
                 (:lilfes 
                  (when local-p
                    (error "Local only output not supported with LiLFeS"))
                  (output-instance-as-lilfes 
                           rule-name entry
                           ostream))
                 (t (error "Unsupported syntax specifier ~A"
                           syntax))))))))

(defun output-lrules (syntax &optional file-name local-p)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
        (for rule-name in (reverse *ordered-lrule-list*)
             do
             (let ((entry (get-lex-rule-entry rule-name)))                  
               (case syntax
                 (:tdl (output-instance-as-tdl rule-name entry
                                               ostream local-p))
                 (t (error "Unsupported syntax specifier ~A"
                           syntax))))))))


(defun output-root (syntax &optional file-name)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (for root-symbol in (if (listp *start-symbol*) *start-symbol*
                            (list *start-symbol*))
             do
             (let ((entry (get-psort-entry root-symbol)))   
               (if entry 
                   (case syntax
                     (:lilfes 
                      (output-instance-as-lilfes root-symbol entry
                                                 ostream :root))
                     (t (error "Unsupported syntax specifier ~A"
                               syntax)))
                 (format t "~%Warning ~A not found" root-symbol)))))))

;;; Support functions

;;; Sorting types by appearance order

(defvar *complete-order-alist* nil)

(defun sort-by-appearance-order (types sig-only-p)
  (let ((type-order-alist nil)
        (ok t))
    (setf *complete-order-alist* nil)
    (for type in types
         do
         (let ((types-used (extract-used-types type sig-only-p)))
           (push (cons type types-used) type-order-alist)))
    (for type in types
         do
         (construct-all-ref-types type type-order-alist nil))
    (if ok
        (stable-sort 
         types
         #'(lambda (x y) 
             (< (length (assoc x *complete-order-alist*))
                (length (assoc y *complete-order-alist*))))))))

(defun construct-all-ref-types (type type-order-alist types-so-far)
  (when (member type types-so-far)
    (error "~%Mutual recursion involving ~A" type))
  (let ((done (assoc type *complete-order-alist*)))
    (if done (cdr done)
      (let* ((immediate-ref (cdr (assoc type type-order-alist)))
             (all-ref
              (remove-duplicates
               (append immediate-ref
                       (for ref-type in immediate-ref
                            append
                            (construct-all-ref-types ref-type type-order-alist
                                                     (cons type types-so-far)))))))
        (push (cons type all-ref) *complete-order-alist*)
        all-ref))))

(defun extract-used-types (type sig-only-p)
  ;;; just need parents and signature if we're only outputting
  ;;; the signature - otherwise, recurse through the local feature
  ;;; structure
  (declare (special *res*))
  (setf *res* nil)
  (let* ((type-entry (get-type-entry type))
         (type-local-fs (if type-entry (type-local-constraint type-entry))))
    (unless type-entry (error "%Invalid type ~A" type))
    (when type-local-fs
      (for feat in (top-level-features-of type-local-fs)
           do
           (let ((internal-fs (get-dag-value type-local-fs feat)))
             (if sig-only-p
               (pushnew
                (let ((val (type-of-fs internal-fs)))
                  (if (listp val) (car val) val))
                *res* :test #'eq)
               (collect-types-from-fs internal-fs)))))
    (for parent in (type-parents type-entry)
         do 
         (pushnew parent *res*))
    *res*))
         

(defun collect-types-from-fs (dag-instance)
  (declare (special *res*))
  (let ((real-dag (follow-pointers dag-instance)))
      (pushnew 
       (let ((val (type-of-fs real-dag)))
         (if (listp val) (car val) val))
       *res* :test #'eq)
      (unless (is-atomic real-dag)
        (dolist (arc (dag-arcs real-dag))
          (collect-types-from-fs 
           (get-dag-value real-dag (dag-arc-attribute arc)))))))

;;; expand-local-only-constraints is
;;; called from checktypes if hierarchy-only-p is set
;;; It is useful when reading in a set of types
;;; defined for PAGE which do not expand correctly in the
;;; LKB.  Note that it avoids setting the type of the
;;; local-constraint feature structure.

(defun expand-local-only-constraints nil
   (let ((ok t))
     (unmark-type-table)
     (determine-atomic-types)
     (for node in *type-names*
          do
          (let ((type-entry (get-type-entry node)))
            (unless 
                (expand-local-only-constraint node type-entry)
              (setf ok nil))))
     ok))
         

(defun expand-local-only-constraint (node type-entry)
  (let* ((*unify-debug-cycles* t)       ; turn on cyclic dag warning messages
         (constraint-spec (type-constraint-spec type-entry))
         (local-constraint 
          (if constraint-spec (process-unifications constraint-spec))))
    (if (and constraint-spec (null local-constraint))
        (progn
          (format t "~%Type ~A has an invalid constraint specification" node)
          nil)
     (progn
       (setf (type-local-constraint type-entry) local-constraint)
       t))))






