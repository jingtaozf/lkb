;;; Hey, emacs, this file is -*- Mode: common-lisp; -*- ... got that?

;;;
;;; a set of functions that facilitate user-level customization of the LKB.
;;;


(defun establish-linear-precedence (rule)
  ;;
  ;; determine surface order of constituents in rule: returns list of paths
  ;; into feature structure of rule, i.e. (nil (args first) (args rest first))
  ;; for a binary rule, where the first list element is the path to the mother
  ;; node of the rule.
  ;;
  (let ((daughters
         (loop
             for args = (existing-dag-at-end-of rule '(args))
             then (existing-dag-at-end-of args *list-tail*)
             for daughter = (when args 
                              (get-value-at-end-of args *list-head*))
             for path = (list 'args) then (append path *list-tail*)
             while (and daughter (not (eq daughter 'no-way-through)))
             collect (append path *list-head*))))
    (if (null daughters)
      (cerror "Ignore it" "Rule without daughters")
      (cons nil daughters))))


(defun spelling-change-rule-p (rule)
  ;;
  ;; detect rules that have orthographemic variation associated to them; those 
  ;; who do should only be applied within the morphology system; this version
  ;; is a little complicated because we change from a full-form set-up to one
  ;; with on-line morphology during the course.
  ;;
  (let ((rule-type (type-of-fs (tdfs-indef (rule-full-fs rule)))))
    (when (or (eql rule-type 'word)
              (subtype-p rule-type 'word)
              (eql rule-type 'lexeme)
              (subtype-p rule-type 'lexeme))
      (let* ((mother (tdfs-indef (rule-full-fs rule)))
             (morth (existing-dag-at-end-of mother '(ORTH)))
             (daughter (existing-dag-at-end-of mother '(ARGS FIRST)))
             (dorth (existing-dag-at-end-of daughter '(ORTH))))
        (not (eq morth dorth))))))


(defun make-orth-tdfs (orthography)
  ;;
  ;; create feature structure representation of orthography value for insertion
  ;; into the output structure of inflectional rules.
  ;;
  (let* ((unification
          (make-unification :lhs (create-path-from-feature-list *orth-path*)
                            :rhs (make-u-value :type orthography)))
         (indef (process-unifications (list unification))))
    (when indef
      (make-tdfs :indef (create-wffs indef)))))
