(in-package :cl-user)

(eval-when (compile load eval)
(export '(edge-dag follow-pointers existing-dag-at-end-of dag-p is-atomic
          type-of-fs tdfs-indef lex-or-psort-id lex-or-psort-full-fs 
          dag-arcs subtype-p extend-typed-path path-p typed-path-p
          path-typed-feature-list typed-path-typed-feature-list
          type-feature-pair-p type-feature-pair-feature type-feature-pair-type
          *parse-record* *toptype*
; for vitrification
          *ordered-mrs-rule-list*
          make-funny-unification funny-unification-rhs
          funny-unification-lhs funny-unification-p
          mrs-rule-sexp-p  mrs-rule-sexp-value
          mrs-rule-predicate-p mrs-rule-predicate-value
          mrs-rule-constant-p mrs-rule-constant-value
; for lexlookup
          make-pv-unif construct-tdfs create-wffs 
          process-unifications yadu)))

#|
; just to avoid errors because package is undefined
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (unless (find-package "TREES")
    (defpackage "TREES")))              ; used in acl-mrs.lisp
|#

#-excl (defpackage "EXCL")

(in-package "MRS")

(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) "CL-USER"))










