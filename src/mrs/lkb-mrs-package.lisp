#+mcl (defpackage :common-lisp-user (:nicknames :user :cl-user))

(in-package "USER")

#+:lkb
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
          process-unifications yadu))

(defpackage "MRS"
  (:use "COMMON-LISP" "COMMON-LISP-USER" "USER")
  )

; just to avoid errors because package is undefined
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (unless (find-package "DISCO")
    (defpackage "DISCO"))               ; still used in time-convert
  (unless (find-package "TREES")
    (defpackage "TREES")))              ; used in acl-mrs.lisp


(in-package "MRS")

(export '(psoa-handel psoa-top-h psoa-index psoa-liszt psoa-h-cons
          psoa-message psoa-wgliszt
          rel-extra rel-type rel-sort rel-handel rel-label rel-flist
          fvpair-feature fvpair-value
          var-name var-extra var-id
          handle-var-name handle-var-extra handel-var-id
          group-var-name group-var-extra group-var-id
          hcons-scarg hcons-cands hcons-outscpd
          leq-sc-scarg leq-sc-cands leq-sc-outscpd leq-sc-relation
          whg-id-id whg-id-word whg-id-handel))

(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) "USER"))


(in-package "USER")


#-excl (defpackage "EXCL")










