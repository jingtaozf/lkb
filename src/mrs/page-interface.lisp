(defpackage "MRS"
  (:use "COMMON-LISP" "COMMON-LISP-USER" "USER")
  )

(in-package "USER")

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

(in-package "MRS")

(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) lex:*lex-package*))

;;
;; unfortunately, all TDL domains have a few symbols from the :tdl package; the
;; vsym() mechanism has no way to tell and, potentially, when we load() MRS
;; the :disco domain is not yet defined |:-{.             (25-aug-98  -  oe)
;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (import 'tdl::*diff-list* lex:*lex-package*))

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



