#+mcl (defpackage :common-lisp-user (:nicknames :user :cl-user))

(in-package "USER")

(export '(edge-dag follow-pointers existing-dag-at-end-of dag-p is-atomic
          type-of-fs tdfs-indef lex-or-psort-id lex-or-psort-full-fs 
          dag-arcs subtype-p extend-typed-path path-p typed-path-p
          path-typed-feature-list typed-path-typed-feature-list
          type-feature-pair-p type-feature-pair-feature
; for vitrification
          *ordered-mrs-rule-list*
; for lexlookup
          make-pv-unif construct-tdfs create-wffs 
          process-unifications yadu copy-tdfs-completely))

(defpackage "MRS"
  (:use "COMMON-LISP" "COMMON-LISP-USER" "USER")
  )

; just to avoid errors because package is undefined
(defpackage "DISCO")
(defpackage "TREES")
(defpackage "MAIN")
(defpackage "UNIFY")
(defpackage "TDL")
(defpackage "LEXICON")
(defpackage "PG")
(defpackage "LEX")
(defpackage "TDL-CONTROL")
(in-package "TDL-CONTROL")
(export '(MESSAGE))
(in-package "USER")


#-excl (defpackage "EXCL")










