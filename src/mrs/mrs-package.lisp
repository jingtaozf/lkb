(in-package "COMMON-LISP-USER")

(pushnew :mrs *features*)

#+page
(defpackage "MRS"
  #+acl(:use "COMMON-LISP" "COMMON-LISP-USER" "USER" "LEX")
  #-acl(:use "COMMON-LISP" "COMMON-LISP-USER")
  )

#+lkb
(defpackage "MRS"
  (:use "COMMON-LISP" "COMMON-LISP-USER" "USER"))

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
