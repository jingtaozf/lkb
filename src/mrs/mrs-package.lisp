(in-package "COMMON-LISP-USER")

(pushnew :mrs *features*)

#+lkb
(defpackage "MRS"
  #+acl(:use "COMMON-LISP" "COMMON-LISP-USER" "USER")
  #-acl(:use "COMMON-LISP" "COMMON-LISP-USER")
  )

(in-package "MRS")

(eval-when (compile load eval)
(export '(psoa-top-h psoa-index psoa-liszt psoa-h-cons
          rel-sort rel-handel rel-flist rel-extra
          fvpair-feature fvpair-value
          var-name var-extra var-id
          handle-var-name handle-var-extra handel-var-id
          hcons-scarg hcons-outscpd hcons-relation)))
