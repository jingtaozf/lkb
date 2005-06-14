(in-package "MRS")
;;; Globals for YGG

(defparameter *alex-munge* nil)

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *construction-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *top-semantics-type* (vsym "RELN"))

(defparameter *rel-name-path* `(,(vsym "RELN") ))

(defparameter *rel-handel-path* nil)

#|
SEM [sem-struc
     MODE ref
     INDEX ref-index <8>
     RESTR list-of-predications
           LIST 
                FIRST noun_pred
                      RELN r_ande
                      SIT sit-index
                      INSTANCE <8>
                REST <9>
           LAST <9>
|#

(defparameter *value-feats* `(,(vsym "NAME")))

(defparameter *psoa-top-h-path* nil)
(defparameter *psoa-liszt-path* `(,(vsym "RESTR") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* nil)

(defparameter *fix-spelling-fn* 'lkb::fix-spelling)

;;; for generation to work, we set the following

;;; we can only generate from the results of parsing because we have no filter 
;;; rules
(defparameter *null-semantics-hack-p* t)

;;; the equality check currently (May 2005) doesn't work with the
;;; handle-free MRS.  This needs to be fixed.
(defparameter lkb::*bypass-equality-check* t)
