(in-package "MRS")
;;; Globals for YGG

(defparameter *mrs-to-vit* nil)

(defparameter *mrs-for-language* 'english)

(defparameter *mrs-scoping* nil)

(defparameter *alex-munge* nil)

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *top-semantics-type* (vsym "RELN"))

(defparameter *construction-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *rel-name-path* `(,(vsym "RELN") ))

(defparameter *rel-name-path-only* t)

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
(defparameter *psoa-event-path* `(,(vsym "INDEX")))
(defparameter *psoa-liszt-path* `(,(vsym "RESTR") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* nil)

