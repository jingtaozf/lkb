(in-package "MRS")
;;; Globals for YGG

(defparameter *mrs-to-vit* nil)

(defparameter *mrs-for-language* 'english)

(defparameter *mrs-scoping* nil)

(defparameter *alex-munge* nil)

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *construction-semantics-path* `(,(vsym "SEM") ,(vsym "RESTR") ,(vsym "LIST")))

(defparameter *top-semantics-type* (vsym "RELN"))

(defparameter *rel-name-path* `(,(vsym "RELN") ))

(defparameter *rel-name-path-only* t)

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

(defparameter *reln-feature* (vsym "RELN"))

(defparameter *psoa-handel-path* `(,(vsym "FOO")))
(defparameter *psoa-top-h-path* `(,(vsym "FOO")))
(defparameter *psoa-event-path* `(,(vsym "INDEX")))
(defparameter *psoa-liszt-path* `(,(vsym "RESTR") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* `(,(vsym "FOO")))
(defparameter *psoa-message-path* `(,(vsym "FOO")))
(defparameter *psoa-wgliszt-path* `(,(vsym "FOO")))
