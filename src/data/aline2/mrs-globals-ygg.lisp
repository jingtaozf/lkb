(in-package "MRS")
;;; Globals for YGG

(defparameter *initial-semantics-path* `(,(vsym "SEM2") ))

(defparameter *main-semantics-path* `(,(vsym "SEM2") ,(vsym "RESTR1") ,(vsym "LST")))

(defparameter *rel-name-path* `(,(vsym "RELN1") ))

(defparameter *rel-name-path-only* t)

#|
SEM2 [sem-struc
     MODE1 ref
     INDEX1 ref-index <8>
     RESTR1 list-of-predications
           LST 
                HEAD noun_pred
                      RELN1 r_ande
                      SIT1 sit-index
                      INST <8>
                TL <9>
           LAST <9>
|#

(defparameter *value-feats* `(,(vsym "NAME1")))

(defparameter *reln-feature* (vsym "RELN1"))

(defparameter *psoa-handel-path* `(,(vsym "FOO")))
(defparameter *psoa-top-h-path* `(,(vsym "FOO")))
(defparameter *psoa-event-path* `(,(vsym "INDEX1")))
(defparameter *psoa-liszt-path* `(,(vsym "RESTR1") ,(vsym "LST")))
(defparameter *psoa-rh-cons-path* `(,(vsym "FOO")))
(defparameter *psoa-message-path* `(,(vsym "FOO")))
(defparameter *psoa-wgliszt-path* `(,(vsym "FOO")))
(defparameter *psoa-key-handel-path* `(,(vsym "FOO")))

