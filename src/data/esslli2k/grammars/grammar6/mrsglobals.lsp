(in-package "MRS")

(defparameter *mrs-to-vit* nil)

(defparameter *mrs-for-language* 'english)

(defparameter *mrs-scoping* nil)

(defparameter *alex-munge* nil)

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RELS") ,(vsym "LIST")))

(defparameter *construction-semantics-path* nil)

(defparameter *top-semantics-type* (vsym "RELATION"))

(defparameter *rel-name-path* `(,(vsym "PRED") ))

(defparameter *rel-name-path-only* t)

(defparameter *rel-handel-path* nil)

#|
SEM [semantix
     INDEX index <8>
     KEY <1>
     RESTR *dlist*
           LIST 
	        FIRST <1>
	              arg1-relation
                      RELN bark_rel
                      ARG0 <8>
                      ARG1 index
                REST *dlist* <2>
           LAST *dlist* <2>
|#

(defparameter *value-feats* `(,(vsym "NAME")))

(defparameter *psoa-top-h-path* nil)
(defparameter *psoa-event-path* `(,(vsym "INDEX")))
(defparameter *psoa-liszt-path* `(,(vsym "RELS") ,(vsym "LIST")))
(defparameter *psoa-rh-cons-path* nil)

(defparameter *ref-ind-type* (vsym "object"))

