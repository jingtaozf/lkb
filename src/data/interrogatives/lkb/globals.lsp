;;; Copyright Ann Copestake 1991-1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

;;; Strings
#|
(defparameter *last-parses* '("-----------------------"
                              "These bagels, I like."
                              "These bagels, they think you like."
                              "Whose bagels do you like?"
                              "Whose bagels do they think you like?"
                              "Whose books did Kim read?"
                              "Whose pictures of Sandy did Kim like?"
                              "Sandy's pictures of Chris did Kim like?"
                              "Sandy's pictures of who did Kim like?"
                              ))
|#

#|
(defparameter *last-parses* '("-----------------------"
Kim left.
*Kim leave.
 I insist that Kim leave.
 Kim did not leave.
*I insist that Kim does not leave.
 I insist that Kim not leave.

*Kim did leave.
 Kim DID leave.
 Did/DID/will Kim leave?
 I wonder if/whether Kim left.
*I wonder if/whether did Kim leave.

 Who did/will Sandy see?
*Who Sandy saw?
 Who did/will Sandy think she saw?
*Who did Sandy think did she see?
 I wonder who Sandy saw.
*I wonder who did/will Sandy see.

*Who did Sandy see and __?
 Who did Sandy like __ and Kim hate __?
 Which students do you think Sandy likes __ and Kim hates __?
*Which students do you think Sandy likes __ and Kim hates Lee?
*Which students do you think Sandy likes Lee and Kim hates __?

 Who saw Kim?
*Who did see Kim?
 Who DID see Kim?

 Whose books did Kim read?
 Whose pictures of Sandy did Kim like?
*Sandy's pictures of whom did Kim like?
 I wonder whose pictures of Sandy Kim likes.
 How tall is Sandy?
 How tall do you think Sandy is?
 I wonder how tall Sandy is.
*I wonder how tall is Sandy.

 Who saw WHAT?
 Who gave WHAT to WHOM?
 What did WHO give to WHOM?
 To whom did WHO give WHAT?
 Who wondered who saw WHAT? 
 Who the hell left?
*Kim read WHAT the hell?
*Who saw WHAT the hell?
 I wonder whether/*if the hell they can see that.

 Kim saw WHO?
 Who wondered what WHO saw?
 Sandy's pictures of WHO did Kim like?
 Who saw WHAT?
 Who wondered what WHO saw? 
 Kim is angry?
)
|#
  
(defparameter *toptype* '*top*)

(defparameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; Lexical files

(defparameter *orth-path* '(orth list))

(defparameter *list-tail* '(rest))

(defparameter *list-head* '(first))

(defparameter *lex-rule-suffix* "_INFL_RULE"
 "creates the inflectional rule name from the information
   in irregs.tab - for PAGE compatability")

;;; Parsing

;(defparameter *last-parses* '("Kim wondered whether Sandy had slept"))

(defparameter *chart-limit* 100)

;(defparameter *sign-type* 'synsem-struc
;   "a special type wrt parsing - rule indexing is checked for its
;   descendants")


(defparameter *simple-tree-display* t)

(defparameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")


;;; ?
(defparameter *start-symbol* '(root)
   "specifing valid parses")

;;; ?
(defparameter *morph-rule-type* 'word
    "morphology system checks for rules which are
     of this type or a subtype of it")

(defparameter *maximal-lex-rule-applications* 7
   "The number of lexical rule applications which may be made
   before it is assumed that some rules are applying circularly")

#|

(defparameter *deleted-daughter-features* '(ARGS HEAD-DTR NON-HEAD-DTR LCONJ-DTR RCONJ-DTR)
   "features pointing to daughters deleted on building a constituent")

(defparameter *head-marking-path* '(SYNSEM LOCAL CONT KEY)
   "coreferenced between mother and head daughter")

|#

;;; Generation

(defparameter  *semantics-index-path* '(SEM INDEX))

;;;;;; Parse tree node labels
;;;
;;;;;; the path where the name string is stored
;;;(defparameter *label-path* '(LABEL-NAME))
;;;
;;;;;; the path for the meta prefix symbol
;;;(defparameter *prefix-path* '(META-PREFIX))
;;;
;;;;;; the path for the meta suffix symbol
;;;(defparameter *suffix-path* '(META-SUFFIX))
;;;
;;;;;; the path for the recursive category
;;;(defparameter *recursive-path* '(SYN GAP LIST FIRST))
;;;
;;;;;; the path inside the node to be unified with the recursive node
;;;(defparameter *local-path* nil)
;;;
;;;;;; the path inside the node to be unified with the label node
;;;(defparameter *label-fs-path* nil)
;;;
;;;(defparameter *label-template-type* 'label)


;;; Changing the max number of lex rules before warning...

(defparameter *maximal-lex-rule-applications* 20)