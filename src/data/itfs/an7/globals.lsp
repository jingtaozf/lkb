;;; Globals file

;;; Types

(def-lkb-parameter *toptype* '*top*)

(def-lkb-parameter *string-type* 'string
   "a special type name - any lisp strings are subtypes of it")

;;; modified for exercise 5.1.1-4
;;; (def-lkb-parameter *orth-path* '(orth)) --> (def-lkb-parameter *orth-path* '(orth list first))
(def-lkb-parameter *orth-path* '(orth list first))

(def-lkb-parameter *list-tail* '(rest))

(def-lkb-parameter *list-head* '(first))

;;; added for exercise 5.1.1-4
(def-lkb-parameter *diff-list-type* '*diff-list*)

;;; added for exercise 5.1.1-4
(def-lkb-parameter *diff-list-list* 'list)

;;; added for exercise 5.1.1-4
(def-lkb-parameter *diff-list-last* 'last)

;;;

(def-lkb-parameter *display-type-hierarchy-on-load* t)

(def-lkb-parameter *simple-tree-display* t)

;;; Parsing

(def-lkb-parameter *mother-feature* NIL
   "The feature giving the mother in a grammar rule")

(def-lkb-parameter *start-symbol* '(start)
   "specifing valid parses")












