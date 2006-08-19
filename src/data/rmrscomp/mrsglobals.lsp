(in-package "MRS")

(defparameter *initial-semantics-path* `(,(vsym "SEM") ))

(defparameter *main-semantics-path* `(,(vsym "SEM") ,(vsym "RELS") ,(vsym "LIST")))

(defparameter *construction-semantics-path* nil)

(defparameter *dummy-relations* `("no_rel"))

(defparameter *psoa-top-h-path* nil
  "NIL because no messages")

(defparameter *top-level-rel-types* 
    '("it_rel" "she_rel" "he_rel" "they_rel" "her_rel" "kim_rel" "sandy_rel"))

;;; parameters for slot detection

(defparameter *algebra-ignore-feats* '(lkb::arg-s lkb::kcmp))

(defparameter *algebra-ignore-paths*
    '((lkb::args) (lkb::c-cont)
      (lkb::sem)
      (lkb::synsem lkb::local lkb::cont)))

;;; following is for naming of slots
(defparameter *non-slot-features* '(lkb::SEM lkb::cont lkb::HOOK lkb::SYNSEM 
				    lkb::LOCAL lkb::cat lkb::val lkb::head))

(defparameter *fix-spelling-fn* 'lkb::fix-spelling)



