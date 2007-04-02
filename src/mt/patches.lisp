(in-package :mrs)

;;;
;;; patching a few core MRS functions for LOGON transfer functionality; this
;;; should go back into the MRS code base, ideally, at some point ...
;;;
(#+:allegro excl:without-redefinition-warnings
 #-:allegro eval-when #-:allegro  (:load-toplevel :execute)
            
  (defun determine-variable-type (dag)
    (let ((type (fs-type dag)))
      (cond 
       ((equal-or-subtype type *event-type*) "e")
       ((equal-or-subtype type *ref-ind-type*) "x")
       ((equal-or-subtype type *non_expl-ind-type*) "i")
       ((equal-or-subtype type *handle-type*) "h")
       ((equal-or-subtype type *non_event-type*) "p")
       ((equal-or-subtype type *event_or_index-type*) "i")
       ;;
       ;; _patch_
       ;; add an `anti' type to block MTR application against a bound variable
       ;;
       ((equal-or-subtype type (mrs::vsym "a")) "a")
       (t "u"))))
  
  (defun determine-mrs-class (mrs)
    (cond
     ((loop
          for ep in (psoa-liszt mrs)
          when (member (rel-pred ep) mt::*semi-token-relations* :test #'equal)
          return t)
      :token)
     ((mt:fragmentp mrs) :fragment)))

  (defun determine-ep-class (ep)
    (cond
     ((member (rel-pred ep) mt::*semi-fragment-relations* :test #'equal)
      :fragment)
     ((member (rel-pred ep) mt::*semi-token-relations* :test #'equal)
      :token)
     ((member (rel-pred ep) mt::*semi-punctuation-relations* :test #'equal)
      :token))))


