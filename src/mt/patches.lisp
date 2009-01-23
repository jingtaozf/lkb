(in-package :mrs)

;;;
;;; patching a few core MRS functions for LOGON transfer functionality; this
;;; should go back into the MRS code base, ideally, at some point ...
;;;
(#+:allegro excl:without-redefinition-warnings
 #-:allegro eval-when #-:allegro  (:load-toplevel :execute)
            
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


