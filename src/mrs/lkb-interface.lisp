;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(eval-when (compile load eval)
(export '(edge-dag follow-pointers existing-dag-at-end-of dag-p
          type-of-fs tdfs-indef lex-or-psort-id lex-or-psort-full-fs 
          dag-arcs subtype-p extend-typed-path path-p typed-path-p
          path-typed-feature-list typed-path-typed-feature-list
          type-feature-pair-p type-feature-pair-feature type-feature-pair-type
          *parse-record* *toptype*
          ;; for vitrification
          *ordered-mrs-rule-list*
          make-funny-unification funny-unification-rhs
          funny-unification-lhs funny-unification-p
          mrs-rule-sexp-p  mrs-rule-sexp-value
          mrs-rule-predicate-p mrs-rule-predicate-value
          mrs-rule-constant-p mrs-rule-constant-value
          ;; for lexlookup
          make-pv-unif construct-tdfs create-wffs 
          process-unifications yadu)))

#-:excl 
(defpackage "EXCL")

#-:lkb
(defun define-break-characters (char-list)
   (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char char-list)
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(in-package :mrs)

(defun vsym (str) 
  ;;; allow mrsglobals files to be system independent
  (intern (string-upcase str) :lkb))
