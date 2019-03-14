;;; Hey, emacs(1), this is -*- Mode: Common-Lisp; Package: MRS; -*- got it?

;;; Copyright (c) 1998--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

(eval-when (compile load eval)
  (export '(edge-dag follow-pointers existing-dag-at-end-of dag-p
            type-of-fs tdfs-p tdfs-indef lex-entry-id lex-entry-full-fs 
            dag-arcs subtype-p extend-typed-path path-p typed-path-p
            path-typed-feature-list typed-path-typed-feature-list
            type-feature-pair-p 
            type-feature-pair-feature type-feature-pair-type
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

#-(or :excl :acl-compat) 
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

(defconstant *mrs-package* :lkb)

(let ((vsym-cache (make-hash-table :test #'equal)))
(defun vsym (str)
  ;; allow mrsglobals files to be system independent
  ;; implemented as a memo function since it is called a lot
  ;; use an equal test rather than the weaker case-insensitive equalp, since it is faster
  ;; and the overall behaviour is no different
  ;; *mrs-package* is a constant - since it can't change we don't need to worry about
  ;; the cache contents ever becoming invalid
  (or (gethash str vsym-cache)
    (setf (gethash str vsym-cache) (intern (string-upcase str) *mrs-package*))))
)

;;;
;;; the following functions are basically accessors for those parts of feature
;;; structures that are used in constructing an MRS from a parse result; also,
;;; the functions interfacing to the type system are used in various places,
;;; including the equivalence test on MRSs and the munging machinery.

(defun get-parse-fs (edge)
  ;;
  ;; given a parse result (i.e. whatever structure the parser returns _after_
  ;; unpacking), extract the feature structure (DAG) that includes the MRS.
  ;;
  (let* ((tdfs (edge-dag edge))
         (fs (and (tdfs-p tdfs) (tdfs-indef tdfs))))
    fs))

(defun deref (fs)
  ;;
  ;; given a feature structure, dereference it (i.e. follow pointer, if need
  ;; be).
  ;; _fix_me_
  ;; probably, this should not be exposed through the interface but called by
  ;; all fs-manipulating routines in the interface instead.   (24-aug-03; oe)
  ;;
  (follow-pointers fs))
  
(defun path-value (fs path)
  ;;
  ;; given a feature structure and a list of symbols naming features, extract
  ;; the feature structure under the specified path.
  ;;
  (existing-dag-at-end-of fs path))

(defun is-valid-fs (fs)
  ;;
  ;; given a feature structure, test its validity.
  ;;
  (dag-p fs))

(defun fs-arcs (fs)
  ;;
  ;; given a feature structure, return an association list containing feature
  ;; -- value (aka feature structure) pairs, e.g.
  ;;
  ;;   ((LBL . #D[handle ...]) (WLINK . #D[*cons* ...]) (PRED . #D[*top* ...])
  ;;    (ARG0 . #D[event ...]) (ARG1 . #D[ref-ind ...]))
  ;;
  ;; where features are symbols and values whatever representation is used for
  ;; feature structures in the interface (i.e. integers for PET).
  ;;
  (dag-arcs fs))

(let ((specialist-user-p :unset))
(defun fs-type (fs)
  ;;
  ;; given a feature structure, extract its type.
  ;;
  (when (eq specialist-user-p :unset)
    ;; memo-ise specialist-user-p, since we shouldn't be calling getenv repeatedly
    (let ((user (make:getenv "USER")))
      (setq specialist-user-p
        (member user '("aac" "dan" "danf") :test #'equal))))
  (let ((fs-type (type-of-fs fs)))
    (when (and specialist-user-p
               (> (length (string fs-type)) (length "GLBTYPE"))
               (string= (string fs-type) "GLBTYPE" :end1 (length "GLBTYPE")))
      ;; this is a glbtype and the user is expected to care, so be annoying
      (dotimes (n 5)
        (lkb::lkb-beep)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!"))
      (format t "~%GLBTYPE ~A in MRS" fs-type)
      (dotimes (n 5)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!")))
    fs-type))
)

(defun is-valid-type (type)
  ;;
  ;; given a type, test its validity.
  ;;
  (lkb::is-valid-type type))


(defun is-top-type (type)
  ;;
  ;; given a type, return true if it is the top (i.e. most general) type.
  ;;
  (eql lkb::*toptype* type))


(defun equal-or-subtype (type1 type2)
  ;;
  ;; given two types, return true if .type1. is equal to .type2. or is one of
  ;; its descendants.
  ;;
  (lkb::subtype-or-equal type1 type2))

(defun compatible-types (type1 type2)
  ;;
  ;; given two types, return true if .type1. and .type2. are either identical
  ;; or have a greatest lower bound (common descendant).
  ;;
  (and type1 type2
       (lkb::greatest-common-subtype type1 type2)))

;;;
;;; convert PSOA to LKB dag representation; enables use of DAG browsing tools
;;; for MRS viewing (specifically the emerging LUI AVM browser, while LUI
;;; does not include a specialized MRS browser).             (10-jul-03; oe)
;;;
(defun lui-dagify-mrs (mrs &key (stream t))
  (declare (special *rel-handel-path*))
  
  (let ((cache (make-hash-table)))
    (labels ((dagify-variable (variable)
               (if (var-p variable)
                 (if (gethash variable cache)
                   (format stream "<~(~s~)>" (var-string variable))
                   (loop
                       initially
                         (format
                          stream
                          "<~(~s~)>=~:[~;#D[~]~(~a~)"
                          (var-string variable) 
                          (var-extra variable) (var-type variable))
                       for extra in (var-extra variable)
                       for feature = (extrapair-feature extra)
                       for value = (extrapair-value extra)
                       do (format stream " ~a: ~(\"~a\"~)" feature value)
                       finally
                         (when (var-extra variable) (format stream "]"))
                         (setf (gethash variable cache) variable)))
                 (format stream "?"))))
      (format stream "#D[mrs")
      (when *rel-handel-path*
        (format stream " LTOP: ")
        (dagify-variable (psoa-top-h mrs)))
      (when (psoa-index mrs)
        (format stream " INDEX: ")
        (dagify-variable (psoa-index mrs)))
      (if (psoa-liszt mrs)
        (loop
            with cons = lkb::*non-empty-list-type*
            with null = lkb::*empty-list-type*
            with first = (first lkb::*list-head*)
            with rest = (first lkb::*list-tail*)
            with n = 0
            initially (format stream " RELS:")
            for ep in (psoa-liszt mrs)
            for label = (rel-handel ep)
            for pred = (rel-pred ep)
            for lnk = (output-lnk (rel-lnk ep) :stream nil)
            do
              (format
               stream
               " #D[~(~s~) ~a: #D[\"~(~a~)~@[~a~]\""
               cons first pred lnk)
              (when label 
                (format stream " LBL: ")
                (dagify-variable label))
              (loop
                  for role in (rel-flist ep)
                  for feature = (fvpair-feature role)
                  for value = (fvpair-value role)
                  when (var-p value) do
                    (format stream " ~a: " feature)
                    (dagify-variable value)
                  else do
                    (format stream " ~a: ~s" feature value))
              (format stream "] ~a:" rest)
              (incf n)
            finally 
              (format stream " ~(~a~)" null)
              (loop 
                  for i from 1 to n
                  do (format stream "]"))))
      (if (psoa-h-cons mrs)
        (loop
            with cons = lkb::*non-empty-list-type*
            with null = lkb::*empty-list-type*
            with first = (first lkb::*list-head*)
            with rest = (first lkb::*list-tail*)
            with n = 0
            initially (format stream " HCONS:")
            for hcons in (psoa-h-cons mrs)
            for type = (hcons-relation hcons)
            for hi = (hcons-scarg hcons)
            for lo = (hcons-outscpd hcons)
            do
              (format stream " #D[~(~a~) ~a: #D[~(~a~) HARG: " cons first type)
              (dagify-variable hi)
              (format stream " LARG: ")
              (dagify-variable lo)
              (format stream "] ~a:" rest)
              (incf n)
            finally 
              (format stream " ~(~a~)" null)
              (loop 
                  for i from 1 to n
                  do (format stream "]"))))
      (format stream "]"))))

(defun lui-indexed-mrs (mrs &key (stream t))
  (let ((attic (make-hash-table :test #'equal))
        (id 0))
    (labels ((newp (object) (not (gethash object attic)))
             (record (object)
               (or (gethash object attic)
                   (let ((n id))
                     (setf (gethash object attic) n)
                     (incf id)
                     n)))
             (output (variable stream &optional (newp t))
               (when (var-p variable)
                 (format stream "\"~(~a~)\"" (var-string variable))
                 (when (and newp (var-extra variable))
                   (format stream " \"{\"")
                   (loop
                       for extra in (var-extra variable)
                       do
                         (format
                          stream
                          " \" ~(~a~)\""
                          (extrapair-value extra)))
                   (format stream " \" }\"")))))
      (format stream "#X[~a " (record (psoa-top-h mrs)))
      (output (psoa-top-h mrs) stream)
      (format stream " \" \" ~a " (record (psoa-index mrs)))
      (output (psoa-index mrs) stream)
      (format stream " newline~%\"{ \" #X[")
      (loop
          with eps = (psoa-liszt mrs)
          with last = (first (last eps))
          for ep in eps
          for label = (rel-handel ep)
          for pred = (rel-pred ep)
          for lnk = (output-lnk (rel-lnk ep) :stream nil)
          do
            (format stream "#X[~a " (record label))
            (output label stream)
            (format stream " \":~(~a~)~@[~a~](\"" pred lnk)
            (loop
                for role in (rel-flist ep)
                for value = (fvpair-value role)
                unless (eq role (first (rel-flist ep))) do
                  (format stream " \", \"")
                when (var-p value) do
                  (let ((newp (newp value)))
                    (format stream " ~a " (record value))
                    (output value stream newp))
                else do
                  (format stream "\"~a\"" value))
            (format stream " \")\" ]~@[ newline~]~%    " (not (eq ep last))))
      (format stream "] \" }\" newline~% \"{ \" #X[")
      (loop
          for hcons in (psoa-h-cons mrs)
          for type = (string (hcons-relation hcons))
          for hi = (hcons-scarg hcons)
          for lo = (hcons-outscpd hcons)
          do
            (format stream "#X[~a " (record hi))
            (output hi stream)
            (format
             stream
             " \" ~(~a~) \" "
             (cond
              ((not (stringp type)) "=?")
              ((string-equal type "qeq") "=q")
              ((string-equal type "leq") "<q")
              ((string-equal type "geq") ">q")
              (t "=?")))
            (format stream "~a " (record lo))
            (output lo stream)
            (format stream "] \" \" wrap~%"))
      (format stream "] \"}\" ]"))))
