;;; Hey, emacs(1), this is -*- Mode: Common-Lisp; Package: MRS; -*- got it?

;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(eval-when (compile load eval)
  (export '(edge-dag follow-pointers existing-dag-at-end-of dag-p
            type-of-fs tdfs-indef lex-entry-id lex-entry-full-fs 
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

(defconstant *mrs-package* :lkb)

(defun vsym (str) 
  ;;; allow mrsglobals files to be system independent
  (intern (string-upcase str) *mrs-package*))

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
  (let ((fs (tdfs-indef (edge-dag edge))))
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
  (and fs (dag-p fs)))

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

(defun fs-type (fs)
  ;;
  ;; given a feature structure, extract its type.
  ;;
  (let* ((real-type (type-of-fs fs)))
    (when (and #+allegro 
               (let ((user (system:getenv "USER")))
                 (member user '("aac" "dan" "danf") :test #'string-equal))
               #-allegro 
               nil
               (search "GLBTYPE" (if (stringp real-type)
                                   real-type
                                   (symbol-name real-type))))
      ;;; if there's a glbtype, and the user is expected to care, be annoying
      (dotimes (n 5)
        (lkb::lkb-beep)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!" real-type))
      (format t "~%GLBTYPE ~A in MRS" real-type)
      (dotimes (n 5)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!" real-type)))
    real-type))

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
  ;; given two types, return true if .type1. is equal to .type2. or one of its
  ;; descendants.
  ;;
  (or (equal type1 type2)
      (subtype-p type1 type2)))

(defun compatible-types (type1 type2)
  ;;
  ;; given two types, return true if .type1. and .type2. are either identical
  ;; or have a greatest lower bound (common descendant).
  ;;
  (or (eq type1 type2) (lkb::greatest-common-subtype type1 type2)))

;;;
;;; convert PSOA to LKB/ERG dag representation; 
;;; enables use of DAG browsing tools
;;; for MRS viewing (specifically the emerging LUI AVM browser, while LUI
;;; does not include a specialized MRS browser).             (10-jul-03; oe)
;;;

;;; Note - this is entirely ERG specific and should be FIXed - aac

(defun psoa-to-dag (mrs)
  (let ((dag (lkb::make-dag :type 'lkb::mrs))
        (cache (make-hash-table :test #'equal)))
    (setf (lkb::dag-arcs dag)
      (list
       (lkb::make-dag-arc
        :attribute (vsym "LTOP")
        :value (lkb::make-dag :type (var-string (psoa-top-h mrs))))
       (lkb::make-dag-arc 
        :attribute (vsym "INDEX") 
        :value (lkb::make-dag :type (var-string (psoa-index mrs))))
       (lkb::make-dag-arc
        :attribute (vsym "RELS")
        :value (loop
                   with dags = nil
                   for ep in (psoa-liszt mrs)
                   for predicate = (rel-pred ep)
                   for handel = (let* ((foo (rel-handel ep))
                                       (bar (when (is-handel-var foo)
                                              (var-string foo))))
                                  (when bar (lkb::make-dag :type bar)))
                   for flist = (rel-flist ep)
                   when handel do
                     (let ((dag (lkb::make-dag 
                                 :type (intern (string predicate) :lkb))))
                       (loop
                           with arcs = (list (lkb::make-dag-arc 
                                              :attribute (vsym "LBL")
                                              :value handel))
                           for pair in flist
                           for feature = (mrs:fvpair-feature pair)
                           for foo = (mrs:fvpair-value pair)
                           for value = (let* ((bar (cond
                                                    ((stringp foo) foo)
                                                    ((var-p foo) 
                                                     (var-string foo)))))
                                         (lkb::make-dag :type bar))
                           for arc = (lkb::make-dag-arc 
                                      :attribute feature :value value)
                           for extras = (when (var-p foo)
                                          (var-extra foo))
                           do
                             (when (and extras 
                                        (not (gethash (var-string foo) cache)))
                               (setf (gethash (var-string foo) cache) foo)
                               (loop
                                   with arcs = nil
                                   for extra in extras
                                   for efeature = (extrapair-feature extra)
                                   for evalue = (lkb::make-dag
                                                 :type (extrapair-value extra))
                                   for earc = (lkb::make-dag-arc
                                               :attribute efeature
                                               :value evalue)
                                   do
                                     (push earc arcs)
                                   finally
                                     (setf (lkb::dag-arcs value)
                                       (nreverse arcs))))
                             (push arc arcs)
                           finally
                             (setf (lkb::dag-arcs dag) (nreverse arcs)))
                       (push dag dags))
                   finally (return (lkb::list-to-dag (nreverse dags)))))
       (lkb::make-dag-arc
        :attribute (vsym "HCONS")
        :value (loop
                   with dags = nil
                   for hcons in (psoa-h-cons mrs)
 ;;;                   for relation = (hcons-relation hcons)
 ;;; redundant currently cos always a qeq - FIX eventually
                   for hi = (let ((foo (hcons-scarg hcons)))
                              (when (var-p foo) 
                                (lkb::make-dag :type (var-string foo))))
                   for lo = (let ((foo (hcons-outscpd hcons)))
                              (when (var-p foo) 
                                (lkb::make-dag :type (var-string foo))))
                   for dag = (lkb::make-dag 
			      :type (vsym "qeq"))
			     ;;; FIX - should be *qeq-type*
			     ;;; but this file is read in before 
			     ;;; mrsglobals because vsym defined here
			     ;;; obviously this is not a good idea
			     ;;; given that this code should all be making
			     ;;; use of mrsglobals - but leave for now
			     ;;; since this is evidently all a hack ...
                   when (and hi lo) do
                     (setf (lkb::dag-arcs dag)
                       (list
                        (lkb::make-dag-arc 
                         :attribute (vsym "HARG") :value hi)
                        (lkb::make-dag-arc 
                         :attribute (vsym "LARG") :value lo)))
                     (push dag dags)
                   finally (return (lkb::list-to-dag (nreverse dags)))))))
    dag))

