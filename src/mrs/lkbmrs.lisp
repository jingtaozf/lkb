;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; Variant of the mrs extraction code for the LKB
;;; This redefines some functions used by PAGE

;;; The functions in patches/mrsfns are those actually called from PAGE
;;; call extract-mrs and extract-and-output
;;; on *parse-record* for the LKB

;;; The following functions are basically PAGE fs accessors,
;;; defined here to allow the code in mrsoutput
;;; to be used with the LKB as well as PAGE

(defun get-parse-fs (edge)
  (let ((fs (tdfs-indef (edge-dag edge))))
    fs))

;;; disposed of get-category-label

(defun output-parse-tree (tree stream)
  (format stream "~A" tree))
;;; called from mrsfns.lsp
;;; this is a temporary expedient
;;; page version calls kh-parse-tree

(defun get-last-sentence nil
  lkb::*sentence*)
;; *last-parses* is not defined for tty version
;;  (car lkb::*last-parses*))  

(defun deref (fs)
  ;;; just a guess, but can't do any harm ...
  (follow-pointers fs))

(defun cyclic-p (fs)
  ;;; new version of unifier can't create cyclic fs
  (declare (ignore fs))
  nil)
  
(defun path-value (fs path)
  (existing-dag-at-end-of fs path))

(defun is-disjunctive-fs (fs)
  (declare (ignore fs))
  ;  damn well better not be !
  nil)

(defun is-valid-fs (fs)
  (and fs (dag-p fs)))

(defun fs-arcs (dag-instance)
  ;;; assumed to return an association list
  (dag-arcs dag-instance))

(defun no-path-to (path-value-res)
;;; for PAGE this is (eql path-value-res 'unify::*fail*)
  (null path-value-res))

(defun fs-type (fs)
  ;;; also defined for PAGE
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

(defun equal-or-subtype (type1 type2)
  (or (equal type1 type2)
      (subtype-p type1 type2)))

(defun is-top-type (val)
;;; also defined for PAGE
  (eql lkb::*toptype* val))

(defun tdl-precedes (type1 type2)
  (or (equal type1 type2)
      (subtype-p type2 type1)))

(defun compatible-types (type1 type2)
  (if (and (null type1) (null type2))
      t ; *** fudge, since sometimes erroneously gets called with nil args
      (lkb::greatest-common-subtype type1 type2)))

(defun is-valid-type (val)
  (lkb::is-valid-type val))

(defun determine-variable-type (fs)
  (let ((type (create-type (fs-type fs))))
    (cond ((equal-or-subtype type *event-type*) "e")
          ((equal-or-subtype type *conj-ind-type*) "e")
          ((equal-or-subtype type *ref-ind-type*) "x")
          ((equal-or-subtype type *full_ref-ind-type*) "x")
          ((equal-or-subtype type *deg-ind-type*) "d")
          ((equal-or-subtype type *non_expl-ind-type*) "v")
          ((equal-or-subtype type *event_or_index-type*) "e")
          ((equal-or-subtype type *eventtime-type*) "t")
          ((equal-or-subtype type *handle-type*) "h")  
          ((equal-or-subtype type *hole-type*) "h")
          ((equal-or-subtype type *label-type*) "h")
          ;;((equal-or-subtype type *individual-type*) "d")
          ((equal-or-subtype type *difference-list-type*) "c") 
          ;; Assume coordination structure
          (t "v"))))

;;;
;;; convert PSOA to LKB dag representation; enables use of DAG browsing tools
;;; for MRS viewing (specifically the emerging LUI AVM browser, while LUI
;;; does not include a specialized MRS browser).             (10-jul-03; oe)
;;;
(defun psoa-to-dag (mrs)
  (let ((dag (lkb::make-dag :type 'lkb::mrs))
        (cache (make-hash-table :test #'equal)))
    (setf (lkb::dag-arcs dag)
      (list
       (lkb::make-dag-arc
        :attribute (vsym "LTOP")
        :value (lkb::make-dag :type (var-name (psoa-top-h mrs))))
       (lkb::make-dag-arc 
        :attribute (vsym "INDEX") 
        :value (lkb::make-dag :type (var-name (psoa-index mrs))))
       (lkb::make-dag-arc
        :attribute (vsym "RELS")
        :value (loop
                   with dags = nil
                   for ep in (psoa-liszt mrs)
                   for predicate = (or (rel-reltype ep) (rel-sort ep))
                   for handel = (let* ((foo (rel-handel ep))
                                       (bar (when (handle-var-p foo)
                                              (var-name foo))))
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
                                                     (var-name foo)))))
                                         (lkb::make-dag :type bar))
                           for arc = (lkb::make-dag-arc 
                                      :attribute feature :value value)
                           for extras = (when (var-p foo)
                                          (var-extra foo))
                           do
                             (when (and extras 
                                        (not (gethash (var-name foo) cache)))
                               (setf (gethash (var-name foo) cache) foo)
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
                   for relation = (hcons-relation hcons)
                   for hi = (let ((foo (hcons-scarg hcons)))
                              (when (var-p foo) 
                                (lkb::make-dag :type (var-name foo))))
                   for lo = (let ((foo (hcons-outscpd hcons)))
                              (when (var-p foo) 
                                (lkb::make-dag :type (var-name foo))))
                   for dag = (lkb::make-dag 
                                 :type (intern (string relation) :lkb))
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

