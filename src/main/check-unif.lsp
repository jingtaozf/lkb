;;; Copyright John Carroll 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 


#|
;;; find ALL failing paths, then order them

;;; performs similar function to reordering features so most common failing
;;; features come first - here, the most commonly failing paths are appended to
;;; front of feature structures, in order of most commonly failing first

;;; NB must make sure only to collect when actually parsing

(defvar *inside-unify-dags* nil)
(defvar *unify-dags-fail* nil)

(defparameter *fail-path-list* nil)
(defparameter *unify-dags-fail-count* 0)

(defun unify-dags (dag1 dag2)
   (if *within-unification-context-p*
      (progn #+:mcl(decf bb (CCL::%HEAP-BYTES-ALLOCATED))
             (prog1
                (let ((*inside-unify-dags* t)
                      (*unify-dags-fail* nil))
                   (incf *unify-dags-fail-count*)
                   (catch '*fail*
                      (unify1 dag1 dag2 nil)
                      (if *unify-dags-fail* nil dag1)))
                #+:mcl(incf bb (CCL::%HEAP-BYTES-ALLOCATED))
                ))
      (with-unification-context (dag1) (when (unify-dags dag1 dag2) (copy-dag dag1)))))

(defun unify2 (dag1 dag2 path)
   (multiple-value-bind (new-type constraintp)
         (find-gcsubtype (unify-get-type dag1) (unify-get-type dag2))
      (if (or new-type *inside-unify-dags*)
         (progn
            (unless new-type
               (setq *unify-dags-fail* t)
               (let* ((p (reverse path))
                      (item (assoc p *fail-path-list* :test #'equal)))
                  (unless item
                     (setq item (cons p (make-hash-table)))
                     (push item *fail-path-list*))
                  (setf (gethash *unify-dags-fail-count* (cdr item)) t)))
            (setf (dag-new-type dag1) new-type)
            (if (type-spec-atomic-p new-type)
               (if (or (dag-arcs dag1) (dag-comp-arcs dag1)
                       (dag-arcs dag2) (dag-comp-arcs dag2))
                  (progn
                     (when *unify-debug*
                        (format t "~%Unification failed due to atomic/~
                           non-atomic clash at path ~:A" (reverse path)))
                     (throw '*fail* nil))
                  (setf (dag-forward dag2) dag1))
               (progn
                  ;; unify in constraints if necessary - may have to copy them to
                  ;; prevent separate uses of same constraint in same unification
                  ;; becoming reentrant
                  (when (and constraintp *unify-wffs*)
                     (let ((constraint (may-copy-constraint-of new-type)))
                        (if *unify-debug*
                           (let ((res
                                   (catch '*fail* (unify1 dag1 constraint path))))
                              (unless res
                                 (format t 
                                    "~%Unification with constraint of type ~A failed ~
                                    at path ~:A" new-type (reverse path))
                                 (throw '*fail* nil)))
                           (unify1 dag1 constraint path)))
                     ;; dag1 might just have been forwarded so dereference it again
                     (setq dag1 (deref-dag dag1)))
                  (setf (dag-copy dag1) :inside)
                  ;; cases for each of dag1 and dag2 where they have no arcs
                  ;; just considering straightforward use of unify1: if we've previously
                  ;; visited a node with no arcs then it must have
                  ;; got forwarded then so we won't ever visit it again - so no need to
                  ;; test for presence of any comp-arcs
                  ;; BUT: unify-paths-dag-at-end-of1 adds to comp-arcs independently
                  ;; so we do need the additional tests
                  (cond
                     ((and (null (dag-arcs dag1)) (null (dag-comp-arcs dag1)))
                        (setf (dag-new-type dag2) new-type)
                        (setf (dag-forward dag1) dag2))
                     ((and (null (dag-arcs dag2)) (null (dag-comp-arcs dag2)))
                        (setf (dag-forward dag2) dag1))
                     (t
                        (setf (dag-forward dag2) dag1)
                        (unify-arcs dag1 dag2 path)))
                  (setf (dag-copy dag1) nil))))
         (progn
            (when *unify-debug*
               (format t "~%Unification of ~A and ~A failed at path ~:A" 
                  (unify-get-type dag1) (unify-get-type dag2) (reverse path)))
            (throw '*fail* nil)))))

;;;

(setq *check-paths* nil)
(setq *fail-path-list* nil)
(setq *unify-dags-fail-count* 0)

;run parser

(parse-tsdb-sentences "Macintosh HD:lkb99-expt:big:itemsamp30"
     "Macintosh HD:lkb99-expt:big:resultsamp30")

;;;

(defun extract-check-paths (fail-path-list)
   (when fail-path-list
      (let ((max 0) (max-item nil))
         (dolist (item fail-path-list)
            (when (> (hash-table-count (cdr item)) max)
               (setq max (hash-table-count (cdr item)))
               (setq max-item item)))
         (cons (cons (car max-item) max)
            (extract-check-paths
               (mapcan
                  #'(lambda (item)
                      (let ((item-table (cdr item)))
                         (maphash
                            #'(lambda (key val)
                                (declare (ignore val))
                                (remhash key item-table))
                            (cdr max-item))
                         (when (> (hash-table-count item-table) 0)
                            (list (cons (car item) item-table)))))
                  (remove max-item fail-path-list :test #'eq)))))))

(setq *check-paths* (extract-check-paths *fail-path-list*))

(mapc #'print *check-paths*)

|#


;;;

(defparameter *check-paths* nil)

(setq *check-paths* '(
((SYNSEM LOCAL CAT HEAD) . 250690) 
((SYNSEM LOCAL CAT VALENCE COMPS FIRST OPT) . 144867) 
((INFLECTED) . 100348) 
((SYNSEM LOCAL CONT KEY) . 87957) 
((SYNSEM LOCAL CAT VALENCE COMPS) . 76012) 
((SYNSEM LOCAL CAT VALENCE SUBJ) . 44495) 
((SYNSEM LOCAL CAT ROOT) . 27268) 
((SYNSEM LOCAL CAT VALENCE SUBJ FIRST LOCAL CAT HEAD CASE) . 25531) 
((SYNSEM LOCAL CONJ) . 23818) 
((SYNSEM LOCAL CAT HEAD MOD) . 16292) 
((SYNSEM NON-LOCAL SLASH LAST) . 13786) 
((SYNSEM LOCAL CAT HEAD VFORM) . 12790) 
((SYNSEM LOCAL CAT VALENCE SPR FIRST LOCAL CONT KEY) . 8029) 
((SYNSEM LOCAL CONT MESSAGE) . 7206) 
((SYNSEM LOCAL CAT VALENCE SUBJ FIRST OPT) . 5612) 
((SYNSEM LOCAL CAT VALENCE SPR FIRST OPT) . 5480) 
((SYNSEM LOCAL CAT VALENCE SPR) . 3967) 
((SYNSEM LOCAL CAT HEAD INV) . 3562) 
((SYNSEM) . 3490) 
((SYNSEM NON-LOCAL SLASH LIST) . 3302) 
((SYNSEM LOCAL CAT VALENCE SUBJ FIRST) . 1991) 
((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD) . 1601) 
((SYNSEM LOCAL CAT HEAD MOD CAT HEAD) . 1474) 
((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD MOD CAT HEAD) . 1409) 
((SYNSEM LOCAL CAT POSTHEAD) . 1263) 
((SYNSEM LOCAL CAT VALENCE SUBJ FIRST LOCAL CONT INDEX) . 1124) 
((SYNSEM LOCAL CONT INDEX) . 1099) 
((SYNSEM LOCAL CAT VALENCE COMPS FIRST) . 1005) 
;180
((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD MOD) . 897) 
((ARGS FIRST AFFIX) . 845) 
;179
;((SYNSEM LOCAL CTXT ACTIVATED) . 534) 
;((SYNSEM NON-LOCAL SLASH LIST REST) . 487) 
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST NON-LOCAL SLASH LAST) . 453) 
;180
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST NON-LOCAL SLASH LIST) . 394) 
;((SYNSEM LOCAL CAT VALENCE COMPS FIRST LOCAL CAT HEAD) . 381) 
;((SYNSEM LOCAL CAT VALENCE COMPS REST FIRST OPT) . 376) 
;((SYNSEM LOCAL CAT HEAD PRD) . 283) 
;((SYNSEM LOCAL CAT HEAD MOOD) . 266) 
;((SYNSEM LOCAL ARG-S) . 266) 
;((SYNSEM LOCAL CAT HEAD MOD CONT INDEX) . 253) 
;((SYNSEM LOCAL CAT VALENCE SPR FIRST LOCAL CAT HEAD) . 244) 
;(NIL . 202) 
;192
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST NON-LOCAL SLASH LAST REST) . 200) 
;((SYNSEM LOCAL CONT --STEMLISZT LIST FIRST INST PNG PN) . 191) 
;((SYNSEM LOCAL CAT VALENCE COMPS REST) . 172) 
;((SYNSEM LOCAL CAT HEAD MOD CAT VALENCE SPR) . 167) 
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST LOCAL CAT HEAD) . 153) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD MOD CONT KEY) . 131) 
;((SYNSEM LOCAL CAT VALENCE COMPS FIRST NON-LOCAL SLASH LAST REST) . 125) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST AGR PNG PN) . 120) 
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST LOCAL AGR PNG PN) . 117) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT VALENCE SUBJ) . 104) 
;((SYNSEM LOCAL CAT VALENCE COMPS FIRST LOCAL CAT HEAD CASE) . 74) 
;((SYNSEM LOCAL CAT HEAD MOD CAT VALENCE SUBJ) . 53) 
;((SYNSEM LOCAL CONT LISZT LAST FIRST) . 31) 
;((SYNSEM LOCAL CONJ CHEAD LEFT FIRST) . 30) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CONT KEY) . 30) 
;((SYNSEM LOCAL CAT VALENCE SPR FIRST LOCAL CONT KEY BV DIVISIBLE) . 29) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD PRD) . 28) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD CASE) . 27) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CONT INDEX) . 21) 
;((SYNSEM LOCAL CAT HEAD POSS) . 19) 
;((SYNSEM LOCAL CAT HEAD MOD CONT KEY) . 17) 
;((SYNSEM LOCAL CONJ CHEAD LEFT REST) . 16) 
;((SYNSEM LOCAL CONT LISZT LIST FIRST) . 14) 
;((SYNSEM LOCAL CONT INDEX DIVISIBLE) . 14) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD MOD CAT VALENCE SPR) . 12) 
;((SYNSEM LOCAL CAT VALENCE SUBJ FIRST NON-LOCAL SLASH LIST FIRST CAT HEAD) . 10) 
;((SYNSEM LOCAL CONT ECONT KEY) . 8) 
;((SYNSEM LOCAL CAT HEAD CASE) . 7) 
;((SYNSEM LOCAL CAT HEAD MOD CAT HEAD TENSE) . 5) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT HEAD MOD CONT LISZT LIST REST REST REST FIRST) . 5) 
;((SYNSEM LOCAL CAT HEAD MOD CONT INDEX PNG PN) . 5) 
;((SYNSEM LOCAL CAT VALENCE COMPS FIRST LOCAL CONT ECONT LISZT LAST REST FIRST) . 4) 
;((SYNSEM LOCAL AGR PNG PN) . 4) 
;((SYNSEM LOCAL STEMHEAD) . 4) 
;((SYNSEM LOCAL CONT INDEX VIT VITMOOD) . 3) 
;((SYNSEM LOCAL CONJ CHEAD LEFT REST FIRST) . 2) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT POSTHEAD) . 2) 
;((SYNSEM LOCAL CONT --STEMLISZT LIST FIRST) . 2) 
;((SYNSEM LOCAL CAT VALENCE COMPS FIRST LOCAL CAT VALENCE SUBJ FIRST LOCAL CONT LISZT LAST REST REST FIRST INST PNG PN) . 1) 
;((SYNSEM NON-LOCAL SLASH LIST FIRST CAT VALENCE SUBJ FIRST LOCAL CONT KEY) . 1) 
;((SYNSEM LOCAL CONT INDEX PNG PN) . 1) 
;((SYNSEM LOCAL CAT VALENCE COMPS REST FIRST) . 1) 
))


#|
;;;

(dolist (table (list *rules* *lexical-rules*))
   (maphash
      #'(lambda (id rule)
         (declare (ignore id))
         (let* ((fs (rule-full-fs rule))
                (f-list (rule-order rule)))
          (setf (rule-daughters-restricted rule)
            (mapcar
               #'(lambda (path)
                   (restrict-fs (existing-dag-at-end-of (tdfs-indef fs) path)))
               (cdr f-list)))))
      table))
(clear-type-cache)
(gc)
(parse-tsdb-sentences "Macintosh HD:lkb99-expt:big:itemsamp30"
   "Macintosh HD:lkb99-expt:big:parsesamp30" "Macintosh HD:lkb99-expt:big:resultsamp30")

|#


;;;

(defun restrict-fs (fs)
   (when (tdfs-p fs) (setq fs (tdfs-indef fs)))
   (mapcar
      #'(lambda (path-spec)
          (let ((v (existing-dag-at-end-of fs (car path-spec))))
             (if v (type-of-fs v) nil)))
      *check-paths*))


(defun restrictors-compatible-p (daughter-restricted child-restricted)
   (dolist (dt daughter-restricted t)
      (let ((ct (pop child-restricted)))
         ;; pull eq to avoid function call if true
         (when (and dt ct (not (eq dt ct)) (not (find-gcsubtype dt ct)))
            (return-from restrictors-compatible-p nil)))))




#|
;;; for calling dynamically inside the scope of a set of unifications

(defun x-restrict-fs (fs)
   (when (tdfs-p fs) (setq fs (tdfs-indef fs)))
   (mapcar
        #'(lambda (path-spec)
            (let ((v (x-existing-dag-at-end-of fs (car path-spec))))
               (if v (unify-get-type v) nil)))
        *check-paths*))

(defun x-existing-dag-at-end-of (real-dag labels-chain)
   (cond 
      ((null labels-chain) real-dag)
      ((is-atomic real-dag) nil)
      (t
         (let ((one-step-down
                  (x-get-dag-value real-dag
                     (car labels-chain))))
            (if one-step-down
               (x-existing-dag-at-end-of one-step-down
                  (cdr labels-chain))
               nil)))))

(defun x-get-dag-value (dag attribute)
   (dolist (arc (dag-arcs dag) nil)
      (when (eql attribute (dag-arc-attribute arc))
         (return-from x-get-dag-value (dag-arc-value arc))))
   (dolist (arc (dag-comp-arcs dag) nil)
      (when (eql attribute (dag-arc-attribute arc))
         (return-from x-get-dag-value (dag-arc-value arc)))))
|#
