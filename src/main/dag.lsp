;;; LKB
;;;
;;; Base level unification of dags
;;;
;;; Copyright Rob Malouf, John Carroll 1997-1998 All Rights Reserved.
;;; CSLI, Stanford University
;;; No use or redistribution without permission.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-unification-context)))

#+mcl
(progn ; for space profiling and similar investigation
(defparameter aa 0)
(defparameter bb 0)
(defparameter cc 0)
(defparameter dd 0)
(defparameter ee 0)
(defparameter ff 0)
(defparameter gg 0)
(defparameter hh 0)
(defparameter ii 0)
(defparameter jj 0)
)


;;; **********************************************************************
;;; Each temporary value set by the unifier is marked with a unification
;;; generation counter.  That way, we can invalidate all values set by an
;;; computation simply by incrementing the counter.

(defvar *unify-generation* 1)
(defvar *unify-generation-max* 1)
(defvar *visit-generation* 1)
(defvar *visit-generation-max* 1)

(eval-when (:compile-toplevel :load-toplevel)
  (proclaim '(type fixnum *unify-generation* *unify-generation-max*
                *visit-generation* *visit-generation-max*)))

(defun invalidate-marks ()
   (setq *unify-generation*
      (1+ (max *unify-generation-max* *unify-generation*)))
   (setq *unify-generation-max* *unify-generation*))

(defun invalidate-visit-marks ()
   (setq *visit-generation*
      (1+ (max *visit-generation-max* *visit-generation*)))
   (setq *visit-generation-max* *visit-generation*))

(defvar *unify-debug* nil)
(defvar *unify-debug-cycles* nil) ; report when cycle found during copy?

;;;
;;; establish way to return information about failure (rather than printing
;;; the nature of the failure right away).          (2-mar-99  -  oe@csli)
;;;
(defparameter %failure% nil)

(defvar *unify-wffs* nil)

(defvar *expanding-types* nil
  "used to signal we're within a unification which may try
   to access a constraint we haven't calculated yet")

(defvar *within-unification-context-p* nil)
(defvar *safe-not-to-copy-p* nil)


;;; **********************************************************************
;;; This is the structure for representing feature structures. Slots prefixed
;;; with x- must be accessed only via macros, not directly

(defstruct (dag
             (:constructor make-dag-x (type arcs))
             (:copier copy-dag-x))
   (type nil)
   (arcs nil)
   ;; new type computed during a unification
   (x-new-type nil)
   (x-new-type-mark 0 :type fixnum)
   ;; new arcs computed during a unification
   (x-comp-arcs-slot nil)
   ;; pointer to a copy of this fs
   (x-copy nil)
   (x-copy-mark 0 :type fixnum)
   ;; pointer to representative for this fs's equivalence class
   (x-forward nil)
   (x-forward-mark 0 :type fixnum)
   ;; flag used when traversing a fs doing interleaved unifications
   (x-visit-slot nil))

(defstruct (safe-dag
             (:include dag)
             (:constructor make-safe-dag-x (type arcs))
             (:copier copy-safe-dag-x))
   ;; save a boolean slot in dag structure by making 'safe' dags a different
   ;; type. We likely save 8 bytes per dag if structure then gets an even
   ;; number of slots
   )

(defmacro make-dag (&key type arcs)
   `(if *safe-not-to-copy-p*
      ;; safe not to copy this node? False for rules, lexical entries, type
      ;; constraints - when we pick them or parts of them up those bits must be copied
      ;; else multiple uses in the same analysis could become reentrant. True during
      ;; parsing and generation - assumes that the same derived constituent can't be
      ;; used more than once in a single analysis
      (make-safe-dag-x ,type ,arcs)
      (make-dag-x ,type ,arcs)))


;; (pushnew :lkb-nochecks *features*)

(defmacro with-dag-optimize ((dag) &body body)
   #-lkb-nochecks (declare (ignore dag))
   `(locally (declare (type fixnum *unify-generation*))
       #+lkb-nochecks (declare ,@(if (symbolp dag) `((type dag ,dag)))
                               (optimize (speed 3) (safety 0)))
      ,@body))

(defmacro dag-safe-p (dag)
   `(with-dag-optimize (,dag) (safe-dag-p ,dag)))


(defmacro dag-new-type (dag)
  `(with-dag-optimize (,dag)
     (when (= (the fixnum (dag-x-new-type-mark ,dag)) *unify-generation*)
        (dag-x-new-type ,dag))))

(defsetf dag-new-type (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-new-type-mark ,dag) *unify-generation*
           (dag-x-new-type ,dag) ,new)))


(defmacro dag-comp-arcs (dag)
  `(with-dag-optimize (,dag)
     (when (and (consp (dag-x-comp-arcs-slot ,dag))
                (= (the fixnum (car (the cons (dag-x-comp-arcs-slot ,dag))))
                   *unify-generation*))
        (cdr (dag-x-comp-arcs-slot ,dag)))))

(defsetf dag-comp-arcs (dag) (new)
  `(with-dag-optimize (,dag)
     (if (consp (dag-x-comp-arcs-slot ,dag))
        (setf (car (the cons (dag-x-comp-arcs-slot ,dag))) *unify-generation*
              (cdr (the cons (dag-x-comp-arcs-slot ,dag))) ,new)
        (progn
           (setf (dag-x-comp-arcs-slot ,dag) (cons *unify-generation* ,new))
           ,new))))


(defmacro dag-copy (dag)
  `(with-dag-optimize (,dag)
     (when (= (the fixnum (dag-x-copy-mark ,dag)) *unify-generation*)
        (dag-x-copy ,dag))))

(defsetf dag-copy (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-copy-mark ,dag) *unify-generation*
           (dag-x-copy ,dag) ,new)))


(defmacro dag-forward (dag)
  `(with-dag-optimize (,dag)
     (when (= (the fixnum (dag-x-forward-mark ,dag)) *unify-generation*)
        (dag-x-forward ,dag))))

(defsetf dag-forward (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-forward-mark ,dag) *unify-generation*
           (dag-x-forward ,dag) ,new)))


(defmacro dag-visit (dag)
  `(with-dag-optimize (,dag)
     (when (and (consp (dag-x-visit-slot ,dag))
                (= (the fixnum (car (the cons (dag-x-visit-slot ,dag))))
                   *visit-generation*))
        (cdr (dag-x-visit-slot ,dag)))))

(defsetf dag-visit (dag) (new)
  `(with-dag-optimize (,dag)
     (if (consp (dag-x-visit-slot ,dag))
        (setf (car (the cons (dag-x-visit-slot ,dag))) *visit-generation*
              (cdr (the cons (dag-x-visit-slot ,dag))) ,new)
        (progn
           (setf (dag-x-visit-slot ,dag) (cons *visit-generation* ,new))
           ,new))))


(defun clone-dag (dag)
   ;; make a 'copy' of dag, carrying forward just type/new-type and arcs/comp-arcs
   (let ((new-dag
           (make-dag :type (dag-type dag) :arcs (dag-arcs dag))))
      (when (dag-new-type dag) (setf (dag-new-type new-dag) (dag-new-type dag)))
      (when (dag-comp-arcs dag) (setf (dag-comp-arcs new-dag) (dag-comp-arcs dag)))
      new-dag))


;;; Arcs

(defmacro make-dag-arc (&key attribute value)
   ;; this is a cons rather than a structure, halving its space needs
   `(cons ,attribute ,value))

(defmacro with-arc-optimize ((arc) &body body)
   #-lkb-nochecks (declare (ignore arc))
   `(locally #+lkb-nochecks (declare ,@(if (symbolp arc) `((type cons ,arc)))
                                     (optimize (speed 3) (safety 0)))
      ,@body))

(defmacro dag-arc-attribute (arc)
   `(with-arc-optimize (,arc)
      (car ,arc)))

(defsetf dag-arc-attribute (arc) (new)
  `(with-arc-optimize (,arc)
     (setf (car ,arc) ,new)))

(defmacro dag-arc-value (arc)
   `(with-arc-optimize (,arc)
      (cdr ,arc)))

(defsetf dag-arc-value (arc) (new)
  `(with-arc-optimize (,arc)
     (setf (cdr ,arc) ,new)))


;;; Deref-dag follows forwarding pointers during a unification operation.
;;; Outside unification code proper follow-pointers should be called instead
;;; (it could still be called inside a unification context e.g. when printing
;;; out dags during debugging). It can afford to do some useful consistency
;;; checks

(defun deref-dag (dag) 
  (with-dag-optimize (dag)
     (if (= (the fixnum (dag-x-forward-mark dag)) *unify-generation*)
        (deref-dag (dag-x-forward dag))
        dag)))


(defun follow-pointers (dag)
   (cond
      ((not (dag-p dag))
         (error "~A inconsistency: called with non-dag argument" 'follow-pointers))
      (*within-unification-context-p* (deref-dag dag))
      ((or (dag-new-type dag) (dag-comp-arcs dag) (dag-forward dag))
         (error "~A inconsistency: dag contains temporary structure outside ~
                context of a set of unification operations" 'follow-pointers))
      (t dag)))


;;; **********************************************************************

;;; Creation of dags

(defmacro create-typed-dag (type)
  `(make-dag :type ,type :arcs nil))

(defmacro create-dag nil
  `(make-dag :type *toptype* :arcs nil))

(defun create-atomic-dag (type)
  (make-dag
   :type (if (consp type) type (list type))
   :arcs nil))


(defmacro type-spec-atomic-p (type)
   `(consp ,type))


;;; NB don't use following inside unification since they ignore temporary structure
;;; (deref pointer, new-type, comp-arcs)

(defmacro is-atomic (dag)
   `(type-spec-atomic-p (dag-type ,dag)))                 

(defmacro has-features (dag)
   `(consp (dag-arcs ,dag)))

(defmacro type-of-fs (dag)
   `(dag-type ,dag))


(defun top-level-features-of (dag)
   (unless (is-atomic dag)
      (mapcar #'(lambda (arc) (dag-arc-attribute arc)) (dag-arcs dag))))


(defun get-dag-value (dag attribute)
   (dolist (arc (dag-arcs dag) nil)
      (when (eq attribute (dag-arc-attribute arc))
         (return-from get-dag-value (dag-arc-value arc)))))


(defun get-value-at-end-of (dag labels-chain)
   (cond
      ((null labels-chain) (type-of-fs dag))
      ((is-atomic dag) 'no-way-through)
      (t
         (let ((one-step-down
                  (get-dag-value dag
                                 (car labels-chain))))
            (cond (one-step-down
                     (get-value-at-end-of one-step-down
                                          (cdr labels-chain)))
                  (t nil))))))
      
;;; **********************************************************************
;;; Unify

;;; Interface functions for the unifier.  The first returns the
;;; result of unifying the the second argument with the first argument.
;;; The second returns T if that unification would be
;;; successful without actual building the result.  Neither damages the input
;;; feature structures.
;;;
;;; Marks work as follows:
;;; - the unify mark is updated AFTER every series of unifications (with or
;;; without a final copy operation) so that any temporary changes to
;;; feature structures disappear
;;; - the visit mark is updated BEFORE any new series of visits so code
;;; does not see any old marks

(defmacro with-unification-context ((dag) &body body)
  ;; caller must call copy-dag explicitly at end - before any other
  ;; unification attempt - if result is needed 
  ;; NB unwind-protect is needed to deal properly with local exits (e.g.
  ;; return-from) out of body, as well as throws and errors
  (declare (ignore dag))
  `(if *within-unification-context-p*
       (error "Entered a nested unification context - should not happen")
     (let ((*within-unification-context-p* t))
       (unwind-protect (progn ,@body) (invalidate-marks)))))

(defun unify-dags (dag1 dag2)
  (if *within-unification-context-p*
      (progn #+(and mcl powerpc)(decf bb (CCL::%HEAP-BYTES-ALLOCATED))
             (prog1
		 (catch '*fail*
                   (progn
                     (unify1 dag1 dag2 nil)
                     (when (or *unify-debug* *unify-debug-cycles*)
                       (if (cyclic-dag-p dag1)
                         ;;
                         ;; for the (eq *unify-debug* :return) variant the 
                         ;; %failure% value is determined in cyclic-dag-p()
                         ;; already; hence suppress printed output here.  the
                         ;; baroque conditionals preserve the original LKB
                         ;; behaviour.                   (19-mary-99  -  oe)
                         ;;
                         (unless (and (null *unify-debug-cycles*)
                                      (eq *unify-debug* :return))
                           (format t "~%Unification failed - cyclic result"))
                         (when (and *unify-debug* 
                                    (not (eq *unify-debug* :return)))
                           (format t "~%Unification succeeded"))))
                     dag1))
                #+(and mcl powerpc)(incf bb (CCL::%HEAP-BYTES-ALLOCATED))
                ))
     (with-unification-context (dag1) 
       (when (unify-dags dag1 dag2) (copy-dag dag1)))))

(defun unifiable-dags-p (dag1 dag2)
  (if *within-unification-context-p*
      (catch '*fail*
         (progn
            (unify1 dag1 dag2 nil)
            (if (cyclic-dag-p dag1)
              (progn 
                ;;
                ;; see above for baroque conditional       (19-may-99  -  oe)
                ;;
                (when (and (or *unify-debug* *unify-debug-cycles*)
                           (not (eq *unify-debug* :return)))
                  (format t "~%Unification failed - cyclic result"))
                nil)
              (progn
                (when (and *unify-debug* (not (eq *unify-debug* :return)))
                  (format t "~%Unification succeeded"))
               t))))
      (with-unification-context (dag1) (unifiable-dags-p dag1 dag2))))

;;; This is the heart of the unification algorithm, and is based on Hideto
;;; Tomabechi's paper in the 1991 ACL proceedings.  We walk through the two
;;; feature structures checking for compatibility and setting forward
;;; pointers.  As soon as we find a problem, we stop.  If we get through the
;;; whole structure without finding a problem, then we copy the first unifact
;;; using the forward pointers we set during the check.
;;;
;;; Unique marker is passed in on each individual unification attempt to mark
;;; dags that we're currently inside, so if a circularity has just cropped up
;;; we don't get stuck in it

(defmacro unify-get-type (fs)
  `(or (dag-new-type ,fs) (dag-type ,fs)))

(defun unify1 (dag1 dag2 path)
  (setq dag1 (deref-dag dag1))
  (setq dag2 (deref-dag dag2))
  (cond
   ((eq (dag-copy dag1) :inside)
    (when (or *unify-debug* *unify-debug-cycles*)
      (if (eq *unify-debug* :return)
        (setf %failure% (list :cycle (reverse path)))
        (format 
         t 
         "~%Unification failed: unifier found cycle at < ~{~A ~^: ~}>" 
         (reverse path))))
    (throw '*fail* nil))
   ((not (eq dag1 dag2)) (unify2 dag1 dag2 path)))
  dag1)

(defparameter *recording-constraints-p* nil
  "needed for LilFes conversion")

(defvar *type-constraint-list* nil)

(defun unify2 (dag1 dag2 path)
  (multiple-value-bind (new-type constraintp)
      (find-gcsubtype (unify-get-type dag1) (unify-get-type dag2))
    (if new-type
	(progn
	  (setf (dag-new-type dag1) new-type)
	  (if (type-spec-atomic-p new-type)
	      (if (or (dag-arcs dag1) (dag-comp-arcs dag1)
		      (dag-arcs dag2) (dag-comp-arcs dag2))
                  (progn
		    (when *unify-debug*
		      (if (eq *unify-debug* :return)
                        (setf %failure% (list :atomic (reverse path)))
                        (format 
                         t 
                         "~%Unification failed due to atomic/~
                          non-atomic clash at path < ~{~A ~^: ~}>" 
                         (reverse path))))
		    (throw '*fail* nil))
		(setf (dag-forward dag2) dag1))
	    (progn
	      ;; unify in constraints if necessary - may have to copy them to
	      ;; prevent separate uses of same constraint in same unification
	      ;; becoming reentrant
	      (when (and constraintp *unify-wffs*)
		(let ((constraint (if *expanding-types*
                                      (copy-dag-completely (wf-constraint-of new-type))
                                      (may-copy-constraint-of new-type))))
                  (when *recording-constraints-p*
                      (pushnew new-type *type-constraint-list* :test #'eq))
		  (if  *unify-debug*
		      (let ((res 
			     (catch '*fail* (unify1 dag1 constraint path))))
			(unless res
			  (if (eq *unify-debug* :return)
                            (setf %failure% 
                              (list :constraints 
                                    (reverse path) new-type nil nil))
                            (format 
                             t 
                             "~%Unification with constraint 
                              of type ~A failed ~
                              at path < ~{~A ~^: ~}>" 
                             new-type (reverse path)))
			  (throw '*fail* nil)))
		    (unify1 dag1 constraint path)))
		;; dag1 might just have been forwarded so dereference it again
		(setq dag1 (deref-dag dag1)))
	      ;; cases for each of dag1 and dag2 where they have no arcs just
	      ;; considering straightforward use of unify1: if we've
	      ;; previously visited a node with no arcs then it must have got
	      ;; forwarded then so we won't ever visit it again - so no need
	      ;; to test for presence of any comp-arcs BUT:
	      ;; unify-paths-dag-at-end-of1 adds to comp-arcs independently so
	      ;; we do need the additional tests
	      (cond
	       ((and (null (dag-arcs dag1)) (null (dag-comp-arcs dag1)))
		(setf (dag-new-type dag2) new-type)
		(setf (dag-forward dag1) dag2))
	       ((and (null (dag-arcs dag2)) (null (dag-comp-arcs dag2)))
		(setf (dag-forward dag2) dag1))
	       (t
		(setf (dag-forward dag2) dag1)
		(setf (dag-copy dag1) :inside)
		(unify-arcs dag1 dag2 path)
		(setf (dag-copy dag1) nil))))))
      (progn
	(when *unify-debug*
          (if (eq *unify-debug* :return)
            (setf %failure% 
              (list :clash (reverse path) 
                    (unify-get-type dag1) (unify-get-type dag2)))
            (format 
             t 
             "~%Unification of ~A and ~A failed at path < ~{~A ~^: ~}>"
             (unify-get-type dag1) (unify-get-type dag2) 
             (reverse path))))
	(throw '*fail* nil)))))

(defmacro unify-arcs-find-arc (attribute arcs comp-arcs)
  ;; find arc in arcs or comp-arcs with given attribute - also used in
  ;; structs.lsp
  (let ((v (gensym)))
    `(macrolet ((find-attribute (v arcs)
		  (let ((a (gensym)))
		    `(block find-attribute
		       (dolist (,a ,arcs nil)
			 (when (eq (dag-arc-attribute ,a) ,v)
			   (return-from find-attribute ,a)))))))
       (let ((,v ,attribute))
	 (or (find-attribute ,v ,arcs) (find-attribute ,v ,comp-arcs))))))

(defun unify-arcs (dag1 dag2 path)
  (let ((arcs1 (dag-arcs dag1))
        (comp-arcs1 (dag-comp-arcs dag1))
        (new-arcs1 (dag-comp-arcs dag1)))
    (macrolet ((process-arcs (arcs)
                 `(dolist (elem2 ,arcs)
                     (let ((elem1
                             (unify-arcs-find-arc (dag-arc-attribute elem2) arcs1
                                comp-arcs1)))
                       (if elem1
                          (let ((new-path (cons (dag-arc-attribute elem1) path)))
                             (declare (dynamic-extent new-path))
                             (unify1 (dag-arc-value elem1) (dag-arc-value elem2)
                                new-path))
                          (push elem2 new-arcs1))))))
      (process-arcs (dag-arcs dag2))
      (process-arcs (dag-comp-arcs dag2)))
    (when new-arcs1 (setf (dag-comp-arcs dag1) new-arcs1))))


;;; similar to constraint-of return the type's constraint, a 'fresh'
;;; copy if we've previously returned it during this unification
;;; attempt, otherwise as is. Copies are cached and we cycle through
;;; them, creating new ones if needed we want normal 'safe' dag
;;; behaviour, i.e. when we're parsing/generating we don't need the
;;; copy itself to be 'safe' wrt copying. In all other cases we want
;;; it safe

(defun may-copy-constraint-of (type-name)
  (let* ((type-parent-name (instance-type-parent type-name))
	 (type-record (get-type-entry (or type-parent-name type-name)))
	 (constraint (type-constraint type-record))
	 (cache (type-constraint-mark type-record)))
    (unless (consp cache)
      (setq cache (list* 0 nil nil))	; mark, unused, used
      (setf (type-constraint-mark type-record) cache))
    (cond
     ((not (= (car cache) *unify-generation*))
      ;; (print (list "not copying constraint for" type-name))
      (setf (car cache) *unify-generation*)
      (setf (cadr cache)
	(nconc (cadr cache) (cddr cache))) ; old used copies become ready for use
      (setf (cddr cache) nil)
      constraint)			; first return constraint itself
     ((cadr cache)
      ;; (print (list "not copying constraint for" type-name))
      (let ((pre (pop (cadr cache))))
	(push pre (cddr cache))		; previously computed copy becomes used
	pre))
     (t
      ;; (print (list "copying constraint for" type-name))
      (let ((new (copy-dag-completely constraint)))
	(push new (cddr cache))		; new copy becomes used
	new)))))


;;; Greatest common subtype of two type specs - atomic (maybe a
;;; disjunction) and/or non-atomic

(defun find-gcsubtype (type1 type2)
  (when (eq type1 type2) (return-from find-gcsubtype type1))
  ;;#+(and mcl powerpc)(decf cc (CCL::%HEAP-BYTES-ALLOCATED))
  ;;(multiple-value-prog1
  (cond
   ((and (not (type-spec-atomic-p type1)) (not (type-spec-atomic-p type2))) 
    (greatest-common-subtype type1 type2))
   ((not (type-spec-atomic-p type1)) (gcssemi type1 type2))
   ((not (type-spec-atomic-p type2)) (gcssemi type2 type1))
   ((and (null (cdr type1)) (null (cdr type2)))
    (let ((res (greatest-common-subtype (car type1) (car type2))))
      (cond
       ((listp res) res)
       ((eq res (car type1)) type1)
       ((eq res (car type2)) type2)
       (t (list res)))))
   (t (gcslists type1 type2)))
  ;;#+(and mcl powerpc)(incf cc (CCL::%HEAP-BYTES-ALLOCATED)))
  )


(defun gcssemi (type tlist)
  ;; first arg is non-atomic, second is atomic
  (if (null (cdr tlist))
      (if (and (symbolp type) (symbolp (car tlist)))
	  ;; this result can be cached - a single disjunct and only in
	  ;; first argument We're bypassing greatest-common-subtype
	  ;; here since we don't have strings and we want to cache an
	  ;; atomic type (which is represented as a list) to save
	  ;; consing. There's special-purpose code for this in the
	  ;; caching which we would otherwise not be able to get to
	  (cached-greatest-common-subtype tlist type t)
	(let ((res (greatest-common-subtype type (car tlist))))
	  (cond
	   ((listp res) res)
	   ((eq res (car tlist)) tlist)
	   (t (list res)))))
    (let ((res nil))
      (dolist (t1 tlist res)
	(let ((gcs (greatest-common-subtype t1 type)))
	  (when gcs (pushnew (if (consp gcs) (car gcs) gcs) res)))))))

(defun gcslists (tlist1 tlist2)
   ;; called with two (atomic) disjunctive values, at least one of which has more
   ;; than a single disjunct
   (let ((res nil))
      (dolist (t1 tlist1 res)
         (dolist (t2 tlist2)
            (let ((gcs (greatest-common-subtype t1 t2)))
               (when gcs (pushnew (if (consp gcs) (car gcs) gcs) res)))))))


;;; Copy first feature structure after a successful unification, respecting
;;; any forward pointers set by the unifier.

(defun copy-dag (dag)
   #+(and mcl powerpc)(decf aa (CCL::%HEAP-BYTES-ALLOCATED))
   (prog1 (catch '*fail* (copy-dag1 dag nil))
      #+(and mcl powerpc)(incf aa (CCL::%HEAP-BYTES-ALLOCATED))
   ))

;;; Tomabechi/Rob/John: not copying when dag is 'safe', type has not changed,
;;; no comp-arcs, and no copied dags underneath. No garbage generated in case
;;; where no lower level dags need to be copied

(defun copy-dag1 (dag path)
  (setq dag (deref-dag dag))
  (cond
   ((eq (dag-copy dag) :inside)
    (when (or *unify-debug* *unify-debug-cycles*)
      (if (eq *unify-debug* :return)
        (setf %failure% (list :cycle (reverse path)))
        (format t "~%Unification failed: copy found cycle at < ~{~A ~^: ~}>" 
                (reverse path))))
    (throw '*fail* nil))
   ((not (symbolp (dag-copy dag)))
    (dag-copy dag))
   ((and (null (dag-arcs dag)) (null (dag-comp-arcs dag)))
    (setf (dag-copy dag)
      (if (or (not (dag-safe-p dag))
	      (and (dag-new-type dag)
		   (not (eq (dag-new-type dag) (dag-type dag)))))
	  (make-dag :type (unify-get-type dag) :arcs nil)
	dag)))
   (t
    ;; would have liked to have put path here, but it would hang around after
    ;; a circularity was detected - bad news for stack allocated conses
    (setf (dag-copy dag) :inside)
    (let ((copy-p (or (not (dag-safe-p dag))
		      (and (dag-new-type dag)
			   (not (eq (dag-new-type dag) (dag-type dag))))
		      (dag-comp-arcs dag)))
	  (new-arcs (nreverse (dag-comp-arcs dag))))
      (do ((tail new-arcs (cdr tail))) 
	  ((null tail))
	;; top-level conses in comp-arcs can be re-used, but an arc needs to
	;; be fresh structure if its value is changed
	(let ((new-path (cons (dag-arc-attribute (car tail)) path)))
	  (declare (dynamic-extent new-path))
	  (let ((v (copy-dag1 (dag-arc-value (car tail)) new-path)))
	    (unless (eq v (dag-arc-value (car tail)))
	      (setf (car tail)
		(make-dag-arc
		 :attribute (dag-arc-attribute (car tail))
		 :value v))))))
      (setq new-arcs
	(copy-dag-arcs (dag-arcs dag) nil path nil (dag-arcs dag) new-arcs))
      (unless copy-p
	(setq copy-p (not (eq new-arcs (dag-arcs dag)))))
      (setf (dag-copy dag)
	(if copy-p
	    (make-dag :type (unify-get-type dag) :arcs new-arcs)
	  dag))))))


(defun copy-dag-arcs (arcs-tail vals path lower-copied-p arcs new-arcs)
   ;; compiler must not convert recursive function into interative otherwise
   ;; stack allocation will break. Allegro 4.3 (at least) must be explicitly
   ;; stopped from doing this 
   (declare (notinline copy-dag-arcs))
   (cond
      (arcs-tail
         (let* ((arc (car arcs-tail))
                (new-path (cons (dag-arc-attribute arc) path))
                (new-vals (cons nil vals)))
            (declare (dynamic-extent new-path new-vals))
            (let ((v (copy-dag1 (dag-arc-value arc) new-path)))
               (unless (eq v (dag-arc-value arc))
                  (setq lower-copied-p arcs-tail)) ; a lower-level dag was copied
               (setf (car new-vals) v)
               (copy-dag-arcs
                  (cdr arcs-tail) new-vals path lower-copied-p arcs new-arcs))))
      (lower-copied-p
         ;; need to make a copy of arcs here - but (cdr lower-copied-p)
         ;; is a tail that we can re-use
         (setq vals (nreverse vals))
         (do ((arcs-tail arcs (cdr arcs-tail))
              (shared-tail (cdr lower-copied-p))
              (copied-arcs nil))
             ((eq arcs-tail shared-tail)
                (setq new-arcs
                   (nreconc copied-arcs (nconc new-arcs shared-tail))))
             (let ((old-arc (car arcs-tail))
                   (v (pop vals)))
                  (push
                     (if (eq v (dag-arc-value old-arc))
                        old-arc
                        (make-dag-arc :attribute (dag-arc-attribute old-arc)
                           :value v))
                     copied-arcs))))
      (new-arcs
         (nconc new-arcs arcs))
      (t arcs)))


#|
(defun copy-dag1 (dag path)
  (setq dag (deref-dag dag))
  (let ((copy (dag-copy dag)))
    (cond ((eq copy 'visited)
             (when (or *unify-debug* *unify-debug-cycles*)
                (format t "~%Unification failed: copy found cycle at ~:A" (reverse path)))
             (throw '*fail* nil)) ; Oh no, a cycle!
          ((eq copy 'dont-copy) 
             (values dag nil)) ; A dag we don't need to copy
          ((dag-p copy) 
             (values copy t)) ; A dag we've already copied
          (t
             (let ((copy-p (or (not (dag-safe-p dag)) (eq (dag-copy dag) 'copy)))
                   (new-arcs nil))
               (setf (dag-copy dag) 'visited)
               (dolist (arc (dag-arcs dag))
                 (let ((new-path (cons (dag-arc-attribute arc) path)))
                   (declare (dynamic-extent new-path))
                   (multiple-value-bind (v c)
                        (copy-dag1 (dag-arc-value arc) new-path)
                     (when c (setq copy-p t))
                     (push
                        (make-dag-arc :attribute (dag-arc-attribute arc) :value v)
                        new-arcs))))
               (dolist (arc (dag-comp-arcs dag))
                 (let ((new-path (cons (dag-arc-attribute arc) path)))
                   (declare (dynamic-extent new-path))
                   (push
                      (make-dag-arc :attribute (dag-arc-attribute arc)
                                    :value (copy-dag1 (dag-arc-value arc) new-path))
                      new-arcs)))
               (setf (dag-copy dag)
                  (if copy-p
                     (make-dag :type (unify-get-type dag) :arcs new-arcs)
                     'dont-copy))
               (values (if copy-p (dag-copy dag) dag) copy-p))))))

;;; less highly optimised version - always does a full copy

(defun copy-dag1 (dag path)
  (setq dag (deref-dag dag))
  (cond ((eq (dag-copy dag) 'visited)
         (when (or *unify-debug* *unify-debug-cycles*)
           (format t "~%Unification failed: copy found cycle at ~A" 
                   (reverse path)))
         (throw '*fail* nil))        ; Oh no, a cycle!
        ((dag-p (dag-copy dag)) (dag-copy dag))
        (t (let ((new-arcs nil))
             (setf (dag-copy dag) 'visited)
             (dolist (arc (dag-arcs dag))
               (push (make-dag-arc :attribute (dag-arc-attribute arc)
                                   :value (copy-dag1 (dag-arc-value arc) 
                                               (cons (dag-arc-attribute arc) path)))
                     new-arcs))
             (dolist (arc (dag-comp-arcs dag))
               (push (make-dag-arc :attribute (dag-arc-attribute arc)
                                   :value (copy-dag1 (dag-arc-value arc) 
                                               (cons (dag-arc-attribute arc) path)))
                     new-arcs))
             (setf (dag-copy dag)
               (make-dag :type (unify-get-type dag)
                         :arcs new-arcs))))))
|#


;;; **********************************************************************

;;; Physically copy a dag - leaves the source dag untouched.
;;; Assumes all forwarding pointers, comp-arcs, and new-type have been dealt
;;; with already (i.e. by copy-dag)
;;; Use -visit field not -copy since may be called from within unify

(defun copy-dag-completely (dag)
   #+(and mcl powerpc)(decf dd (CCL::%HEAP-BYTES-ALLOCATED))
   (invalidate-visit-marks)
   (prog1 (copy-dag-completely1 dag (create-dag))
      #+(and mcl powerpc)(incf dd (CCL::%HEAP-BYTES-ALLOCATED))
      ))

(defun copy-dag-completely1 (dag toptype-dag)
   (or (dag-visit dag)
      (progn
         (setf (dag-visit dag) toptype-dag) ; 3/98 - avoid crashes with cyclic dags
         (let ((new-instance 
                  (make-dag
                     :type (dag-type dag) ; no one must modify an atomic type (list)
                     :arcs
                     (mapcar
                        #'(lambda (arc)
                            (let ((label (dag-arc-attribute arc)))
                               (make-dag-arc :attribute label
                                  :value
                                  (copy-dag-completely1
                                     (dag-arc-value arc) toptype-dag))))
                        (dag-arcs dag)))))
            (setf (dag-visit dag) new-instance)
            new-instance))))


;;; **********************************************************************

;;; Test for cycles explicitly. Copy-dag will return nil if there are cycles,
;;; but we can use this in the limited circumstances where we just want to
;;; check for cycles and not do the copy.
;;; We use the copy slot, so have to set up a new unification context if we're
;;; not already in one. If we are, we'll be leaving :outside in copy slots
;;; so subsequent copying will have to take care. On entry we assume that 
;;; copy slots will be empty (or at least not contain :outside or :inside)
;;; We use 2 markers: :outside is used to mark where we've been already so
;;; we don't go checking re-entrant structure that we've already checked,
;;; and :inside is used for the cyclic check itself

(defun cyclic-dag-p (dag)   
   ;; return t if cyclic
  (if *within-unification-context-p*
      (catch '*cyclic*
        (progn
          (cyclic-dag-p1 dag nil)
          (when (and *unify-debug* (not (eq *unify-debug* :return)))
            (format t "~%Dag not cyclic"))
          nil))
    (with-unification-context (dag) (cyclic-dag-p dag))))

(defun cyclic-dag-p1 (dag path)   
   (setq dag (deref-dag dag))
   (cond
      ((eq (dag-copy dag) :outside))
      ((eq (dag-copy dag) :inside)
       (when (or *unify-debug* *unify-debug-cycles*)
         (if (eq *unify-debug* :return)
           (setf %failure% (list :cycle (reverse path)))
           (format t "~%Cyclic check found cycle at < ~{~A ~^: ~}>" 
                   (reverse path))))
         (throw '*cyclic* t))
      ((or (dag-arcs dag) (dag-comp-arcs dag))
         (setf (dag-copy dag) :inside)
         (dolist (arc (dag-arcs dag))
            (let ((new-path (cons (dag-arc-attribute arc) path)))
               (declare (dynamic-extent new-path))
               (cyclic-dag-p1 (dag-arc-value arc) new-path)))
         (dolist (arc (dag-comp-arcs dag))
            (let ((new-path (cons (dag-arc-attribute arc) path)))
               (declare (dynamic-extent new-path))
               (cyclic-dag-p1 (dag-arc-value arc) new-path)))
         (setf (dag-copy dag) :outside))))

;; Removes the marks left by cyclic-dag-p

(defun fix-dag (dag)   
  (setq dag (deref-dag dag))
  (when (dag-copy dag)
    (setf (dag-copy dag) nil)
    (dolist (arc (dag-arcs dag))
      (fix-dag (dag-arc-value arc)))
    (dolist (arc (dag-comp-arcs dag))
      (fix-dag (dag-arc-value arc)))
    dag))



;;; **********************************************************************

;;; Changing the top-level type of a dag

(defun retype-dag (dag new-type)
   (if *within-unification-context-p*
      (progn
         (setq dag (deref-dag dag))
         (setf (dag-new-type dag) new-type)
         dag)
      (with-unification-context (dag) (retype-dag dag new-type) (copy-dag dag))))

(defun destructively-retype-dag (dag new-type)
  (setf (dag-type dag) new-type)
  dag)


;;; **********************************************************************

;;; WFFs

;;; Well-formedness checking
;;; we assume we have a feature structure which we want to convert
;;; to a well-formed fs if possible.
;;; 1. Push down type so all features are appropriate
;;; 2. Recurse on features
;;; 3. Unify with constraint of new type 
;;; (given that this is well-formed)

(defun create-wffs (fs)
   ;; non-destructive!
   ;; returns nil on failure
   (with-unification-context (fs)
      (invalidate-visit-marks)
      (let ((res (make-well-formed fs nil)))
         (if res (copy-dag fs)))))


(defun make-well-formed (fs features-so-far &optional type-name)
   (let ((real-dag (deref-dag fs)))
    (cond ((dag-visit real-dag) ; been here before
           t)
          (t
           (setf (dag-visit real-dag) t)
           (or (type-spec-atomic-p (unify-get-type real-dag)) ; atomic types are well-formed by definition 
               (let
                   ((fs-type (find-type-of-fs real-dag 
                                              type-name features-so-far)))
                   (if fs-type
                     (cond ((and type-name
                                 (or (eq fs-type type-name)
                                     (subtype-p fs-type type-name)))
                            (format t "~%Error in ~A: ~%  Type ~A occurs in constraint ~
                                       for type ~A at ~:A"
                                    type-name fs-type type-name (reverse features-so-far))
                            nil)
                           (t (really-make-well-formed real-dag fs-type 
                                                       features-so-far type-name)))
                   nil)))))))


(defun find-type-of-fs (real-dag &optional id path)
   (let* ((existing-features (top-level-features-of real-dag))
          (current-type (unify-get-type real-dag))
          (possible-type 
            (if existing-features 
               (maximal-type-of-list existing-features)
               *toptype*)))
      (cond
         ((null possible-type) 
            (format t
               "~%Error in ~A:~%   No possible type for features ~A at path ~:A" 
               (or id "unknown") existing-features (reverse path))
            nil)
         ((greatest-common-subtype current-type possible-type))
         (t 
            (format t "~%Error in ~A:~%  Type of fs ~A at path ~:A is incompatible with ~
                       features ~:A which have maximal type ~A" 
                    (or id "unknown")
                  current-type (reverse path) existing-features possible-type)
            nil))))


(defun really-make-well-formed (real-dag fs-type features-so-far type-name)
  (when (really-make-features-well-formed real-dag features-so-far type-name)
    (let ((constraint
            ;; !!! outside here must stay within current visiting
            ;; generation
            (let ((*visit-generation* *visit-generation*))
               ;; if we're making a type wf then type-name is non-nil - in this case
               ;; we must always copy constraints before unifying in to ensure that
               ;; no types contain structure in common in the end
               ;; - otherwise we only need a copy in cases where a particular type
               ;; constraint is being used more than once
               (if type-name
                  (copy-dag-completely (wf-constraint-of fs-type))
                  (may-copy-constraint-of fs-type)))))
      (cond
       ((null constraint) 
          (error ; real error in code
                 "~%No constraint found for ~A" fs-type))
       ((progn
          (setq real-dag (retype-dag real-dag fs-type))
          (unify-wffs real-dag constraint type-name)))
       (t (format t
             "~%Error in ~A:~%  Unification with constraint of ~A failed at path ~:A"
             (or type-name "unknown") fs-type (reverse features-so-far))
          nil)))))

;; (defun really-make-features-well-formed (real-dag features-so-far type-name)
;;   (every #'(lambda (label)
;;                   (make-well-formed (get-dag-value real-dag label)
;;                                     (cons label features-so-far)
;;                                     type-name))
;;          (top-level-features-of real-dag)))

(defun really-make-features-well-formed (real-dag features-so-far type-name)
  (loop for label in (top-level-features-of real-dag)
      always (make-well-formed (get-dag-value real-dag label)
			       (cons label features-so-far)
			       type-name)))

;;; It is possible for two wffs to be unified and the result to need 
;;; the constraint of the resulting type to be unified in - 
;;; signalled by *unify-wffs* being t

(defun unify-wffs (dag1 dag2 &optional expanding-types)
  (let ((*unify-wffs* t)
        (*expanding-types* expanding-types))
    (unify-dags dag1 dag2)))

(defun unifiable-wffs-p (dag1 dag2)
  (let ((*unify-wffs* t)) 
    (unifiable-dags-p dag1 dag2)))

;;; to support interactive unification checking
;;; which informs the user where unification failed

(defun unify-wffs-with-fail-messages (dag1 dag2 path)
  ;; non-destructive
  (declare (ignore path))
  (let ((*unify-debug* t)) 
    (unify-wffs dag1 dag2)))


;;; End of file

(defun compress-dag (dag)
  (when (dag-p dag)
    (setq dag (deref-dag dag))
    (setf (dag-x-comp-arcs-slot dag) nil)
    (setf (dag-x-copy dag) nil)
    (setf (dag-x-forward dag) nil)
    (dolist (arc (dag-arcs dag))
      (compress-dag (dag-arc-value arc)))))
