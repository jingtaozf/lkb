;;; Copyright (c) 1997--2018 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

;;; LKB
;;;
;;; Base level unification of dags
;;;
;;; Copyright Rob Malouf, John Carroll 1997-1998 All Rights Reserved.
;;; CSLI, Stanford University
;;; No use or redistribution without permission.

(in-package :lkb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-unification-context)))


;;; **********************************************************************
;;; Each temporary value set by the unifier is marked with a unification
;;; generation counter.  That way, we can invalidate all values set by an
;;; computation simply by incrementing the counter.

(#+:sbcl sb-ext:defglobal #-:sbcl defvar *unify-generation* 2)
(#+:sbcl sb-ext:defglobal #-:sbcl defvar *unify-generation-max* 2)
(defvar *visit-generation* 1)
(defvar *visit-generation-max* 1)

#+:sbcl
(declaim
   (sb-ext:always-bound *unify-generation* *unify-generation-max*))
(declaim
   (fixnum *unify-generation* *unify-generation-max*
           *visit-generation* *visit-generation-max*))

(defun invalidate-marks ()
   (setq *unify-generation*
      (+ (max *unify-generation-max* *unify-generation*) 2))
   (when (oddp *unify-generation*)
      (error "unify generation has unexpectedly become odd"))
   (setq *unify-generation-max* *unify-generation*))

(defun invalidate-visit-marks ()
   (setq *visit-generation*
      (1+ (max *visit-generation-max* *visit-generation*)))
   (setq *visit-generation-max* *visit-generation*))

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
(defvar *unify-debug* nil)
(defvar *unify-debug-cycles* nil) ; report when cycle found during copy?
#+:sbcl
(declaim (sb-ext:always-bound *within-unification-context-p* *unify-debug* *unify-debug-cycles*))

(defvar *safe-not-to-copy-p* nil)


;;; ***********************************************************************
;;; The structure for representing a feature structure node. Slots prefixed
;;; with x- must be accessed only via macros below, not directly
;;; Size: 64 bytes in 64-bit SBCL

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct (dag
             (:constructor make-dag-x (type arcs))
             (:copier copy-dag-x))
   (type nil)
   (arcs nil)
   ;; generation counter for the subsequent 3 temporary slots
   (x-generation 0 :type fixnum)
   ;; new type computed during unification / pointer to representative for fs's equivalence class
   (x-new-type/forward nil)
   ;; new arcs computed during a unification
   (x-comp-arcs nil)
   ;; pointer to a copy of this fs
   (x-copy nil)
   ;; independent slot used when traversing a fs doing interleaved unifications
   (x-visit-slot nil))

(defstruct (safe-dag
             (:include dag)
             (:constructor make-safe-dag-x (type arcs))
             (:copier copy-safe-dag-x))
   ;; save a slot that would just hold a boolean by making 'safe' dags a different type
   )
)

#+:sbcl
(declaim (sb-ext:freeze-type dag safe-dag))


(defmacro current-generation-strict-p (dag)
   `(= (the fixnum (dag-x-generation ,dag)) (the fixnum *unify-generation*)))

(defmacro current-generation-p (dag)
#|
   `(current-generation-strict-p ,dag)
|#
   ;; ignore least significant bit of dag generation wrt global generation
   `(<= (the fixnum
          (logxor (the fixnum (dag-x-generation ,dag)) (the fixnum *unify-generation*)))
        1)
   )

(defmacro with-dag-optimize ((dag) &body body)
   ;; wrap around code accessing a dag so we could potentially cause the compiler not
   ;; to insert any type checks - but in practice with modern compilers this gains
   ;; very little 
   #-:lkb-nochecks (declare (ignore dag))
   `(locally
       #+:lkb-nochecks (declare ,@(if (symbolp dag) `((type dag ,dag)))
                               (optimize (speed 3) (safety 0) (space 0)))
      ,@body))

(defmacro with-verified-dag ((dag) &body body)
   ;; wrap around code when we know the dag has previously been type checked
   ;; in case the compiler has not spotted this
   `(locally (declare ,@(if (symbolp dag) `((type dag ,dag)))
                      (optimize (speed 3) (safety 0) (space 0)))
       ,@body))


;;;
;;; first attempt at dag recycling.  to reduce creation of garbage, keep a pool
;;; of (safe) dag instances.  while parsing (i.e. when creating temporary data)
;;; use dags from the pool rather than allocating new ones.  the parser has to
;;; record the initial pool pointer (on entry) and can then reset the pointer
;;; once the parse has completed.  this should reduce dynamic allocation quite
;;; significantly (at the minor cost of slightly increased initial image size);
;;; garbage collection time should drop accordingly.        (17-jul-99  -  oe)
;;;

;;;
;;; some general thoughts on dag recycling:
;;;
;;; processing typically is organized in discrete intervals, e.g. parsing one
;;; sentence or generating from one input.  within one interval, large amounts
;;; of temporary data are created that mostly become garbage as the interval
;;; is completed.  a big chunk of garbage results from dag nodes allocated in
;;; the unifier.  after one parse is completed, say, and the chart is emptied,
;;; all references (pointers) to edges created during that parse are dropped;
;;; accordingly, references to feature structures associated with those edges
;;; disappear; accordingly, references to dag nodes within those structures.
;;;
;;; although the processor after completion of a parse knows explicitly that
;;; all the temporary data will no longer be used, there is no direct way to
;;; take advantage of this knowledge.  instead, the application has to wait for
;;; the garbage collector to eventually infer that knowledge (viz. from lack
;;; of references to those objects) from the overall state of the Lisp system,
;;; and only then dispose of the garbage; call this indirect memory management.
;;;
;;; the dag pool is an attempt to do direct memory management and allow the
;;; processor to use knowledge about temporary data that is no longer used.
;;; the pool is a generic data structure that has the following slots:
;;;
;;;   - size: overall size (in number of objects) of this pool;
;;;   - position: current fill pointer;
;;;   - data: vector of objects held in this pool;
;;;   - constructur: function to allocate new objects when pool has exhausted;
;;;   - garbage: counter for pool accesses after exhaustion.
;;;
;;; currently, there is one pool for dag nodes that is initialized (to a size
;;; of *dag-pool-size*) in `dag.lisp'; i.e. *dag-pool-size* many dag nodes are
;;; allocated at load() time and stored in the .data. vector.  the dag node
;;; constructor make-dag() on each call tries to retrieve a dag object from the
;;; pool; if the pool is not exhausted it returns a pointer to the dag node at
;;; the current pool position and increments the fill pointer; if allocation
;;; from the pool fails, it falls back into calling the actual constructor; it
;;; will allocate a new object which will ultimately become garbage (in this 
;;; case the .garbage. counter is incremented to record that we overshot the
;;; pool size).
;;;
;;; the tricky thing is to decide on the interval boundaries and seal the pool
;;; from leakage.  leakage here means that after the pool position was reset,
;;; someone outside of the interval can (will) still make reference to a dag
;;; from the pool.  suppose that dag recycling was on in interactive mode: the
;;; pool pointer is reset (to 0) on entry into the parser.  since people may 
;;; have feature structure windows open browsing results from a previous parse,
;;; dag recycling could mean that parts of those feature structures suddenly
;;; are changed, because dag nodes have been reused (nb: it is not clear, the
;;; problem cannot be solved; it seems most of the display of previous results
;;; is closed or frozen anyway, when new input is parsed; in Allegro at least).
;;;
;;; in batch parsing, on the contrary, it can be guaranteed that once the next
;;; parse is started there will be no reference to edges created earlier.  the
;;; global *dag-recycling-p* is used to signal whether dag recycling shall be
;;; used (i.e. whether dag node allocation requests try the pool first).
;;;
;;;                                                       (7-sep-99  -  oe)
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct pool
    (size 0 :type fixnum)
    (position 0 :type fixnum)
    (constructor #'(lambda ()) :type function)
    (data #(nil) :type simple-vector)
    (garbage 0 :type fixnum)))

#+:pooling
(defmacro with-verified-pool ((pool) &body body)
  `(locally (declare ,@(if (symbolp pool) `((type pool ,pool)))
                     (optimize (speed 3) (safety 0) (space 0)))
     ,@body))

#+:pooling
(defun create-pool (size constructor)
  (#+:allegro excl:tenuring #-:allegro progn
      (let ((pool (make-pool :size size :constructor constructor))
            (data (make-array (+ size 1)
                              :initial-element nil 
                              :adjustable nil :fill-pointer nil)))
        (loop
            for i from 0 to (max 0 (- size 1))
            do (setf (svref data i) (funcall constructor)))
        (setf (pool-data pool) data)
        pool)))

#+:pooling
(defparameter *dag-pool* 
  (create-pool *dag-pool-size* #'(lambda () (make-safe-dag-x nil nil))))

#+:pooling
(defun reset-pool (pool &key forcep compressp)
  (when (or forcep compressp)
    (loop
        with data = (pool-data pool)
        for i from 0 to (pool-position pool)
        for dag = (svref data i) 
        when dag do
          (setf (dag-type dag) nil)
          (setf (dag-arcs dag) nil)
          (when compressp (compress-dag dag :recursivep nil))))
  (setf (pool-position pool) 0)
  (setf (pool-garbage pool) 0))
  

#+:pooling
(defun reset-pools (&key forcep compressp)
  (loop
      for pool in (list *dag-pool*)
      do (reset-pool pool :forcep forcep :compressp compressp)))


#+:pooling
(defmacro pool-next (pool)
  `(let* ((position (pool-position ,pool))
          (next
           (svref (the simple-vector (with-verified-pool (,pool)
                                       (pool-data ,pool)))
                  (the fixnum position))))
     (cond
      (next
       (with-verified-pool (,pool)
         (setf (pool-position ,pool)
           (the fixnum (1+ (the fixnum position)))))
       next)
      (t
       (with-verified-pool (,pool)
         (setf (pool-garbage ,pool)
           (the fixnum (1+ (the fixnum (pool-garbage ,pool))))))
       (funcall (with-verified-pool (,pool) (pool-constructor ,pool)))))))


;;; Dag creation

(defparameter *dag-recycling-p* nil)

(defmacro make-dag (&key type arcs)
  ;; safe not to copy this node? False for rules, lexical entries, type 
  ;; constraints - when we pick them or parts of them up those bits must be
  ;; copied else multiple uses in the same analysis could become reentrant.
  ;; True during parsing and generation - assumes that the same derived
  ;; constituent can't be used more than once in a single analysis
  `(if *safe-not-to-copy-p*
      #+:pooling 
      (if *dag-recycling-p*
        (make-safe-recycled-dag-x ,type ,arcs)
        (make-safe-dag-x ,type ,arcs))
      #-:pooling
      (make-safe-dag-x ,type ,arcs)
      (make-dag-x ,type ,arcs)))

#+:pooling 
(defun make-safe-recycled-dag-x (type arcs)
  (let* ((pool *dag-pool*)
         (new (pool-next pool)))
    ;; set type and arcs slots. Clear out others - some cpu overhead, but
    ;; reduces dynamic store footprint
    (setf (dag-type new) type)
    (with-verified-dag (new) ; it must really be a dag if we've got here
      (setf (dag-arcs new) arcs)
      (setf (dag-x-generation new) 0)
      new)))

(defmacro dag-safe-p (dag)
   ;; argument must be known already to be some type of dag structure
   `(with-dag-optimize (,dag)
      (safe-dag-p ,dag)))


(defmacro dag-forward (dag)
  ;; what would be separate structure slots for the forwarding pointer and new-type share
  ;; a single slot, since in 'Tomabechi World' a new type value is irrelevant if the dag
  ;; node forwards to another - although when we're wanting a forward value we need to
  ;; check whether there's actually a type occupying this slot instead
  (let ((v (gensym)))
    `(with-dag-optimize (,dag)
       (when (current-generation-p ,dag)
          (with-verified-dag (,dag)
             (let ((,v (dag-x-new-type/forward ,dag)))
                (unless (typep ,v '(or symbol string)) ,v)
                #+:ignore (if (typep ,v 'structure-object) ,v))))))) ; alternative

(defsetf dag-forward (dag) (new)
  `(with-dag-optimize (,dag)
     ;; if there's a type here already then it's made redundant by this forwarding pointer
     (setf (dag-x-new-type/forward ,dag) ,new)
     (with-verified-dag (,dag) 
        (unless (current-generation-p ,dag)
           (setf (dag-x-generation ,dag) *unify-generation*)
           (setf (dag-x-comp-arcs ,dag) nil)
           (setf (dag-x-copy ,dag) nil)))
     ,new))


(defmacro dag-new-type (dag)
  ;; although the forwarding pointer and new-type share a slot, if there's actually a
  ;; forwarding pointer (i.e. a dag) here then the dag has not been dereferenced properly
  `(with-dag-optimize (,dag)
     (when (current-generation-p ,dag)
        (with-verified-dag (,dag) 
           (dag-x-new-type/forward ,dag)))))

(defsetf dag-new-type (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-new-type/forward ,dag) ,new)
     (with-verified-dag (,dag) 
        (unless (current-generation-p ,dag)
           (setf (dag-x-generation ,dag) *unify-generation*)
           (setf (dag-x-comp-arcs ,dag) nil)
           (setf (dag-x-copy ,dag) nil)))
     ,new))


(defmacro dag-comp-arcs (dag)
  `(with-dag-optimize (,dag)
     (when (current-generation-p ,dag)
        (with-verified-dag (,dag) (dag-x-comp-arcs ,dag)))))

(defsetf dag-comp-arcs (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-comp-arcs ,dag) ,new)
     (with-verified-dag (,dag) 
        (unless (current-generation-p ,dag)
           (setf (dag-x-generation ,dag) *unify-generation*)
           (setf (dag-x-new-type/forward ,dag) nil)
           (setf (dag-x-copy ,dag) nil)))
     ,new))


(defmacro dag-copy (dag)
  `(with-dag-optimize (,dag)
     (when (current-generation-strict-p ,dag)
        (with-verified-dag (,dag) (dag-x-copy ,dag)))))

(defsetf dag-copy (dag) (new)
  `(with-dag-optimize (,dag)
     (setf (dag-x-copy ,dag) ,new)
     (with-verified-dag (,dag)
        (unless (current-generation-strict-p ,dag)
           (setf (dag-x-generation ,dag) *unify-generation*)
           (setf (dag-x-new-type/forward ,dag) nil)
           (setf (dag-x-comp-arcs ,dag) nil)))
     ,new))


(defmacro dag-visit (dag)
  `(with-dag-optimize (,dag)
     (when (and (consp (dag-x-visit-slot ,dag))
                (= (the fixnum
                     (with-verified-dag (,dag) (car (the cons (dag-x-visit-slot ,dag)))))
                   *visit-generation*))
        (with-verified-dag (,dag) (cdr (the cons (dag-x-visit-slot ,dag)))))))

(defsetf dag-visit (dag) (new)
  `(with-dag-optimize (,dag)
     (if (consp (dag-x-visit-slot ,dag))
        (with-verified-dag (,dag) 
           (setf (car (the cons (dag-x-visit-slot ,dag))) *visit-generation*
                 (cdr (the cons (dag-x-visit-slot ,dag))) ,new))
        (progn
           (with-verified-dag (,dag)
              (setf (dag-x-visit-slot ,dag) (cons *visit-generation* ,new)))
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
   #-:lkb-nochecks (declare (ignore arc))
   `(locally #+:lkb-nochecks (declare ,@(if (symbolp arc) `((type cons ,arc)))
                                     (optimize (speed 3) (safety 0) (space 0)))
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
;;; Outside unification, the follow-pointers function should be called instead
;;; (it could still be called inside a unification context e.g. when printing
;;; out dags during debugging). It can afford to do some useful consistency
;;; checks

(defmacro deref-dag (dag) 
   (let ((dag-var (if (consp dag) (gensym) dag)) ; new variable if necessary
         (next (gensym)))
      `(let ,(if (consp dag) `((,dag-var ,dag)) nil)
         (with-dag-optimize (,dag-var)
            (loop
               for ,next = (dag-forward ,dag-var)
               unless ,next return ,dag-var
               do (setq ,dag-var ,next))))))

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

;;; Convenient macros for dag creation

(defmacro create-typed-dag (type)
  `(make-dag :type ,type :arcs nil))

(defmacro create-dag nil
  `(make-dag :type *toptype* :arcs nil))

;;; NB don't use following inside unification since they ignore temporary structure
;;; (deref pointer, new-type, comp-arcs)

(defmacro has-features (dag)
   `(consp (dag-arcs ,dag)))

(defmacro type-of-fs (dag)
   `(dag-type ,dag))


(defun top-level-features-of (dag)
  (mapcar #'(lambda (arc) (dag-arc-attribute arc)) (dag-arcs dag)))

(defun get-dag-value (dag attribute)
   (dolist (arc (dag-arcs dag) nil)
      (when (eq attribute (dag-arc-attribute arc))
         (return-from get-dag-value (dag-arc-value arc)))))


(defun get-value-at-end-of (dag labels-chain)
   (cond
      ((null labels-chain) (type-of-fs dag))
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
      (when (catch '*fail* (unify1 dag1 dag2 nil))
        (if (or *unify-debug* *unify-debug-cycles*)
            (if (cyclic-dag-p dag1)
                ;;
                ;; for the (eq *unify-debug* :return) variant the %failure%
                ;; value is already recorded, so suppress printed output here.
                ;; the baroque conditionals preserve the original LKB behaviour.
                ;; (19-mar-99  -  oe)
                ;;
                (progn
                  (when (not (eq *unify-debug* :return))
                    (format t "~%Unification failed - cyclic result"))
                  nil)
                (progn
                  (when (and *unify-debug*
                             (not (eq *unify-debug* :return)))
                    (format t "~%Unification succeeded"))
                  dag1))
            dag1))
      (with-unification-context (dag1)
        (when (unify-dags dag1 dag2) (copy-dag dag1)))))

(defun unifiable-dags-p (dag1 dag2)
  (if *within-unification-context-p*
      (when (catch '*fail* (unify1 dag1 dag2 nil))
        (if (cyclic-dag-p dag1)
            (progn
              ;;
              ;; see above for baroque conditionals      (19-may-99  -  oe)
              ;;
              (when (and (or *unify-debug* *unify-debug-cycles*)
                         (not (eq *unify-debug* :return)))
                (format t "~%Unification failed - cyclic result"))
              nil)
            (progn
              (when (and *unify-debug*
                         (not (eq *unify-debug* :return)))
                (format t "~%Unification succeeded"))
              t)))
      (with-unification-context (dag1) (unifiable-dags-p dag1 dag2))))


;;; This is the heart of the unification algorithm, and is based on Hideto
;;; Tomabechi's paper in the 1991 ACL proceedings.  We walk through the two
;;; feature structures checking for compatibility and setting forward
;;; pointers.  As soon as we find a problem, we stop.  If we get through the
;;; whole structure without finding a problem, then we copy the first unifact
;;; using the forward pointers we set during the check.
;;;
;;; On each individual unification attempt we mark the dag on entry and unmark
;;; it on exit, so we can detect if we have ended up inside cyclic structure.

(defmacro unify-get-type (fs)
  `(or (dag-new-type ,fs) (dag-type ,fs)))

(defun unify1 (dag1 dag2 path)
  (setq dag1 (deref-dag dag1))
  (setq dag2 (deref-dag dag2))
  (cond
    ((eq (dag-copy dag1) :inside)
      (when (or *unify-debug* *unify-debug-cycles*)
        (setq path (reverse path))
        (if (eq *unify-debug* :return)
            (setf %failure% (list :cycle path))
            (format t "~%Unification failed: unifier found cycle at < ~{~A ~^: ~}>" path)))
      (throw '*fail* nil))
    ((eq dag1 dag2) dag1)
    (t
      (unify2 dag1 dag2 path))))

;;; (defparameter *recording-constraints-p* nil
;;;  "needed for LilFes conversion")
;;; stores cases where a new constraint is unified in -
;;; uncomment this defparameter and stuff below if necessary

(defvar *type-constraint-list* nil)

(defun unify2 (dag1 dag2 path)
  (let ((t1 (unify-get-type dag1))
        (t2 (unify-get-type dag2)))
    (multiple-value-bind (new-type constraintp)
        (greatest-common-subtype t1 t2)
      (if new-type
        (progn
          (unless (eq new-type t1) (setf (dag-new-type dag1) new-type))
          ;; an atomic type check here is superfluous because an atomic type
          ;; can't have a gcsubtype with a type that has features.  This means
          ;; that type constraints etc which specify features on atomic types
          ;; are only found when we try and make them well-formed

          ;; unify in constraints if necessary - may have to copy them to
          ;; prevent separate uses of same constraint in same unification
          ;; becoming reentrant
          (when (and constraintp *unify-wffs*)
            (let ((constraint (if *expanding-types*
                                  (possibly-new-constraint-of new-type)
                                  (may-copy-constraint-of new-type))))
              ;; (when *recording-constraints-p*
              ;;   (pushnew new-type *type-constraint-list* :test #'eq))
              (if *unify-debug*
                (unless (catch '*fail* (unify1 dag1 constraint path))
                  (if (eq *unify-debug* :return)
                      (setq %failure%
                        (list :constraints (reverse path) new-type nil nil))
                      (progn
                        (when *expanding-types*
                          (format t "~%Problem in ~A" *expanding-types*))
                        (format t
"~%Unification with constraint of type ~A failed at path < ~{~A ~^: ~}>"
                          new-type (reverse path))))
                  (throw '*fail* nil))
                (setq dag1 (unify1 dag1 constraint path)))))

          ;; cases for each of dag1 and dag2 where they have no arcs just
          ;; considering straightforward use of unify1: if we've
          ;; previously visited a node with no arcs then it must have got
          ;; forwarded then so we won't ever visit it again - so no need
          ;; to test for presence of any comp-arcs BUT:
          ;; unify-paths-dag-at-end-of1 adds to comp-arcs independently so
          ;; we do need the additional tests
          (cond
            ((and (null (dag-arcs dag1)) (null (dag-comp-arcs dag1)))
              (unless (eq new-type t2) (setf (dag-new-type dag2) new-type))
              (setf (dag-forward dag1) dag2))
            ((and (null (dag-arcs dag2)) (null (dag-comp-arcs dag2)))
              (setf (dag-forward dag2) dag1))
            (t
              (setf (dag-forward dag2) dag1)
              (setf (dag-copy dag1) :inside)
              (unify-arcs dag1 dag2 path)
              (setf (dag-copy dag1) nil)
              dag1)))

        ;; type unification failure
        (progn
          (when *unify-debug*
            (if (eq *unify-debug* :return)
                (setq %failure% (list :clash (reverse path) t1 t2))
                (let ((msg
                        (format nil
                          "Unification of ~A and ~A failed at path < ~{~A ~^: ~}>"
                          t1 t2 (reverse path))))
                  (when (eq *unify-debug* :window)
                    (show-message-window msg))
                  ;; deliberately also show in the LKB top as before, since some
                  ;; people may have got used to it there
                  (format t "~%~A" msg))))
          (throw '*fail* nil))))))

(defun unify-arcs (dag1 dag2 path)
  (let* ((arcs1 (dag-arcs dag1))
         (arcs1-tail arcs1)
         (comp-arcs1 (dag-comp-arcs dag1))
         (new-arcs1 comp-arcs1))
    (macrolet
      ((process-arcs (arcs2)
         `(dolist (elem2 ,arcs2)
            (let* ((v (dag-arc-attribute elem2))
                   (elem1
                     (block find-attribute
                       ;; since attributes are usually 'nearly-sorted', traverse arcs1
                       ;; starting just beyond the last match
                       (do ((tail arcs1-tail (cdr tail)))
                           ((null tail))
                           (when (eq (dag-arc-attribute (car tail)) v)
                             (setq arcs1-tail (cdr tail))
                             (return-from find-attribute (car tail))))
                       (do ((tail arcs1 (cdr tail)))
                           ((eq tail arcs1-tail))
                           (when (eq (dag-arc-attribute (car tail)) v)
                             (setq arcs1-tail (cdr tail))
                             (return-from find-attribute (car tail))))
                       ;; more straightforward approach for the usually shorter comp-arcs1
                       (dolist (a comp-arcs1)
                         (when (eq (dag-arc-attribute a) v)
                           (return-from find-attribute a)))
                       nil)))
              (if elem1
                  (let ((new-path (cons v path)))
                    (declare (dynamic-extent new-path))
                    (unify1 (dag-arc-value elem1) (dag-arc-value elem2) new-path))
                  (push elem2 new-arcs1))))))
      (process-arcs (dag-arcs dag2))
      (process-arcs (dag-comp-arcs dag2)))
    (unless (eq new-arcs1 comp-arcs1) (setf (dag-comp-arcs dag1) new-arcs1))))


(defun possibly-new-constraint-of (new-type)
  ;;; only called when type hierarchy is being expanded
  ;;; therefore need to check that new-type actually
  ;;; has a well-formed constraint and fail informatively if not
  (let ((new-constraint (wf-constraint-of new-type)))
    (if new-constraint 
        (copy-dag-completely new-constraint)
      (progn
        (when *unify-debug*
          (if (eq *unify-debug* :return)
              (setf %failure% 
                (list :illformed-constraint nil new-type nil nil))
              (format t "Problem in ~A due to ~A" 
                *expanding-types* new-type)))
        (throw '*fail* nil)))))


;;; similar to constraint-of return the type's constraint, a 'fresh'
;;; copy if we've previously returned it during this unification
;;; attempt, otherwise as is. Copies are cached and we cycle through
;;; them, creating new ones if needed we want normal 'safe' dag
;;; behaviour, i.e. when we're parsing/generating we don't need the
;;; copy itself to be 'safe' wrt copying. In all other cases we want
;;; it safe

(defun may-copy-constraint-of (type-name)
  (let* ((type-record (get-type-entry type-name))
         (constraint (ltype-constraint type-record))
         (cache (ltype-constraint-mark type-record))
         (*safe-not-to-copy-p* nil)
         (*dag-recycling-p* nil))
    (unless (consp cache)
      (setq cache (list* 0 nil nil))    ; mark, unused, used
      (setf (ltype-constraint-mark type-record) cache))
    (cond
     ((not (= (the fixnum (car cache)) *unify-generation*))
      #+:cdebug
      (format 
       t 
       "~&may-copy-constraint-of(): `~(~a~)' cache hit; new cycle;~%"
       type-name)
      (setf (car cache) *unify-generation*)
      (setf (cadr cache)
        (nconc (cadr cache) (cddr cache))) ; old used copies become ready for use
      (setf (cddr cache) nil)
      constraint)                        ; first return constraint itself
     ((cadr cache)
      #+:cdebug
      (format 
       t 
       "~&may-copy-constraint-of(): `~(~a~)' cache hit;~%"
       type-name)
      (let ((pre (pop (cadr cache))))
        (push pre (cddr cache))                ; previously computed copy becomes used
        pre))
     (t
      #+:cdebug
      (format 
       t 
       "~&may-copy-constraint-of(): `~(~a~)' cache miss (~d);~%"
       type-name (+ (length (cadr cache)) (length (cddr cache))))
      (let ((new (copy-dag-completely constraint)))
        (push new (cddr cache))                ; new copy becomes used
        new)))))


;;; Copy first feature structure after a successful unification, respecting
;;; any forward pointers set by the unifier.

(defun copy-dag (dag)
  (catch '*cyclic* (copy-dag1 dag nil)))

;;; Tomabechi/Rob/John: no copy made when dag is 'safe', type has not changed,
;;; no comp-arcs, and no copied dags underneath. No new structure created in case
;;; where no lower level dags need to be copied. Must pass the feature path down
;;; through the copy functions; we cannot consider putting it in the copy slot since
;;; then it would hang around afterwards, which is bad news for stack allocated conses

(defun copy-dag1 (dag path &aux copy (comp-arcs-inserted-p nil))
  ;; JAC 21-Jan-2019: numbers of edges and parses are sensitive to this function's precise
  ;; workings at grammar load time (but not at parse time) - needs investigating
  ;; the number of parses can differ if comp-arcs are prepended to arcs and not to shared
  ;; tail, and the number of edges can differ if comp-arcs is not reversed
  (labels
    ((copy-dag1-arcs (arcs comp-arcs)
       ;; we can share arcs and the tail below the last arc that is copied, but we must
       ;; not actually modify any of the list structure
       ;; we recurse on the arcs tail instead of iterating across the arcs so we can
       ;; build the new arcs list starting from the end
       (if arcs
           (let* ((arc (car arcs))
                  (new-path (cons (dag-arc-attribute arc) path))
                  (v (copy-dag1 (dag-arc-value arc) new-path))
                  (tail (copy-dag1-arcs (cdr arcs) comp-arcs)))
             (declare (dynamic-extent new-path))
             (cond
               ((not (eq v (dag-arc-value arc)))
                 (unless comp-arcs-inserted-p
                   (setq comp-arcs-inserted-p t)
                   (setq tail (nreconc comp-arcs tail)))
                 (cons
                   (make-dag-arc :attribute (dag-arc-attribute arc) :value v)
                   tail))
               ((not (eq tail (cdr arcs)))
                 (cons arc tail))
               (t arcs)))))) ; good, we are able to return 'arcs' as sharable
    (declare (dynamic-extent copy-dag1-arcs)) ; closure on stack
    (setq dag (deref-dag dag))
    (setq copy (dag-copy dag))
    (cond
      ((eq copy :inside)
        (when (or *unify-debug* *unify-debug-cycles*)
          (setq path (reverse path))
          (if (eq *unify-debug* :return)
              (setq %failure% (list :cycle path))
              (format t "~%Copy found cycle at < ~{~A ~^: ~}>" path)))
        (throw '*cyclic* nil))
      ((not (symbolp copy)) ; already copied?
        copy)
      (t
        (setf (dag-copy dag) :inside)
        (let ((new-type (dag-new-type dag))
              (arcs (dag-arcs dag))
              (comp-arcs (dag-comp-arcs dag)))
          ;; in comp-arcs we can share arcs and modify any of the list structure
          (loop for tail on comp-arcs
            do
            (let ((new-path (cons (dag-arc-attribute (car tail)) path)))
              (declare (dynamic-extent new-path))
              (let ((v (copy-dag1 (dag-arc-value (car tail)) new-path)))
                (unless (eq v (dag-arc-value (car tail)))
                  (setf (car tail)
                    (make-dag-arc
                      :attribute (dag-arc-attribute (car tail))
                      :value v))))))
          (setq arcs (copy-dag1-arcs arcs comp-arcs))
          (when (and comp-arcs (not comp-arcs-inserted-p))
            (setq arcs (nreconc comp-arcs arcs)))
          (setf (dag-copy dag)
            (if (or (not (dag-safe-p dag))
                    new-type
                    (not (eq arcs (dag-arcs dag))))
                (make-dag :type (or new-type (dag-type dag)) :arcs arcs)
                dag)))))))


#|
;;; Basic version - does not reuse any of the existing list or arc structure, nor does it
;;; record path for circularity message

(defun copy-dag1 (dag &optional path)
  (let* ((dag (deref-dag dag))
         (copy (dag-copy dag)))
    (cond
      ((eq copy :inside)
        (when (or *unify-debug* *unify-debug-cycles*) (format t "~%Copy found cycle"))
        (throw '*cyclic* nil))
      ((not (symbolp copy)) ; already copied?
        copy)
      (t
        (let ((new-arcs nil))
          (setf (dag-copy dag) :inside)
          (dolist (arc (dag-arcs dag))
            (push
              (make-dag-arc :attribute (dag-arc-attribute arc)
                            :value (copy-dag1 (dag-arc-value arc) nil))
              new-arcs))
          (dolist (arc (dag-comp-arcs dag))
            (push
              (make-dag-arc :attribute (dag-arc-attribute arc)
                            :value (copy-dag1 (dag-arc-value arc) nil))
              new-arcs))
          (setf (dag-copy dag)
            (if (or (not (dag-safe-p dag))
                    (dag-new-type dag)
                    new-arcs)
                (make-dag :type (unify-get-type dag) :arcs (nreverse new-arcs))
                dag)))))))
|#


;;; **********************************************************************

;;; Physically copy a dag - leaves the source dag untouched.
;;; Assumes all forwarding pointers, comp-arcs, and new-type have been dealt
;;; with already (i.e. by copy-dag)
;;; Use -visit field not -copy since may be called from within unify

(defun copy-dag-completely (dag)
   (invalidate-visit-marks)
   (copy-dag-completely1 dag (create-dag)))

(defun copy-dag-completely1 (dag toptype-dag)
   (or (dag-visit dag)
      (progn
         (setf (dag-visit dag) toptype-dag) ; 3/98 - avoid crashes with cyclic dags
         (let ((new-instance 
                  (make-dag
                     :type (dag-type dag) 
                     :arcs
                     (mapcar
                        #'(lambda (arc)
                            (let ((label (dag-arc-attribute arc)))
                               (make-dag-arc :attribute label
                                  :value
                                  (copy-dag-completely1
                                     (dag-arc-value arc) toptype-dag))))
                        (dag-arcs dag)))))
            (setf (dag-visit dag) new-instance)))))


;;;
;;; _fix_me_
;;; functions to facilitate ambiguity packing: currently, hard-wire which parts
;;; of the structure have to go: everything below `CONT' (at all levels) except
;;; for `CONT|MESSAGE'.  this badly needs a generalization using both PAGE-type
;;; restrictors as well as a mechanism to extinct all occurences of a feature.
;;;                                                          (12-nov-99  -  oe)
;;; --- it looks like we improved some of the above, with *packing-restrictor*
;;; as a global naming features to be erased wherever they occur.  still, the
;;; PAGE style restrictor (allowing full path specifications and re-entrancies
;;; in the restrictor by using a restrictor dag as a `mask') would be a good
;;; thing to have.  berthold, for the german grammar, reports the cannot get
;;; full ROI from packing, since he would require a more powerful restrictor
;;; to eliminate a feature in some context but not in others, since otherwise
;;; there is too much inconsistency in the forest.  something to do with 
;;; partial VP fronting, and he wants it for PET anyway, but still worth for us
;;; to keep in mind.                                            (30-oct-04; oe)
;;;
;;; We also need to know about minimal types for feature values
;;; for packing.  The following code caches the values as accessed
;;; (replaced property lists with hash table)

(defun minimal-type-for (feature)
  (or (gethash feature *feature-minimal-type*)
      (let* ((introduction (maximal-type-of feature))
             (constraint (and introduction (constraint-of introduction)))
             (type (or (and constraint 
                            (type-of-fs (get-dag-value constraint feature)))
                       *toptype*)))
        (setf (gethash feature *feature-minimal-type*) type)
        type)))

(defun copy-dag-partially (dag)
  (invalidate-visit-marks)
  (copy-dag-partially1 dag nil))

(defun copy-dag-partially1 (old path)
  (if (dag-visit old)
    (dag-visit old)
    (let* ((restrictp (when *packing-restrictor*
                        (smember (first path) *packing-restrictor*)))
           (type (if restrictp
                   (minimal-type-for (first path))
                   ;;
                   ;; _fix_me_
                   ;; experiment more with generalizing the null path, since
                   ;; the ERG has a distinct top-level type on each rule, i.e.
                   ;; we will never pack two edges unless they were built from
                   ;; the same rule; see the comment in `fs.cpp' in PET too.
                   ;;                                          (21-sep-05; oe)
                   ;; --- doing this unconditionally causes termination issues
                   ;; in grammars that encode the lexeme vs. word distinction
                   ;; in the top-level types of lexical rules; maybe the (new)
                   ;; restrictor language should allow inclusion of the null
                   ;; path then.                               (30-may-06; oe)
                   ;;
                   (if #+:null (null path) #-:null nil
                     *toptype*
                     (dag-type old))))
           (arcs (unless restrictp
                   (loop
                       for arc in (dag-arcs old)
                       for label = (dag-arc-attribute arc)
                       collect 
                         (let ((path (cons label path)))
                           (declare (dynamic-extent path))
                           (make-dag-arc :attribute label
                                         :value (copy-dag-partially1
                                                 (dag-arc-value arc) 
                                                 path)))))))
      (setf (dag-visit old) (make-dag :type type :arcs arcs)))))


;;; **********************************************************************

;;; Test for cycles explicitly. Copy-dag will return nil if there are cycles,
;;; but we can use this in the limited circumstances where we just want to
;;; check for cycles and not do the copy.
;;; We use the copy slot, so have to set up a new unification context if we're
;;; not already in one. If we are, we'll be leaving :outside in copy slots
;;; so any subsequent processing must take care. On entry we assume that copy
;;; slots will either be nil or contain a dag (if there's already been a copying
;;; phase within this unification context).

(defun cyclic-dag-p (dag)
  ;; return t if dag is cyclic
  (labels
    ((cyclic-dag-p1 (dag &aux copy)
       (macrolet
         ((check-arcs (arcs)
            `(dolist (arc ,arcs)
               (let ((c (cyclic-dag-p1 (dag-arc-value arc))))
                 (when c
                   (return-from cyclic-dag-p1 (cons (dag-arc-attribute arc) c)))))))
         (setq dag (deref-dag dag))
         (setq copy (dag-copy dag))
         (cond
           ((eq copy :outside) nil)
           ((eq copy :inside)
              (list t)) ; cycle detected
           ((not (symbolp copy)) ; has copy-dag already been here and checked?
              nil)
           ((or (dag-arcs dag) (dag-comp-arcs dag))
              (setf (dag-copy dag) :inside)
              (check-arcs (dag-arcs dag))
              (check-arcs (dag-comp-arcs dag))
              (setf (dag-copy dag) :outside)
              nil)))))
    (if *within-unification-context-p*
        (let ((c (cyclic-dag-p1 dag)))
          (if c
              (progn
                (when (or *unify-debug* *unify-debug-cycles*)
                  (setq c (butlast c)) ; remove final t flag
                  (if (eq *unify-debug* :return)
                      (setq %failure% (list :cycle c))
                      (format t "~%Cyclic check found cycle at < ~{~A ~^: ~}>" c)))
                t)
              nil))
        (with-unification-context (dag) (cyclic-dag-p dag)))))

;; Remove the marks left by cyclic-dag-p

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


(defun replace-dag-types (dag path replace-alist)
  (if *within-unification-context-p*
    ;; (format t "~%replace-dag-types ~a ~a" path replace-alist)
    (let ((dag2
            (unify-paths-dag-at-end-of1 dag path))) ; should complain if path doesn't exist?
      (replace-dag-types-aux dag2 replace-alist)
      dag)
    (with-unification-context (dag)
      (replace-dag-types dag path replace-alist)
      (copy-dag dag))))

(defun replace-dag-types-aux (dag replace-alist)
  (flet
    ((replace-dag-types-arcs (arcs replace-alist)
      (loop
          for arc in arcs
          for dag2 = (deref-dag (dag-arc-value arc))
          for type = (or (dag-new-type dag2) (dag-type dag2))
          if (or (eq type *toptype*) (eq type *string-type*))
          do ;; arc value is fully unspecific - now check attribute to see whether to replace
            (let ((repl
                    (cdr (assoc (dag-arc-attribute arc) replace-alist :test #'eq))))
              (when repl
                ;; (format t "~%set ~a -> ~a" (dag-arc-attribute arc) repl)
                (setf (dag-new-type dag2) repl)))
          else
          do ;; arc value is specific so recurse on it
            (replace-dag-types-aux dag2 replace-alist))))
    (replace-dag-types-arcs (dag-arcs dag) replace-alist)
    (replace-dag-types-arcs (dag-comp-arcs dag) replace-alist)
    dag))
    

;;; **********************************************************************

;;; WFFs

;;; Well-formedness checking
;;; we assume we have a feature structure which we want to convert
;;; to a well-formed fs if possible.
;;; 1. Push down type so all features are appropriate
;;; 2. Recurse on features
;;; 3. Unify with constraint of new type 
;;; (given that this is well-formed)

;;;
;;; _fix_me_
;;; see comments on process-unifications() regarding the need for a separate
;;; unification context here; i should email john and ann about this, i guess.
;;;                                                              (7-dec-06; oe)
(defun create-wffs (fs &optional (contextp t))
  ;; non-destructive!
  ;; returns nil on failure
  (cond
   (contextp
    (with-unification-context (fs) (create-wffs fs nil)))
   (t
    (invalidate-visit-marks)
    (let ((res (make-well-formed fs nil)))
      (when res (copy-dag fs))))))


(defun make-well-formed (fs features-so-far &optional type-name)
  ;;; this code could no doubt be sped up quite a lot by being more
  ;;; careful about multiple lookups etc
   (let ((real-dag (deref-dag fs)))
     (or (dag-visit real-dag)        ; been here before
         (let ((current-type (unify-get-type real-dag)))
           (setf (dag-visit real-dag) t)
           (if (atomic-type-p current-type)
               ;; this does a type table lookup, which will
               ;; be repeated below if the type isn't atomic
                (if (not (has-features real-dag))
                    ;; atomic types are well-formed by definition 
                    ;; as long as there aren't any features on the dag
                    t
                  (progn 
                    (format t "~%Error in ~A: ~% Atomic type ~A specified to have features at ~:A" type-name current-type (reverse features-so-far))
                    nil))
             (let
                 ((fs-type (find-type-of-fs real-dag current-type
                                            type-name features-so-far)))
               (if fs-type
                   (cond ((and type-name
                               (or (eq fs-type type-name)
                                   (subtype-p fs-type type-name)))
                          (format t "~%Error in ~A: ~%  Type ~A occurs in constraint ~
                                       for type ~A at ~:A"
                                  type-name fs-type type-name (reverse features-so-far))
                          nil)
                         (t
                          (really-make-well-formed real-dag fs-type 
                                                   features-so-far type-name)))
                 nil)))))))


(defun find-type-of-fs (real-dag current-type id path)
   (let* ((existing-features (top-level-features-of real-dag))
          (possible-type 
           (if existing-features
               (maximal-type-of-list existing-features)
               *toptype*)))
      (cond
         ((null possible-type) 
            (format t
               "~%Error in ~S:~%   No possible type for features ~S at path ~:S" 
               (or id "unknown") existing-features (reverse path))
            nil)
         ((not existing-features) current-type)
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
                  (catch '*fail*
                    (possibly-new-constraint-of fs-type))
                  (may-copy-constraint-of fs-type)))))
      (cond
       ((null constraint) 
        ;;          (format t
        ;;                  "~%No well-formed constraint for ~A" fs-type)
        ;; we've already warned about this, and duplication is annoying
          nil)
       ((progn
          (setq real-dag (retype-dag real-dag fs-type))
          (unify-wffs real-dag constraint type-name)))
       (t (format t
             "~%Error in ~A:~%  Unification with constraint of ~A failed at path ~:A"
             (or type-name "unknown") fs-type (reverse features-so-far))
          nil)))))

(defun really-make-features-well-formed (real-dag features-so-far type-name)
  (loop for arc in (dag-arcs real-dag)
    always
    (let ((path (cons (dag-arc-attribute arc) features-so-far)))
      (declare (dynamic-extent path))
      (make-well-formed (dag-arc-value arc)
                        path
                        type-name))))

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

(defun unify-wffs-with-fail-messages (dag1 dag2 path &optional window-p)
  ;; non-destructive
  (declare (ignore path))
  (let ((*unify-debug* (if window-p :window t))) 
    (unify-wffs dag1 dag2)))


;;; Get rid of pointers to any temporary dag structure

(defun compress-dag (dag &key (recursivep t))
  ;; no point in deref-ing dag since we don't expect to be called within
  ;; a unification context (so if there's a forward pointer it won't be
  ;; valid anyway)
  (when (and dag
             ;; if no temp dags at this level then don't recurse - if there are
             ;; any then they will be reached via another re-entrancy
             (or (dag-x-new-type/forward dag) (dag-x-comp-arcs dag) (dag-x-copy dag)
                 (dag-x-visit-slot dag)))
    (with-verified-dag (dag) ; it's definitely a dag
      (setf (dag-x-new-type/forward dag) nil)
      (setf (dag-x-comp-arcs dag) nil)
      (setf (dag-x-copy dag) nil)
      (setf (dag-x-visit-slot dag) nil)) ; copy-dag-completely can put a dag in this
    (when recursivep
      (dolist (arc (with-verified-dag (dag) (dag-arcs dag)))
        (compress-dag (dag-arc-value arc))))))

(defun find-substructures-subsumed-by (dag type &optional path)
  (let* ((dag (deref-dag dag)))
    (append
     (when (subtype-or-equal (type-of-fs dag) type)
       (list (cons path dag)))
     (loop
         for arc in (dag-arcs dag)
         for next = (cons (dag-arc-attribute arc) path)
         nconc (find-substructures-subsumed-by
                (dag-arc-value arc) type next)))))

(defun list-to-dag (dags)
  (if (null dags)
    (make-dag :type *empty-list-type*)
    (let ((dag (make-dag :type *non-empty-list-type*)))
      (setf (dag-arcs dag)
        (list
         (make-dag-arc
          :attribute (first *list-head*)
          :value (first dags))
         (make-dag-arc
          :attribute (first *list-tail*)
          :value (list-to-dag (rest dags)))))
      dag)))


(defun dag-to-list (dag &key end key)
  (labels ((collect (dag &key end)
             (let* ((dag (deref-dag dag))
                    (first (existing-dag-at-end-of dag *list-head*))
                    (rest (existing-dag-at-end-of dag *list-tail*)))
               (unless (or (null first) (null rest) (eq dag end))
                 (let ((value (if key (funcall key first) first)))
                   (cons value (collect rest :end end)))))))
    (let* ((dag (deref-dag dag))
           (list (get-dag-value dag *diff-list-list*))
           (last (get-dag-value dag *diff-list-last*)))
      (if (and list last)
        (collect list :end (or end (deref-dag last)))
        (collect dag :end end)))))

;;; End of file
