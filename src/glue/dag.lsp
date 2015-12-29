;;; Copyright (c) 1997--2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;;
;;; keep track of all conflicts among two structures; collect for interactive
;;; unification and display in new AVM browser.              (13-apr-04; oe)
;;;
(defparameter *failure-raw-output-p* nil)

(defparameter *unify-robust-p* nil)

(defparameter %failures% nil)

;;;
;;; _fix_me_
;;; moving to ACL 8.0, the following generates a compiler warning (`undeclared
;;; variable N') in the constructor; apparently lexical closure policies have
;;; changed.  i wonder what ANSI CL has to say about this.     (31-jan-06; oe)
;;;
#+:null
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (let ((n -1))
    (defun reset-failure-count () 
      (setf n -1))

    (defstruct failure
      (id (incf n))
      nature type1 type2 glb path suffix context)))

(defparameter %failures-counter% -1)

(defun reset-failure-count () 
  (setf %failures-counter% -1))

(defstruct failure
  (id (incf %failures-counter%))
  nature type1 type2 glb path suffix context)


(defmethod print-object ((object failure) stream)
  (if *failure-raw-output-p*
    (call-next-method)
    (case (failure-nature object)
      (:type
       (format 
        stream 
        "#U[type ~:[-1~*~;~a~] [~:[~*~;~{~a~^ ~}~]] ~
            ~:[top~*~;~a~] ~:[top~*~;~a~] ~:[-1~*~;~a~]]"
        (failure-id object) (failure-id object)
        (failure-path object) (failure-path object)
        (failure-type1 object) (failure-type1 object)
        (failure-type2 object) (failure-type2 object)
        (failure-context object) (failure-context object)))
      (:cycle
       (format 
        stream 
        "#U[cycle ~:[-1~*~;~a~] [~:[~*~;~{~a~^ ~}~]] [~:[~*~;~{~a~^ ~}~]] ~
            ~:[-1~*~;~a~]]"
        (failure-id object) (failure-id object) 
        (failure-path object) (failure-path object)
        (failure-suffix object) (failure-suffix object)
        (failure-context object) (failure-context object)))
      (:constraint
       (format 
        stream 
        "#U[constraint ~:[-1~*~;~a~] [~:[~*~;[~{~a~^ ~}~]] ~
           ~:[top~*~;~a~] ~:[top~*~;~a~] ~:[top~*~;~a~] ~:[-1~*~;~a~]]"
        (failure-id object) (failure-id object)
        (failure-path object) (failure-path object)
        (failure-type1 object) (failure-type1 object) 
        (failure-type2 object) (failure-type2 object) 
        (failure-glb object) (failure-glb object)
        (failure-context object) (failure-context object))))))

(defun debug-yadu! (tdfs1 tdfs2 &optional path
                    &key (robustp *unify-robust-p*))
  #+:debug
  (setf %tdfs1 tdfs1 %tdfs2 tdfs2 %path path)
  (if *within-unification-context-p*
    (let* ((dag1 (tdfs-indef tdfs1))
           (tdfs2 (create-temp-parsing-tdfs tdfs2 path))
           (dag2 (tdfs-indef tdfs2))
           (*unify-wffs* t)
           (*expanding-types* nil)
           (*unify-robust-p* robustp)
           (result (debug-unify-dags dag1 dag2)))
      (when result (make-tdfs :indef result)))
    (with-unification-context (ignore)
      (debug-yadu! tdfs1 tdfs2 path :robustp robustp))))

(defun debug-unify-dags (dag1 dag2)
  #+:debug
  (setf %dag1 dag1 %dag2 dag2)
  (debug-unify1 dag1 dag2 nil) 
  (debug-copy-dag dag1))

(defun debug-unify1 (dag1 dag2 path)
  (setf dag1 (deref-dag dag1))
  (setf dag2 (deref-dag dag2))
  (when (consp (dag-copy dag1))
    (let* ((prefix (reverse (dag-copy dag1)))
           (suffix (subseq (reverse path) (length prefix)))
           (failure
            (make-failure :nature :cycle :path prefix :suffix suffix)))
      (push failure %failures%)))
  (unless (eq dag1 dag2) (debug-unify2 dag1 dag2 path))
  dag1)

(defun debug-unify2 (dag1 dag2 path)
  (multiple-value-bind (glb constraintp)
      (greatest-common-subtype (unify-get-type dag1) (unify-get-type dag2))
    (cond
     (glb
      (setf (dag-new-type dag1) glb)
      (when constraintp
        (let ((constraint (may-copy-constraint-of glb))
              failures)
          (let* ((%failures% nil))
            (debug-unify1 dag1 constraint path)
            (when %failures%
              (loop
                  with context = (make-failure 
                                  :nature :constraint
                                  :path (reverse path)
                                  :type1 (unify-get-type dag1) 
                                  :type2 (unify-get-type dag2)
                                  :glb glb)
                  with id = (failure-id context)
                  for failure in %failures%
                  do
                    (setf (failure-context failure) id)
                    (push failure failures)
                  finally (push context failures))))
          (nconc %failures% failures)))
      (setf dag1 (deref-dag dag1))
      (setf (dag-forward dag2) dag1)
      (setf (dag-copy dag1) path)
      (debug-unify-arcs dag1 dag2 path)
      (setf (dag-copy dag1) nil))
     (t
      ;;
      ;; to build a robust unifier, would it be sufficient to
      ;; - determine the most specific type subsuming both input types
      ;; - recurse over features only that are appropriate for that type
      ;; - discard any additional information from the two input structure
      ;; --- we wonder ...
      ;;
      (push (make-failure 
             :nature :type :path (reverse path)
             :type1 (unify-get-type dag1) :type2 (unify-get-type dag2))
            %failures%)
      (case *unify-robust-p*
        (:lub
         (let* ((type (least-common-supertype
                       (unify-get-type dag1) (unify-get-type dag2)))
                (features (appropriate-features-of type)))
           (declare (ignore features))
           (setf (dag-arcs dag1) nil)))
        (:size
         (let ((i (debug-unify-size dag1))
               (j (debug-unify-size dag2))
               k l)
           (when (= i j)
             ;;
             ;; if need be, use the count of subtypes as a tie-breaker, making
             ;; the (debatable) assumption that more subtypes indicate a larger
             ;; degree of uncertainty.
             ;;
             (let* ((type1 (get-type-entry (unify-get-type dag1)))
                    (type2 (get-type-entry (unify-get-type dag2))))
               (setf k (length (and type1 (ltype-descendants type1))))
               (setf l (length (and type2 (ltype-descendants type2))))))
           (cond
            ((or (< i j) (and k l (> k l)))
             (setf (dag-forward dag1) dag2)
             (debug-unify-arcs dag1 dag2 path)
             (setf (dag-copy dag2) nil))
            (t
             (setf (dag-forward dag2) dag1)
             (debug-unify-arcs dag1 dag2 path)
             (setf (dag-copy dag1) nil)))))
        (:default
         ;;
         ;; the cheapest possible strategy: arbitrarily select one of the two
         ;; input dags as the result dag; still recurse over all arcs to try
         ;; and pick up additional information (on shared arcs).
         ;;
         (setf (dag-forward dag2) dag1)
         (debug-unify-arcs dag1 dag2 path)
         (setf (dag-copy dag1) nil)))))))

(defun debug-unify-size (dag)
  (let ((n 0)
        seen)
    (labels ((traverse (dag)
               (unless (member dag seen :test #'eq)
                 (incf n)
                 (push dag seen)
                 (loop
                     for arc in (dag-arcs dag)
                     do (traverse (dag-arc-value arc)))
                 (loop
                     for arc in (dag-comp-arcs dag)
                     do (traverse (dag-arc-value arc))))))
      (traverse dag))
    n))

(defun debug-unify-arcs-find-arc (feature arcs comp-arcs)
  (or
   (loop
       for arc in arcs
       when (eq (dag-arc-attribute arc) feature) return arc)
   (loop
       for arc in comp-arcs
       when (eq (dag-arc-attribute arc) feature) return arc)))

(defun debug-unify-arcs (dag1 dag2 path)
  (let* ((arcs1 (dag-arcs dag1))
         (comp-arcs1 (dag-comp-arcs dag1))
         (arcs2 (dag-arcs dag2))
         (comp-arcs2 (dag-comp-arcs dag2))
         (new-arcs comp-arcs1))
    (loop
        for arc2 in arcs2
        for arc1 = (debug-unify-arcs-find-arc 
                    (dag-arc-attribute arc2) arcs1 comp-arcs1)
        when arc1 do
          (let ((path (cons (dag-arc-attribute arc1) path)))
            (declare (dynamic-extent path))
            (debug-unify1 
             (dag-arc-value arc1)
             (dag-arc-value arc2)
             path))
        else do
          (push arc2 new-arcs))
    (loop
        for arc2 in comp-arcs2
        for arc1 = (debug-unify-arcs-find-arc 
                    (dag-arc-attribute arc2) arcs1 comp-arcs1)
        when arc1 do
          (let ((path (cons (dag-arc-attribute arc1) path)))
            (declare (dynamic-extent path))
            (debug-unify1 
             (dag-arc-value arc1)
             (dag-arc-value arc2)
             path))
        else do
          (push arc2 new-arcs))
    (when new-arcs
      (setf (dag-comp-arcs dag1) new-arcs))))

(defun debug-copy-dag (dag &optional path &key (robustp *unify-robust-p*))
  (let ((*unify-robust-p* robustp)
        (dag (deref-dag dag))
        (copy (dag-copy dag)))
    (cond
     ((dag-p copy) copy)
     ((consp copy)
      ;;
      ;; _fix_me_
      ;; no need to do cycle detection here, we think.  debug-unify1() should
      ;; have done it already; at least i cannot quickly think of a way for a
      ;; cycle to escape that test ... why would we not be doing it inside the
      ;; unifier for production use?                            (2-aug-05; oe)
      ;;
      #+:null
      (let* ((prefix (reverse (first copy)))
             (suffix (subseq (reverse path) (length prefix)))
             (failure
              (make-failure :nature :cycle :path prefix :suffix suffix)))
        (push failure %failures%))
      (setf (dag-copy dag) (rest (dag-copy dag))))
     (t
      (let ((new (make-dag :type (unify-get-type dag))))
        (setf (dag-copy dag) (cons path new))
        (setf (dag-arcs new)
          (loop
              ;;
              ;; _fix_me_
              ;; the append() is lazy, wasteful cons()ing.     (24-sep-13; oe)
              ;;
              for arc in (append (dag-arcs dag) (dag-comp-arcs dag))
              for feature = (dag-arc-attribute arc)
              for copy = (let ((path (cons feature path)))
                           (declare (dynamic-extent path))
                           (debug-copy-dag (dag-arc-value arc) path))
              collect (make-dag-arc :attribute feature :value copy)))
        (setf (dag-copy dag) new))))))

;;;
;;;
;;;
(defparameter *token-ignore* nil)

(defun read-dag (stream &key path coreferences (ignore *token-ignore*))
  (when (stringp stream)
    (return-from read-dag
      (with-input-from-string (stream stream)
        (read-dag
         stream :path path :coreferences coreferences :ignore ignore))))
  (if (null coreferences)
    (let ((*package* (find-package *lkb-package*))
          (*readtable* (copy-readtable)))
      (set-syntax-from-char #\= #\( *readtable*)
      (ignore-errors
       (read-dag
        stream :coreferences (make-hash-table :test #'eql) :ignore ignore)))
    (let ((c (peek-char t stream nil nil))
          unifications)
      (when (and c (char= c #\#))
        (read-char stream)
        (let ((id (read stream nil nil)))
          (unless id
            (error
             "read-dag(): incomplete coreference label at `~{~a~^.~}'."
             (reverse path)))
          (push path (gethash id coreferences))
          (let ((c (peek-char t stream nil nil)))
            (if (and c (char= c #\=))
              (read-char stream)
              (return-from read-dag nil)))))
      (let* ((type (read stream nil nil))
             (c (peek-char t stream nil nil)))
        (when type
          (let ((path (create-path-from-feature-list (reverse path)))
                (value (make-u-value :type type)))
            (push (make-unification :lhs path :rhs value) unifications)))
        (when (and c (char= c #\[))
          (read-char stream)
          (loop
              for feature = (read stream nil nil)
              for value = (read-dag
                           stream :path (cons feature path)
                           :coreferences coreferences :ignore ignore)
              for c = (peek-char t stream nil nil)
              when (and value (not (smember feature ignore)))
              do (nconc unifications value)
              until (or (null c) (char= c #\]))
              finally (when (and c (char= c #\])) (read-char stream)))))
      (if (null path)
        (loop
            for paths being each hash-value in coreferences
            do
              (loop
                  with base = (first paths)
                  with lhs = (create-path-from-feature-list (reverse base))
                  for path in (rest paths)
                  for rhs = (create-path-from-feature-list (reverse path))
                  for unification = (make-unification :lhs lhs :rhs rhs)
                  do (push unification unifications))
            finally
              (let* ((*standard-output* (make-string-output-stream))
                     (dag (process-unifications unifications t t))
                     (tdfs (and dag (make-tdfs :indef dag))))
                (return tdfs)))
        unifications))))
