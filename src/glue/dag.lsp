;;; Copyright (c) 1997--2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

;;;
;;; keep track of all conflicts among two structures; collect for interactive
;;; unification and display in new AVM browser.              (13-apr-04; oe)
;;;
(defparameter *failure-raw-output-p* nil)

(defparameter %failures% nil)

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (let ((n -1))
    (defun reset-failure-count () 
      (setf n -1))

    (defstruct failure
      (id (incf n))
      nature type1 type2 glb path suffix context)))

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
        "#U[cycle ~:[-1~*~;~a~] [~:[~*~;~{~a~^ ~}~] ~:[.~*~;~{~a~^.~}~] ~
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

(defun debug-yadu! (tdfs1 tdfs2 path)
  #+:debug
  (setf %tdfs1 tdfs1 %tdfs2 tdfs2 %path path)
  (with-unification-context (ignore)
    (let* ((dag1 (tdfs-indef tdfs1))
           (tdfs2 (create-temp-parsing-tdfs tdfs2 path))
           (dag2 (tdfs-indef tdfs2))
           (*unify-wffs* t)
           (*expanding-types* nil)
           (result (debug-unify-dags dag1 dag2)))
      (when result (make-tdfs :indef result)))))

(defun debug-unify-dags (dag1 dag2)
  #+:debug
  (setf %dag1 dag1 %dag2 dag2)
  (debug-unify1 dag1 dag2 nil) 
  (copy-dag dag1))

(defun debug-unify1 (dag1 dag2 path)
  (setf dag1 (deref-dag dag1))
  (setf dag2 (deref-dag dag2))
  (cond
   ((eq (dag-copy dag1) :inside)
    (push (make-failure :nature :cycle :suffix (reverse path)) %failures%))
   ((not (eq dag1 dag2)) (debug-unify2 dag1 dag2 path)))
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
      (setf (dag-copy dag1) :inside)
      (debug-unify-arcs dag1 dag2 path)
      (setf (dag-copy dag1) nil))
     (t
      (push (make-failure 
             :nature :type :path (reverse path)
             :type1 (unify-get-type dag1) :type2 (unify-get-type dag2))
            %failures%)))))

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
