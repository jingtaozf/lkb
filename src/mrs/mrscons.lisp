;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; code for coping with h-cons information
;;;

(in-package "MRS")

(defvar *qeqs* nil)

(defstruct (qeq)
  left
  right)


(defun qeq-equal (x y) 
  (and (eql (qeq-left x) (qeq-left y))
       (eql (qeq-right x) (qeq-right y))))


;;; 



(defun process-hcons (hcons labels holes equated-list)
  (setf *qeqs* nil)
  (check-all-qeqs hcons labels holes)
  (let ((problems nil))
    (do* ((current (car *qeqs*) (car rest))
          (rest (cdr *qeqs*) (cdr rest)))
        ((null rest) nil)
      (let ((left (qeq-left current)) (right (qeq-right current)))
        (loop for qeq2 in rest
             do
             (cond ((eql (qeq-left qeq2) left)
                   (unless *giving-demo-p*
                     (format t
                             "~%WARNING: multiple qeqs with left member ~A ignored"
                             left))
                   (pushnew current problems)
                   (pushnew qeq2 problems))
                   ((eql (qeq-right qeq2) right)
                    (unless *giving-demo-p*
                      (format t
                              "~%WARNING: multiple qeqs with right member ~A ignored"
                              right))
                   (pushnew current problems)
                   (pushnew qeq2 problems))
                   (t nil)))))
    (setf *qeqs* (set-difference *qeqs* problems)))
  (define-qeq-chains holes *qeqs* equated-list))

(defun check-all-qeqs (hcons labels holes)
  (loop for constr in hcons
       do
       (let ((left (get-var-num (hcons-scarg constr)))
             (right (get-var-num (hcons-outscpd constr))))
         (if (eql left right)
             (unless *giving-demo-p*
               (format t
                       "~%WARNING:  qeq between identical handels ~A" 
                       left))
           (if (and (member left holes) (not (member left labels)))
               (if (and (member right labels) (not (member right holes)))
                   (pushnew (make-qeq :left left :right right)
                            *qeqs* 
                            :test #'qeq-equal)
                 (unless *giving-demo-p*
                   (format t
                           "~%WARNING:  in ~A qeq ~A, ~A is not a label - ignored" 
                           left right right)))
             (unless *giving-demo-p*
               (format t
                       "~%WARNING:  in ~A qeq ~A, ~A is not a hole - ignored" 
                       left right left)))))))

(defun satisfy-qeq-p (x y) 
  (dolist (qeq *qeqs*)
     (when
       (and (eql (qeq-left qeq) x)
            (eql (qeq-right qeq) y))
       (return t))))
    
            
;;;
;;; qeq constraints
;;;
;;; *qeqs* is a list of qeq structures - e.g.
;;; (#S(QEQ :H1 8 :H2 11)) 
;;; H1 qeq H2

(defvar *qeq-outscopes* nil)

(defun define-qeq-chains (holes qeqs equated-list)
;;  (format t "~%~A" *label-hole-pairs*)
  (setf *qeq-outscopes* nil)
  (loop for hole in holes
       do
       (discover-outscoped-labels hole qeqs equated-list)))

(defun discover-outscoped-labels (hole qeqs equated-list)
  (let ((existing (assoc hole *qeq-outscopes*)))
    (if existing (cdr existing)
      (let ((outscpd-list
             (append
              (if (member hole equated-list)
                  (loop for new-hole in
                       (cdr (assoc hole *label-hole-pairs*))
                       append
                       ;; DPF 21-Jul-99 - Added from WK
                       (unless (eql hole new-hole)
                        (discover-outscoped-labels 
                         new-hole qeqs equated-list))))
              (loop for qeq in qeqs
                   append
                   (if (eql (qeq-left qeq) hole)
                       (let* ((immediately-outscoped (qeq-right qeq))
                              (hole-handels 
                               (cdr (assoc immediately-outscoped 
                                           *label-hole-pairs*))))
                         (cons immediately-outscoped
                               (loop for hole-handel in hole-handels
                                    append
				    ;; DPF 21-Jul-99 - Added from WK
                                    (unless (eql hole hole-handel)
				      (discover-outscoped-labels 
				       hole-handel qeqs equated-list))))))))))
        (push (cons hole outscpd-list)
              *qeq-outscopes*)
        outscpd-list))))
                       
  


#|
a rel can be excluded from consideration if it is labelled with a
handel h1 such that h2 qeq h1 and the pending handel h3 neq h2.
The reasoning is that hole handels are necessarily distinct
(because we have a tree structure, not a DAG), so if
h2 qeq h1 and h3 qeq h4, then h2 neq h3 and h1 cannot fill h3.
A handel can only be qeq to one thing, since we never specify the
scopes of quantifiers.
|#

(defun find-qeq (top-handel bindings)
  (let* ((handels-to-check 
          (get-bindings-for-handel top-handel bindings))
         (res nil))
    (dolist (h handels-to-check)
      (when res 
        (return nil))
      (dolist (qeq *qeqs*)
        (when (eql h (qeq-left qeq))
          (setf res (qeq-right qeq))
          (return nil))))
    res))

(defun find-rel-qeq (rel)
  ;;; called in pre-processing, before bindings are relevant
  (let* ((h (get-var-num (rel-handel rel)))
         (qeqs (loop for qeq in *qeqs*
                    when (eql h (qeq-right qeq))
                    collect qeq)))
    (when (cdr qeqs)
        (error "Multiple qeqs - condition should have been checked for"))
    (car qeqs)))

(defun check-quant-qeqs (previous-holes pending-qeq quantifier bindings)
  ;;; we are trying to see whether a quantifier will go in the
  ;;; current location.  It won't if there is some h1 that
  ;;; qeq-outscopes h2 (i.e. a chain of qeqs)
  ;;; where we have found h1 on this branch, but haven't found h2
  ;;; where the relation labelled h2 contains a variable 
  ;;; bound by the quantifier and where h2 is NOT the
  ;;; pending qeq (i.e. we're off down a side branch).
  ;;; We don't need to check that h2 hasn't been found, 
  ;;; since we couldn't have h2 before the quantifier  
  (declare (ignore bindings))
;;;  (format t "~%check prev ~A" previous-holes)
  (let ((bound-rels (cdr (assoc quantifier *q-rel-store* :test #'eq)))
        (qeq-outscoped (loop for hole in previous-holes
                            append
                            (let ((outscpd-labels
                                   (cdr (assoc hole *qeq-outscopes*))))
                              (unless (and pending-qeq 
                                           (member pending-qeq outscpd-labels))
                                  outscpd-labels)))))
    (dolist (bound-rel bound-rels)
        (when
         (let ((brhandel (get-rel-handel-num bound-rel)))
           (member brhandel qeq-outscoped))
         (return t)))))
           

    
                  
(defun not-qeq-p (pending-qeq handel-to-check bindings)
  ;; pending-qeq may be nil, in which case there's a violation
  ;; if there's any qeq with handel-to-check
  (if (eql handel-to-check pending-qeq)
      nil
    (let ((handels-to-check (get-bindings-for-handel handel-to-check
                                                     bindings))
          (full-qeq-list
           (if pending-qeq
               (get-bindings-for-handel pending-qeq
                                        bindings))))
      (let ((violation nil))
        (dolist (h handels-to-check)
          (when violation (return nil))
          (dolist (qeq *qeqs*)
            (when (eql h (qeq-right qeq))
              (unless
                  (member (qeq-left qeq) full-qeq-list)
                (setf violation t)
                (return nil)))))
        violation))))


(defun incompatible-quantifiers (rel1 rel2)
  ;;; find quantifiers which cannot be sisters because
  ;;; they both scope the same relation or both scope
  ;;; something with the same handel
  (let ((q1rels (cdr (assoc rel1 *q-rel-store* :test #'eq)))
        (q2rels (cdr (assoc rel2 *q-rel-store* :test #'eq))))
    (dolist (q1rel q1rels)
       (when
         (let* ((args (get-handel-args q1rel))
                (outscoped1 (loop for h in args 
                                 append
                                 (cdr (assoc h *qeq-outscopes*)))))
         (dolist (q2rel q2rels)
            (when
              (or (eq q1rel q2rel)
                  (eql (get-rel-handel-num q1rel)
                       (get-rel-handel-num q2rel))
                  (member (get-rel-handel-num q2rel) 
                          outscoped1)
                  (member (get-rel-handel-num q1rel)
                          (let* ((args (get-handel-args q2rel)))
                            (loop for h in args 
                                 append
                                 (cdr (assoc h *qeq-outscopes*))))))
              (return t))))
         (return t)))))


(defun incompatible-q-and-scope (qrel screl)
  ;;; a quantifier cannot be sister to a scopal relation 
  ;;; if it is qeq something which the quantifier scopes over
  (let ((bound-rels (cdr (assoc qrel *q-rel-store* :test #'eq))))
    (dolist (h (get-handel-args screl))
       (when
         (let ((outscpd (cdr (assoc h *qeq-outscopes*))))
           (dolist (brel bound-rels)
               (when
                   (member (get-rel-handel-num brel) outscpd)
                 (return t))))
         (return t)))))
            
        
