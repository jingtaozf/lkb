;;; code for coping with h-cons information
;;;

(in-package "MRS")

(defvar *qeqs* nil)

(defstruct (qeq)
  left
  right)

;;; 

(defun process-hcons (hcons labels holes)
  (setf *qeqs* nil)
  (for constr in hcons
       do
       (let ((left (get-var-num (hcons-scarg constr)))
             (right (get-var-num (hcons-outscpd constr))))
         (if (eql left right)
             (unless *giving-demo-p*
                 (format t
                         "WARNING:  qeq between identical handels ~A" 
                         left))
           (if (and (member left holes) (not (member left labels)))
               (if (and (member right labels) (not (member right holes)))
                   (pushnew (make-qeq :left left :right right)
                            *qeqs* 
                            :test 
                            #'(lambda (x y) 
                                (and (eql (qeq-left x) (qeq-left y))
                                     (eql (qeq-right x) (qeq-right y)))))
                 (unless *giving-demo-p*
                   (format t
                           "WARNING:  in ~A qeq ~A, ~A is not a label - ignored" 
                           left right right)))
             (unless *giving-demo-p*
               (format t
                       "WARNING:  in ~A qeq ~A, ~A is not a hole - ignored" 
                       left right left))))))
  (let ((problems nil))
    (do* ((current (car *qeqs*) (car rest))
          (rest (cdr *qeqs*) (cdr rest)))
        ((null rest) nil)
      (let ((left (qeq-left current)) (right (qeq-right current)))
        (for qeq2 in rest
             do
             (cond ((eql (qeq-left qeq2) left)
                   (unless *giving-demo-p*
                     (format t
                             "WARNING: multiple qeqs with left member ~A ignored"
                             left))
                   (pushnew current problems)
                   (pushnew qeq2 problems))
                   ((eql (qeq-right qeq2) right)
                    (unless *giving-demo-p*
                      (format t
                              "WARNING: multiple qeqs with right member ~A ignored"
                              right))
                   (pushnew current problems)
                   (pushnew qeq2 problems))
                   (t nil)))))
    (setf *qeqs* (set-difference *qeqs* problems))))

            
;;;
;;; qeq constraints
;;;
;;; *qeqs* is a list of qeq structures - e.g.
;;; (#S(QEQ :H1 8 :H2 11)) 
;;; H1 qeq H2

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
         (qeqs
          (for h in handels-to-check
               append
               (for qeq in *qeqs*
                    filter
                    (if (eql h (qeq-left qeq))
                        (qeq-right qeq))))))
    (if (cdr qeqs)
        (error "Multiple qeqs - condition should have been checked for"))
      (car qeqs)))

    

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

(defun violates-outscopes-p  (handel-to-check current-top-handel
                              scoping-handels bindings)
  (declare (ignore handel-to-check current-top-handel
                   scoping-handels bindings))
  nil)
  #|
  ;;; true if handel-to-check is 
  ;;; outscoped by a handel which is not on the list
  ;;; of scoping-handels
  ;;; Only complication is that we have to check this wrt bindings
  (if (eql handel-to-check current-top-handel)
    nil
  (let ((handels-to-check (get-bindings-for-handel handel-to-check
                                                   bindings))
        (full-scoping-list (for handel in scoping-handels
                                append 
                                (get-bindings-for-handel handel
                                                   bindings)))
        (top-bindings (get-bindings-for-handel current-top-handel
                                                   bindings))
        (violation nil))
    (dolist (h handels-to-check)
      (when violation (return nil))
         (dolist (outs *outscopes*)
              (when (eql h (outscopes-h2 outs))
                 (unless
                     (or
                      (member (outscopes-h1 outs) full-scoping-list)
                      (member (outscopes-h1 outs) top-bindings))
                   (setf violation t)
                   (return nil)))))
    violation)))
|#


                       
    
    
