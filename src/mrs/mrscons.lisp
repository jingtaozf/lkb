;;; code for coping with h-cons information
;;;

(in-package "MRS")

(defvar *is-one-ofs* nil)

(defstruct (possible-binding)
  h1
  hset)

(defvar *outscopes* nil)

(defstruct (outscopes)
  h1
  h2)

;;; 

(defun process-hcons (hcons labels holes)
  (setf *outscopes* nil)
  (let ((is-one-ofs nil)
        (left-intvars nil)
        (right-intvars nil))
  (for constr in hcons
       do
       (if (hcons-cands constr)
         (let ((left (get-var-num (hcons-scarg constr)))
               (candidates (mapcar #'get-var-num (hcons-cands constr))))
;;; WK: removed for VM: don't generate errors
;           (when (member left labels)
;             (cerror "Struggle on"
;              "Left member of is-one-of constraint ~A is a label"
;                    left))
           (unless (member left holes)
             (pushnew left left-intvars))
           (for candidate in candidates
                do
;               (when (member candidate holes)
;                 (error "Right element of is-one-of constraint ~A is a hole"
;                   candidate))
               (unless (member candidate labels)
                 (pushnew candidate right-intvars))) 
           (push (make-possible-binding :h1 left
                                        :hset 
                                        candidates)
                 is-one-ofs))
         (let ((left (get-var-num (hcons-scarg constr)))
               (right (get-var-num (hcons-outscpd constr))))
;; WK: removed for VM:
;;           (unless (and (member left labels)
;;                        (member right labels))
;;             (cerror "Struggle on"
;;              "Outscopes pair ~A > ~A are not both labels" left right))
           (push (make-outscopes :h1 left :h2 right)
               *outscopes*))))
;;  (for left-intvar in left-intvars
;;       do
;;       (unless (member left-intvar right-intvars)
;;          (cerror "Struggle on"
;;                  "~A is not a hole and does not have a solution set"
;;                 left-intvar)))
;;  (for right-intvar in right-intvars
;;       do
;;       (unless (member right-intvar left-intvars)
;;          (cerror "Struggle on"
;;           "~A is not a label and is not an intermediate variable"
;;                 right-intvar)))
  (setf *is-one-ofs* (substitute-int-vars is-one-ofs 
                                       left-intvars labels))
  ))


;;;
;;; is-one-of constraints


(defun substitute-int-vars (constraints left-intvars labels)
  (for constraint in constraints
       filter
       (unless (member (possible-binding-h1 constraint) left-intvars)
         (make-possible-binding :h1 (possible-binding-h1 constraint)
                    :hset 
                    (remove-duplicates 
                     (for right in (possible-binding-hset constraint)
                               append                                
                               (find-substitute-set constraints right
                                                    labels)))))))

 

(defun find-substitute-set (constraints var labels)
  (if (member var labels) (list var)
        (for constr in constraints
             append
             (if (eql var (possible-binding-h1 constr))
                 (for right in (possible-binding-hset constr)
                      append
                      (find-substitute-set constraints right labels))))))

(defun get-is-one-ofs (handel)
  (let ((constraint
         (find handel *is-one-ofs* :key #'possible-binding-h1)))
    (if constraint
      (possible-binding-hset constraint))))


;;;
;;; outscopes constraints
;;;
;;; *outscopes* is a list of outscopes structures - e.g.
;;; (#S(OUTSCOPES :H1 8 :H2 11)) 
;;; H1 outscopes H2

(defun violates-outscopes-p  (handel-to-check current-top-handel
                              scoping-handels bindings)
  ;;; true if handel-to-check is in an
  ;;; outscopes relationship with top-handel (since equality
  ;;; is prohibited or handel-to-check is 
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
                 (when (member (outscopes-h1 outs) top-bindings)
                   (setf violation t)
                   (return nil))
                 (unless
                    (member (outscopes-h1 outs) full-scoping-list)
                   (setf violation t)
                   (return nil)))))
    violation)))



                       
    
    
