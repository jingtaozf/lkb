;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; Functions for taking a FOL representation and converting
;;; it into an MRS representation
;;;

#|
1. remove any event variable quantifiers, leaving event variables unbound

2. convert existentials into an underspecified GQ representation (if quantifier
is unused, drop it). We don't know what should be in the restriction and what
should be in the scope of an existential GQ, since these are logically
equivalent.  We therefore leave this as underspecified, and let the grammar
sort it out.  Basically, this should normally work, because nominal type things
will have to go into the restriction and the rest of the stuff into scope.

Eventually we probably need constraints other than qeqs, to avoid getting
impossible sentences, but ignore this for now.

3. convert universals, by putting all not's in a disjunction in the restriction
(removing not) and all non-negated things in the body.

4. (OPTIONAL) add `the' for all constants that should be variables, for LinGO
style grammars

|#


;;; right now, just concentrate on the quantifier free fragment


(defparameter *const-arg-list* nil)

(defparameter *converted-args* nil)

(defparameter *fol-conversion-liszt* nil)

(defun convert-fol-top (sentence)
  (setf *const-arg-list* nil)
  (setf *converted-args* nil)
  (setf *fol-conversion-liszt* nil)
  (convert-fol-sentence sentence (create-variable-generator))
  (make-psoa :liszt *fol-conversion-liszt*))

(defun convert-fol-sentence (sentence gen)
  (cond ((conj-p sentence) 
         (convert-fol-sentence (second sentence) gen)
         (convert-fol-sentence (third sentence) gen))
        (t
         (push
          (convert-fol-predicate-formula sentence gen)
          *fol-conversion-liszt*))))
                               

(defun convert-fol-predicate-formula (predf gen)
  (let*  ((pred (convert-fol-pred-symbol (first predf)))
          (count 0)
          (flist (loop for arg in (rest predf)
                     collect
                     (progn 
                       (incf count)  
                       (convert-fol-argument arg pred count gen)))))
    (make-rel
     :pred pred
     :flist flist)))

(defun convert-fol-pred-symbol (symbol)
  (vsym (string-upcase (string symbol))))

(defun convert-fol-argument (arg sort pos gen)
  (let* ((feature (determine-mrs-feature sort pos))
         (val
           (if (member feature *value-feats*
                       :test #'eq)
               (convert-fol-const arg sort)
               (convert-fol-arg arg sort gen))))
      (make-fvpair :feature feature
                   :value val)))


(defun convert-fol-const (arg rel)
  ;;; this really had better be a constant
  ;;; (eventually, we might allow variables 
  ;;; which were eq to constants)
  (when (gq-var-p arg)
    (error "~%Constant expected in ~A but ~A found" rel arg))
  (string-upcase (string arg)))

(defun convert-fol-arg (arg rel gen)
  (declare (ignore rel))
  (or (cdr (assoc arg *converted-args*))
  ;;; if this isn't a variable, we can convert it to
  ;;; one - in some styles of grammar, we'd better add
  ;;; a quantifier (e.g. the) but ignore for now
      (let* ((id (funcall gen))
             (newvar (make-var :id (funcall gen)
                               :name (format nil "v~A"  id))))
             (when (gq-const-p arg)
               (pushnew arg *const-arg-list*))
             (push (cons arg newvar) *converted-args*)
             newvar)))

