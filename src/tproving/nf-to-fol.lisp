(in-package "MRS")

;;; Functions for conjunctive and implicative normal forms and 
;;; converting to more normal

#|
1. if implicative nf convert back to conjunctive nf
2. add back explicit conjunction
3. add back universals for free variables
4. replace skolem constants/functions with existentials

Obviously this won't lead to something which is the same as an original FOL
expression.  It may or may not lead to something expressible in natural
language according to a particular grammar.  It probably won't lead to
something that is particularly elegantly expressible ...

|# 

#|
Example 1

(convert-inf-to-fol
` ((dog Sk1)
   (own j Sk1)))

(and (dog Sk1) (own j Sk1)) 

(exists ?sk1 (and (dog ?sk1) (own j ?sk1)))

Example 2

(convert-inf-to-fol
  '((=> (and (dog ?y) (own ?x ?y)) (animal-lover ?x))))

(or (not (dog ?y)) (not (own ?x ?y)) (animal-lover ?x))

(forall ?x 
        (forall ?y 
            (or (not (dog ?y)) (not (own ?x ?y)) (animal-lover ?x))))

Example 3


(convert-inf-to-fol
 '((=> (and (animal-lover ?x) (animal ?y) (kill ?x ?y)) false)))

(or (not (animal-lover ?x)) (or (not (animal ?y)) (not (kill ?x ?y))))

(forall ?x (forall ?y 
                   (or (not (animal-lover ?x) 
                              (or (not (animal ?y)) (not (kill ?x ?y)))))))

Example 4

(convert-inf-to-fol
 '((=> (cat ?x) (animal ?x))))

(or (not (cat ?x)) (animal ?x)))
 
(forall ?x (or (not (cat ?x)) (animal ?x)))

Example 5

(convert-inf-to-fol
 '((=> (CAT ?X) (EATS ?X (SKFN?Y ?X)))))
 
(forall ?x (exists ?y (or (not (cat ?x)) (eats ?x ?y))))

|#

(defun convert-inf-to-fol (inf-list)
  (let* ((cnf-list
          (loop for inf in inf-list
              collect
                (convert-inf-to-cnf inf)))
         (anded-cnf-list
          (insert-binary-and cnf-list))
         (quantified
          (insert-universals-and-existentials anded-cnf-list)))
    quantified))

#|

(convert-inf-to-cnf '(own j Sk1))

(OWN J SK1)

(convert-inf-to-cnf
 '(=> (and (dog ?y) (own ?x ?y)) (animal-lover ?x)))

(OR (OR (NOT (DOG ?Y)) (NOT (OWN ?X ?Y))) (ANIMAL-LOVER ?X))

(convert-inf-to-cnf
 '(=> (and (animal-lover ?x) (animal ?y) (kill ?x ?y)) false))

(OR (NOT (ANIMAL-LOVER ?X)) (OR (NOT (ANIMAL ?Y)) (NOT (KILL ?X ?Y))))

(convert-inf-to-cnf
 '(=> (cat ?x) (animal ?x)))

(OR (NOT (CAT ?X)) (ANIMAL ?X))

|#

(defun convert-inf-to-cnf (expr)
  (if (implication-p expr)
      (let ((left (second expr))
            (right (third expr)))
        (if (eql left 'true)
            (convert-right-impl right)
          (if (eql right 'false)
              (convert-left-impl left)
            (list 'or
                  (convert-left-impl left)
                  (convert-right-impl right)))))
    (if (disj-p expr)
        (convert-to-binary-disjunction expr)
        expr)))
                    
(defun convert-right-impl (right)                    
  (if (disj-p right)
      (convert-to-binary-disjunction right)
    right))
                    
(defun convert-left-impl (left)
  (if (conj-p left)
      (convert-to-binary-disjunction
       (cons 'or
             (loop for left-el in (rest left)
                 collect (negate left-el))))
    (negate left)))

(defun negate (expr)
  (list 'not expr))

(defun convert-to-binary-disjunction (expr)
  ;;; takes a disjunction with n disjuncts and
  ;;; returns a binary expression
  (unless (disj-p expr)
    (error "~%~A is not a disjunction" expr))
  (let ((disjuncts (rest expr)))
    (recursive-binary-convert disjuncts 'or)))

(defun recursive-binary-convert (expr binary-op)
  (if (null (rest (rest expr)))
      (list binary-op (first expr) (second expr))
    (list binary-op (first expr)
          (recursive-binary-convert (rest expr) binary-op))))

(defun insert-binary-and (expr-list)
  (if (rest expr-list)
      (recursive-binary-convert expr-list 'and)
      (first expr-list)))

#|

to put back the quantifiers, we first collect two sets - one for universals,
one for existantials.  The universal set is simply a list of variables.
The existential set is a list of triples

constant new-var variables

variables will be nil for a skolem constant


|#

(defstruct sk-record
  const new vars)

(defparameter *universals* nil)

(defparameter *existentials* nil)

(defun insert-universals-and-existentials (expr)
  (setf *universals* nil)
  (setf *existentials* nil)
  (collect-variables expr)              ; sets the globals
  (let* ((s-ex (sort-existentials *existentials*))
        (s-univ (sort-universals *universals* s-ex nil)))
    (recursively-insert expr s-ex
                        s-univ)))

(defun collect-variables (expr)
  ;;; takes an expression with free variables and Skolem constants
  ;;; and functions and constructs lists of these
  (cond ((or (disj-p expr) (conj-p expr) (implication-p expr))
         (collect-variables (second expr))
         (collect-variables (third expr)))
        ((negated-p expr) (collect-variables (second expr)))
        (t (loop for arg in (rest expr)
               do
                 (cond ((universal-var-p arg) (pushnew arg *universals*))
                       ((skolem-fn-p arg)
                        ;; skfns are lists
                        (unless (member (first arg) *existentials*
                                        :test #'equal
                                        :key #'sk-record-const)
                          (push (make-sk-record 
                                 :const arg
                                 :new (create-new-var-from-sk (first arg))
                                 :vars (rest arg))
                                *existentials*)))
                       ((skolem-const-p arg)
                        (unless (member arg *existentials*
                                        :test #'equal
                                        :key #'sk-record-const)
                          (push (make-sk-record 
                                 :const arg
                                 :new (create-new-var-from-sk arg)
                                 :vars nil)
                                *existentials*)))
                       (t nil))))))

(defun create-new-var-from-sk (skconst)
  (intern (concatenate 'string "?" (string skconst))))
                            
(defun universal-var-p (arg)
  (and (not (consp arg))
       (char-equal (elt (string arg) 0) #\?)))

(defun skolem-const-p (arg)
  (and (not (consp arg))
       (char-equal (elt (string arg) 0) #\S)
       (char-equal (elt (string arg) 1) #\K)))

(defun skolem-fn-p (arg)
  (and (consp arg)
       (char-equal (elt (string (first arg)) 0) #\S)
       (char-equal (elt (string (first arg)) 1) #\K)))


(defun sort-existentials (extlist)
  (sort extlist 
        #'(lambda (a b)
            (> (length (sk-record-vars a))
               (length (sk-record-vars b))))))

(defun sort-universals (univlist extlist univ-so-far)
  (cond ((null extlist) (append univ-so-far univlist))
        ((null univlist) univ-so-far)
        (t (let ((ext-vars (sk-record-vars (first extlist)))
                 (already-seen nil))
             (dolist (univ-var univ-so-far)
               (if (member univ-var ext-vars)
                   (push univ-var already-seen)
                 (error "~%Invalid skolemization")))
             (let ((next-vars (set-difference ext-vars already-seen)))
               (sort-universals (set-difference univlist next-vars)
                                (rest extlist)
                              (append univ-so-far next-vars)))))))
                 


(defun recursively-insert (expr unused-existentials unused-universals)
  (cond ((null unused-existentials)
         (insert-universal-quantifiers expr unused-universals))
        ((null unused-universals)
         (insert-existential-quantifiers expr unused-existentials))
        (t (let ((top-universal (first unused-universals)))
             (if (member top-universal 
                         (sk-record-vars (first unused-existentials)))
                 (list 'forall top-universal
                       (recursively-insert expr unused-existentials 
                                           (rest unused-universals)))
               (let ((new-var (sk-record-new (first unused-existentials)))
                     (old (sk-record-const (first unused-existentials))))
                 (list 'exists new-var
                       (recursively-insert 
                        (subst new-var old expr) 
                        (rest unused-existentials) 
                        unused-universals))))))))

(defun insert-universal-quantifiers (expr vars)
  ;;; can only be called once we're left with all universals 
  (if vars
      (list 'forall
            (first vars)
            (insert-universal-quantifiers expr (rest vars)))
    expr))


(defun insert-existential-quantifiers (expr vars)
  ;;; can only be called once we're left with all existential
  (if vars
      (let ((new (sk-record-new (first vars))))
        (list 'exists
              new
              (insert-existential-quantifiers 
               (subst new (sk-record-const (first vars))
                      expr) 
               (rest vars))))
    expr))
  