;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; Functions for taking an FOL expression and converting it to
;;; conjunction and implicative normal forms
;;;
;;; This basically follows Russell and Norvig, except as noted below
;;; Note that there is an option to cheat on Skolemization, because
;;; some theorem provers won't deal with Skolem functions

#|
1. replace implication by disjunction
2. move negation inwards so it only applies to atomic sentences, 
   flipping quantifiers etc to maintain validity
3. rename any spuriously duplicate variables 
   - ignored, since variables are distinct in the input   
4. move quantifiers left, maintaining order of quantifiers (this is now valid)
5. replace existential quantifiers by constants or functions (Skolemization, 
see below)
   - an option exists to replace by constants only, since Skolem
     functions aren't accepted by all theorem provers
6. drop universals (all remaining variables free, which is effectively 
like being universally quantified)
7. distribute `and' over `or' (i.e., we want to end up with a conjunction 
of disjunctions)
8. flatten nested conjuncts or disjuncts
   - actually merge this with prior step
9. convert to implicative form (optionally) by conjoining 
all negative literals on the left of the implication (removing negation) and 
all positive ones on the right.

|#

#|
Example 1

(convert-fol-to-inf
'(exists ?x (and (dog ?x) (own j ?x)))
)

Skolemization: (and (dog Sk1) (own j Sk1)) 

((=> true (dog Sk1))
 (=> true (own j Sk1)))

or

((dog Sk1)
 (own j Sk1))


Example 2

(convert-fol-to-inf
'(forall ?x (=> (exists ?y (and (dog ?y) (own ?x ?y)))
		    (animal-lover ?x)))
)


replace implication by disjunction:
(forall ?x (or (not (exists ?y (and (dog ?y) (own ?x ?y))))
	       (animal-lover ?x)))

move negation inwards:
  the quantifier flips from existential to universal:
  (forall ?x (or (forall ?y (not (and (dog ?y) (own ?x ?y))))
	       (animal-lover ?x)))
  and converts to or
  (forall ?x (or (forall ?y (or (not (dog ?y)) (not (own ?x ?y))))
	       (animal-lover ?x)))

move quantifiers out and drop universal quantifiers:
  (or (or (not (dog ?y)) (not (own ?x ?y))) (animal-lover ?x)))

flatten:
  (or (not (dog ?y)) (not (own ?x ?y)) (animal-lover ?x))

convert to implication:
  (=> (and (dog ?y) (own ?x ?y)) (animal-lover ?x))

Example 3

(convert-fol-to-inf
'(forall ?x (=> (animal-lover ?x) 
		    (forall ?y (=> (animal ?y) 
					(not (kill ?x ?y)))))))

replace implications by disjunctions:
(forall ?x (or (not (animal-lover ?x))
	       (forall ?y (or (not (animal ?y)) 
			      (not (kill ?x ?y))))))

move negation inwards, quantifiers left, Skolemize:
(or (not (animal-lover ?x))
    (or (not (animal ?y)) 
	(not (kill ?x ?y))))

(=> (and (animal-lover ?x) (animal ?y) (kill ?x ?y)) false)
     

Example 4

(convert-fol-to-inf 
 '(forall ?x (=> (cat ?x) (animal ?x)))
 )

replace implication: (forall ?x (or (not (cat ?x)) (animal ?x)))

drop universals: (or (not (cat ?x)) (animal ?x)))
convert to inf: (=> (cat ?x) (animal ?x))
\end{enumerate}

|#

#|

(convert-fol-to-inf 
 '(forall ?x (exists ?y (=> (cat ?x) (eats ?x ?y))))
 )

|#

(defun convert-fol-to-inf (fol-exp &key (proper-skolem t) (noisy t))
  (let* ((cnf (convert-fol-to-cnf fol-exp :proper-skolem proper-skolem
				  :noisy noisy))
	 (inf
	  (loop for conjunct in cnf
	      collect
		(convert-cnf-to-inf conjunct))))
    (when noisy
      (format t "~%inf ~A" inf))
    inf))

(defun convert-fol-to-cnf (fol-exp &key (proper-skolem t) (noisy t))
  (let* ((step1 (replace-fol-implications fol-exp))
	 (step2 (move-negation-in step1 nil))
	 (step3 (move-quantifiers-left step2))
	 (step4 (skolemize step3 proper-skolem nil))
         (step5 (drop-universals step4))
	 (step6 (distribute-and-over-or step5)))
    (when noisy
      (format t "~%Input ~A" fol-exp)
      (format t "~%replace-fol-implications ~A" step1)
      (format t "~%move-negation-in ~A" step2)
      (format t "~%move-quantifiers-left ~A" step3)
      (format t "~%skolemize (~Acheating) ~A" 
	      (if proper-skolem "not " "") step4)
      (format t "~%drop-universals ~A" step5)
      (format t "~%distribute-and-over-or ~A" step6))
    (if (conj-p step6)
	(rest step6)
      (list step6))))

(defun replace-fol-implications (expr)
  (if (consp expr)
      (if (eql (first expr) '=>)
	  (list 'or
		(list 'not (replace-fol-implications (second expr)))
		(replace-fol-implications (third expr)))
	(loop for el in expr
	    collect
	      (replace-fol-implications el)))
    expr))
    

#| 
(move-negation-in '(not (forall ?x P)) nil) 
   becomes (exists ?x (not P))
(move-negation-in '(not (exists ?x P)) nil) 
   becomes (forall ?x (not P))
(move-negation-in '(not (and P Q)) nil)
   becomes (or (not P) (not Q))
(move-negation-in '(not (or P Q)) nil)
   becomes (and (not P) (not Q))
(move-negation-in '(not (not P)) nil)
   becomes P
|#

(defun move-negation-in (expr neg-p)
  ;;; neg-p is t if we're currently in a negated context
  (cond ((forall-p expr) (list (if neg-p 'exists 'forall)
			       (second expr)
			       (move-negation-in (third expr) neg-p)))
	((exists-p expr) (list (if neg-p 'forall  'exists)
			       (second expr)
			       (move-negation-in (third expr) neg-p)))
	((conj-p expr) (list (if neg-p 'or 'and)
			     (move-negation-in (second expr) neg-p)
			     (move-negation-in (third expr) neg-p)))
	((disj-p expr) (list (if neg-p 'and 'or)
			     (move-negation-in (second expr) neg-p)
			     (move-negation-in (third expr) neg-p)))
        ((negated-p expr) (move-negation-in (second expr) (not neg-p)))
	(t (if neg-p 
	       (list 'not expr)
	       expr))))
  

(defvar *qstack* nil)


#|
(print
(move-quantifiers-left 
 '(forall ?y (and (exists ?x (not P))
		  (forall ?z (or (forall ?w (exists ?r (Q ?w ?r)))
                                 (and (not (Z ?z)) (not (W ?y)))))))))
|#

(defun move-quantifiers-left (expr)
  (setf *qstack* nil)
  ;;; qstack maintains a stack of quantifier variable
  ;;; pairs which are stripped off as we come to them
  ;;; then pushed back on the front at the end
  (let ((qfree (move-quantifiers-left-aux expr)))
    (replace-quantifiers *qstack* qfree)))
    
(defun move-quantifiers-left-aux (expr)  
  (cond ((not (consp expr)) expr)
        ((or (exists-p expr) (forall-p expr))
         (push (list (first expr) (second expr)) *qstack*)
	 (move-quantifiers-left-aux (third expr)))
	(t (loop for el in expr
	       collect
	       (move-quantifiers-left-aux el)))))

(defun replace-quantifiers (qstack expr)
  (if qstack (replace-quantifiers (rest qstack)
				  (append (first qstack)
					  (list expr)))
    expr))
  
(defun skolemize (expr proper-p univ-store)
  (cond ((exists-p expr)
	 (let* ((bv (second expr))
		(skolem (create-skolem bv univ-store proper-p)))
	   (skolemize (subst skolem bv (third expr))
		      ;; (subst new old tree) - subst is recursive
		      proper-p univ-store)))
	((forall-p expr)
	 (list (first expr) 
	       (second expr)
	       (skolemize (third expr) proper-p 
			  (cons (second expr) univ-store))))
	(t expr))) ;; don't need to recurse because all quantifiers
                   ;; must have been seen

(defun create-skolem (var univ-store proper-p)
  (if (and proper-p univ-store)
      (cons (intern (concatenate 'string "SKFN" (string var)))
            univ-store)
      (intern (concatenate 'string "SKCONST" (string var)))))

(defun drop-universals (expr)
  ;;; we don't need to recurse if it isn't a 
  ;;; universal because we know all quantifiers are at the front
  (if (forall-p expr)
      (drop-universals (third expr))
      expr))

#| 
(distribute-and-over-or '(or (and P Q) R))
(and (or P R)
     (or Q R))
(distribute-and-over-or '(or (and P Q) (and R S)))
(distribute-and-over-or '(or (and (or (and P Q) R) (or (and S T) (and Y Z))) W))
|#

;;; the following is a somewhat complicated fn
;;; and a little iffy, in that the binary `and' and `or'
;;; which we've relied on above, become n-ary here

(defun distribute-and-over-or (expr)
  (cond ((disj-p expr) 
	 (let ((subexpr1 (distribute-and-over-or (second expr)))
	       (subexpr2 (distribute-and-over-or (third expr))))
	   (if (conj-p subexpr1)
	       (if (conj-p subexpr2) ;; two conjuncts
		   (cons 'and
			 (loop for subconj1 in (rest subexpr1)
			     append
			       (loop for subconj2 in (rest subexpr2)
				   collect
				     (make-flat-disj subconj1 subconj2))))
		 ;; first conjunct, second literal or disjunct
		 (cons 'and
		       (loop for subconj in (rest subexpr1)
			   collect
			     (make-flat-disj subconj subexpr2))))
	     (if (conj-p subexpr2)
		 ;; second conjunct, first literal or disjunct
		 (cons 'and
		       (loop for subconj in (rest subexpr2)
			   collect
			     (make-flat-disj subexpr1 subconj)))
	       ;; neither is a conjunct, just need to flatten
	       (make-flat-disj subexpr1 subexpr2)))))
	((conj-p expr)
	 (cons 'and
	       (loop for subconj in (rest expr)
		   append
		     (let ((new-expr
			    (distribute-and-over-or subconj)))
		       (if (conj-p new-expr)
			   (rest new-expr) ;; flatten
			   (list new-expr))))))
	(t expr)))

(defun make-flat-disj (sub1 sub2)
  (cons 'or
	(append
	 (flatten-or sub1) 
	 (flatten-or sub2))))
		     
(defun flatten-or (subexpr)
  (if (disj-p subexpr)
      (rest subexpr) 
    (list subexpr)))

;;; Implicative normal form
;;; this is called on each element of a list of 
;;; flattened disjunctions
  ;;; (convert-cnf-to-inf '(or P (not Q) (not R) S)) 
  ;;; result
  ;;; (=> (and Q R) (or S P))
  ;;;
  ;;; (convert-cnf-to-inf '(or P Q)) 
  ;;; result 
  ;;; (or P Q)
  ;;;
  ;;; (convert-cnf-to-inf '(or (not P) (not Q)) )
  ;;; result
  ;;; (=> (and P Q) false)



(defun convert-cnf-to-inf (expr)
  (if (disj-p expr)
      (let ((left nil)
	    (right nil))
	(loop for disjunct in (rest expr)
	    do
	      (if (negated-p disjunct)
		  (push (second disjunct) left)
		(push disjunct right)))
	(setf left (cond ((null left) nil)
			 ((rest left) 
			  (cons 'and left))
		         (t (first left))))
	(setf right (cond ((null right) 'false)
			 ((rest right) 
			  (cons 'or right))
		         (t (first right))))
	(if left
	    (list '=> left right)
	  right))
    expr))

;;; Utility functions
;;;

(defun implication-p (expr)
  (and (consp expr)
       (eql (first expr) '=>)))

(defun negated-p (expr)
  (and (consp expr)
       (eql (first expr) 'not)))

(defun conj-p (expr)
  (and (consp expr)
       (eql (first expr) 'and)))

(defun disj-p (expr)
  (and (consp expr)
       (eql (first expr) 'or)))

(defun exists-p (expr)
  (and (consp expr)
       (eql (first expr) 'exists)))

(defun forall-p (expr)
  (and (consp expr)
       (eql (first expr) 'forall)))


