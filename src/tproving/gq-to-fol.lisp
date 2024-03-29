;;; Copyright (c) 2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package "MRS")

;;; Not MRS specific at all
;;;
;;; Functions for taking a GQ representation and converting
;;; it into an approximately equivalent FOL representation
;;;
;;;


(defvar *gq-event-vars* nil)
;;; this is set to a list of event variables
;;; as a side effect of conversion.  The assumption
;;; is made that all event variables are unquantified
;;; in the GQ rep.  In the FOL rep, they are
;;; given widest scope existential quantifiers.
;;;
;;; 8/2006 - make `u' things also behave this way

(defun convert-gq-to-fol-top (gq-exp)
  ;;; top level call to these routines
  ;;; called by make-fol-approximation
  (setf *gq-event-vars* nil)
  (let ((main-exp (convert-gq-to-fol gq-exp)))
    (if main-exp
      (add-event-vars *gq-event-vars* main-exp))))

(defun add-event-vars (events main-exp)
  (if events
      (add-event-vars (rest events)
                      (cons 'exists 
                            (list (first events) main-exp)))
    main-exp))


#| 
Syntax

Prefixed form

FOL

qformula -> (quant var formula)

quant -> forall | exists

binary-formula -> (binary-op formula1 formula2)

binary-op -> and | or

unary-formula -> (unary-op formula)

unary-op -> not

predicate-formula -> (pred arg*)

arg -> ?varname | constant

GQ language is similar, but operators are unrestricted and
quantifiers are all of the following form:

(gq var f1 f2)

Mappings

gq = every, all

maps to

(forall var (or f2 (not f1)))

gq = some, a

maps to

(exists var (and f1 f2))

gq = the

maps to 

(and f1 f2) with var substituted by const-var

|#

#| 
(print
(convert-gq-to-fol 
    '(every ?x (cat ?x) 
      (some ?y (or (dog ?y) (rat ?y))
               (or (the ?z (mouse ?z) (and (like ?x ?z) (like ?y ?z)))
		(and (chase ?x ?y) (bite ?x ?y)))))))
|#

(defun convert-gq-to-fol (gq-exp)    
  (cond ((gq-quantifier-exp-p gq-exp) (convert-gq-quantifier gq-exp))
	((binary-exp-p gq-exp) (convert-gq-binary gq-exp))
	((unary-exp-p gq-exp) (convert-gq-unary gq-exp))
	((fol-pred-exp-p gq-exp) (convert-fol-pred gq-exp))
	(t nil)))
    

;;; general tests

(defun gq-var-p (x)
  (and (atom x)
       (char= (elt (string x) 0) #\?)))

(defun gq-const-p (x)
  (and (atom x)
       (not (gq-var-p x))))

(defun gq-event-var-p (x)
  (and (atom x)
       (and (char= (elt (string x) 0) #\?)
            (or (char-equal (elt (string x) 1) #\e)
		(char-equal (elt (string x) 1) #\u)))))
;;; char-equal matches both upper and lowercase

;;; quantifiers

(defun str-mem (el listy) 
  ;;; avoid package mess ups
  (member el listy 
	  :test #'(lambda (x y) (string-equal (string x) (string y)))))

(defun gq-quantifier-exp-p (exp) 
  (str-mem (first exp) 
	   '(every all some a an the _every_q _all_q _some_q 
	     _some_q_indiv _a_q _an_q _the_q)))

(defun convert-gq-quantifier (exp)
  (let ((gq (first exp))
	(bv (second exp))
	(restr (third exp))
        (body (fourth exp)))
  (cond ((str-mem gq '(every all _every_q _all_q))
	 (list 'forall
	       bv
	       (list 'or
		     (list 'not
			   (convert-gq-to-fol restr))
		     (convert-gq-to-fol body))))
	((str-mem gq '(some a an _some_q _some_q_indiv _a_q _an_q))
	 (list 'exists 
	       bv
	       (list 'and
		     (convert-gq-to-fol restr)
		     (convert-gq-to-fol body))))
	((str-mem gq '(the _the_q))
	 (let ((new-const (intern (concatenate 'string "CONST" (string bv)))))
	   (list 'and (convert-gq-to-fol 
		       (subst new-const bv restr))
		 ;; (subst new old tree) - subst is recursive
		 (convert-gq-to-fol 
		  (subst new-const bv body)))))
	(t (error "Unecognised gq ~A
	    gq-quantifier-exp-p and convert-gq-quantifier are out of sync"
		  gq)))))

;;; binary

(defun binary-exp-p (exp)
  (or (eql (first exp) 'and)
      (eql (first exp) 'or)))

(defun convert-gq-binary (exp)
  (list (first exp)
	(convert-gq-to-fol (second exp))
	(convert-gq-to-fol (third exp))))

;;; unary

(defun unary-exp-p (exp)
  (eql (first exp) 'not))

(defun convert-gq-unary (exp)
  (list (first exp)
	(convert-gq-to-fol (second exp))))

;;; pred

(defun fol-pred-exp-p (exp)
  (and exp 
       (listp exp)
       (atom (first exp))
       (every #'(lambda (x) (or (gq-var-p x)
				(gq-const-p x)))
	      (rest exp))))

(defun convert-fol-pred (exp)
  ;;; just called for side effect of pushing event variables
  ;;; onto *gq-event-vars*
  (loop for var in (rest exp)
      do
        (when (gq-event-var-p var)
          (pushnew var *gq-event-vars*)))
  exp)
