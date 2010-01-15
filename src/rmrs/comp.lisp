;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Code for RMRS composition
;;; Building semantic structures via an algebra

;;; FIX - not dealing with optional elements in rules
;;; FIX - share-var-info

(in-package :mrs)

;;; final output structure in basermrs.lisp

;;; ****************************************************
;;; Structures

(defstruct (semstruct (:include rmrs))
  features
  hook
  slots
  )

;;; hook has an indices structure as a value
;;;
;;; a full version of slots would be a list of indices, 
;;; tagged by some sort of name
;;; corresponding to the syntax
;;; however, for the robust semantic composition,
;;; we don't know anything about argument-hood lexically,
;;; and slots is generally unset, except (perhaps) for prepositions
;;; where the class may have known properties (this assumes the
;;; preposition/particle distinction is made)
;;;
;;; doing without slots completely doesn't work as soon as scopal 
;;; modification  (including negation) is treated properly,
;;; because the modifier `hides' the verb etc's ltop.
;;; But may be able to
;;; get away with an `anchor' - a single label - instead of the full thing


(defstruct indices
  index
  label
  default                   ;; don't want to output hooks 
                            ;; which have been created by defaul
  ;;; extarg
  )

;;; Rules

(defstruct rmrs-rule-set 
  name
  alternatives)
;;; alternatives are a list of rules (mostly singletons)

(defstruct rmrs-rule 
  name
  condition
  dtrs
  arity
  head
  semstruct
  eqs)

(defstruct equality
  ;;; just used in rules - eqs in rule is a list
  ;;; of these
  eq-els)

(defstruct pointer
  ;;; just used in rule equalities - a pointer points
  ;;; to a variable in a dtr
  dtrnum
  hook-el)

;;; Tag information

(defstruct rmrs-tag-template
  name
  semstruct)

;;; information from a parse tree
;;; constructing this will be formalism specific

(defstruct word-info
  lemma ;; downcase this systematically
  original ;; original string
  pos
  from
  to)

;;;; ***********************************
;;;; Utility functions

(defun find-var-type (str)
  ;;; this is only used in grammar/lextag rmrs files
  ;;; to abbreviate variables
  ;;; It should never be used when reading in an MRS/RMRS file
  (string-downcase (subseq str 0 1)))


(defun construct-grammar-var (var-string &optional extras)
  (make-grammar-var :type (find-var-type var-string)
                    :id var-string
		    :extra extras))

(defparameter *DEFAULT-TEMPLATE*
    (MAKE-RMRS-TAG-TEMPLATE
     :NAME "DEFAULT"
     :SEMSTRUCT
     (MAKE-SEMSTRUCT 
      :HOOK (make-indices :index (construct-grammar-var "U") 
                          :label (construct-grammar-var "H")) 
      :LISZT (LIST (MAKE-REL :pred "DUMMY-PRED"
                            :handel (construct-grammar-var "H")
                          :flist (LIST (construct-grammar-var "U")))))))

(defun dummy-pred-p (str)
  (equal str "DUMMY-PRED"))

(defun make-dummy-pred nil
  "DUMMY-PRED")

(defun dummy-constant-p (str)
  (equal str "DUMMY-CONSTANT"))

(defun make-dummy-constant nil
  "DUMMY-CONSTANT")

(defun make-default-hook nil
  ;;; while reading in
  (make-indices :index (construct-grammar-var "U") 
                :label (construct-grammar-var "H") :default t))


;;; ***************************
;;; Main entry point

(defparameter *rmrs-output-type* 'xml)

(defparameter *anchor-rmrs-p* t)
;;; if t, uses new style composition
;;; set to nil for old style
;;; FIX - to be removed once the new code is working
;;; and we can delete the old version

(defun construct-sem-for-tree (tree origin ostream &optional original)
  ;;; takes a tree and returns a semstruct - guaranteed
  ;;; - unless there's a syntax error in input data
  ;;;
  ;;; origin is a record of where the rmrs comes from 
  ;;; - currently this will always be :rasp
  ;;; original is the original string (in some form)
  (initialize-rmrs-variables)
  (let* ((semstruct
          (construct-sem-for-tree-aux tree original))
         (canonical-bindings 
          (close-bindings (semstruct-bindings semstruct)))
         (rmrs (make-rmrs
		   :top-h (indices-label (semstruct-hook semstruct))
                   :liszt (semstruct-liszt semstruct)
                   :rmrs-args (semstruct-rmrs-args semstruct)
                   :in-groups (semstruct-in-groups semstruct) 
                   :h-cons (semstruct-h-cons semstruct) 
		   :cfrom (semstruct-cfrom semstruct)
		   :cto (semstruct-cto semstruct)
		   :bindings canonical-bindings
		   :origin origin)))
    (canonicalise-rmrs rmrs) 
    ;; destructively modifies rmrs
    ;; so variables are replaced by the canonical variable
    (if (eql ostream :quiet)
        rmrs
      (output-rmrs1 rmrs
                    *rmrs-output-type* ostream))))

;;; **********************************************************
;;; Code for computing transitive closure of variable equalities
;;;
;;; the bindings are a list of binding structures,
;;; each of which relates a variable with a set of equivalents
;;; (as the bindings are being constructed), and ultimately
;;; with a canonical variable.  The key is the id

(defstruct binding 
  var
  id 
  equivs
  canonical)

(defun lookup-canonical-var (var binding-list)
  ;;; returns the canonical var
  (let* ((id (var-id var))
         (var-entry (find id binding-list :key #'binding-id)))
    (if var-entry
        (binding-canonical var-entry)
        var)))


;;; revised code to avoid ridiculously slow version.  This version could
;;; be further improved, but may not be the rate limiting step any more.
;;; Very occasional differences in results compared to prior version 
;;; current version appears correct.
;;; AAC Jan 2010

(defun close-bindings (bindings)
  (let ((id-bindings nil)
	  ;;; main work just done with ids to keep data structures smaller
	(canonical-bindings nil))
    (dolist (pair bindings)
      (dolist (el pair)
        (unless (member (var-id el) 
                        canonical-bindings
                        :key #'binding-id)
          (push (make-binding :var el :id (var-id el))
                 canonical-bindings))))
    (dolist (pair bindings)
      (when (cddr pair) 
            (error "Non binary binding ~A unexpected" pair))
      (let ((a (if (car pair) (var-id (car pair))))
	    (b (if (cadr pair) (var-id (cadr pair)))))
	(when (and a b (not (eql a b)))
	  (push (list a b) id-bindings))))
    (let* ((equivalence-sets-unsorted
	    (close-bindings-rec (list (car id-bindings)) (cdr id-bindings)))
	   (equivalence-sets-sorted
	    (loop for equiv-set in equivalence-sets-unsorted
		collect
		  (sort equiv-set #'<))))
      (dolist (binding canonical-bindings)
	(let* ((id (binding-id binding))
	       (equivs (dolist (equiv-set equivalence-sets-sorted)
			 (when (member id equiv-set)
			   (return equiv-set))))
	       (canon-id (car equivs)))
	  (setf (binding-equivs binding)
	    (remove id equivs))
	  (setf (binding-canonical binding)
	    (binding-var 
	     (find canon-id 
		   canonical-bindings 
		   :key #'binding-id)))))
      canonical-bindings)))



(defun close-bindings-rec (equivalence-sets to-do)
  (if to-do
      (let* ((next-pair (car to-do))
	     (a-member (find-equivalence-set (car next-pair) equivalence-sets))
	     (b-member (find-equivalence-set (cadr next-pair) equivalence-sets)))
	(cond ((and (not a-member) (not b-member))
	       (close-bindings-rec (cons next-pair equivalence-sets)
				   (cdr to-do)))
	      ((not b-member)
	       (close-bindings-rec
		(add-el-to-equivalence-sets (cadr next-pair) a-member 
					    equivalence-sets)
		(cdr to-do)))
	      ((not a-member)
	       (close-bindings-rec
		(add-el-to-equivalence-sets (car next-pair) b-member 
					    equivalence-sets)
		(cdr to-do)))
	      ((eql a-member b-member)
	       (close-bindings-rec equivalence-sets
				   (cdr to-do)))
	      (t (close-bindings-rec 
		  (merge-equivalence-sets a-member b-member
					  equivalence-sets)
		  (cdr to-do)))))
    equivalence-sets))

(defun find-equivalence-set (element equiv-sets)
  (dolist (eqset equiv-sets)
    (when (member element eqset)
      (return eqset))))

(defun add-el-to-equivalence-sets (element set-toupdate equivalence-sets)
  (push element (car (member set-toupdate equivalence-sets)))
  equivalence-sets)

(defun merge-equivalence-sets (a-member b-member equivalence-sets)
  (let ((reduced-sets (delete a-member (delete b-member equivalence-sets))))
    (push (nconc a-member b-member) 
	  reduced-sets)
    reduced-sets))



;;; end transitive closure code

;;; ******** Code to reset variables to canonical ids *********

(defun canonicalise-rmrs (rmrs)
  ;;; destructively modifies an rmrs by replacing the 
  ;;; bindings with the canonical binding
  ;;; Must be called after bindings have been closed
  (when (semstruct-p rmrs)
    (error "Not intended for semstructs"))
  (let ((top-h (rmrs-top-h rmrs))
        (eps (rmrs-liszt rmrs))
        (rmrs-args (rmrs-rmrs-args rmrs))
        (rmrs-h-cons (rmrs-h-cons rmrs))
        (rmrs-in-groups (rmrs-in-groups rmrs))
        (bindings (rmrs-bindings rmrs)))
    (when top-h
      (canonicalise-rmrs-variable top-h bindings))
    (loop for ep in eps
        do
        (canonicalise-rmrs-ep ep bindings))
    (loop for arg in rmrs-args
	do
	  (canonicalise-rmrs-arg arg bindings))
    (loop for ing in rmrs-in-groups
	do
	  (canonicalise-rmrs-in-group ing bindings))
    (loop for hcons in rmrs-h-cons
	do
	  (canonicalise-rmrs-hcons hcons bindings))
    (setf (rmrs-bindings rmrs) nil)
    rmrs))

(defun canonicalise-rmrs-variable (var bindings)
  (let ((id
         (find-rmrs-var-id var bindings)))
    (setf (var-id var) id)))

(defun canonicalise-rmrs-ep (ep bindings)
  (canonicalise-rmrs-variable (rel-handel ep) bindings)
  (when *anchor-rmrs-p*
    (canonicalise-rmrs-variable (rel-anchor ep) bindings))
  ;;; actually anchors should probably never change
  (let ((value (car (rel-flist ep))))
    (unless (var-p value) (error "Unexpected value ~A" value))
    (canonicalise-rmrs-variable value bindings)))

(defun canonicalise-rmrs-arg (arg bindings)
  (let ((label (rmrs-arg-label arg))
        (value (rmrs-arg-val arg)))
    (canonicalise-rmrs-variable label bindings)
    (if (var-p value)
        (canonicalise-rmrs-variable value bindings))))

(defun canonicalise-rmrs-in-group (ing bindings)
  (canonicalise-rmrs-variable
   (in-group-label-a ing)
   bindings)
  (canonicalise-rmrs-variable
   (in-group-label-b ing)
   bindings))

(defun canonicalise-rmrs-hcons (hcons bindings)
  (canonicalise-rmrs-variable
   (hcons-scarg hcons) bindings)
  (canonicalise-rmrs-variable
   (hcons-outscpd hcons) bindings))
  
        

;;; ******** Main composition code ************

;;; the code assumes a tree structure, where there's some
;;; notion of a node, which either contains a base tag/lexeme
;;; specification or a rule-name plus dtrs.  The functions
;;; daughter-nodes-p, get-rule-name, get-dtr-nodes, get-lexical-tag
;;; and get-lexeme are all defined for the particular format
;;; they all take a node.  The calling function is also
;;; format specific, because there may be other stuff in the file

;;; there's no particular reason why we need an overt rule name
;;; on the node - we just need some form of identification of
;;; a recipe for composition

(defun construct-sem-for-tree-aux (tree-node original)
  (if (daughter-nodes-p tree-node)
      (let* ((rule-name (get-rule-name tree-node))
	     (dtr-nodes (get-dtr-nodes tree-node))
	     (dtr-structures 
	      (loop for dtr in dtr-nodes
		     collect
		    (construct-sem-for-tree-aux dtr original))))
	(if *anchor-rmrs-p*
	    (algebra-compose rule-name dtr-structures)
	  (compose rule-name dtr-structures)))
    (let ((base-tag (get-lexical-tag tree-node))
	  (lexeme (get-lexeme tree-node original)))
      (if *anchor-rmrs-p*
	  (algebra-create-base-struct base-tag lexeme)
	(create-base-struct base-tag lexeme)))))


(defvar *local-var-context* nil)
;;; for equalities

(defvar *trace-rmrs-composition-p* nil)
;;; for debugging

(defun compose (rule-name dtrs)
  ;;; compose takes a rule name and a list of daughters
  ;;; the rule name is looked up
  ;;; this may give full instructions, or just mark the
  ;;; head.  If there's no instruction, default composition
  ;;; alone operates.
  (unless dtrs ;;; really there always ought to be dtrs
             ;;; but this prevents errors on trees with just a rule name
    (error "~% defective tree"))
  (let* ((dtr-features (loop for dtr in dtrs
			   when (semstruct-features dtr)
			   collect (semstruct-features dtr)))
	(rule-instruction (lookup-instruction rule-name dtr-features))
	(dtr-hooks (loop for dtr in dtrs
		       collect (semstruct-hook dtr)))
	(dtr-slots (loop for dtr in dtrs
		       collect (semstruct-slots dtr)))
	(dtr-eps (loop for dtr in dtrs
		     append (semstruct-liszt dtr)))
        (dtr-rargs (loop for dtr in dtrs
                       append (semstruct-rmrs-args dtr)))
        (dtr-ings (loop for dtr in dtrs
                      append (semstruct-in-groups dtr)))
        (dtr-hcons (loop for dtr in dtrs
                      append (semstruct-h-cons dtr)))
	(dtr-binding-list 
	 (loop for dtr in dtrs
	     append (semstruct-bindings dtr)))
	(cfrom (calculate-cfrom-from-daughters dtrs))
	(cto (calculate-cto-from-daughters dtrs))
	(semhead nil)
	(semstruct nil)
	(equalities nil))      	 
    (when (and rule-instruction
	       (eql (rmrs-rule-arity rule-instruction) 
		    (length dtrs)))
      ;;; fixed arity assumption may cause probs,
      ;;; but assume for now
      (setf semhead (rmrs-rule-head rule-instruction))
      (setf *local-var-context* nil)
      (setf semstruct 
	(when (rmrs-rule-semstruct rule-instruction)
	  (construct-new-semstruct
	   (rmrs-rule-semstruct rule-instruction)
	   cfrom
	   cto
	   nil)))
      ;;; if semstruct has been computed - this affects the
      ;;; equality computation via *local-var-context*
      (setf equalities 
	(compute-equalities dtr-hooks dtr-slots
			    (rmrs-rule-eqs rule-instruction))))
    (let ((semstruct-out
           (make-semstruct :hook 
                           (if semhead	; a number indicating the dtr
			       ; -1 means the rule itself is the head
			       (if (eql semhead -1)
				   (if semstruct
				       (semstruct-hook semstruct)
				     (make-default-running-hook))
				 (semstruct-hook 
				  (elt dtrs semhead)))
                             (if (not (cdr dtrs))
                                 (semstruct-hook (car dtrs))
                                ;;; assume semhead is single dtr 
                                ;;; if unary rule
                               (make-default-running-hook)))
			   :slots (cond 
				   (semstruct 
				    (semstruct-slots semstruct))
				   ((eql semhead -1)
				    :none)
				   (semhead 
				    (semstruct-slots 
				     (elt dtrs semhead)))
				   ((not (cdr dtrs))
				    (semstruct-slots (car dtrs)))
				   (t nil))
                           :liszt (if semstruct 
                                      (append 
                                       (semstruct-liszt semstruct) dtr-eps)
                                    dtr-eps)
                           :rmrs-args 
                           (if semstruct 
                               (append 
                                (semstruct-rmrs-args semstruct) dtr-rargs)
                             dtr-rargs)
                           :in-groups 
                           (if semstruct 
                               (append 
                                (semstruct-in-groups semstruct) dtr-ings)
                             dtr-ings)
                           :h-cons 
                           (if semstruct 
                               (append 
                                (semstruct-h-cons semstruct) dtr-hcons)
                             dtr-hcons)
                           :bindings 
                           (append equalities dtr-binding-list)
			   :cfrom cfrom
			   :cto cto)))
      (when *trace-rmrs-composition-p*
        (format t "~%Applying rule ~A" rule-name)
        (unless rule-instruction
          (format t " (not found)"))
        (dolist (dtr dtrs)  
          (internal-output-rmrs dtr 'vcompact t))
        (internal-output-rmrs semstruct-out 'vcompact t))
      semstruct-out)))

;;; cfrom and cto utility fns

(defun calculate-cfrom-from-daughters (dtrs)
  (let ((current-min most-positive-fixnum))
    (dolist (dtr dtrs)
      (let ((dtr-cfrom (semstruct-cfrom dtr)))
	(when (and dtr-cfrom
		   (< dtr-cfrom
		      current-min))
	  (setf current-min dtr-cfrom))))
    (if (eql current-min most-positive-fixnum)
	NIL
      current-min)))

(defun calculate-cto-from-daughters (dtrs)
  (let ((current-max -1))
    (dolist (dtr dtrs)
      (let ((dtr-cto (semstruct-cto dtr)))
	(when (and dtr-cto
		   (> dtr-cto
		      current-max))
	  (setf current-max dtr-cto))))
    (if (< current-max 0)
	NIL
      current-max)))

;;;
;;; Tag lookup
;;; 

(defun create-base-struct (tag lexeme)
  (let* ((tag-template (get-tag-template tag))
	 (tag-semstruct (rmrs-tag-template-semstruct
			 (or tag-template
			     *default-template*)))
	 (from (word-info-from lexeme))
	 (to (word-info-to lexeme)))
      (construct-new-semstruct 
       tag-semstruct
       from
       to
       lexeme)))

;;; A new semstruct may be created either for a lexical tag or for
;;; a semstruct contributed by a grammar rule.  In either case,
;;; a new set of variables has to be created, which are unique
;;; for this semstruct in this derivation, and a base binding list
;;; is also created

;;; In the case of tag lookup, the pred corresponding to
;;; the lexeme will be substituted.  

#|
 In the case of a rule, when a new semstruct is created,
 the variables also affect the equalities in the rule.
 For instance:

<rule>
<semstruct>
 <index> e 
 <ep> <realpred> p-agt <arg> e <arg> x2 </ep>
</semstruct>
<eq> VP.index e </eq>
<eq> NP.index x </eq>
</rule>

goes to

<rule>
<semstruct>
 <index> e101 
 <ep> <realpred> p-agt <arg> e101 <arg> x201 </ep>
</semstruct>
<eq> VP.index e101 </eq>
<eq> NP.index x101 </eq>
</rule>

|#

;;; ***********************************************
;;; Variable handling during composition 

(defvar *rmrs-variable-generator* nil)

(defun init-rmrs-variable-generator ()
  (setf *rmrs-variable-generator* (create-variable-generator)))

(defun initialize-rmrs-variables nil
  (if *restart-variable-generator*
      (init-rmrs-variable-generator)))

(defun initialize-rmrs-variables-plus nil
    (setf *rmrs-variable-generator* (create-variable-generator 10000)))

(init-rmrs-variable-generator)

(defun create-new-rmrs-var (type gen extras)
  ;;; constructs a new variable of a given type
  (let* ((idnumber (funcall gen)))
    (make-var 
     :type type
     :id idnumber
     :extra extras)))


(defun make-default-running-hook nil
  (make-indices :index (create-new-rmrs-var 
                        :other 
                        *rmrs-variable-generator* nil)
                :label (create-new-rmrs-var 
                        :handle 
                        *rmrs-variable-generator* nil)))


(defun generate-new-var (old-var-struct)
  ;;; called from construct-new-semstruct
  ;;; takes a var-struct with a dummy id 
  ;;; and creates a new variable of the same type
  (or (let  ((existing-var
	      (rest (assoc old-var-struct *local-var-context* :test #'eql-var-id))))
	(when (and existing-var (var-extra old-var-struct)
		   (not (var-extra existing-var)))
	  (setf (var-extra existing-var) (var-extra old-var-struct)))
	existing-var)
      (let* ((var-type (var-type old-var-struct))
             (varstruct (create-new-rmrs-var var-type 
					     *rmrs-variable-generator* 
					     (var-extra old-var-struct))))
	(push (cons old-var-struct varstruct) *local-var-context*)
	varstruct)))

;;; end variable handling code


(defun construct-new-semstruct (semstruct cfrom cto lex)
  ;;; this is only called when we have a structure
  ;;; corresponding to a read-in rule or tag
  ;;; *local-var-context* gets used when interpreting the eqs
  ;;; in the case of a rule
  ;;; lex should only be set in the case of a new tag
  ;;; when it will be a word-info structure
  (setf *local-var-context* nil)
  (let* ((new-hook (generate-new-hook (semstruct-hook semstruct))))
    (make-semstruct 
     :hook new-hook
     :slots (let ((slots-spec (semstruct-slots semstruct)))
	      (cond ((eql slots-spec :none) :none)
		    (slots-spec (generate-new-var slots-spec))
		    (t (indices-label new-hook))))
     :liszt
     (loop for old-ep in (semstruct-liszt semstruct)
         collect
           (make-rel :handel 
                     (if (rel-handel old-ep)
                         (generate-new-var (rel-handel old-ep))
                       (create-new-rmrs-var 
			:handle 
			*rmrs-variable-generator* nil))
                       :pred 
                       (let ((old-pred (rel-pred old-ep)))
                         (if (dummy-pred-p old-pred)
                             (make-realpred 
                              :lemma (string-downcase (word-info-lemma lex))
                              :pos (word-info-pos lex))
                           old-pred))
                       :flist
                       (loop for old-arg in (rel-flist old-ep)
                           collect
                             (generate-new-var old-arg))
                       :str (if (word-info-p lex)
                                (word-info-original lex))
		       :cfrom cfrom
		       :cto cto))
     :rmrs-args
     (loop for old-rarg in (semstruct-rmrs-args semstruct)
         collect
           (let ((val (rmrs-arg-val old-rarg)))
             (make-rmrs-arg 
              :arg-type (rmrs-arg-arg-type old-rarg)
              :label (generate-new-var (rmrs-arg-label old-rarg))
              :val (if (var-p val)
                       (generate-new-var val)
                     (if (dummy-constant-p val)
                         (string-downcase (word-info-lemma lex))
					; for constants in names etc
					; downcased, but may revisit
		                        ; this
		       val)))))
     :in-groups
     (loop for old-ing in (semstruct-in-groups semstruct)
         collect
           (make-in-group 
            :label-a
            (generate-new-var (in-group-label-a old-ing))
	    :label-b
            (generate-new-var (in-group-label-b old-ing))))
     :h-cons
     (loop for old-hcons in (semstruct-h-cons semstruct)
         collect
           (make-hcons
	    :relation (hcons-relation old-hcons)
            :scarg (generate-new-var (hcons-scarg old-hcons))
	    :outscpd (generate-new-var (hcons-outscpd old-hcons))))
     :cfrom cfrom
     :cto cto)))

(defun generate-new-hook (old-hook)
  (make-indices 
   :label (generate-new-var (indices-label old-hook))
   :index (generate-new-var (indices-index old-hook))))

(defun compute-equalities (dtr-hooks dtr-slots equalities)
  ;;; equality components in rules are either 
  ;;; integers plus path (indicating dtr-hook elements or dtr-slots ) or
  ;;; variables which should correspond to the *local-var-context*
  ;;;
  ;;; this function returns a list of lists of variables
  (let ((real-eqs nil))
    (dolist (equality equalities)
      (let ((els (equality-eq-els equality))
	    (new-els nil))
	(dolist (el els)
	  (let  ((new-var 
		  (if (pointer-p el)
		      (get-var-for-pointer el dtr-hooks dtr-slots)
		    (rest (assoc el *local-var-context* 
                                 :test #'eql-var-id)))))
	    (when new-var
	      (dolist (el1 new-els)
		(share-var-info new-var el1))
	      (push new-var new-els))))
	(when new-els
	  (push new-els real-eqs))))
    real-eqs))

(defun share-var-info (v1 v2)
  ;;; FIX
  ;;; eventually this should deal with the cases of compatible
  ;;; extra info, but for now, just assume only one variable has
  ;;; extra info
  ;;; should also deal with compatible sorts
  (if (var-extra v1)
      (setf (var-extra v2) (var-extra v1))
    (setf (var-extra v1) (var-extra v2))))

(defun get-var-for-pointer (pointer dtr-hooks dtr-slots)
  (let* ((dtr-num (pointer-dtrnum pointer))
	 (hook (elt dtr-hooks dtr-num))
	 (slot (elt dtr-slots dtr-num)))
    (cond ((and
	    (equal (pointer-hook-el pointer)
		   "INDEX")
	    hook (indices-p hook))
	   (indices-index hook))
	  ((and (equal (pointer-hook-el pointer)
		       "LABEL") 	       
		hook (indices-p hook))
	   (indices-label hook))
	  ((and (equal (pointer-hook-el pointer)
		       "ANCHOR") 	       
		slot (not (eql slot :none)))
	   slot)
	  (t nil))))

;;; ********************************
;;; Rule lookup

(defparameter *rule-instructions* nil)
;;; now a list of rule sets

(defun lookup-instruction (rule-name features)
  ;;; first check for an exact match
  ;;; failing this, check for a match ignoring the optional spec
  ;;; If this is found, dtrs may need adjusting
  (let* ((rule (rule-and-condition-match rule-name features 
					 *rule-instructions*)))
    (if rule
        (progn 
          (increment-rule-record rule-name nil t)
          rule)
      (let ((base-name (remove-optional-spec rule-name)))
        (if base-name
            (let ((mrule (rule-and-condition-match 
			  base-name features *rule-instructions*))
                  (opt-dtrs (if base-name (find-opt-dtrs rule-name))))
              (if mrule
                  (progn (increment-rule-record base-name opt-dtrs t)
                         (rule-with-adjusted-dtrs mrule opt-dtrs))
                (progn (increment-rule-record base-name opt-dtrs nil)
                       nil)))
          (progn (increment-rule-record rule-name nil nil)
                 nil))))))

(defun rule-and-condition-match (rule-name features rule-list)
  ;;; a conditional rule comes in two (or more) variants
  ;;; one may have an empty condition.  If we return
  ;;; multiple rules on the rule-name match, we check the conditions
  ;;; If features is empty, we match the unconditional rules
  ;;; If there are features, then we take the conditional version
  ;;; if they match the condition specification.  If no, we take
  ;;; the unconditional version
  (let ((rule-set (find rule-name rule-list
				 :test #'equal :key #'rmrs-rule-set-name)))
    (if rule-set
	(let ((rule-options 
	       (rmrs-rule-set-alternatives rule-set)))
	  (if (or (not (cdr rule-options)) (not features))
	      (car rule-options)
	    (or 
	     (find-if #'(lambda (rule)
			  (member (rmrs-rule-condition rule) features 
				  :test #'equal))
		      rule-options)
	   ;;; null features - default rule has null condition
	   ;;; and will match.  If there are
	   ;;; features then (first) matching rule
	     (car rule-options))))
              ;;; no feature that matches -> default
	      ;;; first thing on the rule options list is the default
      nil)))


(defun remove-optional-spec (rule-name)
  ;;; for rules of form N1/ap_n1/- return N1/ap_n1
  ;;; if no optional spec, return nil (we should have found it already)
  (let* ((slash-pos1 (position #\/ rule-name))
         (slash-pos2 (if slash-pos1
                         (position #\/ rule-name :start (+ 1 slash-pos1)))))
    (if slash-pos1
	(if slash-pos2
	    (subseq rule-name 0 slash-pos2)
	  (let ((postslash (subseq rule-name 0 slash-pos1)))
	    (if (every #'(lambda (x) 
			   (member x '(#\+ #\-))) 
		       (coerce postslash 'list))
		postslash))))))

(defun find-opt-dtrs (rule-name)
  ;;; given we've got something after the slash, return a list of ts and
  ;;; nils
  (let* ((slash-pos-end (position #\/ rule-name :from-end t))
         (opt-dtr-str 
          (subseq rule-name (+ 1 slash-pos-end))))
    (loop for char in (coerce opt-dtr-str 'list)
        collect
        (cond ((eql char #\+) t)  
              ((eql char #\-) nil)
              (t (error "Unexpected character in optional part of ~A"
                        rule-name))))))

(defun rule-with-adjusted-dtrs (rule opt-dtrs)
  ;;; opt-dtrs is a list with t and nil - we go through the
  ;;; OPT things is order, adjusting the rule so that it
  ;;; behaves as it would if just the OPTs that are actually present were 
  ;;; specified.  This involves renumbering the daughter pointers.
  ;;; e.g. if we have D1 OPT D2 OPT D3 OPT
  ;;; and - - +
  ;;; then we end up with D1 D2 D3 OPT (arity 4)
  ;;; and need to map dtr numbers 0->0, 1->?, 2->1, 3->?, 4->2, 5->3?
  ;;; though the ? shouldn't actually be used 
  ;;; OPT OPT D1 with -- should give 2->0
  ;;; Attempt to make this robust to screwups !
  (let ((new-rule (copy-rmrs-rule rule))
        (rule-dtrs (rmrs-rule-dtrs rule))
	(number-map nil)
	(real-count 0))
    (dotimes (n (length rule-dtrs))
      (push (cons n n) number-map))
    (setf number-map (nreverse number-map))
    (setf (rmrs-rule-dtrs new-rule)
      (loop for dtr in rule-dtrs and
	    mapping in number-map 
          nconc
            (if (not (member dtr '("OPT" "OPT*") :test #'string-equal)) 
                (progn 
		  (setf (cdr mapping) real-count)
		  (incf real-count) 
		  (list dtr))
	      (let ((next-opt (car opt-dtrs)))
		(setf (cdr mapping) nil)
		(setf opt-dtrs (cdr opt-dtrs))
		(if (null next-opt)
		    nil
		  (progn
		      (incf real-count) 		    
		      (list dtr)))))))
;;;    (pprint number-map)
    (setf (rmrs-rule-arity new-rule)
      (length (rmrs-rule-dtrs new-rule)))
    (unless (eql (rmrs-rule-head new-rule) -1)
      (setf (rmrs-rule-head new-rule)
	(cdr (assoc (rmrs-rule-head new-rule)
		    number-map))))
    (unless (rmrs-rule-head new-rule)
      (error "Head missing"))
    (setf (rmrs-rule-eqs new-rule)
      (loop for eq in (rmrs-rule-eqs new-rule)
	  collect
	    (make-equality 
	     :eq-els
	     (loop for eq-el in (equality-eq-els eq)
		 collect
		   (if (pointer-p eq-el)
		       (let ((new-dtr (cdr (assoc (pointer-dtrnum eq-el)
						  number-map))))
			 (unless new-dtr
			   (error "Optional daughter not optional"))
			 (make-pointer :dtrnum new-dtr
				      :hook-el (pointer-hook-el eq-el)))
		     eq-el)))))
    new-rule))

;;; ****************************************
;;; recording rule use  
;;;
;;; This is to aid grammar development, so that 
;;; the developer can tell which rules are most important
;;; for a given test suite
          
(defvar *known-rule-record* (make-hash-table :test #'equal))
(defvar *unknown-rule-record* (make-hash-table :test #'equal))

(defun clear-rule-record nil
  (setf *known-rule-record* (make-hash-table :test #'equal))
  (setf *unknown-rule-record* (make-hash-table :test #'equal)))

(defun show-rule-record (knownp)
  (maphash #'(lambda (key val)
               (format t "~%~A " key)
               (loop for rec in val
                   do
                     (let ((opt (car rec))
                           (count (cdr rec)))
                       (if opt
                           (progn 
                             (format t "~{~A~}" 
                                     (loop for el in opt
                                         collect 
                                           (if el "+" "-")))
                             (format t " ~A; " count))
                         (format t "/ ~A" count)))))
           (if knownp
               *known-rule-record*
             *unknown-rule-record*)))

(defun show-sorted-rule-record (knownp)
  (let ((rules nil))
    (maphash #'(lambda (key val)
		 (let ((max 0))
		   (loop for rec in val
		       do
			 (let ((count (cdr rec)))
			   (when (> count max) (setf max count))))
		   (push (list max key val) rules)))
	     (if knownp
		 *known-rule-record*
	       *unknown-rule-record*))
    (setf rules (sort rules #'> :key #'car))
    (dolist (rule rules)
      (let ((key (cadr rule))
	    (val (caddr rule)))
	(format t "~%~A " key)
	(loop for rec in val
	    do
	      (let ((opt (car rec))
		    (count (cdr rec)))
		(if opt
		    (progn 
		      (format t "~{~A~}" 
			      (loop for el in opt
				  collect 
				    (if el "+" "-")))
		      (format t " ~A; " count))
		  (format t "/ ~A" count))))))))

(defun increment-rule-record (rule-name opt-spec knownp)
  (let* ((rule-table (if knownp *known-rule-record*
                       *unknown-rule-record*))
         (record (gethash rule-name rule-table)))
    (if record
        (let ((opt-part
               (assoc opt-spec record :test #'equal)))
          (if opt-part 
              (incf (cdr opt-part))
            (push (cons opt-spec 1) 
                  (gethash rule-name rule-table))))
      (setf (gethash rule-name rule-table)
        (list (cons opt-spec 1))))))  
