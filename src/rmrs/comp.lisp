;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; FIX - not dealing with optional elements in rules
;;; FIX - share-var-info

(in-package :mrs)

;;; final output structure

(defstruct (rmrs (:include basemrs))
  rmrs-args 
  in-groups
  bindings)

(defstruct rmrs-arg
  arg-type
  label
  val)

(defstruct in-group
  labels)

;;; the variables are replaced by canonical forms when 
;;; the structure is printed

(defstruct realpred
  lemma
  pos
  sense)


;;; Building semantic structures via an algebra

(defstruct (semstruct (:include rmrs))
  hook
;;;  holes
)

;;; hook has an indices structure as a value
;;;
;;; holes would be a list of indices, tagged by some sort of name
;;; corresponding to the syntax
;;; however, for the robust semantic composition,
;;; we don't know anything about argument-hood lexically,
;;; and holes is generally unset, except (perhaps) for prepositions
;;; where the class may have known properties (this assumes the
;;; preposition/particle distinction is made)
;;; for now, I'll try and do without holes
;;;


(defstruct indices
  index
  label
  default                   ;; don't want to output hooks 
                            ;; which have been created by defaul
  ;;; extarg
  )

;;; Rules

(defstruct rmrs-rule 
  name
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
  lemma ;; need to downcase
  pos
  from
  to)

;;;;

(defun construct-grammar-var (var-string &optional extras)
  (make-grammar-var :type (find-var-type var-string)
                    :id var-string
		    :extra extras))

(defparameter *rmrs-var-types*
    '((#\h . :handle)
      (#\e . :event)
      (#\x . :ref-ind)
      (#\u . :other)
      (#\H . :handle)
      (#\E . :event)
      (#\X . :ref-ind)
      (#\U . :other)
      (#\h . "HANDLE")
      (#\e . "EVENT")
      (#\x . "REF-IND")
      (#\u . "OTHER")))
      

(defun find-var-type (str)
  (let ((letter (elt str 0)))
    (or (cdr (assoc letter *rmrs-var-types*))
        :other)))

(defun find-var-letter (type)
  (or (car (rassoc type *rmrs-var-types* :test #'equal))
      (car (rassoc (string type) *rmrs-var-types* :test #'equal))
      #\u))


(defparameter *DEFAULT-TEMPLATE*
    (MAKE-RMRS-TAG-TEMPLATE
     :NAME "DEFAULT"
     :SEMSTRUCT
     (MAKE-SEMSTRUCT 
      :HOOK (make-indices :index (construct-grammar-var "U") 
                          :label (construct-grammar-var "H")) 
      :LISZT (LIST (MAKE-REL :sort "DUMMY-PRED"
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


(defparameter *rmrs-output-type* 'xml)

;;; Main entry point

(defun construct-sem-for-tree (tree &optional (ostream t))
  ;;; takes a tree and returns a semstruct - guaranteed
  ;;; - unless there's a syntax error in input data
  (initialize-rmrs-variables)
  (let* ((semstruct
          (construct-sem-for-tree-aux tree))
         (canonical-bindings 
          (close-bindings (semstruct-bindings semstruct))))
    (output-rmrs1 (make-rmrs
		   :top-h (indices-label (semstruct-hook semstruct))
                   :liszt (semstruct-liszt semstruct)
                   :rmrs-args (semstruct-rmrs-args semstruct)
                   :in-groups (semstruct-in-groups semstruct) 
                   :h-cons (semstruct-h-cons semstruct) 
                  :bindings canonical-bindings)
                 *rmrs-output-type* ostream)))

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
        
(defun close-bindings (bindings)
  ;;; the bindings are a list of pairs, linking ids
  ;;; this function effectively performs the transitive
  ;;; closure, resulting in a list where each id used
  ;;; is paired with its canonical var (could be the same)
  (let ((canonical-bindings nil))
    (dolist (pair bindings)
      (dolist (el pair)
        (unless (member (var-id el) 
                        canonical-bindings
                        :key #'binding-id)
          (push (make-binding :var el :id (var-id el))
                 canonical-bindings))))             
    (dolist (pair bindings)
      (when (and (car pair) (cadr pair))
        (let* ((var1-id (var-id (car pair)))
               (var2-id (var-id (cadr pair))))
          (when (cddr pair) 
            (error "Non binary binding ~A unexpected" pair))
          (augment-bindings var1-id var2-id
                            canonical-bindings nil)
          (augment-bindings var2-id var1-id
                            canonical-bindings nil))))
    (dolist (binding canonical-bindings)
      (setf (binding-canonical binding)
        (let* ((canon-id
               (apply #'min (cons (binding-id binding)
                                  (binding-equivs binding))))
               (canon-entry (find canon-id 
                                  canonical-bindings 
                                  :key #'binding-id)))
          (binding-var canon-entry))))
    ;; i.e., sort all the equivalents, including the original var
    canonical-bindings))
        
(defun augment-bindings (main-var second-var bindings done)
  (unless (or (eql main-var second-var) (member main-var done))
    (let ((main-var-entry (find main-var bindings :key #'binding-id)))
      (unless main-var-entry
        (error "Incorrectly set up bindings for ~A" main-var))
      (unless (member second-var (binding-equivs main-var-entry))
        (dolist (id (binding-equivs main-var-entry))
          (augment-bindings second-var id
                            bindings (cons main-var done))
          (augment-bindings id second-var 
                            bindings (cons main-var done)))
        (push second-var (binding-equivs main-var-entry))))))

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

(defun construct-sem-for-tree-aux (tree-node)
  (if (daughter-nodes-p tree-node)
      (let ((rule-name (get-rule-name tree-node))
	    (dtr-nodes (get-dtr-nodes tree-node)))
	(compose rule-name
		 (loop for dtr in dtr-nodes
		     collect
		       (construct-sem-for-tree-aux dtr))))
    (let ((base-tag (get-lexical-tag tree-node))
	  (lexeme (get-lexeme tree-node)))
      (create-base-struct base-tag lexeme))))


;;; composition

(defvar *local-var-context* nil)

(defvar *trace-rmrs-composition* nil)

(defun compose (rule-name dtrs)
  ;;; compose takes a rule name and a list of daughters
  ;;; the rule name is looked up in *rule-instructions*
  ;;; this may give full instructions, or just mark the
  ;;; head.  If there's no instruction, default composition
  ;;; alone operates.
  (unless dtrs ;;; really there always ought to be dtrs
             ;;; but this prevents errors on trees with just a rule name
    (error "~% defective tree"))
  (let ((rule-instruction (lookup-instruction rule-name))
	(dtr-hooks (loop for dtr in dtrs
		       collect (semstruct-hook dtr)))
	(dtr-eps (loop for dtr in dtrs
		     append (semstruct-liszt dtr)))
        (dtr-rargs (loop for dtr in dtrs
                       append (semstruct-rmrs-args dtr)))
        (dtr-ings (loop for dtr in dtrs
                      append (semstruct-in-groups dtr)))
        (dtr-hcons (loop for dtr in dtrs
                      append (semstruct-h-cons dtr)))
	(dtr-binding-list (loop for dtr in dtrs
		     append (semstruct-bindings dtr)))
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
	   (rmrs-rule-semstruct rule-instruction))))
      ;;; if semstruct has been computed - this affects the
      ;;; equality computation via *local-var-context*
      (setf equalities 
	(compute-equalities dtr-hooks
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
                           (append equalities dtr-binding-list))))
      (when *trace-rmrs-composition*
        (format t "~%Applying rule ~A" rule-name)
        (unless rule-instruction
          (format t " (not found)"))
        (dolist (dtr dtrs)  
          (internal-output-rmrs dtr 'vcompact t))
        (internal-output-rmrs semstruct-out 'vcompact t))
      semstruct-out)))

;;; Tag lookup
;;; 

(defun create-base-struct (tag lexeme)
  (let ((tag-template (get-tag-template tag)))
    (construct-new-semstruct
     (rmrs-tag-template-semstruct
       (or tag-template
	   *default-template*))
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

(defvar *rmrs-variable-generator* nil)

(defun init-rmrs-variable-generator ()
  (setf *rmrs-variable-generator* (create-variable-generator)))

(defun initialize-rmrs-variables nil
  (if *restart-variable-generator*
      (init-rmrs-variable-generator)))

(init-rmrs-variable-generator)

(defun create-new-rmrs-var (type gen extras)
  ;;; constructs a new variable of a given type
  (let* ((idnumber (funcall gen))
         (letter (find-var-letter type))
         (variable-name (format nil "~A~A" letter idnumber)))
    (if (eql type :handle)
        (make-handle-var 
         :name variable-name
         :type 'handle
         :id idnumber
         :extra extras)
    (make-var 
     :name variable-name
     :type type
     :id idnumber
     :extra extras))))

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
	      (rest (assoc old-var-struct *local-var-context* :test #'rmrs-var-equal))))
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


(defun construct-new-semstruct (semstruct &optional lex)
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
     :liszt
     (loop for old-ep in (semstruct-liszt semstruct)
         collect
           (make-char-rel :handel 
                     (if (rel-handel old-ep)
                         (generate-new-var (rel-handel old-ep))
                       (create-new-rmrs-var 
			:handle 
			*rmrs-variable-generator* nil))
                       :sort 
                       (let ((old-pred (rel-sort old-ep)))
                         (if (dummy-pred-p old-pred)
                             (make-realpred 
                              :lemma (string-downcase (word-info-lemma lex))
                              :pos (word-info-pos lex))
                           old-pred))
                       :flist
                       (loop for old-arg in (rel-flist old-ep)
                           collect
                             (generate-new-var old-arg))
		       :cfrom (if lex (word-info-from lex))
		       :cto (if lex (word-info-to lex))))
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
                         (word-info-lemma lex) ; for constants in names etc
                     val)))))
     :in-groups
     (loop for old-ing in (semstruct-in-groups semstruct)
         collect
           (make-in-group 
            :labels
            (loop for old-arg in (in-group-labels old-ing)
                collect
                  (generate-new-var old-arg))))
     :h-cons
     (loop for old-hcons in (semstruct-h-cons semstruct)
         collect
           (make-hcons
	    :relation (hcons-relation old-hcons)
            :scarg (generate-new-var (hcons-scarg old-hcons))
	    :outscpd (generate-new-var (hcons-outscpd old-hcons)))))))

(defun generate-new-hook (old-hook)
  (make-indices 
   :label (generate-new-var (indices-label old-hook))
   :index (generate-new-var (indices-index old-hook))))

(defun compute-equalities (dtr-hooks equalities)
  ;;; equality components in rules are either 
  ;;; integers (indicating dtr-hooks) or
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
		      (get-var-for-pointer el dtr-hooks)
		    (rest (assoc el *local-var-context* 
                                 :test #'rmrs-var-equal)))))
	    (when new-var
	      (dolist (el1 new-els)
		(share-var-info new-var el1))
	      (push new-var new-els))))
	(when new-els
	  (push new-els real-eqs))))
    real-eqs))

(defun rmrs-var-equal (v1 v2)
  (equal (var-id v1) (var-id v2)))

(defun share-var-info (v1 v2)
  ;;; FIX
  ;;; eventually this should deal with the cases of compatible
  ;;; extra info, but for now, just assume only one variable has
  ;;; extra info
  ;;; should also deal with compatible sorts
  (if (var-extra v1)
      (setf (var-extra v2) (var-extra v1))
    (setf (var-extra v1) (var-extra v2))))

(defun get-var-for-pointer (pointer dtr-hooks)
  (let* ((dtr-num (pointer-dtrnum pointer))
	 (hook (elt dtr-hooks dtr-num)))
    (if (and hook (indices-p hook))
	(cond ((equal (pointer-hook-el pointer)
		      "INDEX") 
	       (indices-index hook))
              ((equal (pointer-hook-el pointer)
		      "LABEL") 
	       (indices-label hook))
	      (t nil)))))

(defparameter *rule-instructions* nil)

;;; rule lookup

(defun lookup-instruction (rule-name)
  ;;; first check for an exact match
  ;;; failing this, check for a match ignoring the optional spec
  ;;; If this is found, dtrs may need adjusting
  (let ((rule (find rule-name *rule-instructions*
         :test #'equal :key #'rmrs-rule-name)))
    (if rule
        (progn 
          (increment-rule-record rule-name nil t)
          rule)
      (let ((base-name (remove-optional-spec rule-name)))
        (if base-name
            (let ((rule (find base-name *rule-instructions*
                              :test #'equal :key #'rmrs-rule-name))
                  (opt-dtrs (if base-name (find-opt-dtrs rule-name))))
              (if rule
                  (progn (increment-rule-record base-name opt-dtrs t)
                         (rule-with-adjusted-dtrs rule opt-dtrs))
                (progn (increment-rule-record base-name opt-dtrs nil)
                       nil)))
          (progn (increment-rule-record rule-name nil nil)
                 nil))))))

(defun remove-optional-spec (rule-name)
  ;;; for rules of form N1/ap_n1/- return N1/ap_n1
  ;;; if no optional spec, return nil (we should have found it already)
  (let* ((slash-pos1 (position #\/ rule-name))
         (slash-pos2 (if slash-pos1
                         (position #\/ rule-name :start (+ 1 slash-pos1)))))
    (if slash-pos2
        (subseq rule-name 0 slash-pos2))))

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
  (let ((new-rule (copy-rmrs-rule rule))
        (rule-dtrs (rmrs-rule-dtrs rule)))
    (setf (rmrs-rule-dtrs new-rule)
      (loop for dtr in rule-dtrs
          nconc
            (if (not (member dtr '(opt opt*))) 
                (list dtr)
                (let ((next-opt (car opt-dtrs)))
                   (setf opt-dtrs (cdr opt-dtrs))
                   (if (null next-opt)
                       nil
                     (list dtr))))))
    new-rule))
      
;;; recording rule use  
          
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
  
(defun test-rule-record nil
  (let ((foo *unknown-rule-record*))
    (setf *unknown-rule-record* (make-hash-table :test #'equal))
    (dotimes (n 10)
      (increment-rule-record "foobar" nil nil))
    (dotimes (n 5)
      (increment-rule-record "foobar" '(t nil) nil))
    (dotimes (n 22)
      (increment-rule-record "foobar" '(t nil t) nil))
    (show-rule-record nil)
    (setf *unknown-rule-record* foo)))
