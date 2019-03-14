;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see LICENSE for conditions


(in-package :lkb)

;;; Feb 1998 - removed fairly useless indexing scheme
;;;            currently stubs for a better one, which will have to be implemented by
;;;            defining get-indexed-lrules and get-indexed-rules sensibly

;;; April 1997 - YADUized 
;;;            - I/O stuff moved to new file 
;;;                    io-general/ruleinput.lsp

;;; Lexical rules and grammar rules

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;

;;; All rules are feature structures which are special in that they used
;;; differently from lexical entries, but they are constructed 
;;; by specifying types in exactly the same way
;;; Rule entries thus consist of an identifier plus a series of
;;; unifications etc.  
;;; All rules have a mother and one or more daughters 
;;; lexical rules are special only in that they may 
;;; have associated spelling changes (morphological rules)


(defvar *rules* (make-hash-table :test #'eq))

(defvar *lexical-rules* (make-hash-table :test #'eq))

(defvar *ordered-rule-list* nil)
(defvar *ordered-lrule-list* nil)
(defvar *ordered-sprule-list* nil)

(defun clear-grammar nil
  (setf *ordered-rule-list* nil)
  (when (fboundp 'clear-generator-grules)
    (funcall (symbol-function 'clear-generator-grules)))
  (clrhash *rules*))

(defun clear-lex-rules nil
  (setf *ordered-lrule-list* nil)
  (setf *ordered-sprule-list* nil)
  (when (fboundp 'reset-cached-lex-entries)
    (funcall (symbol-function 'reset-cached-lex-entries)))
  (when (fboundp 'clear-generator-lrules)
    (funcall (symbol-function 'clear-generator-lrules)))
  (clrhash *lexical-rules*))

(defstruct (rule (:include psort))
  ;;; NOTE - any changes to slots here have to be mirrored
  ;;; in mrs/lexlookup.lsp make-new-found-rule
  rtdfs ;; restricted feature structure for improved packing
  daughters-restricted
  daughters-restricted-reversed
  daughters-apply-order
  order
;;; order is an ordered list of the paths that get at mother and daughters
;;; e.g. 0 1 2
;;; nil (ARGS HD) (ARGS TL HD)
  rhs ;; list of indices into `order' --- for key-driven parsing
  daughters-order-reversed
  apply-filter ;;; rule filter
  apply-index ;;; rule filter
  feeders ;;; slot used in construction of lrfsm
  allfed ;;; slot used in construction of lrfsm for spelling and nosp
  lrfsm	;;; generalisation of filter for lexical rules
  nospfeeders
  nospfsm ;;; yet another, with only non-spelling rules
  head
  orthographemicp)
  
(defmethod print-object ((inst rule) stream)
  (format stream "#[rule ~S ~:[~;o~]]" 
	  (rule-id inst)
	  (rule-orthographemicp inst)
	  ))

;;; Accessing

(defun get-lex-rule-entry (name)
   (gethash name *lexical-rules*))

(defun get-grammar-rule-entry (name)
   (gethash name *rules*))

(defun get-indexed-lrules (tdfs &optional test-fn)
  ;; don't return any rules with satisfy test-fn (if specified)
  (declare (ignore tdfs))
  (get-rules-internal test-fn *lexical-rules*))

(defun get-indexed-rules (tdfs &optional test-fn)
  ;; don't return any rules with satisfy test-fn (if specified)
  (declare (ignore tdfs))
  (get-rules-internal test-fn *rules*))

(defun get-rules-internal (test-fn rule-set)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (unless
                    (or (redundancy-rule-p value)
                        (and test-fn (funcall test-fn value))
                        (and *current-language*
                             (not (eq *current-language* 
                                      (rule-language value)))))
                    (push value result)))
           rule-set)
    result))


(defun greater-than-binary-p nil
  (maphash #'(lambda (k v) 
               (declare (ignore k))
               (if (> (length (rule-order v)) 3) 
                   (return-from greater-than-binary-p t)))
           *rules*))

(defun lexical-rule-p (x)
  (when (and (rule-p x) 
	     (get-lex-rule-entry (rule-id x)))
    t))

;;; **************************************************
;;;
;;; Application of lexical rules
;;;
;;; **************************************************

(defparameter *number-of-applications* 0)

(defun try-all-lexical-rules (entries &optional ignore-list)
  ;;; this is now just called interactively 
  ;;; and by output-lex-and-derived, but not by the parser
  ;;; entries are pairs with list of rules applied plus result
  ;;; With a large grammar, this can easily go beserk
  ;;; and the test for the standard *maximal-lex-rule-applications* is not
  ;;; constraining enough.  I have therefore changed the interface
  ;;; so this is called with *maximal-lex-rule-applications* let
  ;;; to 1 so it only applies once.  The interface then allows
  ;;; the user to investigate further applications on particular
  ;;; results.
   (incf *number-of-applications*)
   (unless (> *number-of-applications* *maximal-lex-rule-applications*)
     (let ((transformed-entries 
            (loop for entry in entries
		append
		  (loop for rule in 
			(get-indexed-lrules 
			 (cdr entry)
			 #'(lambda (rule) (member (rule-id rule) ignore-list)))
		      nconc
			(let* ((spelling-rule-p 
				(and 
				 (spelling-change-rule-p rule)
				 ;; (bmw) fix_me
				 (in-morph-rule-set-p rule)
				 ))
			       (new-morph 
				(if spelling-rule-p
				    (construct-new-morph entry rule)))
			       (result
				(if (or (not spelling-rule-p) new-morph)
					; allow morphographemics to block generation
				    (evaluate-unifications rule
							   (list (cdr entry))
							   new-morph))))
			  (if result 
			      (list
			       (cons 
				(cons (rule-id rule) (car entry))
				result))))))))
       (if transformed-entries
	   (append transformed-entries
		   (try-all-lexical-rules transformed-entries ignore-list))))))


(defun construct-new-morph (entry rule)
#+:debug  (format t "~%construct-new-morph: ~A ~A" 
		   (extract-orth-from-fs 
		    (cdr entry)) (rule-id rule))
  (let ((new-morph
         (full-morph-generate 
          (extract-orth-from-fs 
           (cdr entry))
          (rule-id rule))))
    (if new-morph
        (car (mapcar #'car new-morph)))))
    
;;; **************************************************
;;;
;;; Accessing rules for parsing
;;;
;;; **************************************************



(defun get-matching-rules (rhd-fs &optional no-unary)
  #+:arboretum
  (declare (special *mal-active-p*))
  ;;; the test which stops the parser applying a rule
  ;;; with orthographic effects is now
  ;;; spelling-change-rule-p which is defined in the
  ;;; globals file
  ;;; AAC Feb 1996
  (let ((all (if no-unary
               (get-indexed-rules
                rhd-fs
                #'(lambda (rule) (<= (length (rule-order rule)) 2)))
               (union (get-indexed-lrules rhd-fs #'spelling-change-rule-p)
                      (get-indexed-rules rhd-fs #'spelling-change-rule-p)
                      :test #'eq))))
    ;;
    ;; _fix_me_
    ;; incorporate support for grammar checking application, using an
    ;; additional set of `mal' rules that detect and correct certain error
    ;; types, hence want to be available in parsing at times but probably never
    ;; in generation.  this would seem to call for a better generalization,
    ;; allowing grammars to tag rules according to various contexts for use,
    ;; e.g. in a phased parsing set-up, if we were to move that direction.
    ;;                                                    (23-apr-04; erb & oe)
    #+:arboretum
    (loop
        for rule in all 
        for mal-rule-p = (mal-rule-p rule)
        when (or (null mal-rule-p) *mal-active-p*)
        collect rule)
    #-:arboretum
    all))

(defun get-matching-lex-rules (rhd-fs)
  (get-indexed-lrules rhd-fs #'spelling-change-rule-p))

(defun apply-lex-interactive (lex lex-entry-fs lex-rule)
  ;;; called by apply-lex
  (declare (ignore lex))
  (if 
      ;; modification to check whether a particular 
      ;; lexical rule is morphological - if so, then the 
      ;; unification function is called with an extra 
      ;; option value which describes the new 
      ;; orthography of the result.
      (spelling-change-rule-p lex-rule)
      ;; need to reimplement evaluate-unifications-with-fail-messages
      (evaluate-unifications 
       lex-rule 
       (list lex-entry-fs) 
       (car (mapcar #'car 
                    (full-morph-generate 
                     (extract-orth-from-fs lex-entry-fs)
                     (rule-id lex-rule)))))
    (evaluate-unifications lex-rule
                           (list lex-entry-fs))))


#|
see the wish list on the wiki

given a comment in a type of the form

grammar-type := parent &
"composite: base + rule ; human readable blah"
etc

we want to extract the comments and then check the grammar-type constraint
to make sure it's equivalent to what we'd have generated from the 
base fs plus rule (except the type itself, of course)

(defun check-composite-type (base rule grammar-type)
  (let* ((base-fs (make-wellformed (make-dag :type base)))
	 (result (evaluate-unifications rule (list base-fs))))
    (if result
	(or 
	 (equal-feature-structures result (ltype-constraint gramar-type))
	 (complain))
      (complain))))
 |#
  
;;; *************************************************************
;;;
;;; adding rules - function called from tdlruleinput and ruleinput
;;;
;;; *************************************************************

(defun add-grammar-rule (id non-def def rule-persistence
                         lexical-p &optional orthographemicp)
  (let ((entry (make-rule :id id :orthographemicp orthographemicp)))
    (setf (rule-unifs entry) non-def)
    (setf (rule-def-unifs entry) def)
    (when (gethash id (if lexical-p *lexical-rules* *rules*))
      (format t "~%WARNING: Rule `~A' redefined." id))
    (expand-rule id entry non-def def rule-persistence lexical-p)))

;;; expanding rules - also called from type redefinition functions

(defparameter *c-cont-check-path* nil)
      ;;; (setf lkb::*c-cont-check-path* '(C-CONT))

(defun expand-rule (id rule non-def def rule-persistence lexical-p)
  (process-unif-list id non-def def rule rule-persistence)
  (let ((fs (rule-full-fs rule)))  
    (when fs
      (when *c-cont-check-path*
	(sanitize (existing-dag-at-end-of (tdfs-indef fs) 
					  *c-cont-check-path*)
		  id *c-cont-check-path* t))
      (setf (rule-rtdfs rule) (copy-tdfs-partially fs))
      (if lexical-p 
	  (progn
	    (when (eql *morph-option* :distinct-mphon)
	      (extract-rule-affixation-type fs id))
	    ;;; in morph.lsp
	    ;;; looks for path (by default AFFIXATION)
	    ;;; that has as its value a morphophon type
	    ;;; and adds it to the appropriate morphonphon rule
	    (pushnew id *ordered-lrule-list*)
	    (when (spelling-change-rule-p rule)
	      ;;; if this has the default definition
	      ;;; it relies on the morphophon stuff having already
	      ;;; been processed at this point
	      (pushnew id *ordered-sprule-list*)))
        (pushnew id *ordered-rule-list*))
      (let ((f-list
             (mapcar #'(lambda (x) (if (listp x) x (list x)))
                     (establish-linear-precedence (tdfs-indef fs)))))
        (setf (rule-order rule) f-list)
        ;; note that generator requires all slots related to rule-order
        ;; to contain paths that are eq when they are equal
        (setf (rule-daughters-order-reversed rule) (reverse (cdr f-list)))
        (setf (rule-daughters-restricted rule)
          (mapcar
           #'(lambda (path)
               (restrict-fs
                (existing-dag-at-end-of (tdfs-indef fs) path)))
           (cdr f-list)))
        (setf (rule-daughters-restricted-reversed rule)
          (reverse (rule-daughters-restricted rule)))
        (let ((key-path
               (some
                #'(lambda (path)
                    (let ((dag
                           (existing-dag-at-end-of 
                            (tdfs-indef fs)
                            (append path *key-daughter-path*))))
                      (and dag
                           (bool-value-true dag)
                           path)))
                (cdr f-list))))
          ;; if there is a key daughter, remaining daughters ordered to be
          ;; processed r->l before key-path, then l->r after - generator
          ;; assumes this
          (setf (rule-daughters-apply-order rule)
            (if key-path
                (let ((tail (member key-path (cdr f-list) :test #'eq)))
                  (cons key-path
                        (nconc (nreverse (ldiff (cdr f-list) tail)) 
                               (cdr tail))))
              (cdr f-list))))
        ;;
        ;; make several attempts to identify the (linguistic) head daughter
        ;; (for use in Redwoods tree lexicalization, but potentially useful for
        ;; other processing aspects).                         (29-nov-02; oe)
        ;;
        (let* ((daughters (rest (rule-order rule)))
               (dag (tdfs-indef fs))
               (daughter (existing-dag-at-end-of dag *head-daughter-path*))
               (head (existing-dag-at-end-of dag *head-path*)))
          (setf (rule-head rule)
            (or (when head
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      for bar = (when foo
                                  (existing-dag-at-end-of foo *head-path*))
                      when (eq bar head) return i))
                (when daughter
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      when (eq foo daughter) return i))
                0)))
        ;;
        ;; compute list of indices into `order' slot; these are used in
        ;; key-driven parsing (using the new (hyper-)active parser); there
        ;; seems to be some overlap with `daughters-apply-order' used in
        ;; the generator.  however, the parser really needs the numerical
        ;; encoding to decide wheter an edge wants to extend forwards or
        ;; backwards.  brief inspection of the generation code suggests
        ;; that `daughters-apply-order' currently is only used to compute
        ;; a numerical index of the (linguistic) head daughter.  --- so, 
        ;; maybe the two mechanisms could be joined.     (19-jul-99  -  oe)
        ;;
        #-:head-first
        (let* ((daughters (rest (rule-order rule)))
               (arity (length daughters))
               (dag (tdfs-indef fs))
               (key (or
                     (let ((key (rest (assoc id *rule-keys*))))
                       (and (integerp key) (<= key arity) (- key 1)))
                     (loop
                         for path in daughters
                         for i from 0
                         for daughter = (existing-dag-at-end-of dag path)
                         when (key-daughter-p daughter)
                         return i)
                     0)))
          (setf (rule-rhs rule)
            (cons
             key
             (nconc
              (loop for i from (- key 1) downto 0 collect i)
              (loop for i from (+ key 1) to (- arity 1) collect i)))))
        #+:head-first
        (let ((head (rule-head rule))
              (arity (length (rest (rule-order rule)))))
          (setf (rule-rhs rule)
            (cons
             head
             (nconc
              (loop for i from 0 to (- head 1) collect i)
              (loop for i from (+ head 1) to (- arity 1) collect i)))))
        (setf (gethash id (if lexical-p *lexical-rules* *rules*)) rule)))))


(defun key-daughter-p (dag)
  ;;; AAC - moved from user-fns because it's not really likely
  ;;; anyone will want to change this given the global variables
  (let* ((key (existing-dag-at-end-of dag *key-daughter-path*))
         (type (and (dag-p key) (dag-type key))))
    (when type
      (or (eq type *key-daughter-type*) 
          (and (consp type) (eq (first type) *key-daughter-type*))))))

;;; The following is called from the code which redefines types

(defun expand-rules nil
  (maphash #'(lambda (id rule)
               (reexpand-rule id rule nil))
	   *rules*)
  (maphash #'(lambda (id rule)
               (reexpand-rule id rule t))
	   *lexical-rules*))

(defun reexpand-rule (id rule lexical-p)
  (let ((non-def (rule-unifs rule))
        (def (rule-def-unifs rule)))
    (expand-rule id rule non-def def *description-persistence* lexical-p)))

;;; **********************************************
;;;
;;; Irregular morphophonology
;;;
;;; **********************************************

;;; Currently we have a format where there is a file with
;;; a big string with entries such as
;;;
;;; aliases PLUR_NOUN alias
;;;
;;; and more sensibly we should also have the lexdb (TODO - FIX)
;;; 
;;; load-irregular-spellings  is in utils.lsp
;;; this calls read-irreg-form-strings (below)

;;; For analysis, the irregular forms are indexed by irregular-form 
;;; in *irregular-forms*: the values are lists of the form
;;; ("STEM" (rule "IRREG"))
;;; (this was in order to match the output of the old morph-analyse)
;;;
;;; For generation, the forms are indexed by stem and the values are
;;; (rule . "IRREG")

(defvar *irregular-forms* (make-hash-table :test #'equal))

(defvar *irregular-forms-gen* (make-hash-table :test #'equal))

(defun find-irregular-morphs (word)
  ;;; called by default morphological analyser
  (loop for result in
	(gethash word *irregular-forms*)
      collect
	(cons (first result) (caadr result))))

(defun full-morph-generate (stem rule)
  (setf stem (string-upcase stem))
  (list (list 
	 (let ((irreg-form (gen-irreg-morphs stem rule)))
	   (or irreg-form (morph-generate stem rule)))
	 (list rule stem))))
      ;;; prefer irregular spellings (i.e., always get
      ;;; dreamt rather than dreamed if only dreamt is in irregulars -
      ;;; if both are in, prefer the first)

(defun gen-irreg-morphs (stem rule)
  ;;; assumes only one answer which is clearly wrong, but until we
  ;;; have a mechanism for alternate spellings in
  ;;; lexical entries it'll have to do
  (let ((irreg
         (reverse ; so order matches textual order
          (gethash stem *irregular-forms-gen*))))
    (cdr (assoc rule irreg)))) 


(defun morph-matches-irreg-list (stem rule)
  ;;; returns a list of surface forms corresponding to
  ;;; stem and rule in irregs
  ;;; called by morph-not-blocked-p in parse.lsp
  (if (and stem rule)
      (let ((irreg-stems 
	     (gethash stem *irregular-forms-gen*)))
	(loop for rec in irreg-stems
	    when (eql rule (car rec))
	    collect (cdr rec)))))


;;; old irregular handling code - still used if there's an
;;; external function returning a complete partial tree

(defun find-irregular-morphs-old (word)
  (gethash word *irregular-forms*))

(defun filter-for-irregs (reg-list)
  ;;; called from parse.lsp, but not now used by the default code
  ;;; input is a list of  ("A" (RULE1 "AB") (RULE2 "ABC"))
  ;;; as old morph analyse
  ;;; if *irregular-forms-only-p*  is set this will
  ;;; remove anything from the regular morphology results which corresponds
  ;;; to the application of a rule to a stem corresponding to one of 
  ;;; the regular forms (i.e. we won't parse `eated' or `dreamed'
  ;;; unless these are added specifically to the irregular forms)
  (if *irregular-forms-only-p* 
      (loop for reg in reg-list
           unless
           (let ((stem (car reg))
                 (first-rule (caadr reg)))
	     (morph-matches-irreg-p stem first-rule))
	  collect reg)
    reg-list))

(defun morph-matches-irreg-p (stem rule)
    ;;; returns t if stem and rule are in irregs
  (if (and stem rule)
      (let ((irreg-stems 
	     (gethash stem *irregular-forms-gen*)))
	(dolist (rec irreg-stems)
	  (when (eql rule (car rec))
	    (return t))))))

;;;
;;; Loading irregulars
;;; 

(defun read-irreg-form-strings (strings)
  ;;; caller has checked these are actual strings
  (clrhash *irregular-forms*)
  (clrhash *irregular-forms-gen*) 
  (dolist (string strings)
    (with-input-from-string (stream string)
         (loop for irreg = (read-line stream nil nil)
             while irreg
             unless (or (zerop (length irreg)) (char= (char irreg 0) #\;))
             do
               (let* ((irreg-right (position #\space irreg))                         
                      (spelling 
                       (if irreg-right
                           (nstring-upcase
                             (subseq irreg 0 irreg-right))))
                      (aff-right 
                       (if irreg-right
                           (position #\space irreg :start (1+ irreg-right))))
                      (affixname 
                       (if (and irreg-right aff-right)
                           (nstring-upcase
                             (subseq irreg (1+ irreg-right) aff-right))))
                      (stem-right 
                       (if aff-right
                           (position #\space irreg :start (1+ aff-right))))
                      (stem 
                       (if aff-right
                           (nstring-upcase
                             (subseq irreg (1+ aff-right) stem-right)))))
                 (if (and spelling affixname stem)
                     (add-to-irregulars spelling (create-lex-rule-name affixname) 
                                        stem)))))))

(defun add-to-irregulars (irreg-form rule stem)
  (push (list stem (list rule irreg-form))
        (gethash irreg-form *irregular-forms*))
  (push (cons rule irreg-form)
        (gethash stem *irregular-forms-gen*)))

(defun create-lex-rule-name (rule-name)
  (if *lex-rule-suffix*
    (intern (concatenate 'string (string rule-name) *lex-rule-suffix*))
    (intern rule-name)))

;;; the following strips the standard affix - used in chart output

(defun concise-rule-name (rule-name)
  (let ((str (string rule-name)))
    (if *lex-rule-suffix*
	(let ((suffix-pos (- (length str)
			     (length *lex-rule-suffix*))))
	  (if 
	      (equal (subseq str suffix-pos)
		     *lex-rule-suffix*)
	      (subseq str 0 suffix-pos)
	    str))
      str)))


;;; **************************************************************
;;;
;;; DISCO-style rule filter (build-rule-filter)
;;;
;;; **************************************************************

;;; eg. see paper http://www.aclweb.org/anthology/P99-1061

;;; the rule-filter is implemented by associating an array with each rule with 
;;; - the first dimension corresponding to an index for each rule
;;; - the second dimension corresponding to an index for each daughter

(defun build-rule-filter nil
  (format t "~%Building rule filter")
  (unless (find :vanilla *features*)
    (let ((max-arity 0)
          (nrules 0)
          (rule-list nil))
      (flet ((process-rule (name rule)
               (declare (ignore name))
               (setq max-arity (max max-arity (1- (length (rule-order rule)))))
               (push rule rule-list)
               (setf (rule-apply-index rule) nrules)
               (incf nrules)))
        (maphash #'process-rule *rules*)
        (maphash #'process-rule *lexical-rules*)
        (dolist (rule rule-list)
          (let ((filter
                 (make-array (list nrules max-arity) :initial-element nil)))
            (setf (rule-apply-filter rule)
              (fill-rule-filter rule filter rule-list))))
        t))))


(defun fill-rule-filter (rule filter test-list)
  (let ((rule-tdfs (rule-rtdfs rule))
        (rule-daughters (cdr (rule-order rule))))
    (loop for test in test-list
        do
          (let ((test-tdfs (rule-rtdfs test))
                (test-index (rule-apply-index test)))
            (loop for arg from 0 to (1- (length rule-daughters))
                for dtr in rule-daughters
                do
                  (with-unification-context (ignore)
                    (when
			;;test-tdfs in dtr posn compatible with rule-tdfs
                        (yadu rule-tdfs
                              (create-temp-parsing-tdfs
                               (if (eq test-tdfs rule-tdfs)
				   (copy-tdfs-completely test-tdfs)
                                 test-tdfs)
                               dtr))
                      (setf (aref filter test-index arg) t))))))
    filter))


(defun check-rule-filter (rule test arg)
  ;; can test fill argth daughter of rule?
  ;; argth starting at 0 
  (let ((filter (rule-apply-filter rule)))
    (if (and filter (not (stringp test)))
	(aref (the (simple-array t (* *)) filter) 
	      (rule-apply-index test)
	      arg)
      t)))


(defun dump-rule-filter (&optional (file t))
  (declare (special common-lisp-user::*grammar-version*))
  (let* ((stream (if (stringp file)
                   (open file :direction :output
                         :if-does-not-exist :create :if-exists :supersede)
                   file))
         (rules (append (get-indexed-lrules nil) (get-indexed-rules nil)))
         (rules (sort rules #'< :key #'rule-apply-index)))
    (format
     stream ";;;~%;;; rule filter for grammar ~a~%;;;~%~%"
     common-lisp-user::*grammar-version*)
    (loop
        for rule in rules
        for filter = (rule-apply-filter rule)
        do
          (format stream "~%~(~a~)::~%" (rule-id rule))
          (loop
              for i from 0 to (- (length (rule-order rule)) 2)
              do
                (format stream "  ~a:" i)
                (loop
                    for argument in rules
                    when (aref filter (rule-apply-index argument) i)
                    do (format stream " ~(~a~)" (rule-id argument)))
                (terpri stream)))
    (when (stringp file) (close stream))
    rules))
  
#|

For lexical/morphological rule application we need
to test whether there is any sequence of lexical/morphological
rules that can link two given rules because the morphophonology
specifies a partial tree.
In general we can think of rules as constructing an FSM.
For now, we built a reachability table from the rule-filter.
FIX  - Eventually we want to test lexical types to see where they plug in
and perhaps also start associating probabilities with FSMs.

the rule-filter is implemented by associating an array with each rule
with one diminsion being the number of daughters and the other dimension
corresponding to an index for each rule.   
So for lexical rule A and B - if lexical rule A has index n and 
A could be a daughter of B, then B's apply-filter will have t
as the value of cell (1,n).  For the sake of simplicity
we use the same index for the rules in the lrfsm.  
;;;
For each rule, find all possible immediate feeders and store a 
list of these.  We also have a slot in each rule for the lrfsm
and a slot for the things the rule is known to feed (as found during 
search by the algorithm).  This slot is allfed

pseudocode

for rule in rulelist

(build-lrfsm-aux rule nil)

(defun build-lrfsm-aux (rule fed) 
  (let ((newfed (set-difference fed (rule-allfed rule))))
	(when newfed
	  update all the fsms in newfed
          (setf (rule-allfed rule) (union newfed (rule-allfed rule)))
	  (for descendant in (rule-feeders rule)
	       (build-lrfsm-aux descendant (rule-allfed rule))))))

complete-lrfsm by setting all :unk to nil in each rule

in the worst case, this code will do n^2 set-difference and union operations
each on two lists of order n.  
Test case in polymorphan is with 48 rules which can all feed eachother.

Thanks to John Bowler for help with this.
|#

;;; We also need a version of the fsm where we're only interested in
;;; the case where non-spelling rules connect two rules.  This is
;;; currently done by building an fsm with just non-spelling rules
;;; which uses two more slots (but reuses the ancestor slot)

(defparameter *lrstruct-list* nil)
(defparameter *spstruct-list* nil)
(defparameter *nospstruct-list* nil)

(defparameter *check-nosp-feeding-cache* nil)
(defparameter *spelling-rule-feed-cache* nil)

(defparameter *cyclic-lr-list* nil)

(defun coindexed-orth-paths (rule)
  ;;; this function tests whether the mother and first daughter
  ;;; have coindexed ORTH values.  This is bad news in a spelling rule.
  ;;; FIX - not very happy that this function is robust - it probably doesn't
  ;;; work to test for coindexation this way if the value is an atomic FS
  ;;; hence test for list type
  (let* ((rule-fs (tdfs-indef (rule-full-fs rule)))
	 (lp (establish-linear-precedence rule-fs))
	 (mother-path (first lp))
	 (d1-path (second lp))
	 (morth (existing-dag-at-end-of rule-fs 
					(append mother-path *orth-path*)))
	 (dorth (existing-dag-at-end-of rule-fs 
					(append d1-path *orth-path*))))
    (and morth dorth 
	 (eql (type-of-fs morth) *list-type*)
	 (eq morth dorth))))

(defun build-lrfsm nil
  (format t "~%Building lr connections table")
  (let ((lrlist nil) 
	(revindex-lrules nil)
	(revindex-rules nil)
	(nosprules nil)
	(sprules nil)
	(nrules nil)
	(fedrules nil)
	(unfedrules nil)
	(nospfedrules nil)
	(nospunfedrules nil))
    (setf *spelling-rule-feed-cache* nil)
    (setf *cyclic-lr-list* nil)
    (setf *check-nosp-feeding-cache* nil)
    (setf *lrstruct-list* nil)
    (setf *spstruct-list* nil)
    (setf *nospstruct-list* nil)
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (unless nrules
		   (setf nrules (array-dimension (rule-apply-filter v) 0)))
		 (setf (rule-feeders v) nil)
		 (setf (rule-nospfeeders v) nil)
		 (setf (rule-nospfsm v) 
		   (make-array (list nrules) :initial-element :unk))
		 (setf (rule-lrfsm v) 
		   (make-array (list nrules) :initial-element :unk))
		 (setf (rule-allfed v) nil)
		 (push v lrlist)
		 (push (cons (rule-apply-index v) v) revindex-lrules)
		 (if (spelling-change-rule-p v)
		     (push v sprules)
		   (push v nosprules)))
	     *lexical-rules*)
    (dolist (sprule sprules)
      (when (coindexed-orth-paths sprule)
	(setf *syntax-error* t)
	(format t "~%Error: ~A is being treated as a spelling rule but has ORTH values coindexed~%this may crash the parser" (rule-id sprule))))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push (cons (rule-apply-index v) v) revindex-rules))
	     *rules*)
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 ;;; looking at rule v, rules which feed it can be
		 ;;; found
		 (dotimes (i nrules)
		   (when (aref (rule-apply-filter v) i 0)
		     ;; lex/morph rules have single daughter
		     (let ((feeder (cdr (assoc i revindex-lrules))))
		       (when feeder
			 (when (and
				(member v nosprules)
				(member feeder nosprules))
			   (push feeder (rule-nospfeeders v)))
			 (push feeder (rule-feeders v))
			     )))))
	     *lexical-rules*)
    (dolist (v lrlist)
      (if (rule-feeders v)
	  (push v fedrules)
	(push v unfedrules)))
    (dolist (v nosprules)
      (if (rule-nospfeeders v)
	  (push v nospfedrules)
	(push v nospunfedrules)))
;;; e.g., A can feed B, B can feed C, C can feed D, A can feed C
;;; rule-feeders of A - nil
;;;                 B - (A)
;;;                 C - (B A)
;;;                 D - (C)
    (when lrlist
      (setf *lrstruct-list* lrlist)
      (setf *spstruct-list* sprules)
      (setf *nospstruct-list* nosprules)
      ;;; three globals set because they are useful later on
      #|
      ;; commented out because it seems we don't need the complete
      ;; lrfsm and constructing it is slow in some cases
      (format t "~%Constructing main lr table")
      (dolist (rule (append unfedrules fedrules))
	(dolist (feeder (rule-feeders rule))
	  (build-lrfsm-aux 
	   feeder nrules (list rule) 'feeders 'lrfsm)))
      (dolist (lr lrlist)
	(setf (rule-allfed lr) nil)
	(complete-lrfsm lr nrules 'lrfsm))
	|#
      (when nosprules
	(format t "~%Constructing lr table for non-morphological rules")
	(dolist (rule (append nospunfedrules nospfedrules))
	  (dolist (feeder (rule-nospfeeders rule))
	    (build-lrfsm-aux 
	     feeder nrules (list rule) 'nospfeeders 'nospfsm)))
	(dolist (lr nosprules)
	  (complete-lrfsm lr nrules 'nospfsm))))))

#|
suppose A can feed B, B can feed C, C can feed D
and D can feed B (so there's a cycle)

rule-feeders of A - nil
B - (D A)
C - (B)
D - (C) 

The problem occurs that when we are checking what feeds B
(say) and we check the D branch first.  We record that D feeds B,
recurse on D, record that C feeds D, recurse on C and find we're at B.

the desired result is:

   --- A     B     C     D
   A   nil   nil   nil   nil
   B   t     t     t     t
   C   t     t     t     t
   D   t     t     t     t

note: problematic case in ERG that caused last version to loop
seemed to involve two linked cycles.  Added a test case for this to
polymorphan and also one for the pathological case where all rules feed 
eachother.

|#


  ;;; generalised so it works for both the full lrfsm
  ;;; when it will be called with feeder-slot = feeders
  ;;;                        and  fsm-slot = lrfsm
  ;;; and the no spelling rule variant 
  ;;; when it will be called with feeder-slot = nospfeeders
  ;;;                        and  fsm-slot = nospfsm


(defun build-lrfsm-aux (lr nrules in-progress feeder-slot fsm-slot)
  (let* ((all-fed (rule-allfed lr))
	 (feeders (slot-value lr feeder-slot))
	 (rule-index (rule-apply-index lr))
	 (newfed (set-difference in-progress all-fed :test #'eq)))
    (when newfed
      (setf (rule-allfed lr) (union in-progress all-fed :test #'eq))
      (dolist (fedrule newfed)
	(let ((fed-lrfsm (slot-value fedrule fsm-slot)))
	  (setf (aref fed-lrfsm rule-index) t))) ;; mark the feeding	    
      (dolist (frule feeders)
	(when (member frule in-progress)
	  ;; we've found a cycle
	  (unless (member frule *cyclic-lr-list*)
	    (push frule *cyclic-lr-list*)
	    (unless (member (rule-id frule) *known-cyclic-rules*)
	  ;;; grammar writer can choose to disable this message
	      (format t "~%Warning: ~A can feed itself" 
		      (rule-id frule)))))
	  ;; regardless of whether we're in a cycle, we keep going,
	;; because we may have more fed things to add
	(build-lrfsm-aux 
	 frule nrules (cons lr (rule-allfed lr)) feeder-slot fsm-slot)))))

(defun complete-lrfsm (lr nrules fsm-slot)
  (let ((lrfsm (slot-value lr fsm-slot)))
    (dotimes (i nrules)
      (when (eql (aref lrfsm i) :unk)
	(setf (aref lrfsm i) nil)))))

#|

(defun check-lrfsm (rule test)
 #+:pdebug (format t "~%Can ~A be a descendant of ~A? " (rule-id test)
		   (rule-id rule))
  ;; can test be a descendant of rule 
  (let ((filter (rule-lrfsm rule)))
    (if filter
	(let ((res 
	       (aref filter (rule-apply-index test))))
	  #+:pdebug (if res (format t "Yes")  (format t "No"))
	  res)
      (progn 
	#+:pdebug (format t "Don't know") 
	t))))

|#

(defun check-nospfsm (rule test)
 #+:pdebug (format t "~%Can ~A be a descendant of ~A? " (rule-id test)
		   (rule-id rule))
  ;; can test be a descendant of rule 
  (let ((filter (rule-nospfsm rule)))
    (if filter
	(let ((res 
	       (aref filter (rule-apply-index test))))
	  #+:pdebug (if res (format t "Yes")  (format t "No"))
	  res)
      (progn 
	#+:pdebug (format t "Don't know") 
	t))))

;;; FIX - probably better to turn the following into proper tables


(defun check-nosp-feeding (rule test)
  ;;; this is a bit more complex.  rule and test should both be
  ;;; sp rules (this is called on the morphophon stuff)
  ;;; We want to see if either there's a direct feeding relationship
  ;;; or some chain of non-spelling rules between them.  
  ;;; We memoise the calls to this
  (let* ((rule-known (assoc rule *check-nosp-feeding-cache*))
	 (known (if rule-known (assoc test (cdr rule-known)))))
    (if known
	(cdr known)
      (let* ((direct-feed (check-rule-filter rule test 0))
	     (res (or direct-feed
		      (indirect-lr-feed rule test))))
	;;; cache result
	(let ((testres (cons test res))
	      (rule-knowns (assoc rule *check-nosp-feeding-cache*)))
	  (if rule-knowns
	      (push testres (cdr rule-knowns))
	    (push (cons rule (list testres)) *check-nosp-feeding-cache*)))
	;;; and return it ...
	res))))

(defun indirect-lr-feed (rule test)
  (let ((rule-in nil)
	(test-out nil))
    (dolist (inter *nospstruct-list*)
      (when (check-rule-filter rule inter 0)
	(push inter rule-in))
      (when (check-rule-filter inter test 0)
	(push inter test-out)))
    (dolist (inter1 rule-in)
      (when
	  (dolist (inter2 test-out)
	    (when (or (eq inter1 inter2)
		      (check-nospfsm inter1 inter2))
	      (return t)))
	(return t)))))

(defun check-sp-lr-feeding (rule test)
  ;;; and yet another one - for the case where a morphological
  ;;; rule is tested against a nosp rule to see if they are linked
  ;;; directly or by nosp rules
  (or (check-rule-filter rule test 0)
      (dolist (inter *nospstruct-list*)
	(when (check-rule-filter rule inter 0)
	  (when (check-nospfsm inter test)
	    (return t))))))
		
  
#|
(defun print-lrfsm (&key (stream t))
  (format stream "~%Specified feeding relationships~% Descendant~30TMother")
  (dolist (rule1 *lrstruct-list*)
    (dolist (rule2 *lrstruct-list*)
      (let ((feeds (check-lrfsm rule2 rule1)))
	(when feeds
	  (format stream "~% ~A~30T~A" 
		  (rule-id rule1) (rule-id rule2)))))))
|#

;;; called by print-lrfsm-toplevel
;;; and by testing code in polymorphan

(defun print-nospfsm (&key (stream t))
  (format stream "~%Specified feeding relationships~% Descendant~30TMother")
  (dolist (rule1 *nospstruct-list*)
    (dolist (rule2 *nospstruct-list*)
      (let ((feeds (check-nospfsm rule2 rule1)))
	(when feeds
	  (format stream "~% ~A~30T~A" 
		  (rule-id rule1) (rule-id rule2)))))))

(defun print-nospfeeding (&key (stream t))
  (format stream "~%Specified feeding relationships~% Descendant~30TMother")
  (dolist (rule1 *spstruct-list*)
    (dolist (rule2 *spstruct-list*)
      (let ((feeds (check-nosp-feeding rule2 rule1)))
	(when feeds
	  (format stream "~% ~A~30T~A" 
		  (rule-id rule1) (rule-id rule2)))))))


(defun spelling-rule-feeds-p (rule-id)
  ;;; use the lrfsm to check whether anything can feed this rule.
  ;;; cache the results
  (let ((seen (assoc rule-id *spelling-rule-feed-cache*)))
    (if seen (cdr seen)
      (let* ((rule-entry (get-lex-rule-entry rule-id))
	     (feeder-p
	      (dolist (srule *spstruct-list*)
		(when 
		    (check-nosp-feeding rule-entry srule)
		  (return t)))))
	(push (cons rule-id feeder-p)
	      *spelling-rule-feed-cache*)
	feeder-p))))

;;; *********************************************************
;;;
;;; Filter for lexical types and rules
;;;
;;; *********************************************************

;;; the idea is to check whether the lexical type is compatible
;;; either with the given rule or with any lexical rules in the lrfsm 
;;; that can feed that rule

(defparameter *lex-type-filter* nil)

(defstruct lex-type-filter
  rule-tests)
;;; rule tests is an a-list with rules paired with boolean values

(defun check-lex-type-filter (entry rule)
  (declare (ignore entry rule))
  t)
  #|
  (let ((lex-type
	 (get-type-from-unifs (lex-entry-unifs entry))))
    (if lex-type
	(let ((type-filter-entry (gethash lex-type *lex-type-filter*)))
	  (if type-filter-entry
	      (let ((checked-p
		     (assoc rule (lex-type-filter-rule-tests type-filter-entry))))
		(if checked-p 
		    (cdr checked-p)
		  (push (cons rule (check-type-with-rule lex-type rule))
			(lex-type-filter-rule-tests type-filter-entry))))
	    (setf (gethash lex-type *lex-type-filter*)
	      (make-lex-type-filter
	       :rule-tests
	       (list 
		(cons rule (check-type-with-rule lex-type rule)))))))
      t))) ; can't identify a type so the filter is no use
|#

;;; rules-to-xml moved to io-general/outputsrc.lsp since this is where
;;; all other output functions live
