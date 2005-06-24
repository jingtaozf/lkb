;;; Copyright (c) 1991-2005 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see licence.txt for conditions


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

(defun clear-grammar nil
  (setf *ordered-rule-list* nil)
  (when (fboundp 'clear-generator-grules)
    (funcall 'clear-generator-grules))
  (when (fboundp 'clear-generator-grules)
    (funcall 'clear-generator-grules))
  (clrhash *rules*))

(defun clear-lex-rules nil
  (setf *ordered-lrule-list* nil)
  (when (fboundp 'reset-cached-lex-entries)
    (funcall 'reset-cached-lex-entries))
  (when (fboundp 'clear-generator-lrules)
    (funcall 'clear-generator-lrules))
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
  lrfsm ;;; generalisation of filter for lexical rules
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
  ;;; this is now just called interactively etc, not by the parser
   ;;; entries are pairs with list of rules applied plus result
   (incf *number-of-applications*)
   (when (> *number-of-applications* *maximal-lex-rule-applications*)
      (error "~%Probable circular lexical rule"))
   (let ((transformed-entries 
            (loop for entry in entries
               append
               (loop for rule in 
                  (get-indexed-lrules (cdr entry)
                     #'(lambda (rule) (member (rule-id rule) ignore-list)))
                  nconc
                  (let* ((spelling-rule-p (spelling-change-rule-p rule))
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
            (try-all-lexical-rules transformed-entries ignore-list)))))


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
    ;; additional set of `mal' rules that detect and correct certain errors
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

(defun expand-rule (id rule non-def def rule-persistence lexical-p)
  (process-unif-list id non-def def rule rule-persistence)
  (let ((fs (rule-full-fs rule)))  
    (when fs
      (setf (rule-rtdfs rule) (copy-tdfs-partially fs))
      (if lexical-p 
        (pushnew id *ordered-lrule-list*)
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
               (daughter (existing-dag-at-end-of dag *head-daugher-path*))
               (head (existing-dag-at-end-of dag *head-path*)))
          (setf (rule-head rule)
            (or (when daughter
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      when (eq foo daughter) return i))
                (when head
                  (loop
                      for path in daughters
                      for i from 0
                      for foo = (existing-dag-at-end-of dag path)
                      for bar = (when foo
                                  (existing-dag-at-end-of foo *head-path*))
                      when (eq bar head) return i))
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
;;; and more sensibly we should also have the lexdb (TODO)
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
  (gethash word *irregular-forms*))

(defun gen-irreg-morphs (stem rule)
  ;;; assumes only one answer which is clearly wrong, but until we
  ;;; have a mechanism for alternate spellings in
  ;;; lexical entries it'll have to do
  (let ((irreg
         (reverse ; so order matches textual order
          (gethash stem *irregular-forms-gen*))))
    (cdr (assoc rule irreg)))) 

(defun full-morph-generate (stem rule)
  (setf stem (string-upcase stem))
  (list (list 
	 (let ((irreg-form (gen-irreg-morphs stem rule)))
	   (or irreg-form (morph-generate stem rule)))
	 (list rule stem))))
      ;;; prefer irregular spellings (i.e., always get
      ;;; dreamt rather than dreamed)


(defun filter-for-irregs (reg-list)
  ;;; called from parse.lsp
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
    (let* ((irreg-stems 
	    (gethash stem *irregular-forms-gen*))
	   (irreg-rules (mapcar #'car irreg-stems)))
      (and irreg-stems rule
	   (member rule irreg-rules))))

;;; 

(defun read-irreg-form-strings (strings)
  ;;; caller has checked these are actual strings
  (clrhash *irregular-forms*)
  (clrhash *irregular-forms-gen*) 
  (dolist (string strings)
    (with-input-from-string (stream string)
         (loop for irreg = (read-line stream nil nil)
             while irreg
             unless (or (zerop (length irreg)) (eq (elt irreg 0) #\;))
             do
               (let* ((irreg-right (position '#\  irreg))                         
                      (spelling 
                       (if irreg-right
                           (string-upcase (subseq irreg 0 irreg-right))))
                      (aff-right 
                       (if irreg-right
                           (position '#\  irreg :start (+ 1 irreg-right))))
                      (affixname 
                       (if (and irreg-right aff-right)
                           (string-upcase
                            (subseq irreg (+ 1 irreg-right) aff-right))))
                      (stem-right 
                       (if aff-right
                           (position '#\  irreg :start (+ 1 aff-right))))
                      (stem 
                       (if aff-right
                           (string-upcase
                            (subseq irreg (+ 1 aff-right) stem-right)))))
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

;;; For lexical/morphological rule application we need
;;; to test whether there is any sequence of lexical/morphological
;;; rules that can link two given rules because the morphophonology
;;; specifies a partial tree.
;;; In general we can think of rules as constructing an FSM.
;;; For now, we built a reachability table from the rule-filter.
;;; FIX  - Eventually we want to test lexical types to see where they plug in
;;; and perhaps also start associating probabilities with FSMs.

;;; the rule-filter is implemented by associating an array with each rule
;;; with one diminsion being the number of daughters and the other dimension
;;; corresponding to an index for each rule.   
;;; So for lexical rule A and B - if lexical rule A has index n and 
;;; A could be a daughter of B, then B's apply-filter will have t
;;; as the value of cell (1,n).  For the sake of simplicity
;;; we use the same index for the rules in the lrfsm.  
;;;
;;; For each rule, find all possible immediate feeders and store a 
;;; list of these.
;;; For each rule, look at feeders' arrays and set these values on 
;;; the new rule array, recursing as needed.  If we find ourselves in a cycle, 
;;; we have to stop recursing.  We record we're not done, and will
;;; reexplore that branch (see below).  This means that, although
;;; the code works when there are cycles, it could be very expensive to
;;; compute the fsm.

(defparameter *spelling-rule-feed-cache* nil)

(defparameter *cyclic-lr-list* nil)

(defun build-lrfsm nil
  (let ((lrlist nil)
	(revindex-lrules nil)
	(revindex-rules nil)
	(nrules nil))
    (setf *spelling-rule-feed-cache* nil)
    (setf *cyclic-lr-list* nil)
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (unless nrules
		   (setf nrules (array-dimension (rule-apply-filter v) 0)))
		 (setf (rule-feeders v) nil)
		 (setf (rule-lrfsm v) 
		   (make-array (list nrules) :initial-element :unk)) 
		 (push v lrlist)
		 (push (cons (rule-apply-index v) v) revindex-lrules))
	     *lexical-rules*)
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
		     (let ((feeder (assoc i revindex-lrules)))
		       (if feeder
		       (push feeder (rule-feeders v))
		       (format t "~&Warning: Non-lexical rule ~a potentially feeds lexical rule ~a" 
			       (rule-id (cdr (assoc i revindex-rules))) (rule-id v))
		       )))))
	     *lexical-rules*)
;;; e.g., A can feed B, B can feed C, C can feed D, A can feed C
;;; rule-feeders of A - nil
;;;                 B - (A)
;;;                 C - (B A)
;;;                 D - (C)
    (when lrlist
      (build-lrfsm-aux (car lrlist) (cdr lrlist) nrules nil)
      (dolist (crule *cyclic-lr-list*)
	(format t "~%Warning: ~A can feed itself" (rule-id crule)))
      (dolist (lr lrlist)
       (complete-lrfsm lr nrules)))))

#|
suppose A can feed B, B can feed C, C can feed D, A can feed C
and D can feed B (so there's a cycle)

;;; rule-feeders of A - nil
;;;                 B - (A D)
;;;                 C - (B A)
;;;                 D - (C) 

;;; desired result
;;;
   --- A     B     C     D
   A   nil   nil   nil   nil
   B   t     t     t     t
   C   t     t     t     t
   D   t     t     t     t
|#


(defun build-lrfsm-aux (lr remainder nrules in-progress)
  ;;; looking at rule lr, set the lrfsm values
  ;;; to t for each feeder and to t for each feeder's feeder
  ;;; if feeder is done.  If not done, recurse on feeder.
  ;;; Test for done is whether rule-feeders is :done
  (let ((feeders (rule-feeders lr)))
    (unless (eql feeders :done)
      (let ((lrfsm (rule-lrfsm lr))
	    (todo nil))
	(dolist (feeder feeders)
	  (build-lrfsm-aux-process-feeders feeder lrfsm nrules in-progress todo lr))
	(if todo
	    (setf (rule-feeders lr) todo)
	  ;;; we've found a cycle in the feeders, so we can't complete yet
	  (progn
	    (dotimes (i nrules)
	      (when (eql (aref lrfsm i) :unk)
		(setf (aref lrfsm i) nil)))
	    (setf (rule-feeders lr) :done)))))
    (when remainder 
      (build-lrfsm-aux (car remainder)
		       (cdr remainder) nrules nil))))
    
(defun build-lrfsm-aux-process-feeders (feeder lrfsm nrules in-progress todo lr)    
    (let ((frule (cdr feeder))
	  (fnum (car feeder)))
	    	;;; set the feeder index
      (setf (aref lrfsm fnum) t)
      (if (member frule in-progress)
	  ;; we're in a cycle, so the feeder fsm isn't complete
	  ;; stop, but record we're not done
	  (progn
	    (pushnew frule *cyclic-lr-list*)
	    (push feeder todo))
	;; otherwise find the feeder fsm, recursing if necessary
	(progn 
	  (build-lrfsm-aux frule nil nrules 
			   (cons lr in-progress))
	  ;; propagate the feeder values
	  (dotimes (i nrules)
	    (when (eql (aref (rule-lrfsm frule) i) t)
	      (setf (aref lrfsm i) t)
	      ))
	  ;; if there's a cycle further back, record we're
	  ;; not done
	  (unless (eql (rule-feeders frule) :done)
	    (push feeder todo))))))

(defun complete-lrfsm (lr nrules)
  ;;; go through the cases we haven't been able to complete because
  ;;; of cycles and set values.
  (let ((feeders (rule-feeders lr)))
    (unless (eql feeders :done)
      (let ((lrfsm (rule-lrfsm lr)))
	(dolist (feeder feeders)
	  (let ((frule (cdr feeder)))
		;; propagate the feeder values
		(dotimes (i nrules)
		  (when (eql (aref (rule-lrfsm frule) i) t)
		    (setf (aref lrfsm i) t)))))
	(dotimes (i nrules)
	  (when (eql (aref lrfsm i) :unk)
	    (setf (aref lrfsm i) nil)))
	(setf (rule-feeders lr) :done)))))

			  
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

(defun print-lrfsm (&key (stream t))
  (let ((rule-list nil))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v rule-list))
	     *lexical-rules*)
    (format stream "~%Specified feeding relationships~% Descendant      Mother")
    ;;; FIX - can't remember how to tab!
    (dolist (rule1 rule-list)
      (dolist (rule2 rule-list)
	(let ((feeds (check-lrfsm rule2 rule1)))
	  (when feeds
	    (format stream "~% ~A ~A" 
		    (rule-id rule1) (rule-id rule2))))))))




(defun spelling-rule-feeds-p (rule-id)
  ;;; use the lrfsm to check whether anything can feed this rule.
  ;;; cache the results
  (let ((seen (assoc rule-id *spelling-rule-feed-cache*)))
    (if seen (cdr seen)
      (let ((spelling-rules nil))
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (when (spelling-change-rule-p v)
		       (push v spelling-rules)))
		 *lexical-rules*)
	(let ((feeder-p
	       (dolist (srule spelling-rules)
		 (when 
		     (check-lrfsm 
		      (get-lex-rule-entry rule-id)
		      srule)
		   (return t)))))
	  (push (cons rule-id feeder-p)
		*spelling-rule-feed-cache*)
	  feeder-p)))))

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
