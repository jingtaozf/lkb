;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Ann Copestake (CSLI),Walter Kasper (DFKI)
;;    Purpose: Converting MRS to VIT
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting MRS (as lispified structures defined in basemrs.lsp)
;;; to VIT (as lispified structures defined in vit.lsp)

(in-package "MRS")

#|

here is an example of a lispified MRS (slightly faked compared to the actual grammar output
in order to illustrate some stuff succinctly)

#S(PSOA :HANDEL #S(VAR :NAME "h1" :EXTRA NIL :ID 1)
             :INDEX #S(VAR :NAME "e2" :EXTRA "(E:t3, R:t3, S:t4)" :ID 2)
             :LISZT
             (#S(REL :SORT PRPSTN_REL :HANDEL #S(VAR :NAME "h1" :EXTRA NIL :ID 1)
                     :FLIST (#S(FVPAIR :FEATURE SOA :VALUE #S(VAR :NAME "h5" :EXTRA NIL :ID 5))))
              #S(REL :SORT _EVERY_REL :HANDEL #S(VAR :NAME "h6" :EXTRA NIL :ID 6)
                     :FLIST (#S(FVPAIR :FEATURE BV :VALUE #S(VAR :NAME "x9" :EXTRA "(3SG_N)" :ID 9))
                             #S(FVPAIR :FEATURE RESTR :VALUE #S(VAR :NAME "h8" :EXTRA NIL :ID 8))
                             #S(FVPAIR :FEATURE SCOPE :VALUE #S(VAR :NAME "h7" :EXTRA NIL :ID 7))))
              #S(REL :SORT _DOG_N_REL :HANDEL #S(VAR :NAME "h10" :EXTRA NIL :ID 10)
                     :FLIST (#S(FVPAIR :FEATURE INST :VALUE #S(VAR :NAME "x9" :EXTRA "(3SG_N)" :ID 9))))
              #S(REL :SORT _CHASE_V_REL :HANDEL #S(VAR :NAME "h11" :EXTRA NIL :ID 11)
                     :FLIST (#S(FVPAIR :FEATURE EVENT :VALUE #S(VAR :NAME "e2" :EXTRA "(E:t3, R:t3, S:t4)" :ID 2))
                             #S(FVPAIR :FEATURE ACT :VALUE #S(VAR :NAME "x9" :EXTRA "(3SG_N)" :ID 9))
                             #S(FVPAIR :FEATURE UND :VALUE #S(VAR :NAME "x12" :EXTRA "(3SG_N)" :ID 12))))
              #S(REL :SORT TEMP_PREC_REL :HANDEL #S(VAR :NAME "h11" :EXTRA NIL :ID 11)
                     :FLIST (#S(FVPAIR :FEATURE EVENT1 :VALUE #S(VAR :NAME "t3" :EXTRA NIL :ID 3))
                             #S(FVPAIR :FEATURE LATER :VALUE #S(VAR :NAME "t4" :EXTRA NIL :ID 4))))
              #S(REL :SORT _SOME_INDIV_REL :HANDEL #S(VAR :NAME "h13" :EXTRA NIL :ID 13)
                     :FLIST (#S(FVPAIR :FEATURE BV :VALUE #S(VAR :NAME "x12" :EXTRA "(3SG_N)" :ID 12))
                             #S(FVPAIR :FEATURE RESTR :VALUE #S(VAR :NAME "h15" :EXTRA NIL :ID 15))
                             #S(FVPAIR :FEATURE SCOPE :VALUE #S(VAR :NAME "h14" :EXTRA NIL :ID 14))))
              #S(REL :SORT _CAT_N_REL :HANDEL #S(VAR :NAME "h16" :EXTRA NIL :ID 16)
                     :FLIST (#S(FVPAIR :FEATURE INST :VALUE #S(VAR :NAME "x12" :EXTRA "(3SG_N)" :ID 12))))
              #S(REL :SORT _BLACK_ADJ_REL :HANDEL #S(VAR :NAME "h16" :EXTRA NIL :ID 16)
                     :FLIST (#S(FVPAIR :FEATURE INST :VALUE #S(VAR :NAME "x12" NIL :ID 12)))))
             :H-CONS
             (#S(HCONS :SCARG #S(VAR :NAME "h5" :EXTRA NIL :ID 5)
                       :CANDS (#S(VAR :NAME "h13" :EXTRA NIL :ID 13)
                               #S(VAR :NAME "h6" :EXTRA NIL :ID 6)
                               #S(VAR :NAME "h11" :EXTRA NIL :ID 11))
                       :OUTSCPD NIL)
              #S(HCONS :SCARG #S(VAR :NAME "h8" :EXTRA NIL :ID 8)
                       :CANDS (#S(VAR :NAME "h10" :EXTRA NIL :ID 10))
                       :OUTSCPD NIL)
              #S(HCONS :SCARG #S(VAR :NAME "h6" :EXTRA NIL :ID 6)
                       :CANDS NIL
                       :OUTSCPD #S(VAR :NAME "h10" :EXTRA NIL :ID 10))
              #S(HCONS :SCARG #S(VAR :NAME "h15" :EXTRA NIL :ID 15)
                       :CANDS (#S(VAR :NAME "h16" :EXTRA NIL :ID 16))
                       :OUTSCPD NIL)
              #S(HCONS :SCARG #S(VAR :NAME "h13" :EXTRA NIL :ID 13)
                       :CANDS NIL
                       :OUTSCPD #S(VAR :NAME "h16" :EXTRA NIL :ID 16))
              #S(HCONS :SCARG #S(VAR :NAME "h13" :EXTRA NIL :ID 13)
                       :CANDS NIL
                       :OUTSCPD #S(VAR :NAME "h6" :EXTRA NIL :ID 6))))

The parts of the VIT that we are attempting to generate from this are:
1. Semantics
2. Main condition
3. Scope

We should also be able to generate

4. Tenseandaspect
5. Syntax

I ignore tense and aspect for now, since there may well be changes
Syntax looks a bit tedious but reasonably trivial, so I am leaving this to later

Here is what I think the target VIT semantics would be (ignoring the issues
involved in translation of relation names etc):

        :SEMANTICS (#S(P-TERM :PREDICATE PRPSTN_REL :ARGS (L1 H5))
                    #S(P-TERM :PREDICATE EVERY_REL :ARGS (L6 I9 H8 H7)) 
                    #S(P-TERM :PREDICATE DOG_REL :ARGS (L10 I9)) 
                    #S(P-TERM :PREDICATE CHASE_REL :ARGS (L11 I2))
                    #S(P-TERM :PREDICATE ARG2 :ARGS (L11 I2 I9))
                    #S(P-TERM :PREDICATE ARG3 :ARGS (L11 I2 I12))
                    #S(P-TERM :PREDICATE SOME_REL :ARGS (L13 I12 H15 H14)) 
                    #S(P-TERM :PREDICATE CAT_REL :ARGS (L161 I12)) 
                    #S(P-TERM :PREDICATE BLACK_REL :ARGS (L162 I12)))
        :MAIN-CONDITION L1
        :SCOPE (#S(P-TERM :PREDICATE IN_G :ARGS (L161 L16))
                #S(P-TERM :PREDICATE IN_G :ARGS (L162 L16))
                #S(P-TERM :PREDICATE EQ :ARGS (L10 H8))
                #S(P-TERM :PREDICATE EQ :ARGS (L16 H15))
                #S(P-TERM :PREDICATE EQ :ARGS (L13 H5))
                #S(P-TERM :PREDICATE EQ :ARGS (L6 H14))
                #S(P-TERM :PREDICATE EQ :ARGS (L11 H7))
                #S(P-TERM :PREDICATE LEQ :ARGS (L11 H7))
                #S(P-TERM :PREDICATE LEQ :ARGS (L11 H14))
                etc etc

what's happening here, beside the trivial rearrangement of structures is:
1. renaming variables - MRS x's and e's collapse to i, h's go to l's when they're
   labelling known rels.  Since we are effectively making the distinction between
   holes and labels in the MRS scoping stuff anyway, I have added a parameter to 
   the structure for handel variables to record if they are label handels
2. splitting the chase_rel into a Parsons style representation
3. changing the labels of black_rel and cat_rel so they are distinct
- this requires that we add grouping statements in scope -
old style would be group(L15,[L151, L152]), new style is in_g(L151,L15), in_g(L152,L15)
4. Adding scoping statements of the form LEQ or EQ
- in this example, the `outscoped' constraint has actually disambiguated the structure
Since the simplest technique (and possibly the only technique) for converting
the scope conditions currently used in MRS to VIT is to expand all structures and then
deduce the constraints, a plausible idea is to generate ALL
the possible LEQ and EQ statements.  Obviously this may not correspond to the output of a 
particular grammar - this however is going to be true of any mechanism.  (Even if
we use LEQs in MRS, the _precise_ leqs generated will depend to some extent on assumptions
about syntax.)  It's not worth devoting too much effort to sorting out the current situation,
because of the likelyhood of changes in the grammar.

|#
;;; hash table form semdbs:

(defvar *vit-semdb* (make-hash-table :test #'eq))

(defstruct semdbitem
  gramrel
  vitrel
  relsort
  args
  extra)

(defstruct vitarg
  gramrole
  vitrole
  vitsort)

;;; I assume that args is a list of the parameters 
;;; needs extension as the information becomes available from SemDB
(defun create-db-item (arglist)
  (make-semdbitem :gramrel (first arglist)
                  :vitrel (second arglist)
                  :relsort (third arglist)
                  :args (if (fourth arglist)
                               (loop for arg in (fourth arglist)
                                   collect
                                     (make-vitarg :gramrole (first arg)
                                                  :vitrole (second arg)
                                                  :vitsort (when (third arg)
                                                             (third arg)))))
                  :extra (subseq arglist 4)))


(defun insert-db-item (&rest args)
  (let ((item (create-db-item args)))
    (setf (gethash (semdbitem-gramrel item) *vit-semdb*)
      item)))

(defun get-db-item (key &optional (table *vit-semdb*))
  (gethash key table))


;;; **********  Globals  ***********


;;; *** `local' globals ****

(defparameter *group-members* nil)

(defvar *vit-instances* nil)

(defvar *hole-label-eqs* nil
  "store additional eqs for labels which must be holes according to vitADT")

(defparameter *used-handel-labels* nil
  "store handels which are used as base labels")

;;; **** General purpose utility functions ****

;;; general purpose functions for creating new atoms ...

(defun next (template)
       (let ((instance nil)
             (number (+ (or (get template 'last-number)
                            0)
                        1)))
         (setf (get template 'last-number)
               number)
         (setf instance
               (intern
                 (concatenate 'string
                              (string template)
                              (princ-to-string number))))
         (push instance
               (get template 'children))
         (setf (get instance 'root-template)
               template)
         instance))

;;; and destroying them
;;; (not used at the moment, so get different numbers every time the groups are
;;; constructed)

(defun scratch (templates)
;;; see the function Next - scratch removes all info from the template symbols
;;; used by Next and in effect reinitialises the values.  It can take a single
;;; item or a list of templates to be reinitialised.
    (dolist (template (if (listp templates) templates (list templates)))
	    (remprop template 'last-number)
	    (dolist (child (get template 'children))
		    (setf (symbol-plist child)
			  nil))
	    			; a bit drastic - have to hope nothing
				; important is kept on p-list by anyone else
	    (remprop template 'children)))

(defun clear-temporary-dbs ()
  (setf *group-members* nil
        *vit-instances* nil
        *hole-label-eqs* nil
        *used-handel-labels* nil))

;;; here with fixed package
(defun p-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args) "MRS"))

;;; pred1 is assumed to be a symbol in the grammar domain
;;; at present the DBs do not appear appropriate to the purpose
(defun insert-predicate-translation (pred1 pred2 
                                     &optional 
                                     (domain (tdl-show-current-domain)))
  (let ((dompred (find-symbol (string pred1) domain)))
    (setf (gethash (if dompred
                       dompred
                     pred1) *vit-semdb*) pred2)))

;;; table lookup noch einbauen
(defun get-vit-predicate-name (relname)
  (let ((dbitem (get-db-item relname)))
    (if dbitem
        (semdbitem-vitrel dbitem)
      (let* ((rel1 (if *sem-relation-suffix*
                       (remove-name-suffix relname *sem-relation-suffix*)
                     relname))
             (rel2 (if *sem-relation-prefix*
                       (remove-name-prefix rel1 *sem-relation-prefix*)
                     rel1)))
        (string-trim "*" rel2)))))

(defun collect-values-from-rel (rel)
  ;;; returns the values from a relation (other than the handel)
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (for fvp in (rel-flist  rel)
       collect 
       (fvpair-value fvp)))


(defun get-arg-role (var)
  (if (var-p var)
      (cond ((mrs-language '(german japanese))
             (let ((throle (find *throle-feature*
                                  (var-extra var) 
                                  :key #'fvpair-feature)))
                   (if throle
                       (fvpair-value throle))))
               
            (t nil))))

(defun coord-var-p (var)
  (and (var-p var)
       (eql (elt (var-name var) 0) '#\c)))

;;; from Dan's 25/8/97-version

;; Modified collect-args-and-values-from-rel to filter out optional argument
;; variables that were not filled.  These variables are always given a name
;; beginning with "v", so this encoding is tested here.

(defun optional-var-p (var)
  (and (var-p var)
       (eql (elt (var-name var) 0) '#\v)))       

(defun collect-args-and-values-from-rel (rel)
  ;;; returns the values from a relation (other than the handel)
  ;;; split up in those for arg-relations and normal
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (let ((args nil)
        (others nil))
    (loop for fvp in (rel-flist rel)
             do
          (let* ((value (fvpair-value fvp))
                 (feat (fvpair-feature fvp))
                 (def (assoc feat *mrs-arg-features*))
                 (arg (if def
                          (or (and (consp (rest def))
                                   (first (rest def)))
                           (get-arg-role value) (rest def) feat))))
            ;; or better use fvp???
            (unless (and (optional-var-p value)
                         (mrs-language '(english)))
              (if arg
                   ;; args assoc-Liste (arg . var)
                  (setf args (acons arg value args))
                (setf others (append others (list (fvpair-value fvp))))))))
    (list args others)))

(defun collect-all-handel-vars (rels)
  ;;; Given a set of relations, this returns the list of labels and of
  ;;; groups (i.e. labels which turn up more than once)
  ;;; a bit redundant, given that we're using the scoping code,
  ;;; but I don't want to mess with that unnecessarily
  ;;; label-list now is a assoc-list (handel . label)
  ;;; WK: now we treat every handel as a group
  (let ((label-list nil)
        (group-list nil)
        (except-list nil))
    (loop for rel in rels
         do
          (let ((var (rel-handel rel))
                (label (rel-label rel))
                (sp-rel (assoc (rel-sort rel)
                               *vm-special-label-hack-list*)))
;           (unless (is-handel-var var)
;             (error "~%Relation ~A has incorrect handel ~A"
;                    (rel-sort rel) var))
;;           (cond ((assoc (var-id var) label-list)
;;                  (pushnew (var-id var) group-list))
;;                 ((group-var-p var)
;;                  (pushnew (var-id var) group-list))
            ;;                 (t t))
            (when (var-p var)
              (pushnew (cons (var-id var) 
                             (if (var-p label)
                                 (var-id label))) 
                       label-list)
              (if sp-rel
                  (let* ((arg (nth (rest sp-rel) (rel-flist rel))))
                    (when (is-handel-var (fvpair-value arg))
                      (push (var-id (fvpair-value arg)) except-list)
                      (setf group-list (delete (var-id (fvpair-value arg))
                                               group-list)))))
              (if (member (var-id var) except-list)
                  t
                (pushnew (var-id var) group-list)))))
    (values label-list group-list)))


(defun collect-all-instance-vars (liszt)
  (let ((instances))
    (loop for rel in liszt
        do
          (loop for var in (rel-flist rel)
              do
                (if (and (var-p var)
                         (not (is-handel-var var)))
                    (pushnew var instances))))
    instances))

;;; ***** Main code *******

;;; (mrs-to-vit-file "Macintosh HD:lkb96:mrs:mrs-vit-tests")

(defun mrs-to-vit-file (file-name)
  (with-open-file
   (istream file-name :direction :input)
   (do* ((sentence (read istream nil 'eof) (read istream nil 'eof ))
         (mrsstruct (read istream nil 'eof) (read istream nil 'eof)))
        ((or (eql sentence 'eof) (eql mrsstruct 'eof)) nil)
        (when (and mrsstruct (psoa-p mrsstruct))
          (multiple-value-bind 
            (vit binding-sets)
            (mrs-to-vit mrsstruct)
            (setf *canonical-bindings* nil)
            (format t "~%~A" sentence)
            (format t "~%FSs")
            (output-mrs mrsstruct 'simple)
            (format t "~%Unscoped form")
            (output-mrs mrsstruct 'indexed)
            ;;; then try and find sets of bindings which will give a fully scoped 
            ;;; structure, and output the results
            (if binding-sets
              (format t "~%Scoped form(s)")
              (format t "~%WARNING: Invalid MRS structure"))
            (for binding in binding-sets
                 do
                 (setf *canonical-bindings* (canonical-bindings binding))
                 (output-connected-mrs mrsstruct 'indexed)
                 (output-scoped-mrs mrsstruct))
            (when vit
              (write-vit-pretty t (horrible-hack-2 vit)))
            (format t "~%****************************************"))))))

;;; function to call from PAGE or LKB interface

(defun mrs-to-vit-convert (mrs-psoa &optional (standalone t))
  (if (eq *mrs-for-language* 'english)
      (let ((mrsstruct
             (if (boundp '*ordered-mrs-rule-list*)
                 (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
             mrs-psoa)))
        (multiple-value-bind 
            (vit binding-sets)
            (mrs-to-vit mrsstruct)
          (setf *canonical-bindings* nil)
          (when standalone
            (format t "~%Unscoped form")
            (output-mrs mrsstruct 'indexed)
            ;;; then try and find sets of bindings which will give a fully scoped 
            ;;; structure, and output the results
            (if binding-sets
                (format t "~%~A scoped form(s) ~A" (length binding-sets)
                        (if (> (length binding-sets) 10)
                            "only printing first 10" 
                          ""))
              (format t "~%WARNING: No valid scopes - invalid MRS structure "))
            (for binding in (subseq binding-sets 0 (min (length binding-sets) 10))
                 do
                 (setf *canonical-bindings* (canonical-bindings binding))
                 (output-connected-mrs mrsstruct 'indexed)
                 (output-scoped-mrs mrsstruct)))
          (when (and vit standalone)
            (write-vit-pretty t (horrible-hack-2 vit))
            (format t "~%"))
	  (check-vit vit)
          vit))
    (let ((vit (german-mrs-to-vit mrs-psoa)))
      (when standalone
        (format t "~%Unscoped form")
        (output-mrs mrs-psoa 'indexed))
      (when (and vit standalone)
        (write-vit-pretty t vit)
        (format t "~%"))
      vit)))

(defun check-vit (vit &optional (as-string nil) (stream *standard-output*))
  (with-open-file (vit-out "~/tmp/vitcheck" :direction :output
	                                    :if-exists :supersede)
    (format vit-out "ensure_loaded(vitADT).~%V = ")
    (if as-string 
	(format vit-out "~A" vit)
      (write-vit vit-out vit))
    (format vit-out ",vitCheck(V).~%~%halt.~%"))
  (excl::run-shell-command "cd /eo/e1/vm2/vitADT/lib/Vit_Adt;/opt/quintus/bin3.2/sun4-5/prolog < ~/tmp/vitcheck" :output "~/tmp/vitout" :if-output-exists :supersede :error-output "~/tmp/viterror" :if-error-output-exists :supersede)
  (excl::run-shell-command "tail +56 ~/tmp/viterror | tail -r | tail +2 | tail -r" :output stream)
  (format stream "~%"))

(defun horrible-hack-2 (vit)
  ;;; deletes any leqs which cannot be expressed correctly because
  ;;; the second argument has been stipulated to be a label
  (let* ((scope (vit-scope vit))
         (new-scope (for item in scope
                         filter
                         (let* ((args (p-term-args item)))
                           (if (or (not (eql (p-term-predicate item) 'leq))
                                   (and (car args) (cadr args)
                                        (char-equal 
                                         (elt (string (car args)) 0) #\l)
                                        (char-equal 
                                         (elt (string (cadr args)) 0) #\h)))
                               item)))))
    (setf (vit-scope vit) new-scope)
    vit))

(defun mrs-to-vit (vitrified-mrs)
  ;;; first we produce all scoped structures, using the code in mrsresolve.lsp
  ;;; we also collect up all the var structures for the handels
  (clear-temporary-dbs)
  (setf *current-vit* (make-vit))
  (let* ((unstrung-psoa (mrs-unstring-psoa vitrified-mrs))
         ; Because VIT stuff doesn't like strings.
         ; The old code assumed string types would be converted to
         ; symbols on construction of the MRS, but this 
         ; loses information
         ; this function also converts the path values in the
         ; `extra' property lists into single feature values
         (mrs-psoa (if (mrs-language '(english))
		       (time-convert-mrs-struct unstrung-psoa)
		     unstrung-psoa))
	 (binding-sets (make-scoped-mrs mrs-psoa))
	 (equalities)
	 (leqs))
    (multiple-value-bind 
        (label-vars group-list)
        (collect-all-handel-vars (psoa-liszt mrs-psoa))
  ;;; then we work out which equalities and leqs are common to
  ;;; all of the scoped structures
          (when binding-sets
              (multiple-value-setq (equalities leqs)
                (work-out-scoping-restrictions mrs-psoa (car binding-sets)))
              (for binding-set in (cdr binding-sets)
                   do
                   (multiple-value-bind (new-eqs new-leqs)
                       (work-out-scoping-restrictions mrs-psoa binding-set)
		     (setf equalities 
                           (delete-if-not   
                            #'(lambda (equality)
                                (member equality new-eqs 
                                        :test #'(lambda (new old)
                                                  (or (equal new old)
                                                      (and (eql (car new) 
                                                                (cdr old))
                                                           (eql (cdr new) 
                                                                (car old)))))))  
                            equalities))
                     (setf leqs (delete-if-not  
                                 #'(lambda (leq)
                                     (member leq new-leqs :test #'equal)) 
                                 leqs)))))
      (let* ((labels (if (assoc (var-id (psoa-handel mrs-psoa)) label-vars)
                       label-vars
                     (push (list (var-id (psoa-handel mrs-psoa))) label-vars)))
             (rels (psoa-liszt mrs-psoa))
             (scope (construct-vit-scope equalities leqs labels))
             (converted-rels (convert-mrs-rels-to-vit rels 
                                                      *current-vit* 
                                   group-list labels))
;; *group-members* is constructed as a side effect of converting rels
;; in the english grammar the 'message' is included in liszt
             (mood nil)
             (groups (construct-vit-groups *group-members*)))
        (setf (vit-utterance-id *current-vit*) (construct-segment-description
                                                mrs-psoa)
              (vit-main-condition *current-vit*) (construct-main-label 
                                                  mrs-psoa
                                                  *current-vit* mood labels)
              (vit-scope *current-vit*) (append groups scope (vit-scope *current-vit*)))
        (values *current-vit*
                binding-sets)))))

(defun german-mrs-to-vit (mrs)
  (clear-temporary-dbs)
  (setf *current-vit* (make-vit))
  (multiple-value-bind
      (label-vars group-list)
      (collect-all-handel-vars (if (psoa-message mrs)
                                   (cons (psoa-message mrs) (psoa-liszt mrs))
                                 (psoa-liszt mrs)))
    (let* ((labels (if (assoc (var-id (psoa-handel mrs)) label-vars)
                       label-vars
                     (push (list (var-id (psoa-handel mrs))) label-vars)))
                                             
;           (inst-vars (collect-all-instance-vars (psoa-liszt mrs)))
           (rels (psoa-liszt mrs))
           (converted-rels (convert-mrs-rels-to-vit rels 
                                                    *current-vit* 
                                                    group-list labels))
           (scope (convert-vit-scope (psoa-h-cons mrs) *current-vit* labels))
           (mood (if (and (psoa-message mrs) 
                          (not (member (rel-sort (psoa-message mrs))
                                       *vm-ignored-sentence-mood*)))
                     (first (convert-mrs-rel-to-vit (psoa-message mrs)
                                                    *current-vit*
                                                    group-list labels))))
           (groups (construct-vit-groups *group-members*)))
      (setf (vit-utterance-id *current-vit*) 
        (construct-segment-description mrs)
            
            (vit-main-condition *current-vit*) 
            (construct-main-label mrs
                                  *current-vit* mood labels)
            (vit-scope *current-vit*) (append groups scope 
                                              (vit-scope *current-vit*)))
      *current-vit*)))

;;; for VM sid comes from the parser and is stored in *segment-id*
(defun construct-segment-description (mrs)
  (make-p-term :predicate "vitID"
               :args (list (if *segment-id*
                               *segment-id*
                             (let ((lang (case *mrs-for-language*
                                           (german '(ge syntaxger))
                                           (english '(en syntaxeng))
                                           (japanese '(jp syntaxjap))
                                           (t '(de syntaxger)))))
                               (make-sid :sourcelanguage (first lang)
                                         :currentlanguage (first lang)
                                         :sender (second lang))))
                           (collect-whg-id-labels (psoa-wgliszt mrs)))))

(defun collect-whg-id-labels (wg-liszt)
  (if wg-liszt
      (let ((res nil))
         (loop for wg in wg-liszt
             do
               (let* ((id (whg-id-id wg))
                      (labels (loop for l in (whg-id-handel wg)
                                  collect
                                    (let ((hand (if *used-handel-labels* 
                                                    (rassoc (var-id l) 
                                                        *used-handel-labels*))))
                                      (if hand
                                          (intern (format nil "L~A" 
                                                          (first hand)))
                                        (intern (format nil "L~A" (var-id l)))))))
                      (existp (find id res :key #'whg-id-id)))
                 (if existp
                     (setf (whg-id-handel existp)
                       (union (whg-id-handel existp) labels))
                   (push (make-whg-id :id id
                                      :word (whg-id-word wg)
                                      :handel labels) res))))
         res)))
          
;;; we can add an leq/eq-constraint mainlabel < tophandel if we like
;;; at present the tophandel is regarded as a label instead as a hole
;;; we also treat sentence mood (message) here
(defun construct-main-label (mrs vit mood labels)
  (let* ((tophandel (convert-mrs-val-to-vit (psoa-top-h mrs) labels))
         (mainlabel (convert-mrs-val-to-vit (psoa-handel mrs) labels))
         (index (convert-mrs-val-to-vit (psoa-index mrs) labels))
         (tophole tophandel)
;         (leq (make-p-term :predicate 'leq
;                           :args (list mainlabel tophandel)))
         )
    (cond ((and mood (= (length (p-term-args mood)) 2))
;           (setf tophole (get-group-of-label (first (p-term-args mood))
;                                             *group-members*))
           (push mood (vit-semantics vit)))
;;          ((consp (vit-semantics vit))
;;           (setf tophole (first (p-term-args (first (vit-semantics vit))))))
          (t nil ; (setf tophole 
             ; (intern (format nil "H~A" (funcall *variable-generator*))))
          ))
    (make-p-term :predicate 'index
                 :args (list tophole mainlabel index))))

;;; for German I remove leq's with holes in first position (they should
;;; correspond to unbound holes for which no condition exists)
;;; logically there is no need to treat this but I guess the vitADT would
;;; complain at present
(defun convert-vit-scope (hcons vit labels)
  (loop for leq in hcons
      nconc
        (when (leq-sc-p leq)
          (let* ((relation (remove-name-suffix 
                           (leq-sc-relation leq) "_sc"))
                 (labelarg (convert-mrs-val-to-vit (leq-sc-scarg leq)
                                                   labels))
                 (handelarg (if (string= relation "eq")
                                       (convert-handel-to-vit 
                                        (leq-sc-outscpd leq) 
                                        labels)
                                     (convert-handle-to-hole
                                      (leq-sc-outscpd leq) vit
                                      labels))))
            (if (and (eq (elt (symbol-name labelarg) 0) #\H)
                     (mrs-language '(german)))
                nil
              (list (make-p-term :predicate relation
                                 :args (list labelarg handelarg))))))))

(defun get-vit-instance-from-rel (pterm)
  ;; assume that instance is always second arg for ordinary relations
  (second (p-term-args pterm)))

;;; ******* conversion of relations *********
;;; this is very simple at the moment and will have to be modified in a number of ways
;;; WK: rels can fill several vit-slots: semantics,syntax,discourse,sorts
(defun convert-mrs-rels-to-vit (rels vit groups labels)
    (loop for rel in rels 
        do
          (let* ((relation (rel-sort rel))
                 (special (assoc relation *special-type-treatment*)))
            (cond ((member relation *do-not-convert-sort-list*) nil)
                  (special (funcall (rest special) rel vit groups labels))
                  (t (setf (vit-semantics vit) 
                       (append (vit-semantics vit)
                               (convert-mrs-rel-to-vit rel vit groups labels))))))))

;; Modify convert-mrs-rel-to-vit to allow for special roles like DIM which are
;; treated Parsons-style with a shared label, but which do not include the
;; instance variable of the predicate they come from.  Might also be used for
;; stat_rel if we decide to include EVENT role in predicative PPs, APs, etc.

(defun convert-mrs-rel-to-vit (rel vit groups labels)
  ;;; returns a list, to allow for splitting of relations into multiple p-terms
  ;;; e.g. for verbs
  (let* ((label (convert-label-to-vit (rel-handel rel) 
                                      groups 
                                      labels 
                                      (rel-label rel)))
         (args (collect-args-and-values-from-rel rel))
         (pred (make-p-term :predicate 
               (get-vit-predicate-name (rel-sort rel))
               :args 
               (cons label
                     (loop for val in (second args)
                          collect
                           (convert-mrs-val-to-vit val labels)))))
         (inst (get-vit-instance-from-rel pred))
         (semantics 
          (cons pred
                (loop for arg in (first args)
                                 ;; arg is (argN . value)
                    collect
                      (make-p-term :predicate
                                   (first arg)
                                   :args (if (member (first arg) 
						     *no-inst-arg-roles*)
					     (list label
                                               (convert-mrs-val-to-vit 
                                                (rest arg)
                                                labels))
					   (list label
						 inst
						 (convert-mrs-val-to-vit 
						  (rest arg)
						  labels))))))))
    (convert-mrs-var-extra (second args) vit inst groups labels)
    (when (rel-extra rel)
      (convert-mrs-rel-extra (rel-extra rel) vit inst label groups labels))
    (when *relation-type-check*
      (convert-relation-type-info-to-vit rel vit inst label))
    semantics))


;;; the group/label/hole-distinction needs reworking
(defun convert-mrs-val-to-vit (val labels)
  ;;; ignoring groups for the moment
  ;;; for variables this is simply a matter of providing the correct
  ;;; letter - apparently this is
  ;;; h for holes
  ;;; l for labels
  ;;; i for everything else
  ;;; For conjunctions at least, we may have lists of variables
  ;;; we may also have constants
  (cond ((listp val)
         (for subval in val
              collect (convert-mrs-val-to-vit subval labels)))
        ((var-p val)
         (cond ((is-handel-var val)
                (convert-handel-to-vit val labels))
               ((coord-var-p val)
                (convert-mrs-val-to-vit 
                 (fvpair-value 
                  (find 'list (var-extra val) :key #'fvpair-feature))
                 labels))
               (t (intern (format nil "I~A" (var-id val))))))
        ((is-top-type val) nil)
        (t val)))

(defun convert-handel-to-label (val)
  (if (var-p val)
      (intern (format nil "L~A" (var-id val)))))

(defun convert-handel-to-vit (val labels)
   (if (assoc (var-id val) labels)
     (intern (format nil "L~A" (var-id val)))
     (intern (format nil "H~A" (var-id val)))))

(defun convert-label-to-vit (val groups labels &optional (label nil))   
  (let* ((id (var-id val))
         (lh-pair (if (and label (var-p label))
                      (find (cons id (var-id label)) labels :test #'equal)
                    (assoc id labels))))
;   (unless lh-pair
;     (cerror "Treat as label anyway" "Val ~A is supposed to be a label but is not on label list" val))
   (if (member id groups)
       (let ((new-var (cond ((and label (rest lh-pair))
                             (intern (format nil "L~A" (rest lh-pair))))
                            (t (intern 
                                (format nil "L~A" 
                                        (funcall *variable-generator*))) )))
           (previous-members (assoc id *group-members*)))
         (if previous-members
             (pushnew new-var (cdr previous-members))
           (push (list id new-var) *group-members*))
         new-var)
     (progn (if label
                (push lh-pair *used-handel-labels*))
            (intern (format nil "L~A" id))))))

;;; since the VIT specification forbids that leq(l1,l2)
;;; we introduce an additional eq(l1,h2)
(defun convert-handle-to-hole (var vit labels)
  (let* ((id (var-id var))
         (hole (intern (format nil "H~A" id)))
         (lab-id (assoc id labels)))
    (if lab-id
        (unless (member id *hole-label-eqs*)
          (push id *hole-label-eqs*)
          (push (make-p-term :predicate 'eq
                             :args (list (intern (format nil "L~A" id))
                                         hole))
                (vit-scope vit))))
    hole))
                 
(defun create-special-relations (val fvp inst)
  (if (and (eq (fvpair-feature fvp) *vit-sort-feature*) ; temporary hack
           (member (fvpair-value fvp) *vm-ignored-sort-list*))
      nil
    (cond ((eq (first val) t)
           (list (funcall (p-symbol 'make- (first (rest val)))
                          :instance inst
                          :args (list (fvpair-value fvp)))))
          ((eq (first val) 'others)
           (list (funcall (p-symbol 'make- (first (first (rest val))))
                          :instance inst
                          :args (rest (first (rest val))))))
          ((symbolp (first (rest val)))
           (list (funcall (p-symbol 'make- (first (rest val)))
                          :instance inst
                          :args (list (fvpair-value fvp)))))
          (t (loop for form in (rest val)
                 collect
                   (funcall (p-symbol 'make- (first form))
                            :instance inst
                            :args (rest form)))))))

(defun get-transformation-table-value (fval values)
    (dolist (val values)
      (let ((key (first val)))
        (cond ((equal fval key) (return val))
              ((and (eq 'type key)
                    (or (eq (second val) fval)
                        (tdl-precedes (second val) fval)))
               (return (cons fval (cddr val))))
              ((member key '(others t)) (return val))
              (t nil)))))
              
;; now we pass throuygh the extras and use *index-feature-transform-table* to
;; find out whether there is something to do
(defun convert-mrs-var-extra (vars vit inst groups labels)
  (loop for var in vars
      do
        (when (and (var-p var) (not (member inst *vit-instances*)))
          (loop for fvp in (var-extra var)
              do
                (let ((todo (assoc (fvpair-feature fvp) 
                                   *index-feature-transform-table*)))
                  (when todo
                    (let* ((access (first (rest todo)))
                           (values (rest (rest todo)))
                           (val (get-transformation-table-value
                                 (fvpair-value fvp) values))
                           (newrels (if (and (cdr val)
					     (not (eq (car val) :AND)))
                                        (create-special-relations val fvp 
                                                                  inst))))
                      (if newrels
                          (add-rels-to-vit access newrels vit))))))
          (push inst *vit-instances*))))

;;;; Label/Inst-selection unsolved; at present used only for 'dir' taking a
;;;; label; extension of table format will be required
;;;; the routine is identical to that for var-extra
(defun convert-mrs-rel-extra (feats vit inst label groups labels)
  (when (not (member label *vit-instances*))
    (loop for fvp in feats
        do
          (let ((todo (assoc (fvpair-feature fvp) 
                             *relation-extra-transform-table*)))
            (when todo
              (let* ((access (first (rest todo)))
                     (values (rest (rest todo)))
                     (val (get-transformation-table-value
                           (fvpair-value fvp) values))
                     (newrels (if (cdr val)
                                  (create-special-relations val fvp 
                                                            label))))
                (if newrels
                    (add-rels-to-vit access newrels vit))))))
    (push label *vit-instances*)))

(defun convert-relation-type-info-to-vit (rel vit inst label)
  (let ((pred (rel-sort rel)))
    (loop for type in *relation-type-check*
        do
          (when (or (eq (first type) pred)
                    (tdl-precedes (first type) pred))
            (let* ((access (cadr type))
                   (values (cddr type))
                   (newrels (if values
                                (loop for form in values
                                    collect
                                      (funcall (p-symbol 'make- (first form))
                                               :instance label
                                               :args (rest form))))))
              (if newrels
                  (add-rels-to-vit access newrels vit)))
            (return vit)))))

 ;;; we can extend this to allow for new vitADT-accessors
(defun add-rels-to-vit (slot rels vit)
  (if rels
      (case slot
       (vit-sorts (setf (vit-sorts vit) 
                                   (append (vit-sorts vit) rels)))
       (vit-syntax (setf (vit-syntax vit) 
                     (append (vit-syntax vit) rels)))
       (vit-tenseandaspect 
        (setf (vit-tenseandaspect vit) 
          (append (vit-tenseandaspect vit)
                  rels)))
       (vit-discourse
        (setf (vit-discourse vit) 
          (append (vit-discourse vit)
                  rels)))
       (vit-semantics
        (setf (vit-semantics vit) 
          (append (vit-semantics vit)
                  rels)))
       (vit-prosody
        (setf (vit-prosody vit) 
          (append (vit-prosody vit)
                  rels)))
       (t nil))))

(defun find-group-of-label (label groups)
  (dolist (group groups)
    (if (member label (rest group))
        (return (first group)))))

;; ********* Construction of scope and of groupings ***********

(defun make-vit-handel (id labels)   
   (if (assoc id labels)
     (intern (format nil "L~A" id))
     (intern (format nil "H~A" id))))

(defun construct-vit-scope (equalities leqs labels)
  (append
   (for equ in equalities
        collect
        (make-p-term :predicate 'eq :args (list (make-vit-handel (car equ) labels) 
                                                (make-vit-handel (cdr equ) labels))))
   (for leq in leqs
        collect
        (make-p-term :predicate 'leq :args (list (make-vit-handel (car leq) labels) 
                                                (make-vit-handel (cdr leq) labels))))))

(defun construct-vit-groups (group-alist)
  (for grstr in group-alist
       append
       (let ((group (intern (format nil "L~A" (car grstr))))
             (gmembers (cdr grstr)))
       (for el in gmembers
            collect
            (make-p-term :predicate 'in_g :args (list el group))))))

(defun get-group-of-label (label group-alist)
  (dolist (group group-alist label)
    (if (member label (rest group))
        (return (intern (format nil "L~A" (first group)))))))



;;; ******* Code for finding leqs and equalities from scoped structures ********

(defparameter *leqs* nil "convenient to store leqs in a global")

(defun work-out-scoping-restrictions (mrs-psoa binding-set)
  (when binding-set
      (let* ((equalities nil)
	     (top-handel (get-var-num (psoa-handel mrs-psoa)))
	     (all-rels (psoa-liszt mrs-psoa))
	     (rels (loop for rel in all-rels
		       appending (unless (assoc (rel-sort rel)
						*vm-special-label-hack-list*)
				   (list rel)))))
  ;;; Equalities are just calculated from the bindings
  ;;; initially, don't bother to distinguish between equalities that 
  ;;; correspond to equalities in VIT and those that correspond to groupings
        (for binding in binding-set
             do
             (let ((var1 (car binding)))
               (for var2 in (cdr binding)
                    do
                    (unless (eql var1 var2)
                      (pushnew (cons var1 var2) equalities 
                               :test #'(lambda (new old)
                                         (or (equal new old)
                                             (and (eql (car new) (cdr old))
                                                  (eql (cdr new) (car old))))))))))
    ;;; collecting the leqs is more complex - it's done
    ;;; by walking down the tree for the connected MRS
        (setf *leqs* nil)
        (collect-leqs-from-rels top-handel rels binding-set nil)
        (values equalities *leqs*))))
    
(defun collect-leqs-from-rels (top-handel rel-list bindings holes-so-far)
  ;;; Given a current top-handel, find all the relations
  ;;; which are labelled with this handel (or with a label bound to this handel)
  ;;; Add leqs between each of these labels and each of the holes we've come across
  ;;; so far.  Then for each of these rels, find all their handel arguments
  ;;; add these to holes-so-far and recurse
   (let ((top-rels (for rel in rel-list
                        filter
                        (let ((relh (get-var-num (rel-handel rel))))
                          (if (is-locally-equivalent relh top-handel bindings)
                            (progn
                              (for hole in holes-so-far
                                   do
                                   (unless (eql relh hole)
                                     (pushnew (cons relh hole) *leqs* :test #'equal)))
                              rel))))))
    (for rel in top-rels
         do
         (for feat-val in (rel-flist rel)
              do     
              (let ((var (fvpair-value feat-val)))
                (if (listp var)
                  (for val in var
                       do
                       (if (is-handel-var val)
                         (collect-leqs-from-rels (get-var-num val) rel-list bindings
                                                 (cons (get-var-num val) holes-so-far))))
                  (if (is-handel-var var)
                    (collect-leqs-from-rels (get-var-num var) rel-list bindings
                                            (cons (get-var-num var) holes-so-far)))))))))

(defun is-locally-equivalent (h1 h2 bindings)
  (member h2 (get-bindings-for-handel h1 bindings)))
  

;;;; new function for converting strings in psoa to symbols

(defun mrs-unstring-psoa (psoa)
  ;;; non-destructive
  (let ((new-psoa (copy-psoa psoa)))
    (setf (psoa-message new-psoa)
          (if (rel-p (psoa-message psoa))
              (mrs-unstring-rel (psoa-message psoa))))
    (setf (psoa-liszt new-psoa)
          (for rel in (psoa-liszt psoa)
               collect
               (mrs-unstring-rel rel)))
    (setf (psoa-wgliszt new-psoa)
          (for rel in (psoa-wgliszt psoa)
               collect
               (mrs-unstring-rel rel)))
    new-psoa))

(defun mrs-unstring-rel (rel)
  (let ((new-rel (copy-rel rel)))
    (setf (rel-extra new-rel)
          (for fvp in (rel-extra rel)
               collect
               (mrs-unstring-fvp fvp)))
    (setf (rel-type new-rel)
          (mrs-unstring-value (rel-type rel))) 
    (setf (rel-sort new-rel)
          (mrs-unstring-value (rel-sort new-rel)))
    (setf (rel-flist new-rel)
          (for fvp in (rel-flist rel)
               collect
               (mrs-unstring-fvp fvp)))
    new-rel))

(defun mrs-unstring-fvp (fvp)
  (let* ((current-fvp-feature (fvpair-feature fvp))
         (vitrified-feature (last-path-feature current-fvp-feature)))
    ;;; last-path-feature is a no-op for atomic features
    ;;; but returns the last feature for paths
    (make-fvpair :feature (mrs-unstring-value (fvpair-feature fvp))
                 :value (mrs-unstring-value (fvpair-value fvp)))))

(defun mrs-unstring-value (val)
  (if (listp val)
      (for el in val
           collect
           (if (stringp el)
               (read-from-string el)
             el))
    (if (stringp val)
               (read-from-string val)
             val)))


    
    
               
    