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

(defvar *mrs2vit-version* "$Revision$")


;;; hash table for semdbs:

(defvar *vit-semdb* (make-hash-table :test #'eq))

(defstruct semdbitem
  gramrel
  vitrel
  args
  extra)

;;; I assume that args is a list of the parameters 
;;; needs extension as the information becomes available from SemDB
(defun create-db-item (arglist)
  (make-semdbitem :gramrel (first arglist)
                  :vitrel (second arglist)
                  :args (fourth arglist)
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

(defparameter *bound-vit-vars* nil
  "store bound vit-vars")

(defparameter *hack-parameters* nil
  "something to pass infos some hack might need")

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
        *used-handel-labels* nil
        *top-level-variables* nil
        *bound-vit-vars* nil
        *hack-parameters* nil))

;;; n-ary 'and', 'or' are transformend into binary ones
(defun convert-complex-types (type &optional (op nil))
  "transform possibly complex TDL-types into prolog"
  (cond ((atom type) type)
        ((consp type)
         (case (car type)
           (:and (if (> (length (rest type)) 1) 
                     (make-p-term :predicate "'&'"
                                  :args (convert-complex-types (rest type) :and))
                   (convert-complex-types (rest type))))
           (:or (if (> (length (rest type)) 1) 
                    (make-p-term :predicate "';'"
                                 :args (convert-complex-types (rest type) :or))
                  (convert-complex-types (rest type))))
           (:not (make-p-term :predicate "'~'"
                              :args (convert-complex-types (rest type))))
           (t (cons (convert-complex-types (car type))
                    (if op
                        (let ((ty (convert-complex-types 
                                   (cons op (rest type)))))
                          (if (listp ty) ty (list ty)))
                    (convert-complex-types (rest type)))))
           ))
        (t nil)))

(defun p-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args) "MRS"))

;;; rather hacky; only the indices can provide filter info
(defun valid-coord-element-p (mrsvar)
  (or (handle-var-p mrsvar)
      (and (var-p mrsvar)
	   (var-extra mrsvar)
	   (not (member (elt (var-name mrsvar) 0) '(#\v))))))

;;; after all arguments have been converted *hack-parameters* should contain a (coord Indizes Handels) entry (reversed argument order!)
(defun prepare-coordination-filter (clist labels)
  (let ((coordpat nil)
	(res nil)
	(val nil))
    (loop for ele in clist
	do
	  (setf val nil)
	  (when (valid-coord-element-p ele)
	    (setf val (convert-mrs-val-to-vit ele labels))
	    (setf res (append res (list val))))
	  (setf coordpat (append coordpat (list val))))
    (if (assoc 'coord *hack-parameters*)
	(push coordpat (rest (assoc 'coord *hack-parameters*)))
      (push (list 'coord coordpat) *hack-parameters*))
    res))

(defun filter-coordination-args (arglist)
  (let* ((inds (second arglist))
	 (hands (first arglist))
	 (filter (assoc 'coord *hack-parameters*))
	 (finds (second filter)))
    ; reset the filter:
    (setf (rest filter) nil)
    (loop for i in finds
	do
	  (if (null i)
	      (setf hands (rest hands))))
    (list hands inds)))

;;; just some hacks for known problems
(defun check-for-repair (arglist dbitem)
  (when (and dbitem (semdbitem-extra dbitem))
    (let ((class (first (semdbitem-extra dbitem))))
      (case class
	(qua (if (vit-hole-var-p (second arglist))
		 (setf (second arglist) 
		   (make-vit-label-var :id (vit-var-id 
					    (second arglist))))))
	(coord (setf (rest arglist)
		 (filter-coordination-args (rest arglist))))
	(t t)
	)))
  arglist)

(defun get-vit-predicate-name (relname dbitem)
  (if (semdbitem-p dbitem)
      (semdbitem-vitrel dbitem)
    (let* ((rel1 (if *sem-relation-suffix*
                     (remove-name-suffix relname *sem-relation-suffix*)
                   relname))
           (rel2 (if *sem-relation-prefix*
                     (remove-name-prefix rel1 *sem-relation-prefix*)
                   rel1)))
      (string-trim "*" rel2))))

(defun collect-values-from-rel (rel)
  ;;; returns the values from a relation (other than the handel)
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (for fvp in (rel-flist  rel)
       collect 
       (fvpair-value fvp)))


(defun get-arg-role (var)
  (if (var-p var)
      (cond ((mrs-language '(japanese))
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

(defun collect-args-and-values-from-rel (rel dbitem)
  ;;; returns the values from a relation (other than the handel)
  ;;; split up in those for arg-relations and normal
  ;;; note that this returns the entire structures (necessary because
  ;;; of identification of sorts of variables)
  (let ((args nil)
        (others nil)
        (dbargs (if (semdbitem-p dbitem)
                    (semdbitem-args dbitem))))
    (loop for fvp in (rel-flist rel)
             do
          (let* ((value (fvpair-value fvp))
                 (feat (fvpair-feature fvp))
                 ;;; this treats *mrs-arg-features* as default for mismatches
  ;;; between semdb and grammar:
                 (def (or (assoc feat dbargs)
                          (assoc feat *mrs-arg-features*)))
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
            (when (var-p var)
              (pushnew (cons (var-id var) 
                             (if (var-p label)
                                 (var-id label))) 
                       label-list)
              (if sp-rel
                  (let* ((arg (rel-handel rel)))
                    (when (is-handel-var arg)
                      (push (var-id arg) except-list)
                      (setf group-list (delete (var-id arg) group-list))
                      )))
                (pushnew (var-id var) group-list))))
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

;;; hack for identifying rels whose instance counts as bound
(defun vit-bind-inst-rel-p (mrsrel)
  (if (and (rel-p mrsrel) (rel-flist mrsrel))
      (or (vit-quant-rel-p mrsrel)
          (nonquantified-var-p (fvpair-value (first (rel-flist mrsrel))))
          (member (rel-sort mrsrel) *top-level-rel-types*))))

;;; use simply presence of feature BV as indicator of quantifier
(defun vit-quant-rel-p (mrsrel)
  (eq *bv-feature* (fvpair-feature (first (rel-flist mrsrel)))))

(defun vit-variable-p (var)
  (and (symbolp var)
       (member (subseq (symbol-name var) 0 2) 
               '("IH" "HH" "LH") :test #'equal)))

;;; returns list of free "individual" variables

(defun collect-unbound-vars (vit)
  (let* ((rels (vit-semantics vit))
         (free nil))
    (loop for rel in rels
        do
          (unless (member (p-term-predicate rel) *top-level-rel-types*)
            (loop for arg in (cdr (p-term-args rel))
                do
                  (when (and (vit-variable-p arg)
                             (not (member arg *bound-vit-vars* :test #'equalp)))
                    (pushnew arg free :test #'equalp)))))
    free))

(defun split-unknown-name-string (str)
  (let* ((pos1 (position #\. str))
         (pos2 (if pos1
                   (position #\. str :start (+ pos1 1)))))
    (if (and pos1 pos2)
        (let ((sampa (subseq str (+ pos1 1) pos2))
              (letters (subseq str (+ pos2 1) (- (length str) 1))))
          (values (if (> (length sampa) 0)
                      sampa
                    'none)
                  (if (> (length letters) 0)
                      letters
                    'none)))
      (values 'none 'none))))

(defun convert-unknown-name2vit (rel vit groups labels)
  (let* ((phon (find (vsym "NAMED") (rel-flist rel) :key #'fvpair-feature)))
    (when phon
      (multiple-value-bind 
       (sampa letters)
       (split-unknown-name-string (fvpair-value phon))
       (let ((vitrel (first (convert-mrs-rel-to-vit rel vit groups labels))))
         (setf (p-term-args vitrel)
           (append (subseq (p-term-args vitrel) 0 2) (list letters sampa)))
         (setf (vit-semantics vit) (cons vitrel (vit-semantics vit))))))))

;;; ***** Main code *******

;;; function to call from PAGE or LKB interface

(defun mrs-to-vit-convert (mrs-psoa &optional (standalone t) (stream t))
  (when mrs-psoa
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
              (format stream "~%Premunged form")
              (output-mrs1 mrs-psoa 'indexed stream)
              (format stream "~%Unscoped form")
              (output-mrs1 mrsstruct 'simple stream)
              (output-mrs1 mrsstruct 'indexed stream)
            ;;; then try and find sets of bindings which will give a fully scoped 
            ;;; structure, and output the results
              (show-some-scoped-structures mrsstruct binding-sets stream))
          (when (and vit standalone)
            (write-vit-pretty stream (horrible-hack-2 vit))
            (format stream "~%"))
	  (check-vit vit nil stream)
          vit))
    (let ((vit (german-mrs-to-vit mrs-psoa)))
      (when standalone
        (format stream "~%Unscoped form")
        (output-mrs1 mrs-psoa 'indexed stream))
      (when (and vit standalone)
        (write-vit-pretty stream vit)
        (format stream "~%"))
      vit))))

;; Sends output from a Unix command to an arbitrary stream.  Unlike
;; run-shell-command, this version can send the output to a CLIM
;; window.

(defun run-command (command &optional (stream *standard-output*))
  #+allegro
  (multiple-value-bind (output error process)
      (excl::run-shell-command command 
			       :input nil 
			       :output :stream 
			       :error-output :output 
			       :wait nil)
    (declare (ignore error))
    (unwind-protect
        (loop 
	    as line = (read-line output nil)
	    until (null line)
	    do (write-line line stream))
      (sys:os-wait nil process)))
  #-allegro
  (warn "function run-command needs customising for this Lisp"))


(defun check-vit (vit &optional (as-string nil) (stream *terminal-io*))
  ;;; AAC - won't work with a CLIM window, so change 
  ;;; *standard-output* to *terminal-io*
  (when (eql stream t) (setf stream *terminal-io*))
  #+(and :allegro :clim)
  (progn
   (with-open-file (vit-out "~/tmp/vitcheck" :direction :output
	                                    :if-exists :supersede)
    (format vit-out "ensure_loaded(vitADT).~%V = ")
    (if as-string 
	(format vit-out "~A" vit)
      (write-vit vit-out vit))
    (format vit-out ",vitCheck(V).~%~%halt.~%"))
   (excl::run-shell-command "cd /eo/e1/vm2/vitADT/lib/Vit_Adt;/opt/quintus/bin3.2/sun4-5/prolog < ~/tmp/vitcheck" :output "~/tmp/vitout" :if-output-exists :supersede :error-output "~/tmp/viterror" :if-error-output-exists :supersede)
   (excl::run-shell-command "tail +65 ~/tmp/viterror | tail -r | tail +2 | tail -r" :output stream :error-output "~/tmp/realerrorout" :if-output-exists :supersede :if-error-output-exists :supersede)
   (format stream "~%"))
  #-(and :allegro :clim)
  (warn "function check-vit needs customising for this Lisp"))

(defun horrible-hack-2 (vit)
  ;;; deletes any leqs which cannot be expressed correctly because
  ;;; the second argument has been stipulated to be a label
  (when (vit-p vit)
    (let* ((scope (vit-scope vit))
           (new-scope (for item in scope
                           filter
                           (let* ((args (p-term-args item)))
                             (if (or (not (eql (p-term-predicate item) 'leq))
                                     (and (car args) (cadr args)
                                          (vit-label-var-p (car args))
                                          (vit-hole-var-p (cadr args))))
                                 item)))))
      (setf (vit-scope vit) new-scope)
      vit)))

;; defines some minimal requirements on MRS for preventing bad partial analyses from breaking the system
(defun check-instantiated-mrs (mrs)
  (and (var-p (psoa-handel mrs))
       (var-p (psoa-top-h mrs))
       (var-p (psoa-index mrs))))

(defun mrs-to-vit (vitrified-mrs)
  ;;; first we produce all scoped structures, using the code in mrsresolve.lsp
  ;;; we also collect up all the var structures for the handels
  (when (check-instantiated-mrs vitrified-mrs)
    (clear-temporary-dbs)
    (setf *current-vit* (make-vit))
    (let* ((unstrung-psoa (mrs-unstring-psoa vitrified-mrs))
        ;; Because VIT stuff doesn't like strings.
        ;; The old code assumed string types would be converted to
        ;; symbols on construction of the MRS, but this 
        ;; loses information
        ;; this function also converts the path values in the
        ;; `extra' property lists into single feature values
           (mrs-psoa (if (mrs-language '(english))
                         (time-convert-mrs-struct unstrung-psoa)
                       unstrung-psoa))
           (binding-sets (unless *fragment-p* 
                           (make-scoped-mrs mrs-psoa)))
           (equalities)
           (leqs))
      (multiple-value-bind 
          (label-vars group-list)
          (collect-all-handel-vars (psoa-liszt mrs-psoa))
        (if binding-sets 
            ;;; if we're dealing with a complete sentence
            ;;; or in general a sentence that can be scoped
            ;;; we work out which equalities and leqs are common to
            ;;; all of the scoped structures
            (progn 
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
          ;;; for fragments or other cases where there are no valid scopes 
          ;;; we use whatever information we can to get
          ;;; at least some leqs and equalities
          (progn 
            (set-up-cheap-hcons mrs-psoa)
            ;; sets globals for leqs
            (setf leqs (find-cheap-leqs mrs-psoa))))
        (let* ((labels (if (or (not (var-p (psoa-handel mrs-psoa)))
                             (assoc (var-id (psoa-handel mrs-psoa)) label-vars))
                         label-vars
                         (push (list (var-id (psoa-handel mrs-psoa))) label-vars)))
               (rels (psoa-liszt mrs-psoa))
               (scope (construct-vit-scope equalities leqs labels))
               (converted-rels (convert-mrs-rels-to-vit rels 
                                                        *current-vit* 
                                                        group-list labels))
;;; *group-members* is constructed as a side effect of converting rels
;;; in the english grammar the 'message' is included in liszt
               (mood nil)
               (groups (construct-vit-groups *group-members*)))
          (declare (ignore converted-rels))
          (setf (vit-utterance-id *current-vit*) (construct-segment-description
                                                  mrs-psoa)
                (vit-main-condition *current-vit*) (construct-main-label 
                                                    mrs-psoa
                                                    *current-vit* mood labels)
                (vit-scope *current-vit*) (append groups scope (vit-scope *current-vit*)))
          (add-unbounds-to-vit *current-vit*)
          (convert-psoa-extras-to-vit (psoa-extras mrs-psoa) *current-vit* group-list labels)
          (values *current-vit*
                  binding-sets))))))

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
                     (first (convert-mrs-rel-to-vit 
			     (compute-mood-from-prosody (psoa-message mrs))
                                                    *current-vit*
                                                    group-list labels))))
           (groups (construct-vit-groups *group-members*)))
      (declare (ignore converted-rels))
      (setf (vit-utterance-id *current-vit*) 
        (construct-segment-description mrs)
        (vit-scope *current-vit*) (append groups scope 
                                          (vit-scope *current-vit*))           
            (vit-main-condition *current-vit*) 
            (construct-main-label mrs
                                  *current-vit* mood labels))
      (add-unbounds-to-vit *current-vit*)
      (convert-psoa-extras-to-vit (psoa-extras mrs) *current-vit* 
				  group-list labels)
      *current-vit*)))

;;; for VM sid comes from the parser and is stored in *segment-id*
(defun construct-segment-description (mrs)
  (let* ((whg-id-labels (collect-whg-id-labels (psoa-wgliszt mrs)))
         (first-word-id (when whg-id-labels (whg-id-id (first whg-id-labels))))
         (last-word-id (when whg-id-labels (whg-id-id (first 
                                                       (last whg-id-labels))))))
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
                                           :begintime first-word-id
                                           :endtime last-word-id
                                           :sender (second lang))))
                             whg-id-labels))))

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
                                          (make-vit-label-var 
                                           :id (first hand))
                                        (make-vit-label-var :id (var-id l))))))
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
         (mainlabel (convert-mrs-val-to-vit (if (and (psoa-key-h mrs) 
                                                     (mrs-language '(english)))
                                                (psoa-key-h mrs)
                                              (psoa-handel mrs)) labels))
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
             ; (intern (format nil "HH~A" (funcall *variable-generator*))))
             ))
    (setf *bound-vit-vars* (union (list tophole mainlabel index) 
                                  *bound-vit-vars* :test #'equalp))
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
            (if (and (vit-hole-var-p labelarg)
                     (mrs-language '(german japanese)))
                nil
              (progn (pushnew handelarg *bound-vit-vars* :test #'equalp)
                     (list (make-p-term :predicate relation
                                        :args (list labelarg handelarg)))))))))

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
  (when (rel-handel rel)
    (let* ((label (convert-label-to-vit (rel-handel rel) 
                                        groups 
                                        labels 
                                        (rel-label rel)))
           (dbitem (get-db-item (rel-sort rel)))
           (args (collect-args-and-values-from-rel rel dbitem))
           (pred (make-p-term :predicate 
                              (get-vit-predicate-name (rel-sort rel) dbitem)
                              :args 
                              (cons label
                                    (check-for-repair 
                                     (loop for val in (second args)
                                         collect
                                           (convert-mrs-val-to-vit val labels))
                                     dbitem))))
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
      (when (vit-bind-inst-rel-p rel)
        (pushnew inst *bound-vit-vars* :test #'equalp))
      (when (rel-extra rel)
        (convert-mrs-rel-extra (rel-extra rel) vit inst label groups labels))
      (when *relation-type-check*
        (convert-relation-type-info-to-vit rel vit inst label))
      semantics)))


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
                (prepare-coordination-filter
                 (fvpair-value 
                  (find 'list (var-extra val) :key #'fvpair-feature))
                 labels))
               (t (make-vit-instance-var :id (var-id val)))))
        ((is-top-type val) nil)
        (t val)))

(defun convert-handel-to-label (val)
  (if (var-p val)
      (make-vit-label-var :id (var-id val))))

(defun convert-handel-to-vit (val labels)
   (if (assoc (var-id val) labels)
       (make-vit-label-var :id (var-id val))
     (make-vit-hole-var :id (var-id val))))

(defun convert-label-to-vit (val groups labels &optional (label nil))   
  (let* ((id (var-id val))
         (lh-pair (if (and label (var-p label))
                      (find (cons id (var-id label)) labels :test #'equal)
                    (assoc id labels))))
    (unless lh-pair
      (struggle-on-error 
       "Val ~A is supposed to be a label but is not on label list" val))
   (if (member id groups)
       (let ((new-var (cond ((and label (rest lh-pair))
                             (make-vit-label-var :id (rest lh-pair)))
                            (t (make-vit-label-var
                                        :id (funcall *variable-generator*))) ))
           (previous-members (assoc id *group-members*)))
         (if previous-members
             (pushnew new-var (cdr previous-members) :test #'equalp)
           (push (list id new-var) *group-members*))
         new-var)
     (progn (if label
                (push lh-pair *used-handel-labels*))
            (make-vit-label-var :id id)))))

;;; since the VIT specification forbids that leq(l1,l2)
;;; we introduce an additional eq(l1,h2)
(defun convert-handle-to-hole (var vit labels)
  (let* ((id (var-id var))
         (hole (make-vit-hole-var :id id))
         (lab-id (assoc id labels)))
    (if lab-id
        (unless (member id *hole-label-eqs*)
          (push id *hole-label-eqs*)
          (push (make-p-term :predicate 'eq
                             :args (list (make-vit-label-var :id id)
                                         hole))
                (vit-scope vit))))
    hole))

(defun create-special-relations (val fvp inst)
  (cond ((eq (first val) t)
         (list (funcall (p-symbol 'make- (first (rest val)))
                        :instance inst
                        :args (list (convert-complex-types 
                                     (fvpair-value fvp))))))
        ((eq (first val) 'others)
         (list (funcall (p-symbol 'make- (first (first (rest val))))
                        :instance inst
                        :args (rest (first (rest val))))))
        ((eq (first (rest val)) 'call)
         (list (apply (first (first (rest (rest val))))
                      (rest (first (rest (rest val)))))))
        ((symbolp (first (rest val)))
         (list (funcall (p-symbol 'make- (first (rest val)))
                        :instance inst
                        :args (list (fvpair-value fvp)))))
        (t (loop for form in (rest val)
               collect
                 (funcall (p-symbol 'make- (first form))
                          :instance inst
                          :args (if (eq (second form) 'call)
                                    (list (apply (first (first (rest (rest
                                                                      form))))
                                                 (rest (first (rest (rest form))))))
                                  (rest form)))))))

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
  (declare (ignore groups labels))
  (loop for var in vars
      do
        (when (and (var-p var) (not (member inst *vit-instances* :test #'equalp))
		   (var-extra var))
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
  (declare (ignore inst groups labels))
  (when (not (member label *vit-instances* :test #'equalp))
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
  (declare (ignore inst))
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
    (if (member label (rest group) :test #'equalp)
        (return (first group)))))

(defun add-unbounds-to-vit (vit)
  (let ((unbound (collect-unbound-vars vit)))
    (when unbound
      (setf (vit-discourse vit)
        (append 
         (for var in unbound
           collect
           (make-p-term :predicate 'unbound
                        :args  (list var)))
         (vit-discourse vit))))))

;; ********* Construction of scope and of groupings ***********

(defun make-vit-handel (id labels)   
  (if (assoc id labels)
      (make-vit-label-var :id id)
    (make-vit-hole-var :id id)))

(defun construct-vit-scope (equalities leqs labels)
  (append
   (for equ in equalities
        collect
        (progn (pushnew (make-vit-handel (car equ) labels) *bound-vit-vars* :test #'equalp)
        (make-p-term :predicate 'eq :args (list (make-vit-handel (car equ) labels) 
                                                (make-vit-handel (cdr equ) labels)))))
   (for leq in leqs
        collect
        (progn (pushnew (make-vit-handel (cdr leq) labels) *bound-vit-vars* :test #'equalp)
               (make-p-term :predicate 'leq :args (list (make-vit-handel (car leq) labels) 
                                                (make-vit-handel (cdr leq) labels)))))))

(defun construct-vit-groups (group-alist)
  (for grstr in group-alist
       append
       (let ((group (make-vit-label-var :id (car grstr)))
             (gmembers (cdr grstr)))
	 (pushnew group *bound-vit-vars* :test #'equalp)
	 (for el in gmembers
	      collect
	      (make-p-term :predicate 'in_g :args (list el group))))))

(defun get-group-of-label (label group-alist)
  (dolist (group group-alist label)
    (if (member label (rest group))
        (return (make-vit-label-var :id (first group))))))

;;; extra hacks dispatcher: 
;;; the called function is specified in the fourth slot of *psoa-extras-paths*
;;; entries
(defun convert-psoa-extras-to-vit (extras vit groups labels)
  (if (consp extras)
      (loop for ele in extras
          do
            (funcall (fourth 
                      (assoc (fvpair-feature ele) *psoa-extras-paths*))
                     ele vit groups labels)))
  vit)

(defun convert-fvpair-to-pterm (fv groups labels)
  (declare (ignore groups))
  (make-p-term :predicate (fvpair-feature fv)
               :args (list (convert-mrs-val-to-vit (fvpair-value fv) labels))))

(defun compute-mood-from-prosody (mrsrel pmood)
  (when (rel-p mrsrel)
    (let* ((smood (rel-sort mrsrel))
	   (pslist (assoc pmood *prosodic-syntactic-mood-table*)))
      (when (and pslist (member smood (rest pslist)))
	(setf (rel-sort mrsrel) (first pslist)))))
  ;; AAC - removed unbound var
  ;; WK's code has pval instead of (first pslist)
  ;; this seems best guess
  mrsrel)

;;; ******* Code for finding leqs and equalities from scoped structures ********

(defparameter *leqs* nil "convenient to store leqs in a global")

(defun work-out-scoping-restrictions (mrs-psoa binding-set)
  (when binding-set
      (let* ((equalities nil)
	     (top-handel (get-var-num (psoa-handel mrs-psoa)))
	     (all-rels (psoa-liszt mrs-psoa))
	     (rels (loop for rel in all-rels
		       appending (list rel))))
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
        (collect-leqs-from-rels top-handel rels binding-set nil nil)
        (values equalities *leqs*))))
    
(defun collect-leqs-from-rels (top-handel rel-list bindings holes-so-far labels-so-far)
  ;;; Given a current top-handel, find all the relations
  ;;; which are labelled with this handel 
  ;;; (or with a label bound to this handel)
  ;;; Add leqs between each of these labels and each of the holes 
  ;;; we've come across
  ;;; so far.  Then for each of these rels, find all their handel arguments
  ;;; add these to holes-so-far and recurse
  (if (member top-handel labels-so-far)
      (progn (struggle-on-error "Reentrant structure?") nil)
    (let ((top-rels 
           (for rel in rel-list
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
                               (collect-leqs-from-rels 
                                (get-var-num val) rel-list bindings
                                (cons (get-var-num val) holes-so-far)
                                (cons top-handel labels-so-far))))
                    (if (is-handel-var var)
                        (collect-leqs-from-rels 
                         (get-var-num var) rel-list bindings
                         (cons (get-var-num var) holes-so-far)
                         (cons top-handel labels-so-far))))))))))

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
;    (setf (psoa-wgliszt new-psoa)
;          (for rel in (psoa-wgliszt psoa)
;               collect
;               (mrs-unstring-rel rel)))
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
    (make-fvpair :feature (mrs-unstring-value vitrified-feature)
                 :value (mrs-unstring-value (fvpair-value fvp)))))

(defun mrs-unstring-value (val)
  (if (var-p val)
      (progn
	(setf (var-extra val)
          (for fvp in (var-extra val)
               collect
               (mrs-unstring-fvp fvp)))
	val)
    (if (listp val)
	(for el in val
	     collect
	     (if (stringp el)
		 (read-from-string el)
	       el))
      (if (stringp val)
	  (read-from-string val)
	val))))
