;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package "MRS")

;;; Retrieving lexical entries given a parse structure

#| 
Lookup algorithm

Do the lookup on the first lexical rel (i.e. one that was indexed in the
lexicon), allowing the entries found to gobble other rels that are in the MRS.
If this fails, check whether that rel could be provided by a lex-rule etc, if
not, fail completely.  If it doesn't fail, each solution will contain a list of
other relations which have also been contributed (possibly nil).  
Recurse on the rest of the relations, checking to see if a
result has already been constructed for a particular lex-id.

Given a set of lex ids, and the relations they account for,
apply lexical rules and instantiate indices (possibly failing
at this point).

|#


;;; ************************************************
;;;
;;; Utility stuff
;;;
;;; ************************************************


;;; check whether relations are lexical or come from rules
;;; (not mutually exclusive)

(defun lexical-rel-p (rel-name)
  (gethash rel-name *relation-index*))

(defun grammar-rel-p (rel-name)
  (member rel-name *grule-rel-index* :key #'car))
  
(defun lex-rule-rel-p (rel-name)
  (member rel-name *lrule-rel-index* :key #'car))

;;; ************************************************
;;;
;;; Data structures
;;;
;;; ************************************************

(defstruct (base-lex)
  ;;; for intermediate structures
  ;;; main-rels etc is packaged so
  ;;; that the alternatives are not explictly expanded
  lex-id
  lex-entry
  rule-list
  main-rels)

(defstruct (found-lex)
  ;;; for reporting results of lexical checking
  lex-id
  inst-fs ; instantiated
  rule-list
  main-rels)

(defstruct (found-rule (:include lkb::rule))
  ;;; the rule-fs is replaced by the instantiated version
  main-rels ; for grammar rules, these will be the things in ccont
  )

;;; following fns are defined so they can be called in a file
;;; loaded before this file

(defun found-lex-lex-id-fn (item)
  (found-lex-lex-id item))

(defun found-lex-rule-list-fn (item)
  (found-lex-rule-list item))

(defun found-lex-inst-fs-fn (item)
  (found-lex-inst-fs item))

;;; *********************************************************
;;;
;;; Call point
;;;
;;; *********************************************************

(defun collect-lex-entries-from-mrs (psoa)
  (let* ((all-rels (psoa-liszt psoa))
         (lex-rule-rels (loop for rel in all-rels
                            when (lex-rule-rel-p (rel-sort rel))
                            collect rel))
         ; specified by lexical rule
         (lexical-rels (loop for rel in all-rels 
                             when (lexical-rel-p (rel-sort rel))
                             collect rel))
         ; specified in lexical entry
         (grammar-rels (loop for rel in all-rels 
                             when (grammar-rel-p (rel-sort rel))
                             collect rel))
                                        ; specified in grammar rule
    ; these are not necessarily mutually exclusive classes
         (possibles 
          ; Part 1
          ; candidates found without getting actual lex-entry
          (find-lexical-candidates lexical-rels lex-rule-rels 
                                   grammar-rels nil)))
    ;; Part 2
    (if possibles
        (let* ((lrules (find-possible-rules lex-rule-rels t))
            ;;; lexical rules are possible if they have no effect
            ;;; on semantics or if they contribute relations
            ;;; which are on the list to be accounted for
               (nullsem (instantiate-null-semantic-items psoa lrules))
            ;;; nullsem items are ones we have to postulate without
            ;;; direct evidence from the MRS
               (nonnull (loop for possible in possibles
                             append
                                        ; check unification etc
                             (create-instantiated-structures possible lrules)))
               (lexres (append nullsem nonnull)))
          (values
           lexres
           (find-possible-rules grammar-rels nil)
           (find-linear-order-spec lexres))))))

;;; third value is an ordering specification 
;;; given as a list of lex-ids 

;;; *****************************************************
;;;
;;; Ordering code (currently non-functional)
;;;
;;;  *****************************************************

(defun find-linear-order-spec (lexres)
  (declare (ignore lexres))
  nil)
#|
  (let ((ids nil)
        (found-lex-list nil))
    (loop for resbundle in lexres
         do
         (loop for foundlex in resbundle
              do
              (when (found-lex-p foundlex)
                (push foundlex found-lex-list)
                (pushnew (found-lex-lex-id foundlex) ids))))
    (mapcar
            #'(lambda (ordering)
                (mapcar
                   #'(lambda (id)
                       (remove-if-not #'(lambda (x) (eq x id)) found-lex-list
                                      :key #'mrs::found-lex-lex-id))
                   ordering))
            (find-partial-orders ids))))
|#
            
(defun find-partial-orders (ids)
  (declare (ignore ids))
  nil)
;;;  '((PROJECT_N1 MANAGER_N1)))


;;; ***********************************************************
;;;
;;; Initial access of lexical ids from semdb
;;;
;;; ***********************************************************


(defun find-lexical-candidates (lex-rels lex-rule-rels grammar-rels
                                results-so-far)
  ;;; results-so-far is an list of base-lex 
  ;;; structures 
  (if lex-rels
      (let* ((initial-rel (car lex-rels))
             (initial-match 
              (find-lex-entries-from-rel lex-rels
                                         results-so-far)))
        (if (or initial-match 
                (member initial-rel lex-rule-rels)
                (member initial-rel grammar-rels))
         ; if we've found a matching rel, or an alternative
         ; source, then we recurse on the rest of the rels
            (find-lexical-candidates (cdr lex-rels)
                                     lex-rule-rels grammar-rels
                                     (union initial-match
                                             results-so-far))
          ; union, because initial-match can include things
          ; we've found already
          ; else - generation fails
          (format t "~%Not found ~S" (rel-sort initial-rel))))
    results-so-far))

(defun find-lex-entries-from-rel (lex-rels results-so-far)
  ;;; use the first lex rel to trigger retrieval.
  ;;; lex-rels in general are things the base lex entry MIGHT 
  ;;; also account for (i.e. all the things in the input
  ;;; MRS which MIGHT be lexical and which we haven't already
  ;;; used as triggers)
  (let* ((rel (car lex-rels))
         (rel-sort (rel-sort rel))
         (rel-params (get-rel-parameter-strings rel))
         (initial-candidates (find-candidates-from-rel
                              rel-sort rel-params rel))
        ;;; candidates are things which have the  
        ;;; relevant relation
         (candidates
              (filter-candidates initial-candidates lex-rels
                                 results-so-far)))
        ;;; filter-candidates removes any entries which have
        ;;; stuff which doesn't show up in the lex-rels.
        ;;; It returns a list of base-lex structures.
        ;;; Entries we have already found are returned 
        ;;; as eq to current members of results-so-far.
        ;;; Potentially there are other things it could do
        ;;; to exclude possibilities on the basis of the other 
        ;;; relations, but we ignore that for now
        candidates))

;;; get-rel-parameter-strings
;;; matches-rel-record
;;; find-candidates-from-rel - moved to lexindex.lisp

  ;;; matching a relation
  ;;; a) relation sort matches
  ;;; b) if relation sort is special (e.g. named_rel) then
  ;;;    the special feature(s) also match
  ;;; c) any coindexation is consistent    
  ;;; only a) and b) are done in find-candidates-from-rel 
  ;;; the coindexation
  ;;; check is done as part of the process of adding `Skolem'
  ;;; constants (below)


;;; candidates contain at least one relation but the
;;; filter rule has to ensure that any other relations are
;;; satisfied. In general, relations are allowed if they
;;; are somewhere in target-rels
;;; All rels used are recorded in the result, via the base-lex structure

;;; the first instance of any possible triggering relation
;;; returns all results (even if relation is duplicated)
;;; so if this happens we can return the existing result
;;; which is OK, because we use union, rather than append,
;;; to stitch results together

(defun filter-candidates (candidate-set target-rels existing-solutions)
  (loop for candidate in candidate-set
      nconc
        (let ((existing
               (dolist (soln existing-solutions)
                  (when (eql (base-lex-lex-id soln) candidate)
                      (return soln)))))
          (if existing
              (list existing)
            (let ((new
                   (make-new-base-lex candidate target-rels)))
              (if new 
                  (list new)))))))

(defun make-new-base-lex (candidate target-rels)
  ;;; having found a candidate, access the
  ;;; database that is indexed by id in order to check it
  (let ((semantic-entry (gethash candidate *semantic-table*)))
    (unless semantic-entry
      (error "~%Inconsistent database? 
                   ~A has no entry in *semantic-table*" candidate))
    (let ((found-main-rels nil)
          (ok t))
      (dolist (lex-rel (semantics-record-relations 
                        semantic-entry))
        ; in order found in lex entry
        (let ((found-rels
               (loop for target-rel in target-rels
                    when (matches-rel-record target-rel lex-rel)
                    collect target-rel)))
          (unless found-rels
            (setf ok nil)
            (return))
          (push found-rels found-main-rels)))
      (if (and ok found-main-rels)
          ;;; empty semantics things are done elsewhere
          (make-base-lex 
           :lex-id candidate
           ;; preserve lexical ordering of rels in LISZTs 
           :main-rels (reverse found-main-rels))))))
                                                       
;;; **************************************************         
;;;
;;; Creating FSs with appropriate instances
;;;
;;; **************************************************         

(defvar *number-of-lrule-applications* 0)

(defun create-instantiated-structures (lex-res lrules)
  ;;; this has to create instantiated structures and apply lexical rules
  ;;; It's not clear which should be done first, but with the
  ;;; current state of the filtering mechanism, it looks like 
  ;;; instantiating first should be best, because this is
  ;;; likely to weed out some entries without having to put
  ;;; more grammar specific stuff in the filtering code
  (let* ((lex-id (base-lex-lex-id lex-res))
         (lex-e (lkb::get-psort-entry lex-id))
         (base-fs (lkb::lex-or-psort-full-fs lex-e))
         (new-found-lex-list 
          (instantiate-semantic-indices 
           lex-id lex-e base-fs 
           (base-lex-main-rels lex-res))))
    (loop for new-found-str in new-found-lex-list
         collect
         (apply-instantiated-rules-base new-found-str lrules))))


(defun apply-instantiated-rules-base (new-found-str lrules)
  (setf *number-of-lrule-applications* 0)
  (cons new-found-str
        (let ((res-fs-and-rules 
               (apply-instantiated-lexical-rules 
                (list (cons nil 
                            (found-lex-inst-fs new-found-str)))
                lrules)))
          (loop for pair in res-fs-and-rules
               nconc
               (let ((lr-str (copy-found-lex new-found-str))
                     (ok t))
                 (dolist (rule (car pair))
                   (when (not ok) (return))
                   (when (found-rule-p rule)
                     (dolist (main-rel (found-rule-main-rels rule))
                       (when (member main-rel
                                         (found-lex-main-rels lr-str))
                         (setf ok nil)
                         (return))
                       (push main-rel (found-lex-main-rels lr-str)))))
                 (setf (found-lex-inst-fs lr-str) (cdr pair))
                 (setf (found-lex-rule-list lr-str) 
                   (mapcar #'lkb::rule-id (car pair)))
                 (if ok
                     (list lr-str)))))))


(defun apply-instantiated-lexical-rules (entries rules)
  ;; similar to try-all-lexical-rules, but rule list is given, because rules
  ;; may have instantiated semantics.  entries are pairs with list of rules
  ;; applied plus result
  (incf *number-of-lrule-applications*)
  (when (> *number-of-lrule-applications* 
	   lkb::*maximal-lex-rule-applications*)
    (error "~%Probable circular lexical rule"))
  (let ((transformed-entries 
	 (loop for entry in entries
	     append
	       (let* ((fs (cdr entry))
		      (fs-restricted (lkb::restrict-fs (tdfs-indef fs))))
		 (loop for rule in rules
		      nconc
		      (let* ((spelling-rule-p 
			      (lkb::spelling-change-rule-p rule))
			     (new-morph 
			      (when spelling-rule-p
				(lkb::construct-new-morph entry rule)))
			     (result
			      (when (or (not spelling-rule-p) new-morph)
				;; allow morphographemics to block generation
				(lkb::apply-morph-rule 
				 rule fs fs-restricted new-morph))))
			(if result
                            (list
                             (cons (cons rule (car entry)) result)))))))))
    (when transformed-entries
      (append transformed-entries
	      (apply-instantiated-lexical-rules transformed-entries rules)))))

(defun instantiate-semantic-indices (lex-id lex-e base-fs main-rels)
;;; produces found-lex structures
  (declare (ignore lex-e))
  (unless main-rels
    (error "~%~A has no main relations - instantiate-semantic-indices
            should not be called"))
  (apply-rels-to-base lex-id base-fs main-rels 
                           *main-semantics-path*))

(defun apply-rels-to-base (lex-id base-fs rel-list path)
  (loop for rel-sequence in (create-all-rel-sequences rel-list)
      when
       ;; needs fixing - unnecessary expense since we repeat this on the same
       ;; rels for multiple ids
	(let ((new-fs (create-liszt-fs-from-rels rel-sequence path)))
	  (if new-fs 
	      (let* (; (lkb::*unify-debug* t)
                     (result (yadu base-fs new-fs)))
		(if result
		    (if lex-id
			(make-found-lex 
			 :lex-id lex-id
			 :inst-fs result
			 :main-rels rel-sequence)
		      ;; if null lex-id assume it's a rule
		      (cons result rel-sequence))))
	    (cerror "Ignore this entry/rule" 
		    "~%Problem in create-liszt-fs-from-rels")))
	collect it))

(defun create-all-rel-sequences (rels)
  ;;; we have an ordered list of lists
  ;;; e.g. ((a b) (c) (d e)) and want to
  ;;; generate (a c d) (a c e) (b c d) (b c e)
  (if (null rels)
      nil
    (loop for rel in (car rels)
	nconc
	  (let ((combinations (create-all-rel-sequences (cdr rels))))
	    (if combinations
		(loop for combination in combinations
		    collect (cons rel combination))
	      (list (list rel)))))))

(defun match-rels (a b)
  (and (equal (second a)
	      (second b))
       (loop for x in (first a)
	   for y in (first b)
	   always (eq x y))))

;;; ********************************************************
;;;
;;; Actual fs construction (subpart of instantiation code)
;;;
;;;  ********************************************************


(defun create-liszt-fs-from-rels (rels sem-path)
  ;; inverse direction to mrsoutput functions, here we're creating a FS from a
  ;; Lisp structure
  (let* ((path-list nil)
	 (current-path sem-path))
    (loop for rel in rels
	do
	  (let ((first-path (append current-path *first-path*)))
	    (loop for unif in (create-unifs-for-rel rel first-path)
		do
		  (push unif path-list))
	    (setf current-path (append current-path *rest-path*))))
    (let* (; (lkb::*unify-debug* t)
           (fs (process-unifications path-list))
	   (wffs (when fs (create-wffs fs)))
	   (tdfs (when wffs (construct-tdfs wffs nil nil))))
      tdfs)))

(defun create-unifs-for-rel (rel-str path)
  (let ((handel-unif (if (rel-handel rel-str)
                         (make-pv-unif (append (append path *rel-handel-path*)
                                               *instloc-path*)
                                       (make-instance-type 
                                        (rel-handel rel-str)))))
        (other-unifs
         (loop for fvp in (rel-flist rel-str)
              append
              (let* ((feature (fvpair-feature fvp)) ; should be a symbol
                     (value (fvpair-value fvp))
                     (new-path (append path (list feature))))
                (if (var-p value)
                    (cons
                     (make-pv-unif (append new-path *instloc-path*)
                                   (make-instance-type value))
                     (lkb::make-mrs-unifs (var-extra value) new-path))
                  (list (make-pv-unif new-path
                                      value)))))))
  (if handel-unif (cons handel-unif other-unifs)
      other-unifs)))

(defun make-instance-type (var-struct)
  ;;; a var structure consists of a name (which we ignore) 
  ;;; an `extra' slot and a unique id
  ;;; We do the `extra' slot stuff in create-unifs-for-rel
  ;;; JAC seems to have decided that all instance
  ;;; types should start with `%', so this function does this
  ;;; 
  ;;; modified because all instance types are now stored on the
  ;;; *instloc-path* in the variable fs
       (let* ((instance nil)
             (template *instloc-type*)
             (number (var-id var-struct)))
         (setf (get template 'lkb::last-number)
               number)
         (setf instance
               (intern
                 (concatenate 'string
                              "%"
                              (string template)
                              (princ-to-string number)) :lkb))
         (push instance
               (get template 'lkb::children))
         (setf (get instance 'lkb::root-template)
               template)
         instance))

;;; *******************************************************       
;;;
;;; Secondary entries
;;;
;;; An entry may have to be added to the bag without evidence in 
;;; the input semantics 

;;; null semantics lexical entries - including the
;;; result of all the lexical rule applications.

(defun instantiate-null-semantic-items (input-sem lrules)
  (let* ((real-ids (if lkb::*gen-rule-list*
                       (genpredict-mrs-struct input-sem 
                                              lkb::*gen-rule-list*)
                     (if *null-semantics-hack-p*
                         (let ((found-list 
                                (apply #'append 
                                       (lkb::retrieve-lex-from-parses))))
                           (loop for empty in *empty-semantics-lexical-entries*
                                when (member empty found-list)
                                collect empty))
                       *empty-semantics-lexical-entries*)))
        (instantiated-sets
          (loop for lex-id in real-ids
               nconc
               (let ((lex-e (lkb::get-psort-entry lex-id)))
                 (if lex-e
                   (let*
                      ((base-fs (lkb::lex-or-psort-full-fs lex-e))
                       (new-found-str
                        (make-found-lex 
                         :lex-id lex-id :inst-fs base-fs))
                       (res (apply-instantiated-rules-base
                             new-found-str lrules)))
                     (if res (list res)))
                   (progn 
                     (format t 
                             "~%Warning: invalid generation rule --- ~A does not exist" lex-id)
                     nil))))))
    instantiated-sets))

;;; ********************************************
;;;
;;; Rules
;;;
;;; ********************************************

;;; Lexical rules
;;;
;;; lexical rules which affect the main semantics have to be
;;; applied obligatorily
;;; 
;;; lexical rules which do not affect the semantics have to be
;;; applied generally 
;;; 

;;; Grammar rules
;;;
;;; Similar to lexical rules, but they are passed to the parser
;;; rather than applied here


(defun find-possible-rules (rel-set lexicalp)
  (append 
   (loop for rule-record in (if lexicalp *contentful-lrs* *contentful-grs*)
        append
        (let ((rule (if lexicalp 
                        (lkb::get-lex-rule-entry 
                         (semantics-record-id rule-record))
                      (lkb::get-grammar-rule-entry 
                       (semantics-record-id rule-record))))
              (main-rels
               (semantics-record-relations rule-record)))
          (let ((rel-list
                 (loop for main-rel-rec in main-rels
                      collect
                      (let ((matching-rels
                             (loop for rel in rel-set
                                  when
                                   (matches-rel-record rel main-rel-rec)
                                  collect rel)))
                        (unless matching-rels (return nil))
                        matching-rels))))
            (if rel-list
                (loop for rel-comb-and-fs in 
                     (apply-rels-to-base nil (lkb::rule-full-fs rule)
                                         rel-list *construction-semantics-path*)
                     collect
                     (make-new-found-rule rule (car rel-comb-and-fs)
                                          (cdr rel-comb-and-fs)))
              (if (null main-rels)
                  (list (make-new-found-rule 
                         rule (lkb::rule-full-fs rule)
                         nil)))))))
  (if lexicalp *contentless-lrs* *contentless-grs*)))


(defun make-new-found-rule (rule new-fs rels)
  (make-found-rule
   :id (lkb::rule-id rule)
   :language (lkb::rule-language rule)
   :unifs (lkb::rule-unifs rule)
   :def-unifs (lkb::rule-def-unifs rule)
   :full-fs new-fs
   #+:packing :rtdfs #+:packing (lkb::copy-tdfs-partially new-fs)
   :daughters-restricted
   (lkb::rule-daughters-restricted rule)
   :daughters-restricted-reversed
   (lkb::rule-daughters-restricted-reversed rule)
   :daughters-apply-order 
   (lkb::rule-daughters-apply-order rule)
   :order (lkb::rule-order rule)
   :rhs (lkb::rule-rhs rule)
   :daughters-order-reversed
   (lkb::rule-daughters-order-reversed rule)
   :apply-filter
   (lkb::rule-apply-filter rule)
   :apply-index
   (lkb::rule-apply-index rule)
   :main-rels rels))



