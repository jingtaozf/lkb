(in-package "MRS")

;;; Retrieving lexical entries given a parse structure

(defun lexical-rel-p (rel-name)
  (gethash rel-name *relation-index*))

(defun grammar-rel-p (rel-name)
  nil)

(defun lex-rule-rel-p (rel-name)
  nil)
; ignore extraneous sources for now



(defun find-possible-lrules (rel-set)
  (declare (ignore rel-set))
  *contentless-lrs*)



#| 
Lookup algorithm

Do the lookup on the first lexical rel (i.e. one that was indexed in the
lexicon), allowing the entries found to gobble other rels that are in the MRS.
If this fails, check whether that rel could be provided by a lex-rule etc, if
not, fail completely.  If it doesn't fail, each solution will contain a list of
other relations which have also been contributed (possibly nil) or could have
been contributed.  Recurse on the rest of the relations, checking to see if a
result has already been constructed for a particular lex-id.

Given a set of lex ids, and the relations they (may) account for,
apply lexical rules and instantiate indices (possibly failing
at this point).

|#

(defstruct (base-lex)
  ;;; for intermediate structures
  ;;; main-rels etc is packaged so
  ;;; that the alternatives are not explictly expanded
  lex-id
  lex-entry
  rule-list
  main-rels
  alternative-rels
  message-rels)

(defstruct (found-lex)
  ;;; for reporting results of lexical checking
  lex-id
  inst-fs ; instantiated
  rule-list
  main-rels
  alternative-rels
  message-rels)


(defun collect-lex-entries-from-mrs (psoa)
  (let* ((all-rels (psoa-liszt psoa))
         (lex-rule-rels (for rel in all-rels filter 
                             (if (lex-rule-rel-p (rel-sort rel))
                                 rel)))
         ; specified by lexical rule
         (lexical-rels (for rel in all-rels filter 
                             (if (lexical-rel-p (rel-sort rel))
                                 rel)))
         ; specified in lexical entry
         (grammar-rels (for rel in all-rels filter 
                            (if (grammar-rel-p (rel-sort rel))
                                 rel)))
         ; specified in grammar rule or root fs
    ; these are not necessarily mutually exclusive classes
         (possibles 
          ; candidates found without getting actual lex-entry
          (find-lexical-candidates lexical-rels lex-rule-rels 
                                   grammar-rels nil)))
    (if possibles
        (let ((lrules (find-possible-lrules lex-rule-rels)))
            ;;; lexical rules are possible if they have no effect
            ;;; on semantics or if they contribute relations
            ;;; which are on the list to be accounted for         
          (for possible in possibles
               append
               ; check unification etc
               (create-instantiated-structures possible lrules))))))
                                         

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
          (format t "~%Not found ~S" initial-rel))) 
    results-so-far))

(defun find-lex-entries-from-rel (lex-rels results-so-far)
  ;;; use the first lex rel to trigger retrieval and test
  ;;; lex-rels in general are things the base lex entry MIGHT 
  ;;; also account for (i.e. all the things in the input
  ;;; MRS which MIGHT be lexical and which we haven't already
  ;;; used as triggers)
  (let* ((rel (car lex-rels))
         (rel-sort (rel-sort rel))
         (rel-params (car (get-rel-parameter-strings rel)))
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

(defun matches-rel-record (rel rec)
  (and (rel-p rel)
       (relation-record-p rec)
       (eql (rel-sort rel)
            (relation-record-relation rec))
       (equal (car (get-rel-parameter-strings rel))
              (relation-record-feature-string rec))))       


(defun get-rel-parameter-strings (rel)
  (for fvp in (rel-flist rel)
       filter
       (let ((feat
              (fvpair-feature fvp)))
         (if (member feat *value-feats*)
             (let ((val (fvpair-value fvp)))
               (if (listp val) 
                   (car val) val)))))) 

                
(defun find-candidates-from-rel (rel-name parameter-str rel)
  ;;; matching a relation
  ;;; a) relation sort matches
  ;;; b) if relation sort is special (e.g. named_rel) then
  ;;;    the special feature(s) also match
  ;;; c) any coindexation is consistent    
  ;;; only a) and b) are checked here - the coindexation
  ;;; check is done as part of the process of adding `Skolem'
  ;;; constants (below)
  (if rel-name
      (let ((matching (gethash rel-name *relation-index*)))
        (if matching
            (if (hash-table-p matching)
                (if parameter-str
                    (gethash parameter-str matching)
                  (progn 
                    (cerror "~%return all relations"  
                            "~%parameterized rel ~A without parameter string"
                            rel)
                    (let ((ids nil))
                      (maphash #'(lambda (k v)
                                   (declare (ignore k))
                                   (setf ids (nconc ids v)))
                               matching)
                      ids)))
              (progn
                (when parameter-str
                  (cerror "~%ignore parameter"
                          "~%unparameterised relation ~A has parameter ~A"
                          rel parameter-str))
                matching))))))


;;; candidates contain at least one relation but the
;;; filter rule has to ensure that any other relations are
;;; satisfied. In general, relations are allowed if they
;;; are somewhere in target-rels
;;; All rels used are recorded in the result, via the base-lex structure
;;;
;;; extras are required if they are in the LISZT of the candidate,
;;; extras are allowed if they are only in the 
;;; alternative LISZT (but we must use all or none
;;; of the alternative LISZT) or are in the MESSAGE rels
;;; (again, we need all or none)

;;; the first instance of any possible triggering relation
;;; returns all results (even if relation is duplicated)
;;; so if this happens we can return the existing result
;;; which is OK, because we use union, rather than append,
;;; to stitch results together
(defun filter-candidates (candidate-set target-rels existing-solutions)
  (for candidate in candidate-set
       filter
       (or
        (some #'(lambda (soln) 
                  (if (eql (base-lex-lex-id soln) candidate)
                      soln))
                  existing-solutions)
        (make-new-base-lex candidate target-rels))))

(defun make-new-base-lex (candidate target-rels)
  (let ((semantic-entry (gethash candidate *semantic-table*)))
    (unless semantic-entry
      (error "~%Inconsistent database? 
                   ~A has no entry in *semantic-table*" candidate))
    (let ((found-main-rels nil)
          (found-alt-rels nil)
          (found-message-rels nil)
          (ok t))
      (dolist (lex-rel (semantics-record-main-relations 
                        semantic-entry))
        ; in order found in lex entry
        (let ((found-rels
               (for target-rel in target-rels
                    filter
                    (if (matches-rel-record target-rel lex-rel)
                        target-rel))))
          (unless found-rels
            (setf ok nil)
            (return))
          (push found-rels found-main-rels)))
      (if ok
          (progn
            (dolist (lex-rel (semantics-record-alternate-relations 
                              semantic-entry))
              (let ((found-rels
                     (for target-rel in target-rels
                          filter
                          (if (matches-rel-record target-rel lex-rel)
                              target-rel))))
                (if found-rels
                    (push found-rels found-alt-rels)
                  (setf ok nil))))
            (if (or (null found-alt-rels) ; found nothing
                    ok)                   ; found everything
                (progn
                  (dolist (lex-rel 
                           (semantics-record-message-relations 
                            semantic-entry))
                    (let ((found-rels
                           (for target-rel in target-rels
                                filter
                                (if (matches-rel-record target-rel lex-rel)
                                    target-rel))))
                      (if found-rels
                          (push found-rels found-message-rels)
                        (setf ok nil))))
                  (when (or (null found-message-rels)
                            ok)
                    (make-base-lex 
                     :lex-id candidate
                 ; preserve lexical ordering of rels in LISZTs 
                     :main-rels (reverse found-main-rels)
                     :alternative-rels (reverse found-alt-rels)
                     :message-rels (reverse found-message-rels))))))))))
                                                       
         

;;; Creating FSs with appropriate instances

(defun create-instantiated-structures (lex-res lrules)
  ;;; this has to create instantiated structures and apply lexical rules
  ;;; It's not clear which should be done first, but with the
  ;;; current state of the filtering mechanism, it looks like 
  ;;; instantiating first should be best, because this is
  ;;; likely to weed out some entries without having to put
  ;;; more grammar specific stuff in the filtering code
  (let* ((lex-id (base-lex-lex-id lex-res))
         (lex-e (user::get-psort-entry lex-id))
         (base-fs (user::lex-or-psort-full-fs lex-e))
         (new-found-lex-list 
          (instantiate-semantic-indices 
           lex-id lex-e base-fs 
           (base-lex-main-rels lex-res)
           (base-lex-alternative-rels lex-res)
           (base-lex-message-rels lex-res))))
    (for new-found-str in new-found-lex-list
         collect
         (setf user::*number-of-applications* 0)
         (cons new-found-str
               (let ((res-fs-and-rules 
                      (user::try-all-lexical-rules 
                       (list (cons nil (found-lex-inst-fs new-found-str)))
                       *contentful-lrs*)))
         ;;; the ignored list - temporary measure to
         ;;; to take advantage of existing try-all-lexical-rules
                 (for pair in res-fs-and-rules
                      collect
                      (let ((lr-str (copy-found-lex new-found-str)))
                        (setf (found-lex-inst-fs lr-str) (cdr pair))
                        (setf (found-lex-rule-list lr-str) (car pair))
                        lr-str)))))))

(defun apply-possible-lexical-rules (potential-rules fs)
  ;;; ideally we would maintain a list of the lexical rules
  ;;; which could apply to a particular lex id
  ;;; rather than check each time
  ;;; but ignore this for now
  (declare (ignore potential-rules fs))
  nil)


(defun instantiate-semantic-indices (lex-id lex-e base-fs main-rels
                                            alt-rels message-rels)
;;; produces found-lex structures
;;; this won't yet work correctly for structures with no main-relations
;;; because it will allow them to have empty message/alternates as well
;;; we hack this for the moment by not allowing anything with no semantics
;;; to get through
  (let* ((main-rel-fl-strs
         (if main-rels
             (apply-rels-to-base lex-id base-fs main-rels 
                                 *main-semantics-path* :main-rels)
           (list (make-found-lex :lex-id lex-id
                                 :inst-fs base-fs
                                 :main-rels nil))))
        (alt-rel-fl-strs
         (for main-rel-fl-str in main-rel-fl-strs
              append
              (let ((inst-fs (found-lex-inst-fs main-rel-fl-str))
                    (actual-main-rels (found-lex-main-rels main-rel-fl-str)))
                (cons main-rel-fl-str
                      (apply-rels-to-base lex-id inst-fs alt-rels
                                   *external-semantics-path* :alternative-rels 
                                   actual-main-rels)))))
        (message-rel-fl-strs
         (for alt-rel-fl-str in alt-rel-fl-strs
              append
              (let ((inst-fs (found-lex-inst-fs alt-rel-fl-str))
                    (actual-main-rels (found-lex-main-rels alt-rel-fl-str))
                    (actual-alt-rels (found-lex-alternative-rels 
                                      alt-rel-fl-str)))
                (cons alt-rel-fl-str
                      (apply-rels-to-base lex-id inst-fs message-rels
                                          *message-semantics-path* 
                                          :message-rels 
                                          actual-main-rels actual-alt-rels))))))
    (for res in message-rel-fl-strs
         filter
         (if (and (null (found-lex-main-rels res))
                  (null (found-lex-alternative-rels res))
                  (null (found-lex-message-rels res)))
             nil
           res))))
    

(defun apply-rels-to-base (lex-id base-fs rel-list path slot &optional
                                  main-rels alt-rels)
  (for rel-sequence in (create-all-rel-sequences rel-list)
       filter
       (let* ((new-fs (create-liszt-fs-from-rels 
                       rel-sequence
                       path))
              (inst-fs (yadu 
                        (copy-tdfs-completely base-fs) 
                        new-fs)))
         (if inst-fs
             (let ((new-str
                    (make-found-lex :lex-id lex-id
                                    :inst-fs inst-fs
                                    slot rel-sequence)))
               (cond ((eql slot :alternative-rels)
                      (setf (found-lex-main-rels new-str) main-rels))
                     ((eql slot :message-rels)
                      (setf (found-lex-main-rels new-str) main-rels)
                      (setf (found-lex-alternative-rels new-str) alt-rels))
                     (t nil))
               new-str)))))
                      



(defun create-all-rel-sequences (rels)
  ;;; we have an ordered list of lists
  ;;; e.g. ((a b) (c) (d e)) and want to
  ;;; generate (a c d) (a c e) (b c d) (b c e)
  (if (null rels)
    nil
    (for rel in (car rels)
         append
         (let ((combinations (create-all-rel-sequences (cdr rels))))
           (if combinations
             (for combination in combinations
                  collect
                  (cons rel combination))
             (list (list rel)))))))

         

(defun create-liszt-fs-from-rels (rels sem-path)
  ;;; inverse direction to mrsoutput functions,
  ;;; here we're creating a FS from a Lisp structure
  (let ((path-list nil)
        (current-path sem-path))
    (for rel in rels
         do
         (let ((first-path (append current-path *first-path*)))
           (for unif in (create-unifs-for-rel rel first-path)
                do
                (push unif path-list))
           (setf current-path (append current-path *rest-path*))))
    (let* ((fs (process-unifications path-list))
           (wffs (if fs (create-wffs fs))))
          (if wffs (construct-tdfs wffs nil nil)))))

(defun create-unifs-for-rel (rel-str path)
  ;;; for the time being, just instantiate variables
  ;;; and ignore agreement etc
  (cons
   (make-tdl-path-value-unif (append path *rel-handel-path*)
                            (make-instance-type (rel-handel rel-str)))
   (for fvp in (rel-flist rel-str)
        collect
        (let ((feature (fvpair-feature fvp))
              (value (fvpair-value fvp)))
          (make-tdl-path-value-unif (append path (list feature)) 
                                    (if (var-p value)
                                      (make-instance-type value)
                                      value))))))




(defun make-instance-type (var-struct)
  ;;; a var structure consists of a name (which we ignore) 
  ;;; an `extra' slot and a unique id
  ;;; Handling the extra slot requires that
  ;;; we fix mrsoutput.lisp because currently this
  ;;; isn't really reversible
  ;;; JAC seems to have decided that all instance
  ;;; types should start with `%', so this function does this
       (let* ((instance nil)
             (type (var-type var-struct))
             (template (if (null type) 
                           (error "~%Need type of variable ~A" var-struct)
                           (if (listp type)
                               (car type)
                             type)))
             (number (var-id var-struct)))
         (when (stringp template)
             (setf template user::string-type*))
         ;;; probably this shouldn't happen
         (setf (get template 'user::last-number)
               number)
         (setf instance
               (intern
                 (concatenate 'string
                              "%"
                              (string template)
                              (princ-to-string number))))
         (push instance
               (get template 'user::children))
         (setf (get instance 'user::root-template)
               template)
         instance))


;;; Secondary entries
;;;
;;; An entry may have to be added to the bag without evidence in 
;;; the input semantics 
;;; case 1: "do" etc - these have to be hypothesized freely
;;; (perhaps)
;;; case 2: case marking prepositions - these are licensed by a specific
;;; verb which will explicitly mention a particular relation

;;; Root types and grammar rules
;;;
;;; A set of lexical entries may be a match for an input structure
;;; which has `extra' relations as long as these are all either
;;; in root FSs or in the C-CONT of some grammar rule 

;;; Lexical rules
;;;
;;; lexical rules which affect the semantics will have to be
;;; applied obligatorily
;;; 
;;; lexical rules which do not affect the semantics have to be
;;; applied generally 

#|
(in-package "USER")

;;; Testing lookup code.  
;;; If the following is evaluated, then the parse-tsdb-sentences
;;; code will call the fn on each parse

(defparameter *do-something-with-parse* 'check-lex-retrieval)

(defun check-lex-retrieval nil
  (time
   (for parse-res in *parse-record*
        do
        (let* ((lrules-and-entries-used (collect-parse-base parse-res))
               (mrs (car (mrs::extract-mrs (list parse-res)))))
          (setf *mrs* mrs)
          (let
               ((identified-entry-sets
                (mrs::collect-lex-entries-from-mrs mrs)))
          (mrs::output-mrs mrs 'mrs::simple)
          (let ((retrieved-ids
                 (for res in identified-entry-sets
                      collect
                      (mrs::found-lex-lex-id (car res)))))
            (format t "~%Retrieved ids ~A" retrieved-ids)
            (for id-and-rules in lrules-and-entries-used
                 do
                 (unless
                     (member (car id-and-rules) retrieved-ids)
                   (format t "~%Entry ~A not retrieved" 
                           (car id-and-rules))))))))))

;;; needs to be made more sophisticated to deal with lex rules etc

(defun quick-check-lex-retrieval nil
     (for parse-res in *parse-record*
        do
        (let* ((mrs (car (mrs::extract-mrs (list parse-res))))
               (identified-entry-sets
                (mrs::collect-lex-entries-from-mrs mrs)))
          (format t "~%~A"
          (for res in identified-entry-sets
               collect
               (for item in res
                    collect
                    (cons (mrs::found-lex-lex-id item)
                            (mrs::found-lex-rule-list item))))))))

|#

