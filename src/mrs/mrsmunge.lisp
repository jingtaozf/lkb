(in-package "MRS")

;;; Two sorts of functionality:
;;; 1) convert from one MRS to another according to a set of rules which
;;; specify transformations on the MRS
;;; 2) triggering rules for interaction with discourse stuff - e.g. recognition;;;  when MRS is a question, concerns a meeting etc
;;;
;;; Assumption
;;; a) operate on potentially underspecified forms
;;; b) if rules feed eachother, the rule writer will specify them in the 
;;; correct order

(defstruct (mrs-munge-rule)
  id
  input-spec
  input-condition
  output-spec)

;;; input spec, input condition and output spec are psoas, 
;;; with variables specified appropriately
;;; If the input spec (and any specified input-conditions) match
;;; then the input-spec elements are deleted, and replaced with the 
;;; output-spec elements

(defstruct (mrs-trigger-rule)
  id
  input-condition
  output-parameters)

;;; input condition is as above
;;; output-parameters is a list of parameter value pairs

;;; for now, condition is a LISZT of rels, but eventually
;;; might want boolean e.g.

#|

( AND 
     [ meet_rel  
       EVENT: e     
       ACT : x
       UND : y ]

   ( NOT 
     [ yesterday_rel
       ARG: e ] ))
|#

(defstruct (action-condition)
  feat
  value)

(defun get-appropriate-constant (action-conditions feature)
  (dolist (ac action-conditions)
    (let ((ac-feat (action-condition-feat ac))
          (ac-val (action-condition-value ac)))
    (when (and
           (or (and (null feature) (null ac-feat))
               (same-names feature ac-feat))
           (mrs-rule-constant-p ac-val))
      (return (mrs-rule-constant-value ac-val))))))


;;; utility structure - for passing results around

(defstruct (munge-result)
  matching-rels
  bindings
  constant-bindings)

(defstruct (psoa-result)
  matching-psoa
  bindings
  constant-bindings)

(defparameter *original-variables* nil)

(defun record-munge-variable (var)
  (if (var-p var)
      (let ((id (get-var-num var)))
        (unless (assoc id *original-variables*)
          (push (cons (get-var-num var) var)
                *original-variables*)))))

(defun look-up-munge-variable (var-id)
  (cdr (assoc var-id *original-variables*)))


(defun munge-mrs-struct (mrsstruct rules)
  ;;; takes an mrs structure and a set of rules
  ;;; converts the mrs structure according to the rules, in order
  ;;; i.e. the output of one rule may feed the input of another.
  ;;; Rules are applied in order and are not applied recursively
  (setf *original-variables* nil)
  (dolist (rule rules)
    (let ((results
           (match-mrs-rule mrsstruct 
                           (mrs-munge-rule-input-spec rule))))
;;;      (when results
;;;            (display-mrs-rule rule))
      (dolist (result (remove-overlapping-psoas results))
        (let ((new-results 
               (matches-input-condition 
                mrsstruct result  
                (mrs-munge-rule-input-condition rule))))
          (when new-results
            (when (cdr new-results)
                (unless *giving-demo-p*
                  (cerror "Ignore extras" 
                          "~%Ambiguous results")))
            (setf (psoa-result-bindings result)
              (psoa-result-bindings (car new-results)))
            (setf mrsstruct
              (alter-mrs-struct mrsstruct result
                                (mrs-munge-rule-output-spec 
                                 rule))))
;;;        (output-mrs mrsstruct 'indexed)
          ))))
   mrsstruct)


(defun remove-overlapping-psoas (results)
  (when results
    (let ((ok nil)
          (interim results))
      (loop
        (unless interim (return))
        (let ((initial (car interim))
                  (rest (cdr interim)))
              (setf interim nil)
              (for thing in rest
                   do
                   (if (overlapping-psoas initial thing)
                       (unless *giving-demo-p*
                         (cerror "Ignore extras" 
                                 "~%Overlapping ambiguous results"))
                     (push thing interim)))
              (push initial ok)))
      ok)))

(defun overlapping-psoas (res1 res2)
  (intersection
   (psoa-liszt (psoa-result-matching-psoa res1))
   (psoa-liszt (psoa-result-matching-psoa res2))))


(defun matches-input-condition (mrs result condition-spec) 
  (if (null condition-spec)
      (list result)
      (let* ((bindings (copy-alist (psoa-result-bindings result)))
             (i-top-h (psoa-top-h condition-spec))
             (top-h (psoa-top-h mrs))
             (i-index (psoa-index condition-spec))
             (index (psoa-index mrs)))
        (when i-top-h
          (setf bindings
            (bindings-match (get-var-num i-top-h)
                            (get-var-num top-h)
                            bindings)))
        (when i-index
          (setf bindings
            (bindings-match (get-var-num i-index) 
                            (get-var-num index) bindings)))
        (match-input-condition-rest mrs condition-spec bindings))))

(defun match-input-condition-rest (mrs input-spec initial-bindings)
  (let ((results nil)
        (i-liszt (psoa-liszt input-spec))
        (liszt (psoa-liszt mrs))
        (i-h-cons (psoa-h-cons input-spec))
        (h-cons (psoa-h-cons mrs)))
    (setf results
      (if i-liszt
          (for int-res in 
               (match-mrs-rule-rels i-liszt liszt nil initial-bindings nil)
               collect
               (make-psoa-result 
                :bindings (munge-result-bindings int-res)
                :constant-bindings (munge-result-constant-bindings int-res)
                :matching-psoa                
                (make-psoa :liszt (munge-result-matching-rels int-res))))
        (list (make-psoa-result 
               :bindings initial-bindings
               :matching-psoa                
               (make-psoa)))))
    (when results
      (construct-hcons-results results i-h-cons h-cons))))

(defun match-mrs-rule (mrs input-spec)
  ;;; first match top-h etc, if specified, in order to produce
  ;;; bindings
  ;;; 
    (let ((initial-bindings nil)
          (i-top-h (psoa-top-h input-spec))
          (top-h (psoa-top-h mrs))
          (i-index (psoa-index input-spec))
          (index (psoa-index mrs)))
      (record-munge-variable top-h)
      (record-munge-variable index)           
      (when i-top-h
        (push (cons 
               (get-var-num i-top-h) 
               (get-var-num top-h)) 
              initial-bindings))
      (when i-index
        (push (cons (get-var-num i-index) 
                    (get-var-num index)) initial-bindings))
      (match-mrs-rule-rest mrs input-spec initial-bindings)))

(defun match-mrs-rule-rest (mrs input-spec initial-bindings)
  (let ((results nil)
        (i-h-cons (psoa-h-cons input-spec))
        (h-cons (psoa-h-cons mrs))
        (i-liszt (psoa-liszt input-spec))
        (liszt (psoa-liszt mrs)))
      (setf results
            (if i-liszt
                (for int-res in 
                     (match-mrs-rule-rels i-liszt liszt nil initial-bindings nil)
                   collect
                   (make-psoa-result 
                    :bindings (munge-result-bindings int-res)
                    :constant-bindings (munge-result-constant-bindings int-res)
                    :matching-psoa                
                    (make-psoa :liszt (munge-result-matching-rels int-res))))
              (list (make-psoa-result 
                     :bindings initial-bindings
                     :matching-psoa                
                     (make-psoa)))))
      (when results
        (construct-hcons-results results i-h-cons h-cons))))
          
(defun construct-hcons-results (results i-h-cons hcons)
  (for curr-res in results
       append
       (let ((hcons-results 
              (match-mrs-rule-hcons i-h-cons hcons nil 
                                    (psoa-result-bindings curr-res)
                                    (psoa-result-constant-bindings curr-res))))
         (for hcons-result in hcons-results
              collect
              (let ((new-psoa 
                     (copy-psoa (psoa-result-matching-psoa curr-res))))
                (setf (psoa-h-cons new-psoa) 
                      (munge-result-matching-rels hcons-result))
                (make-psoa-result 
                 :constant-bindings 
                 (munge-result-constant-bindings hcons-result)
                 :bindings (munge-result-bindings hcons-result)
                 :matching-psoa new-psoa))))))

(defun compatible-types-or-values (val1 val2)
  (or (is-top-type val1) (is-top-type val2)
      (and (is-valid-type val1) (is-valid-type val2) 
           (compatible-types val1 val2))
      (cond ((and (symbolp val1) (symbolp val2))
	     (same-names val1 val2))
	    ((and (stringp val1) (stringp val2))
	     (equal val1 val2))
	    ((and (stringp val1) (symbolp val2))
	     (equal val1 (symbol-name val2)))
	    ((and (stringp val2) (symbolp val1))
	     (equal val2 (symbol-name val1)))
	    (t (equal val1 val2)))))

(defun same-names (sym1 sym2)
  ;;; avoid package problems
  (equal (symbol-name sym1) (symbol-name sym2)))

(defun find-constant-values (extra rel constant-bindings)
  (let ((new-bindings nil))
     (for ac in extra
          do
          (let ((ac-feat (action-condition-feat ac))
                (ac-val (action-condition-value ac)))
            (when (mrs-rule-constant-p ac-val)
              (let ((rel-val (if (null ac-feat)
                                 (rel-sort rel)
                               (dolist (fvp (rel-flist rel))
                                 (when (same-names (fvpair-feature fvp) ac-feat)
                                   (return (fvpair-value fvp)))))))
                (when rel-val
                  (push (cons (mrs-rule-constant-value ac-val)
                              rel-val)
                        new-bindings))))))
  (append new-bindings constant-bindings)))

(defun match-mrs-rule-rels (remaining-rels rels matching-rels bindings 
                            constant-bindings)
  ;;; remaining-rels is the list of things in the rule,
  ;;; rels is the list of rels in the relevant part of the input MRS.
  ;;; Each function call attempts to match the top remaining-rel
  ;;; with the input MRSs.
  ;;; The function always takes one set of matching-rels and of bindings
  ;;; but the result may be a set, because we may have multiple
  ;;; matches for a particular relation
  (if (null remaining-rels)
      ; normally this will be the end condition
      ; but it allows rules to be written which always fire,
      ; effectively allowing material to be appended to a LISZT
      ; without anything being deleted
      (list (make-munge-result :matching-rels matching-rels
                               :constant-bindings constant-bindings
                               :bindings bindings))
    (let ((input-rel (car remaining-rels))
          (results nil))
      (dolist (rel rels)
        (when (and (compatible-types-or-values (rel-sort input-rel)
                        (rel-sort rel))
                   (not (member rel matching-rels)))
          ; conditions such as predicates should be checked here
          (let ((local-bindings (copy-alist bindings)))
            (setf local-bindings
                (bindings-match (get-var-num (rel-handel input-rel))
                                (get-var-num (rel-handel rel))
                                local-bindings))
            (record-munge-variable (rel-handel rel))
            (when local-bindings
              (setf local-bindings
                 (compatible-values
                     (rel-flist input-rel)
                     (rel-flist rel)
                     local-bindings))
              (when local-bindings
              ; constant-values are also local to a particular solution
                (setf constant-bindings
                  (find-constant-values (rel-extra input-rel) rel
                                        constant-bindings))
              ; locally successful match, so we assume this
              ; condition is checked off, and continue with
              ; the rest of the conditions
                (let ((local-results (match-mrs-rule-rels 
                                      (cdr remaining-rels)
                                      rels
                                      (cons rel matching-rels)
                                      local-bindings
                                      constant-bindings)))
                  (when local-results
                              ; all conditions satisfied
                    (setf results (append local-results results)))))))))
        results)))

(defun match-mrs-rule-hcons (remaining-hcons hcons-list 
                             matching-hcons bindings 
                             constant-bindings)
  ; similar to above, but for hcons
  (if (null remaining-hcons)
      (list (make-munge-result :matching-rels matching-hcons
                               :bindings bindings 
                               :constant-bindings constant-bindings))
    (let ((ihcon (car remaining-hcons))
          (results nil))
      (dolist (hcons hcons-list)
        (if (not (member hcons matching-hcons))
          (let ((local-bindings (copy-alist bindings)))
            (setf local-bindings
                (bindings-match (get-var-num (hcons-scarg ihcon))
                                (get-var-num (hcons-scarg hcons))
                                local-bindings))
            (record-munge-variable (hcons-scarg hcons))
            (when local-bindings
              (if (and (hcons-outscpd ihcon) (hcons-outscpd hcons))
                  ; outscpd constraint
                (progn
                  (setf local-bindings
                        (bindings-match (get-var-num (hcons-outscpd ihcon))
                                        (get-var-num (hcons-outscpd hcons))
                                        local-bindings))
                  (record-munge-variable (hcons-outscpd hcons))
                  (when local-bindings
                    (let ((local-results (match-mrs-rule-hcons 
                                          (cdr remaining-hcons)
                                          hcons-list
                                          (cons hcons matching-hcons)
                                          local-bindings
                                          constant-bindings)))
                      (when local-results
                        (setf results (append local-results results)))))))))))
        results)))

           
(defun bindings-match (input-var actual-var bindings)
  ;;; assumption is that we are only concerned with
  ;;; matching a single input variable with a single mrs variable
  (let ((existing-match (assoc input-var bindings)))
    (if existing-match 
        (if (eql (cdr existing-match) actual-var)
            bindings)
      (progn (push (cons input-var actual-var) bindings)
             bindings))))

(defun compatible-values (input-flist actual-flist bindings)
  (if
     (every
      #'(lambda (input-fvpair)
          (dolist (actual-fvpair actual-flist)
            (if (same-names (fvpair-feature input-fvpair)
                     (fvpair-feature actual-fvpair))
                (if (and (match-var-extras (fvpair-value input-fvpair)
                                           (fvpair-value actual-fvpair))
                    (if 
                        (member
                         (fvpair-feature input-fvpair)
                         *value-feats* :test #'same-names)
                        (compatible-types-or-values 
                         (fvpair-value input-fvpair)
                         (fvpair-value actual-fvpair))
                      (progn
                        (record-munge-variable (fvpair-value actual-fvpair))
                        (setf bindings 
                            (bindings-match
                             (get-var-num (fvpair-value input-fvpair))
                             (get-var-num (fvpair-value actual-fvpair))
                             bindings)))))
                    (return t)
                  (return nil)))))
      input-flist)
      bindings))

(defun match-var-extras (value1 value2)
  (if (and (var-p value1) (var-p value2))
      (let ((type1 (var-type value1))
            (type2 (var-type value2))
            (extra1 (var-extra value1))
            (extra2 (var-extra value2)))
        (and
         (if (and type1 type2 
                  (is-valid-type type1) 
                  (is-valid-type type2))
             (compatible-types type1 type2)
           t)
        (if (and extra1 extra2)
            (compatible-extra-vals extra1 extra2)
            t)))
      t))


;;; once we've matched the input, we need to remove the matching relations
;;; and to append the output, with the appropriate binding replacements

(defun alter-mrs-struct (input-structure result output-spec)
  (let ((matching-psoa (psoa-result-matching-psoa result))
        (bindings (psoa-result-bindings result))
        (constant-bindings (psoa-result-constant-bindings result)))
    (make-psoa 
     :top-h (change-psoa-variable (psoa-top-h input-structure)
                                  (psoa-top-h output-spec)
                                   bindings)
     :index (change-psoa-variable (psoa-index input-structure)
                                  (psoa-index output-spec)
                                   bindings)
     :h-cons (change-psoa-hcons (psoa-h-cons input-structure)
                             (psoa-h-cons matching-psoa)
                             (psoa-h-cons output-spec)
                             bindings)
     :liszt (change-psoa-rel-list (psoa-liszt input-structure)
                             (psoa-liszt matching-psoa)
                             (psoa-liszt output-spec)
                             bindings constant-bindings))))

(defun change-psoa-variable (existing-var new-var bindings)
  (if new-var
      (convert-var-to-new-bindings new-var bindings)
    existing-var))

(defun change-psoa-hcons (old-hcons matching-hcons new-hcons-specs bindings)
  (if (or matching-hcons new-hcons-specs)
      (append (set-difference old-hcons matching-hcons)
              (for hcons in new-hcons-specs
                   collect
                   (let ((new-hcons
                          (copy-hcons hcons)))
                     (setf (hcons-scarg new-hcons)
                           (convert-var-to-new-bindings (hcons-scarg hcons)
                                                        bindings))
                     (setf (hcons-outscpd new-hcons)
                           (if (hcons-outscpd hcons)
                               (convert-var-to-new-bindings 
                                (hcons-outscpd hcons) bindings)))
                     new-hcons)))
    old-hcons))



(defun change-psoa-rel-list (old-rels matching-rels new-rel-specs 
                             bindings constant-bindings)
  (if (or matching-rels new-rel-specs)
      (append (set-difference old-rels matching-rels)
              (change-rel-bindings new-rel-specs bindings 
                                   constant-bindings))
    old-rels))

(defun make-name-in-correct-package (sym)
    (vsym (symbol-name sym)))

(defun make-value-in-package (value)
  (if (symbolp value)
      (make-name-in-correct-package value)
      value))

(defun change-rel-bindings (new-rel-specs bindings constant-bindings)
  (for rel in new-rel-specs
       collect
       (let ((new-rel
              (make-rel :extra nil ; rules should never specify extra
               :sort (make-name-in-correct-package 
                      (make-output-sort
                       (rel-sort rel) (rel-extra rel)
                       constant-bindings)))))
         (setf (rel-handel new-rel)
               (convert-var-to-new-bindings (rel-handel rel)
                                            bindings))
         (setf (rel-flist new-rel)
               (sort
                (for fvpair in (rel-flist rel)
                     collect
                     (make-fvpair :feature 
                                  (make-name-in-correct-package 
                                   (fvpair-feature fvpair))
                                  :value
                                  (if 
                                      (member (fvpair-feature fvpair) 
                                              *value-feats* 
                                              :test #'same-names)
                                      (make-value-in-package
                                       (make-output-value
                                        (fvpair-value fvpair)
                                        (fvpair-feature fvpair)
                                        (rel-extra rel)
                                        constant-bindings))
                                    (convert-var-to-new-bindings 
                                     (fvpair-value fvpair)
                                     bindings))))
                #'feat-sort-func))
         new-rel)))


(defun make-output-sort (rel-spec extra constant-bindings)
  (let* ((constant (get-appropriate-constant extra nil))
         (constant-match (if constant
                             (cdr (assoc constant constant-bindings)))))
    (or constant-match rel-spec)))

(defun make-output-value (val-spec feat extra constant-bindings)
  (let* ((constant (get-appropriate-constant extra feat))
         (constant-match (if constant
                             (cdr (assoc constant constant-bindings)))))  
    (or constant-match val-spec)))
  

(defun convert-var-to-new-bindings (variable bindings)
  (if (var-p variable)
      (let* ((old-var-id (get-var-num variable))
             (existing-binding (assoc old-var-id bindings))
             (original-variable (if existing-binding
                                    (look-up-munge-variable
                                        (cdr existing-binding))))
             (new-var (if original-variable
                          (let ((unchanged-var
                                 (copy-var original-variable)))
                            (if (var-extra variable)
                                (setf (var-extra unchanged-var)
                                      (var-extra variable)))
                            unchanged-var)
                        (copy-var variable))))
        new-var)
    variable))


;;;

(defun eval-mrs-rule-exp (var-alist exp)
  (setf exp (sublis var-alist exp))
  (eval exp))

;;;


;;; Given an mrs rule expressed as a FS, convert it to
;;; the internal format, which uses the same structures
;;; as general MRSs

(defparameter *mrs-rule-input-path* '(lkb::input))

(defparameter *mrs-rule-output-path* '(lkb::output))

(defparameter *mrs-rule-condition-path* '(lkb::lcondition))

(defun construct-munge-rule-from-fs (id fs funny-unifs)
  ;;; input and output are constructed using construct-mrs
  ;;; with a given variable-generator
  (declare (ignore id))
  (let ((input-funny (collect-funny-unifs funny-unifs *mrs-rule-input-path*))
        (output-funny (collect-funny-unifs funny-unifs *mrs-rule-output-path*))
        (condition-funny (collect-funny-unifs funny-unifs 
                                              *mrs-rule-condition-path*))
        (input-fs (path-value fs *mrs-rule-input-path*))
        (output-fs (path-value fs *mrs-rule-output-path*))
        (condition-fs (path-value fs *mrs-rule-condition-path*)))
      (if (and input-fs output-fs)
          (let* ((variable-generator (create-variable-generator 10000))
                 (input-spec (construct-mrs input-fs variable-generator))
                 (output-spec (construct-mrs output-fs variable-generator))
                 (condition-spec 
                  (if condition-fs
                      (construct-mrs condition-fs variable-generator))))
            (when (and input-spec output-spec)
              (add-funny-stuff input-spec input-funny)
              (add-funny-stuff output-spec output-funny)
              (when condition-spec
                  (add-funny-stuff condition-spec condition-funny))
              (make-mrs-munge-rule 
               :input-spec input-spec
               :output-spec output-spec
               :input-condition condition-spec))))))
    

;;; This is a bit grubby, because I want to use the standard code
;;; for constructing an mrs, but then add in any extra stuff which
;;; may have been specified 
;;; For now, the only sort of extra stuff is a binding variable for
;;; a constant: either a value (e.g. a string) or a type, typically
;;; used so these can be copied from input to output
;;; The code identifies the rel to which the funny stuff belongs and
;;; makes use of the extra slot to store it

(defun collect-funny-unifs (funny-unifs initial-path)
  (for funny-unif in funny-unifs
       filter
       (let ((path (funny-unification-lhs funny-unif))
             (initial-path-length (length initial-path)))
         (unless (> initial-path-length (length path))
           (if (equal (subseq path 0 initial-path-length) 
                                      initial-path)
               (make-funny-unification 
                :lhs (subseq path initial-path-length)
                :rhs (funny-unification-rhs funny-unif)))))))


(defun add-funny-stuff (mrs extra)
  ;; destructively modifies the relations
  (for funny-unif in extra
       do
       (let* ((path (funny-unification-lhs funny-unif))
              (real-path (cddr path))
              (liszt (psoa-liszt mrs)))
         (unless (and (> (length path) 2)
                      (eql (car path)  (vsym "LISZT"))
                      (eql (cadr path)  (vsym "LIST")))
           (struggle-on-error "~A is not a valid path in add-funny-stuff" path))
         (multiple-value-bind (rel rel-feat)
             (find-relevant-rel liszt real-path)
           (when rel
             (push
              (make-action-condition :feat rel-feat
                                     :value (funny-unification-rhs funny-unif))
              (rel-extra rel)))))))

(defun find-relevant-rel (liszt path)
  (when (and liszt path)
    (if (eql (car path) 'FIRST)
        (if (cddr path)
            (struggle-on-error "Too many components ~A in path" (cdr path))
          ; should be single feature or nil
          (values (car liszt) (cadr path)))
      (if (eql (car path) 'REST)
          (find-relevant-rel (cdr liszt) (cdr path))
        (struggle-on-error "Unexpected component ~A in path" (car path))))))
               
         
       


;;; Display a rule

(defun display-mrs-rule (mrs-rule)
  (format t "~%~A" (mrs-munge-rule-id mrs-rule))
  (output-mrs (mrs-munge-rule-input-spec mrs-rule)  'simple)
  (format t "~%---->")
  (output-mrs (mrs-munge-rule-output-spec mrs-rule)  'simple))

;;; Invert rules for VIT to MRS

(defun invert-munge-rules (rules)
  (let ((result nil)) 
    (for rule in rules
         do
         (push (make-mrs-munge-rule
                :id (intern (concatenate 'string 
                              (string 
                               (mrs-munge-rule-id rule))
                              "-inv"))
                :input-spec (mrs-munge-rule-output-spec rule)
                :input-condition (mrs-munge-rule-input-condition rule)
                :output-spec (mrs-munge-rule-input-spec rule))
               result))
    ;;; we want an inverted result
    result))

        