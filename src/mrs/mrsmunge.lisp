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
             (i-handel (psoa-handel condition-spec))
             (handel (psoa-handel mrs))
             (i-top-h (psoa-top-h condition-spec))
             (top-h (psoa-top-h mrs))
             (i-index (psoa-index condition-spec))
             (index (psoa-index mrs)))
        (when i-handel
          (setf bindings
            (bindings-match (get-var-num i-handel)
                            (get-var-num handel)
                            bindings)))
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
          (i-handel (psoa-handel input-spec))
          (handel (psoa-handel mrs))
          (i-top-h (psoa-top-h input-spec))
          (top-h (psoa-top-h mrs))
          (i-index (psoa-index input-spec))
          (index (psoa-index mrs)))
      (record-munge-variable handel)
      (record-munge-variable top-h)
      (record-munge-variable index)           
      (when i-handel
        (push (cons (get-var-num i-handel) 
                    (get-var-num handel)) 
              initial-bindings))
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
        (i-message (psoa-message input-spec))
        (message (psoa-message mrs))
        (i-wgliszt (psoa-wgliszt input-spec))
        (wgliszt (psoa-wgliszt mrs))
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
        (when i-message
          (setf results
                (construct-message-results results i-message message)))                
        (when results
          (when i-wgliszt
            (setf results
                  (construct-wgliszt-results results i-wgliszt wgliszt)))
          (when results
            (construct-hcons-results results i-h-cons h-cons))))))
          
(defun construct-message-results (results i-message message)
  (for curr-res in results
       filter
       (let ((message-results 
              (match-mrs-rule-rels i-message message nil 
                                   (psoa-result-bindings curr-res)
                                   (psoa-result-constant-bindings curr-res))))
         (for message-result in message-results
              collect
              (let ((new-psoa 
                     (copy-psoa (psoa-result-matching-psoa curr-res))))
                (setf (psoa-message new-psoa) 
                      (munge-result-matching-rels message-result))
                (make-psoa-result 
                 :constant-bindings 
                 (munge-result-constant-bindings message-result)
                 :bindings (munge-result-bindings message-result)
                 :matching-psoa new-psoa))))))

(defun construct-wgliszt-results (results i-wgliszt wgliszt)              
  (for curr-res in results
       filter
       (let ((wgliszt-results 
              (match-mrs-rule-rels i-wgliszt wgliszt nil 
                                   (psoa-result-bindings curr-res)
                                   (psoa-result-constant-bindings curr-res))))
         (for wgliszt-result in wgliszt-results
              collect
              (let ((new-psoa 
                     (copy-psoa (psoa-result-matching-psoa curr-res))))
                (setf (psoa-wgliszt new-psoa) 
                      (munge-result-matching-rels wgliszt-result))
                (make-psoa-result 
                 :constant-bindings 
                 (munge-result-constant-bindings wgliszt-result)
                 :bindings (munge-result-bindings wgliszt-result)
                 :matching-psoa new-psoa))))))
                 
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
      ;(and (is-valid-type val1) (is-valid-type val2) 
      ;     (compatible-types val1 val2))
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
	    (when (get-var-num (rel-label input-rel))
             (setf local-bindings
                (bindings-match (get-var-num (rel-label input-rel))
                                (get-var-num (rel-label rel))
                                local-bindings)))
            (record-munge-variable (rel-handel rel))
            (record-munge-variable (rel-label rel))
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
        (if (and (eql (length (hcons-cands hcons)) 
                      (length (hcons-cands ihcon)))
                 (not (member hcons matching-hcons)))
            ;; if the MRS is well-formed, the hcons
            ;; is either an outscpd (in which case cands is 0)
            ;; or an is-one-of (in which case we only wnat to match
            ;; things with identical length candidates lists
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
                                          (cons ihcon matching-hcons)
                                          local-bindings
                                          constant-bindings)))
                      (when local-results
                        (setf results (append local-results results))))))
                ; else - an is-one-of constraint - may be
                ; multiple possible bindings ...
                (for new-bindings in (compatible-hcons-values
                                      (hcons-cands ihcon)
                                      (hcons-cands hcons) 
                                      local-bindings nil)
                     do
                     (let ((local-results (match-mrs-rule-hcons 
                                           (cdr remaining-hcons)
                                           hcons-list
                                           (cons hcons matching-hcons)
                                           new-bindings
                                           constant-bindings)))
                       (when local-results
                         (setf results (append local-results results))))))))))
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
                (if 
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
                             bindings))))
                    (return t)
                  (return nil)))))
      input-flist)
      bindings))


(defun compatible-hcons-values (remaining-input-varlist actual-varlist 
                                              bindings matched-vars)
  ;;; this is fairly horrible because we have to do an n-to-n match
  ;;; e.g. if we have h1 is-one-of h2 h3 in the rule
  ;;;      and        ha is-one-of hb hc in the input
  ;;; there are the following possible binding sets
  ;;; {{<1.a>, <2.b>, <3.c>}, {<1.a>, <2.c>, <3.b>}}
  ;;; However, since the hcons is checked last, we can hope that
  ;;; some of the possibilities will have been excluded already 
  (if (and (null remaining-input-varlist) bindings)
      (list bindings)
    (let ((ivar (car remaining-input-varlist))
          (results nil))
      (dolist (actual-var actual-varlist)
        (unless (member actual-var matched-vars)
          (let ((local-bindings (copy-alist bindings)))
            (setf local-bindings
                  (bindings-match
                   (get-var-num ivar)
                   (get-var-num actual-var)
                   bindings))
            (record-munge-variable actual-var)
            (when local-bindings
              (let ((local-results 
                     (compatible-hcons-values (cdr remaining-input-varlist)
                                              actual-varlist 
                                              local-bindings 
                                              (cons actual-var matched-vars))))
                (when local-results
                  (setf results (append local-results results))))))))
      results)))

              

;;; once we've matched the input, we need to remove the matching relations
;;; and to append the output, with the appropriate binding replacements

(defun alter-mrs-struct (input-structure result output-spec)
  (let ((matching-psoa (psoa-result-matching-psoa result))
        (bindings (psoa-result-bindings result))
        (constant-bindings (psoa-result-constant-bindings result)))
    (make-psoa 
     :handel (change-psoa-variable (psoa-handel input-structure)
                                   (psoa-handel output-spec)
                                   bindings)
     :top-h (change-psoa-variable (psoa-top-h input-structure)
                                  (psoa-top-h output-spec)
                                   bindings)
     :key-h (change-psoa-variable (psoa-key-h input-structure)
                                  (psoa-key-h output-spec)
                                   bindings)
     :index (change-psoa-variable (psoa-index input-structure)
                                  (psoa-index output-spec)
                                   bindings)
     :h-cons (change-psoa-hcons (psoa-h-cons input-structure)
                             (psoa-h-cons matching-psoa)
                             (psoa-h-cons output-spec)
                             bindings)
     :message (change-psoa-rel-list (psoa-message input-structure)
                             (psoa-message matching-psoa)
                             (psoa-message output-spec)
                             bindings constant-bindings)
     :wgliszt (change-psoa-rel-list (psoa-wgliszt input-structure)
                             (psoa-wgliszt matching-psoa)
                             (psoa-wgliszt output-spec)
                             bindings constant-bindings)
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
                     (setf (hcons-cands new-hcons)
                           (for var in (hcons-cands hcons)
                                collect
                                (convert-var-to-new-bindings var bindings)))
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
              (make-rel :extra nil ; rules should never specify extra,
                        :type nil  ; type or label
                        :label nil
                        :sort (make-name-in-correct-package 
                               (make-output-sort
                                (rel-sort rel) (rel-extra rel)
                                constant-bindings)))))
         (setf (rel-handel new-rel)
               (convert-var-to-new-bindings (rel-handel rel)
                                            bindings))
         (setf (rel-label new-rel)
               (convert-var-to-new-bindings (rel-label rel)
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

(defparameter *mrs-rule-input-path* '(cl-user::input))

(defparameter *mrs-rule-output-path* '(cl-user::output))

(defparameter *mrs-rule-condition-path* '(cl-user::lcondition))

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
          (let* ((variable-generator (create-variable-generator 1000))
                 (*psoa-rh-cons-path* `( ,(vsym "H-CONS")  ,(vsym "LIST")))
                 (*psoa-liszt-path* `( ,(vsym "LISZT")  ,(vsym "LIST")))
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
