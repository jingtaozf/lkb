(in-package :mrs)

(setf *alex-munge* t)
(setf *mrs-to-vit* nil)

(setf cl-user::*do-something-with-parse* 'show-mrs)



;;; redefine the following
(defun create-scoped-structures (top-handel bvs bindings rels scoping-handels)
  ;;; we have a top-handel (i.e. a current `top-handel' - this function
  ;;; is called recursively) and a set of rels, some of which may have the
  ;;; same handel as the top-handel.  We have to generate all possibilities
  ;;; for combinations of rels which COULD have the same handel
  ;;; bindings - current bindings
  ;;; bvs - a list of the variables which are bound by quantifiers we've
  ;;;       already dealt with (nil on first call)
  ;;; rels - rels we haven't incorporated yet (all rels on first call)
  ;;; scoping-handels - handels which will outscope the handels we're
  ;;;        about to incorporate (nil on first call) - added for outscopes
  ;;;        constraint
  (incf *scoping-calls*)
  (when (> *scoping-calls* *scoping-call-limit*)
    (throw 'up nil))
  (if rels
    ; fail immediately if we're going to be left with
    ; an unsatisfiable handel arg
    (let* ((handel-args (for rel in rels
                                    append (get-handel-args rel)))
           (impossible-top-rels 
            ; the list of rels which introduce variables which are
            ; currently free or which have a handel which is coindexed with a handel
            ; argument of one of the rels
            ; or which are `outscoped' by top-handel
            ; or which are outscoped by a handel which is not on the list
            ; of scoping-handels
            (for rel in rels
                 filter 
                 (let ((vars (collect-unbound-vars-from-rel rel)))
                   (if (or (and vars (not (subsetp vars bvs)))
                           (member (get-var-num (rel-handel rel)) handel-args)
                           (violates-outscopes-p  (get-var-num (rel-handel rel)) 
                                                  top-handel scoping-handels bindings))
                     ; violates-outscopes-p is in mrscons.lsp
                     rel))))
           (impossible-handels 
            (remove-duplicates (for rel in impossible-top-rels
                                    collect (get-var-num (rel-handel rel)))))
           (possible-top-rels 
            ; the list of rels which wouldn't introduce any free variables
            ; and which don't have sisters (i.e. rels with the same handel)
            ; which introduce free variables
            (for rel in (set-difference rels impossible-top-rels)
                 filter 
                 (unless (member (get-var-num (rel-handel rel)) impossible-handels)
                   rel)))
           (top-rels ; the list of rels which have the same handel as the
                     ; top handel
            (for rel in rels
                          filter (if (eql (get-var-num (rel-handel rel)) top-handel)
                                   rel))))
      (if (and possible-top-rels (subsetp top-rels possible-top-rels))
        ; unless there are some possible rels and all the top rels
        ; are possible, we fail
        (for rel-comb-and-bindings in 
             (make-combinations-with-is-one-ofs top-handel top-rels
                            (set-difference possible-top-rels top-rels)
                                                ; experiment for Alex
						; (if top-rels nil 
						;   possible-top-rels)
                           ;;(set-difference possible-top-rels top-rels) 
                           ;; if this third argument is set to nil
                           ;; when there are top-rels, then 
                           ;; no rels can be added to
                           ;; an existing set of relations 
                           ;; which are known to fill a hole
                           bindings)
             ;;; for each possible set of rels which can have their handels
             ;;; bound to the top-handel, generate a set of results
             append
             (sister-structure-scope 
              (car (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cdr (bindings-and-sisters-sisters rel-comb-and-bindings)) 
              bvs 
              (bindings-and-sisters-bindings rel-comb-and-bindings) 
              (set-difference 
               rels 
               (bindings-and-sisters-sisters rel-comb-and-bindings))
              (cons top-handel scoping-handels)))))))


(defun alex-munge (mrs-psoa)
  (when mrs-psoa
    (unless *ordered-mrs-rule-list*
      (error "~%No munging rules defined"))
    (let* ((mrsstruct
           (if (boundp '*ordered-mrs-rule-list*)
               (munge-mrs-struct mrs-psoa *ordered-mrs-rule-list*)
             mrs-psoa))
           (binding-sets (make-scoped-mrs mrs-psoa)))
      (setf *canonical-bindings* nil)
      (format t "~%Premunged form")
      (output-mrs mrs-psoa 'indexed)
      (format t "~%Unscoped form")
      (output-mrs mrsstruct 'indexed)
      (if binding-sets
          (format t "~%~A scoped form(s) ~A~%" (length binding-sets)
                  (if (> (length binding-sets) 10)
                      "only printing first 10" 
                    ""))
        (format t "~%WARNING: No valid scopes~%"))
      (for binding in (subseq binding-sets 0 
                              (min (length binding-sets) 10))
           do
           (setf *canonical-bindings* (canonical-bindings binding))
           (output-connected-mrs mrsstruct 'indexed)
           (output-scoped-mrs mrsstruct)))))


#|

(in-package :cl-user)

(read-mrs-rule-file-aux "~aac/grammar/data/alex.rules")

|#