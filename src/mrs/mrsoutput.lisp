;;; Copyright (c) 1998--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;; Creating MRS structures from results of parse
;;;
;;; Outputting MRS structures
;;;
;;; Ann Copestake (2000) - radically new cleaned-up version 
;;; removing VM clutter and historical notes

;;; now requires basemrs.lisp for structures and printing
;;; requires mrsglobals.lisp for global variables for paths etc.

(in-package :mrs)

;;; Assumptions about the grammar
;;;
;;; 1. fragmentness is indicated by *root-path* not being
;;;    *true-type*
;;;
;;; 2. There is a single `semantic' feature structure
;;; which can be used to build the MRS at the end of
;;;  *initial-semantics-path*
;;;
;;; 3. *psoa-index-path* - variable 
;;;    *psoa-liszt-path* - list of relations
;;;
;;; 4. *psoa-top-h-path* - handle variable 
;;;    *psoa-rh-cons-path* - hcons structure
;;;    not in handle-free fragment
;;;
;;; (5. *psoa-mode-path*   - mode (optional)
;;;    *psoa-info-s-path* - information structure (optional)
;;;    both removed)
;;;
;;; 6. variables
;;;    a. variable equivalence is indicated by structure sharing
;;;    (assumed to mean same lisp structure - see *named-nodes*)
;;;    b. determine-variable-type controls the letter used
;;;    to type the variable - see below
;;;    c. `extra' information associated with a variable is
;;;    built using create-index-property-list
;;;    all extra information must be represented by a path
;;;    with an atomic value terminating it
;;;    all features are valid as extras, except those which are in 
;;;    *ignored-extra-features*
;;;
;;; 7. liszt and relations
;;;    a. represented via a first/rest structure (*first-path* and
;;;    *rest-path*)
;;;    b. relation handel - *rel-handel-path* (not for hfree MRS)
;;;    c. relation predicate is either 
;;;       value of first element of *rel-name-path* 
;;;                  OR, if this doesn't exist
;;;       type of fs
;;;    d. arguments represented by features
;;;       (features ignored if member of *ignored-sem-features*)
;;;       if they are members of *value-feats* then assumed
;;;       to have a constant value - otherwise variable
;;;       features are ordered by *feat-priority-list*
;;;       this is relevant for the feature-free MRS rep
;;;
;;; 8. hcons
;;;    hcons is a list - relationship is represented by the
;;;    type of the list element - two arguments, represented
;;;    by *sc-arg-feature* and *outscpd-feature*
;;;    anything else is ignored

;;; ********************************************************
;;;
;;; Entry points and globals
;;;
;;; First necessary to retrieve the structure from the result of
;;; a parse.  The FS returned will have an initial path to get to
;;; the MRS *initial-semantics-path*
;;; Following this gets you to a psoa structure
;;; 

(defun extract-mrs (parse)
  ;;; takes whatever structure the parser returns
  (let ((fs (get-parse-fs parse))) ;; get-parse-fs is system specific
    (when (is-valid-fs fs)
      (extract-mrs-from-fs fs))))
         
(defun extract-mrs-from-fs (fs)
  (let ((sem-fs (path-value fs *initial-semantics-path*)))
    (setf *fragment-p* (is-fragment-fs fs))
    ;; *fragment-p* controls whether the scoping code is run
    (if (is-valid-fs sem-fs)
        (construct-mrs sem-fs nil))))

(defun is-fragment-fs (fs)
  ;;; grammar specific
  (and *root-path* *true-type*
       (let ((root-value (path-value fs *root-path*)))
         (if root-value 
             (not
              (eql (fs-type root-value) *true-type*))))))

(defparameter *named-nodes* nil
  "an alist so that if a feature structure representing a variable is shared, the same variable will be used each time it is encountered")

;;; variables get unique names via the variable generator
;;; which can be passed as a parameter

(defvar *restart-variable-generator* t
  "if t the variable counter is restarted for each sentence
This is set to nil for work on discourse to avoid spurious 
duplicate variables")

(defun construct-mrs (fs &optional existing-variable-generator)
  (if existing-variable-generator
      (setf *variable-generator* existing-variable-generator)
    (if *restart-variable-generator*
        (init-variable-generator)))
  (unless existing-variable-generator (setf *named-nodes* nil))
  (let ((top-h-fs (if *psoa-top-h-path*
                      (path-value fs *psoa-top-h-path*)))
        (index-fs (path-value fs *psoa-index-path*))
        (liszt-fs (path-value fs *psoa-liszt-path*))
        (h-cons-fs (if *psoa-rh-cons-path*
                       (path-value fs *psoa-rh-cons-path*))))
    (make-psoa
     :top-h (if top-h-fs
                (create-variable top-h-fs
                                 *variable-generator*)
                (create-new-handle-var *variable-generator*))
     :index (if (is-valid-fs index-fs)
                (create-variable index-fs
                                 *variable-generator*))
     :liszt (nreverse (construct-liszt liszt-fs
                                       nil
                                       *variable-generator*))
     :h-cons (nreverse (construct-h-cons h-cons-fs
                                         nil
                                         *variable-generator*)))))


;;; ***************************************************
;;;
;;; Variables
;;;
;;; ***************************************************


(defun get-var-num (var-struct)
  ;; Allow NIL argument
  (when (var-p var-struct)
    (var-id var-struct)))

(defun get-existing-variable (fs)
  (when (is-valid-fs fs)
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable (cdr existing-variable)
        nil))))

(defun create-new-handle-var (gen)
  (let* ((idnumber (funcall gen)))
    (make-handle-var 
     :type "h"
     :id idnumber)))

(defun create-variable (fs gen)
  (when (is-valid-fs fs)
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable (cdr existing-variable)
        (let* ((idnumber (funcall gen))
               (var-type (determine-variable-type fs))
               (extra (create-index-property-list fs))
               (variable-identifier (cond ((eql var-type :handle)
                                           (make-handle-var 
                                            :type var-type 
                                            :extra extra 
                                            :id idnumber))
                                          (t (make-var 
                                              :type var-type
                                              :extra extra 
                                              :id idnumber)))))
          (push (cons fs variable-identifier) *named-nodes*)
          variable-identifier)))))

(defun create-indexing-variable (fs)
  ;;; this is called when we are building an mrs structure for indexing
  (when (is-valid-fs fs)
    (let* ((idletter (determine-variable-type fs))
           (var-type (fs-type fs))
           (extra (create-index-property-list fs))
           (variable-identifier (cond ((equal idletter "h")
                                       (make-handle-var 
                                        :type var-type 
                                        :extra extra 
                                        :id :dummy))
                                      (t (make-var 
                                          :type var-type
                                          :extra extra 
                                          :id :dummy)))))
      variable-identifier)))

;;; The `extra' information on variables is represented
;;; as a structure consisting of a combination of feature
;;; (possibly consisting of a composite structure created
;;; by interposing `.' between features)
;;; and an atomic value.  We do not maintain
;;; intermediate types on the path - all relevant information
;;; must be contained in the atomic types

(defun create-index-property-list (fs &optional path-so-far)
  (when (is-valid-fs fs)
    (setf fs (deref fs)))
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if (and label-list (consp label-list))
          (loop for feat-val in label-list
               append
               (let ((new-path (cons (car feat-val) path-so-far))
                     (next-fs (cdr feat-val)))
                 (unless (member (car feat-val) *ignored-extra-features*)
                   (create-index-property-list 
                    next-fs
                    new-path))))
          (if path-so-far
              (list
               (make-extrapair 
                :feature (make-mrs-feature (reverse path-so-far))
                :value (create-type 
                        (fs-type fs)))))))))

(defun make-mrs-feature (flist)
  (if (cdr flist)
      (intern (format nil "~A~{.~A~}" (car flist) (cdr flist)) :lkb)
    (car flist)))


;;; ****************************************************
;;;
;;; LISZT construction
;;;
;;; ****************************************************


(defun construct-liszt (fs rels-list variable-generator)
  (if (is-valid-fs fs)
        (let ((label-list (fs-arcs fs)))
          (if label-list
              (let ((first-part (assoc (car *first-path*)
                                       label-list))
                    (rest-part (assoc (car *rest-path*)
                                      label-list)))
                (if (and first-part rest-part)
                    (let ((rel-struct
                           (create-rel-struct
                            (cdr first-part)
                            variable-generator nil)))
                      (when rel-struct
                        (push rel-struct
                              rels-list))
                      (construct-liszt
                       (cdr rest-part)
                       rels-list variable-generator))
                  rels-list))
            rels-list))))

;;; defined so it can also be used by the lexicon indexing
;;; code

(defun create-rel-struct (fs variable-generator indexing-p)
  ;;; indexing-p is set if this is being called when we're indexing lexical
  ;;; entries
  (if (is-valid-fs fs)
      (let* ((handel-pair (if (not indexing-p)
                              (extract-handel-pair-from-rel-fs fs)))
             (handle-var (if handel-pair
                             (create-variable
                              (cdr handel-pair)
                              variable-generator)))
             (pred (create-type (extract-pred-from-rel-fs fs)))
             (fvps (extract-fvps-from-rel-fs fs variable-generator indexing-p))
             (parameter-strings (get-fvps-parameter-strings fvps)))
        (unless (member pred *dummy-relations*)
          (make-rel :pred pred
                    :handel handle-var
                    :flist fvps
                    :parameter-strings parameter-strings)))))
;;; FIX?? flist may be wrong way round

(defun get-fvps-parameter-strings (fvps)
  (loop for fvp in fvps
        for feat = (fvpair-feature fvp)
        when (member feat *value-feats*)
        collect fvp))

(defun extract-handel-pair-from-rel-fs (fs)
  (let ((label-list (fs-arcs fs)))
    (if *rel-handel-path*
        (assoc (car *rel-handel-path*)
               label-list))))

(defun extract-pred-from-rel-fs (rel-fs)
    (let* ((label-list (fs-arcs rel-fs))
           (pred (assoc (car *rel-name-path*)
                          label-list))
           (pred-type (if pred (fs-type (rest pred)))))
      (if (and pred-type
               (not 
                (is-top-type pred-type)))
          pred-type
        (fs-type rel-fs))))

(defun extract-type-from-rel-fs (rel-fs)
  (fs-type rel-fs))

(defun extract-fvps-from-rel-fs (rel-fs variable-generator indexing-p)
  (let* ((label-list (fs-arcs rel-fs))
         (reduced-list
          (loop for fvp in label-list
              for feature = (car fvp)
              for value = (cdr fvp)
              unless (or (member feature *ignored-sem-features*)
                         (eql feature (car *rel-handel-path*))
                         (eql feature (car *rel-name-path*)))
              collect 
                (make-fvpair :feature feature
                             :value 
                             (if (member feature *value-feats*)
                                 (create-type (fs-type value))
                               (if indexing-p
                                   (create-indexing-variable value)   
                                 (create-variable
                                  value
                                  variable-generator)))))))
    (sort reduced-list #'feat-sort-func)))

(defun create-type (sort)
  ;;; currently a no-op but kept
  ;;; because we keep needing to mess around when we create types
  sort)

(defun feat-sort-func (fvp1 fvp2)
  (let ((feat1 (fvpair-feature fvp1))
         (feat2 (fvpair-feature fvp2)))
    (feat-order-func feat1 feat2)))

(defun feat-order-func (feat1 feat2)
  (let ((remlist (member feat1 *feat-priority-list*)))
    (if remlist (or (member feat2 remlist)
                    (not (member feat2 *feat-priority-list*)))
      (unless (member feat2 *feat-priority-list*)
        (string-lessp feat1 feat2)))))


;;; *******************************************************
;;;
;;; HCONS
;;;
;;; *******************************************************

(defun construct-h-cons (fs constr-list variable-generator)
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *first-path*)
                                     label-list))
                  (rest-part (assoc (car *rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-constr-struct
                           (cdr first-part)
                           variable-generator)
                          constr-list)
                    (construct-h-cons
                     (cdr rest-part)
                     constr-list variable-generator))
                constr-list))
          constr-list))))


(defun create-constr-struct (fs variable-generator)
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
             (rel (create-type (fs-type fs)))
             (scarg (assoc  *sc-arg-feature* label-list))
             (outscpd (assoc *outscpd-feature* label-list)))
        (make-hcons 
           :relation rel
           :scarg (when scarg
                    (create-variable (cdr scarg) variable-generator))
           :outscpd (when outscpd
                      (create-variable (cdr outscpd) variable-generator))))))





        

