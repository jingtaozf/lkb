;;; Creating MRS structures from results of parse
;;;
;;; Outputting MRS structures
;;;
;;; Ann Copestake (2000) - radically new cleaned-up version 
;;; removing VM clutter and historical notes

;;; now requires basemrs.lisp for structures and printing
;;; requires mrsglobals.lisp for global variables for paths etc.
(in-package :mrs)

(defun remove-trailing-periods (sentence-string)
  (string-right-trim '(#\Space #\.) sentence-string))

(defvar *synlabel* nil
  "Syntactic label - used for fragments, may be worth retaining")

;;; First necessary to retrive the structure from the result of
;;; a parse.  The FS returned will have an initial path to get to
;;; the MRS *initial-semantics-path*
;;; Following this gets you to a psoa structure
;;; 

(defun extract-mrs (parse &optional generator-p)
  (setf *fragment-p* nil)
  (setf *synlabel* nil)
  (let* ((fs (get-parse-fs parse))
         ;; get-parse-fs also sets *fragment-p*
         ;; which controls whether the scoping code is run
         ;;
         ;; the synlabel mechanism, apparently, is no longer required; ann
         ;; says she wants to look into this sometime  (28-mar-00  -  oe)
         ;;
         (synlabel #+:vm (get-category-label parse) nil)
         (sem-fs (path-value fs *initial-semantics-path*)))
    (if (is-valid-fs sem-fs)
        (progn
          (setf *synlabel* synlabel)
          (construct-mrs sem-fs nil generator-p)))))

(defun is-fragment-fs (fs)
  (and *root-path* *false-type*
  (let ((root-value (path-value fs *root-path*)))
    (if root-value 
        (not
         (eql (fs-type root-value) *true-type*))))))
  

(defparameter *named-nodes* nil)
(defvar *restart-variable-generator* t
    "if t the variable counter is restarted for each sentence")

(defun construct-mrs (fs &optional existing-variable-generator generator-p)
  (declare (ignore generator-p))
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
     :top-h (create-variable top-h-fs
                             *variable-generator*)
     :index (if (is-valid-fs index-fs)
                (create-variable index-fs
                                 *variable-generator*))
     :liszt (nreverse (construct-liszt liszt-fs
                                       nil
                                       *variable-generator*))
     :h-cons (nreverse (construct-h-cons h-cons-fs
                                         nil
                                         *variable-generator*)))))

;; Allow NIL argument to get-var-num
(defun get-var-num (var-struct)
  (when (var-p var-struct)
    (var-id var-struct)))

(defun create-variable (fs gen &optional type)
  ;; optional type argument allows for
  ;; the case where PAGE doesn't type the top-handel
  ;; as a handel
  (when (is-valid-fs fs)
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable (cdr existing-variable)
        (let* ((idletter (determine-variable-type fs))
               (idnumber (funcall gen))
               (variable-name (format nil "~A~A" idletter idnumber))
               (var-type (or type (fs-type fs)))
               (extra (create-index-property-list fs))
               (variable-identifier (cond ((equal idletter "h")
                                           (make-handle-var 
                                            :name variable-name
                                            :type var-type 
                                            :extra extra 
                                            :id idnumber))
                                          (t (make-var 
                                              :name variable-name 
                                              :type var-type
                                              :extra extra 
                                              :id idnumber)))))
          (push (cons fs variable-identifier) *named-nodes*)
          variable-identifier)))))

;;; The `extra' information on variables is now represented
;;; as a structure consisting of a combination of feature
;;; (possibly consisting of a composite structure created
;;; by interposing `.' between features)
;;; and an atomic value.  We no longer attempt to maintain
;;; intermediate types on the path - all relevant information
;;; must be contained in the atomic types

(defun create-index-property-list (fs &optional path-so-far)
  (when (is-valid-fs fs)
    (setf fs (deref fs)))
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if (and label-list (consp label-list))
          (for feat-val in label-list
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

#+lkb
(defun determine-variable-type (fs)
  (let ((type (create-type (fs-type fs))))
    (cond ((equal-or-subtype type *event-type*) "e")
          ((equal-or-subtype type *conj-ind-type*) "e")
          ((equal-or-subtype type *ref-ind-type*) "x")
          ((equal-or-subtype type *full_ref-ind-type*) "x")
          ((equal-or-subtype type *deg-ind-type*) "d")
          ((equal-or-subtype type *non_expl-ind-type*) "v")
          ((equal-or-subtype type *event_or_index-type*) "e")
          ((equal-or-subtype type *eventtime-type*) "t")
          ((equal-or-subtype type *handle-type*) "h")  
          ((equal-or-subtype type *hole-type*) "h")
          ((equal-or-subtype type *label-type*) "h")
          ;((equal-or-subtype type *individual-type*) "d")
          ((equal-or-subtype type *difference-list-type*) "c") 
          ;; Assume coordination structure
          (t "v"))))


(defun construct-liszt (fs rels-list variable-generator)
  (if (is-valid-fs fs)
        (let ((label-list (fs-arcs fs)))
          (if label-list
              (let ((first-part (assoc (car *first-path*)
                                       label-list))
                    (rest-part (assoc (car *rest-path*)
                                      label-list)))
                (if (and first-part rest-part)
                    (progn
                      (push (create-rel-struct
                             (cdr first-part)
                             variable-generator)
                            rels-list)
                      (construct-liszt
                       (cdr rest-part)
                       rels-list variable-generator))
                  rels-list))
            rels-list))))

(defun create-rel-struct (fs variable-generator)
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
             (handel-pair (if *rel-handel-path*
                              (assoc (car *rel-handel-path*)
                                 label-list)))
             (pred (assoc (car *rel-name-path*)
                          label-list))
             (pred-type (if pred (fs-type (rest pred))))
             (rel nil))
        (setf rel (make-rel :sort
                            (create-type (if (and pred-type
                                                  (not 
                                                   (is-top-type pred-type)))
                                             pred-type
                                            (fs-type fs)))
                            :handel (if handel-pair
                                        (create-variable
                                         (cdr handel-pair)
                                         variable-generator))))
        (loop for feat-val in 
              (sort (remove pred 
                            (remove handel-pair label-list))
                    #'feat-sort-func)
            do
              (let ((feature (car feat-val)))
                (unless (member feature *ignored-sem-features*)
                  (setf (rel-flist rel) 
                    (cons (make-fvpair :feature feature
                                       :value 
                                       (if (member (car feat-val)
                                                   *value-feats*)
                                           (create-type
                                            (fs-type (cdr feat-val))) 
                                         (create-variable
                                          (cdr feat-val)
                                          variable-generator)))
                          (rel-flist rel))))))
        (setf (rel-flist rel) (reverse (rel-flist rel)))
        rel)))


#+lkb
(defun create-type (sort)
  (horrible-hack-3 sort))

(defun horrible-hack-3 (sort)
  ;;; this hack would not be necessary if the grammar used
  ;;; a persistent default.  It converts a type of the form
  ;;; Xrel_a into Xrel.  This is needed for preposition
  ;;; fragments because abstract relations get removed
  ;;; by munging rules.  This would be cleaner if someone would
  ;;; define the PAGE interface consistently - as it is, we have to
  ;;; rely on the typographic convention
  (if (and (symbolp sort)
	   (not (numberp sort)))
    (let* ((str (string sort))
           (start-pos (- (length str) 6)))
      (if (and (> start-pos 0)
	       (string-equal (subseq str start-pos) "_rel_a"))
	  (intern (subseq str 0 (+ start-pos 4)) :lkb)
        sort))
    sort))

(defun feat-sort-func (fvp1 fvp2)
  (let* ((feat1 (if (fvpair-p fvp1) 
                    (fvpair-feature fvp1)
                    (car fvp1)))
         (feat2 (if (fvpair-p fvp2) 
                    (fvpair-feature fvp2)
                    (car fvp2)))
         (remlist (member feat1 *feat-priority-list*)))
    (if remlist (or (member feat2 remlist)
                    (not (member feat2 *feat-priority-list*)))
      (unless (member feat2 *feat-priority-list*)
              (string-lessp feat1 feat2)))))

#+lkb
(defun create-word-identifier (id gen)
  (if id
      (let ((val (if (stringp id)
                     id
                   (create-type (fs-type (rest id))))))
        (if (or (numberp val)
                (stringp val)
                (and (symbolp val)
                     (member (elt (string-downcase val) 0) '(#\R))))
            val
          (funcall gen)))
    (funcall gen)))

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





        

