;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   $RCSfile$
;;  $Revision$
;;      $Date$
;;     Author: Ann Copestake (CSLI),Walter Kasper (DFKI)
;;    Purpose: Creating and outputting MRS structures 
;;   Language: Allegro Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Log$
;; Revision 1.6  1998/07/23 01:24:06  aac
;; mrs equality and removing remnants of page packages
;;
;; Revision 1.5  1998/07/22 01:55:51  aac
;; mrs equality and type file patching
;;
;; Revision 1.4  1998/07/19 03:08:41  aac
;; reduced size of cached lexicon
;;
;; Revision 1.3  1998/07/06 01:09:09  aac
;; mostly fixes to lexical lookup for generation
;;
;; Revision 1.2  1998/06/26 02:35:28  aac
;; at least partially working VIT construction
;;
;; Revision 1.1  1998/06/24 17:15:13  aac
;; adding mrs code to source control
;;
;; Revision 1.2  1998/01/16 23:59:55  malouf
;; Revisions to work with CSLI English grammar.
;;
;; Revision 1.1.1.1  1997/12/12 20:18:29  malouf
;; DFKI preliminary version of 11-Dec-1997.
;;
;; Revision 1.4  1997/11/27 16:35:11  kasper
;; Umstellung auf neues Interface; Kein Disjunktionscheck mehr
;;
;; Revision 1.3  1997/11/21 12:50:02  kasper
;; Prototyp-Version
;;
;; Revision 1.2  1997/09/23 12:19:05  kasper
;; Dan's patches
;;
;; Revision 1.1  1997/07/18 15:45:40  kasper
;; Initial revision
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Creating MRS structures from results of parse
;;;
;;; Outputting MRS structures
;;;

;;; now requires basemrs.lisp for structures and printing
;;; WK: requires mrsglobals.lisp for global variables for paths etc.
(in-package "MRS")

;; now in mrsglobals.lisp
;; (defparameter *mrs-to-vit* nil)

(defparameter *mrs-scoping-p* nil)

;;; First necessary to retrive the structure from the result of
;;; a parse.  The FS returned will have an initial path to get to
;;; the MRS *initial-semantics-path*
;;; Following this gets you to a psoa structure
;;; 
;;; function moved to lkb/page specific files

;; To avoid mis-printing circular structures, locally set *print-circle* to nil

;;; orig
;(defun sort-mrs-struct (mrs-struct)
;  ;;; destructive!!!
;  (setf (psoa-liszt mrs-struct)
;        (sort (psoa-liszt mrs-struct)
;              #'(lambda (rel1 rel2)
;                  (or (string-lessp (rel-sort rel1) (rel-sort rel2))
;                      (and (string-equal (rel-sort rel1) (rel-sort rel2))
;                           (< (cdr (rel-handel rel1))
;                              (cdr (rel-handel rel2))))))))
;  mrs-struct)
;;; new Dan (25.8.)
(defun sort-mrs-struct (mrs-struct)
  ;;; destructive!!!
  (setf (psoa-liszt mrs-struct)
        (sort (psoa-liszt mrs-struct)
              #'(lambda (rel1 rel2)
                  (or (string-lessp (rel-sort rel1) (rel-sort rel2))
                      (and (string-equal (rel-sort rel1) (rel-sort rel2))
                           (< (get-var-num (rel-handel rel1))
                              (get-var-num (rel-handel rel2))))))))
  mrs-struct)

(defun remove-trailing-periods (sentence-string)
  (string-right-trim '(#\Space #\.) sentence-string))

(defun extract-mrs (parse-list &optional generator-p)
  (loop for parse in parse-list
        collect
        (let* ((fs (get-parse-fs parse))
               (sem-fs (path-value fs *initial-semantics-path*)))
         (if (is-valid-fs sem-fs)
          (construct-mrs sem-fs nil generator-p)))))

(defun mrs-language (languages)
  (member *mrs-for-language* languages))

;;; unify::sub-fs returns *fail* if there isn't a path
;;; path-value returns nil if there isn't a path
;;; fs-arcs returns nil if there are no labels
;;; otherwise it gives an assoc list of label fs pairs

;;; can be used as blackboard so we don't have to pass the VIT under
;;; construction around; 
;;; in VIT-conversion a MRS-structure contributes information to several slots
(defvar *current-vit* nil)
(defvar *current-mrs* nil)
(defvar *current-fs* nil)
(defvar *input-string* nil)
(defvar *segment-id* nil)
(defvar *short-test-vit* nil)

;;; called from VM-Parser
;;; AAC - not called for CSLI version of PAGE or the LKB

#-(or :lkb :lingo)
(defun fs2vit (fs sid)
  (setf *segment-id* sid)
  (let* #-pagelite
    ((cp (copy fs))
     (sem-fs  (path-value cp *initial-semantics-path*))
     (dnf (if (is-valid-fs sem-fs) 
              (ndnf sem-fs)))
     (fs1 (cond ((or (consp dnf)
                     (unify::disjunction-node-p dnf))
                 (get-first-real-alter dnf))
                (t dnf)))
     (mrs (if (is-valid-fs fs1)
              (construct-mrs fs1))))
    #+pagelite
    ((fs1 (path-value cfs *initial-semantics-path*))
     (mrs (if (is-valid-fs fs1)
              (construct-mrs fs1))))
    (if mrs
        (or (mrs-to-vit-convert mrs nil)
            (make-vit :utterance-id
                      (make-p-term :predicate "vitID"
                                   :args (list *segment-id* nil))))
      (make-vit :utterance-id
                (make-p-term :predicate "vitID"
                             :args (list *segment-id* nil) )))))

;;; it useful to store the variable-generator for VIT conversion

(defvar *variable-generator* nil)
(defparameter *named-nodes* nil)

;;; WK: distinction between handel and top-handel
;;; added test for cycles
;;; cycles should not occur but they seem to arise occasionally in lattice
;;; parsing
(defun construct-mrs (fs &optional existing-variable-generator generator-p)
  (let ((*VM-arg-roles-only-p* (if generator-p nil *VM-arg-roles-only-p*)))
      (when #-pagelite (not (cyclic-p fs))
            #+pagelite t
            (let ((variable-generator (setf *variable-generator*
                                        (or existing-variable-generator
                                            (create-variable-generator)))))
              (unless existing-variable-generator (setf *named-nodes* nil))
              #-pagelite
              (SETQ fs (deref fs))
              (let ((handel-fs (path-value fs *psoa-handel-path*))
                    (top-h-fs (path-value fs *psoa-top-h-path*))
                    (event-fs (path-value fs *psoa-event-path*))
                    (liszt-fs (path-value fs *psoa-liszt-path*))
                    (h-cons-fs (path-value fs *psoa-rh-cons-path*))
                    (message-fs (path-value fs *psoa-message-path*))
                    (wgliszt-fs (path-value fs *psoa-wgliszt-path*)))
                (make-psoa
                 :handel (create-variable (if (mrs-language '(english))
                                              top-h-fs
                                            handel-fs)
                                          variable-generator)
                 :top-h (create-variable top-h-fs
                                         variable-generator)
                 :index (if (is-valid-fs event-fs)
                            (create-variable event-fs
                                             variable-generator))
                 :liszt (nreverse (construct-liszt liszt-fs
                                                   nil
                                                   variable-generator))
                 :h-cons (nreverse (construct-h-cons h-cons-fs
                                                     nil
                                                     variable-generator))
                 :message (if (is-valid-fs message-fs)
                              (create-rel-struct message-fs 
                                                 variable-generator))
                 :wgliszt (nreverse (construct-wgliszt 
                                     wgliszt-fs
                                     variable-generator))))))))



(defun create-variable-generator ()
  (let ((number 0))
    #'(lambda nil
        (incf number)
        number)))

;; Allow NIL argument to get-var-num
(defun get-var-num (var-struct)
  (when (var-p var-struct)
    (var-id var-struct)))

;;; WK: the extras for the VIT simply collect the feature structures associated
;;; with the INDEXes for VIT conversion
;;; this works presently nice for the German grammar but not English
;;; new is the introduction of group labels; for compatibility with other code
;;; printed as handels; this must be cleaned up later
(defun create-variable (fs gen)
  (when (is-valid-fs fs)
    #-pagelite
    (SETQ fs (deref fs))
    (let ((existing-variable (assoc fs *named-nodes*)))
      (if existing-variable (cdr existing-variable)
        (let* ((idletter (determine-variable-type fs))
               (idnumber (funcall gen))
               (variable-name (if (equal idletter "g")
                                  (format nil "h~A" idnumber)
                                (format nil "~A~A" idletter idnumber)))
               (var-type (fs-type fs))
               (extra (create-index-property-list fs))
;;; create-index-property list is defived
;;; differently for LKB and PAGE versions - abbreviations are
;;; moved to output routines in basemrs 
               (variable-identifier (cond ((equal idletter "g")
                                           (make-group-var 
                                            :name variable-name
                                            :type var-type 
                                            :extra extra 
                                            :id idnumber))
                                          ((equal idletter "h")
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

(defun get-tdl-val (fs pathlist)
  (fs-type (path-value fs pathlist)))

;; WK: I prefer to collect the extra prerties of INDEX on a simple
;; feature-value list assuming that they have simple values
;; temporary hack for English VIT-feature
;; should be made recursive
;;; according to Bernd the dnf is now correct and does not leave embedded
;; disjuntions which I had to check for before
#-lkb
(defun create-index-property-list (fs)
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (setf fs (deref fs)))
;    )
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs))
            (feat-list nil))
        (if (and label-list (consp label-list))
            (loop for feat-val in label-list
                do
                  (cond ((member (car feat-val) *complex-extra-feats*)
                         (setf feat-list 
                           (append feat-list
                                   (create-index-property-list 
                                    (cdr feat-val)))))
                        ((eq (car feat-val) *list-feature*)
                         (push (make-fvpair :feature (car feat-val)
                                            :value (create-coord-list 
                                                    (cdr feat-val)))
                               feat-list))
                        (t (push (make-fvpair :feature (car feat-val)
                                              :value (create-type 
                                                      (fs-type (cdr feat-val))))
                                 feat-list)))))
        feat-list)))

;;; AAC - for generation, we need to retain more information
;;; so the index-proprty-list contains (typed) paths, rather than features
;;; The code in mrs-to-vit converts the paths to the last feature
;;; so the structures there are as expected

#+lkb
(defun create-index-property-list (fs &optional path-so-far)
  (when (is-valid-fs fs)
    (setf fs (deref fs)))
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs))
            (feat-list nil)
            (fs-type (type-of-fs fs)))
        (if (and label-list (consp label-list))
            (loop for feat-val in label-list
                do
                (let ((new-path (extend-typed-path path-so-far fs-type
                                                 (car feat-val)))
                      (next-fs (cdr feat-val)))
                  (cond ((eq (car feat-val) *list-feature*)
                         (push (make-fvpair :feature new-path
                                            :value (create-coord-list 
                                                    next-fs))
                               feat-list))
                        ((is-atomic next-fs)
                         (push (make-fvpair :feature new-path
                                            :value (create-type 
                                                    (fs-type next-fs)))
                               feat-list))
                        (t
                         (setf feat-list 
                           (append feat-list
                                   (create-index-property-list 
                                    next-fs
                                    new-path))))))))
        feat-list)))

(defun create-coord-list (fs)
  (let* ((firstval (path-value fs *first-path*))
         (restval (path-value fs *rest-path*)))
    (when (is-valid-fs firstval)
      (cons (create-variable firstval *variable-generator*)
            (create-coord-list restval)))))

;;; global variables are defined in mrsglobals

(defun determine-variable-type (fs)
  (let ((type (create-type (fs-type fs))))
    (cond ((equal-or-subtype type *event-type*) "e")
          ((equal-or-subtype type *eventtime-type*) "t")
          ((equal-or-subtype type *handle-type*) "h")  
          ((equal-or-subtype type *group_lab-type*) "g")  
          ((equal-or-subtype type *hole-type*) "h")
          ((equal-or-subtype type *label-type*) "h")
          ((equal-or-subtype type *ref-ind-type*) "x")
          ((equal-or-subtype type *deg-ind-type*) "d")
          ((equal-or-subtype type *individual-type*) "d")
          ((equal-or-subtype type *difference-list-type*) "c") 
          ;; Assume coordination structure
          (t "v"))))

;; Add check for disjunction nodes, which MRS can't handle

(defun construct-liszt (fs rels-list variable-generator)
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (setf fs (deref fs)))
;  )
  (if (is-valid-fs fs)
        (let ((label-list (fs-arcs fs)))
          (if label-list
              (let ((first-part (assoc (car *liszt-first-path*)
                                       label-list))
                    (rest-part (assoc (car *liszt-rest-path*)
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
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (SETQ fs (deref fs)))
;  )
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
             (handel-pair (assoc (car *rel-handel-path*)
                                 label-list))
             (label-pair (assoc (car *psoa-label-path*) label-list))
             (pred (assoc (car *rel-name-path*)
                          label-list))
             (rel nil))
;        (unless handel-pair
;          (format t
;                  "ERROR: Handel missing in MRS?"))
        (setf rel (make-rel :sort (create-type (if pred 
                                                   (fs-type (rest pred))
                                                 (fs-type fs)))
                            :handel (if handel-pair
                                        (create-variable
                                         (cdr handel-pair)
                                         variable-generator))           
                            :label (if (and label-pair (is-valid-fs (cdr label-pair)))
                                       (create-variable (cdr label-pair)
                                                        variable-generator))))
        (loop for feat-val in 
	      (sort (remove pred 
		      (remove handel-pair 
			      (remove label-pair label-list)))
		    #'feat-sort-func)
            do
              (when (or (not (boundp '*VM-arg-roles-only-p*))
                        (and *VM-arg-roles-only-p*
                             (not (member (car feat-val) 
                                          *suppressed-VM-arg-roles*)))
                        (and (not *VM-arg-roles-only-p*)
                             (not (member (car feat-val)
                                          *VM-arg-roles*))))
                (let ((feature (car feat-val)))
                  (cond ((member feature *ignored-sem-features*) t)
                        ((member feature *relation-extra-feats*)
                         (setf (rel-extra rel) 
                           (cons (make-fvpair :feature feature
                                              :value (create-type
                                                      (fs-type 
                                                       (cdr feat-val))))
                                 (rel-extra rel))))
                        (t (setf (rel-flist rel) 
                             (cons (make-fvpair :feature feature
                                                :value 
                                                (if (member (car feat-val)
                                                            *value-feats*)
                                                    (create-type
                                                     (fs-type (cdr feat-val))) 
                                                  (create-variable
                                                   (cdr feat-val)
                                                   variable-generator)))
                                   (rel-flist rel))))))))
	(setf (rel-flist rel) (reverse (rel-flist rel)))
        rel)
    ))

(defun feat-sort-func (fvp1 fvp2)
  (let* ((feat1 (car fvp1))
         (feat2 (car fvp2))
         (remlist (member feat1 *feat-priority-list*)))
    (if remlist (or (member feat2 remlist)
                    (not (member feat2 *feat-priority-list*)))
      (unless (member feat2 *feat-priority-list*)
              (string-lessp feat1 feat2)))))

#|                                    
(defun create-type (type)
  (if (and (consp type) (eq (first type) :atom))
      (second type)
    type))
|#

(defun create-word-identifier (id gen)
  (if id
      (let ((val (create-type (fs-type (rest id)))))
        (if (or (numberp val) 
                (and (symbolp val)
                     (member (elt (string val) 0) '(#\R))))
            val
          (funcall gen)))
    (funcall gen)))

(defun construct-h-cons (fs constr-list variable-generator)
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (setf fs (deref fs)))
;  )
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                  (rest-part (assoc (car *liszt-rest-path*)
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
  #-pagelite
  (when (is-valid-fs fs)
    (SETQ fs (deref fs)))
  (if (is-valid-fs fs)
      (let* ((label-list (fs-arcs fs))
             (rel (create-type (fs-type fs)))
             (scarg (assoc  *sc-arg-feature* label-list))
             (cands (assoc  *cands-feature* label-list))
             (outscpd (assoc *outscpd-feature* label-list))
             (prec (assoc *prec-feature* label-list)))
        (if prec
            (make-leq-sc
             :relation rel
             :scarg (when scarg
                      (create-variable (cdr scarg) variable-generator))
             :outscpd (create-variable (cdr prec) variable-generator))
          (make-hcons 
           :scarg (when scarg
                    (create-variable (cdr scarg) variable-generator))
           :cands (when cands 
                    (construct-cands-list (cdr cands)
                                          nil
                                          variable-generator))
           :outscpd (when outscpd
                      (create-variable (cdr outscpd) variable-generator)))))))


;; In mrsoutput.lisp, modify CONSTRUCT-CANDS-LIST to accommodate now treating
;; CANDS attribute as taking a diff-list, so we can distinguish local from
;; non-local values for the attribute.

(defun construct-cands-list (fs cands-list variable-generator)
  (let ((real-list-fs (path-value fs *list-path*))
        (last-pointer (path-value fs *last-path*)))
    #-pagelite
    (when (is-valid-fs real-list-fs)
      (setf real-list-fs (deref real-list-fs)))
    (construct-cands-list-aux real-list-fs cands-list variable-generator 
                              last-pointer)))

(defun construct-cands-list-aux (real-list-fs cands-list variable-generator
				 last-pointer)
  (if (and (is-valid-fs real-list-fs)
           (not (eql real-list-fs last-pointer)))
      (let ((label-list (fs-arcs real-list-fs)))
        (if label-list
            (let ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                  (rest-part (assoc (car *liszt-rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-variable (cdr first-part) variable-generator)
                          cands-list)
                    (construct-cands-list-aux
                     (cdr rest-part)
                     cands-list variable-generator last-pointer))
                cands-list))
          cands-list))
    cands-list))



(defun construct-wgliszt (fs variable-generator)
  #-pagelite
  (when (is-valid-fs fs)
;    (if (is-disjunctive-fs fs)
;        (setf fs (get-first-real-alter fs))
    (setf fs (deref fs)))
;  )
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                  (rest-part (assoc (car *liszt-rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (let* ((first (fs-arcs 
                                 (deref (rest first-part))))
                         (word (assoc *word-feature* first))
                         (id (assoc *id-feature* first))
                         (handels (assoc *handels-feature* first)))
                    (when (and word handels)
                      (cons (make-whg-id :word (create-type 
                                                (fs-type (rest word)))
                                         :id (create-word-identifier id
                                                                     variable-generator)
                                         :handel (construct-wgliszt-handels 
                                                  (rest handels) 
                                                  nil 
                                                  variable-generator))
                            (construct-wgliszt (cdr rest-part) 
                                               variable-generator))))))))))




;;; make a list of handels
(defun construct-wgliszt-handels (fs handle-list variable-generator)
  #-pagelite
  (when (is-valid-fs fs)
    (setf fs (deref fs)))    
  (if (is-valid-fs fs)
      (let ((label-list (fs-arcs fs)))
        (if label-list
            (let ((first-part (assoc (car *liszt-first-path*)
                                     label-list))
                  (rest-part (assoc (car *liszt-rest-path*)
                                    label-list)))
              (if (and first-part rest-part)
                  (progn
                    (push (create-variable (cdr first-part) variable-generator)
                          handle-list)
                    (construct-wgliszt-handels
                     (cdr rest-part)
                     handle-list variable-generator)))
              handle-list))
        handle-list))) 


        

