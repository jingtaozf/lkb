(in-package "MRS")

;;; convert a VIT structure into an MRS structure

(defun get-item-from-vit-pred (pred)
  ;;; temporary
  (let ((res nil))
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (and (semdbitem-p v)
                          (string-equal (string pred)
                                        (string (semdbitem-vitrel v))))
                 (push v res)))
           *vit-semdb*)
  res))

(defparameter *stored-args* nil)

(defparameter *stored-labels* nil)

(defparameter *index-info* (make-hash-table))

(defun initialize-vit-to-mrs-vars nil
  (setf *stored-args* nil)
  (setf *stored-labels* nil)
  (clrhash *index-info*))

;;; (mrs::generate-from-vit (car mrs::*last-vits*))

(defun generate-from-vit (vit-struct)
  (for mrs in (mrs::vit-to-mrs vit-struct)
     do
  (cl-user::generate-from-mrs mrs)))

#|
(for mrs in (mrs::vit-to-mrs (car mrs::*last-vits*))
     do
(mrs::output-mrs1 mrs
                 'mrs::simple t))
|#

#|
(mrs::vit-to-mrs-file "vittest2")
|#

(defun vit-to-mrs-file (filename)
  ;;; takes a file of turns expressed as VITS
  ;;; converts them to MRS
  (let ((turns (read-vit-turn-file filename)))
    (for turn in (nreverse turns)
         do
         (let ((vit (vitturn-vit turn)))
           (format t "~%Input: ~A~%" (vitturn-input turn))
           (write-vit t vit)
           (format t "~%")
           (let ((mrss (vit-to-mrs vit)))
             (for mrs in mrss
                  do
                  (output-mrs1 mrs 'simple t); ))))))
                  (cl-user::generate-from-mrs mrs)
                  (cl-user::show-gen-result)))))))


(defun vit-to-mrs (vit-struct)
  ;;; takes a VIT, returns a list of MRSs
  (when (vit-p vit-struct)
    (initialize-vit-to-mrs-vars)
    (let ((syntax (vit-syntax vit-struct))
          (tense (vit-tenseandaspect vit-struct))
          (scope (vit-scope vit-struct))
          (sem (vit-semantics vit-struct)))
      (make-syntax-info syntax)
      (make-syntax-info tense)
      (let ((mrs-rels (convert-vit-sem sem scope)))
        (for rel-alts in (expand-combinations mrs-rels)
             collect
             (make-psoa :liszt rel-alts))))))

(defun make-syntax-info (syntax)
  ;;; creates a record which is accessed when variables are
  ;;; created
  (for vit-pterm in syntax
       do
       (if (vit-special-form-p vit-pterm)
         (let ((index (vit-special-form-instance vit-pterm)))
           (when (and index (vit-var-p index))
             (push vit-pterm 
                   (gethash (vit-var-id index) *index-info*))))
         (if (p-term-p vit-pterm)
             ;;; file reading code doesn't create special-form
             (let ((index (first (p-term-args vit-pterm))))
           (when (and index (vit-var-p index))
             (push vit-pterm 
                   (gethash (vit-var-id index) *index-info*))))))))

(defun expand-combinations (lst)
  (if (null lst) '(nil)
      (let ((subcombs (expand-combinations (cdr lst))))
        (for subcomb in subcombs
             append
             (for el in (car lst) 
                  collect
                  (append el subcomb))))))

(defun convert-vit-sem (sem scope)
  (let ((label-sets nil))
    (for vit-pterm in sem
         do
         (when (p-term-p vit-pterm)
           (let ((label (first (p-term-args vit-pterm))))
             (when label
               (let* ((label-id (vit-var-id label))
                     (exists (assoc label-id label-sets)))
                 (if exists
                   (push vit-pterm (cddr exists))
                   (push (list label-id label vit-pterm)
                         label-sets)))))))
    (for set in label-sets
         filter
         (construct-mrs-relations-from-vit 
          (cadr set)
          (cddr set) scope))))

(defun construct-mrs-relations-from-vit (label set scope)
  ;;; returns a set of alternatives
  (let ((main nil))
    ;; main is a list of elements, where each
    ;; element corresponds to a single predication in the VIT
    ;; but is a list of alternatives after access to the sem db
  (for item in set
       do
       (let ((main-items 
              (get-item-from-vit-pred (p-term-predicate item))))
         (push
           (for mi in main-items
                collect
                (cons item mi))
           main)))
  (expand-combinations 
   (for main-item in main
        filter
        (for alt-item-pair in main-item
             filter   
             (let* ((main-pterm (cdr alt-item-pair))
                    (item (car alt-item-pair)))
               (if (string-equal (string (semdbitem-gramrel main-pterm))
                        "NOP_REL")
                   nil
                 (let ((args
                        (for subitem in set
                             filter
                             (if (arg-for-main-item subitem main-pterm item)
                                 subitem))))
                 (list
                  (construct-mrs-rel-from-vit label
                                              item main-pterm args scope))))))))))

(defun arg-for-main-item (sub-item db-item item)
  ;;; temporary
  (declare (ignore db-item item))
  (member (p-term-predicate sub-item)
          *mrs-arg-features*
          :test #'(lambda (x y)
                    (string-equal 
                     (string x)
                     (string (cdr y))))))
  
(defun construct-mrs-rel-from-vit (label item db-item args scope)
  (make-rel
   :handel (convert-vit-label-to-mrs label scope)
   :sort (semdbitem-gramrel db-item)
   :flist (convert-vit-args-to-mrs item args db-item))) 

(defun convert-vit-args-to-mrs (item args db-item)
  (let ((mrs-args (find-mrs-args db-item))
        (internal-args (cdr (p-term-args item))))
    (for mrs-arg in mrs-args
         filter
         (let* ((mrs-arg-feature (car mrs-arg))
                (mrs-arg-fstring (string mrs-arg-feature))
                (mrs-arg-type (cdr mrs-arg)))
           (unless (member mrs-arg-feature '(cl-user::handel
                                             cl-user::label))
             (cond ((member mrs-arg-fstring
                            *mrs-arg-features*
                            :test #'(lambda (x y)
                                      (string-equal 
                                       x
                                       (string (cdr y)))))
             ;;; parsons arg
                    (dolist (arg args)
                      (when (string-equal 
                             (string (p-term-predicate arg))
                             mrs-arg-fstring)
                        (return
                          (make-fvpair :feature
                                       mrs-arg-feature
                                       :value
                                       (convert-vit-arg-val-to-mrs 
                                        mrs-arg-type
                                        (third (p-term-args arg))))))))
                   ((eql mrs-arg-feature 'cl-user::arg4)
                    (dolist (arg args)
                      (when (string-equal 
                             (string (p-term-predicate arg))
                             "arg3")
                        (return
                          (make-fvpair :feature
                                       mrs-arg-feature
                                       :value
                                       (convert-vit-arg-val-to-mrs 
                                        mrs-arg-type
                                        (third (p-term-args arg))))))))
                    (t
           (let ((internal-arg (car internal-args)))
             (unless internal-arg
               (error "argument mismatch"))
             (setf internal-args (cdr internal-args))
             (make-fvpair :feature mrs-arg-feature
                          :value
                          (convert-vit-arg-val-to-mrs 
                           mrs-arg-type
                           internal-arg))))))))))
    
             



(defun find-mrs-args (db-item)
  (let* ((relname (semdbitem-gramrel db-item))
         (known-type 
          (progn (cl-user::eval-possible-leaf-type cl-user::*leaf-types* 
                                                   relname)
                 (cl-user::get-type-entry relname))))
    (if known-type
        (sort
         ;; feat-sort-func should accept this
         ;; it returns an alist of features and type values
         (cl-user::type-signature known-type)
         #'feat-sort-func)
      (error "Unknown relation ~A" relname))))

(defun convert-vit-arg-val-to-mrs (type arg)
  (let ((stored-arg 
         (assoc arg *stored-args* 
                :test #'(lambda (x y)
                          (equal (extract-vit-var-id x)
                                 (extract-vit-var-id y))))))
    (let ((existing (cdr stored-arg)))
      (if existing
          (progn
            (if (var-type existing)
                (setf (var-type existing)
                  (cl-user::greatest-common-subtype (var-type existing)
                                                    type))
              (setf (var-type existing) type))
            (setf (var-name existing)
              (determine-mrs-var-name type 
                                      (var-id existing)))
            existing)
        (let* ((vittype nil)
        ;;; need to fix this so that type can be made more specific
        ;;; if warranted by VIT
               (id (extract-vit-var-id arg))
               (res
                (make-var :extra (extract-extra-info id)
                          :type type
                          :name (determine-mrs-var-name type id)
                          :id id)))
          (push (cons arg res)
                *stored-args*)
          res)))))

(defun determine-mrs-var-name (type id)
  (format nil "~A~A" 
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
                ((equal-or-subtype type *difference-list-type*) "c") 
                ;; Assume coordination structure
                (t "v"))
          id))



(defun convert-vit-label-to-mrs (label scope)
  (unless label
    (error "no label"))
  (let ((stored-label 
         (assoc label *stored-labels* 
                :test
                #'(lambda (x y)
                          (equal (extract-vit-var-id x)
                                 (extract-vit-var-id y))))))
    (or (cdr stored-label)
        (let* ((g-label
                (dolist (scope-item scope)
                  (when 
                      (and (p-term-p scope-item)
                           (string-equal 
                            (string (p-term-predicate scope-item))  "in_g")
                           (equal (extract-vit-var-id label)
                                  (extract-vit-var-id 
                                   (first (p-term-args scope-item)))))
                    (return (second (p-term-args scope-item))))))
               (type 'cl-user::handle)
               (id (extract-vit-var-id (or g-label label))))
          (let ((res
                 (make-var :type type
                           :extra (extract-extra-info id)
                           :name (determine-mrs-var-name type id)        
                           :id id)))
            (push (cons label res) 
                  *stored-labels*)
            res)))))

(defun extract-vit-var-id (id)
  (vit-var-id id))


(defun extract-extra-info (id)
  (let* ((info (gethash id *index-info*))
        (pers nil)
         (num nil)
         (res nil))
    (for pterm in info
         do
         (let ((pred (p-term-predicate pterm)))
           (cond ((string-equal (string pred) "pers")
                  (push (get-syn-val pterm) pers))
                 ((string-equal (string pred) "num")
                  (push (get-syn-val pterm) num))
                 (t
                  (let ((extra 
                         (make-extra-path pred
                                          (get-syn-val pterm))))
                    (when extra
                      (push extra
                            res)))))))
    (let ((extra-pn (make-pers-num pers num)))
      (when extra-pn
        (push extra-pn res)))
    res))

(defun get-syn-val (pterm)
  (let ((val
         (if (vit-special-form-p pterm)
             (first (p-term-args pterm))
           (second (p-term-args pterm)))))
    (if val
        (if (p-term-p val)
            (convert-disj-val (p-term-args val))
          val))))

(defun convert-disj-val (vals)
  'cl-user::*top*)

(defun make-pers-num (pers num)
  (let* ((real-pers (car pers))
         (real-num (car num))
         (val
    (cond ((and real-pers real-num)
           (intern (concatenate 'string (format nil "~A" real-pers)
                                (format nil "~A" real-num))
                   :cl-user))
          (real-pers (intern (concatenate 'string (format nil "~A" real-pers)
                                "PER"))
                     :cl-user)
          (t nil))))
    (if val
        (make-fvpair :feature 
                     (cl-user::create-typed-path-from-feature-list 
                      '(cl-user::png cl-user::pn))
                   :value val))))

(defun make-extra-path (pred val)
  (multiple-value-bind (mrspath mrsvalue)
      (lookup-vit-to-mrs-extra pred val)
    (when (and mrspath mrsvalue)
      (make-fvpair :feature 
                   (cl-user::create-typed-path-from-feature-list mrspath)
                   :value mrsvalue))))

(defun lookup-vit-to-mrs-extra (pred val)
  (cond 
   ((and (string-equal (string pred) "ta_tense")
           (string-equal (string val) "pres"))
    (values '(cl-user::vit cl-user::vittense) 'cl-user::present))
   ((and (string-equal (string pred) "ta_mood")
           (string-equal (string val) "ind"))
    (values '(cl-user::vit cl-user::vitmood) 'cl-user::indicative))
   ((string-equal (string pred) "gend")
    (cond ((string-equal (string val) "masc")
           (values '(cl-user::png cl-user::gen) 'cl-user::masc))
          ((string-equal (string val) "neut")
           (values '(cl-user::png cl-user::gen) 'cl-user::neut))
          ((string-equal (string val) "fem")
           (values '(cl-user::png cl-user::gen) 'cl-user::fem))
          (t nil)))
   (t (format t "~%FIX ~A ~A" pred val)
    nil)))



