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

(defparameter *vit-to-mrs-generator* nil)

(defun initialize-vit-to-mrs-vars nil
  (setf *stored-args* nil)
  (setf *stored-labels* nil)
  (clrhash *index-info*)
  (setf *vit-to-mrs-generator* (create-variable-generator 100000)))

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

(mrs::vit-to-mrs-file "vittest13" "~aac/grammar/data/new-rules1.mrs")
|#

#|             
                  (let
                      ((identified-entry-sets
                        (mrs::collect-lex-entries-from-mrs mrs)))
                         (for res in identified-entry-sets
                              do
                              (for item in res
                                   do
                                   (format t "~A ~A " (mrs::found-lex-lex-id-fn item)
                                           (mrs::found-lex-rule-list-fn item))
                                   (cl-user::display-dag 
                                    (cl-user::existing-dag-at-end-of 
                                     (cl-user::tdfs-indef (mrs::found-lex-inst-fs-fn item)) 
                                     mrs::*main-semantics-path*) 'cl-user::simple))))
                                     |#

(defun vit-to-mrs-file (filename &optional rule-file)
  ;;; takes a file of turns expressed as VITS
  ;;; converts them to MRS
  (if rule-file
      (read-inverted-rules rule-file)
    (setf *inverted-mrs-rule-list* nil))
  (let ((turns (read-vit-turn-file filename)))
    (for turn in (nreverse turns)
         do
         (handler-case
         (let ((vit (vitturn-vit turn)))
           (when vit
           (format t "~%Input: ~A~%" (vitturn-input turn))
           (write-vit t vit)
           (format t "~%")
           (let ((mrss (vit-to-mrs vit)))
             (for mrs in mrss
                  do
                  (format t "~%Munged form~%")
                  (output-mrs1 mrs 'simple t) 
                  (cl-user::generate-from-mrs mrs)
;                  (cl-user::show-gen-result)
                  (if cl-user::*gen-record*
                      (for generate in 
                           (sort
                            (mapcar
                             #'(lambda (edge)
                                 (format nil "~{~A~^ ~}" 
                                               (cl-user::fix-spelling 
                                                (cl-user::g-edge-leaves edge))))
                             cl-user::*gen-record*)
                            #'string-lessp)
                           do
                           (format t "~%~A" generate))
                    (format t "~%No strings generated"))))))
         (storage-condition (condition)
           (format t 
                   "~%Memory allocation problem: ~A~%" condition)) 
         (error (condition)
           (format t  "~%Error: ~A~%" condition))))))


(defun vit-to-mrs (vit-struct)
  ;;; takes a VIT, returns a list of MRSs
  (when (vit-p vit-struct)
    (initialize-vit-to-mrs-vars)
    (let ((syntax (vit-syntax vit-struct))
          (tense (vit-tenseandaspect vit-struct))
          (scope (vit-scope vit-struct))
          (sem (vit-semantics vit-struct))
          (discourse (vit-discourse vit-struct)))
      (make-syntax-info syntax)
      (make-syntax-info tense)
      (make-syntax-info discourse)
      (let ((mrs-rels (convert-vit-sem sem scope)))
        (for rel-alts in (expand-combinations mrs-rels)
             collect
             (invert-munging
              (make-psoa :liszt rel-alts)))))))



(defun invert-munging (mrs-psoa)
  (if *inverted-mrs-rule-list*
      (progn 
        (format t "~%Premunged form~%")
        (output-mrs1 mrs-psoa 'simple t)
        (munge-mrs-struct mrs-psoa *inverted-mrs-rule-list*))
    mrs-psoa))

(defun read-inverted-rules (rule-file)
  (let ((saved-rules *ordered-mrs-rule-list*))
    (unwind-protect 
        (progn
          (cl-user::read-mrs-rule-file-aux rule-file)
          (setf *inverted-mrs-rule-list*
            (invert-munge-rules *ordered-mrs-rule-list*)))
      (setf *ordered-mrs-rule-list* saved-rules))))

(defun make-syntax-info (syntax)
  ;;; creates a record which is accessed when variables are
  ;;; created
  (for vit-pterm in syntax
       do
       (if (vit-special-form-p vit-pterm)
         (let ((index (vit-special-form-instance vit-pterm)))
           (when (and index (vit-var-p index))
             (push vit-pterm 
                   (gethash (extract-vit-var-id index) *index-info*))))
         (if (p-term-p vit-pterm)
             ;;; file reading code doesn't create special-form
             (let ((index (first (p-term-args vit-pterm))))
           (when (and index (vit-var-p index))
             (push vit-pterm 
                   (gethash (extract-vit-var-id index) *index-info*))))))))

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
               (let* ((label-id (extract-vit-var-id label))
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
   :sort (munge-supplied-sort (semdbitem-gramrel db-item)
                              item)
   :flist (convert-vit-args-to-mrs item args db-item))) 

(defun munge-supplied-sort (sort item)
  sort)

#|
Finding the arguments is unfortunately complicated.  There are
two types of arguments in a VIT - ones which are internal to the 
predication and external `Parson's style' arguments.  Both of these
have to be converted into internal arguments on the MRS elpred.
The problem is
a) the semdb doesn't record the MRS feature names
b) some arguments are optional, in which case they don't appear in the VIT
c) the munging rules sometimes convert arguments (e.g., suit_rel)
with an ill-formed VIT output

The code that follows is therefore not guaranteed to work ...
It finds a list of the correct arguments for a type from the MRS relation.
All external arguments of the VIT have to be converted - some of these may be
additional to the valid features of the MRS relation because they are 
the output of a munging rule.  If an argument is not external, it is assumed
that it corresponds to one of the features of the MRS other than 
those in *mrs-arg-features* and that the order is preserved ...

As a couple of fun complications, just in case the above was too easy
1) modals have internal arguments in the VIT but are labelled with 
ARG1 etc in the MRS
2) MRS has ARG4s which are converted to other arg numbers by non-reversible
munging rules.  ARG4 has to be treated specially here - generally it's ARG3
in the VIT but there is one class of exceptions.  Warning - this crucially
assumes that there are no (reversible) munging rules which produce something
with an ARG4 without changing the relation type
|#

(defun convert-vit-args-to-mrs (item args db-item)
  (multiple-value-bind (mrs-args rel-type) 
      (find-mrs-args db-item)
    (let* ((internal-args (cdr (p-term-args item)))
	   (dbextra (semdbitem-extra db-item))
	   (arg4-mapping (find-arg4-mapping args mrs-args)))
      (append
       (for mrs-arg in mrs-args
         filter
         (let* ((mrs-arg-feature (car mrs-arg))
                (mrs-arg-fstring (string mrs-arg-feature))
                (mrs-arg-type (cdr mrs-arg))
                (vit-match nil)
                (res
           (unless (or (and (equal mrs-arg-feature 'cl-user::event)
			    (not (equal-or-subtype rel-type 'cl-user::v_event_rel)))
		       (member mrs-arg-feature '(cl-user::handel
						 cl-user::label)))
             (cond ((vit-external-arg-p mrs-arg-fstring dbextra)
                    (setf vit-match 
                      (find-vit-external-arg args 
                                             mrs-arg-fstring))
                    (if vit-match
                        (make-mrs-arg-from-vit 
                         mrs-arg-feature
                         (convert-vit-arg-val-to-mrs 
                          mrs-arg-type
                          (third (p-term-args vit-match))))
                      (make-mrs-arg-from-vit 
                       mrs-arg-feature
                       (make-dummy-mrs-var mrs-arg-type))))
                   ((mrs-arg4-p mrs-arg-fstring dbextra)
                    (setf vit-match
                      (find-vit-external-arg args 
                               arg4-mapping)) 
                    (if vit-match
                        (make-mrs-arg-from-vit 
                         mrs-arg-feature
                         (convert-vit-arg-val-to-mrs 
                          mrs-arg-type
                          (third (p-term-args vit-match))))
                      (make-mrs-arg-from-vit 
                       mrs-arg-feature
                       (make-dummy-mrs-var mrs-arg-type))))
                   (t
                    (let ((internal-arg (car internal-args)))
                      (if internal-arg
                          (progn
                            (setf internal-args (cdr internal-args))
                            (make-mrs-arg-from-vit 
                             mrs-arg-feature
                             (convert-vit-arg-val-to-mrs 
                                          mrs-arg-type
                                          internal-arg)))
                        (make-mrs-arg-from-vit 
                         mrs-arg-feature
                         (make-dummy-mrs-var mrs-arg-type)))))))))
             (when vit-match
               (setf args (remove vit-match args))
               (setf vit-match nil))
             res))
    (for leftover in args
         ;; mismatch due to munging situation
         collect
         (make-mrs-arg-from-vit 
          (intern (string (p-term-predicate leftover))
                  'cl-user)
          (convert-vit-arg-val-to-mrs 
           cl-user::*toptype*
           (third (p-term-args leftover)))))))))
         

(defun find-arg4-mapping (vit-args mrs-args)
  (let ((vit-arg-strings 
         (mapcar #'(lambda (vit-arg)
                     (string
                      (p-term-predicate vit-arg)))
                 vit-args))
        (mrs-arg-strings
         (for mrs-arg in mrs-args
              filter
              (let ((mrs-arg-fstring (string (car mrs-arg))))
                (if (member mrs-arg-fstring
                            *mrs-arg-features*
                            :test #'(lambda (x y)
                                      (string-equal 
                                       x
                                       (string (cdr y)))))
                    mrs-arg-fstring)))))
    (if (and (eql (length mrs-arg-strings) 3)
             (eql (length vit-arg-strings) 3)
             (member "ARG1" mrs-arg-strings :test #'string-equal)
             (member "ARG3" mrs-arg-strings :test #'string-equal)
             (member "ARG4" mrs-arg-strings :test #'string-equal)
             (member "ARG1" vit-arg-strings :test #'string-equal)
             (member "ARG2" vit-arg-strings :test #'string-equal)
             (member "ARG3" vit-arg-strings :test #'string-equal))
        "ARG2"
      "ARG3")))
                                      

(defun vit-external-arg-p (mrs-arg-fstring dbextra)             
  (and (member mrs-arg-fstring
               *mrs-arg-features*
               :test #'(lambda (x y)
                         (string-equal 
                          x
                          (string (cdr y)))))
       (not (vit-external-exception-p dbextra))))
        
(defun vit-external-exception-p (dbextra)
  (and dbextra
       (string-equal (string (car dbextra))
                     "MV")))

(defun mrs-arg4-p (mrs-arg-fstring dbextra)             
  (and (string-equal mrs-arg-fstring "arg4")
       (not (vit-external-exception-p dbextra))))

(defun find-vit-external-arg (args mrs-arg-fstring)
  (dolist (arg args)
    (when (string-equal 
           (string (p-term-predicate arg))
           mrs-arg-fstring)
      (return arg))))

(defun make-mrs-arg-from-vit (mrs-arg-feature value)
  (make-fvpair :feature
               mrs-arg-feature
               :value value))

(defun find-mrs-args (db-item)
  (let* ((relname (semdbitem-gramrel db-item))
         (known-type 
          (progn (cl-user::eval-possible-leaf-type cl-user::*leaf-types* 
                                                   relname)
                 (cl-user::get-type-entry relname))))
    (if known-type
	(values 
	 (sort
         ;; feat-sort-func should accept this
         ;; it returns an alist of features and type values
         (cl-user::type-signature known-type)
         #'feat-sort-func)
	 relname)
      (error "Unknown relation ~A" relname))))

(defun make-dummy-mrs-var (type)
  (let* ((id (funcall *vit-to-mrs-generator*))
          (name (determine-mrs-var-name type id)))
  (make-var :type type :id id :name name)))

(defun convert-vit-arg-val-to-mrs (type arg)
  (if (vit-var-p arg)
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
              res))))
    (format nil "~A" arg)))

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
  (let ((actual-id (vit-var-id id))
        (subid (if (char-equal (vit-var-subid id) #\h) 0
                 1000)))
    (cond ((vit-hole-var-p id) (+ 100 actual-id subid))
          ((vit-label-var-p id) (+ 200 actual-id subid))
          ((vit-instance-var-p id) (+ actual-id subid))
          (t (error "Unexpected VIT id ~A" id)))))


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
                  (let ((extras 
                         (make-extra-paths pred
                                          (get-syn-val pterm)
                                          (get-second-syn-val pterm))))
                    (for extra in extras do
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

(defun get-second-syn-val (pterm)
  (let ((val
         (if (vit-special-form-p pterm)
             (second (p-term-args pterm))
           (third (p-term-args pterm)))))
    (if val
        (if (p-term-p val)
            (convert-disj-val (p-term-args val))
          val))))

(defun convert-disj-val (vals)
  (declare (ignore vals))
  'cl-user::*top*)

(defun make-pers-num (pers num)
  (let* ((real-pers (and (not (eql (car pers) 'cl-user::*top*)) (car pers)))
         (real-num (and (not (eql (car num) 'cl-user::*top*)) (car num)))
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

(defun make-extra-paths (pred val second-val)
  (for local in (list val second-val)
       filter
       (multiple-value-bind (mrspath mrsvalue)
           (lookup-vit-to-mrs-extra pred local)
         (when (and mrspath mrsvalue)
           (make-fvpair :feature 
                        (cl-user::create-typed-path-from-feature-list mrspath)
                        :value mrsvalue)))))
  

(defun lookup-vit-to-mrs-extra (pred val)
  (cond 
   ((and (string-equal (string pred) "ta_tense")
           (string-equal (string val) "pres"))
    (values '(cl-user::vit cl-user::vittense) 'cl-user::present*))
   ((and (string-equal (string pred) "ta_mood")
           (string-equal (string val) "ind"))
    (values '(cl-user::vit cl-user::vitmood) 'cl-user::indicative))
   ((and (string-equal (string pred) "prontype")
           (string-equal (string val) "refl"))
    (values '(cl-user::prontype) 'cl-user::refl))
   ((and (string-equal (string pred) "prontype")
           (string-equal (string val) "std"))
    (values '(cl-user::prontype) 'cl-user::std_pron))
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



