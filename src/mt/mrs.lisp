(in-package :mt)

(defstruct (mrs)
  top index eps hcons variables)

(defstruct (ep)
  label pred roles)

(defstruct (role)
  name value optionality)

(defstruct (variable)
  type id properties external optionality)

(defstruct constant
  value)

(defstruct (property)
  name value)

(defstruct (hcons)
  relation harg larg)

;;;
;;; make debugging with MRSs a little easier: print very compact representation
;;; of object by default; set *mrs-raw-output-p* to see things in full glory.
;;;
(defparameter *mrs-raw-output-p* nil)

(defmethod print-object ((object mrs) stream)
  (cond
   (*mrs-raw-output-p* 
    (call-next-method))
   (t
    (format stream "~a:~@[~a:~]{" (mrs-top object) (mrs-index object))
    (loop
        for ep in (mrs-eps object)
        do (format stream " ~a" ep))
    (format stream " }"))))

(defmethod print-object ((object ep) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (let ((pred (ep-pred object))
          (roles (ep-roles object)))
      (format 
       stream
       "~@[~a:~]~(~a~)("
       (ep-label object)
       (if (stringp pred)
         (format nil "~s" pred)
         (or pred "_")))
      (loop
          for role in roles
          for value = (role-value role)
          do (format stream "~:[ ~;~]~a" (eq role (first roles)) value))
      (format stream ")"))))

(defmethod print-object ((object variable) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (format
     stream
     "~@[~*[~]~(~a~)~@[~d~]~@[~*]~]"
     (variable-optionality object)
     (variable-type object) (variable-id object)
     (variable-optionality object))))

(defmethod print-object ((object constant) stream)
  (if *mrs-raw-output-p*
    (call-next-method)
    (format
     stream
     "`~a'"
     (constant-value object))))

(defun pred= (pred1 pred2)
  ;;
  ;; the symbol vs. string distinction is, at best, technically motivated.  in
  ;; comparing two predicates, symbol comparison will be faster, but otherwise
  ;; we want to consider _foo_rel and "_foo_rel" equal, nevertheless.  also,
  ;; pred names are _not_ case sensitive, thus string-equal().
  ;; 
  (or
   (eq pred1 pred2)
   (string-equal (string pred1) (string pred2))))

(defun type= (type1 type2)
  (string-equal type1 type2))

(defun constant= (value1 value2)
  ;;
  ;; see comments on pred=(), only constants _are_ case-sensitive
  ;;
  (when (and (constant-p value1) (constant-p value2))
    (let ((value1 (constant-value value1))
          (value2 (constant-value value2)))
      (or
       (eq value1 value2)
       (string= (string value1) (string value2))))))

(defun copy-value (value)
  (cond
   ((variable-p value) (copy-variable value))
   ((constant-p value) (copy-constant value))
   (t value)))

(defparameter %mrs-construction-cache% nil)

(defun import-mrs (psoa &key externals)
  (let* ((%mrs-construction-cache% nil)
         (new (make-mrs
               :top (import-variable (mrs:psoa-top-h psoa))
               :index (import-variable (mrs:psoa-index psoa)))))
    (setf (mrs-eps new)
      (loop
          for ep in (mrs:psoa-liszt psoa)
          collect (import-ep ep)))
    (setf (mrs-hcons new)
      (loop
          for hcons in (mrs:psoa-h-cons psoa)
          collect (import-hcons hcons)))
    (if externals
      (loop
          for (foo . internal) in %mrs-construction-cache%
          for external = (first (rassoc foo externals))
          do
            (setf (variable-external internal) external)
            (push internal (mrs-variables new)))
      (loop
          for (external . internal) in %mrs-construction-cache%
          do
            (setf (variable-external internal) external)
            (push internal (mrs-variables new))))
    new))
        
(defun import-ep (rel)
  (let ((new (make-ep
              :label (import-variable (mrs:rel-handel rel))
              :pred (import-pred (mrs:rel-pred rel)))))
    (setf (ep-roles new)
      (loop
          for role in (mrs:rel-flist rel)
          for value = (mrs:fvpair-value role)
          collect (make-role 
                   :name (mrs:fvpair-feature role) 
                   :value (if (mrs::var-p value)
                            (import-variable value)
                            (import-constant value)))))
    new))

(defun import-hcons (hcons)
  (make-hcons 
   :relation (mrs:hcons-relation hcons) 
   :harg (import-variable (mrs:hcons-scarg hcons))
   :larg (import-variable (mrs:hcons-outscpd hcons))))

(defun import-pred (pred)
  pred)

(defun import-variable (var)
  (or (rest (assoc var %mrs-construction-cache%))
      (let ((new (make-variable
                  :type (mrs:var-type var) :id (mrs:var-id var))))
        (setf (variable-properties new)
          (loop
              for extra in (mrs:var-extra var)
              collect (make-property 
                       :name (mrs::extrapair-feature extra)
                       :value (mrs::extrapair-value extra))))
        (push (cons var new) %mrs-construction-cache%)
        new)))

(defun import-constant (atom)
  (make-constant :value atom))
