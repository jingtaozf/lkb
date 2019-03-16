(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

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

(defun clone-mrs (mrs &key skolemizep)
  (let ((copy (mrs::make-psoa)))
    (setf (mrs:psoa-top-h copy)
      (clone-variable (mrs:psoa-top-h mrs) :skolemizep skolemizep))
    (setf (mrs:psoa-index copy)
      (clone-variable (mrs:psoa-index mrs) :skolemizep skolemizep))
    (setf (mrs:psoa-liszt copy)
      (loop
          for ep in (mrs:psoa-liszt mrs)
          collect (clone-ep ep :skolemizep skolemizep)))
    (setf (mrs:psoa-h-cons copy)
      (loop
          for hcons in (mrs:psoa-h-cons mrs)
          collect (clone-hcons hcons :skolemizep skolemizep)))
    copy))

(defun clone-variable (variable &key skolemizep (cachep t))
  (when variable
    (or (and cachep (rest (assoc variable %mrs-copy-cache%)))
        (let ((copy (mrs::make-var 
                     :type (mrs:var-type variable) :id (mrs:var-id variable))))
          (setf (mrs:var-extra copy)
            (loop
                for extra in (mrs:var-extra variable)
                collect (mrs::make-extrapair 
                         :feature (mrs::extrapair-feature extra)
                         :value (mrs::extrapair-value extra))))
          (when skolemizep
            (push
             (mrs::make-extrapair 
              :feature *mtr-skolem-property*
              :value (mrs::var-id variable))
             (mrs:var-extra copy)))
          (when cachep (push (cons variable copy) %mrs-copy-cache%))
          copy))))

(defun clone-ep (ep &key skolemizep)
  (let ((copy (mrs::make-rel 
               :handel (clone-variable
                        (mrs:rel-handel ep) :skolemizep skolemizep)
               :pred (mrs:rel-pred ep) :lnk (mrs::rel-lnk ep))))
    (setf (mrs:rel-flist copy)
      (loop
          for role in (mrs:rel-flist ep)
          for value = (mrs:fvpair-value role)
          when (mrs::var-p value)
          collect (mrs::make-fvpair 
                   :feature (mrs:fvpair-feature role) 
                   :value (clone-variable value :skolemizep skolemizep))
          else
          collect (mrs::make-fvpair 
                   :feature (mrs:fvpair-feature role) 
                   :value value)))
    copy))

(defun clone-hcons (hcons &key skolemizep)
  (mrs::make-hcons 
   :relation (mrs:hcons-relation hcons) 
   :scarg (clone-variable (mrs:hcons-scarg hcons) :skolemizep skolemizep)
   :outscpd (clone-variable (mrs:hcons-outscpd hcons) :skolemizep skolemizep)))

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
  (when var
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
          new))))

(defun import-constant (atom)
  (make-constant :value atom))

(defun export-mrs (mrs &key ids)
  (let* ((%mrs-construction-cache% nil)
         (new (mrs::make-psoa
               :top-h (export-variable (mrs-top mrs) :ids ids)
               :index (export-variable (mrs-index mrs) :ids ids))))
    (setf (mrs:psoa-liszt new)
      (loop
          for ep in (mrs-eps mrs)
          collect (export-ep ep :ids ids)))
    (setf (mrs:psoa-h-cons new)
      (loop
          for hcons in (mrs-hcons mrs)
          collect (export-hcons hcons :ids ids)))
    new))
        
(defun export-ep (ep &key ids)
  (let ((new (mrs::make-rel
              :handel (export-variable (ep-label ep) :ids ids)
              :pred (export-pred (ep-pred ep)))))
    (setf (mrs:rel-flist new)
      (loop
          for role in (ep-roles ep)
          for value = (role-value role)
          collect (mrs::make-fvpair
                   :feature (role-name role) 
                   :value (if (variable-p value)
                            (export-variable value :ids ids)
                            (export-constant value)))))
    new))

(defun export-hcons (hcons &key ids)
  (make-hcons 
   :relation (hcons-relation hcons) 
   :scarg (export-variable (hcons-harg hcons) :ids ids)
   :outscpd (export-variable (hcons-larg hcons) :ids ids)))

(defun export-pred (pred)
  pred)

(defun export-variable (variable &key ids)
  (when variable
    (or (rest (assoc variable %mrs-construction-cache%))
        (let ((new (mrs::make-var
                    :type (variable-type variable)
                    :id (or (and ids (funcall ids)) (variable-id variable)))))
          (setf (mrs:var-extra new)
            (loop
                for property in (variable-properties variable)
                collect (mrs::make-extrapair 
                         :feature (property-name property)
                         :value (property-value property))))
          (push (cons variable new) %mrs-construction-cache%)
          new))))

(defun export-constant (constant)
  (constant-value constant))

(defun test-integrity (mrs)
  (labels ((test-variable (variable)
             (or (not (mrs:var-p variable))
                 (not (stringp (mrs:var-type variable)))
                 (not (numberp (mrs:var-id variable)))
                 (not
                  (loop
                      for extra in (mrs:var-extra variable)
                      always (and (symbolp (mrs::extrapair-feature extra))
                                  (mrs::extrapair-value extra)
                                  (symbolp (mrs::extrapair-value extra))))))))
    (when (mrs:psoa-p mrs)
      (loop
          with invalid
          for ep in (mrs:psoa-liszt mrs)
          for pred = (mrs:rel-pred ep)
          unless (and (or (symbolp pred) (stringp pred))
                      (mrs:var-p (mrs:rel-handel ep)))
          do (pushnew ep invalid)
          else when (test-variable (mrs:rel-handel ep))
          do (pushnew (mrs:rel-handel ep) invalid)
          else do
            (loop
                for role in (mrs:rel-flist ep)
                for value = (mrs:fvpair-value role)
                unless (symbolp (mrs:fvpair-feature role))
                do (pushnew ep invalid)
                when (mrs:var-p value) do
                  (when (test-variable value)
                    (pushnew value invalid))
                else do
                  (unless (stringp value)
                    (pushnew ep invalid)))
          finally (return (nreverse invalid))))))
