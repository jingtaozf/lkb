(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2008 Stephan Oepen (oe@ifi.uio.no)
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

(defparameter *mrs-comparison-ignore-roles* nil)

(defparameter *mrs-comparison-ignore-properties* nil)

(defparameter *mrs-comparison-equivalent-types* nil)

(defparameter *mrs-comparison-equivalent-predicates* nil)

(defun mrs= (mrs1 mrs2
             &key (roles *mrs-comparison-ignore-roles*)
                  (properties *mrs-comparison-ignore-properties*)
                  (types *mrs-comparison-equivalent-types*)
                  (predicates *mrs-comparison-equivalent-predicates*)
                  hcons debug)
  (compare-mrss
   mrs1 mrs2 :type :equal :roles roles :properties properties
   :types types :predicates predicates :hcons hcons :debug debug))

(defun compare-mrss (mrs1 mrs2
                     &key (type :subsumption)
                          (roles *mrs-comparison-ignore-roles*)
                          (properties *mrs-comparison-ignore-properties*)
                          (types *mrs-comparison-equivalent-types*)
                          (predicates *mrs-comparison-equivalent-predicates*)
                          hcons debug)
  ;;
  ;; in (default) :subsumption mode, e.g. when testing post-generation, .mrs2.
  ;; is expected to be more general than .mrs1.
  ;;
  #+:debug
  (setf %mrs1 mrs1 %mrs2 mrs2)
  (incf (lkb::statistics-comparisons lkb::*statistics*))
  (let ((*mrs-comparison-ignore-roles* roles)
        (*mrs-comparison-ignore-properties* properties)
        (*mrs-comparison-equivalent-types* types)
        (*mrs-comparison-equivalent-predicates* predicates)
        (*transfer-debug-p* (cons (and debug :solutions) *transfer-debug-p*))
        (%transfer-solutions% nil)
        (solution (copy-solution))
        solutions)
    (when (compare-variables
           (mrs:psoa-top-h mrs1) (mrs:psoa-top-h mrs2) solution :type type)
      (setf solutions
        (compare-epss
         (mrs:psoa-liszt mrs1) (mrs:psoa-liszt mrs2)
         solution :type type))
      (unless hcons
        (setf solutions
          (loop
              with hcons1 = (mrs:psoa-h-cons mrs1)
              with hcons2 = (mrs:psoa-h-cons mrs2)
              for solution in solutions
              append (compare-hconss hcons1 hcons2 solution :type type))))
      (when (and (null solutions) debug)
        (setf %transfer-solutions%
          (stable-sort %transfer-solutions% #'solution<=))
        (pprint (first (last %transfer-solutions%))))
      (first solutions))))

(defun compare-epss (eps1 eps2 solution &key type)
  ;;
  ;; we must not destructively modify .solution. --- assume that compare-eps()
  ;; will always copy its input parameter first.
  ;;
  (when eps1
    (if (null eps2)
      (list solution)
      (let* ((ep2 (first eps2))
             (solutions
              (loop
                  for ep1 in eps1
                  for result = (compare-eps ep1 ep2 solution :type type)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (compare-epss eps1 (rest eps2) solution :type type)))))))

(defun compare-eps (ep1 ep2 solution
                    &key type
                         (filter *mrs-comparison-ignore-roles*))

  (unless (or (retrieve-ep ep1 solution) (retrieve-ep ep2 solution))
    (let ((solution (copy-solution solution)))
      (and (compare-preds (mrs::rel-pred ep1) (mrs::rel-pred ep2) :type type)
           (compare-variables
            (mrs:rel-handel ep1) (mrs:rel-handel ep2)
            solution :type type)
           (loop
               with flist1 = (loop
                                 for role in (mrs:rel-flist ep1)
                                 for feature = (mrs:fvpair-feature role)
                                 unless (or (eq filter t)
                                            (member feature filter :test #'eq))
                                 collect role)
               with flist2 = (loop
                                 for role in (mrs:rel-flist ep2)
                                 for feature = (mrs:fvpair-feature role)
                                 unless (or (eq filter t)
                                            (member feature filter :test #'eq))
                                 collect role)
               with intersection 
               = (let ((foo (intersect
                             flist1 flist2 :key #'mrs:fvpair-feature)))
                   (if (eq type :subsumption)
                     ;;
                     ;; in subsumption mode, make sure all roles from the 
                     ;; second EP are present in the target EP. 
                     ;;
                     (loop
                         for role in flist2
                         for feature = (mrs:fvpair-feature role)
                         unless (find feature flist1 :key #'mrs:fvpair-feature)
                         do (return-from compare-eps))
                     ;;
                     ;; otherwise, the intersection better be the same as each
                     ;; of the two input sets (in terms of role labels).
                     ;;
                     (unless (= (length foo) (length flist1) (length flist2))
                       (return-from compare-eps)))
                   foo)
               for role in intersection
               for feature = (mrs:fvpair-feature role)
               for role1 = (find feature flist1 :key #'mrs:fvpair-feature)
               for role2 = (find feature flist2 :key #'mrs:fvpair-feature)
               unless (compare-values 
                       (mrs:fvpair-value role1) (mrs:fvpair-value role2)
                       solution :type type)
               do (return-from compare-eps)
               finally
                 (align-eps ep1 ep2 solution)
                 (return solution))))))

(defun compare-hconss (hconss1 hconss2 solution &key type)
  (if hconss1
    (if (null hconss2)
      (list solution)
      (let* ((hcons2 (first hconss2))
             (solutions
              (loop
                  for hcons1 in hconss1
                  for result = (compare-hcons
                                hcons1 hcons2 solution :type type)
                  when result collect result)))
        (when solutions
          (loop
              for solution in solutions
              nconc (compare-hconss
                     hconss1 (rest hconss2) solution :type type)))))
    (unless hconss2 (list solution))))

(defun compare-hcons (hcons1 hcons2 solution &key type)

  (let* ((solution (copy-solution solution))
         (harg (compare-values
                (mrs:hcons-scarg hcons1) (mrs:hcons-scarg hcons2) 
                solution :type type))
         (larg (when harg
                 (compare-values
                  (mrs:hcons-outscpd hcons1) (mrs:hcons-outscpd hcons2)
                  solution :type type))))
    (when larg 
      (align-hconss hcons1 hcons2 solution)
      solution)))

(defun compare-preds (pred1 pred2 &key type)
  (or
   (compare-types pred1 pred2 :type type)
   (loop
       for (new . old) in *mrs-comparison-equivalent-predicates*
       when (and (loop
                     for foo in old
                     thereis (compare-types pred1 foo :type type))
                 (compare-preds new pred2 :type type))
       return t)))

(defun compare-values (value1 value2 solution &key type)
  (if (mrs::var-p value1)
    (when (mrs::var-p value2)
      (compare-variables value1 value2 solution :type type))
    (unless (mrs::var-p value2)
      (compare-constants value1 value2 :type type))))

(defun compare-variables (variable1 variable2 solution &key type)

  #+:debug
  (setf %variable1 variable1 %variable2 variable2)
  (or (and (null variable1) (null variable2))
      (let ((foo (lookup-variable variable2 solution)))
        (cond
         ((eq variable1 foo)
          variable1)
         ((null foo)
          (when (and (or
                      (compare-types 
                       (mrs::var-type variable1) (mrs::var-type variable2) 
                       :internp t :type type)
                      (loop
                          with type = (mrs::var-type variable1)
                          for (new . old) in *mrs-comparison-equivalent-types*
                          when (and (loop
                                        for foo in old
                                        thereis (compare-types
                                                 type foo :type type))
                                    (compare-types
                                     new (mrs::var-type variable2)
                                     :internp t :type type))
                          return t))
                     (compare-extras
                      (mrs:var-extra variable1) (mrs:var-extra variable2)
                      :type type))
            (align-variables variable2 variable1 solution)
            variable1))))))

(defun compare-extras (extras1 extras2
                       &key type
                            (filter *mrs-comparison-ignore-properties*))

  (when filter
    (setf extras1
      (loop
          for extra in extras1
          unless (or (eq filter t)
                     (member (mrs::extrapair-feature extra) filter :test #'eq))
          collect extra))
    (setf extras2
      (loop
          for extra in extras2
          unless (or (eq filter t)
                     (member (mrs::extrapair-feature extra) filter :test #'eq))
          collect extra)))
  
  (let ((intersection
         (intersect extras1 extras2 :key #'mrs::extrapair-feature :test #'eq)))
    (if (eq type :subsumption)
      (loop
          for extra in extras2
          for feature = (mrs::extrapair-feature extra)
          unless (find
                  feature extras1 :key #'mrs::extrapair-feature :test #'eq)
          do (return-from compare-extras))
      (unless (= (length intersection) (length extras1) (length extras2))
        (return-from compare-extras))))
  
  (loop
      for extra1 in (intersect 
                     extras1 extras2 :key #'mrs::extrapair-feature :test #'eq)
      for feature = (mrs::extrapair-feature extra1)
      for value1 = (mrs::extrapair-value extra1)
      for extra2 = (find feature extras2 :key #'mrs::extrapair-feature)
      for value2 = (mrs::extrapair-value extra2)
      always (compare-types value1 value2 :type type)))

(defun compare-constants (constant1 constant2 &key type)
  (cond
     ((eq constant1 constant2)
      constant1)
     ((and (numberp constant1) (numberp constant2))
      (= constant1 constant2))
     (t (compare-types constant1 constant2 :type type))))

(defun compare-types (type1 type2 &key internp type)

  (or (eq type1 type2)
      (and (stringp type1) (stringp type2) (string-equal type1 type2))
      (ignore-errors
       (let ((type1 (if internp
                      (intern (string-upcase type1) mrs:*mrs-package*)
                      type1))
             (type2 (if internp
                      (intern (string-upcase type2) mrs:*mrs-package*)
                      type2)))
         (when (and (mrs:is-valid-type type1) (mrs:is-valid-type type2))
           (if (eq type :subsumption)
             (mrs:equal-or-subtype type1 type2)
             (eq type1 type2)))))))

(defun lookup-variable (variable solution)
  (getf (solution-variables solution) variable))

(defun align-variables (old new solution)
  (let ((foo (getf (solution-variables solution) old)))
    (or foo (setf (getf (solution-variables solution) old) new))))
