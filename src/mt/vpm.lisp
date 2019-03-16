(in-package :mt)

;;;
;;; Copyright (c) 2006 -- 2008 Stephan Oepen (oe@ifi.uio.no)
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

(defconstant *vpm-wildcard* (mrs:vsym "*"))

(defconstant *vpm-blade* (mrs:vsym "!"))

(defstruct vpm
  id tms pms flags)

(defstruct pm
  lhs rhs mrs)

(defstruct mr
  left right (test :subsumption) (direction (cons t t)))

(defparameter *vpms* nil)

(defun read-vpm (file &optional (id :default))
  (unless (probe-file file)
    (error "read-vpm(): unable to open `~a'" (namestring file)))
  (format 
   t 
   "~&read-vpm(): reading file `~a~@[.~a~]'.~%"
   (pathname-name file) (pathname-type file))
  (with-open-file (stream file :direction :input)
    (loop
        with vpm = (make-vpm :id id) with pm
        for line = (let ((foo (read-line stream nil nil)))
                     (and foo (string-trim '(#\return) foo)))
        while line
        when (or (ppcre::scan "^[ \\t]*;+" line)
                 (ppcre:scan "^[ \\t]*$" line))
        do (setf line nil)
        when line do
          (multiple-value-bind (foo bar starts ends) 
              (ppcre::scan
               "^[ \\t]*([^:]+)[ \\t]*:[ \\t]*([^:]+)[ \\t]*$"
               line)
            (declare (ignore foo bar))
            (when (and starts ends)
              (when pm (setf (pm-mrs pm) (nreverse (pm-mrs pm))))
              (setf pm (make-pm))
              (let ((lhs (subseq line (aref starts 0) (aref ends 0)))
                    (rhs (subseq line (aref starts 1) (aref ends 1))))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" lhs)
                  (declare (ignore foo bar))
                  (push
                   (mrs:vsym (subseq lhs (aref starts 0) (aref ends 0)))
                   (pm-lhs pm)))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" rhs)
                  (declare (ignore foo bar))
                  (push
                   (mrs:vsym (subseq rhs (aref starts 0) (aref ends 0)))
                   (pm-rhs pm))))
              (setf (pm-lhs pm) (nreverse (pm-lhs pm)))
              (setf (pm-rhs pm) (nreverse (pm-rhs pm)))
              (push pm (vpm-pms vpm))
              (setf line nil)))
        when line do
          (multiple-value-bind (foo bar starts ends) 
              (ppcre::scan
               "^\\s*(.+)\\s*([<>=]{2,3})\\s*(.+)\\s*$"
               line)
            (declare (ignore foo bar))
            (when (and starts ends)
              (let ((mr (make-mr))
                    (left (subseq line (aref starts 0) (aref ends 0)))
                    (test (subseq line (aref starts 1) (aref ends 1)))
                    (right (subseq line (aref starts 2) (aref ends 2))))
                (setf (mr-direction mr)
                  (if (string= test "==")
                    (cons t t)
                    (cons (position #\> test) (position #\< test))))
                (when (position #\= test)
                  (setf (mr-test mr) :equality))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" left)
                  (declare (ignore foo bar))
                  (let* ((match (subseq left (aref starts 0) (aref ends 0)))
                         (n (length match)))
                    (push
                     (cond
                      ((null pm) match)
                      ((and (char= (char match 0) #\[)
                            (char= (char match (- n 1)) #\]))
                       (cons :type (subseq match 1 (- n 1))))
                      (t (mrs:vsym match)))
                     (mr-left mr))))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" right)
                  (declare (ignore foo bar))
                  (let* ((match (subseq right (aref starts 0) (aref ends 0)))
                         (n (length match)))
                    (push
                     (cond
                      ((null pm) match)
                      ((and (char= (char match 0) #\[)
                            (char= (char match (- n 1)) #\]))
                       (cons :type (subseq match 1 (- n 1))))
                      (t (mrs:vsym match)))
                     (mr-right mr))))
                (setf (mr-left mr) (nreverse (mr-left mr)))
                (setf (mr-right mr) (nreverse (mr-right mr)))
                (unless pm
                  (setf (mr-left mr) (first (mr-left mr)))
                  (setf (mr-right mr) (first (mr-right mr))))
                (if pm
                  (push mr (pm-mrs pm))
                  (push mr (vpm-tms vpm))))))
        finally
          (when pm (setf (pm-mrs pm) (nreverse (pm-mrs pm))))
          (setf (vpm-tms vpm) (nreverse (vpm-tms vpm)))
          (setf (vpm-pms vpm) (nreverse (vpm-pms vpm)))
          (push vpm *vpms*)
          (return vpm))))

(defun map-mrs (mrs vpm
                &optional (direction :forward)
                &key skolemizep preserve (copyp t))
  (declare (ignore copyp))
  ;;
  ;; _fix_me_
  ;; given the various contexts to call this function, we would be better off
  ;; providing a destructive version too, i.e. add a :copyp &key parameter to
  ;; turn the initial copy on or off.                           (23-jun-06; oe)
  ;;
  (when (consp vpm)
    (setf direction (second vpm))
    (setf vpm (find (first vpm) *vpms* :key #'vpm-id)))
  (when (symbolp vpm)
    (setf vpm (find vpm *vpms* :key #'vpm-id)))
  (when (null vpm) (return-from map-mrs mrs))
  
  (let ((%variables% nil)
        (type-map-cache nil)
        (copy (mrs::make-psoa)))
    (labels ((map-variable (variable)
               (if (mrs::var-p variable)
                 (or (rest (assoc variable %variables% :test #'eq))
                     (let* ((var-type (mrs:var-type variable))
                            (copy (mrs::make-var 
                                    :type
                                    (or (cdr (assoc var-type type-map-cache :test #'equal))
                                        (let ((mt (map-type var-type vpm direction)))
                                          (push (cons var-type mt) type-map-cache)
                                          mt))
                                    :id
                                    (mrs:var-id variable))))
                       (setf (mrs:var-extra copy)
                         (map-properties
                          var-type
                          (mrs:var-extra variable)
                          vpm direction))
                       (when preserve
                         (loop
                             for extra in (mrs:var-extra variable)
                             for name = (mrs::extrapair-feature extra)
                             when (member name preserve :test #'eq)
                             do
                               (push
                                (mrs::make-extrapair
                                 :feature name
                                 :value (mrs::extrapair-value extra))
                                (mrs:var-extra copy))))
                       (when skolemizep
                         (push
                          (mrs::make-extrapair 
                           :feature *mtr-skolem-property*
                           :value (mrs::var-string variable))
                          (mrs:var-extra copy)))
                       (push (cons variable copy) %variables%)
                       copy))
                 variable)))
      (setf (mrs:psoa-top-h copy) (map-variable (mrs:psoa-top-h mrs)))
      (setf (mrs:psoa-index copy) (map-variable (mrs:psoa-index mrs)))
      (setf (mrs:psoa-liszt copy)
        (loop
            for ep in (mrs:psoa-liszt mrs)
            for pred = (if mrs::*normalize-predicates-p*
                         (mrs::normalize-predicate (mrs:rel-pred ep))
                         (mrs:rel-pred ep))
            collect
              (let ((handel (map-variable (mrs:rel-handel ep)))
                    (flist (loop
                               for role in (mrs:rel-flist ep)
                               for value = (mrs:fvpair-value role)
                               collect (mrs::make-fvpair 
                                        :feature (mrs:fvpair-feature role) 
                                        :value (map-variable value)))))
                (mrs::make-rel
                 :handel handel :pred pred :flist flist
                 :lnk (mrs::rel-lnk ep)
                 :cfrom (mrs::rel-cfrom ep) :cto (mrs::rel-cto ep)))))
      (setf (mrs:psoa-h-cons copy)
        (loop
            for hcons in (mrs:psoa-h-cons mrs)
            collect
              (mrs::make-hcons 
               :relation (mrs:hcons-relation hcons) 
               :scarg (map-variable (mrs:hcons-scarg hcons))
               :outscpd (map-variable (mrs:hcons-outscpd hcons)))))
      (setf (mrs::psoa-icons copy)
        (loop
            for icons in (mrs::psoa-icons mrs)
            collect
              (mrs::make-icons 
               :relation (mrs::icons-relation icons) 
               :iarg1 (map-variable (mrs::icons-iarg1 icons))
               :iarg2 (map-variable (mrs::icons-iarg2 icons))))))
    copy))

;;;
;;; _fix_me_
;;; make sure properties end up in an order compatible with the VPM itself,
;;; i.e. the order in which they were mapped.
;;;
(defun map-properties (type properties vpm &optional (direction :forward))
  (when (null properties) (return-from map-properties))
  ;;
  ;; _fix_me_
  ;; a special case: when there are no property mappings, in principle we might
  ;; opt to delete all properties, as the VPM definition states that only those
  ;; matched explicitly will be preserved.  however, in our `dual' universe of
  ;; grammar-internal and SEM-I namespaces and our current half-baked approach
  ;; to using grammar-internal names for MTR matching, convert-dag-to-mtr() may
  ;; manufacture a VPM with variable type but no property mappings for creation
  ;; of MTRs.  to make that work, treat nil property mappings as the identity
  ;; mapping.                                                   (23-jan-09; oe)
  ;;
  (when (null (vpm-pms vpm)) (return-from map-properties properties))
  (loop
      with result ;; with mapped ; see below
      for pm in (vpm-pms vpm)
      for lhs = (if (eq direction :forward) (pm-lhs pm) (pm-rhs pm))
      for rhs = (if (eq direction :forward) (pm-rhs pm) (pm-lhs pm))
      for values = (loop
                       for property in lhs
                       for match
                       = (loop
                             for foo in properties
                             when (eq (mrs::extrapair-feature foo) property)
                             return (mrs::extrapair-value foo))
                       collect match)
      do
        (loop
            for mr in (pm-mrs pm)
            when (test-pmr type values mr direction)
            do
              ;; JAC 04-01-2019 - commented out since variable mapped is never used
              ;; (loop
              ;;     for property in lhs
              ;;     do (pushnew property mapped :test #'eq))
              (loop
                  with right
                  = (if (eq direction :forward) (mr-right mr) (mr-left mr))
                  for property in rhs
                  for new in right
                  do
                    (cond
                     ((eq new *vpm-blade*))
                     ((eq new *vpm-wildcard*)
                      (let ((old (pop values)))
                        (when old
                          (push
                           (mrs::make-extrapair :feature property :value old)
                           result))))
                     (t
                      (push
                       (mrs::make-extrapair :feature property :value new)
                       result))))
              (loop-finish))
      finally (return (nreverse result))))

(defun test-pmr (type values mr &optional (direction :forward))
  (unless (if (eq direction :forward)
            (first (mr-direction mr))
            (rest (mr-direction mr)))
    (return-from test-pmr))
  (let ((left (if (eq direction :forward) (mr-left mr) (mr-right mr))))
    (loop
        for value in values
        for match in left
        always
          (cond
           ((and (consp match) (eq (first match) :type))
            (compare-types type (rest match) :type :subsumption :internp t))
           ((eq match *vpm-wildcard*) value)
           ((eq match *vpm-blade*) (null value))
           (t
            (case (mr-test mr)
              (:equality
               (eq value match))
              (:subsumption
               (or (eq value match)
                   (and value
                     ;; LKB subtype test does not require its arguments to be valid type names
                     #-:lkb (mrs::is-valid-type value) #-:lkb (mrs::is-valid-type match)
                     (mrs::equal-or-subtype value match))))))))))

(defun map-type (type vpm &optional (direction :forward))
  (when (consp vpm)
    (setf direction (second vpm))
    (setf vpm (find (first vpm) *vpms* :key #'vpm-id)))
  (when (symbolp vpm)
    (setf vpm (find vpm *vpms* :key #'vpm-id)))
  (when (null vpm) (return-from map-type type))
  ;;
  ;; for a transition period, until we can import the full SEM-I hierarchy and
  ;; use it for MRS comparison, only map variable types in :forward direction.
  ;; this is backwards-compatible with what the LKB used to do, in the sense
  ;; that comparsing MRSs post-generation will end up using MRS variable types
  ;; (`e', `x', et al.) but SEM-I variable properties.         (22-jan-09; oe)
  ;;
  ;; JAC 04-01-2019 - when testing against *vpm-wildcard*, instead of symbol
  ;; equality (which requires a costly vsym call), test string equality
  (loop
      with wildcard = (string *vpm-wildcard*)
      for mr in (and (eq direction :forward) (vpm-tms vpm))
      for left
      = (if (eq direction :forward) (mr-left mr) (mr-right mr))
      when (and (if (eq direction :forward)
                  (first (mr-direction mr))
                  (rest (mr-direction mr)))
                (or (string-equal (string left) wildcard) 
                    (compare-types
                     type left :type (mr-test mr) :internp t)))
      return (let ((right (if (eq direction :forward)
                            (mr-right mr)
                            (mr-left mr))))
               (if (string-equal (string right) wildcard) 
                 type
                 right))
      finally (return type)))
