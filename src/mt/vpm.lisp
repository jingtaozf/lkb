(in-package :mt)

;;;
;;; Copyright (c) 2006 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
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

(defstruct tm
  lhs rhs)

(defstruct pm
  lhs rhs pmrs)

(defstruct pmr
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
        for line = (read-line stream nil nil)
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
              (when pm (setf (pm-pmrs pm) (nreverse (pm-pmrs pm))))
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
        when line
        do
          (multiple-value-bind (foo bar starts ends) 
              (ppcre::scan
               "^\\s*(.+)\\s*([<>=]{2,3})\\s*(.+)\\s*$"
               line)
            (declare (ignore foo bar))
            (when (and starts ends)
              (let ((pmr (make-pmr))
                    (left (subseq line (aref starts 0) (aref ends 0)))
                    (test (subseq line (aref starts 1) (aref ends 1)))
                    (right (subseq line (aref starts 2) (aref ends 2))))
                (setf (pmr-direction pmr)
                  (if (string= test "==")
                    (cons t t)
                    (cons (position #\> test) (position #\< test))))
                (when (position #\= test)
                  (setf (pmr-test pmr) :equality))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" left)
                  (declare (ignore foo bar))
                  (let* ((match (subseq left (aref starts 0) (aref ends 0)))
                         (n (length match)))
                    (push
                     (if (and (char= (char match 0) #\[)
                              (char= (char match (- n 1)) #\]))
                       (cons :type (subseq match 1 (- n 1)))
                       (mrs:vsym match))
                     (pmr-left pmr))))
                (ppcre:do-scans
                    (foo bar starts ends "([^ ]+)(?: |$)" right)
                  (declare (ignore foo bar))
                  (let* ((match (subseq right (aref starts 0) (aref ends 0)))
                         (n (length match)))
                    (push
                     (if (and (char= (char match 0) #\[)
                              (char= (char match (- n 1)) #\]))
                       (cons :type (subseq match 1 (- n 1)))
                       (mrs:vsym match))
                     (pmr-right pmr))))
                (setf (pmr-left pmr) (nreverse (pmr-left pmr)))
                (setf (pmr-right pmr) (nreverse (pmr-right pmr)))
                (push pmr (pm-pmrs pm)))))
        finally
          (when pm (setf (pm-pmrs pm) (nreverse (pm-pmrs pm))))
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
  
  (let ((%variables%)
        (copy (mrs::make-psoa)))
    (labels ((map-variable (variable)
               (if (mrs::var-p variable)
                 (or (rest (assoc variable %variables%))
                     (let ((copy (mrs::make-var 
                                  :type (mrs:var-type variable)
                                  :id (mrs:var-id variable))))
                       (setf (mrs:var-extra copy)
                         (map-properties
                          (mrs:var-type variable)
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
            collect
              (let ((handel (map-variable (mrs:rel-handel ep)))
                    (flist (loop
                               for role in (mrs:rel-flist ep)
                               for value = (mrs:fvpair-value role)
                               collect (mrs::make-fvpair 
                                        :feature (mrs:fvpair-feature role) 
                                        :value (map-variable value)))))
                (if (mrs::char-rel-p ep)
                  (mrs::make-char-rel
                   :handel handel :pred (mrs:rel-pred ep) :flist flist
                   :lnk (mrs::rel-lnk ep)
                   :cfrom (mrs::char-rel-cfrom ep) :cto (mrs::char-rel-cto ep))
                  (mrs::make-rel
                   :pred (mrs:rel-pred ep) :lnk (mrs::rel-lnk ep)
                   :handel handel :flist flist)))))
      (setf (mrs:psoa-h-cons copy)
        (loop
            for hcons in (mrs:psoa-h-cons mrs)
            collect
              (mrs::make-hcons 
               :relation (mrs:hcons-relation hcons) 
               :scarg (map-variable (mrs:hcons-scarg hcons))
               :outscpd (map-variable (mrs:hcons-outscpd hcons))))))
    copy))

;;;
;;; _fix_me_
;;; make sure properties end up in an order compatible with the VPM itself,
;;; i.e. the order in which they were mapped.
;;;
(defun map-properties (type properties vpm &optional (direction :forward))
  (when (null properties) (return-from map-properties))
  (loop
      with result with mapped
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
            for pmr in (pm-pmrs pm)
            when (test-pmr type values pmr direction) do
              (loop
                  for property in lhs
                  do (pushnew property mapped :test #'eq))
              (loop
                  with right 
                  = (if (eq direction :forward) (pmr-right pmr) (pmr-left pmr))
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

(defun test-pmr (type values pmr &optional (direction :forward))
  (unless (if (eq direction :forward)
            (first (pmr-direction pmr))
            (rest (pmr-direction pmr)))
    (return-from test-pmr))
  (let ((left (if (eq direction :forward) (pmr-left pmr) (pmr-right pmr))))
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
            (case (pmr-test pmr)
              (:equality
               (eq value match))
              (:subsumption
               (or (eq value match)
                   (and (mrs::is-valid-type value) (mrs::is-valid-type match)
                        (mrs::equal-or-subtype value match))))))))))
                               