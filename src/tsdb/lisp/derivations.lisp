;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "TSDB")

(defparameter *inflectional-rule-suffix* "_infl_rule")

(defparameter *derivations-comparison-level* :all)

(defparameter *derivations-preterminals-equivalent-test* nil)

(defparameter *derivations-equivalences*
  #-:logon nil
  #+:logon
  '((yofc_gle yofc_ersatz yofc_3dig_ersatz year-ersatz)
    (named_gle numvalcard3digit-4digit ratioersatz_n2 numvalcard4digit4digit
     numvalcard3digit4digit wrappedweb-ersatz email-ersatz ratioersatz_n5
     ratioersatz_n6 nameersatz identifierersatz_n1 web-ersatz number-ersatz
     mailboxidersatz numvalcard3digit-3digit ratioersatz_n8 ratioersatz_n4
     ratioersatz_n3 numvalcard3digit3digit ratioersatz_n7 mailboxnameersatz
     numvalcard4digit-4digit phone-ersatz ratioersatz_n1)
    (decade_gle fourdigit_plur_n1 decade_ersatz_n1 threedigit_plur_n1
     twodigit_plur_n1)
    (card_gle numvalcard12digit negdecimaldigit negcarddigit numvalcard2digit
     numvalcard13plusdigit numvalcard9digit numvalcard10digit numvalcard6digit
     numvalcard7digit numvalcard8digit fractionersatz numvalcard4digit
     numvalcard3digit numvalcard5digit numvalcard1digit rangeersatz_2
     numvalcard11digit decimalersatz decimalersatz_2 cardwithcommas)
    (ord_gle sixdigitordersatz rangeersatz fourdigitordersatz onedigitordersatz
     eightdigitordersatz elevendigitordersatz ninedigitordersatz
     fivedigitordersatz threedigitordersatz twodigitordersatz tendigitordersatz
     twelvedigitordersatz thirteenplusdigitersatz sevendigitordersatz)
    (dofw_gle dofw-date)
    (dofm_gle one_digit_euro_day two_digit_day twodigitdomersatz
     two_digit_euro_day dofm-date-range onedigitdomersatz
     dofm-date one_digit_day)
    (be_c_am be_c_am_cx be_c_am_cx_2)
    (be_c_are be_c_are_cx be_c_are_cx_2)
    (be_c_is be_c_is_cx be_c_is_cx_2)
    (be_id_am be_id_am_cx be_id_am_cx_2)
    (be_id_are be_id_are_cx be_id_are_cx_2)
    (be_id_is be_id_is_cx be_id_is_cx_2)
    (be_it_cop_is be_it_cop_is_cx be_it_cop_is_cx_2)
    (be_nv_is be_nv_is_cx be_nv_is_cx_2)
    (be_th_cop_is be_th_cop_is_cx be_th_cop_is_cx_2)
    (had_aux had_aux_cx had_aux_cx_2)
    (had_better_aux had_better_cx had_better_cx_2)
    (has_aux has_aux_cx has_aux_cx_2)
    (have_bse_aux have_bse_aux_cx_1 have_bse_aux_cx_2)
    (have_fin_aux have_fin_aux_cx have_fin_aux_cx_2)
    (will_aux_pos will_aux_pos_cx will_aux_pos_cx_2)
    (would_aux_pos would_aux_pos_cx would_aux_pos_cx_2)
    (a_det a_det_2)
    (email_n1 e_mail_n1 e_mail_n2 e_mail_n3 e_mail_n4)))

(defparameter *derivations-yield-skews* 
  #-:logon nil
  #+:logon
  '(dbl_hyphen_punct hyphen_punct_right s_dash_pct s_dbl_dash_pct))

(defparameter *derivations-ignore-leafs-p* t)

(defparameter *derivations-reconstructor* nil)

(defparameter *derivations-reconstruct-lnk-p* nil)

(defmacro derivation-sponsor (derivation)
  `(when (consp (second ,derivation))
     (first ,derivation)))

(defmacro derivation-id (derivation)
  `(if (consp (second ,derivation))
     (first (second ,derivation))
     (when (integerp (first ,derivation))
       (first ,derivation))))

(defmacro derivation-root (derivation)
  `(if (consp (second ,derivation))
     (second (second ,derivation))
     (if (integerp (first ,derivation)) 
       (second ,derivation)
       (first ,derivation))))

(defmacro derivation-score (derivation)
  `(if (consp (second ,derivation))
     (third (second ,derivation)) 
     (when (integerp (first ,derivation))
       (third ,derivation))))

(defmacro derivation-start (derivation)
  `(if (consp (second ,derivation))
     (fourth (second ,derivation))
     (if (integerp (first ,derivation)) 
       (fourth ,derivation)
       (when (integerp (second ,derivation))
         (second ,derivation)))))

(defmacro derivation-end (derivation)
  `(if (consp (second ,derivation))
     (fifth (second ,derivation))
     (if (integerp (first ,derivation)) 
       (fifth ,derivation)
       (when (integerp (second ,derivation))
         (third ,derivation)))))

(defmacro derivation-daughters (derivation)
  `(if (consp (second ,derivation))
     (rest (rest (rest (rest (rest (second ,derivation))))))
     (if (integerp (first ,derivation))
       (rest (rest (rest (rest (rest ,derivation)))))
       (if (integerp (second ,derivation))
         (rest (rest (rest ,derivation)))
         (rest ,derivation)))))

(defun derivation-depth (derivation)
  (if (consp (second derivation))
    (derivation-depth (second derivation))
    (if (null derivation)
      0
      (+ 1 (loop 
               for son in (derivation-daughters derivation)
               maximize (derivation-depth son))))))

(defun derivation-yield (derivation)
  (unless (null derivation)
    (let ((daughters (derivation-daughters derivation)))
      (if (null (derivation-daughters (first daughters)))
        (list (derivation-root derivation))
        (loop 
            for daughter in daughters
            nconc (derivation-yield daughter))))))

(defun derivation-leafs (derivation)
  (unless (null derivation)
    (let ((daughters (derivation-daughters derivation)))
      (if (null daughters)
        (list (derivation-root derivation))
        (loop 
            for daughter in daughters
            nconc (derivation-leafs daughter))))))

(defun derivation-nodes (derivation)
  (unless (null derivation)
    (let ((id (derivation-id derivation))
          (root (derivation-root derivation))
          (score (derivation-score derivation))
          (start (derivation-start derivation))
          (end (derivation-end derivation)))
      (cons (list id root score start end)
            (loop
                for daughter in (derivation-daughters derivation)
                nconc (derivation-nodes daughter))))))

(defun inflectional-rule-p (derivation)
  (let* ((root (cond
                ((stringp derivation) derivation)
                ((symbolp derivation) (symbol-name derivation))
                ((consp derivation) 
                 (let ((root (derivation-root derivation)))
                   (if (symbolp root) 
                     (symbol-name root)
                     root)))
                (t 
                 (error 
                  "inflectional-rule-p(): invalid call `~s'." 
                  derivation))))
         (break (max 0 (- (length root) (length *inflectional-rule-suffix*)))))
    (when (string-equal root *inflectional-rule-suffix* :start1 break)
      (subseq root 0 break))))

(defun derivation-equal (gold blue 
                         &optional (level *derivations-comparison-level*))
  (labels ((node-equal (gold blue)
             (cond
              #+:logon
              ((and *derivations-equivalences*
                    (or (stringp gold) (symbolp gold))
                    (or (stringp blue) (symbolp blue)))
               (let* ((gold (string gold))
                      (bracket (position #\[ gold))
                      (gold (if bracket (subseq gold 0 bracket) gold))
                      (gold (intern gold :tsdb))
                      (blue (string blue))
                      (bracket (position #\[ blue))
                      (blue (if bracket (subseq blue 0 bracket) blue))
                      (blue (intern blue :tsdb))
                      (equivalence
                       (unless (eq gold blue)
                         (loop
                             for equivalence in *derivations-equivalences*
                             when (member gold equivalence :test #'eq)
                             return equivalence))))
                 (or (eq gold blue) (member blue equivalence :test #'eq))))
              ((stringp gold)
               (cond 
                ((stringp blue) (string-equal gold blue))
                ((symbolp blue) (string-equal gold (symbol-name blue)))))
              ((symbolp gold)
               (cond 
                ((symbolp blue) (eq gold blue))
                ((integerp blue) 
                 (string-equal (symbol-name gold) (format nil "~d" blue)))
                ((stringp blue) 
                 (string-equal (symbol-name gold) blue))))
              ((integerp gold)
               (cond 
                ((symbolp blue) 
                 (eq (intern (format nil "~d" gold) :tsdb) blue))
                ((integerp blue) (= gold blue))
                ((stringp blue) 
                 (string-equal (format nil "~d" gold) blue)))))))
    (cond
     ((eq level :yield)
      (let* ((gyield (derivation-yield gold))
             (gyield (if (functionp *derivations-yield-skews*)
                       (remove-if *derivations-yield-skews* gyield)
                       (remove-if
                        #'(lambda (foo)
                            (member
                             foo *derivations-yield-skews* :test #'string=))
                        gyield)))
             (byield (derivation-yield blue))
             (byield (if (functionp *derivations-yield-skews*)
                       (remove-if *derivations-yield-skews* byield)
                       (remove-if
                        #'(lambda (foo)
                            (member
                             foo *derivations-yield-skews* :test #'string=))
                       byield))))
        (loop
            for gold in gyield for blue in byield
            always (and gold blue (node-equal gold blue)))))
     ((and (null gold) (null blue)) t)
     ((and (atom gold) (atom blue)) (node-equal gold blue))
     ((or (null (derivation-daughters gold)) 
          (null (derivation-daughters blue)))
      (if *derivations-ignore-leafs-p*
        t
        (let* ((gleafs (derivation-leafs gold))
               (bleafs (derivation-leafs blue)))
          (every #'(lambda (gold blue)
                     (and (stringp gold) (stringp blue)
                          (string-equal gold blue)))
                 gleafs bleafs))))
     (t
      (when (derivation-equal 
             (derivation-root gold) (derivation-root blue) level)
        (loop
            for daughter1 in (derivation-daughters gold)
            for daughter2 in (derivation-daughters blue)
            always (derivation-equal daughter1 daughter2 level)))))))

;;;
;;; functionality to reconstruct derivation trees and report nature of failure
;;; when unification clashes.
;;;

;;;
;;; _fix_me_
;;; deal with additional reason for failure:
;;;
;;;   :constraints <path> <glb>
;;;
;;; where application of additional (glb) constraints fails.
;;;

(defun reconstruct-item (i-id i-input derivation)
  (let* ((*package* (or (find-package :disco)
                        (find-package :common-lisp-user))))
    (multiple-value-bind (result failure)
        (reconstruct derivation)
      (cond
       (failure
        (let ((*package* (find-package :tsdb))
              (*print-case* :downcase))
          (format
           t
           "~&~%(~d) `~a'~%~%  ~s~%~%"
           i-id i-input (first failure)))
        (case (third failure)
          (:noaffix
           (format t "  no affix ~a.~%" (fourth failure)))
          (:noentry
           (format t "  no lexical entry ~a.~%" (fourth failure)))
          (:norule
           (format t "  no rule ~a.~%" (fourth failure)))
          (t
           (format
            t
            "  ~(~a~) in daughter # ~d;~%  path: "
            (first (third failure)) (or (second failure) 0))
           (if (eq (first (third failure)) :cycle)
             (format
              t
              "`~{~a~^|~}'.~%"
              (second (third failure)))
             (let* ((clash (rest (third failure)))
                    (path (first clash))
                    (one (second clash))
                    (two (third clash)))
               (format
                t
                "`~{~a~^|~}'~%  values: `~(~a~)' vs. `~(~a~)'.~%"
                path one two)))))
        (format t "~%"))
       ((null failure)
        (if (boundp (find-symbol "*RECONSTRUCT-HOOK*" :tsdb))
          (let* ((name (find-symbol "*RECONSTRUCT-HOOK*" :tsdb))
                 (hook (symbol-value name)))
            (when (functionp hook)
              (funcall hook result i-input)))
          (format
           t
           "~&~%(~d) `~a' --- success.~%")))))))

(defun reconstruct (derivation &optional (dagp t) &key counter (cachep t))
  (if *derivations-reconstructor*
    (let ((hook (typecase *derivations-reconstructor*
                  (null nil)
                  (function *derivations-reconstructor*)
                  (symbol (when (fboundp *derivations-reconstructor*) 
                            (symbol-function *derivations-reconstructor*)))
                  (string (ignore-errors 
                           (symbol-function 
                            (read-from-string 
                             *derivations-reconstructor*)))))))
      (when hook (funcall hook derivation dagp)))
    (let ((derivation (cond
                        ((consp derivation) derivation)
                        ((and (stringp derivation)
                              (not (string= derivation "")))
                         (read-from-string derivation))))
          (%derivation-offset% 0)
          %edges%)
      (declare (special %derivation-offset% %edges%))
      (when derivation
        (if (or (numberp (first derivation))
                (and (consp (second derivation))
                     (numberp (first (second derivation)))))
          (catch :fail
            (reconstruct-derivation
             derivation dagp t :counter counter :cachep cachep))
          (reconstruct-cfg-derivation derivation))))))

(defun reconstruct-derivation (derivation
                               &optional (dagp t) topp
                               &key counter (cachep t))
  (declare (special %derivation-offset% %edges%) 
           #-:lkb (ignore topp))
  #+:debug
  (pprint (list %derivation-offset% derivation))
  (let* ((root (derivation-root derivation))
         (daughters (derivation-daughters derivation))
         (id (if counter (funcall counter) (derivation-id derivation)))
         (start (derivation-start derivation))
         (start (if (and (integerp start) (>= start 0))
                  start
                  %derivation-offset%))
         (end (derivation-end derivation))
         (end (if (and (integerp end) (> end start))
                end
                (+ start 1)))
         (edge 
          (or (when (and *reconstruct-cache* cachep)
                (loop
                    for edge in (gethash id *reconstruct-cache*)
                    when (and #+:lkb (eql (lkb::edge-from edge) start)
                              #+:lkb (eql (lkb::edge-to edge) end))
                    do
                      #+:lkb (setf %derivation-offset% (lkb::edge-to edge))
                      (return edge)))
              (cond
               ((and (= (length daughters) 1)
                     (null (derivation-daughters (first daughters))))
                (let* ((surface (derivation-root (first daughters)))
                       (entry 
                        (find-lexical-entry surface root id start end)))
                  (incf %derivation-offset%)
                  (if (null entry)
                    (throw :fail
                           (values
                            nil
                            (list derivation
                                  0
                                  :noentry
                                  (format nil "`~a' (`~a')" root surface))))
                    entry)))
               (t
                (let* ((items
                        (loop
                            for daughter in daughters
                            for item = (reconstruct-derivation
                                        daughter dagp topp
                                        :counter counter :cachep cachep)
                            collect item))
                       (rule (find-rule root)))
                  (if (null rule)
                    (throw :fail
                           (values nil (list derivation 0 :norule 
                                             (format nil "`~a'" root))))
                    (multiple-value-bind (result failure)
                        (instantiate-rule rule items id dagp)
                      (if (null failure)
                        result
                        (throw :fail 
                               (values nil 
                                       (list derivation 
                                             result failure))))))))))))
    (when (and *reconstruct-cache* edge)
      (push edge (gethash id *reconstruct-cache*)))
    #+:lkb
    (when (and topp (null (lkb::edge-string edge)))
      (setf (lkb::edge-string edge) 
        (format nil "~{~(~a~)~^ ~}" (lkb::edge-leaves edge))))
    edge))

;;;
;;; interface function for RASP: given a RASP derivation tree, create (LKB)
;;; edge structures from it, so we can draw and compare trees. 
;;;
;;; _fix_me_
;;; it is beginning to look like a separate Redwoods package (with it own edge
;;; structure, tree drawing, and comparison routines) would be cleaner at some
;;; point; right now, LKB, [incr tsdb()], and Redwoods code mutually depend on
;;; each other.                                                (14-aug-03; oe)
;;;
(defparameter *reconstruct-cfg-separator* #\_)

#+:lkb
(defun reconstruct-cfg-derivation (derivation &key (start 0) (id 0))
  (let* ((root (if (consp derivation) (first derivation) derivation))
         (children (when (consp derivation) (rest derivation))))
    (if (null children)
      (let* ((string (string root))
             (matches (ppcre:all-matches "</?[wW][^>]*>" string))
             (category (when (= (length matches) 4)
                         (subseq string (second matches) (third matches))))
             (break (position 
                     *reconstruct-cfg-separator*
                     (or category string)
                     :from-end t :test #'char=))
             (leaves (list (subseq (or category string) 0 break)))
             (category
              (if (numberp break)
                  (intern (subseq (or category string) (+ break 1)) :tsdb)
                root)))
      (lkb::make-edge :id id :category category
                      :from start :to (+ start 1)
                      :leaves leaves))
      (let ((edges
             (loop
                 for i from (+ id 1)
                 for child in children
                 for edge = (reconstruct-cfg-derivation
                             child :start start :id i)
                 then (reconstruct-cfg-derivation
                       child :start (lkb::edge-to edge) :id i)
                 collect edge)))
        (lkb::make-edge :id id :category root :children edges
                        :leaves (loop
                                    for edge in edges
                                    append (lkb::edge-leaves edge)))))))

(defun qtree (derivation &key (stream t))
  (let ((root (if (atom derivation) derivation (derivation-root derivation)))
        (daughters (unless (atom derivation)
                     (derivation-daughters derivation))))
    (cond
     ((null daughters)
      (format stream "\\leaf{~a}~%" root))
     (t
      (loop
          for daughter in daughters
          do
            (qtree daughter :stream stream)
          finally
             (format stream "\\branch{~d}{~a}~%" (length daughters) root))))))

;;;
;;; install conversion routine and equality predicate for derivations (uniform
;;; derivation format --- UDF).
;;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (setf (gethash :derivation *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (unless (equal string "") (read-from-string string)))))
  (setf (gethash :derivation *statistics-predicates*)
    #'(lambda (gold blue) (not (derivation-equal gold blue)))))
