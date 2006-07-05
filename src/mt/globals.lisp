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

(defparameter %mrs-copy-cache% nil)

(defparameter %mrs-construction-cache% nil)

(defparameter *transfer-rule-sets* nil)

(defparameter *transfer-lexicon* nil)

(defparameter *transfer-triggers* (make-hash-table))

(defparameter *transfer-result-filter* '(:complete))

(defparameter *mtr-filter-path* (list (mrs::vsym "FILTER")))

(defparameter *mtr-context-path* (list (mrs::vsym "CONTEXT")))

(defparameter *mtr-input-path* (list (mrs::vsym "INPUT")))

(defparameter *mtr-output-path* (list (mrs::vsym "OUTPUT")))

(defparameter *mtr-flags-path* (list (mrs::vsym "FLAGS")))

(defparameter *mtr-optional-path* (list (mrs::vsym "OPTIONAL")))

(defparameter *mtr-fail-path* (list (mrs::vsym "FAIL")))

(defparameter *mtr-exhaustive-path* (list (mrs::vsym "EXHAUSTIVE")))

(defparameter *mtr-equal-path* (list (mrs::vsym "EQUAL")))

(defparameter *mtr-subsume-path* (list (mrs::vsym "SUBSUME")))

(defparameter *mtr-trigger-path* (list (mrs::vsym "TRIGGER")))

(defparameter *mtr-rank-path* (list (mrs::vsym "RANK")))

(defconstant *mtr-skolem-property* (mrs::vsym "SKOLEM"))

(defconstant *mtr-scratch-property* (mrs::vsym "SCRATCH"))

(defconstant *mtr-mark-property* (mrs::vsym "MARK"))

(defconstant *mtr-ditch-property* (mrs::vsym "DITCH"))

(defconstant *mtr-upcase-operator* (mrs::vsym "+upcase+"))

(defconstant *mtr-downcase-operator* (mrs::vsym "+downcase+"))

(defconstant *mtr-copy-operator* (mrs::vsym "+copy+"))

(defconstant *mtr-true-type* (mrs::vsym "+"))

(defconstant *mtr-false-type* (mrs::vsym "-"))

(defparameter *semi-u-type* "u")

(defparameter *semi-p-type* "p")

(defparameter *semi-h-type* "h")

(defparameter *semi-i-type* "i")

(defparameter *semi-e-type* "e")

(defparameter *semi-x-type* "x")

(defparameter *semi-fragment-left* (mrs::vsym "L-HNDL"))

(defparameter *semi-fragment-right* (mrs::vsym "R-HNDL"))

(defparameter *semi-punctuation-relations*
  (list "colon_rel" "comma_rel" "period_rel" 
        "exclamation_point_rel" "question_mark_rel"
        (mrs::vsym "colon_rel") (mrs::vsym "comma_rel")
        (mrs::vsym "hyphen_rel") (mrs::vsym "period_rel")
        (mrs::vsym "exclamation_point_rel") (mrs::vsym "question_mark_rel")))

(defparameter *semi-token-relations*
  (list "token_rel" (mrs::vsym "token_rel")))
