;;; -*- Mode: COMMON-LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2006 -- 2009 Stephan Oepen (oe@ifi.uio.no)
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

(in-package :tsdb)

(defparameter *pcfg-include-leafs-p* t)

(defparameter *pcfg-use-preterminal-types-p* t)

(defparameter *pcfg-laplace-smoothing-p* 1e-4)

(defparameter *pcfg-geometric-mean-p* nil)

(defstruct (symbol-table 
            (:constructor make-symbol-table 
                          (&key (test #'eq)
                                (forward (make-hash-table :test test))
                                (backward (make-array 512))
                                (size 512)
                                (count 0))))
  forward backward size count)

(defmethod print-object ((object symbol-table) stream)
  (let ((n (hash-table-count (symbol-table-forward object))))
    (format 
     stream 
     "#[Symbol Table (~d forward~p; ~d backward~p of ~s)]"
     n n (symbol-table-count object) (symbol-table-count object)
     (symbol-table-size object))))

(defparameter *pcfg-symbol-table* (make-symbol-table))

(defun symbol-to-code (symbol
                       &optional (table *pcfg-symbol-table*)
                       &key rop)
  (or
   (gethash symbol (symbol-table-forward table))
   (unless rop
     (let* ((i (symbol-table-count table)))
       (setf (gethash symbol (symbol-table-forward table)) i)
       (when (>= i (symbol-table-size table))
         (setf (symbol-table-size table) (* 2 (symbol-table-size table)))
         (setf (symbol-table-backward table)
           (adjust-array 
            (symbol-table-backward table) (symbol-table-size table))))
       (setf (aref (symbol-table-backward table) i) symbol)
       (incf (symbol-table-count table))
       i))))


(defun code-to-symbol (code &optional (table *pcfg-symbol-table*))
  (when (< code (symbol-table-count table))
    (aref (symbol-table-backward table) code)))

(defun set-symbol-and-code (symbol code &optional (table *pcfg-symbol-table*))
  (setf (gethash symbol (symbol-table-forward table)) code)
  (when (>= code (symbol-table-size table))
    (setf (symbol-table-size table) (* 2 (symbol-table-size table)))
    (setf (symbol-table-backward table)
      (adjust-array (symbol-table-backward table) (symbol-table-size table))))
  (setf (aref (symbol-table-backward table) code) symbol)
  (incf (symbol-table-count table)))

(defstruct (cfr)
  id
  type
  lhs
  rhs
  (count 1)
  probability
  cfg)

(defun cfr-equal (foo bar)
  (and (= (cfr-lhs foo) (cfr-lhs bar))
       (eq (cfr-type foo) (cfr-type bar))
       (equal (cfr-rhs foo) (cfr-rhs bar))))

(defun print-cfr (cfr cfg &key (format :sussex)
                                (stream t) (prefix "") (suffix "~%"))
  (let* ((table (cfg-table cfg))
         (count (cfr-count cfr))
         (probability (cfr-probability cfr))
         (type (case (cfr-type cfr) 
                 (:root "S") (:rule "R") (:irule "I") (:word "W")))
         (lhs (cfr-lhs cfr))
         (rhs (cfr-rhs cfr))
         (*print-case* :downcase))
    (when prefix (format stream prefix))
    (case format
      (:sussex
       (format 
        stream 
        "{~a}~@[ <~,4f>~] ~a: ~a[~a] -->" 
        count probability type (code-to-symbol lhs table) lhs)
       (if (stringp rhs)
         (format stream " ~s" rhs)
         (loop
             for code in rhs
             do (format stream " ~a[~a]" (code-to-symbol code table) code))))
      (:export
       (let ((rhs
              (if (stringp rhs)
                (list rhs)
                (loop for code in rhs collect (code-to-symbol code table)))))
         (format
          stream
          "(~d) [1 (0) ~s ~{~s~^ ~}] ~,4e {~d ~d}"
          (cfr-id cfr) (code-to-symbol lhs table) rhs
          (log (cfr-probability cfr))
          (aref (cfg-counts cfg) lhs) (cfr-count cfr)))))
    (when suffix (format stream suffix))))

(defstruct (cfg)
  (table (make-symbol-table))
  (rules (make-array 4096))
  (counts (make-array 4096 :initial-element 0))
  (epsilon 1.0)
  (samples 0)
  (size 4096)
  (count 0))

(defmethod print-object ((object cfg) stream)
  (format 
   stream 
   "#[CFG (~d sample~p; ~d rule~p) <~,4e>]"
   (cfg-samples object) (cfg-samples object)
   (cfg-count object) (cfg-count object)
   (log (cfg-epsilon object))))

(defun print-cfg (cfg &key (stream t) (format :sussex))
  (if (stringp stream)
    (with-open-file (stream stream :direction :output :if-exists :supersede)
      (print-cfg cfg :stream stream :format format))
    (loop
        with *package* = (find-package :lkb)
        with table = (cfg-table cfg)
        initially
          (case format
            (:export
             (format 
              stream 
              ";;;~%;;; ~a~%;;; (~a@~a; ~a)~%;;;~%"
              cfg (current-user) (current-host) (current-time :long :pretty))
             (format stream "~%:begin :pcfg ~d.~%~%" (cfg-samples cfg))
             (format
              stream
              "*pcfg-use-preterminal-types-p* := ~:[no~;yes~].~%~%~
               *pcfg-include-leafs-p* := ~:[no~;yes~].~%~%~
               *pcfg-laplace-smoothing-p* := ~d.~%~%"
              *pcfg-use-preterminal-types-p* *pcfg-include-leafs-p*
              (if (numberp *pcfg-laplace-smoothing-p*)
                *pcfg-laplace-smoothing-p*
                (if *pcfg-laplace-smoothing-p* 1 0)))
             (format stream ":begin :rules ~d.~%~%" (cfg-count cfg))))
          
        for i from 0 to (- (cfg-size cfg) 1)
        for bucket = (aref (cfg-rules cfg) i)
        for count = (aref (cfg-counts cfg) i)
        when bucket do
          (when (eq format :sussex)
            (let ((code (cfr-lhs (first bucket))))
              (format 
               stream 
               "~a[~a] {~a}~%" 
               (code-to-symbol code table) code count)))
          (loop
              for rule in bucket 
              do 
                (print-cfr
                 rule cfg :stream stream :format format
                 :prefix (and (eq format :sussex) "  ") :suffix "~%"))
        finally
          (case format
            (:export
             (format stream "~%:end :rules.~%~%:end :pcfg.~%"))))))

(defun record-cfr (rule cfg)
  (let ((i (cfr-lhs rule)))
    (if (>= i (cfg-size cfg))
      (let ((n (setf (cfg-size cfg) (* 2 (cfg-size cfg)))))
        (setf (cfg-rules cfg) (adjust-array (cfg-rules cfg) n))
        (setf (cfg-counts cfg) 
          (adjust-array (cfg-counts cfg) n :initial-element 0))
        (push rule (aref (cfg-rules cfg) i))
        (incf (cfg-count cfg)))
      (if (cfr-cfg rule)
        (incf (cfr-count rule))
        (loop
            for foo in (aref (cfg-rules cfg) i)
            when (cfr-equal foo rule) 
            do (incf (cfr-count foo)) (return)
            finally
              (push rule (aref (cfg-rules cfg) i))
              (incf (cfg-count cfg)))))
    (incf (aref (cfg-counts cfg) i)))
  (setf (cfr-cfg rule) cfg))

(defun estimate-probabilities (cfg)
  (loop
      with laplace = (if (numberp *pcfg-laplace-smoothing-p*)
                       *pcfg-laplace-smoothing-p*
                       (and *pcfg-laplace-smoothing-p* 1))
      with n = -1
      for i from 0 to (- (cfg-size cfg) 1)
      for bucket = (aref (cfg-rules cfg) i)
      for total = (aref (cfg-counts cfg) i)
      when bucket do
        (when laplace (incf total (* (+ (length bucket) 1) laplace)))
        (loop
            for rule in bucket
            for count = (cfr-count rule)
            when laplace do (incf count laplace)
            do 
              (setf (cfr-id rule) (incf n))
              (let ((probability (/ count total)))
                (setf (cfr-probability rule) probability)
                (setf (cfg-epsilon cfg) 
                  (min (cfg-epsilon cfg) probability))))))

(defun estimate-cfg (items &key (stream *tsdb-io*))

  (loop
      with *package* = (find-package :lkb)
      with lkb::*edge-registry* = nil
      with cfg = (make-cfg)
      for item in items
      for id = (get-field :i-id item)
      for ranks = (get-field :ranks item)
      for edges = (loop
                      with *reconstruct-cache* = (make-hash-table :test #'eql)
                      for rank in ranks
                      for i = (get-field :rank rank)
                      for derivation = (get-field :derivation rank)
                      for edge = (and i (= i 1) (reconstruct derivation nil))
                      when edge
                      do (setf (lkb::edge-baz edge) derivation)
                      and collect edge)
      when edges do
        (loop
            for edge in edges
            do (edge-to-cfrs edge cfg))
        (incf (cfg-samples cfg))
      else do
        (format 
         stream
         "~&[~a] estimate-cfg(): ignoring item # ~d (no edge);~%"
         (current-time :long :short) id)
      finally 
        (estimate-probabilities cfg)
        (return cfg)))

(defun edge-to-cfrs (edge cfg)
  (let ((rule (edge-to-cfr edge cfg))
        (sponsor (when (consp (lkb::edge-baz edge))
                   (derivation-sponsor (lkb::edge-baz edge))))
        (root (edge-root edge *pcfg-use-preterminal-types-p*)))
    (when (and sponsor root)
      (let* ((table (cfg-table cfg))
             (lhs (symbol-to-code (intern sponsor :lkb) table))
             (rhs (list (symbol-to-code root table))))
        (record-cfr (make-cfr :type :root :lhs lhs :rhs rhs) cfg)))
    (record-cfr rule cfg)
    (unless (smember (cfr-type rule) '(:root :irule :word))
      (loop
          for daughter in (lkb::edge-children edge)
          do (edge-to-cfrs daughter cfg)))))

(defun edge-to-cfr (edge cfg)
  ;;
  ;; to support CFG extraction from packed forests, we use the temporary edge
  ;; slot .foo. to (destructively) associate CFG rules with edges.  hence, in
  ;; case an edge is part of more than one derivation, its CFG rule is stored
  ;; within the edge, and since the rule includes a pointer to `its' grammar,
  ;; we can even tell whether we return to a stored rule in the same context.
  ;;
  (let* ((cfr (lkb::edge-foo edge))
         (table (cfg-table cfg))
         (root (edge-root edge *pcfg-use-preterminal-types-p*))
         (daughters (lkb::edge-children edge)))
    (cond
     ((and (cfr-p cfr) (eq (cfr-cfg cfr) cfg)) cfr)
     ((and (null daughters) *pcfg-include-leafs-p*)
      (let* ((lhs (symbol-to-code root table))
             (rhs (first (lkb::edge-leaves edge))))
        (setf (lkb::edge-foo edge)
          (make-cfr :type :word :lhs lhs :rhs rhs))))
     (t
      (let* ((lhs (symbol-to-code root table))
             (rhs (loop
                      for edge in daughters
                      for root = (edge-root
                                  edge *pcfg-use-preterminal-types-p*)
                      collect (symbol-to-code root table))))
        (setf (lkb::edge-foo edge)
          (make-cfr :type :rule :lhs lhs :rhs rhs)))))))

(defun pcfg-score-edge (edge cfg &optional recursionp)
  
  (if (and (numberp (lkb::edge-score edge)) (eq (lkb::edge-foo edge) cfg))
    (values (lkb::edge-score edge) (lkb::edge-bar edge))
    (let* ((rule (edge-to-cfr edge cfg))
           (i (cfr-lhs rule))
           (match (if (eq (cfr-cfg rule) cfg)
                    rule
                    (loop
                        with i = (cfr-lhs rule)
                        with bucket = (and (< i (cfg-size cfg))
                                           (aref (cfg-rules cfg) i))
                        for foo in bucket
                        thereis (when (cfr-equal foo rule) foo))))
           (probability (if match
                          (cfr-probability match)
                          (if *pcfg-laplace-smoothing-p*
                            (let ((count  (when (< i (cfg-size cfg)) 
                                            (aref (cfg-counts cfg) i))))
                              (if (zerop count)
                                (cfg-epsilon cfg)
                                (/ 1 count)))
                            (cfg-epsilon cfg)))))
      
      (setf (lkb::edge-foo edge) cfg)
      (multiple-value-bind (score count)
          (if (smember (cfr-type rule) '(:irule :word))
            (values probability 1)
            (loop
                with result = 1
                with count = 1
                for daughter in (lkb::edge-children edge)
                do (multiple-value-bind (p n) (pcfg-score-edge daughter cfg t)
                     (setf result (* result p))
                     (incf count n))
                finally 
                  (let* ((probability (* probability result))
                         (score (if (and *pcfg-geometric-mean-p*
                                         (null recursionp))
                                    (expt 10 (/ (log probability 10) count))
                                  probability)))
                    (return (values score count)))))
        (setf (lkb::edge-score edge) score)
        (setf (lkb::edge-bar edge) count)
        (values score count)))))

#+:null
(let* ((items (analyze 
               "gold/terg/jhpstg" 
               :condition "readings > 0 && t-active > 0"
               :thorough '(:derivation) :gold "gold/terg/jhpstg"))
       (cfg (estimate-cfg items)))
  (print-cfg cfg :stream "/tmp/jhpstg.pcfg" :format :export))
