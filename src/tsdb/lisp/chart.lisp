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

(defun write-charts (parse-id language &key cache)
  (let ((lexicon (pg::parser-chart (pg::get-parser :lexicon)))
        (syntax (pg::parser-chart (pg::get-parser :syntax))))
    (when (and lexicon *tsdb-write-lexicon-chart-p*)
      (ignore-errors 
       (write-chart lexicon parse-id :lexicon language :cache cache)))
    (when (and syntax *tsdb-write-syntax-chart-p*)
      (ignore-errors 
       (write-chart syntax parse-id :syntax language :cache cache)))))
     
     
(defun write-chart (chart parse-id parser language &key cache)
  (let ((passives (pg::chart-passive-items chart))
        (sactives (pg::chart-active-items-starting-at chart))
        (eactives (pg::chart-active-items-ending-at chart))
        edges)
    (dotimes (i (array-dimension passives 0))
      (dotimes (j (array-dimension passives 1))
        (dolist (edge (aref passives i j))
          (push edge edges))))
    (dotimes (i (array-dimension sactives 0))
      (dolist (edge (aref sactives i))
        (push edge edges)))
    (dotimes (i (array-dimension eactives 0))
      (dolist (edge (aref eactives i))
        (push edge edges)))
    (let* ((edges (remove-if-not #'pg::combo-item-p edges))
           (edges (sort edges #'< :key #'pg::combo-item-id)))
      (dolist (edge edges)
        (write-edge edge parser parse-id language :cache cache)))))

(defun write-edge (edge parser parse-id language &key cache)

  (let* ((id (pg::combo-item-id edge))
         (e-id (+ (* parse-id 100000) id))
         (e-name (get-informative-item-label edge))
         (e-type (format nil "~(~a~)" (pg::combo-item-itype edge)))
         (e-status (if (pg::item-passive-p edge (pg::get-parser parser)) 0 1))
         (e-result 
          (if (and (pg::item-passive-p edge (pg::get-parser parser))
                   (pg::is-result-p :ignore edge (pg::get-parser parser)))
            1
            0))
         (e-start (pg::item-start edge))
         (e-end (pg::item-end edge))
         (daughters (pg::item-daughters edge))
         (e-daughters 0)
         (epsilons (pg::item-epsilon-descendants edge))
         (e-epsilons (length epsilons)))
    (when daughters
      (dotimes (i (array-dimension daughters 0))
        (when (aref daughters i) (incf e-daughters))))
    (let* ((query "insert into edge values")
           (query
            (format
             nil
             "~a ~
              ~d ~d ~(~s~) ~s ~
              ~s ~d ~d ~d ~
              ~d ~d ~d"
             query
             e-id parse-id (string parser) e-name
             e-type e-status e-result
             e-start e-end e-daughters e-epsilons)))
      (call-tsdb query language :cache cache))
    (when daughters
      (write-daughters daughters parse-id e-id language :cache cache))))

(defun write-daughters (daughters parse-id e-id language &key cache)
  (dotimes (d-position (array-dimension daughters 0))
    (let* ((daughter (aref daughters d-position))
           (d-id (when (pg::combo-item-p daughter) 
                   (+ (* parse-id 100000) (pg::combo-item-id daughter))))
           (query "insert into daughter values")
           (query (format
                   nil
                   "~a ~d ~d ~d"
                   query e-id d-position d-id)))
      (when (pg::combo-item-p daughter)
        (call-tsdb query language :cache cache)))))

(defmacro edge-e-id (edge)
  `(nth 0 ,edge))

(defmacro edge-parse-id (edge)
  `(nth 1 ,edge))

(defmacro edge-e-parser (edge)
  `(nth 2 ,edge))

(defmacro edge-e-name (edge)
  `(nth 3 ,edge))

(defmacro edge-e-type (edge)
  `(nth 4 ,edge))

(defmacro edge-e-status (edge)
  `(nth 5 ,edge))

(defmacro edge-result (edge)
  `(nth 6 ,edge))

(defmacro edge-e-start (edge)
  `(nth 7 ,edge))

(defmacro edge-e-end (edge)
  `(nth 8 ,edge))

(defmacro edge-e-daughter (edge)
  `(nth 9 ,edge))

(defmacro edge-e-epsilons (edge)
  `(nth 10 ,edge))

(defmacro passive-edge-p (edge)
  `(zerop (edge-e-status ,edge)))

(defmacro daughter-e-id (daughter)
  `(nth 0 ,daughter))

(defmacro daughter-d-position (daughter)
  `(nth 1 ,daughter))

(defmacro daughter-d-id (daughter)
  `(nth 2 ,daughter))

(defmacro counter-active (counter)
  `(first ,counter))

(defmacro counter-passive (counter)
  `(rest ,counter))

(defvar *tsdb-chart-cache* (make-hash-table :test #'equal))

(defun read-edges (data &key meter)
  (if (gethash data *tsdb-chart-cache*)
    (gethash data *tsdb-chart-cache*)
    (let* ((directory (find-tsdb-directory data))
           (cache (concatenate 'string directory "edge.cache"))
           (file (concatenate 'string directory "edge.lisp"))
           (edges-by-parse (make-hash-table))
           (daughters-by-parse (make-hash-table))
           (edges-by-name (make-hash-table))
           edges)
      (if (probe-file cache)
        (with-open-file (in cache :direction :input)
          (let ((*package* (find-package lex::*lex-package*)))
            (do ((rule (read in nil nil) (read in nil nil)))
                ((null rule))
              (setf (gethash (first rule) edges-by-name) (rest rule)))))
        (when (probe-file file)
          (with-open-file (in file :direction :input)
            (do ((edge (read in nil nil) (read in nil nil)))
                ((null edge))
              (push edge edges)
              #+:cray
              (setf 
                (gethash (edge-parse-id edge) edges-by-parse) 
                edge)
              (let* ((name (string-upcase (edge-e-name edge)))
                     (name (intern name lex::*lex-package*)))
                (unless (gethash name edges-by-name)
                  (setf (gethash name edges-by-name) (cons 0 0)))
                (if (passive-edge-p edge) 
                  (incf (counter-passive (gethash name edges-by-name)))
                  (incf (counter-active (gethash name edges-by-name)))))))))
      (setf (gethash data *tsdb-chart-cache*)
        (list edges-by-name edges-by-parse daughters-by-parse)))))

(defun rule-statistics (data
                        &key file (format :tcl) logscale)

  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (table (first (read-edges data)))
         (rules (pg::combo-parser-syn-rules (pg::get-parser :syntax)))
         (rules
          (remove-if #'(lambda (foo)
                         (< (array-dimension (pg::item-daughters foo) 0) 2))
                     rules))
         (rules (map 'list #'pg::combo-item-index rules))
         (rules (sort rules #'string< :key #'symbol-name))
        names actives passives)
    (do* ((rules rules (rest rules))
          (rule (first rules) (first rules))
          (counter (gethash rule table) (gethash rule table)))
        ((null rules))
      (when counter
        (push rule names)
        (push (counter-active counter) actives)
        (push (counter-passive counter) passives)))
    (let* ((names (nreverse names))
           (indices (loop for i from 1 to (length names) collect i))
           (passives (nreverse passives))
           (actives (nreverse actives))
           (dactives (map 'list #'- actives passives)))
      (format 
       stream
       "barchart -font {Helvetica 10 bold} -plotbackground white \\~%  ~
          -width 18c -height 12c -barmode stacked -barwidth 0.7 \\~%  ~
          -title \"Rule Postulation and Success Distribution\" \\~%  ~
          -invertxy yes -rightmargin 10~%")
      (format stream "legend -hide yes~%")
      (format
       stream
       "axis x -stepsize 1 -tickfont {Helvetica 9} -subdivisions 1 \\~%  ~
        -labels ~a~%"
       (list2tcl (map 'list #'symbol-name names)))
      (format
       stream
       "axis y -title \"passive # active chart edges\" \\~%  ~
        -logscale ~:[no~;yes~]~%" logscale)
      (format stream "data x1 ~a~%" (list2tcl indices))
      (format stream "data y1 ~a~%" (list2tcl passives))
      (format stream "data y2 ~a~%" (list2tcl dactives))
      (format 
       stream 
       "element e1 -xdata x1 -ydata y1 -stipple \"\" -fg black -relief flat~%")
      (format 
       stream 
       "element e2 -xdata x1 -ydata y2 -stipple gray50 -relief flat~%")
      (list indices passives actives names))
    (force-output stream)
    (when file (close stream))))

       

#+:junk
(defun read-daughters (&key (data *tsdb-data*))
  (let* ((directory (find-tsdb-directory data))
         (file (concatenate 'string directory "daughter.lisp"))
         daughters)
    (when (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((daughter (read in nil nil) (read in nil nil)))
            ((null daughter))
          (push daughter daughters)
          (setf 
            (gethash (daughter-e-id daughter) *tsdb-chart-daughters-by-parse*) 
            daughter))))
    daughters))

