(in-package :lkb)

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

(defparameter *lkb-package* 
  (or (find-package :lkb) (find-package :common-lisp-user)))


(defun current-grammar ()
  (cond 
   ((and (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)
         (boundp (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)))
    (symbol-value (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user)))
   ((and (member :lkb *features*) 
         (find-package :lkb)
         (find-symbol "*GRAMMAR-VERSION*" :lkb)
         (boundp (find-symbol "*GRAMMAR-VERSION*" :lkb)))
    (symbol-value (find-symbol "*GRAMMAR-VERSION*" :lkb)))
   (t "anonymous")))


(defun get-test-run-information ()
  (let* ((*package* *lkb-package*))
    `((:avms . ,(- (hash-table-count *types*) (length *templates*)))
      (:sorts . 0)
      (:templates . ,(length *templates*))
      (:rules . ,(hash-table-count *rules*))
      (:lrules . ,(hash-table-count *lexical-rules*))
      (:grammar . 
       ,(or (symbol-value (find-symbol "*GRAMMAR-VERSION*" *lkb-package*))
            (symbol-value (find-symbol "*GRAMMAR-VERSION*" :common-lisp-user))
            "unknown"))
      (:application . 
       ,(format
         nil
         "SLTG ~@[vivification~]"
         chunk::*sltg-filter-p*)))))
                       

(defun parse-word (word &key load trace)
  (declare (ignore word load trace)))

(defun initialize-run (&key interactive exhaustive)
  (declare (ignore interactive exhaustive)))

(defun finalize-run (environment)
  (declare (ignore environment)))



(defun parse-item (string 
                   &key id exhaustive trace
                        readings edges derivations semantix-hook trees-hook
                        burst derivationp)
  (declare (ignore derivations id
                   readings edges derivations semantix-hook trees-hook
                   derivationp))
  
  (multiple-value-bind (return condition)
   (ignore-errors
    (let* ((*package* (find-package :lkb))
           (stream (make-string-output-stream))
           (chunk::*ebl-trace-stream* 
            (if trace
              (make-broadcast-stream *standard-output* stream)
              stream))
           (*standard-output*
            (if trace
              (make-broadcast-stream *standard-output* stream)
              stream))
           (chunk::*candidates* 0)
           tgc tcpu treal conses symbols others
           (*unifications* 0)
           (*copies* 0)
           (result 
            #+allegro
            (excl::time-a-funcall
             #'(lambda () 
                 (with-package (:chunk)
                   (chunk::ebl-ap string 
                                  :one (not exhaustive)
                                  :rebuild chunk::*sltg-filter-p*)))
             #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                 (declare (ignore ignore))
                 (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                       conses (* scons 8) symbols (* ssym 24) others sother)))
            #-allegro
            (prog1
                (progn
                  (setf treal (get-internal-real-time)
                         tcpu (get-internal-run-time)
                         tgc #+mcl (ccl:gctime) #-mcl 0
                         others #+mcl (ccl::total-bytes-allocated) #-mcl 0)
                   (with-package (:chunk)
                   (chunk::ebl-ap string 
                                  :one (not exhaustive)
                                  :rebuild chunk::*sltg-filter-p*)))
               (let ((rawgc (- #+mcl (ccl:gctime) tgc)))
                 (setf symbols 0 conses 0
                       others
                       (- #+mcl (- (ccl::total-bytes-allocated) 24) #-mcl -1 
                          others)
                       tcpu
                       (round (* (- (get-internal-run-time) tcpu) 1000)
                              internal-time-units-per-second)
                       treal
                       (round (* (- (get-internal-real-time) treal) 1000)
                              internal-time-units-per-second)
                       tgc #+mcl (round (* rawgc 1000)
                                        internal-time-units-per-second)
                       #-mcl -1)))))
      
      (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
             (output (get-output-stream-string stream))
             (readings (if (listp result) (length result) -1))
             (comment (format 
                       nil 
                       "(:candidates . ~d)" 
                       (if chunk::*sltg-filter-p* chunk::*candidates* -1)))
             (timeout (eq result :time-out)))
        (declare (ignore output))
        `((:others . ,others) (:symbols . ,symbols) 
          (:conses . ,conses)
          (:treal . ,treal) (:tcpu . ,tcpu)
          (:tgc . ,tgc)
          (:unifications . ,*unifications*) (:copies . ,*copies*)
          (:readings . ,readings)
          (:error . ,(when timeout "SLTG timeout"))
          (:comment . ,comment)
          (:results .
           ,(unless timeout
              (loop
                  for parse in result
                  for i from 0
                  for tree = (parse-to-udf parse)
                  for derivation = (write-to-string
                                    tree
                                    :escape t :case :downcase)
                  collect
                    (pairlis '(:result-id :derivation)
                             (list i derivation)))))))))
   (append
    (when condition
      (pairlis '(:readings 
                 :condition 
                 :error)
               (list -1 
                     (unless burst condition)
                     (format nil "~a" condition))))
                      
    return)))

;;;
;;; glue functions between SLTG output format and UDF
;;;

(defun parse-to-udf (parse)
  (let* ((tree (chunk::d-tree-unexpanded (second parse)))
         (root (first (first tree)))
         (body (rest tree))
         (tree (and root body (cons root body)))
         (terminals (loop
                        for terminal in (chunk::d-tree-expanded (second parse))
                        for word = (chunk::get-lex-with-id terminal)
                        when word collect word
                        else do (error "hell froze over"))))
    (declare (special terminals))
    (when tree (tree-to-udf tree 0))))


(defun tree-to-udf (tree start)
  (declare (special terminals))
  (labels ((debabel (object)
             (cond
              ((stringp object) (intern (string-upcase object) :lkb))
              ((symbolp object) (intern object :lkb))
              ((numberp object) (intern (format nil "~a" object) :lkb))
              (t (error "hell freeze over, just once again")))))
    (let* ((mother (first tree))
           (daughters (rest tree)))
      (cond
       ((null daughters)
        (let* ((morphology (pop terminals))
               (form (first morphology))
               (analysis (third morphology))
               (word (list (debabel (first analysis)) start (+ start 1)
                           (list form start (+ start 1)))))
          (if (rest analysis)
            (list (debabel (second analysis)) start (+ start 1) word)
            word)))
       (t
        (let ((daughters (loop
                             for tree in daughters
                             for i = start then (third daughter)
                             for daughter = (tree-to-udf tree i)
                             collect daughter into result
                             maximize (third daughter) into end
                             finally (return (cons result end)))))
          (append
           (list (debabel mother) start (rest daughters))
           (first daughters))))))))

;;;
;;; interface functions for reconstruction of derivations (in UDF --- unified
;;; derivation format |:-).
;;;

(defparameter *reconstruct-hook*
  #-:tty
  #'(lambda (edge &optional (i-input "reconstructed item"))
      (declare (ignore i-input))
      (when (edge-p edge)
        (display-parse-tree edge nil)))
  #+:tty
  nil)

(defun find-lexical-entry (form instance)
  (let* ((*package* *lkb-package*)
         (name (intern (if (stringp instance)
                         (string-upcase instance)
                         instance)
                       *lkb-package*))
         (instance (ignore-errors (get-lex-entry-from-id name))))
    (when instance 
      (let ((tdfs (copy-tdfs-completely (lex-entry-full-fs instance)))
            (id (lex-entry-sense-id instance)))
        (make-edge :id 0 :category (indef-type-of-tdfs tdfs)
                   :rule form :leaves (list form) :lex-ids (list id)
                   :dag tdfs)))))

(defun find-affix (type)
  (let* ((*package* *lkb-package*)
         (name (string-upcase (string type)))
         (name (intern name *lkb-package*))
         (rule (find-rule name)))
    (when (rule-p rule) rule)))

(defun find-rule (instance)
  (let* ((name (intern (if (stringp instance)
                           (string-upcase instance)
                         instance)
                       *lkb-package*))
         (rule (or (get-lex-rule-entry name)
                   (get-grammar-rule-entry name))))
    rule))

(defun instantiate-rule (rule edges)
  (let* ((*unify-debug* :return)
         (%failure% nil)
         (status 0)
         (result (rule-full-fs rule))
         (paths (rule-order rule)))
    (with-unification-context (foo)
      (loop
          while result
          for path in (rest paths)
          for edge in edges
          for tdfs = (edge-dag edge)
          for i from 0
          do
            (setf status i)
            (setf result (yadu! result tdfs path))
          finally
            (setf result (and result (restrict-and-copy-tdfs result)))))
    (if result
      (make-edge :id 0 :category (indef-type-of-tdfs result) :rule rule
                 :leaves (loop 
                             for edge in edges
                             append (edge-leaves edge))
                 :lex-ids (loop 
                             for edge in edges
                             append (edge-lex-ids edge))
                 :dag result
                 :children edges
                 :from (edge-from (first edges)) 
                 :to (edge-to (first (last edges))))
      (values status %failure%))))

(defun instantiate-preterminal (preterminal mrule &optional start end)
  ;;
  ;; _fix_me_
  ;; this hardwires some assumptions about how affixation is carried out. 
  ;;                                                        (22-apr-99  -  oe)
  ;;
  (with-unification-context (foo)
    (let* ((*unify-debug* :return)
           (%failure% nil)
           (rtdfs (rule-full-fs mrule))
           (tdfs (edge-dag preterminal))
           (result 
            (yadu! rtdfs tdfs '(args first)))
           (copy (and result (restrict-and-copy-tdfs result))))
      (if copy
        (make-edge :id 0 :category (indef-type-of-tdfs copy) :rule mrule 
                   :leaves (copy-list (edge-leaves preterminal))
                   :lex-ids (copy-list (edge-lex-ids preterminal))
                   :from start :to end
                   :dag copy
                   :children (list preterminal))
        (values nil %failure%)))))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (import '(current-grammar 
            initialize-run finalize-run
            parse-word parse-item
            *reconstruct-hook*
            find-lexical-entry find-affix find-rule
            instantiate-rule instantiate-preterminal)
          :tsdb))
