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

(in-package :tsdb)

(defparameter *derivations-print-lexical-type-p* nil)

(defparameter *derivations-print-tokens-p* t)

(defparameter *derivations-comparison-level* :all)

(defparameter *derivations-preterminals-test* :eq)

(defparameter *derivations-equivalences* nil)

(defparameter *derivations-yield-skews* 
  #-:logon nil
  #+:logon
  '(dbl_hyphen_punct hyphen_punct_right s_dash_pct s_dbl_dash_pct))

(defparameter *derivations-ignore-leafs-p* t)

(defparameter *derivations-ignore-tokens-p* nil)

(defparameter *derivations-ignore-spans-p* nil)

(defparameter *derivations-reconstruct-lnk-p* nil)

(defparameter *derivations-reconstruct-robust-p* nil)

(defparameter *derivations-reconstructor* nil)

(defparameter *derivation-heads* nil)

(defparameter *derivation-token-cache* nil)

(defmacro with-derivation ((output input) &body body)
  `(let ((,output
          (if (and (symbolp (first ,input))
                   (or (consp (third ,input))
                       (and (third ,input) (symbolp (third ,input)))))
            (rest ,input)
            ,input)))
     ,@body))

(defmacro derivation-sponsor (derivation)
  `(with-derivation (derivation ,derivation)
     (when (consp (second derivation))
       (first derivation))))

(defmacro derivation-id (derivation)
  `(with-derivation (derivation ,derivation)
     (if (consp (second derivation))
       (with-derivation (derivation (second derivation))
         (first derivation))
       (when (integerp (first derivation))
         (first derivation)))))

(defmacro derivation-root (derivation)
  `(with-derivation (derivation ,derivation)
     (if (consp (second derivation))
       (with-derivation (derivation (second derivation))
         (second derivation))
       (if (integerp (first derivation)) 
         (second derivation)
         (first derivation)))))

(defmacro derivation-score (derivation)
  `(with-derivation (derivation ,derivation)
     (if (consp (second derivation))
       (with-derivation (derivation (second derivation))
         (third derivation)) 
       (when (integerp (first derivation))
         (third derivation)))))

(defmacro derivation-start (derivation)
  `(with-derivation (derivation ,derivation)
     (unless *derivations-ignore-spans-p*
       (if (consp (second derivation))
         (with-derivation (derivation (second derivation))
           (fourth derivation))
         (if (integerp (first derivation)) 
           (fourth derivation)
           (when (integerp (second derivation))
             (second derivation)))))))

(defmacro derivation-end (derivation)
  `(with-derivation (derivation ,derivation)
     (unless *derivations-ignore-spans-p*
       (if (consp (second derivation))
         (with-derivation (derivation (second derivation))
           (fifth derivation))
         (if (integerp (first derivation)) 
           (fifth derivation)
           (when (integerp (second derivation))
             (third derivation)))))))

(defmacro derivation-daughters (derivation)
  `(with-derivation (derivation ,derivation)
     (if (consp (second derivation))
       (with-derivation (derivation (second derivation))
         (rest (rest (rest (rest (rest derivation))))))
       (if (integerp (first derivation))
         (rest (rest (rest (rest (rest derivation)))))
         (if (integerp (second derivation))
           (unless (stringp (third derivation))
             (rest (rest (rest derivation))))
           (rest derivation))))))

(defun derivation-depth (derivation)
  (with-derivation (derivation derivation)
    (if (consp (second derivation))
      (derivation-depth (second derivation))
      (if (null derivation)
        0
        (+ 1 (loop 
                 for son in (derivation-daughters derivation)
                 maximize (derivation-depth son)))))))

(defun derivation-preterminals (derivation)
  (with-derivation (derivation derivation)
    (unless (null derivation)
      (let ((daughters (derivation-daughters derivation)))
        (if (null (derivation-daughters (first daughters)))
          (list derivation)
          (loop 
              for daughter in daughters
              nconc (derivation-preterminals daughter)))))))

(defun derivation-yield (derivation)
  (with-derivation (derivation derivation)
    (unless (null derivation)
      (let ((daughters (derivation-daughters derivation)))
        (if (null (derivation-daughters (first daughters)))
          (list (derivation-root derivation))
          (loop 
              for daughter in daughters
              nconc (derivation-yield daughter)))))))

(defun derivation-tokenization (derivation)
  (labels ((traverse (derivation)
             (with-derivation (derivation derivation)
               (unless (null derivation)
                 (let ((daughters (derivation-daughters derivation)))
                   (if (null (derivation-daughters (first daughters)))
                     (list (derivation-start derivation)
                           (derivation-end derivation))
                     (loop 
                         for daughter in daughters
                         nconc (traverse daughter))))))))
    (remove-duplicates (traverse derivation))))

(defun derivation-ids (derivation)
  (labels ((ids (derivation)
             (with-derivation (derivation derivation)
               (unless (null derivation)
                 (let ((id (derivation-id derivation)))
                   (when id
                     (cons id
                           (loop
                               for daughter in (derivation-daughters derivation)
                               append (ids daughter)))))))))
    (sort (ids derivation) #'<)))

(defun derivation-leafs (derivation)
  (with-derivation (derivation derivation)
    (unless (null derivation)
      (let ((daughters (derivation-daughters derivation)))
        (if (null daughters)
          (list (derivation-root derivation))
          (loop 
              for daughter in daughters
              nconc (derivation-leafs daughter)))))))

(defun derivation-tokens (derivation)
  (with-derivation (derivation derivation)
    (unless (null derivation)
      (let ((daughters (derivation-daughters derivation)))
        (if (null daughters)
          (when (and (stringp (first derivation))
                     (integerp (second derivation))
                     (stringp (third derivation))
                     (null *derivations-ignore-tokens-p*))
            (loop
                for tokens = (rest (rest derivation))
                then (rest (rest tokens))
                while tokens collect (first tokens)))
          (loop 
              for daughter in daughters
              nconc (derivation-tokens daughter)))))))

(defun derivation-nodes (derivation)
  (with-derivation (derivation derivation)
    (unless (null derivation)
      (let ((id (derivation-id derivation))
            (root (derivation-root derivation))
            (score (derivation-score derivation))
            (start (derivation-start derivation))
            (end (derivation-end derivation)))
        (when id
          (cons (list id root score start end)
                (loop
                    for daughter in (derivation-daughters derivation)
                    append (derivation-nodes daughter))))))))

;;;
;;; _fix_me_
;;; in the following, we decline to fully parse the token feature structure.
;;; in principle, the +FROM and +TO values could be reentrant with other parts
;;; of the structure, in which case our naive assumptions about where to find
;;; the values (made in derivation-from() and derivation-to()) will break.
;;;                                                             (2-jul-11; oe)
(defun derivation-from (derivation start)
  (with-derivation (derivation derivation)
    (if (= (derivation-start derivation) start)
      (let* ((token (first (derivation-tokens derivation)))
             (start (and (stringp token) (search "+FROM \"" token))))
        (and start (parse-integer token :start (+ start 7) :junk-allowed t)))
      (loop
          for daughter in (derivation-daughters derivation)
          thereis (and (derivation-id daughter)
                       (derivation-from daughter start))))))

(defun derivation-to (derivation end)
  (with-derivation (derivation derivation)
    (if (= (derivation-end derivation) end)
      (let* ((token (first (last (derivation-tokens derivation))))
             (start (and (stringp token) (search "+TO \"" token))))
        (and start (parse-integer token :start (+ start 5) :junk-allowed t)))
      (loop
          for daughter in (derivation-daughters derivation)
          thereis (and (derivation-id daughter)
                       (derivation-to daughter end))))))

(defun derivation-token-ids (derivation)
  #+:lkb
  (labels ((extract (dag)
             (when (and (lkb::dag-p dag) (stringp (lkb::dag-type dag)))
               (let ((n (ignore-errors (parse-integer (lkb::dag-type dag)))))
                 (and (numberp n) n)))))
    (loop
        for token in (derivation-tokens derivation)
        for dag = (lkb::read-dag token)
        for id = (when dag
                   (lkb::existing-dag-at-end-of
                    (lkb::tdfs-indef dag) lkb::*token-id-path*))
        unless id return nil
        append (lkb::dag-to-list id :key #'extract))))

#+:null
(defun derivation-tnt-main (derivation &key prefix)
  #+:lkb
  (loop
      for token in (derivation-tokens derivation)
      for dag = (lkb::read-dag token)
      for main = (lkb::existing-dag-at-end-of
                  (lkb::tdfs-indef dag) '(lkb::+tnt lkb::+main lkb::+tag))
      unless main do 
        (format t "~@[[~a] ~]~a~%" prefix token)))

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
    (let ((gdaughters (derivation-daughters gold))
          (bdaughters (derivation-daughters blue)))
      (cond
       ((eq level :tokenization)
        (equal (derivation-tokenization gold) (derivation-tokenization blue)))
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
       ((and (atom gold) (atom blue)) (node-equal gold blue))
       ;;
       ;; the leaf nodes are surface strings (and, in principle, corresponding
       ;; token feature structures; the latter are not compared here, though).
       ;;
       ((or (null gdaughters) (null bdaughters))
        (or *derivations-ignore-leafs-p*
            (let* ((gleafs (derivation-leafs gold))
                   (bleafs (derivation-leafs blue)))
              (every #'(lambda (gold blue)
                         (and (stringp gold) (stringp blue)
                              (string-equal gold blue)))
                     gleafs bleafs))))
       ;;
       ;; preterminal nodes are lexical entries or lexical types
       ;;
       ((and (null (derivation-daughters (first gdaughters)))
             (null (derivation-daughters (first bdaughters))))
        (or (eq *derivations-preterminals-test* :ignore)
            (case *derivations-preterminals-test*
              #+:lkb
              (:type
               (labels ((type (root)
                          (let ((root (string root)))
                            (if (char= (schar root 0) #\@)
                              (intern (subseq root 1) lkb::*lkb-package*)
                              (ignore-errors
                               (type-of-lexical-entry
                                (intern root lkb::*lkb-package*)))))))
                 (let ((gtype (type (derivation-root gold)))
                       (btype (type (derivation-root blue))))
                   (and gtype btype (eq btype gtype)))))
              (:eq
               (when (node-equal (derivation-root gold) (derivation-root blue))
                 (loop
                     for daughter1 in gdaughters
                     for daughter2 in bdaughters
                     always (derivation-equal daughter1 daughter2 level))))
              (t
               (error
                "derivation-equal(): ~
                 invalid *derivations-preterminals-test* (~(~s~))."
                *derivations-preterminals-test*)))))
       (t
        (when (node-equal (derivation-root gold) (derivation-root blue))
          (loop
              for daughter1 in gdaughters
              for daughter2 in bdaughters
              always (derivation-equal daughter1 daughter2 level))))))))

(defun pprint-derivation (derivation &key (stream t))
  (let ((sponsor (derivation-sponsor derivation))
        (daughters (derivation-daughters derivation)))
    (cond
     (sponsor
      (pprint-logical-block (stream derivation :prefix "(" :suffix ")")
        (write sponsor :stream stream)
        (write-char #\space stream)
        (pprint-newline :fill stream)
        (pprint-derivation (first (rest derivation)) :stream stream)))
     (daughters
      (pprint-logical-block (stream derivation :prefix "(" :suffix ")")
        (let ((id (derivation-id derivation))
              (root (derivation-root derivation))
              (score (derivation-score derivation))
              (start (derivation-start derivation))
              (end (derivation-end derivation)))
          (when id
            (write id :stream stream)
            (write-char #\space stream))
          (if (null (derivation-daughters (first daughters)))
            (let ((*print-case* :downcase)
                  (type (when *derivations-print-lexical-type-p*
                          (type-of-lexical-entry root :tsdb))))
              (write root :stream stream)
              (when type
                (write-char #\/ stream)
                (write type :stream stream)))
            (write root :stream stream))
          (write-char #\space stream)
          (loop
              for foo in (list score start end)
              when foo do
                (write foo :stream stream)
                (write-char #\space stream)))
        (loop
            for daughter in (derivation-daughters derivation)
            do
              (pprint-newline :fill stream)
              (pprint-derivation daughter :stream stream))))
     (t
      (if *derivations-print-tokens-p*
        (write derivation :stream stream)
        (let ((form (first derivation)))
          (write-char #\( stream)
          (write form :stream stream)
          (write-char #\) stream)))))))

(defun parseval (derivation gderivation &key log)
  (declare (ignore log))
  ;;
  ;; compute the ParsEval scores for comparing a parse tree to a gold-standard
  ;; tree (for the same input string); the function returns three values: the
  ;; count of correct bracketings, the total count of bracketings in the parse
  ;; tree, and the total count of bracketings in the gold-standard tree.
  ;;
  (labels ((explode (derivation &optional (topp t))
             ;;
             ;; a recursive helper function, to traverse the tree and for each
             ;; node return a list of bracketings; we make sure to include the
             ;; bracketing for a node before bracketings extracted from its
             ;; daughters, so as to keep track of the right-most end vertex.
             ;;
             (let ((daughters (derivation-daughters derivation)))
               (nconc
                (when topp
                  (list (derivation-sponsor derivation)
                        (derivation-start (first daughters))
                        (derivation-end (first daughters))))
                (unless (and (null (rest daughters))
                             (null (derivation-daughters (first daughters))))
                  (let ((root (derivation-root derivation))
                        (start (derivation-start derivation))
                        (end (derivation-end derivation)))
                    (cons (list root start end)
                          (loop
                              for daughter in daughters
                              append (explode daughter nil))))))))
           (intersect (set1 set2 &key (test #'eql))
             ;;
             ;; much like intersection(), except well-defined on multi-sets.
             ;;
             (loop
                 with set2 = (append set2 (list (gensym "")))
                 for foo in set1
                 for bar = (member foo set2 :test test)
                 when bar 
                 do
                   (setf (first bar) (first (rest bar)))
                   (setf (rest bar) (rest (rest bar)))
                 and collect foo)))
    (let* ((test (and derivation (explode derivation)))
           (gold (and gderivation (explode gderivation)))
           (correct (intersect gold test :test #'equal)))
      (pairlis '(:test :gold :correct)
               (list (length test) (length gold) (length correct))))))

(defun tagging-accuracy (derivation gderivation)
  (labels ((type (root)
             #+:lkb
             (let ((root (string root)))
               (if (char= (schar root 0) #\@)
                 (intern (subseq root 1) lkb::*lkb-package*)
                 (ignore-errors
                  (type-of-lexical-entry
                   (intern root lkb::*lkb-package*))))))
           (explode (derivation)
             (loop
                 for preterminal in (derivation-preterminals derivation)
                 collect (list (type (derivation-root preterminal))
                               (derivation-start preterminal)
                               (derivation-end preterminal)))))
    (let* ((test (and derivation (explode derivation)))
           (gold (and gderivation (explode gderivation)))
           (correct (intersection gold test :test #'equal)))
      (pairlis '(:test :gold :correct)
               (list (length test) (length gold) (length correct))))))

(defun f-one (test gold correct)
  (let* ((precision (and test correct (divide correct test)))
         (recall (and gold correct (divide correct gold))))
    (if (and precision recall)
      (divide (* 2 precision recall) (+ precision recall))
      0)))

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
    (multiple-value-bind (result failure) (reconstruct derivation)
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
          (:notoken
           (format t "  invalid token ~a.~%" (fourth failure)))
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
           "~&~%(~d) `~a' --- success.~%"))))
      result)))

(defun reconstruct (derivation
                    &optional (dagp t)
                    &key counter 
                         (robustp *derivations-reconstruct-robust-p*)
                         (cachep t))
  
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
                         (let ((*package* (find-package :tsdb)))
                           (read-from-string derivation)))))
          (%derivation-offset% 0)
          %edges%)
      (declare (special %derivation-offset% %edges%))
      (when derivation
        (with-derivation (derivation derivation)
          (if (or (numberp (first derivation))
                  (and (consp (second derivation))
                       (with-derivation (derivation (second derivation))
                         (numberp (first derivation)))))
            (if robustp
              (or
               (ignore-errors
                (reconstruct-derivation
                 derivation dagp t
                 :counter counter :robustp nil :cachep cachep))
               (catch :fail
                 (reconstruct-derivation
                  derivation dagp t
                  :counter counter :robustp robustp :cachep cachep)))
              (catch :fail
                (reconstruct-derivation
                 derivation dagp t
                 :counter counter :robustp robustp :cachep cachep)))
            (reconstruct-cfg-derivation derivation)))))))

(defun reconstruct-derivation (derivation
                               &optional (dagp t) topp
                               &key counter robustp (cachep t))

  (declare (special %derivation-offset% %edges%))

  (let* ((root (derivation-root derivation))
         (daughters (derivation-daughters derivation))
         (id (if counter (funcall counter) (derivation-id derivation)))
         (start (derivation-start derivation))
         (start (if (and (integerp start) (>= start 0))
                  start
                  %derivation-offset%))
         (end (derivation-end derivation))
         (end (and (integerp end) (> end start) end))
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
               ((and (null (rest daughters))
                     (null (derivation-daughters (first daughters))))
                (let* ((surface (derivation-root (first daughters)))
                       #+:lkb
                       (tokens (derivation-tokens (first daughters)))
                       #-:lkb
                       (tokens nil)
                       entry length)
                  (multiple-value-setq (entry length)
                    (if (char= (char (string root) 0) #\@)
                      (find-lexical-type surface root id start end dagp)  
                      (find-lexical-entry surface root id start end dagp)))
                  (incf %derivation-offset% (or length 1))
                  (if (null entry)
                    (throw :fail
                           (values
                            nil
                            (list derivation
                                  0
                                  :noentry
                                  (format nil "`~a' (`~a')" root surface))))
                    #+:lkb 
                    (if (and (smember dagp '(:word t))
                             tokens (null *derivations-ignore-tokens-p*))
                      (let ((tokens
                             (loop
                                 for token in tokens
                                 for i from 0
                                 for dag = (lkb::read-dag token)
                                 unless dag do
                                   (throw :fail
                                     (values
                                      nil
                                      (list derivation i :notoken token)))
                                 collect dag)))
                        (multiple-value-bind (result failure)
                            (instantiate-lexical-entry entry tokens t robustp)
                          (if (null failure)
                            result
                            (throw :fail 
                              (values nil (list derivation result failure))))))
                      entry)
                    #-:lkb
                    entry)))
               (t
                (let* ((items
                        (loop
                            for daughter in daughters
                            for item = (reconstruct-derivation
                                        daughter dagp nil
                                        :counter counter :robustp robustp
                                        :cachep cachep)
                            collect item))
                       (rule (find-rule root)))
                  (if (null rule)
                    (throw :fail
                           (values nil (list derivation 0 :norule 
                                             (format nil "`~a'" root))))
                    (multiple-value-bind (result failure)
                        (instantiate-rule rule items id dagp robustp)
                      (if (null failure)
                        result
                        (throw :fail 
                               (values
                                nil (list derivation result failure))))))))))))
    (when (and *reconstruct-cache* edge)
      (push edge (gethash id *reconstruct-cache*)))
    #+:lkb
    (when (and topp (null (lkb::edge-string edge)))
      (setf (lkb::edge-string edge)
        (format nil "~{~(~a~)~^ ~}" (lkb::edge-leaves edge))))
    edge))

(defstruct node
  id root score start end from to head nucleus daughters)

(defun derivation-to-node (derivation)
  (let* ((daughters (derivation-daughters derivation))
         (daughters
          (unless (and (null (rest daughters))
                       (null (derivation-daughters (first daughters))))
            (loop
                for derivation in daughters
                collect (derivation-to-node derivation))))
         (tokens (unless daughters (derivation-tokens derivation)))
         (from (if tokens
                 (let* ((token (first tokens))
                        (from
                         (ppcre:scan-to-strings "\\+FROM \"[0-9]+\"" token)))
                   (when from (parse-integer from :start 7 :junk-allowed t)))
                 (node-from (first daughters))))
         (to (if tokens
               (let* ((token (first tokens))
                      (from
                       (ppcre:scan-to-strings "\\+TO \"[0-9]+\"" token)))
                 (when from (parse-integer from :start 5 :junk-allowed t)))
               (node-to (first (last daughters)))))
         #+:lkb
         (head (let* ((rule (intern (derivation-root derivation) :lkb))
                      (rule (gethash rule lkb::*rules*)))
                 (if rule (lkb::rule-head rule) 0)))
         #-:lkb
         (head 0)
         (head (nth head daughters))
         (head (when head (or (node-head head) head))))
    (make-node
     :id (derivation-id derivation)
     :root (derivation-root derivation) :score (derivation-score derivation)
     :start (derivation-start derivation) :end (derivation-end derivation)
     :from from :to to :head head :daughters daughters)))

(defun read-heads (file &key test (stream *tsdb-io*))
  #-:lkb
  (declare (ignore test))
  (setf *derivation-heads*
    (when (probe-file file)
      (with-open-file (input file :direction :input)
        (loop
            with *package* = (find-package :tsdb)
            for rule = (read input nil nil)
            for arity = (read input nil nil)
            for head = (read input nil nil)
            while rule
            when (or (null arity) (null head)) do
              (format
               stream "read-heads(): ignoring invalid rule `~(~a~)'.~%"
               rule)
            else do
              (let (#+:lkb
                    (id (intern rule lkb::*lkb-package*)))
                #+:lkb
                (unless (gethash id lkb::*rules*)
                  (format
                   stream "read-heads(): unknown rule `~(~a~)'.~%"
                   rule)))
            and collect 
              (cons rule (pairlis '(:arity :head) (list arity head)))))))
  #+:lkb
  (when test
    (let ((rules
           (loop for rule being each hash-value in lkb::*rules* collect rule)))
      (loop
          with *package* = (find-package :tsdb)
          for rule in (sort rules #'string< :key #'lkb::rule-id)
          for id = (intern (lkb::rule-id rule) :tsdb)
          unless (member id *derivation-heads* :key #'first)
          do
            (format
             stream "~(~a~) ~a ~a~%"
             id (length (lkb::rule-rhs rule)) (lkb::rule-head rule))))))

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

(defun qtree (derivation &key typep (stream t))
  (let* ((daughters (unless (atom derivation)
                      (derivation-daughters derivation)))
         (preterminalp (when (consp daughters)
                         (null (derivation-daughters (first daughters)))))
         (root (if (atom derivation) derivation (derivation-root derivation)))
         (root (if (and typep preterminalp) (type-of-lexical-entry root) root))
         (root (latex-escape-string (string root))))
    (cond
     ((null daughters)
      (format stream "\\leaf{\\textit{~a}}~%" root))
     (t
      (loop
          for daughter in daughters
          do (qtree daughter :stream stream :typep typep)
          finally
             (format
              stream "\\branch{~d}{~(~a~)}~%"
              (length daughters) root))))))

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
