;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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

(defparameter *inflectional-rule-suffix*
  "_infl_rule")

(defparameter *derivations-ignore-leafs-p* t)

(defmacro derivation-id (derivation)
  `(when (integerp (first ,derivation))
     (first ,derivation)))


(defmacro derivation-root (derivation)
  `(if (integerp (first ,derivation)) 
     (second ,derivation)
     (first ,derivation)))

(defmacro derivation-score (derivation)
  `(when (integerp (first ,derivation))
     (third ,derivation)))

(defmacro derivation-start (derivation)
  `(if (integerp (first ,derivation)) 
     (fourth ,derivation)
     (when (integerp (second ,derivation))
       (second ,derivation))))

(defmacro derivation-end (derivation)
  `(if (integerp (first ,derivation)) 
     (fifth ,derivation)
     (when (integerp (second ,derivation))
       (third ,derivation))))

(defmacro derivation-daughters (derivation)
  `(if (integerp (first ,derivation))
     (rest (rest (rest (rest (rest ,derivation)))))
     (if (integerp (second ,derivation))
       (rest (rest (rest ,derivation)))
       (rest ,derivation))))

(defun derivation-depth (derivation)
  (if (null derivation)
    0
    (+ 1 (loop 
             for son in (derivation-daughters derivation)
             maximize (derivation-depth son)))))

(defun derivation-leafs (derivation)
  (unless (null derivation)
    (let ((daughters (derivation-daughters derivation)))
      (if (null daughters)
        (list (derivation-root derivation))
        (loop 
            for daughter in daughters
            nconc (derivation-leafs daughter))))))

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

(defun derivation-equal (gold blue)
  (cond
   ((and (null gold) (null blue)) t)
   ((stringp gold)
    (cond 
     ((stringp blue) (string-equal gold blue))
     ((symbolp blue) (string-equal gold (symbol-name blue)))))
   ((symbolp gold)
    (cond 
     ((symbolp blue) (eq gold blue))
     ((integerp blue) (string-equal (symbol-name gold) (format nil "~d" blue)))
     ((stringp blue) (string-equal (symbol-name gold) blue))))
   ((integerp gold)
    (cond 
     ((symbolp blue) (eq (intern (format nil "~d" gold) :tsdb) blue))
     ((integerp blue) (= gold blue))
     ((stringp blue) (string-equal (format nil "~d" gold) blue))))
   ((or (null (derivation-daughters gold)) (null (derivation-daughters blue)))
    (if *derivations-ignore-leafs-p*
      t
      (let* ((gleafs (derivation-leafs gold))
             (bleafs (derivation-leafs blue)))
        (every #'(lambda (gold blue)
                   (and (stringp gold) (stringp blue)
                        (string-equal gold blue)))
               gleafs bleafs))))
   (t
    (and (derivation-equal (derivation-root gold) (derivation-root blue))
         (every #'derivation-equal 
                (derivation-daughters gold) 
                (derivation-daughters blue))))))

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

(defun reconstruct (derivation &optional (dagp t))
  (let ((derivation (cond
                      ((consp derivation) derivation)
                      ((and (stringp derivation) (not (string= derivation "")))
                       (read-from-string derivation)))))
    (when derivation
      (catch :fail
        (reconstruct-derivation derivation dagp)))))

(defun reconstruct-derivation (derivation &optional (dagp t))
  (let* ((root (derivation-root derivation))
         (daughters (derivation-daughters derivation))
         (princes (and (= (length daughters) 1) 
                       (derivation-daughters (first daughters))))
         (id (derivation-id derivation))
         (start (derivation-start derivation))
         (end (derivation-end derivation))
         (edge 
          (or (when *reconstruct-cache* (gethash id *reconstruct-cache*))
              (cond
               ((and (= (length daughters) 1) (null princes))
                (let* ((surface (derivation-root (first daughters)))
                       (entry 
                        (find-lexical-entry surface root id start end)))
                  (if (null entry)
                    (throw :fail
                           (values
                            nil
                            (list derivation
                                  0
                                  :noentry
                                  (format nil "`~a' (`~a')" root surface))))
                    entry)))
               ((and (= (length princes) 1)
                     (null (derivation-daughters (first princes)))
                     (inflectional-rule-p derivation))
                (let* ((affix root)
                       (fs (and affix (find-affix affix)))
                       (surface (derivation-root (first princes)))
                       (entry (find-lexical-entry 
                               surface 
                               (derivation-root (first daughters))
                               id start end)))
                  (cond
                   ((null fs)
                    (throw :fail
                           (values
                            nil
                            (list derivation
                                  0
                                  :noaffix
                                  (format nil "`~a'"  affix)))))
                   ((null entry)
                    (throw :fail
                           (values
                            nil
                            (list derivation
                                  0
                                  :noentry
                                  (format nil "`~a' (`~a')" root surface)))))
                   (t
                    (multiple-value-bind (result failure)
                        (instantiate-preterminal entry fs id start end dagp)
                      (if failure
                        (throw :fail 
                               (values nil (list derivation result failure)))
                        result))))))
               (t
                (let* ((items
                        (loop
                         for daughter in daughters
                         for item = (reconstruct-derivation daughter dagp)
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
      (setf (gethash id *reconstruct-cache*) edge))
    edge))

(defun qtree (derivation &key (stream t))
  (let ((root (derivation-root derivation))
        (daughters (derivation-daughters derivation)))
    (if (null daughters)
      (format stream "\\leaf{~a}~%" root)
      (loop
          for daughter in daughters
          do
            (qtree daughter :stream stream)
          finally
             (format stream "\\branch{~d}{~a}~%" (length daughters) root)))))

;;;
;;; install conversion routine and equality predicate for derivations (uniform
;;; derivation format --- UDF).
;;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  #+:null         
  (setf (gethash :derivation *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (read-from-string string))))
  (setf (gethash :derivation *statistics-predicates*)
    #'(lambda (gold blue) (not (derivation-equal gold blue)))))
