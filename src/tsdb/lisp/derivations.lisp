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

(defmacro derivation-root (derivation)
  `(first ,derivation))

(defmacro derivation-start (derivation)
  `(second ,derivation))

(defmacro derivation-end (derivation)
  `(third ,derivation))

(defmacro derivation-daughters (derivation)
  `(rest (rest (rest ,derivation))))

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
                ((consp derivation) (derivation-root derivation))
                (t 
                 (error 
                  "inflectional-rule-p(): invalid call `~s'." 
                  derivation))))
         (break (- (length root) (length *inflectional-rule-suffix*))))
    (when (string-equal 
           (subseq root break)
           *inflectional-rule-suffix*)
      (subseq root 0 break))))

(defun derivation-equal (gold blue)
  (cond
   ((and (null gold) (null blue)) t)
   ((and (stringp gold) (stringp blue))
    (string-equal gold blue))
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
  (let* ((*package* (or (find-package "DISCO")
                        (find-package "COMMON-LISP-USER"))))
    (multiple-value-bind (result failure)
        (reconstruct derivation)
      (cond
       (failure
        (format
         t
         "~&~%(~d) `~a'~%~%  ~s~%~%"
         i-id i-input (first failure))
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
            (first (third failure)) (second failure))
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
       ((and (null failure) 
             (boundp (find-symbol "*RECONSTRUCT-HOOK*" :tsdb)))
        (let ((hook (symbol-value (find-symbol "*RECONSTRUCT-HOOK*" :tsdb))))
          (when (functionp hook)
            (funcall hook result i-input))))))))

(defun reconstruct (derivation)
  (let ((derivation (cond
                      ((consp derivation) derivation)
                      ((and (stringp derivation) (not (string= derivation "")))
                       (read-from-string derivation)))))
    (when derivation
      (catch :fail
        (reconstruct-derivation derivation)))))

(defun reconstruct-derivation (derivation)
  (let* ((root (derivation-root derivation))
         (daughters (derivation-daughters derivation))
         (princes (and (= (length daughters) 1) 
                       (derivation-daughters (first daughters)))))
    (cond
     ((and (= (length daughters) 1) (null princes))
      (let* ((surface (derivation-root (first daughters)))
             (entry (find-lexical-entry surface root)))
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
                     (derivation-root (first daughters)))))
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
              (instantiate-preterminal entry fs)
            (if failure
              (throw :fail (values nil (list derivation result failure)))
              result))))))
     (t
      (let* ((items
              (loop
                  for daughter in daughters
                  for item = (reconstruct-derivation daughter)
                  collect item))
             (rule (find-rule root)))
        (if (null rule)
          (throw :fail
            (values nil (list derivation 0 :norule (format nil "`~a'" root))))
          (multiple-value-bind (result failure)
              (instantiate-rule rule items)
            (if (null failure)
              result
              (throw :fail 
                (values nil (list derivation result failure)))))))))))

;;;
;;; install conversion routine and equality predicate for derivations (uniform
;;; derivation format --- UDF).
;;;
(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (setf (gethash :derivation *statistics-readers*) #'read-from-string)
  (setf (gethash :derivation *statistics-predicates*)
    #'(lambda (gold blue) (not (derivation-equal gold blue)))))
