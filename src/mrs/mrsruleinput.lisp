;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)
;;;; *****************************************************

;;; (defparameter *ordered-mrs-rule-list* nil)
;;; in mrsglobals

(defun clear-mrs-rules nil
  (setf *ordered-mrs-rule-list* nil))

(defparameter *gen-rule-list* nil)

(defun clear-gen-rules nil
  (setf mrs::*gen-rule-ids* nil)
  (setf *gen-rule-list* nil))

;;; Reading in rules expressed in tdl format
;;; 
(defstruct (funny-unification)
  lhs
  rhs)

(defstruct (mrs-rule-sexp)
  value)

(defstruct (mrs-rule-predicate)
  value)

(defstruct (mrs-rule-constant)
  value)

;;; Conversion rules

(defun read-gen-rule-file nil  
   (let* ((file-name 
            (ask-user-for-existing-pathname "Heuristics file?")))
      (when file-name
         (read-mrs-rule-file-aux file-name t))))

(defun read-mrs-rule-file nil  
   (let* ((file-name 
            (ask-user-for-existing-pathname "Rule file?")))
      (when file-name
         (read-mrs-rule-file-aux file-name))))


(defparameter *mrs-rule-fs-list* nil 
  "list of rules expressed as fs for debugging")

(defun read-mrs-rule-file-aux (file-names &optional generator-p)
  (unless (listp file-names)
    (setf file-names (list file-names)))
  
  (let ((mrs::*variable-generator* (if generator-p 
                                mrs::*variable-generator*
                                (mrs::create-variable-generator 10000))))
    (when (every #'(lambda (file-name)
                     (and file-name 
                          (probe-file file-name)))
                 file-names)
      (if generator-p
          (clear-gen-rules) 
        (clear-mrs-rules))
      (let ((*tdl-expanded-syntax-function* 
             #'read-mrs-rule-expanded-syntax)
            (*readtable* (make-tdl-break-table)))
        (setf *mrs-rule-fs-list* nil)
        (loop for file in file-names
             do
             (with-open-file 
                 (istream file :direction :input)
               (format t "~%Reading in rule file ~A" file)
               (read-mrs-rule-stream istream generator-p)))))))

(defun read-mrs-rule-stream (istream generator-p) 
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\#) (read-tdl-comment istream))
               (t (read-mrs-rule-entry istream generator-p))))))

(defun read-mrs-rule-entry (istream generator-p)
   (let* ((id (read istream))
          (non-def nil)
          (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following rule name ~A" id))
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof))
           (normal-unifs nil)
           (funny-unifs nil))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following rule name ~A" id))   
       (read-char istream)
       (setf non-def
             (read-expanded-avm-def istream id))
       (check-for #\. istream id)
       (loop for unif in non-def
            do
            (if (funny-unification-p unif)
                (push unif funny-unifs)
                (push unif normal-unifs)))
       (let* ((temp-fs (process-unifications normal-unifs))
              (entry (if temp-fs
                         (if generator-p
                           (mrs::construct-gen-rule-from-fs id temp-fs
                                                            funny-unifs)
                           (mrs::construct-munge-rule-from-fs 
                            id temp-fs funny-unifs 
                            mrs::*variable-generator*)))))
         (push temp-fs *mrs-rule-fs-list*) ; just for debugging
         (if entry
             (if generator-p
                 (push entry *gen-rule-list*)
                 (push entry *ordered-mrs-rule-list*)))))))

              
         

(defun read-expanded-avm-def (istream name)
  (clrhash *tdl-coreference-table*) ; parameter defined in tdltypeinput
  (let ((constraint nil))
      ;;; read-tdl-conjunction in tdltypeinput
      ;;; returns a list of path constraints
      ;;; plus funny-unifications
    (setf constraint (read-tdl-conjunction istream name nil nil))
    (loop for coref in (make-mrs-rule-coref-conditions *tdl-coreference-table*)
         do
      (push coref constraint))
    constraint))

(defun make-mrs-rule-coref-conditions (coref-table)
  ;;; the coref table is a list of paths, indexed by
  ;;; a coreference atom.  
  (let ((unifs nil))
    (maphash #'(lambda (index value)
                 (declare (ignore index))
                 (let ((path1 (car value))
                       (rest (cdr value)))
                   (if rest
                     (loop for path2 in rest
                          do
                          (push (make-tdl-path-path-unif path1 path2 nil) unifs))
                     (push (make-tdl-path-value-unif path1 *toptype* nil)
                           unifs))))
             coref-table)
    unifs))


(defun read-mrs-rule-expanded-syntax (istream name path-so-far in-default-p)
  ;;; stuff starting with ^
  ;;; intended to be value of *tdl-expanded-syntax-function*
  ;;; and thus to be called in tdltypeinput
  (when in-default-p
    (error "~%read-mrs-rule-expanded-syntax called inside default in ~A" 
           name))
  (unless (eql (read-char istream) #\^)
    (error "~%read-mrs-rule-expanded-syntax called without initial ^ in ~A" 
           name))
  (let* ((next-char (peek-char t istream nil 'eof))
         (value (read istream)))
    (list (make-funny-unification 
           :lhs (reverse path-so-far)
           :rhs (case next-char
                  (#\u :unique)
                  (#\c (make-mrs-rule-constant :value value))
                  (#\( (make-mrs-rule-sexp :value value))
                  (t (make-mrs-rule-predicate :value value))))
          (make-tdl-path-value-unif 
           (reverse path-so-far) *toptype* nil))))

           

  
;;; *************************************************


