(in-package "USER")
;;;; *****************************************************

(defparameter *ordered-mrs-rule-list* nil)

(defun clear-mrs-rules nil
  (setf *ordered-mrs-rule-list* nil))

;;; Reading in rules expressed in tdl format
;;; 
(defstruct (funny-unification)
  lhs
  rhs)

(defstruct (mrs-rule-sexp)
  value)

(defstruct (mrs-rule-predicate)
  value)

;;; Conversion rules



(defun read-mrs-rule-file nil  
   (let* ((file-name 
            (ask-user-for-existing-pathname "Rule file?")))
      (when file-name
         (read-mrs-rule-file-aux file-name))))


(defparameter *mrs-rule-fs-list* nil 
  "list of rules expressed as fs for debugging")


(defun read-mrs-rule-file-aux (file-name)
   (clear-mrs-rules)
  (let ((*tdl-expanded-syntax-function* 
         #'read-mrs-rule-expanded-syntax)
        (*readtable* (make-tdl-break-table)))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in rule file")
         (setf *mrs-rule-fs-list* nil)
         (read-mrs-rule-stream istream))))

(defun read-mrs-rule-stream (istream) 
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\#) (read-tdl-comment istream))
               (t (read-mrs-rule-entry istream))))))

(defun read-mrs-rule-entry (istream)
   (let* ((id (read istream))
          (non-def nil)
          (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following rule name ~A" id))
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following rule name ~A" id))   
       (read-char istream)
       (setf non-def
             (read-expanded-avm-def istream id))
       (check-for #\. istream id)
       (let* ((temp-fs (process-unifications non-def))
              (entry (if temp-fs
                         (mrs::construct-munge-rule-from-fs id temp-fs))))
         (push temp-fs *mrs-rule-fs-list*) ; just for debugging
         (if entry
             (push entry *ordered-mrs-rule-list*))))))

              
         

(defun read-expanded-avm-def (istream name)
  (clrhash *tdl-coreference-table*) ; parameter defined in tdltypeinput
  (let ((constraint nil))
      ;;; read-tdl-conjunction in tdltypeinput
      ;;; returns a list of path constraints
      ;;; plus funny-unifications
    (setf constraint (read-tdl-conjunction istream name nil))
    (for coref in (make-mrs-rule-coref-conditions *tdl-coreference-table*)
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
                     (for path2 in rest
                          do
                          (push (make-tdl-path-path-unif path1 path2) unifs))
                     (push (make-tdl-path-value-unif path1 *toptype*)
                           unifs))))
             coref-table)
    unifs))


(defun read-mrs-rule-expanded-syntax (istream name path-so-far)
  ;;; stuff starting with ^
  ;;; intended to be value of *tdl-expanded-syntax-function*
  ;;; and thus to be called in tdltypeinput
  (unless (eql (read-char istream) #\^)
    (error "~%read-mrs-rule-expanded-syntax called without initial ^ in ~A" 
           name))
  (let* ((next-char (peek-char t istream nil 'eof))
         (value (read istream)))
    (list (make-funny-unification 
           :lhs (create-path-from-feature-list (reverse path-so-far))
           :rhs (case next-char
                  (#\u :unique)
                  (#\( (make-mrs-rule-sexp :value value))
                  (t (make-mrs-rule-predicate :value value))))
          (make-tdl-path-value-unif 
           (reverse path-so-far) *toptype*))))

           

  
;;; *************************************************


