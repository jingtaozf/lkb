;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; WARNING: this file must be kept in sync with mrs/basemrs.lisp
;;; and mrs/mrscorpus.lisp

(in-package :common-lisp-user)

(defpackage :mrs 
  (:use :common-lisp)
  (:export ))


(in-package :mrs)

;;; defines structures also defined in the main MRS code, so the RMRS
;;; can be run standalone

;;; variable generation (from mrs/basemrs.lisp)

(defvar *restart-variable-generator* t)

(defvar *variable-generator* nil)

(defun create-variable-generator (&optional start)
  (let ((number (or start 0)))
    #'(lambda nil
        (incf number)
        number)))

(defun init-variable-generator ()
  (setf *variable-generator* (create-variable-generator)))

(init-variable-generator)

;;; structures (from mrs/basemrs.lisp)

(defstruct (basemrs)
  top-h
  liszt
  h-cons
  a-cons
  vcs)

;;; for composition

(defstruct (hook)
  index
  ltop
  xarg
  anchor)

(defstruct (slot)
  hook
  name)

;;;

(defstruct (rel-base)
  pred					; relation name
  flist)

(defstruct (rel (:include rel-base))
  str 
  handel                                
  anchor				; used by RMRS in version
					; without INGs                         
  parameter-strings			; the constant values
					; a junk slot used by the
                                        ; generator and comparison code
  extra                                 ; extra is a junk slot
                                        ; needed for the munging rules 
  cfrom cto
  link)					; link to surface element(s)

(defstruct (var-base)
  type
  extra) ; e.g. agreement values

(defstruct (var (:include var-base))
  id)

(defstruct (extrapair)
  feature
  value)

(defstruct (grammar-var (:include var)))
;;; a sort of placeholder variable

(defstruct (hcons)
  relation
  scarg
  outscpd)

;;; macros

(defmacro is-handel-var (var)
    ;;; test is whether the type is "h"
  `(and (var-p ,var)
       (equal (var-type ,var) "h")))

;;; test for variable equality

(defun eql-var-id (var1 var2)
  ;;; can't be macroized cos used where fn is required
  ;;; has to be `equal' since
  ;;; used for grammar vars etc in RMRS composition code
  ;;; where the id is a string
  (equal (var-id var1) (var-id var2)))

;;; from mrs/mrscorpus.lisp

(defun combine-similar-relations (liszt result-so-far test-fn)
  (if (null liszt)
      result-so-far
    (let ((test-rel (car liszt))
          (similar nil)
          (non-similar nil))
      (loop for rel in (cdr liszt)
           do
           (if (apply test-fn (list test-rel rel))
               (push rel similar)
             (push rel non-similar)))
      (combine-similar-relations non-similar
                                 (push (cons test-rel similar)
				       result-so-far)
				 test-fn))))

;;; from mrs/basemrs.lisp
;;; 
;;; LaTeX output-type class
;;;
(defun latex-escape-string (string)
  (if (and string (or (stringp string) (symbolp string)))
    (loop
        with string = (string string)
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        when (member c '(#\_ #\% #\# #\{ #\}) :test #'char=) do
          (vector-push #\\ result)
          (vector-push c result)
          (when (zerop (decf padding))
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
        else do
          (vector-push c result)
        finally
          (return result))
    string))

;;; from basemrs - the package is defined as :lkb when we're interfacing 
;;; with an LKB system but :mrs in the standalone code

(defconstant *mrs-package* :mrs)

(defun make-mrs-atom (str)
  (let ((*package* (find-package *mrs-package*)))
    (intern str *mrs-package*)))

