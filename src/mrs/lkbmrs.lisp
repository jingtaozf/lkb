(in-package "MRS")

;;; Variant of the mrs extraction code for the LKB
;;; This redefines some functions used by PAGE

;;; The functions in patches/mrsfns are those actually called from PAGE
;;; call extract-mrs and extract-and-output
;;; on *parse-record* for the LKB

;;; The following functions are basically PAGE fs accessors,
;;; defined here to allow the code in mrsoutput
;;; to be used with the LKB as well as PAGE

(defun get-parse-fs (edge)
  (tdfs-indef (edge-dag edge)))

(defun deref (fs)
  ;;; just a guess, but can't do any harm ...
  (follow-pointers fs))

(defun cyclic-p (fs)
  ;;; new version of unifier can't create cyclic fs
  (declare (ignore fs))
  nil)
  
(defun path-value (fs path)
  (existing-dag-at-end-of fs path))

(defun is-disjunctive-fs (fs)
  (declare (ignore fs))
  ;  damn well better not be !
  nil)

(defun is-valid-fs (fs)
  (and fs (dag-p fs)))

(defun fs-arcs (dag-instance)
   (unless (is-atomic dag-instance)
             (dag-arcs dag-instance)))

(defun no-path-to (path-value-res)
;;; for PAGE this is (eql path-value-res 'unify::*fail*)
  (null path-value-res))

(defun fs-type (fs)
  ;;; also defined for PAGE
  (type-of-fs fs))

(defun create-type (type)
  ;;; also defined for PAGE
  (if (listp type)
      (car type)
    type))

#|
  (if (listp type)
      (let ((string-converted 
             (mapcar #'(lambda (x) (if (stringp x)
                                       (read-from-string x)
                                     x))
                     type)))
        (if (cdr string-converted)
            string-converted ; might be an atomic disjunction
          (car string-converted)))
    (if (stringp type)
        (read-from-string type)
      type)))
|#

;;; called from mrs-to-vit
 
(defun is-top-type (val)
;;; also defined for PAGE
  (eql USER::*toptype* val))

