(in-package :mrs)

;;; Variant of the mrs extraction code for the LKB
;;; This redefines some functions used by PAGE

;;; The functions in patches/mrsfns are those actually called from PAGE
;;; call extract-mrs and extract-and-output
;;; on *parse-record* for the LKB

;;; The following functions are basically PAGE fs accessors,
;;; defined here to allow the code in mrsoutput
;;; to be used with the LKB as well as PAGE

(defun get-parse-fs (edge)
  (let ((fs (tdfs-indef (edge-dag edge))))
    (setf *fragment-p* (is-fragment-fs fs))
    fs))

;;; disposed of get-category-label

(defun output-parse-tree (tree stream)
  (format stream "~A" tree))
;;; called from mrsfns.lsp
;;; this is a temporary expedient
;;; page version calls kh-parse-tree

(defun get-last-sentence nil
  lkb::*sentence*)
;; *last-parses* is not defined for tty version
;;  (car lkb::*last-parses*))  

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
  (dag-arcs dag-instance))

(defun no-path-to (path-value-res)
;;; for PAGE this is (eql path-value-res 'unify::*fail*)
  (null path-value-res))

(defun fs-type (fs)
  ;;; also defined for PAGE
  (let* ((real-type (type-of-fs fs)))
    (when 
        (and #+allegro (or (string-equal (system:getenv "USER") "aac")
                        (string-equal (system:getenv "USER") "dan")
                           (string-equal (system:getenv "USER") "danf"))
             #-allegro nil
             (search "GLBTYPE" (if (stringp real-type) real-type
                                   (symbol-name real-type))))
      ;;; if there's a glbtype, and the user is Dan, be annoying
      (dotimes (n 5)
        (lkb::lkb-beep)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!" real-type))
      (format t "~%GLBTYPE ~A in MRS" real-type)
      (dotimes (n 5)
        (format t "~%!!!!!!!!!!!!!!!!!!!!!!" real-type)))
    real-type))

(defun equal-or-subtype (type1 type2)
  (or (equal type1 type2)
      (subtype-p type1 type2)))

(defun is-top-type (val)
;;; also defined for PAGE
  (eql lkb::*toptype* val))

(defun tdl-precedes (type1 type2)
  (or (equal type1 type2)
      (subtype-p type2 type1)))

(defun last-path-feature (path)
  (cond ((typed-path-p path)
         (let ((last-tfp (car (last (typed-path-typed-feature-list path)))))
           (if (type-feature-pair-p last-tfp)
               (type-feature-pair-feature last-tfp))))
        ((path-p path) (car (last (path-typed-feature-list path))))
        (t path)))

(defun compatible-types (type1 type2)
  (if (and (null type1) (null type2))
      t ; *** fudge, since sometimes erroneously gets called with nil args
      (lkb::greatest-common-subtype type1 type2)))


(defun is-valid-type (val)
  (lkb::is-valid-type val))


