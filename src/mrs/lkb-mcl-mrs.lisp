;;; Copyright (c) 1999-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package :lkb)

;;; MRS windows
;;;
;;; for MCL - John Carroll 21/12/99
;;;
;;; using standard text editing (Fred) windows

(defun show-mrs-window (edge &optional mrs title)
  (let ((mrsstruct (or mrs (edge-mrs edge) (mrs::extract-mrs edge)))
        (stream (make-instance 'fred-window
                  :window-title (or title (show-mrs-window-title edge "Simple MRS"))
                  :view-size (make-point 500 600)
                  :scratch-p t
                  :wrap-p t)))
    (if mrsstruct
        (mrs::output-mrs1 mrsstruct
           (if *show-mrs-prolog-p* 'mrs::prolog 'mrs::simple) stream)
        (format stream "~%::: MRS structure could not be extracted~%"))
    (show-mrs-update-window stream)))

(defun show-mrs-indexed-window (edge &optional mrs title)
  (let ((mrsstruct (or mrs (edge-mrs edge) (mrs::extract-mrs edge)))
        (stream (make-instance 'fred-window
                  :window-title (or title (show-mrs-window-title edge "Indexed MRS"))
                  :view-size (make-point 500 300)
                  :scratch-p t
                  :wrap-p t)))
    (if mrsstruct
        (mrs::output-mrs1 mrsstruct 'mrs::indexed stream)
        (format stream "~%::: MRS structure could not be extracted~%"))
    (show-mrs-update-window stream)))

(defun show-mrs-scoped-window (edge &optional mrs title)
  (let* ((mrsstruct (or mrs (edge-mrs edge) (mrs::extract-mrs edge)))
         (binding-sets (mrs::make-scoped-mrs mrsstruct))
         (stream (make-instance 'fred-window
                   :window-title (or title (show-mrs-window-title edge "Scoped MRS"))
                   :view-size (make-point 700 300)
                   :scratch-p t
                   :wrap-p t)))
    (if binding-sets
        (loop for binding in binding-sets
           do
           (setf mrs::*canonical-bindings* (mrs::canonical-bindings binding))
           (mrs::output-scoped-mrs mrsstruct :stream stream))
        (format stream "~%::: MRS structure does not scope~%"))
    (show-mrs-update-window stream)))


(defun show-mrs-window-title (edge type)
   (if edge
      (format nil "Edge ~A ~A - ~A"
         (edge-id edge) (if (g-edge-p edge) "G" "P") type)
      type))

(defun show-mrs-update-window (stream)
  (file-position stream 0)
  (force-output stream)
  (fred-update stream))



#|
;;; nicer formatting for prolog-style MRS output

(in-package "MRS")
(defmethod mrs-output-start-rel ((mrsout prolog) sort handel)
  (with-slots (stream need-rel-comma) mrsout
    (when need-rel-comma (format stream ","))
    (setf need-rel-comma t)
    (format stream "~%  rel('~A',~A,[" (string-downcase sort) handel)))
(defmethod mrs-output-start-h-cons ((mrsout prolog))
  (with-slots (stream) mrsout
    (format stream ",~%  hcons([")))
(in-package :lkb)

|#
