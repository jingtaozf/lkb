;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TREES -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  $RCSfile$
;;;    module: MRS/trees
;;; $Revision$
;;;   $Author$
;;;     $Date$
;;;    $State$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file is extracted from acl-tree.lisp and requires that it was loaded
;;; after that file, which basically requires the whole trees package to be 
;;; loaded
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is for PAGE only

(in-package "TREES")

;;
;; Define a frame class for MRS window
;;

(clim:define-application-frame mrs-frame ()
  ((fs :initform nil
       :accessor mrs-fs))
  (:panes
   (display :application
	    :display-function 'display-mrs
	    :text-cursor nil
	    :width 400 :height 400
	    :text-style 
            (clim:parse-text-style 
             (list 
              :sans-serif :roman 12))
	    :borders t
	    :display-after-commands :no-clear
	    :incremental-redisplay t))
  (:layouts
   (:default display)))

(define-mrs-frame-command (com-exit-mrs :menu "Exit")
    ()
  (clim:frame-exit clim:*application-frame*))

;; 
;; Add [MRS] button
;;

(define-tree-frame-command (com-mrs-tree :menu "MRS")
    ()
  (draw-mrs (tree-frame-nodes clim:*application-frame*) nil))

;;
;; Open an MRS window for a parse
;;

(defmethod draw-mrs ((tree tree) vitp &key &allow-other-keys)
  (declare (ignore vitp))
  (let ((frame (clim:make-application-frame 'mrs-frame
					    :pretty-name "Parse Results")))
    (setf (mrs-fs frame) (node-fs tree))
    (mp:process-run-function "tree" #'clim:run-frame-top-level frame)))

;; This is like mrs::extract-and-output except it takes a feature structure
;; and a stream argument and sends the output to that stream.

(defun display-mrs (frame stream)
  (let ((sem-fs (path-value (mrs-fs frame) mrs::*initial-semantics-path*)))
    (if (mrs::is-valid-fs sem-fs)
        (let ((mrs-struct (mrs::construct-mrs sem-fs))
	      (*standard-output* stream))
          (clim:with-end-of-line-action (stream :allow)
            (clim:with-end-of-page-action (stream :allow)
	      (mrs::output-mrs mrs-struct 'mrs::simple)))))))

