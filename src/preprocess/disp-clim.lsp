;;; Copyright (c) 2005
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defparameter *maf-menu* nil)

;; CLIM display routines
(defun print-maf-tokens nil
  (let ((frame (clim:make-application-frame 'xml-maf-tokens)))
    (clim:run-frame-top-level frame)))


(defun print-maf-wordforms nil
  (let ((frame (clim:make-application-frame 'xml-maf-wordforms)))
    (clim:run-frame-top-level frame)))


(defun disp-xml-maf-tokens (mframe stream &key max-width max-height)
  (declare (ignore mframe max-width max-height))
  (if *tchart*
      (clim:with-text-style (stream (lkb-parse-tree-font))
	(format stream "~a" (pretty-print-xml (tchart-to-maf-tokens *tchart*))))
    (format stream "No tchart (please parse something first).")))

(define-lkb-frame xml-maf-tokens
    ()
  :display-function 'disp-xml-maf-tokens 
  :width 400 
  :height 400)

(define-lkb-frame xml-maf-wordforms
    ()
  :display-function 'disp-xml-maf-wordforms 
  :width 400 
  :height 400)

(defun disp-xml-maf-wordforms (mframe stream &key max-width max-height)
  (declare (ignore mframe max-width max-height))
  (if *tchart*
      (clim:with-text-style (stream (lkb-parse-tree-font))
	(format stream "~a" (pretty-print-xml (tchart-to-maf-wordforms *tchart*))))
    (format stream "No tchart (please parse something first).")))

;; end of CLIM display routines

;; TODO: LUI display???