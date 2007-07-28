;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :saf)

(defun lxml-rmrs-to-rmrs (lxml)
  (if lxml
      (list
       (make-saf-fv
	:feature :|rmrs|
	:value (mrs::read-rmrs 
		(car 
		 ;; necessary since read-rmrs expects :mrs-interned symbols
		 (lxml:shift-package lxml :mrs)) :rasp)))))

(setf *HOOK-lxml-rmrs-to-mrs-fn* #'lxml-rmrs-to-rmrs)

(defun dump-sentence-analyses (s &key (stream t))
  (dump-sentence-analyses2 :s-id (saf-edge-id s) :stream stream))

;;based on mrs::output-mrs-after-parse
(defun dump-sentence-analyses2 (&key (s-id) 
				     (stream t))
  (let ((*print-circle* nil))
    (loop for edge in lkb::*parse-record* 
	do
	  (let ((mrs (mrs::extract-mrs edge)))
	    (format stream "<annot type='parse' deps='~a'>" ;;move edge into content
		    (or s-id ""))
	    (format stream "<slot name='edge'>~a</slot>" (lkb::xml-escape (lkb::2-str (lkb::edge-id edge))))
	    ;;(format stream "~&~A~&" 
	    ;;(lkb::parse-tree-structure edge))
	    (let ((mrs::*write-compact-xml* t))
	      (setf mrs::*write-compact-xml* mrs::*write-compact-xml*) ;;avoid compiler warning
	      (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::xml stream))
	    (format stream "~&</annot>")))))

(defun resolve-mrs2 (x edge)
  (cond
   ((equal nil x)
    (unless (and (mrs::get-semi) (mrs::get-meta-semi))
      (error "no mrs::*meta-semi*/mrs::*semi*"))
					;(mrs::convert-rmrs-to-mrs
    (saf-fs-path-value '(:|rmrs|) (saf-edge-content edge))
					;)
    )
   ;; temporary hack: eg. handles RMRS with single EP
   ((equal '(:|ep| :|gpred|) x) 
    (mrs::rel-pred (car (mrs::rmrs-liszt (saf-fs-path-value '(:|rmrs|) (saf-edge-content edge)))))
    )
   ((equal '(:|rarg| :|constant|) x) 
    (mrs::rmrs-arg-val (car (mrs::rmrs-rmrs-args (saf-fs-path-value '(:|rmrs|) (saf-edge-content edge))))))
   ))

(setf *resolve-mrs* #'resolve-mrs2)

