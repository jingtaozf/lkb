;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :common-lisp-user)

(defvar LKB::*SAF-L-MAP*)

(defpackage :saf
  (:use :common-lisp) 
  (:export))

(in-package :saf)

(defstruct map-action
  e-edge
  l-content)

(defun instantiate-l-content (saf l-map)
  (unless l-map (error "Please load saf.conf via (setf *saf-l-map* (saf::conf-read-file \"path/to/saf.conf\"))"))
  (loop
      for edge in (lkb::saf-lattice-edges 
		   (lkb::saf-lattice saf))
      for l-content = (edge-l-content edge l-map)
      do (setf (lkb::saf-edge-l-content edge) l-content)
      finally (return saf)))

(defun edge-l-content (edge l-map)
  (loop
      with l-content
      for action in l-map
      if (edge-match edge (map-action-e-edge action))
      do (setf l-content
	   (inject (map-action-l-content action)
		   l-content
		   :edge edge))
      finally (return l-content)))

;;!
;(defun resolve-variables (edge l-content)
;  (loop
;      for x in l-content
;      for val = (lkb::saf-fv-value x)
;      when (symbolp val)
;      do (setf (lkb::saf-fv-value x)
;	   (resolve val edge)))
;  )

(defun resolve (var edge)
  (cond
    ((eq var 'lkb::|content|)
     (lkb::saf-edge-content edge))
    ((eq var 'lkb::|content.stem|)
     (lkb::saf-fs-path-value '("stem") (lkb::saf-edge-content edge)))
    ((eq var 'lkb::|content.partial-tree|)
     (lkb::saf-fs-path-value '("partial-tree") (lkb::saf-edge-content edge)))
    (t
     (error "unknown variable name '~a' found in l-content" var))))

(defun edge-match (edge action)
  (and
   (f-match 'lkb::saf-edge-type edge action)
   (f-match 'lkb::saf-edge-content edge action)))

(defun f-match (f edge action)
  (match (funcall f edge) 
	 (funcall f action)))

(defun match (x y)
  (if (symbolp x) (setf x (string x))) ;;; fix_me
  (cond
   ((null y) t)
   ((equal x y) t)
   ((listp y)
    (loop
	for fv in y
	for val = (lkb::saf-fv-value fv)
	for feat = (lkb::saf-fv-feature fv)
	unless (match 
		val
		(lkb::saf-fs-path-value feat x))
	do (return nil)
	finally (return t)))))
		  
;;!
;(defun inject (x l-content)
;  (declare (ignore l-content))
;  (copy-tree x))

;!
(defun inject (x l-content &key edge)
  (loop 
      for fv in x
      for feat = (lkb::saf-fv-feature fv)
      for val- = (lkb::saf-fv-value fv)
      for val = (if (symbolp val-)
		    (resolve val- edge)
		  val-)
      for fv2 = (find feat l-content 
			:key #'lkb::saf-fv-feature
			:test #'string=)
      do
	(if fv2
	    (setf (lkb::saf-fv-value fv2) val)
	  (push (lkb::make-saf-fv :feature feat :value val)
		l-content)))
  l-content)
  

;;
;; very simple reader for textual conf file
;; - one setting per line
;; - of form "type.[f1='v1' f2='v2'] -> edgeType='tok'"
;; where type, fN and vN consist only of (Perl regex) word characters

(defun get-default-saf-l-map nil
  ;;(format t ";; (no saf-l-map settings loaded... using defaults)")
  (setf lkb::*saf-l-map*
    (list (conf-read-line "token.[] -> edgeType='tok' tokenStr=content")
	  (conf-read-line "wordForm.[] -> edgeType='morph' stem=content.stem partialTree=content.partial-tree"))))

(defun conf-read-file (filename)
  (with-open-file (file filename 
		   :direction :input)
    (loop
	for line = (read-line file nil nil)
	while line
	for a = (conf-read-line line)
	if a collect a)))

(defun conf-read-line (line)
  (multiple-value-bind
      (m regs)
      (cl-ppcre:scan-to-strings "^(\\w*).\\[(.*)\\]\\s*->\\s*(.*)$" line)
      (if m
	  (let ((type (aref regs 0))
		(specs-str (aref regs 1))
		(out-str (aref regs 2)))
	    (make-map-action :e-edge 
			     (lkb::make-saf-edge :type type 
						 :content (conf-read-specs specs-str))
			     :l-content (conf-read-specs out-str)
			     ))
	(format t "; WARNING: ignoring malformed config line \"~a\"" line))))

(defun conf-read-specs (specs-str)
  (loop
      for spec in (ppcre:split "\\s+" specs-str)
      collect (conf-read-spec spec)))

(defun conf-read-spec (spec)
  (or
   (ppcre:register-groups-bind 
    (feat val)
    ("(\\w*)='(\\w*)'.*" spec)
    (lkb::make-saf-fv :feature feat
		      :value val))
   (ppcre:register-groups-bind 
    (feat val)
    ("(\\w*)=(.*).*" spec)
    (lkb::make-saf-fv :feature feat
		      :value (intern val)))))
  