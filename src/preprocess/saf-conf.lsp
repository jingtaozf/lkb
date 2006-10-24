;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :common-lisp-user)


(in-package :saf)

(defvar *lmap*)
(defvar *gmap* nil) ;; reset when necessary!

(defstruct map-action
  e-edge
  l-content)

;;
;; L-CONTENT
;;

;; instantiate :l-content for all annotations in SAF object
(defun instantiate-l-content (saf l-map)
  (unless l-map 
    (reset-conf)
    (setf l-map *lmap*))
  (loop
      with lattice = (saf-lattice saf)
      for edge in (and lattice (saf-lattice-edges lattice))
      for l-content = (edge-l-content edge l-map)
      do (setf (saf-edge-l-content edge) l-content)
      finally 
	(setf (saf-lattice saf)
	  (postprocess-lattice lattice)) ;; clobber rules, etc.
	(return saf)))

;; instantiate :l-content for annotation
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

;; resolve var wrt an edge
;; currently only allow extraction from 'content'
(defun resolve (var edge)
  (let* ((l (lkb::string-2-str-list (string var) :sep #\.))
	 (feat (car l))
	 (x (cdr l)))
    (cond
      ((string= feat "content")
       (saf-fs-path-value x (saf-edge-content edge)))
      ((string= feat "rmrs")
       (cond
	((equal nil x)
	 (unless (and (mrs::get-semi) (mrs::get-meta-semi))
	   (error "no mrs::*meta-semi*/mrs::*semi*"))
	 ;(mrs::convert-rmrs-to-mrs
	  (saf-fs-path-value '(:rmrs) (saf-edge-content edge))
	  ;)
	 )
	;; temporary hack: eg. handles RMRS with single EP
	((equal '("ep" "gpred") x) 
	 (mrs::char-rel-pred (car (mrs::rmrs-liszt (saf-fs-path-value '(:rmrs) (saf-edge-content edge)))))
	 )
	((equal '("rarg" "constant") x) 
	 (mrs::rmrs-arg-val (car (mrs::rmrs-rmrs-args (saf-fs-path-value '(:rmrs) (saf-edge-content edge))))))
	))
      (t
       (error "unhandled variable name '~a' found in l-content" var)))))

;; = "action does not conflict with edge"
;; match on 'type' and 'content'
(defun edge-match (edge action)
  (and
   (f-match 'saf-edge-type edge action)
   (f-match 'saf-edge-content edge action)))

(defun f-match (f edge action)
  (match (funcall f edge) 
	 (funcall f action)))

;; rename to: y-in-x
(defun match (x y)
  (if (symbolp x) (setf x (string x))) ;;; fix_me
  (cond
   ((null y) t)
   ((equal x y) t)
   ((listp y)
    (loop
	for fv in y
	for val = (saf-fv-value fv)
	for feat = (saf-fv-feature fv)
	for m = (match 
		 (saf-fs-path-value (list feat) x)
		 val)
	unless m
	do (return nil)
	finally (return t)))))
		  
;; inject (overwrite) x into l-content
;; resolve any var's wrt edge
(defun inject (x l-content &key edge)
  (loop 
      for fv in x
      for feat = (saf-fv-feature fv)
      for val- = (saf-fv-value fv)
      for val = (if (symbolp val-)
		    (resolve val- edge)
		  val-) ;; resolve val if nec
      for fv2 = (find feat l-content 
		      :key #'saf-fv-feature
		      :test #'string=)
      if (and (not (string= "" feat))
	      (char= #\+ (aref feat 0)))
	 ;; support +variables which collect values in list
      do
	(if val
	    (if fv2
		(setf (saf-fv-value fv2) (cons val (saf-fv-value fv2)))
	      (push (make-saf-fv :feature feat :value (list val))
		    l-content)))
      else
	;; standard variables, for which value may be overwritten
      do
	(if fv2
	    (setf (saf-fv-value fv2) val)
	  (push (make-saf-fv :feature feat :value val)
		l-content)))
  l-content)

;;; inject (overwrite) x into l-content
;;; resolve any var's wrt edge
;(defun inject (x l-content &key edge)
;  (loop 
;      for fv in x
;      for feat = (saf-fv-feature fv)
;      for val- = (saf-fv-value fv)
;      for val = (if (symbolp val-)
;		    (resolve val- edge)
;		  val-) ;; resolve val if nec
;      for fv2 = (find feat l-content 
;		      :key #'saf-fv-feature
;		      :test #'string=)
;      do
;	(if fv2
;	    (setf (saf-fv-value fv2) val)
;	  (push (make-saf-fv :feature feat :value val)
;		l-content)))
;  l-content)

;;

;; fix_me: generalise???
(defun l-edgeType (s-edge)
  (saf-fs-feature-value 
   (saf-edge-l-content s-edge) 
   "edgeType"))

;;
;; READ SAF CONFIG FILE
;;

;;
;; very simple reader for textual conf file
;; - one setting per line
;; - of form "type.[f1='v1' f2='v2'] -> edgeType='tok'"
;; where type, fN and vN consist only of (Perl regex) word characters

(defun get-saf-l-map (filename)
  (setf *lmap*
    (conf-read-file filename)))

(defparameter *ERSATZ-CARG-PATH* nil)
(defparameter *default-config* "
;define gMap.carg (synsem lkeys keyrel carg) STRING

token.[] -> edgeType='tok' tokenStr=content
wordForm.[] -> edgeType='morph' stem=content.stem partialTree=content.partial-tree
;ersatz.[] -> edgeType='tok+morph' stem=content.name tokenStr=content.name gMap.carg=content.surface inject='t' analyseMorph='t'
")

;; fallback case handles smaf as mapped from tchart
(defun reset-conf nil
  (setf *gmap* nil)
  (setf *lmap*
    (with-input-from-string (s *default-config*)
      (conf-read-stream s)))
  (cond
   (*ersatz-carg-path*			; HACK!!!
    (push (list :|carg| *ersatz-carg-path* :STR) *gmap*)
    (push (conf-read-line "ersatz.[] -> edgeType='tok+morph' stem=content.name tokenStr=content.name gMap.carg=content.surface inject='t' analyseMorph='t'") *lmap*))
   (t
    (push (conf-read-line "ersatz.[] -> edgeType='tok+morph' stem=content.name tokenStr=content.name analyseMorph='t'") *lmap*)))
  *lmap*
  )

;;


;; process each line in SAF config file
(defun conf-read-file (filename)
  (setf *gmap* nil)
  (with-open-file (s filename 
		   :direction :input)
    (conf-read-stream s)))

;; process each line in SAF config file
(defun conf-read-stream (s)
  (loop
      for line = (read-line s nil nil)
      while line
      for a = (conf-read-line line)
      if (map-action-p a) collect a))

;; ignore empty lines, and those composed of whitespace
;; otherwise expect lines of form:
;;  type.[x='y' a='b'] -> foo='bar' foo2=bar2
;;  define gMap.pred (synsem lkeys keyrel pred)
(defun conf-read-line (line)
  (or
   (conf-read-line-RULE line)
   (conf-read-line-DEFINE line)
   (conf-read-line-COMMENT line)
   (conf-read-line-EMPTY line)
   (format t "~%;;; WARNING: ignoring malformed config line \"~a\"" line)))

;;eg.  type.[x='y' a='b'] -> foo='bar' foo2=bar2
(defun conf-read-line-RULE (line)
  (multiple-value-bind
      (m regs)
      (cl-ppcre:scan-to-strings "^(\\w*).\\[(.*)\\]\\s*->\\s*(.*)$" line)
    (when m
	(let ((type (aref regs 0))
	      (specs-str (aref regs 1))
	      (out-str (aref regs 2)))
	  (make-map-action :e-edge 
			   (make-saf-edge :type type 
					       :content (conf-read-specs specs-str))
			   :l-content (conf-read-specs out-str))))))

;; eg. define gMap.pred (synsem lkeys keyrel pred)
(defun conf-read-line-DEFINE (line)
  (multiple-value-bind
      (m regs)
      (cl-ppcre:scan-to-strings "^define\\s*gMap\\.(\\w+)\\s+\\((.*)\\)\\s*(\\w+)?\\s*$" line)
    (when m
	(let* ((name-str (aref regs 0))
	       (path-str (aref regs 1))
	       (type-str (aref regs 2))
	       (name (intern name-str :keyword))
	       (path (conf-read-path path-str))
	       (type (conf-read-type type-str))
	       )
	  (push (list name path type) *gmap*)))))

;; eg. "synsem lkeys keyrel pred" 
;;     -> (:|synsem| :|lkeys| :|keyrel| :|pred|)
(defun conf-read-path (path-str)
  (loop
      for f in (lkb::string-2-str-list path-str)
      unless (string= "" f)
      collect (intern (string-upcase f) :lkb)))

;; "STRING" -> :str
;; nil -> :sym
(defun conf-read-type (type-str)
  (cond
   ((or (string= "STRING" type-str) (string= "STR" type-str))
    :str)
   ((null type-str)
    :sym)
   (t
    (error "[internal] unknown type-str: '~S'" type-str)))) 

;; eg. ; comment
(defun conf-read-line-COMMENT (line)
  (and (not (string= "" line))
       (string= ";" (subseq line 0 1))))

(defun conf-read-line-EMPTY (line)
  (lxml::xml-whitespace-p line))

;; eg. "a='b' c='d'" -> "a='b'" "c='d'"
(defun conf-read-specs (specs-str)
  (loop
      for spec in (ppcre:split "\\s+" specs-str)
      collect (conf-read-spec spec)))

;; form: feat='val' or feat=var 
(defun conf-read-spec (spec)
  (or
   (ppcre:register-groups-bind 
    (feat val)
    ("^([+\\w.]*)='([^']*)'.*$" spec)
    (make-saf-fv :feature feat
		      :value val))
   (ppcre:register-groups-bind 
    (feat val)
    ("^([+\\w.]*)=(.*).*$" spec)
    (make-saf-fv :feature feat
		      :value (and (stringp val) (intern val))))))

