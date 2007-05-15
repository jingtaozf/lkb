;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :common-lisp-user)


(in-package :saf)

(defstruct config
  lmap
  gmap)

(defvar *config* (make-config))

(defun config nil *config*)

;(defvar *lmap*)
;(defvar *gmap* nil) ;; reset when necessary!

(defstruct map-action
  e-edge
  l-content)

;;
;; L-CONTENT
;;

;; instantiate :l-content for all annotations in SAF object
;(defun instantiate-l-content (saf l-map)
(defun instantiate-l-content (saf config)
  (loop
      with lattice = (saf-lattice saf)
      for edge in (and lattice (saf-lattice-edges lattice))
      for l-content = (edge-l-content edge config)
      do (setf (saf-edge-l-content edge) l-content) ;!
      finally 
	(setf (saf-lattice saf)
	  (postprocess-lattice lattice)) ;; clobber rules, etc.
	(return saf)))

;(defun instantiate-edge-l-content (edge l-map)
(defun instantiate-edge-l-content (edge config)
  (setf (saf-edge-l-content edge)
    (edge-l-content edge config))
  edge)

;; instantiate :l-content for annotation
;(defun edge-l-content (edge l-map)
(defun edge-l-content (edge config)
  (loop
      with l-content
      with l-map = (config-lmap config)
      for action in l-map
      if (edge-match edge (map-action-e-edge action))
      do (setf l-content
	   (inject (map-action-l-content action)
		   l-content
		   :edge edge))
      finally (return l-content)))

(defun explode-to-chars (string)
  (loop
      for i from 0 to (1- (length string))
      collect (aref string i)))
  
(defun implode-from-chars (char-list)
  (loop
      with res = (make-string (length char-list))
      for c in char-list
      for i upfrom 0
      do
	(setf (schar res i) c)
      finally
	(return res)))

(defun string-2-sym-list (string &key (sep #\Space) (esc t))
  (loop
      with res
      with flag
      with word-chars
      for c in (explode-to-chars string)
      if flag do
	(push c word-chars)
	(setf flag nil)
      else do
	   (cond
	     ((eq sep c)
	      (push 
	       (intern (implode-from-chars (reverse word-chars)) :keyword)
	       res)
	      (setf word-chars nil))
	     ((and (eq #\\ c) esc)
	      (setf flag t))
	     (T
	      (push c word-chars)))
      finally (return (reverse (push 
				(intern (implode-from-chars (reverse word-chars)) :keyword)
				res)))))

(defun string-2-str-list (string &key (sep #\Space) (esc t))
  (loop
      with res
      with flag
      with word-chars
      for c in (explode-to-chars string)
      if flag do
	(push c word-chars)
	(setf flag nil)
      else do
	   (cond
	     ((eq sep c)
	      (push 
	       (implode-from-chars (reverse word-chars))
	       res)
	      (setf word-chars nil))
	     ((and (eq #\\ c) esc)
	      (setf flag t))
	     (T
	      (push c word-chars)))
      finally (return (reverse (push 
				(implode-from-chars (reverse word-chars))
				res)))))

(defparameter *resolve-mrs* (lambda (&rest rest) (declare (ignore rest))))

(defun resolve-mrs (x edge)
  (funcall *resolve-mrs* x edge))

;; resolve var wrt an edge
;; currently only allow extraction from 'content'
(defun resolve (var edge)
  (let* ((l (string-2-sym-list (string var) :sep #\.))
	 (feat (car l)) ;!sym
	 (x (cdr l))) ;!sym list
    (cond
      ((eq feat :|content|)
       (saf-fs-path-value x (saf-edge-content edge)))
      ((eq feat :|deps|)
       nil) ;; not implemented: PET only
      ((eq feat :|rmrs|)
       (resolve-mrs x edge)
       (error "no RMRS support (MRS package unavailable)"))
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
      for feat-str = (string feat)
      for val- = (saf-fv-value fv)
      for val = (if (symbolp val-)
		    (resolve val- edge)
		  val-) ;; resolve val if nec
      for fv2 = (find feat l-content 
		      :key #'saf-fv-feature
		      )
;		      :test #'string=)
      if (and (not (eq :|| feat))
	      (char= #\+ (aref feat-str 0)))
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
  (saf-fs-feature-value2 
   (saf-edge-l-content s-edge) 
   :|edgeType|))

;;
;; READ SAF CONFIG FILE
;;

;;
;; very simple reader for textual conf file
;; - one setting per line
;; - of form "type.[f1='v1' f2='v2'] -> edgeType='tok'"
;; where type, fN and vN consist only of (Perl regex) word characters

;(defun get-saf-l-map (filename)
;  (setf *lmap*
;    (conf-read-file filename)))

(defparameter *ERSATZ-CARG-PATH* nil)
(defparameter *default-config* "
define gMap.carg () STRING

token.[] -> edgeType='tok' tokenStr=content
wordForm.[] -> edgeType='morph' stem=content.stem partialTree=content.partial-tree
ersatz.[] -> edgeType='tok+morph' stem=content.name tokenStr=content.name gMap.carg=content.surface inject='t' analyseMorph='t'")

(defun ersatz-carg-path nil
  *ersatz-carg-path*)

(defun gmap nil
  (config-gmap *config*))

(defun conf-read-string (str)
  (with-input-from-string (s str)
      (conf-read-stream s)))

;; fallback case handles smaf as mapped from tchart
(defun reset-conf nil
  (setf *config*
    (conf-read-string *default-config*)))

;;


;; process each line in SAF config file
(defun conf-read-file (filename)
  (setf *config*
    (with-open-file (s filename 
		     :direction :input)
      (conf-read-stream s))))

;; process each line in SAF config file
(defun conf-read-stream (s)
  (loop
      with config = (make-config)
      for line = (read-line s nil nil)
      while line 
      do (conf-read-line line config)
;      for a = (and line (conf-read-line line config))
;      while line
;      if (map-action-p a) collect a
      finally
	(return config)))

;; ignore empty lines, and those composed of whitespace
;; otherwise expect lines of form:
;;  type.[x='y' a='b'] -> foo='bar' foo2=bar2
;;  define gMap.pred (synsem lkeys keyrel pred)
(defun conf-read-line (line config)
  (or
   (conf-read-line-RULE line config)
   #+:lkb
   (conf-read-line-DEFINE line config)
   (conf-read-line-COMMENT line)
   (conf-read-line-EMPTY line)
   (format t "~%;;; WARNING: ignoring malformed config line \"~a\"" line)))

;; simply collect everything
(defun inject-lmap (map-action config)
  (with-slots (lmap) config
    (push map-action lmap)))

(defun inject-gmap (mapping config)
  (with-slots (gmap) config
    (let* ((name (first mapping))
	   (match (find name gmap :key #'first)))
      (cond
       (match
	;; overwrite existing mapping
	(format t "~&;WARNING: overriding ~a in SMAF config mapping definition" name)
	(setf (cdr match) (cdr mapping)))
       (t
	;; collect mapping for new name
	(push mapping gmap))))))

;;eg.  type.[x='y' a='b'] -> foo='bar' foo2=bar2
(defun conf-read-line-RULE (line config)
  (multiple-value-bind
      (m regs)
      (cl-ppcre:scan-to-strings "^(\\w*).\\[(.*)\\]\\s*->\\s*(.*)$" line)
    (when m
      (let* ((type (aref regs 0))
	     (specs-str (aref regs 1))
	     (out-str (aref regs 2))
	     (map-action
	      (make-map-action :e-edge 
			       (make-saf-edge :type type 
					      :content (conf-read-specs specs-str))
			       :l-content (conf-read-specs out-str))))
	(inject-lmap map-action config)))))

;; eg. define gMap.pred (synsem lkeys keyrel pred)
#+:lkb
(defun conf-read-line-DEFINE (line config)
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
	  (inject-gmap (list name path type) config)))))
;	  (push (list name path type) *gmap*)))))

;; eg. "synsem lkeys keyrel pred" 
;;     -> (:|synsem| :|lkeys| :|keyrel| :|pred|)
#+:lkb
(defun conf-read-path (path-str)
  (loop
      for f in (string-2-str-list path-str)
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

(defparameter *empty-line-regex* 
    (ppcre:create-scanner "^\\s*$" :single-line-mode t))

(defun conf-read-line-EMPTY (line)
  (ppcre:scan *empty-line-regex* line))
;  (lxml::xml-whitespace-p line))

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
    (make-saf-fv :feature (str2feat feat)
		      :value val))
   (ppcre:register-groups-bind 
    (feat val)
    ("^([+\\w.]*)=(.*).*$" spec)
    (make-saf-fv :feature (str2feat feat)
		      :value (and (stringp val) (intern val))))))

(defun str2feat (str)
  (intern str :keyword))