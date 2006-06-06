;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :common-lisp-user)

(defpackage :saf
  (:use :common-lisp) 
  (:export))

(in-package :lkb)

(defvar *SAF-L-MAP*)

(in-package :saf)

(defstruct map-action
  e-edge
  l-content)

;; instantiate :l-content for all annotations in SAF object
(defun instantiate-l-content (saf l-map)
  (unless l-map (error "No SAF config rules loaded ~%   (please load saf.conf via '(setf *saf-l-map* (saf::conf-read-file \"path/to/saf.conf\"))')"))
  (loop
      for edge in (lkb::saf-lattice-edges 
		   (lkb::saf-lattice saf))
      for l-content = (edge-l-content edge l-map)
      do (setf (lkb::saf-edge-l-content edge) l-content)
      finally (return saf)))

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
       (lkb::saf-fs-path-value x (lkb::saf-edge-content edge)))
      (t
       (error "unhandled variable name '~a' found in l-content" var)))))

;; = "action does not conflict with edge"
;; match on 'type' and 'content'
(defun edge-match (edge action)
  (and
   (f-match 'lkb::saf-edge-type edge action)
   (f-match 'lkb::saf-edge-content edge action)))

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
	for val = (lkb::saf-fv-value fv)
	for feat = (lkb::saf-fv-feature fv)
	for m = (match 
		 (lkb::saf-fs-path-value (list feat) x)
		 val)
	unless m
	do (return nil)
	finally (return t)))))
		  
;; inject (overwrite) x into l-content
;; resolve any var's wrt edge
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

;; fix_me: generalise???
(defun l-edgeType (s-edge)
  (lkb::saf-fs-feature-value 
   (lkb::saf-edge-l-content s-edge) 
   "edgeType"))

;(defun l-gType (s-edge)
;  (lkb::saf-fs-feature-value 
;   (lkb::saf-edge-l-content s-edge) 
;   "gType"))

;;
;; very simple reader for textual conf file
;; - one setting per line
;; - of form "type.[f1='v1' f2='v2'] -> edgeType='tok'"
;; where type, fN and vN consist only of (Perl regex) word characters

(defun get-saf-l-map (filename)
  (setf lkb::*saf-l-map*
    (conf-read-file filename)))

;; fallback case handles smaf as mapped from tchart
(defun get-default-saf-l-map nil
  (setf lkb::*saf-l-map*
    (list (conf-read-line "token.[] -> edgeType='tok' tokenStr=content")
	  (conf-read-line "wordForm.[] -> edgeType='morph' stem=content.stem partialTree=content.partial-tree"))))

;;


;; process each line in SAF config file
(defun conf-read-file (filename)
  (with-open-file (file filename 
		   :direction :input)
    (loop
	for line = (read-line file nil nil)
	while line
	for a = (conf-read-line line)
	if a collect a)))

;; ignore empty lines, and those composed of whitespace
;; otherwise expect lines of form:
;;  type.[x='y' a='b'] -> foo='bar' foo2=bar2
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
			     :l-content (conf-read-specs out-str)))
	(unless (or 
		 (xml-whitespace-p line)
		 (string= ";" (subseq line 0 1)))
	  (format t "; WARNING: ignoring malformed config line \"~a\"" line)))))

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
    ("(\\w*)='([^']*)'.*" spec)
    (lkb::make-saf-fv :feature feat
		      :value val))
   (ppcre:register-groups-bind 
    (feat val)
    ("(\\w*)=(.*).*" spec)
    (lkb::make-saf-fv :feature feat
		      :value (intern val)))))

;;;
;;; socket SERVER code
;;; (move to separate file later)

;; characters not allowed in XML
(defvar *xml-bad-chars*
    '(
      #\^a 
      #\^b 
      #\^c 
      #\^d 
      #\^e 
      #\^f 
      #\bel 
      #\Backspace 
      ;#\Tab 
      ;#\Newline 
      #\vt 
      #\Page 
      ;#\Return 
      #\^n 
      #\^o 
      #\^p 
      #\^q 
      #\^r 
      #\^s 
      #\^t 
      #\^u 
      #\^v 
      #\^w 
      #\^x 
      #\^y 
      #\^z 
      #\esc 
      #\fs 
      #\^] 
      #\^^ 
      #\^_ 
      ))

;; read input chunk delimited by term-char followed by newline
;; ignore non-xml chars while constructing chunk
(defun read-input (stream &key (term-char (code-char 17)))
  (loop
      with strm = (make-string-output-stream)
      for c = (read-char stream nil nil)
      while (and c (not (char= c term-char)))
      unless (or
	      (char= c #\Return)
	      (member c *xml-bad-chars* :test 'char=))
      do 
	;;(print c)
	(write-char c strm)
      finally 
	(if (not c) (return-from read-input :eof))
	(read-line stream nil nil)
	(return
	  (get-output-stream-string strm))))

;; remove 
 from end of line
;; (for use with telnet client)
(defun clean-line (line)
  (when (stringp line)
    (let ((len (length line)))
      (when (string= (subseq line (- len 1)) "
")
	(subseq line 0 (- len 1))))))
;;

;; generic socket server
;; caller supplys processor function (and name)
(defun r-server (processor name &key (port 9876))
  (format t "~&;;; [entering ~a server mode]" name)
  (format t "~&;;; starting server on port ~a" port)
  (let ((socket (socket::make-socket :connect :passive :local-port port)))
    (unwind-protect
	(handler-case
	    (r-server-accept-connections socket processor)
	  (error (condition)
	    (format t  "~&Error: ~A" condition)))
      (close socket)
      (format t "~&;;; [exiting server mode]"))))

;; process connections forever
(defun r-server-accept-connections (socket processor)
  (loop
    (r-server-process-connection socket processor)))

;; process single connection
(defun r-server-process-connection (socket processor)
  (format t "~&;;; waiting for connection")
  (let ((s (socket::accept-connection socket)))
    (unwind-protect
	(progn
	   ;; use UTF-8 when communicating with socket
	  (setf (stream-external-format s) :utf-8)
	  (format t "~&;;; connection established")
	  (r-server-process-input s processor))
      (close s)
      (format t "~&;;; connection closed"))))

;; process single input chunk
(defun r-server-process-input (s processor)
  (loop
      for input = (read-input s)
      for empty-input = (lkb::xml-whitespace-p input)
      ;;do (format t "~&I: ~a" input)
      while (not (eq :eof input))
      if empty-input
      do (format t "~&;;; empty input!")
      when (not empty-input)
      do 
	(handler-case
	    (funcall processor s input)
	  (error (condition)
	    (format t  "~&Error: ~A" condition)))
      do
	 (format s "~a" #\^q)
	 (terpri s)
	 (force-output s)))

;;

;; PARSE socket server
(defun run-parse-server (&key (port 9876) (mode :xml) debug)
  (r-server (lambda (x y)
	      (r-server-parse-input x y :mode mode :debug debug))
	    "PARSE"
	    :port port))

;; parse input chunk
(defun r-server-parse-input (s input &key (mode :string) debug)
  (let (id saf)
    (case mode
      (:string 
       (format t "~&;;; parsing input: ~a" input)
       (setf id (format nil "s~a" (lkb::id-to-int nil)))
       (lkb::parse (lkb::split-into-words 
		    (lkb::preprocess-sentence-string input))
		   nil))
      (:xml
       (format t "~&;;; parsing XML input")
       (when debug (format t "~&INPUT: ~%~a~%~%" input))
       (setf saf (lkb::xml-to-saf-object input))
       (setf id (lkb::saf-fs-feature-value 
		 (lkb::saf-meta-olac 
		  (lkb::saf-meta saf)) 
		 "dc:identifier"))
       (lkb::parse saf nil))
      (t
       (error "unknown PARSE server mode '~a'" mode)))
    (when debug
      (format t "~&OUTPUT: ~%")
      (lkb::dump-sentence-analyses2 :s-id id :stream t)
      (format t "~%~%"))
    (lkb::dump-sentence-analyses2 :s-id id :stream s)))
  
;; FSPP socket server
(defun run-fspp-server (&key (port 8876) (mode :string))
  (r-server (lambda (x y)
	      (r-server-fspp-input x y :mode mode))
	    "FSPP"
	    :port port))

;; preprocess input chunk
(defun r-server-fspp-input (s input &key (mode :string) (o-mode :saf))
  (case mode
    (:string 
     (format t "~&;;; preprocessing input: ~a" input)
     (format s "~&~a" (preprocessor:preprocess input :format o-mode)))
    (t
     (error "unknown FSPP server mode '~a'" mode))
    ))

