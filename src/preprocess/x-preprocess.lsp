;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2006
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


;;;
;;; oe's wish list:
;;;
;;; implement the following specification:
;;;
;;; @ version
;;;
;;; : token separator (also mark transition from string- to token-level rules)
;;;
;;; - replace
;;;
;;; + augment (only in token-level mode)
;;;
;;; ^ ersatz (currently only in token-level mode)
;;;
;;; #42
;;;
;;; # grouping: name set of rules # 42; group is not executed unless called
;;;
;;; >42 group call; groups can be recursive and stop when there was no match
;;;
;;; </foo/bar file inclusion: `/foo/bar' is read at this point
;;;
;;; | continuation line for token-level rule, in pattern or substitution, e.g.
;;;
;;;   +it
;;;   |'s			its
;;;
;;;   +its			it
;;;   |				's
;;;

(in-package :preprocessor)

;; some wrappers

(defun x-read-preprocessor (&rest rest) (apply #'read-preprocessor rest))
(defun x-preprocess (&rest rest) (apply #'preprocess rest))
(defun x-clear-preprocessor (&rest rest) (apply #'clear-preprocessor rest))

;; end of wrappers

(defvar *min-regex-char-code-limit* 256)

(defvar *local-to-global-point-mapping* #'identity)

(defvar *preprocess-p* nil)
;(defvar *preprocessor-debug-p* t)
(defvar *preprocessor* nil)
(defvar *x-addressing* nil)

(defvar *span* nil)
(defvar *text* nil)

(defparameter *default-token-sep* 
    (let ((ppcre::*regex-char-code-limit* 256))
      (ppcre:create-scanner "[ \\t]+" :single-line-mode t)))

(defstruct x-fspp
  version
  (tokenizer *default-token-sep*) 
  global
  local)

(defmethod print-object ((object x-fspp) stream)
  (format 
   stream 
   "#[X-FSPP (~d global, ~d token-level rules @ `~a')]"
   (length (x-fspp-global object)) (length (x-fspp-local object)) 
   (x-fspp-tokenizer object)))

(defstruct x-fsr
  type
  source
  scanner
  target)

(defmethod print-object ((object x-fsr) stream)
  (with-slots (type source target) object
      (format 
       stream 
       "#[X-FSR (~a |~a| -> |~a|)]"
       type source target
       )))

(defun preprocessor-initialized-p ()
  (if *preprocessor* t))

(defun clear-preprocessor ()
  (setf *preprocessor* nil))

;; return max code point of all characters in file
(defun file-max-char-code (filename)
  (with-open-file (stream filename :direction :input)
    (loop
	with c
	while (setf c (read-char stream nil nil))
	maximize (char-code c))))

;; top-level call
(defun read-preprocessor (file)
  ;; ppcre::*regex-char-code-limit* will be set smallest possible value
  (let* ((ppcre::*regex-char-code-limit* *min-regex-char-code-limit*)
	 (x-fspp (read-preprocessor-aux file))) 
    ;; finalize x-fspp
    (setf (x-fspp-global x-fspp)
      (nreverse (x-fspp-global x-fspp)))
    (setf (x-fspp-local x-fspp)
      (nreverse (x-fspp-local x-fspp)))
    (format t "~a~%" x-fspp)
    (setf *preprocessor* x-fspp)))

(defparameter *multiple-rule-internal-separator* 
    (ppcre:create-scanner "\\t{2,}" :single-line-mode t))
(defparameter *rule-regex* 
    (ppcre:create-scanner "^.([^\\t]+)\\t+([^\\t]+)" :single-line-mode t))

;; recursive call
(defun read-preprocessor-aux (file &key (x-fspp (make-x-fspp)))
  (when (probe-file file)
    ;; set ppcre::*regex-char-code-limit* to smallest possible value
    (setf ppcre::*regex-char-code-limit*
      (max ppcre::*regex-char-code-limit*
	   (file-max-char-code file)))
    ;; now walk through file and collect rules
    (with-open-file (stream file :direction :input)
      (format t "~&Reading preprocessor rules '~a'~%" file)
      (loop
	  with path = (pathname file)
	  for n from 1
	  for line = (read-line stream nil nil)
	  while line
	  do
	    (read-preprocessor-line line n x-fspp path)))
      x-fspp))

(defparameter *empty-line-regex* 
    (ppcre:create-scanner "^\\s*$" :single-line-mode t))

(defun read-preprocessor-line (line n x-fspp path)
  (unless (= (length line) 0)
    (let ((c (char line 0)))
      (cond
       ((char= #\; c)
	;; comment
	)
       ((char= #\@ c)
	(read-preprocessor-version line x-fspp n))
       ((char= #\< c)
	(read-preprocessor-import line x-fspp path n))
       ((char= #\: c)
	(read-preprocessor-tokenizer line x-fspp))
       ((char= #\! c)
	(read-preprocessor-rule :replace line x-fspp n))
       ((char= #\- c)
	(read-preprocessor-rule :substitute line x-fspp n))
       ((char= #\+ c)
	(read-preprocessor-rule :augment line x-fspp n))
       ((char= #\^ c)
	(read-preprocessor-rule :ersatz line x-fspp n))
       ((char= #\> c)
	(read-preprocessor-rule :ersatz-augment line x-fspp n))
       ((ppcre:scan *empty-line-regex* line)
	;; empty line
	)
       (t
	(format t
		"read-preprocessor(): [line ~d] ~
                    ignoring unknown rule type `~a'~%"
		n c))))))

(defparameter *date-regex* 
    (ppcre:create-scanner "^.\\$[D]ate: +(.*) +\\$" :single-line-mode t))

(defun read-preprocessor-version (line x-fspp n)
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings *date-regex* line :sharedp t)
    (if match
	(let ((version (aref registers 0)))
	  (setf (x-fspp-version x-fspp) version))
      (format t "read-preprocessor(): [line ~d] invalid `~a'~%" n line))))

(defparameter *import-regex* 
    (ppcre:create-scanner "^.(.*)" :single-line-mode t))

(defun read-preprocessor-import (line x-fspp path n)
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings *import-regex* line :sharedp t)
    (if match
	(let* ((name (aref registers 0))
	       (file (or (probe-file name)
			 (probe-file (merge-pathnames name path)))))
	  (if file
	      (read-preprocessor-aux file :x-fspp x-fspp)
	    (format t "read-preprocessor(): [~d] unable to include `~a'~%" n name)))
      (format t "read-preprocessor(): [line ~d] invalid `~a'~%" n line))))

(defun read-preprocessor-tokenizer (line x-fspp)
  (let ((tokenizer (subseq line 1)))
    (setf (x-fspp-tokenizer x-fspp) tokenizer)))

(defun read-preprocessor-rule (type line x-fspp n)
  ;; try to get source/target
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings *rule-regex* line :sharedp t)
    ;; warn user of this fails
    (unless match
      (format t "read-preprocessor(): [~d] invalid `~a'~%" n line)
      (return-from read-preprocessor-rule))
    ;; DEPRECATION warning
;    (if (ppcre:scan *multiple-rule-internal-separator* line)
;		  (format t "~&;WARNING: /deprecated/ use of multiple tabs to separate target/replacement in FSR rule (line ~a)~%" n))
    ;; process source/target
    (let* ((source (aref registers 0))
	   (target (aref registers 1))
	   ;; create scanner
	   (scanner
	    (ignore-errors
	     (ppcre:create-scanner 
	      (if (eq type :replace)
		  source
		(format nil "^~a$" source))
	      :single-line-mode t)))
	   x-fsr)
      (cond
       (scanner
	;; create x-fsr
	(setf x-fsr (make-x-fsr :type type :source source
				:scanner scanner :target target))
	;; :replace goes to globals
	;; all others to local
	(if (eq type :replace)
	    (push x-fsr (x-fspp-global x-fspp))
	  (push x-fsr (x-fspp-local x-fspp))))
       (t
	;; problem creating scanner
	(format t
		"read-preprocessor(): [line ~d] invalid pattern `~a'~%"
		n source))))))

(defun preprocess (string &key (preprocessor *preprocessor*)
                               verbose format)
  (let* ((x (make-preprocessed-x string)) ;; construct x-string
	 (*text* string) ;; see SMAF::X-PARSE
	 ;(*span* (char-map-simple-range (char-map x))) ;; not used???
	)
    ;; if no preprocessor defined...
    (when (null preprocessor)
      (error "no preprocessor available"))
    ;; process text globally
    (preprocess-global x (x-fspp-global preprocessor) :verbose verbose)
    ;; process tokens
    (let* ((x-tokens (x-split (x-fspp-tokenizer preprocessor) x))
	   (saf (x-tokens-to-saf-annots x-tokens)))
      (setf saf
	(preprocess-tokens saf
			   (x-fspp-local preprocessor)
			   :verbose verbose))
      (x-format-preprocessed-output saf :format format))))

;	;; get output in desired format
;	(x-format-preprocessed-output
;	 (x-tokens-to-result result)
;	 length format)))))

(defparameter *smaf-id* 0)
(defparameter *smaf-node* 0)

(defun x-tokens-to-saf-annots (x-tokens)
  (loop
      with init-node = *smaf-node*
      for x in x-tokens
      unless (string= (text x) "")
      collect 
	(smaf::make-saf-edge
	 :id (incf *smaf-id*)
	 :type :|token|
	 :source *smaf-node*
	 :target (incf *smaf-node*)
	 :content x) into edges
      collect *smaf-node* into nodes
      finally
	(return
	  (smaf::make-saf 
	   :meta nil			;!
	   :lattice (smaf::make-saf-lattice
		     :start-node init-node
		     :end-node *smaf-node*
		     :nodes (cons init-node nodes)
		     :edges edges)))))

(defun preprocess-global (x &optional (global (slot-value *preprocessor* 'global)) &key verbose)
  (with-slots (text char-map) x
    (loop
	for rule in global
	for scanner = (x-fsr-scanner rule)
	for target = (x-fsr-target rule)
	do
	  (multiple-value-bind (res matchp)
	      (x-regex-replace-all scanner x target)
	    (when (and matchp verbose)
	      (format t
		      "~&GLOBAL |~a| -> |~a| ~&mapped~&~a~&to~&"
		      (x-fsr-source rule) (x-fsr-target rule)
		      x)
	      (setf x res)
	      (format t "~&~a~%~%" x))
	    (setf x res))))
  x)

#+:null
(defun clone-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (copy-seq (text x))
    :char-map (copy-tree (char-map x))))

(defun copy-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (text x)
    :char-map (char-map x)))

(defun preprocess-tokens (saf local &key verbose)
  (let* ((saf-lattice (smaf::saf-lattice saf))
	 (saf-annots (smaf::saf-lattice-edges saf-lattice))
	 (init-node (smaf::saf-lattice-start-node saf-lattice))
	 (final-node (smaf::saf-lattice-end-node saf-lattice)))
  
  (loop
      for fsr in local
      for type = (x-fsr-type fsr)
      for scanner = (x-fsr-scanner fsr) ;; confusing
      for target = (x-fsr-target fsr)
      do
	(loop 
	    for annot in 
	      (loop for annot in saf-annots
		  when (eq :|token| (smaf::saf-edge-type annot))
		  collect annot)
	    for x-orig = (smaf::saf-edge-content annot)
;	    for x-orig = (smaf::saf-fs-feature-value2 
;			  (smaf::saf-edge-content annot)
;			  :surface)
	    with x-str
	    for annot-source = (smaf::saf-edge-source annot)
	    for annot-target = (smaf::saf-edge-target annot)
	    when (ppcre::scan scanner (text x-orig))
	    do
	      (setf x-str (copy-preprocessed-x x-orig))
	      (x-regex-replace scanner x-str target)
	      (when verbose
		(format t
			"~&~a |~a| -> |~a| in ~a gave ~a"
			type
			(x-fsr-source fsr) target 
			x-orig x-str))
	      (setf (char-map x-str) ;; hack! fix_me ???
		(substitute (char-map-simple-range (char-map x-orig))
			    nil
			    (char-map x-str)))
	      ;; delete existing annot when nec
	      (when (member type '(:ersatz :substitute))
		(setf saf-annots (remove annot saf-annots)))
	      ;; new (single) ersatz
	      (when (member type '(:ersatz :ersatz-augment))
		(push (smaf::make-saf-edge
		       :id (incf *smaf-id*)
		       :type :|ersatz|
		       :source annot-source
		       :target annot-target
		       :content (list
				 (smaf::make-saf-fv
				   :feature :surface
				   :value x-orig)
				 (smaf::make-saf-fv
				  :feature :name
				  :value (text x-str))))
		      saf-annots))
	      ;; new (perhaps multiple) tokens
	      (when (member type '(:augment :substitute))
		(loop
		    with x-bits = 
		      (remove "" 
			      (x-split (x-fspp-tokenizer *preprocessor*) x-str)
			      :key #'(lambda (x)
				       (text x))
			      :test #'string=
			      )
		    for i from 0 to (1- (length x-bits))
		    for x-bit = (nth i x-bits)
		    with last-node = annot-source
		    for next-node = (if (= i (1- (length x-bits)))
					annot-target
				      (incf *smaf-node*)) 
		    do
		      
		      ;; add new annot
		      (push (smaf::make-saf-edge
			     :id (incf *smaf-id*)
			     :type :|token|
			     :source last-node
			     :target next-node
			     :content x-bit)
			    saf-annots)
		      (setf last-node next-node)
		      ))	
	      
	      ))
  
  ;(print "OUT!!!")
  
  ;; contruct saf
  (loop
      with nodes
      for annot in saf-annots
      for source = (smaf::saf-edge-source annot)
      for target = (smaf::saf-edge-target annot)
      ;minimize source into init-node
      ;maximize target into final-node
      do 
	(pushnew source nodes :test #'=)
	(pushnew target nodes :test #'=)
      finally
	(return
	  (smaf::make-saf 
	   :lattice (smaf::make-saf-lattice 
		     :start-node (2-str init-node)
		     :end-node (2-str final-node)
		     :nodes (loop for node in nodes collect (2-str node))
		     :edges (loop 
				for annot in saf-annots
				collect (clean-saf-annot annot)))
	   :meta  (smaf::make-saf-meta 
		   :document nil
		   :addressing :|char|
		   :olac nil)
	   )))))

#+:null
(defun preprocess-tokens2 (saf-annots local &key verbose)
  (loop
      with result
      with x-token
      with length = 0
      for saf-annot in saf-annots
      ;for x = (smaf::saf-edge-content saf-annot)
      unless (string= (text x) "") do
	(incf length)
	(setf x-token x)
	(loop
	    with extra = nil
	    for rule in local
	    for type = (x-fsr-type rule)
	    for scanner = (x-fsr-scanner rule)
	    for target = (x-fsr-target rule)
	    for text-old = (text x-token)
	    for x-old = (copy-preprocessed-x x-token)
	    for x-new = (if (eq :augment type)
			    (x-regex-replace scanner 
					     (copy-preprocessed-x x-token) 
					     target)
			  (x-regex-replace scanner x-token target))
	       ;;
	       ;; _fix_me_
	       ;; regex-replace() always returns a fresh string, even if the
	       ;; pattern did _not_ match; to make this more efficient, it
	       ;; seems, we would have to use scan() and then glue together
	       ;; the result from parsing .target. and filling in register
	       ;; matches as needed :-{.                     (31-jan-03; oe)
	       ;;
	       ;; or hack ppcre code? (bmw)
	    when (and verbose 
		      (not (string= (text x-new) text-old))
		      ) 
	    do
	      (format
	       t
	       "~&~a |~a| -> |~a| in ~a gave ~a"
	       type
	       (x-fsr-source rule) (x-fsr-target rule) 
	       x-token x-new)
	    unless (string= text-old (text x-new))
	    do
	      (case type
		(:ersatz-augment
		 (push (list :ersatz x-old) extra)
		 (push (list :augment (list x-old)) extra)
		 )
		(:ersatz
		 ;;
		 ;; _fix_me_
		 ;; to do ersatzes properly, they should no longer be available
		 ;; to subsequent rule processing: presumably, we should build
		 ;; an ersatzing table and use non-string tokens (indices into
		 ;; the table) instead.                         (1-feb-03; oe)
		 ;;
		 (setf (char-map x-new) ;; hack! fix_me
		   (substitute (char-map-simple-range (char-map x-old))
			       nil
			       (char-map x-new)))
		 (push (list :ersatz x-old) extra)
		 (setf x-token x-new))
		(:augment
		 (push (list 
			:augment 
			(x-split (x-fspp-tokenizer *preprocessor*) x-new)) 
		       extra)) ;; fix_me: regex new token ogso
		(:substitute
		 (setf x-token x-new))
		(t
		 (error "unhandled type: ~a" type)))
	    finally
	      (push (cons x-token extra) result))
      finally
	(return (values result length))))

#+:null
(defun x-tokens-to-result (tokens &key verbose)
  ;; tokens: 
  ;;(
  ;; (|manns|:(0 . 5) (:AUGMENT (|mann s|:(0 . 5)))) 
  ;; (|smiler|:(6 . 12))
  ;;)
  (loop
      with result = nil
      with i = 0
      with id = 0
      for (form . extra) in tokens
      for surface = (second (find :ersatz extra :key #'first))
      for start = i
      for intermediate-nodes = 
	(apply #'+ 
	       (mapcar 
		#'(lambda (x)
		    (1- (length (second x))))
		(loop for x in extra
		    when (eq (first x) :augment)
		    collect x)))
      for end = (+ 1 i intermediate-nodes)
      do
	(loop
	    for (type form) in extra ;; (:AUGMENT (|mann s|:(0 . 5)))
	    when (eq type :augment) do 
	      ;; create edge from extra elt
	      (loop 
		  with toks = (copy-list form)
		  with tok
		  with start2 = start
		  with end2
		    while (setf tok (pop toks))
		  do
		    (if toks
			(setf end2 (incf i))
		      (setf end2 end))
		      (push (list (incf id) start2 end2 tok nil)
			    result)
		    (setf start2 end2)))
	(setf i end)
	;; create edge from form
	(push (list (incf id) start end form surface) 
	      result)
      when verbose do
	(format t "  (~a) [~a:~a] |~a|" id start end form)
	(loop
	    for foo in extra
	    for type = (case (first foo)
			 (:substitute #\-)
			 (:augment #\+)
			 (:ersatz #\^))
	    for form = (second foo)
	    do (format t " {~c |~a|}" type form)
	    finally (format t "~%"))
      finally 
	(return result)))

(defun x-escape-string (string &key (syntax :c))
  (declare (ignore syntax))
           
  (if string
    (loop
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        when (or (char= c #\") (char= c #\\)) do
          (vector-push #\\ result)
          (vector-push c result)
          (when (zerop (decf padding))
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
        else do
          (vector-push c result)
        finally
          (return result))
    ""))

(defun clean-saf-annot (annot)
  (let* ((type (smaf::saf-edge-type annot))
	 (content (smaf::saf-edge-content annot))
	 surface
	 range from to
	 fv)

    ;; replace x-string (surface/content)
    (when (eq type :|token|)
      (setf surface content)
      (setf (smaf::saf-edge-content annot) (text surface)))
    (when (eq type :|ersatz|)
      (setf surface (smaf::saf-fs-feature-value2 content :surface))
      (setf fv (find :surface content 
		     :key #'smaf::saf-fv-feature))
      (setf (smaf::saf-fv-value fv) (text surface)))

    (setf range (char-map-simple-range
		 (char-map surface)))
    (setf from (car range))
    (setf to (cdr range))
    
    ;; instantiate 'from' and 'to'
    (setf (smaf::saf-edge-from annot) from)
    (setf (smaf::saf-edge-to annot) to)
    ;; convert everything to string (temp!)
    (setf (smaf::saf-edge-id annot) (2-str (smaf::saf-edge-id annot)))
    ;(setf (smaf::saf-edge-type annot) (2-str (smaf::saf-edge-type annot)))
    (setf (smaf::saf-edge-source annot) (2-str (smaf::saf-edge-source annot)))
    (setf (smaf::saf-edge-target annot) (2-str (smaf::saf-edge-target annot)))
    (setf (smaf::saf-edge-from annot) (2-str (smaf::saf-edge-from annot)))
    (setf (smaf::saf-edge-to annot) (2-str (smaf::saf-edge-to annot)))
    (loop for fv in (and (listp content) content)
	do
	  (setf (smaf::saf-fv-feature fv) (2-str (smaf::saf-fv-feature fv)))
	  (setf (smaf::saf-fv-value fv) (2-str (smaf::saf-fv-value fv))))
    
    )
  annot)

(defun x-format-preprocessed-output (saf &key format)
  
  (cond
   ((or (eq format :yy)
	(eq format :pet)) ;; deprecate this...?
    (loop
	with length = (length (smaf::saf-lattice-nodes (smaf::saf-lattice saf)))
	with saf-annots = (smaf::saf-lattice-edges (smaf::saf-lattice saf))
	for annot in saf-annots
	for content = (smaf::saf-edge-content annot)
	for type = (smaf::saf-edge-type annot)
	for id = (smaf::saf-edge-id annot)
	for start = (smaf::saf-edge-source annot)
	for end = (smaf::saf-edge-target annot)
	for surface = (if (string= type "ersatz")
			  (smaf::saf-fs-feature-value2 content :surface)
			content)
	for form = (if (string= type "ersatz")
		       (smaf::saf-fs-feature-value2 content :name)
		     surface)
	for token = (format 
		     nil 
		     "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
		     id start end 
		     (x-escape-string form)
		     (x-escape-string surface))
	collect token into tokens
	finally 
	  (return
	    (values (format nil "~{~a~^ ~}" tokens) length))))
   ((null format)
    saf)
   ((or (eq format :saf) (eq format :smaf))
    (smaf::to-xml saf :format format))
   ((eq format :list)
    (error ":list format no longer supported"))
   (t
    (error "unhandled format argument: ~a" format))))


;;
;; (bmw - oct 05)
;;

(defclass preprocessed-x ()
  ((text :initform nil :accessor text :initarg :text)
   (char-map :initform nil :accessor char-map :initarg :char-map)))

(defvar *print-char-map-simple-range* t)
(defmethod print-object ((object preprocessed-x) stream)
  (with-slots (text char-map) object
      (format 
       stream 
       "|~a|:~a"
       text (if *print-char-map-simple-range*
		(char-map-simple-range char-map)
	      char-map))))

;; fix_me: enforce ordering constraint?
(defun char-map-simple-range (char-map)
  (loop 
      for r in char-map
      if (numberp (car r)) collect (car r) into from
      if (numberp (cdr r)) collect (cdr r) into to
      finally
	(return
	  (cons (if from (apply #'min from))
		(if to (apply #'max to))))))

(defun make-preprocessed-x (str)
  (let ((x (make-instance 'preprocessed-x :text str)))
    (setf (char-map x)
      (make-char-map (length (text x))))
    x))

(defun preprocessed-x= (x y)
  (and
   (string= (text x) (text y))
   (equal (char-map x) (char-map y))))

(defun make-char-map (l &key val)
  (loop
      with i = 0
      while (< i l)
      collect 
	(if val
	    (progn (incf i) val)
	  (cons i (incf i)))))

(defun char-map-lookup (n char-map)
  (nth n char-map))

(defstruct repl
  target-string
  target-char-map
  sub-char-map
  start
  end
  match-start
  match-end
  reg-starts
  reg-ends
  replace-string)

(defstruct repl-list
  list)

(defun char-map-match-start (char-map match-start)
  (car (nth match-start char-map)))

(defun char-map-match-end (char-map match-end)
  (cond
   ((= 0 match-end)
    0)
   (t
    (cdr (nth (1- match-end) char-map)))))

(defun catch-repl (target-string start end match-start match-end reg-starts reg-ends &key replace-string repl-l char-map)
  (coerce
   (loop
       with esc
       with reg ;; register
       with reg-start
       with reg-end
       for c across replace-string
		    ;; 
		    ;; register matches
		    ;;
       if (and esc (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) ;; to_do: \& \` \' \{N}
       do (setf reg (1- (read-from-string (string c))))
	  (setf reg-start (aref reg-starts reg))
	  (setf reg-end (aref reg-ends reg))
       and append (coerce (subseq target-string reg-start reg-end) 'list) ;; replacement text
       and append (subseq char-map reg-start reg-end) into new-char-map  ;; replacement spans
       and do (setf esc nil)
       else if esc 
	    ;;
	    ;; escaped character
	    ;;
       collect c
       and collect (cons nil nil) into new-char-map ;; empty replacement span
       and do (setf esc nil)
       else if (char= c #\\)
	    ;;
	    ;; backslash (escape) character
	    ;;
       do (setf esc t)
       else collect c
	    ;;
	    ;; none of the above
	    ;;
       and collect (cons nil nil) into new-char-map ;; empty replacement span 
       finally
	 (fill-char-map new-char-map
			(char-map-match-start char-map match-start)
			(char-map-match-end char-map match-end))
	 (push
	  (make-repl :target-string target-string
		     :target-char-map char-map
		     :sub-char-map new-char-map
		     :start start
		     :end end
		     :match-start match-start
		     :match-end match-end
		     :reg-starts reg-starts
		     :reg-ends reg-ends
		     :replace-string replace-string)
	  (repl-list-list repl-l)))
   'string))

;; instantiate null points to match surrounding spans
(defun fill-char-map (char-map start end)
  ;; update start points
  (loop
      with last-p = start
      for r in char-map
      for s = (car r)
      for e = (cdr r)
      if s do (setf last-p (or e last-p)) ;; update last-p if poss
      else do (setf (car r) last-p) ;; instantiate
	   )
  ;; update end points
  (loop
      with last-p = end
      for r in (reverse char-map)
      for s = (car r)
      for e = (cdr r)    
      if e do (setf last-p (or s last-p)) ;; update last-p if poss
      else do (setf (cdr r) last-p) ;; instantiate
	      )
  char-map)
	 
(defun update-char-map (replacements x)
  (setf (char-map x)
    (loop
	with char-map = (char-map x)
	with last = 0
	for r in replacements
      for start = (repl-match-start r)
	for end = (repl-match-end r)
      for sub-char-map = (repl-sub-char-map r)
			 ;; unmatched portion of string
	append (subseq char-map last start) into new-char-map 
	append sub-char-map into new-char-map
	do (setf last end)
	finally
	  (return (append new-char-map (subseq char-map last (length char-map)))))))

;; returns: REPLACEMENT + flag indicating whether a match took place
(defun x-regex-replace-all (scanner x target)
  (with-slots (text char-map) x
    (let ((repl-l (make-repl-list)))
      (setf (text x)
	(cl-ppcre:regex-replace-all 
	 scanner text 
	 #'(lambda (a b c d e f g) 
	     (catch-repl a b c d e f g 
			 :replace-string target 
			 :repl-l repl-l
			 :char-map char-map))))
      (with-slots (list) repl-l
	(when list
	  (setf list (reverse list))
	  (update-char-map list x))
	(values x (not (null list))))
      )))

(defun x-regex-replace (scanner x target)
  (with-slots (text char-map) x
    (let* ((repl-l (make-repl-list))
	   (x2
	    (cl-ppcre:regex-replace 
	     scanner text 
	     #'(lambda (a b c d e f g) 
		 (catch-repl a b c d e f g 
			     :replace-string target 
			     :repl-l repl-l
			     :char-map char-map)))))
      (with-slots (list) repl-l
	(when list
	  (setf (text x) x2)
	  (setf list (reverse list))
	  (update-char-map list x)))))
  x)

(defun x-split (scanner x)
  (with-slots (text char-map) x
    (loop
	with ranges = (append '(0)
			      (cl-ppcre:all-matches scanner text)
			      (list (length text)))
	with start
	with end
	while ranges
	do
	  (setf start (pop ranges))
	  (setf end (pop ranges))
	collect 
	  (make-instance 'preprocessed-x
	    :text (subseq text start end)
	    :char-map (subseq char-map start end)))))

;;

(defun 2-str (x)
  (cond
   ((stringp x) x)
   ((symbolp x) (symb-2-str x))
   ((numberp x) (num-2-str x))
   ((pathnamep x) (namestring x))
   (t (error "unhandled type"))))

(defun symb-2-str (symb)
  (unless (symbolp symb)
    (error "symbol expected"))
  (cond
   ((null symb) "")
   (t (string-downcase (string symb)))))

(defun num-2-str (num)
  (if (null num)
      (return-from num-2-str))
  (unless (numberp num)
    (error "number expected"))
  (format nil "~a" num))