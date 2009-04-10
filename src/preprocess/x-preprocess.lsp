;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PREPROCESSOR -*-


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

(defun x-read-preprocessor (&rest rest) 
  (format t ";;WARNING: The function 'x-read-preprocessor' is deprecated. Please use 'read-preprocessor' instead.")
  (apply #'read-preprocessor rest))
(defun x-preprocess (&rest rest) 
  (format t ";;WARNING: The function 'x-preprocess' is deprecated. Please use 'preprocess' instead.")
  (apply #'preprocess rest))
(defun x-clear-preprocessor (&rest rest)
  (format t ";;WARNING: The function 'x-clear-preprocessor' is deprecated. Please use 'clear-preprocessor' instead.")
  (apply #'clear-preprocessor rest))

;; end of wrappers


;; 'imported' specials

(defvar *saf-config*)

;;

;; set this to enable multi-token preprocessor rules
(defvar *enable-multi-token* nil)

(defvar *min-regex-char-code-limit* 256)

(defvar *local-to-global-point-mapping* #'identity)

(defvar *preprocess-p* nil)
(defvar *preprocessor* nil)
(defvar *x-addressing* nil)

(defvar *span* nil)
(defvar *text* nil)

;; if this is set, node and ids will start from 0 again on each call of 'preprocess'
(defparameter *reset-ids-on-each-call* t)

#+:null
(defparameter *max-multi-token* 2)
	   
;;
;; regex specials
;;

(defparameter *inter-token-str* " ")
(defparameter *inter-token-regex* 
    (ppcre:create-scanner 
     (list :SEQUENCE *inter-token-str*) ;; treat it LITERALLY
     :single-line-mode t))
(defparameter *default-token-sep* 
    (let ((ppcre:*regex-char-code-limit* 256))
      (ppcre:create-scanner "[ \\t]+" :single-line-mode t)))
(defparameter *multiple-rule-internal-separator* 
    (ppcre:create-scanner "\\t{2,}" :single-line-mode t))
(defparameter *rule-regex* 
;    (ppcre:create-scanner "^.([^\\t]+)\\t+([^\\t]*)" :single-line-mode t))
    (ppcre:create-scanner "^.([^\\t]+)\\t+([^\\t]*)(?:\\t+([^\\t]*))?" :single-line-mode t))
(defparameter *empty-line-regex* 
    (ppcre:create-scanner "^\\s*$" :single-line-mode t))
(defparameter *date-regex* 
    (ppcre:create-scanner "^.\\$[D]ate: +(.*) +\\$" :single-line-mode t))
(defparameter *ersatz-regex* 
    (ppcre:create-scanner "(?i)[^a-zA-Z]*([a-zA-Z]+Ersatz).*" :single-line-mode t))
(defparameter *import-regex* 
    (ppcre:create-scanner "^.(.*)" :single-line-mode t))

;;
;; preprocessor saf interpretation
;;

(defparameter *saf-config-str* "
token.[] -> edgeType='tok' tokenStr=content
ersatz.[] -> edgeType='tok+morph' tokenStr=content.name gMap.carg=content.surface")

;;

(defparameter *smaf-id* 0)
(defparameter *smaf-node* 0)

;; todo: convert to local variable
(defvar *max-spaces-registers* nil)

;;
;; x-strings (aka 'preprocessed-x')
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

(defun x-subseq (x start end)
  (make-instance 'preprocessed-x
    :text (subseq (text x) start end)
    :char-map (subseq (char-map x) start end)))

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

;; equality of x-string
(defun x-= (x y)
  (and (equalp (char-map x) (char-map y))
       (string= (text x) (text y))))

(defun copy-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (text x)
    :char-map (char-map x)))

;;
;;
;;

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
  target
  max-tokens
  pre-test)

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
  ;; ppcre:*regex-char-code-limit* will be set smallest possible value
  (let* ((ppcre:*regex-char-code-limit* *min-regex-char-code-limit*)
	 (x-fspp (read-preprocessor-aux file))) 
    ;; finalize x-fspp
    (setf (x-fspp-global x-fspp)
      (nreverse (x-fspp-global x-fspp)))
    (setf (x-fspp-local x-fspp)
      (nreverse (x-fspp-local x-fspp)))
    (format t "~a~%" x-fspp)
    (setf *preprocessor* x-fspp))
  (setf *saf-config* (saf:conf-read-string *saf-config-str*))
  *preprocessor*
  )

;; recursive call
(defun read-preprocessor-aux (file &key (x-fspp (make-x-fspp)))
  (when (probe-file file)
    ;; set ppcre:*regex-char-code-limit* to smallest possible value
    (setf ppcre:*regex-char-code-limit*
      (max ppcre:*regex-char-code-limit*
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
       ((or (char= #\> c) (char= #\* c))
	(read-preprocessor-rule :ersatz-augment line x-fspp n))
       ((ppcre:scan *empty-line-regex* line)
	;; empty line
	)
       (t
	(format t
		"read-preprocessor(): [line ~d] ~
                    ignoring unknown rule type `~a'~%"
		n c))))))

(defun read-preprocessor-version (line x-fspp n)
  (multiple-value-bind (match registers) 
      (cl-ppcre:scan-to-strings *date-regex* line :sharedp t)
    (if match
	(let ((version (aref registers 0)))
	  (setf (x-fspp-version x-fspp) version))
      (format t "read-preprocessor(): [line ~d] invalid `~a'~%" n line))))

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
	   (max-tokens (ignore-errors
			(parse-integer
			 (aref registers 2))))
	   ;; create scanner
	   (scanner
	    (ignore-errors
	     (ppcre:create-scanner 
	      (if (eq type :replace)
		  source
		(format nil "^~a$" source))
	      :single-line-mode t))
	    )
	   x-fsr)
      (cond
       (scanner
	(cond
	 ((not *enable-multi-token*)
	  (setf max-tokens 1))
	 ((eq :replace type)
	  nil)
	 (t
	  (setf max-tokens 
	    (my-min max-tokens (num-tokens source))))) 
	;(unless (eq :replace type)
	;  (format t "~&~A ~A" source max-tokens))
	;; create x-fsr
	(setf x-fsr (make-x-fsr :type type :source source
				:scanner scanner :target target
				:max-tokens max-tokens
				))
	(unless (eq :replace type)
	  (set-pre-test x-fsr n))
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

(defun my-min (a b)
  (cond
   ((null a) b)
   ((null b) a)
   ((and (numberp a)
	 (numberp b))
    (min a b))   
   (t
    (error "unexpected argument types"))))

;; calculate upper bound on number of tokens FSR will match
(defun num-tokens (str &key (sep *inter-token-str*))
  (cond
   ((string= " " sep)
    (my-+ 1 (max-spaces-in str)))
   (t
    ;; todo: generalise *inter-token-str* to any new character used
    ;;       if we want to constrain num-tokens
    nil)))

;;; calculate upper bound on number of tokens FSR will match
;(defun num-tokens (str &key (sep *inter-token-str*))
;  (1+ (count-token-separators str :sep sep)))

#+:null
(defun count-token-separators (str &key (sep *inter-token-str*))
  (floor
   (/ (length (ppcre:all-matches sep str))
      2)))

;; instantiate pre-test on FSR, if nec
(defun set-pre-test (x-fsr n)
  (with-slots (max-tokens source pre-test) x-fsr
    (when
	(and (numberp max-tokens)
	     (not (zerop max-tokens)))
      (handler-case
	  (setf pre-test
	    (ppcre:create-scanner 
	     (set-pre-test-aux source)
	     :single-line-mode t)
	    )
	(error nil
	  (format t "~&;WARNING: [line ~a] unable to construct pre-test for multi-token FSR: ~A ~&" n source))))))

;; get pre-test regex string
;; this will be used to test FIRST token in multi-token sequence
;; if the test then fails we there is no need to test the full token sequence
(defun set-pre-test-aux (source)
  (loop 
      with bits = (ppcre:split *inter-token-regex* source)
      with pre-test-str
      for bit in (cdr bits)
      for c = (and (> (length bit) 0)
		   (aref bit 0))
      while (eq #\? c)
      collect "(" into bits2
      collect (subseq bit 1) into bits2
      collect ")?" into bits2-end
      finally
	(push (car bits) bits2)
	;(push 'string bits2)
	(setf bits2 (append (list 'string "^") bits2 bits2-end (list "$")))
	;(setf bits2 (append bits2 bits2-end))
	(setf pre-test-str (apply #'concatenate bits2))
	(return pre-test-str)))

;; this is the main entry point
(defun preprocess (string &key (preprocessor *preprocessor*)
                               verbose format)
  ;; optionally, reset counters
  (when *reset-ids-on-each-call*
    (setf *smaf-id* 0)
    (setf *smaf-node* 0))
  
  (let* ((x (make-preprocessed-x string)) ;; construct x-string
	 (*text* string) ;; see SAF:X-PARSE todo: should go directly into saf obj
	 ;(*span* (char-map-simple-range (char-map x))) ;; not used???
	)
    ;; if no preprocessor defined...
    (when (null preprocessor)
      (error "no preprocessor available"))
    ;; process text globally
    (preprocess-global x (x-fspp-global preprocessor) :verbose verbose)
    ;; process tokens
    (let* ((x-tokens (x-split (x-fspp-tokenizer preprocessor) x))
	   (saf (x-tokens-to-saf-annots x-tokens))) ;; initial saf obj
      ;; populate saf obj
      (setf saf
	(preprocess-tokens saf
			   (x-fspp-local preprocessor)
			   :verbose verbose))
      ;; map saf obj to output format
      (x-format-preprocessed-output saf :format format))))

;; x-string => x-string
(defun preprocess-global (x &optional (global (slot-value *preprocessor* 'global)) &key verbose)
  (with-slots (text char-map) x
    ;; apply each global regex, in turn, to input x-string
    (loop
	for rule in global
	for scanner = (x-fsr-scanner rule)
	for target = (x-fsr-target rule)
	for source = (x-fsr-source rule)
	do
	  (multiple-value-bind (res matchp)
	      (x-regex-replace-all scanner x target :source source)
	    (when (and verbose matchp)
	      (format t
		      "~&GLOBAL |~a| -> |~a| ~&mapped~&|~a|~&to~&"
		      (x-fsr-source rule) (x-fsr-target rule)
		      x)
		      ;(text x))
	      (setf x res)
	      (format t "~&|~a|~%~%" x))
;	      (format t "~&|~a|~%~%" (text x)))
	    (setf x res))))
  x)

;; x-tokens (seq) => saf obj
(defun x-tokens-to-saf-annots (x-tokens)
  (loop
      with init-node = *smaf-node*
      with new-annot
      for x in x-tokens
      unless (string= (text x) "")
      do
	;; construct saf annot for each x-token
	(setf new-annot
	  (saf:make-saf-edge
	   :id (incf *smaf-id*)
	   :type :|token|
	   :source *smaf-node*
	   :target (incf *smaf-node*)
	   :content x))
	;; l-content needed for interpretation
	(saf:instantiate-edge-l-content new-annot *saf-config*)
      and collect new-annot into edges
      and collect *smaf-node* into nodes
      finally
	(return
	  (saf:make-saf 
	   :meta nil			;!
	   :lattice (saf:make-saf-lattice
		     :start-node init-node
		     :end-node *smaf-node*
		     :nodes (cons init-node nodes)
		     :edges edges)))))

#+:null
(defun clone-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (copy-seq (text x))
    :char-map (copy-tree (char-map x))))

(defun preprocess-tokens (saf local &key verbose)
  (let* ((saf-lattice (saf:saf-lattice saf)))
    (loop
      ;; for each FSR rule...
	for fsr in local
	for max-tokens = (x-fsr-max-tokens fsr)
	do
	  (loop
	      with paths = (saf:get-all-annot-paths saf-lattice max-tokens)
		  for annot-path in paths
	      do
		;; attempt to apply FSR to ANNOT
		(setf (saf:saf-lattice-edges saf-lattice) 
		  (preprocess-token-multi annot-path fsr (saf:saf-lattice-edges saf-lattice) :verbose verbose))))
  
  ;; contruct SAF output
  (loop
      with init-node = (saf:saf-lattice-start-node saf-lattice)
      with final-node = (saf:saf-lattice-end-node saf-lattice)
      with nodes
      for annot in (saf:saf-lattice-edges saf-lattice)
      for source = (saf:saf-edge-source annot)
      for target = (saf:saf-edge-target annot)
      do 
	;; collect SAF nodes
	(pushnew source nodes :test #'=)
	(pushnew target nodes :test #'=)
      finally
	(return
	  ;; make it
	  ;; ! better to modify input saf
	  (saf:make-saf 
	   :lattice (saf:make-saf-lattice 
		     :start-node (2-str init-node) ;
		     :end-node (2-str final-node) ;
		     :nodes (loop for node in nodes collect (2-str node)) ;
		     :edges (loop 
				for annot in (saf:saf-lattice-edges saf-lattice)
				collect 
				  (clean-saf-annot 
				   annot
				   )))
	   :meta  (saf:make-saf-meta 
		   :document nil
		   :addressing :|char|
		   :olac nil) ;!
	   )))))

;; concatenate x-strings = list of  key(item)
(defun x-concat (items &key (key #'identity) (sep *inter-token-str*))
  (cond
   ((null items)
    nil)
   ((not (cdr items))
    (funcall key (car items)))
   (t
    (loop
	for annot in items
	for x-orig = (funcall key annot) ;
	collect sep into text
	collect nil into char-map
	collect (text x-orig) into text
	append (char-map x-orig) into char-map
	finally
	  (setf (car text) 'string)
	  (setf text (apply #'concatenate text))
	  (setf char-map (cdr char-map))
	  (return
	    (make-instance 'preprocessed-x 
	      :text text
	      :char-map char-map))))))

;; if pre-test fails on first token, to need to test full fsr
(defun quick-check-fail (fsr annots)
  (with-slots (pre-test) fsr
    (let* ((annot1 (first annots))
	   (x1 (and annot1 (get-token-str annot1))))
      ;(format t "~&X1: '~a' PRE-TEST: '~a'~&" x1 pre-test)
      (if pre-test
	  (not (ppcre:scan pre-test (text x1)))))))

#+:null
(defun get-stringified (saf-fs)
  (loop for fv in (and (listp saf-fs) saf-fs)
      collect
	(saf::make-saf-fv 
	 :feature (2-str (saf:saf-fv-feature fv))
	 :value (saf:saf-fv-value fv))))

;;
;; get values from l-content
;;

(defun get-ersatz-type (annot)
  (saf:saf-fs-feature-value2 (saf:saf-edge-l-content annot) :|name|)) ;!

(defun get-token-str (annot)
  (saf:saf-fs-feature-value2 (saf:saf-edge-l-content annot) :|tokenStr|))

(defun get-gmap-carg (annot)
  (saf:saf-fs-feature-value2 (saf:saf-edge-l-content annot) :|gMap.carg|))

;;
;; get values from CONTENT
;; TODO: use l-content instead
;;

;; returns the ersatz-type
(defun get-ersatz-name (annot)
  (and
   (eq :|ersatz| (annot-type annot))
   (saf:saf-fs-feature-value2 (saf:saf-edge-content annot) :|type|)))

;; returns the ersatz surface string
(defun get-ersatz-surface (annot)
  (and
   (eq :|ersatz| (annot-type annot))
   (saf:saf-fs-feature-value2 (saf:saf-edge-content annot) :|surface|)))

;;
;;

;; shorthand
(defun annot-type (annot)
  (saf:saf-edge-type annot))

;; find annot with ersatz-type + span (x-string) matching input
(defun match-ersatz (x annots)
  (find x annots :test #'x-= :key #'get-ersatz-name))

;; TODO: factor this function
;; attempt to apply FSR to ANNOT
(defun preprocess-token-multi (annots fsr saf-annots &key verbose)
  ;; bail out if quick-check fails
  (when (quick-check-fail fsr annots)
      (return-from preprocess-token-multi saf-annots))

  (let* (;; FSR slots
	 (type (x-fsr-type fsr))
	 (scanner (x-fsr-scanner fsr))
	 (target (x-fsr-target fsr))
	 ;; ANNOT slots
	 (x-orig (x-concat annots :key #'get-token-str))
	 (annot-source (saf:saf-edge-source (first annots)))
	 (annot-target (saf:saf-edge-target (car (last annots))))
	 x-str
	 new-annot)
    (when (ppcre:scan scanner (text x-orig))
      ;; found a match...
      ;; so copy input then apply REPLACE
      (setf x-str (copy-preprocessed-x x-orig))
      (x-regex-replace scanner x-str target)
      ;; debugging output
      (when verbose
	(format t
		"~&~a |~a| -> |~a| in |~a| gave |~a|"
		type (x-fsr-source fsr) target 
		(text x-orig) (text x-str)))
      ;; fix NILs in CHAR-MAP
      (setf (char-map x-str)
	(substitute (char-map-simple-range (char-map x-orig))
		    nil
		    (char-map x-str)))
      ;; DELETE existing annot when nec
      (when (member type '(:ersatz :substitute))
	(setf saf-annots 
	  (set-difference saf-annots annots)))
      ;; new (perhaps multiple) tokens
      (when t ;
	(loop
	    with surface
	    with xstr0 =
	      ;; needed for extraction of ersatz'd text
	      (let ((x (make-preprocessed-x (text x-orig))))
		(x-regex-replace scanner x target)
		(setf (char-map x)
		  (substitute (char-map-simple-range (char-map x-orig))
			      nil
			      (char-map x)))
		x)
	    with x-bits =
	      ;; split into bits on *inter-token-regex*
	      (remove "" 
		      (x-split *inter-token-regex* x-str)
		      :key #'(lambda (x)
			       (text x))
		      :test #'string=)
	    with x-bits0 = 
	      ;; needed for extraction of ersatz'd text
	      (remove "" 
		      (x-split *inter-token-regex* xstr0)
		      :key #'(lambda (x)
			       (text x))
		      :test #'string=)
	    for i from 0 to (1- (length x-bits))
	    for x-bit = (nth i x-bits)
	    for x-bit0 = (nth i x-bits0)
	    ;; ensure tie-in to node numbers correctly
	    with last-node = annot-source
	    for next-node = (if (= i (1- (length x-bits)))
				annot-target
			      (incf *smaf-node*))
	    ;; check bit contains ersatz
	    for ersatz-match = 
	      (multiple-value-bind (x flag)
		  (x-regex-replace-all *ersatz-regex* x-bit0 "\\1")
		(if flag x))
	    with range
	    do
	      (cond
	       (ersatz-match
		;; annot is ERSATZ
		(setf range (char-map-simple-range (char-map ersatz-match))) ;; get RANGE
		(setf surface (x-subseq x-orig (car range) (cdr range)))
		(setf (char-map ersatz-match)
		  (loop
		      with surface-range = (char-map-simple-range (char-map surface))
		      for r in (char-map ersatz-match)
		      collect surface-range))

		;; now check whether surface contains an ersatz strings, in which case
		;; we need to retrieve the surface from corresponding ersatz
		(multiple-value-bind (x0 flag0)
		    (x-regex-replace-all *ersatz-regex* (make-preprocessed-x (text surface)) "\\1")
		  (when flag0
		    (let* ((range0 (char-map-simple-range (char-map x0)))
			   (surface0 (x-subseq surface (car range0) (cdr range0))))
		      (setf surface
			(get-ersatz-surface (match-ersatz surface0 annots)))
		      )))
		      
		(setf new-annot 
		  (saf:make-saf-edge
		  :id (incf *smaf-id*)
		  :type :|ersatz|
		  :source last-node
		  :target next-node
		  :content (list
			    (saf:make-saf-fv
			     :feature :|surface|
			     :value surface)
			    (saf:make-saf-fv
			     :feature :|name| ;; the ersatz 'token'
			     :value x-bit)
			    (saf:make-saf-fv
			     :feature :|type|
			     :value ersatz-match)))) ;; the ersatz name
		(saf:instantiate-edge-l-content new-annot *saf-config*)
		(push new-annot saf-annots))
	       (t
		;; annot is normal TOKEN
		(setf new-annot
		  (saf:make-saf-edge
		       :id (incf *smaf-id*)
		       :type :|token|
		       :source last-node
		       :target next-node
		       :content x-bit))
		(saf:instantiate-edge-l-content new-annot *saf-config*)
		(push new-annot saf-annots)))
	      (setf last-node next-node) ;; keep nodes in sync
	      ))))
  saf-annots)

;; annot => annot
;; convert saf annots to starndard form
(defun clean-saf-annot (annot)
  (let* ((type (saf:saf-edge-type annot))
	 (content (saf:saf-edge-content annot)))

    (let* ((surface
	    (cond
	     ((eq type :|token|)
	      content)
	     ((eq type :|ersatz|)
	      (saf:saf-fs-feature-value2 content :|surface|))
	     (t
	      (error "internal"))))
	   (range (char-map-simple-range
		   (char-map surface))))
      ;; instantiate FROM/TO
      (setf (saf:saf-edge-from annot) (car range))
      (setf (saf:saf-edge-to annot) (cdr range)))
    
    ;; clear L-CONTENT
    (setf (saf:saf-edge-l-content annot) nil)

    ;; convert values to string
    (setf (saf:saf-edge-id annot) (2-str (saf:saf-edge-id annot))) ;;int=>str
    (setf (saf:saf-edge-source annot) (2-str (saf:saf-edge-source annot))) ;;int=>str
    (setf (saf:saf-edge-target annot) (2-str (saf:saf-edge-target annot))) ;;int=>str
    (setf (saf:saf-edge-from annot) (2-str (saf:saf-edge-from annot))) ;;int=>str
    (setf (saf:saf-edge-to annot) (2-str (saf:saf-edge-to annot))) ;;int=>str
    
    ;; convert x-strings to strings
    ;; simple content
    (if (typep content 'preprocessed-x)
	(setf (saf:saf-edge-content annot)
	  (text content))
      ;; FV content
      (loop 
	  for fv in (and (listp content) content)
	  for value = (saf:saf-fv-value fv)
	  when (typep value 'preprocessed-x)
	  do
	    (setf (saf:saf-fv-value fv) (text (saf:saf-fv-value fv)))))
    )
  annot)

;;HACK remove when PET can handle unsorted lattice
(defun node-to-int (node)
  (parse-integer node))

;; convert saf obj to output format
;; NIL is raw saf/smaf object
;; :yy (= :pet) is YY MODE
;; :saf / :smaf is saf/smaf XML
(defun x-format-preprocessed-output (saf &key format)
  
  ;; order lattice edges by standoff point
  ;; (coz therz a bug in PET)
  (saf:rename-nodes-by-point-order saf)
  (let ((lattice (saf:saf-lattice-edges (saf:saf-lattice saf))))
    (setf (saf:saf-lattice-edges (saf:saf-lattice saf))
      (sort lattice
	    #'<
	    :key #'(lambda (x)
		     (node-to-int
		      (saf:saf-edge-source x))))))
  (cond
   ((or (eq format :yy)
	(eq format :pet)) ;; deprecate this...?
    (loop
	with length = (1- (length (saf:saf-lattice-nodes (saf:saf-lattice saf))))
	with saf-annots = (saf:saf-lattice-edges (saf:saf-lattice saf))
	for annot in saf-annots
	for type = (saf:saf-edge-type annot)
	for id = (saf:saf-edge-id annot)
	for start = (saf:saf-edge-from annot)
	for end = (saf:saf-edge-to annot)
	for form =  (and (saf:instantiate-edge-l-content annot *saf-config*)
			 (get-token-str annot))
	for surface = (if (eq type :|ersatz|)
			  (get-gmap-carg annot)
			form)
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
    (saf:to-xml saf :format format))
   ((eq format :list) ;; deprecate this...?
    (loop
	with length = (1- (length (saf:saf-lattice-nodes (saf:saf-lattice saf))))
	with saf-annots = (saf:saf-lattice-edges (saf:saf-lattice saf))
	for annot in saf-annots
	for type = (saf:saf-edge-type annot)
	for id = (saf:saf-edge-id annot)
	for start = (saf:saf-edge-from annot)
	for end = (saf:saf-edge-to annot)
	for form =  (and (saf:instantiate-edge-l-content annot *saf-config*)
			 (get-token-str annot))
	for surface = (if (eq type :|ersatz|)
			  (get-gmap-carg annot)
			form)
	for token = (list id start end form surface)
	collect token into tokens
	finally 
	  (return
	    (values tokens length))))
;   ((eq format :list)
;    (error ":list format no longer supported"))
   (t
    (error "unhandled format argument: ~a" format))))

;; for YY format
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

;;
;; x-regex code (regexes on x-strings)
;;

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
      ; with reg ;; register
       with reg-start
       with reg-end
       for c across replace-string
		    ;; 
		    ;; register matches
		    ;;
       for flag = (and esc (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) ;; to_do: \& \` \' \{N}
       for reg = (and flag
		      (1- (read-from-string (string c))))
       if (and reg (< reg (length  reg-starts)))
       do 
	  (setf reg-start (aref reg-starts reg))
	  (setf reg-end (aref reg-ends reg))
       and append (coerce (subseq target-string reg-start reg-end) 'list) ;; replacement text
       and append (subseq char-map reg-start reg-end) into new-char-map  ;; replacement spans
       and do (setf esc nil)
       else if reg
       do
	 (format t "~&;WARNING: ignore nonexistent register '\\~a' in '~a'" 
		 c replace-string)
	    ;; ignore nonexistent register reference
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
	   ;; sanity check (start <= end)
      if (> (or (car r) 0) (or (cdr r) 0))
      do (let ((x (car r)))
	   (setf (car r) (cdr r))
	   (setf (cdr r) x))
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
(defun x-regex-replace-all (scanner x target &key source)
  (with-slots (text char-map) x
    (let ((repl-l (make-repl-list)))
      ;;
      ;; Handler-case is necessary due to an apparent bug in the underlying PPCRE regex module.
      ;; An error is thrown _in_some_cases_ (!) when input string contains a character with code
      ;; point greater than the value of ppcre:*regex-char-code-limit* with which the scanner
      ;; was created.
      ;;
      (handler-case
	  (setf (text x)
	    (cl-ppcre:regex-replace-all 
	     scanner text 
	     #'(lambda (a b c d e f g) 
		 (catch-repl a b c d e f g 
			     :replace-string target 
			     :repl-l repl-l
			     :char-map char-map))))
	(t () 
	  (format t "~&;WARNING: unexpected error thrown by function 'cl-ppcre:regex-replace-all'.")
	  (setf repl-l (make-repl-list))
	  (cond
	   (source
	    (format t " Recovering...")
	    (setf (text x)
	      (cl-ppcre:regex-replace-all 
	       source text 
	       #'(lambda (a b c d e f g) 
		   (catch-repl a b c d e f g 
			       :replace-string target 
			       :repl-l repl-l
			       :char-map char-map)))))
	   (t
	    (format t " Continuing...")
	    ))))
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

(defun max-spaces-in (regex)
  (setf *max-spaces-registers* nil)
  (max-spaces (cl-ppcre::parse-string regex)))

;; ignored: parse tree synonyms
(defun max-spaces (tree)
  (cond
   ((stringp tree)
    (max-spaces-string tree))
   ((characterp tree)
    (max-spaces-char tree))
   ((eq :VOID tree)
    0)
   ((eq :EVERYTHING tree)
    1)
   ((eq :WORD-BOUNDARY tree)
    0)
   ((eq :NON-WORD-BOUNDARY tree)
    0)
   ((eq :DIGIT-CLASS tree)
    0)
   ((eq :NON-DIGIT-CLASS tree)
    1)
   ((eq :WORD-CHAR-CLASS tree)
    0)
   ((eq :NON-WORD-CHAR-CLASS tree)
    1)
   ((eq :WHITESPACE-CHAR-CLASS tree)
    1)
   ((eq :NON-WHITESPACE-CHAR-CLASS tree)
    0)
   ((eq :START-ANCHOR tree)
    0)
   ((eq :END-ANCHOR tree)
    0)
   ((eq :MODELESS-START-ANCHOR tree)
    0)
   ((eq :MODELESS-END-ANCHOR tree)
    0)
   ((eq :MODELESS-END-ANCHOR-NO-NEWLINE tree)
    0)
   ((eq :CASE-SENSITIVE-P tree)
    0)
   ((eq :CASE-INSENSITIVE-P tree)
    0)
   ((eq :MULTI-LINE-MODE-P tree)
    0)
   ((eq :NOT-MULTI-LINE-MODE-P tree)
    0)
   ((eq :SINGLE-LINE-MODE-P tree)
    0)
   ((eq :NOT-SINGLE-LINE-MODE-P tree)
    0)
   ((not (listp tree))
    (max-spaces-unknown tree))
   ((eq :FLAGS (car tree))
    0)
   ((eq :SEQUENCE (car tree))
    (max-spaces-sum (cdr tree)))
   ((eq :GROUP (car tree))
    (max-spaces-sum (cdr tree)))
   ((eq :ALTERNATION (car tree))
    (max-spaces-max (cdr tree)))
   ((eq :BRANCH (car tree)) ;;checkme
    (max-spaces (third tree)))
   ((eq :POSITIVE-LOOKAHEAD (car tree)) ;;checkme
    0)
   ((eq :NEGATIVE-LOOKAHEAD (car tree)) ;;checkme
    0)
   ((eq :POSITIVE-LOOKBEHIND (car tree)) ;;checkme
    0)
   ((eq :NEGATIVE-LOOKBEHIND (car tree)) ;;checkme
    0)
   ((eq :GREEDY-REPETITION (car tree))
    (max-spaces-multiply (third tree) (fourth tree)))
   ((eq :NON-GREEDY-REPETITION (car tree))
    (max-spaces-multiply (third tree) (fourth tree)))
   ((eq :STANDALONE (car tree))
    (max-spaces (second tree)))
   ((eq :REGISTER (car tree))
    (set-max-spaces-register (max-spaces (second tree))))
   ((eq :BACK-REFERENCE (car tree))
    (get-max-spaces-register (second tree)))
   ((eq :REGEX (car tree))
    (max-spaces (cl-ppcre::parse-string (second tree))))
   ((eq :CHAR-CLASS (car tree))
    (max-spaces-char-class (cdr tree)))
   ((eq :INVERTED-CHAR-CLASS (car tree))
    (- 1 (max-spaces-char-class (cdr tree))))
   (t
    (max-spaces-unknown tree))))

(defun max-spaces-unknown (tree)
  (format t "~&;; WARNING: unknown PPCRE parse-tree component: ~A" tree)
  nil)

(defun max-spaces-string (string)
  (floor
   (/ (length (ppcre:all-matches #\Space string))
      2)))

(defun max-spaces-char (char)
  (if (char= char #\Space)
      1
    0))

(defun max-spaces-sum (trees)
  (loop
      for tree in trees
      for num-spaces = (max-spaces tree)
      if (null num-spaces)
      do (return-from max-spaces-sum nil)
      sum num-spaces))

(defun max-spaces-max (trees)
  (loop
      for tree in trees
      for num-spaces = (max-spaces tree)
      if (null num-spaces)
      do (return-from max-spaces-max nil)
      maximize num-spaces))

(defun max-spaces-multiply (max tree)
  (my-* max (max-spaces tree)))

(defun my-+ (a b)
  (cond
   ((or (null a)
	(null b))
    nil)
   ((and (numberp a)
	 (numberp b))
    (+ a b))   
   (t
    (error "unexpected argument types"))))

(defun my-* (a b)
  (cond
   ((or (and (numberp a) (zerop a))
	(and (numberp b) (zerop b)))
    0)
   ((or (null a)
	(null b))
    nil)
   ((and (numberp a)
	 (numberp b))
    (* a b))
   (t
    (error "unexpected argument types"))))
     
(defun set-max-spaces-register (num-spaces)
  (setf *max-spaces-registers* (append *max-spaces-registers*
				       (list num-spaces)))
  num-spaces)

(defun get-max-spaces-register (num-register)
  (nth (1- num-register) *max-spaces-registers*))

(defun max-spaces-char-class (items)
  (loop
      for item in items
      when (= 1 (max-spaces-char-class-item item))
      do (return-from max-spaces-char-class 1))
  0)

(defun max-spaces-char-class-item (item)
  (cond
   ((characterp item)
    (max-spaces-char item))
   ((and (listp item)
	 (eq :RANGE (car item)))
    (let ((space-code (char-code #\Space))
	  (min-code (char-code (second item)))
	  (max-code (char-code (third item))))
      (if (and
	   (>= space-code min-code)
	   (<= space-code max-code))
	  1
	0)))
   ((eq :DIGIT-CLASS item)
    0)
   ((eq :NON-DIGIT-CLASS item)
    1)
   ((eq :WORD-CHAR-CLASS item)
    0)
   ((eq :NON-WORD-CHAR-CLASS item)
    1)
   ((eq :WHITESPACE-CHAR-CLASS item)
    1)
   ((eq :NON-WHITESPACE-CHAR-CLASS item)
    0)
   (t
    (format t "~&;; WARNING: unknown PPCRE char-class item: ~a" item)
    nil)))