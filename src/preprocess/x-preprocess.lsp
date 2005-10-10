;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: x-preprocess.lsp
;;;      module: input preprocessing mimicry (collection of rough utilities)
;;;     version: 0.0 (30-jan-03)
;;;  written by: oe, csli stanford
;;;              bmw, cambridge
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; ToDo
;;;
;;; rework to implement the following specification:
;;;
;;; @ version
;;;
;;; : token separator (also marks transition from string- to token-level rules)
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

(in-package :lkb)

(defvar *x-preprocess-p* nil)
(defvar *x-preprocessor-debug-p* t)
(defvar *x-preprocessor* nil)
(defvar *x-addressing* nil)

(defstruct x-fspp
  version
  (tokenizer (ppcre:create-scanner "[ \\t]+"))
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

(defun x-read-preprocessor (file &key (x-fspp (make-x-fspp) x-fsppp))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (let* ((path (pathname file))
             (type (pathname-type path)))
        (format 
         t 
         "~&Reading preprocessor rules `~a~@[.~a~]'~%" 
         (pathname-name path) type)
        (loop
            with separator = (ppcre:create-scanner "\\t+")
            for n from 1
            for line = (read-line stream nil nil)
            for length = (length line)
            for c = (unless (zerop length) (char line 0))
            when (and c (not (char= c #\;))) do
              (multiple-value-bind (start end) (ppcre:scan separator line)
                (cond
		 ;; @ version
                 ((char= c #\@)
                  (let* ((version (subseq line 1 end))
                         (version (if (string= version "$Date: " :end1 7)
                                    (subseq version 7 (- (length version) 2))
                                    version)))
                    (setf (x-fspp-version x-fspp) 
                      (string-trim '(#\Space) version))))
		 ;; < import
                 ((char= c #\<)
                  (let* ((name (subseq line 1 end))
                         (file (or (probe-file name)
                                   (probe-file (merge-pathnames name path)))))
                    (if file
                      (x-read-preprocessor file :x-fspp x-fspp)
                        (format
                         t
                         "x-read-preprocessor(): [~d] unable to include `~a'~%"
                         n name))))
		 ;; : tokenizer
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (x-fspp-tokenizer x-fspp) tokenizer)))
		 ;; ! replace
		 ;; - substitute
		 ;; + augment
		 ;; ^ ersatz
                 ((member c '(#\! #\- #\+ #\^) :test #'char=)
                  (if (and start end)
                    (let* ((type (case c
                                   (#\! :replace)
                                   (#\- :substitute)
                                   (#\+ :augment)
                                   (#\^ :ersatz)))
                           (source (subseq line 1 start))
                           (target (subseq line end))
                           (scanner
                            (ignore-errors
                             (ppcre:create-scanner 
                              (if (eq type :replace)
                                source
                                (format nil "^~a$" source)))))
                           (match (make-x-fsr :type type :source source
                                            :scanner scanner :target target)))
                      (if scanner
                        (if (eq type :replace)
                          (push match (x-fspp-global x-fspp))
                          (push match (x-fspp-local x-fspp)))
                        (format
                         t
                         "x-read-preprocessor(): [~d] invalid pattern `~a'~%"
                         n source(x-preprocess "The dog--barked." :format :list))))
                    (format
                     t
                     "x-read-preprocessor(): [~d] invalid `~a'~%"
                     n line)))
                 (t
                  (format
                   t
                   "x-read-preprocessor(): [~d] ~
                    ignoring unknown rule type `~a'~%"
                   n c))))
            when (null line) do
              (unless x-fsppp
                (setf (x-fspp-global x-fspp)
                  (nreverse (x-fspp-global x-fspp)))
                (setf (x-fspp-local x-fspp)
                  (nreverse (x-fspp-local x-fspp))))
              (unless x-fsppp (format t "~a~%" x-fspp))
              (return (if x-fsppp x-fspp (setf *x-preprocessor* x-fspp))))))))

(defun x-preprocess (string &key (preprocessor *x-preprocessor*) 
                               (globalp t) (tokenp t)
                               (verbose *x-preprocessor-debug-p*)
                               (format :list))
  (let ((x (make-preprocessed-x string)))
    ;; if no preprocessor defined...
    (when (null preprocessor)
      (return-from x-preprocess (and (eq format :lkb) x)))
    ;; process text globally
    (when globalp
      (x-preprocess-global x (x-fspp-global preprocessor)
			   :verbose verbose))
    ;; process tokens
    (multiple-value-bind (result length)
	(x-preprocess-tokens (x-split (x-fspp-tokenizer preprocessor) x) 
			   (x-fspp-local preprocessor)
			   :tokenp tokenp
			   :verbose verbose)
      ;; get output in desired format
      (x-format-preprocessed-output
       (nreverse (x-tokens-to-result (nreverse result) :format format))
       length format))
;    x
    ))

(defun x-preprocess-global (x &optional (global (slot-value *x-preprocessor* 'global)) &key verbose)
  (with-slots (text char-map) x
    (loop
	for rule in global
	for scanner = (x-fsr-scanner rule)
	for target = (x-fsr-target rule)
	for old-text = text
	do
	  (setf x (x-regex-replace-all scanner x target))
	when (and (eq verbose :trace) (not (string= old-text text))) do
	  (format
	   t
	   "~&|~a|~%  |~a|~%  |~a|~%~%"
	   (x-fsr-source rule) old-text text)
	finally
	  (return x))))

(defun x-preprocess-tokens (x-l &optional (local (slot-value *x-preprocessor* 'local)) &key tokenp verbose)
  (loop
      with result
      with token
      with length = 0
      for x in x-l
      for x-match = (make-instance 'preprocessed-x 
		      :text (text x)
		      :char-map (char-map x))
      unless (string= (text x) "") do
	(incf length)
	(setf token x)
	(loop
	    with extra = nil
	    for rule in (when tokenp local)
	    for type = (x-fsr-type rule)
	    for scanner = (x-fsr-scanner rule)
	    for target = (x-fsr-target rule)
	    do (setf x-match (x-regex-replace scanner x-match target))
			;;
			;; _fix_me_
			;; regex-replace() always returns a fresh string, even if the
			;; pattern did _not_ match; to make this more efficient, it
			;; seems, we would have to use scan() and then glue together
			;; the result from parsing .target. and filling in register
			;; matches as needed :-{.                     (31-jan-03; oe)
			;;
	    when (and (eq verbose :trace) (not (equalp x x-match))) do
	      (format
	       t
	       "~&|~a|~%  |~a|~%  |~a|~%~%"
	       (x-fsr-source rule) (text x) (text x-match))
	    do
	      (case type
		(:ersatz
		 ;;
		 ;; _fix_me_
		 ;; to do ersatzes properly, they should no longer be available
		 ;; to subsequent rule processing: presumably, we should build
		 ;; an ersatzing table and use non-string tokens (indices into
		 ;; the table) instead.                         (1-feb-03; oe)
		 ;;
		 (unless (string= (text x) (text x-match))
		   (push (list :ersatz x) extra)
		   (setf token x-match)))
		(:augment
		 (unless (string= (text x) (text x-match))
		   (push (list :augment x-match) extra)))
		(:substitute
		 (setf token x-match))
		(t
		 (error "unhandled type: ~a" type)))
	    finally
	      (push (cons token extra) result))
      finally
	(return (values result length))))

(defun x-tokens-to-result (tokens &key verbose 
				     format) ;;get rid of this
  (loop
      with result = nil
      with i = 0
      with id = 41
      for (form . extra) in tokens
      for surface = (or (second (find :ersatz extra :key #'first)) form)
      for start = i
      for end = (incf i)
      do
	(push (list (incf id) start end form surface) result)
	(unless (eq format :lkb)
	  (loop
	      for (type form) in extra
	      unless (eq type :ersatz) do 
		(push (list (incf id) start end form form) result)))
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

(defun x-format-preprocessed-output (result length &optional (format :list))
  (cond
   ((eq format :lkb)
    (loop
	for i = -1
	for token in result
	for start = (second token)
	for form = (fourth token)
	unless (= start i) collect form into forms
	finally 
	  (return (values (format nil "~{~a~^ ~}" forms)
			  (length forms)))))
   ((or (eq format :yy)
	(eq format :pet)) ;; deprecate this...?
    (loop
	for (id start end form surface) in result
	for token = (format 
		     nil 
		     "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
		     id start end 
		     (escape-string form) (escape-string surface))
	collect token into tokens
	finally 
	  (return
	    (values (format nil "~{~a~^ ~}" tokens) length))))
   ((eq format :pic)
    (error "not implemented"))
   ((eq format :maf)
    (setf *x-addressing* :xchar)
    (let ((strm (make-string-output-stream)))
      (format strm "~a" (maf-header :addressing *x-addressing*))
      (mapcar #'(lambda (x) 
		  (format strm "~a"
			  (p-token-to-maf-token x)))
	      result)
      (format strm "</maf>")
      (get-output-stream-string strm)))
   ((eq format :list)
    (values result length))
   (t
    (error "unhandled format argument: ~a" format))))

;;(42 0 1 "the" "the")
;;(42 0 1 "EmailErsatz" "bmw20@cam.ac.uk")
(defun p-token-to-maf-token (p-token)
  (let* ((x (fourth p-token))
	 (r (char-map-simple-range (char-map x))))
    (format nil "<token id='~a' from='~a' to='~a' value='~a'/>"
	    (first p-token)
	    (format nil ".~a" (car r))
	    (format nil ".~a" (cdr r))
	    (text x)
	    )))

(defun x-clear-preprocessor ()
  (setf *x-preprocessor* nil))

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
	      char-map)
       )))

(defun char-map-simple-range (char-map)
  (cons
   (car (first char-map))
   (cdr (car (last char-map)))))

(defun make-preprocessed-x (str)
  (let ((x (make-instance 'preprocessed-x :text str)))
    (setf (char-map x)
      (make-char-map (length (text x))))
    x))

(defun make-char-map (l)
  (loop
      with i = 0
      while (< i l)
      collect (cons i (incf i))))

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
       with reg
       with reg-start
       with reg-end
       for c across replace-string
		    ;; to_do: \& \` \' \{N}
       if (and esc (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
       do (setf reg (1- (read-from-string (string c))))
	  (setf reg-start (aref reg-starts reg))
	  (setf reg-end (aref reg-ends reg))
       and append (coerce (subseq target-string reg-start reg-end) 'list)
       and append (subseq char-map reg-start reg-end) into new-char-map 
       and do (setf esc nil)
       else if esc 
       collect c
       and collect (cons nil nil) into new-char-map
       and do (setf esc nil)
       else if (char= c #\\)
       do (setf esc t)
       else collect c
       and collect (cons nil nil) into new-char-map
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

(defun fill-char-map (char-map start end)
  (loop
      with last-s = start
      for r in char-map
      for s = (car r)
      if s do (setf last-s s)
      else do (setf (car r) last-s))
  (loop
      with last-e = end
      for r in (reverse char-map)
      for e = (cdr r)
      if e do (setf last-e e)
      else do (setf (cdr r) last-e))
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
	       (update-char-map list x)))))
  x)

(defun x-regex-replace (scanner x target)
  (with-slots (text char-map) x
    (let ((repl-l (make-repl-list)))
      (setf (text x)
	(cl-ppcre:regex-replace 
	 scanner text 
	 #'(lambda (a b c d e f g) 
	     (catch-repl a b c d e f g 
			 :replace-string target 
			 :repl-l repl-l
			 :char-map char-map))))
      (with-slots (list) repl-l
	     (when list
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

(defun x-parse (str)
  (parse-from-maf (x-preprocess str :format :maf)))