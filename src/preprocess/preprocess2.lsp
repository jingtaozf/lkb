;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2005
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: preprocess.lsp
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

(defvar *preprocessor-debug-p* t)

(defvar *preprocessor* nil)

(defstruct fspp
  version
  (tokenizer (ppcre:create-scanner "[ \\t]+"))
  global
  local)

(defmethod print-object ((object fspp) stream)
  (format 
   stream 
   "#[FSPP (~d global, ~d token-level rules @ `~a')]"
   (length (fspp-global object)) (length (fspp-local object)) 
   (fspp-tokenizer object)))

(defstruct fsr
  type
  source
  scanner
  target)

(defmethod print-object ((object fsr) stream)
  (with-slots (type source target) object
      (format 
       stream 
       "#[FSR (~a |~a| -> |~a|)]"
       type source target
       )))

(defun read-preprocessor (file &key (fspp (make-fspp) fsppp))
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
                    (setf (fspp-version fspp) 
                      (string-trim '(#\Space) version))))
		 ;; < import
                 ((char= c #\<)
                  (let* ((name (subseq line 1 end))
                         (file (or (probe-file name)
                                   (probe-file (merge-pathnames name path)))))
                    (if file
                      (read-preprocessor file :fspp fspp)
                        (format
                         t
                         "read-preprocessor(): [~d] unable to include `~a'~%"
                         n name))))
		 ;; : tokenizer
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (fspp-tokenizer fspp) tokenizer)))
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
                           (match (make-fsr :type type :source source
                                            :scanner scanner :target target)))
                      (if scanner
                        (if (eq type :replace)
                          (push match (fspp-global fspp))
                          (push match (fspp-local fspp)))
                        (format
                         t
                         "read-preprocessor(): [~d] invalid pattern `~a'~%"
                         n source(preprocess "The dog--barked." :format :list))))
                    (format
                     t
                     "read-preprocessor(): [~d] invalid `~a'~%"
                     n line)))
                 (t
                  (format
                   t
                   "read-preprocessor(): [~d] ~
                    ignoring unknown rule type `~a'~%"
                   n c))))
            when (null line) do
              (unless fsppp
                (setf (fspp-global fspp)
                  (nreverse (fspp-global fspp)))
                (setf (fspp-local fspp)
                  (nreverse (fspp-local fspp))))
              (unless fsppp (format t "~a~%" fspp))
              (return (if fsppp fspp (setf *preprocessor* fspp))))))))

(defun preprocess (string &key (preprocessor *preprocessor*) 
                               (globalp t) (tokenp t)
                               (verbose *preprocessor-debug-p*)
                               (format :list))
  (let ((x (make-preprocessed-x string)))
    ;; if no preprocessor defined...
    (when (null preprocessor)
      (return-from preprocess (and (eq format :lkb) x)))
    ;; process text globally
    (when globalp
      (preprocess-global-x x (fspp-global preprocessor)
			   :verbose verbose))
    ;; process tokens
    (multiple-value-bind (result length)
	(preprocess-tokens (x-split (fspp-tokenizer preprocessor) x) 
			   (fspp-local preprocessor)
			   :tokenp tokenp
			   :verbose verbose)
      ;; get output in desired format
      (format-preprocessed-output
       (nreverse (tokens-to-result (nreverse result) :format format))
       length format))
;    x
    ))

(defun preprocess-global-x (x &optional (global (slot-value *preprocessor* 'global)) &key verbose)
  (with-slots (text char-map) x
    (loop
	for rule in global
	for scanner = (fsr-scanner rule)
	for target = (fsr-target rule)
	for old-text = text
	do
	  (setf x (x-regex-replace-all scanner x target))
	when (and (eq verbose :trace) (not (string= old-text text))) do
	  (format
	   t
	   "~&|~a|~%  |~a|~%  |~a|~%~%"
	   (fsr-source rule) old-text text)
	finally
	  (return x))))

(defun preprocess-tokens (x-l &optional (local (slot-value *preprocessor* 'local)) &key tokenp verbose)
  (loop
      with result
      with token
      with length = 0
      for x in x-l
      unless (string= (text x) "") do
	(incf length)
	(setf token x)
	(loop
	    with extra = nil
	    for rule in (when tokenp local)
	    for type = (fsr-type rule)
	    for scanner = (fsr-scanner rule)
	    for target = (fsr-target rule)
	    for x-match = (make-instance 'preprocessed-x 
			    :text (text x)
			    :char-map (char-map x))
	    do (x-regex-replace scanner x-match target)
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
	       (fsr-source rule) (text x) (text x-match))
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

(defun tokens-to-result (tokens &key verbose 
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

(defun format-preprocessed-output (result length &optional (format :list))
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
    (let ((strm (make-string-output-stream)))
      (format strm "~a" (maf-header))
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
  (format nil "<token id='~a' from='~a' to='~a' value='~a'/>"
	  (first p-token)
	  "?"
	  "?"
	  (fourth p-token)
	  ))

(defun escape-string (string &key (syntax :c))
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

(defun clear-preprocessor ()
  (setf *preprocessor* nil))

(defun preprocess-for-pet (string &optional tagger)
  (if (and tagger (consp tagger) (keywordp (first tagger)))
    (multiple-value-bind (tokens length)
        (case (first tagger)
          (:tnt
           (apply 
            #'tag-tnt
            (preprocess string :format :list :verbose nil)
            (rest tagger))))
      (loop
          for (id start end form surface . tags) in tokens
          for token = (format 
                       nil 
                       "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\",~
                       ~{ ~s ~,4f~})" 
                       id start end 
                       (escape-string form) (escape-string surface) tags)
          collect token into tokens
          finally 
            (return (values (format nil "~{~a~^ ~}" tokens) length))))
    (preprocess string :format :yy :verbose nil)))

(defparameter *tagger-application*
  '((:tnt "tnt -z100 /user/oe/src/tnt/models/wsj -")))

(defun tag-tnt (tokens &optional run &key (n 1))
  (labels ((commentp (string)
             (and (>= (length string) 2)
                  (characterp (char string 0)) (char= (char string 0) #\%)
                  (characterp (char string 1)) (char= (char string 1) #\%))))

    (let* ((run (or run 
		    (loop
			for run in *tagger-application*
		       when (eq (first run) :tnt)
			return (first (rest run)))
		    "tnt -z100 /user/oe/src/tnt/models/wsj -"))
	   (command (format nil "exec ~a" run *tagger-application*))
	   (input (format nil "/tmp/.tnt.in.~a" (current-user)))
	   (output (format nil "/tmp/.tnt.out.~a" (current-user)))
	   (length 0) analyses)
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (loop
            with i = -1
            for token in tokens
            for start = (second token)
            unless (= i start) do
              (setf i start)
              (incf length)
              (format stream "~a~%" (fifth token))
            finally (format stream "~%~%")))
      (run-process
       command :wait t 
       :input input :output output :if-output-exists :supersede
       :error-output "/dev/null" :if-error-output-exists :append)
      (with-open-file (stream output :direction :input)
        (loop
            with buffer = (make-array 512 
                                      :element-type 'character
                                      :adjustable t :fill-pointer 0)
            with i = 0
            for string = (read-line stream nil nil)
            while (and string (not (zerop (length string))))
            unless (commentp string) do 
              (incf i)
              (loop
                  with foo = nil
                  with n = 0
                  initially (setf (fill-pointer buffer) 0)
                  for c across string
                  when (char= c #\tab) do
                    (when (not (zerop (fill-pointer buffer)))
                      (push (if (and (evenp n) (not (zerop n)))
                              (read-from-string (copy-seq buffer))
                              (copy-seq buffer))
                            foo)
                      (setf (fill-pointer buffer) 0)
                      (incf n))
                  else do
                       (vector-push c buffer)
                  finally
                    (when (not (zerop (fill-pointer buffer)))
                      (push (read-from-string (copy-seq buffer)) foo))
                    (when foo
                      (push (nreverse foo) analyses)))))
      (loop
          with tags = (make-array (length analyses))
          for analysis in (nreverse analyses)
          for i from 0
          do (setf (aref tags i) (rest analysis))
          finally
            (loop
                for token in tokens
                for analysis = (aref tags (second token))
                do (nconc token (loop
                                    with n = (* 2 n)
                                    for foo in analysis
                                    while (< 0 n) 
                                    collect foo do (decf n)))))
      (values tokens length))))

;;
;; (bmw - oct 05)
;;

(defclass preprocessed-x ()
  ((text :initform nil :accessor text :initarg :text)
   (char-map :initform nil :accessor char-map :initarg :char-map)))

(defmethod print-object ((object preprocessed-x) stream)
  (with-slots (text char-map) object
      (format 
       stream 
       "|~a|:~a"
       text char-map
       )))

(defun make-preprocessed-x (str)
  (let ((x (make-instance 'preprocessed-x :text str)))
    (setf (char-map x)
      (make-char-map (length (text x))))
    x))

(defun make-char-map (l)
  (loop for i from 0 to (1- l)
      collect i))

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

(defun catch-repl (target-string start end match-start match-end reg-starts reg-ends &key replace-string repl-l char-map)
  (coerce
   (loop
       with esc
       with reg
       with reg-start
       with reg-end
       for c across replace-string
       if (and esc (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
       do (setf reg (1- (read-from-string (string c))))
	  (setf reg-start (my-aref reg-starts reg))
	  (setf reg-end (my-aref reg-ends reg))
       and append (coerce (subseq target-string reg-start reg-end) 'list)
       and append (subseq char-map reg-start reg-end) into new-char-map 
       and do (setf esc nil)
       else if esc 
       collect c
       and collect nil into new-char-map
       and do (setf esc nil)
       else if (char= c #\\)
       do (setf esc t)
       else collect c
       and collect nil into new-char-map
       finally
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

(defun my-aref (x y)
  (aref x y))

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