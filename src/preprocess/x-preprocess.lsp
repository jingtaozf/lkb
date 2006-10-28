;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2006
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;;   see `licence.txt' for conditions.


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

(in-package :preprocessor)

;; some wrappers

(defun x-read-preprocessor (&rest rest) (apply #'read-preprocessor rest))
(defun x-preprocess (&rest rest) (apply #'preprocess rest))
(defun x-clear-preprocessor (&rest rest) (apply #'clear-preprocessor rest))

;; end of wrappers

(defvar *min-regex-char-code-limit* 256)

(defvar *local-to-global-point-mapping* #'identity)

(defvar *preprocess-p* nil)
(defvar *preprocessor-debug-p* t)
(defvar *preprocessor* nil)
(defvar *x-addressing* nil)

(defvar *span* nil)
(defvar *text* nil)

(defstruct x-fspp
  version
  (tokenizer 
   (let ((ppcre::*regex-char-code-limit* 256))
     (ppcre:create-scanner "[ \\t]+" :single-line-mode t)))
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

(defun file-max-char-code (filename)
  (with-open-file (stream filename :direction :input)
    (loop
	with c
	while (setf c (read-char stream nil nil))
	maximize (char-code c))))

(defun read-preprocessor (file)
  ;; ppcre::*regex-char-code-limit* will be set smallest possible value
  (let ((ppcre::*regex-char-code-limit* *min-regex-char-code-limit*))
    (read-preprocessor-aux file)))

(defun read-preprocessor-aux (file &key (x-fspp (make-x-fspp) x-fsppp))
  (when (probe-file file)
    ;; set ppcre::*regex-char-code-limit* to smallest possible value
    (setf ppcre::*regex-char-code-limit*
      (max ppcre::*regex-char-code-limit*
	   (file-max-char-code file)))
    (with-open-file (stream file :direction :input)
      (let* ((path (pathname file))
             (type (pathname-type path)))
        (format 
         t 
         "~&Reading preprocessor rules `~a~@[.~a~]'~%" 
         (pathname-name path) type)
        (loop
            with separator = (ppcre:create-scanner "\\t+" :single-line-mode t)
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
                      (read-preprocessor-aux file :x-fspp x-fspp)
                        (format
                         t
                         "read-preprocessor(): [~d] unable to include `~a'~%"
                         n name))))
		 ;; : tokenizer
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (x-fspp-tokenizer x-fspp) tokenizer)))
		 ;; ! replace
		 ;; - substitute
		 ;; + augment
		 ;; ^ ersatz
		 ;; > ersatz
                 ((member c '(#\! #\- #\+ #\^ #\>) :test #'char=)
                  (if (and start end)
                    (let* ((type (case c
                                   (#\! :replace)
                                   (#\- :substitute)
                                   (#\+ :augment)
                                   (#\^ :ersatz)
                                   (#\> :ersatz-augment)
				   ))
                           (source (subseq line 1 start))
                           (target (subseq line end))
                           (scanner
                            (ignore-errors
                             (ppcre:create-scanner 
                              (if (eq type :replace)
                                source
                                (format nil "^~a$" source))
			      :single-line-mode t)))
                           (match (make-x-fsr :type type :source source
                                            :scanner scanner :target target)))
                      (if scanner
                        (if (eq type :replace)
                          (push match (x-fspp-global x-fspp))
                          (push match (x-fspp-local x-fspp)))
                        (format
                         t
                         "read-preprocessor(): [~d] invalid pattern `~a'~%"
                         n source)))
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
              (unless x-fsppp
                (setf (x-fspp-global x-fspp)
                  (nreverse (x-fspp-global x-fspp)))
                (setf (x-fspp-local x-fspp)
                  (nreverse (x-fspp-local x-fspp))))
              (unless x-fsppp (format t "~a~%" x-fspp))
              (return (if x-fsppp x-fspp (setf *preprocessor* x-fspp))))))))

(defun preprocess (string &key (preprocessor *preprocessor*) 
                               (globalp t) (tokenp t)
                               (verbose *preprocessor-debug-p*)
				 (format :list))
  (unless *preprocessor*
    (error "*preprocessor* not loaded"))
  (let* ((x (make-preprocessed-x string))
	 (*text* string)
	 (*span* (char-map-simple-range (char-map x))) 
	)
    ;; if no preprocessor defined...
    (when (null preprocessor)
      (return-from preprocess (and (eq format :lkb) x)))
    ;; process text globally
    (when globalp
      (preprocess-global x (x-fspp-global preprocessor)
			   :verbose verbose))
    ;; process tokens
    (multiple-value-bind (result length)
	(preprocess-tokens (x-split (x-fspp-tokenizer preprocessor) x) 
			   (x-fspp-local preprocessor)
			   :tokenp tokenp
			   :verbose verbose)
      ;; get output in desired format
      (x-format-preprocessed-output
       (nreverse (x-tokens-to-result (nreverse result) :format format))
       length format))))

(defun preprocess-global (x &optional (global (slot-value *preprocessor* 'global)) &key verbose)
  (with-slots (text char-map) x
    (loop
	for rule in global
	for scanner = (x-fsr-scanner rule)
	for target = (x-fsr-target rule)
	for old-x = (clone-preprocessed-x x) ;;effic?
	do
	  (setf x (x-regex-replace-all scanner x target))
	when (and (eq verbose :trace) 
		  (not (preprocessed-x= old-x x)))
	do
	  (format
	   t
	   "~&GLOBAL |~a| -> |~a| ~&mapped~&~a~&to~&~a~%~%"
	   (x-fsr-source rule) (x-fsr-target rule)
	   old-x x)
	finally
	  (return x))))

(defun clone-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (copy-seq (text x))
    :char-map (copy-tree (char-map x))))

(defun copy-preprocessed-x (x)
  (make-instance 'preprocessed-x 
    :text (text x)
    :char-map (char-map x)))

(defun preprocess-tokens (x-l 
			    &optional (local (slot-value *preprocessor* 'local)) 
			    &key tokenp verbose)
  (loop
      with result
      with x-token
      with length = 0
      for x in x-l
      unless (string= (text x) "") do
	(incf length)
	(setf x-token x)
	(loop
	    with extra = nil
	    for rule in (when tokenp local)
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
	    when (and (eq verbose :trace) 
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

(defun x-tokens-to-result (tokens &key verbose 
				       format) ;;get rid of this
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
	(unless (or (eq format :chared)
		    (eq format :lkb)) ;; can't handle lattice
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
	  (setf i end))
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

(defun x-format-preprocessed-output (result length &optional (format :list))
  (cond
   ((or (eq format :yy)
	(eq format :pet)) ;; deprecate this...?
    (loop
	for (id start end form surface2) in result
	for surface = (or surface2 form)
	for token = (format 
		     nil 
		     "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
		     id start end 
		     (x-escape-string (text form)) (x-escape-string (text surface)))
	collect token into tokens
	finally 
	  (return
	    (values (format nil "~{~a~^ ~}" tokens) length))))
   #+:lkb
   ((eq format :pic)
    (error "not implemented"))
   ((or (eq format :saf) (eq format :smaf))
    (p-tokens-to-smaf result :format format))
   ((eq format :list)
    (values result length))
   (t
    (error "unhandled format argument: ~a" format))))

(defun p-tokens-to-smaf (p-tokens &key (format :smaf))
    (setf *x-addressing* :|char|)
    (let ((strm (make-string-output-stream)))
      (format strm "~a" 
	      (saf-header :doctype format :addressing *x-addressing* 
			  :document 
			  #+:lkb (eval (intern "*SAF-DOCUMENT*" :smaf)) 
			  #-:lkb nil))
      (format strm "<~a init='v~a' final='v~a'~a>"
	      (if (member format '(:saf))
		  "fsm"
		"lattice")
	      (loop for tok in p-tokens
		  minimize (second tok))
	      (loop for tok in p-tokens
		  maximize (third tok))
	      (if (eq :smaf format)
		  (format nil " cfrom='~a' cto='~a'"
			  (or (funcall *local-to-global-point-mapping* (point2str (car *span*))) "")
			  (or (funcall *local-to-global-point-mapping* (point2str (cdr *span*))) ""))
		"")
	      )
      (if (or (eq :saf format))
	  (p-tokens-to-xml-states p-tokens strm))
      (mapcar #'(lambda (x) 
		  (format strm "~a"
			  (p-token-to-smaf-token x :doctype format)))
	      p-tokens)
      (format strm "~a" 
	      (if (member format '(:saf))
		  "</fsm>"
		"</lattice>"))
      (format strm "</~a>" (string-downcase (string format)))
      (get-output-stream-string strm)))
    
(defun p-tokens-to-xml-states (p-tokens strm)
  (loop
      with state-ints
      for p-token in p-tokens
      do 
	(pushnew (second p-token) state-ints)
	(pushnew (third p-token) state-ints)
      finally
	(loop 
	    for i in (sort state-ints #'<)
	    do
	      (format strm "<state id='v~a'/>" i))))

(defun point2str (x)
  (if x (format nil "~a" x)))

;(1 0 1 |EmailErsatz|:(0 . 9) |bmw@c.com|:(0 . 9)) 
;(2 1 2 |smiles|:(10 . 16) NIL)
(defun p-token-to-smaf-token (p-token &key doctype)
  (let* ((id (nth 0 p-token))
	 (source (nth 1 p-token))
	 (target (nth 2 p-token))
	 (form (nth 3 p-token))
	 (surface (nth 4 p-token))
	 (range (char-map-simple-range (char-map form)))
	 (from (or (funcall *local-to-global-point-mapping* (point2str (car range))) 
		   ""))
	 (to (or (funcall *local-to-global-point-mapping* (point2str (cdr range))) 
		 ""))
	 elt attr-from attr-to)
;    (declare (ignore surface))
    (case doctype
      (:smaf
       (setf elt "edge")
       (setf attr-from "cfrom")
       (setf attr-to "cto"))
      (:saf
       (setf elt "annot")
       (setf attr-from "from")
       (setf attr-to "to"))
      (t
       (error "unexected doctype")))
    (if surface
	(format nil "<~a type='ersatz' id='t~a' ~a='~a' ~a='~a' source='v~a' target='v~a'><slot name='name'>~a</slot><slot name='surface'>~a</slot></~a>"
	    elt
	    (xml-str id)
	    attr-from (xml-str from)
	    attr-to (xml-str to)
	    (xml-str source)
	    (xml-str target)
	    (xml-str (text form))
	    (xml-str (text surface))
	    elt
	    )
      (format nil "<~a type='token' id='t~a' ~a='~a' ~a='~a' source='v~a' target='v~a'>~a</~a>"
	      elt
	      (xml-str id)
	      attr-from (xml-str from)
	      attr-to (xml-str to)
	      (xml-str source)
	      (xml-str target)
	      (xml-str (text form))
	      elt
	      ))))
  
(defun xml-str (x)
  (xml-escape (2-str x)))

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
  
; obsolete
;;;(42 0 1 |The|:(0 . 3) |The|:(0 . 3))
;;;-> #S(CHARED-WORD :WORD "The" :CFROM 0 :CTO 3)
;#+:lkb
;(defun p-token-to-chared-word (p-token)
;  (let* ((x (fourth p-token))
;	 (r (char-map-simple-range (char-map x))))
;    (funcall (intern "MAKE-CHARED-WORD" :lkb) 
;     :word (text x)
;     :cfrom (car r)
;     :cto (cdr r))))

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

;; escape string for use as XML text
(defun xml-escape (str)
  (coerce 
   (loop
       for c across str
       if (char= #\" c) append '(#\& #\q #\u #\o #\t #\;)
       else if (char= #\' c) append '(#\& #\a #\p #\o #\s #\;)
       else if (char= #\& c) append '(#\& #\a #\m #\p #\;)
       else if (char= #\< c) append '(#\& #\l #\t #\;)
       else if (char= #\> c) append '(#\& #\g #\t #\;)
       else append (list c))
   'string))

;;
;; XML serialization
;;

(defun get-timestamp nil
  (multiple-value-bind
      (second minute hour date month year dummy1 dummy2 dummy3)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore dummy1 dummy2 dummy3))
    (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (UTC)"
	    hour
	    minute
	    second
	    month
	    date
	    year)))

; get rid of this ???
(defun maf-header (&key (addressing :char) document)
  (saf-header :doctype :maf :addressing addressing :document document))

(defun saf-header (&key (addressing :char) document (doctype :saf))
  (let ((doctype-str (string-downcase (string doctype))))
    (format nil
	    "<?xml version='1.0' encoding='UTF-8'?><!DOCTYPE ~a SYSTEM '~a.dtd'><~a~a~a>~a<olac:olac xmlns:olac='http://www.language-archives.org/OLAC/1.0/' xmlns='http://purl.org/dc/elements/1.1/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.language-archives.org/OLAC/1.0/ http://www.language-archives.org/OLAC/1.0/olac.xsd'><identifier>s~a</identifier><creator>~a</creator><created>~a</created></olac:olac>"
	    doctype-str
	    doctype-str
	    doctype-str
	    (if document
		(format nil " document='~a'" (xml-escape (string document)))
	    "")
	    (if (or (eq :saf doctype))
		(format nil " addressing='~a'" (xml-escape (string addressing)))
	      "")
	    (if (eq :smaf doctype)
		(format nil "<text>~a</text>" (xml-escape *text*))
	      "")
	    (gen-id)
	    "x-preprocessor 1.00"
	    (xml-escape (get-timestamp)))))

(defvar *gen-id* 0)
(defun gen-id nil
  (incf *gen-id*))
