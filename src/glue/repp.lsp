;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2007 Stephan Oepen; see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: repp.lsp
;;;      module: regular expression-based preprocessor (some rough utilities)
;;;     version: 0.0 (30-jan-03)
;;;  written by: oe, csli stanford
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
;;; : token separator (applied once string rewriting has saturated)
;;;
;;; - replace
;;;
;;; #42
;;;
;;; # grouping: name set of rules # 42; group is not executed unless called
;;;
;;; >42 group call; groups can be recursive and stop when there was no match
;;;
;;; </foo/bar file inclusion: `/foo/bar' is read at this point
;;;

(in-package :lkb)

(defparameter *repp-debug-p* t)

(defparameter *repps* nil)

(defparameter *repp-calls* nil)

(defparameter *repp-interactive* nil)

(defparameter *repp-iterations* 4711)

(defstruct repp
  id
  version
  tokenizer
  rules
  groups
  legacy)

(defmethod print-object ((object repp) stream)
  (format 
   stream 
   "#[REPP ~(~a~) (~d rules~@[ @ `~a'~])]"
   (repp-id object)
   (length (repp-rules object))
   (repp-tokenizer object)))

(defstruct fsr
  type
  source
  scanner
  target)

(defstruct token
  id form start end from to class ersatz legacy)

(defun read-repp (file &key id (repp (make-repp) reppp) (prefix ""))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (let* ((path (pathname file))
             (type (pathname-type path))
             (name (pathname-name path))
             (id (or id (intern (string-upcase name) :keyword))))
        (unless reppp (setf (repp-id repp) id))
        (format 
         t 
         "~&~aread-repp(): reading file `~a~@[.~a~]'.~%" 
         prefix name type)
        (loop
            with separator = (ppcre:create-scanner "\\t+")
            with repps = (list repp)
            for n from 1
            for line = (read-line stream nil nil)
            for length = (length line)
            for c = (unless (zerop length) (char line 0))
            when (and c (not (char= c #\;))) do
              (multiple-value-bind (start end) (ppcre:scan separator line)
                (cond
                 ((char= c #\@)
                  (let* ((version (subseq line 1 end))
                         (version (if (string= version "$Date: " :end1 7)
                                    (subseq version 7 (- (length version) 2))
                                    version)))
                    (setf (repp-version (first repps)) 
                      (string-trim '(#\Space) version))))
                 ((char= c #\<)
                  (let* ((name (subseq line 1 end))
                         (file (or (probe-file name)
                                   (probe-file (merge-pathnames name path)))))
                    (if file
                      (read-repp
                       file :repp repp :prefix (format nil "~a  " prefix))
                      (format
                       t
                       "~aread-repp(): [~d] unable to include `~a'~%"
                       prefix n name))))
                 ((char= c #\>)
                  (let* ((name (subseq line 1 end))
                         (id (or (parse-integer name :junk-allowed t)
                                 (intern (string-upcase name) :keyword)))
                         (fsr (make-fsr :type :call :source id)))
                    (push fsr (repp-rules (first repps)))))
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (repp-tokenizer (first repps)) tokenizer)))
                 ((char= c #\#)
                  (if (ppcre:scan "^#[ \\t]*$" line)
                    (if (rest repps)
                      (let ((repp (pop repps)))
                        (setf (repp-rules repp) (nreverse (repp-rules repp)))
                        (push repp (repp-groups (first repps))))
                      (format
                       t
                       "~aread-repp(): [~d] spurious group close~%"
                       prefix n))
                    (let ((id (parse-integer line :start 1 :junk-allowed t)))
                      (if (numberp id)
                        (push (make-repp :id id) repps)
                        (format
                         t
                         "~aread-repp(): [~d] invalid group identifier `~a'~%"
                         prefix n (subseq line 1))))))
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
                      (unless (or (eq type :replace) (null (rest repps)))
                        (format
                         t
                         "~aread-repp(): [~d] legacy `~a' invalid in group~%"
                         prefix n c)
                        (setf scanner nil))
                      (if scanner
                        (if (eq type :replace)
                          (push match (repp-rules (first repps)))
                          (push match (repp-legacy (first repps))))
                        (format
                         t
                         "~aread-repp(): [~d] invalid pattern `~a'~%"
                         prefix n source)))
                    (format
                     t
                     "~aread-repp(): [~d] invalid `~a'~%"
                     prefix n line)))
                 (t
                  (format
                   t
                   "~aread-repp(): [~d] ignoring unknown rule type `~a'~%"
                   prefix n c))))
            when (null line) do
              ;;
              ;; _fix_me_
              ;; check for left-over groups, i.e. ones not closed off by `#'
              ;;
              (unless reppp
                (setf (repp-rules repp)
                  (nreverse (repp-rules repp)))
                (setf (repp-legacy repp)
                  (nreverse (repp-legacy repp))))
              (unless reppp
              #+:debug
                (format t "~a~%" repp)
                (push repp *repps*))
              (return repp))))))

(defun repp (string &key (repp (first *repps*))
                         (calls *repp-calls*)
                         (verbose *repp-debug-p*)
                         (format :pet))
  
  (when (keywordp repp)
    (setf repp
      (loop
          for foo in *repps*
          when (eq (repp-id foo) repp) return foo)))
  (when (null repp)
    (return-from repp (and (eq format :string) string)))
  
  (let ((length 0)
        tokens)
    (loop
        with rules = (copy-list (repp-rules repp))
        with repps = (list repp)
        with matchp with counts
        for rule = (pop rules)
        while rule
        #+:debug do
          #+:debug
          (format
           t
           "~&[~@[+~]~a ~a] ~a~%  :~(~a~) ~a~%~%"
           matchp n (length rules) (first repps)
           (fsr-type rule) (fsr-source rule))
        when (eq (fsr-type rule) :call) do
          (let* ((id (fsr-source rule))
                 (repp
                  (loop
                      for repp 
                      in (if (numberp id) (repp-groups (first repps)) *repps*)
                      when (eql id (repp-id repp)) return repp)))
            (unless repp
              (error "repp(): invalid call target `~(~a~)'" id))
            (when (or (numberp id)
                      (and calls (symbolp calls))
                      (smember id calls))
              (push (make-fsr :type :pop) rules)
              (setf rules (append (repp-rules repp) rules))
              (push repp repps)
              (push 0 counts)
              (setf matchp nil)
              (when (eq verbose :trace) (format t "~&>~(~a~)~%~%" id))))
        else when (eq (fsr-type rule) :pop) do
          (cond
           ((and matchp
                 (numberp (first counts))
                 (or (null *repp-iterations*)
                     (< (first counts) *repp-iterations*)))
            (push (make-fsr :type :pop) rules)
            (setf rules (append (repp-rules (first repps)) rules))
            (incf (first counts))
            (when (eq verbose :trace)
              (format t ">~(~a~)~%~%" (repp-id (first repps)))))
           (t
            (when (eq verbose :trace)
              (format t "<~(~a~)~%~%" (repp-id (first repps))))
            (pop repps)
            (pop counts)))
          (setf matchp nil)
        else do
          (let* ((scanner (fsr-scanner rule))
                 (target (fsr-target rule))
                 (match (ppcre:regex-replace-all scanner string target)))
            (unless (string= string match)
              (setf matchp t)
              (when (eq verbose :trace)
                (format
                 t
                 "~&|~a|~%  |~a|~%  |~a|~%~%"
                 (fsr-source rule) string match))
              (setf string match))))
    (setf tokens
      (loop
          with result
          with i = 0
          with id = 41
          for form in (ppcre:split (repp-tokenizer repp) string)
          unless (string= form "")
          do
            (let* ((id (incf id))
                   (start i)
                   (end (incf i))
                   (token
                    (make-token :id id :form form :start start :end end)))
              (incf length)
              (push token result)
              (when verbose
                (format t "  (~a) [~a:~a] |~a|~%" id start end form)))
          finally (return (nreverse result))))
    
    ;;
    ;; _fix_me_
    ;; for a limited transition period, still apply the old (FSPP) legacy,
    ;; token-level rules.  once the HandOn release is done, ditch this code.
    ;;                                                         (17-oct-08; oe)
    (loop
        for token in tokens
        for form = (token-form token)
        do
          (loop
              for rule in (repp-legacy repp)
              for type = (fsr-type rule)
              for scanner = (fsr-scanner rule)
              for target = (fsr-target rule)
              for match = (ppcre:regex-replace scanner form target)
              when (and (eq verbose :trace) (not (string= form match))) do
                (format
                 t
                 "~&|~a|~%  |~a|~%  |~a|~%~%"
                 (fsr-source rule) token match)
              unless (string= form match) do
                ;;
                ;; _fix_me_
                ;; regex-replace() always returns a fresh string, even if the
                ;; pattern did _not_ match; to make this more efficient, it
                ;; seems, we would have to use scan() and then glue together
                ;; the result from parsing .target. and filling in register
                ;; matches as needed :-{.                     (31-jan-03; oe)
                ;;
                ;;
                ;; _fix_me_
                ;; to do ersatzes properly, they should no longer be available
                ;; to subsequent rule processing: presumably, we should build
                ;; an ersatzing table and use non-string tokens (indices into
                ;; the table) instead.                         (1-feb-03; oe)
                ;;
                (case type
                  (:substitute
                   (setf form (setf (token-form token) match)))
                  (:ersatz
                   (setf form (setf (token-ersatz token) match)))
                  (:augment
                   (push (cons type match) (token-legacy token))))))
                          
    #+:null
    (repp-align string tokens)
    (case format
      ((:string :lkb)
       (let ((forms (loop
                        for token in tokens
                        collect (or (token-ersatz token) (token-form token)))))
         (if (eq format :string)
           (values (format nil "~{~a~^ ~}" forms) (length forms))
           (values forms length))))
      (:pet
       (loop
           for token in tokens
           for id = (token-id token)
           for form = (escape-string (token-form token))
           for start = (token-start token)
           for end = (token-end token)
           for yy = (format 
                     nil 
                     "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")"
                     id start end (or (token-ersatz token) form) form)
           collect yy into yys
           do
             (loop
                 for legacy in (token-legacy token)
                 for yy = (format 
                           nil 
                           "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")"
                           id start end (rest legacy) form)
                 do (push yy yys))
           finally 
             (return
               (values (format nil "~{~a~^ ~}" yys) length))))
      (:list
       (let (result)
         (loop 
             for token in tokens
             for id = (token-id token)
             for form = (token-form token)
             for start = (token-start token)
             for end = (token-end token)
             for list = (list id start end form form)
             do
               (push list result)
               (loop
                   for legacy in (token-legacy token)
                   for list = (list id start end (rest legacy) form)
                   do (push list result)))
         (values (nreverse result) length)))
      (:raw
       (values tokens length)))))

#+:null
(defun repp-align (string tokens)
  (loop
      with tokens = (copy-list tokens)
      with token = (pop tokens)
      with form = (token-form token)
      for i from 0
      for c = (char string i)
      unless (char= c #\space) do
        (let ((j (position c form))))))

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

(defun clear-repp ()
  (setf *repps* nil))

(defun repp-for-pet (string &optional tagger &rest arguments)
  (let* ((*repp-calls* (or (getf arguments :calls) *repp-calls*))
         (repp (or (getf arguments :repp) (first *repps*)))
         (format (or (getf arguments :format) :pet))
         (verbose (getf arguments :verbose))
         (result
          (if (or (keywordp tagger)
                  (and (consp tagger) (keywordp (first tagger))))
            (multiple-value-bind (tokens length)
                (case (if (keywordp tagger) tagger (first tagger))
                  (:tnt
                   (apply 
                    #'tnt
                    (repp string :repp repp :format :list :verbose verbose)
                    (unless (keywordp tagger) (rest tagger)))))
              (loop
                  for (id start end form surface . tags) in tokens
                  for token = (format 
                               nil 
                               "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\",~
                                ~{ ~s ~,4f~})" 
                               id start end
                               (escape-string form) (escape-string surface)
                               tags)
                  collect token into tokens
                  finally 
                    (return (values (format nil "~{~a~^ ~}" tokens) length))))
            (repp string :repp repp :format format :verbose verbose))))
    (let ((stream (getf arguments :stream)))
      (cond
       ((or (streamp stream) (eq stream t))
        (format stream "~a~%" result))
       ((stringp stream)
        (with-open-file (stream stream
                         :direction :output :if-exists :supersede)
          (format stream "~a~%" result)))
       (t                 
        result)))))

(defparameter *taggers*
  #+:logon
  '((:tnt "tnt -z100 ${LOGONROOT}/coli/tnt/models/wsj -" 2))
  #-:logon
  '((:tnt "tnt -z100 /user/oe/src/tnt/models/wsj -")))

(defun tnt (tokens &optional run &key (n 1 np))
  (unless (or run np)
    (loop
        for run in *taggers*
        when (eq (first run) :tnt)
        do (setf n (third run))))
  (labels ((commentp (string)
             (and (>= (length string) 2)
                  (characterp (char string 0)) (char= (char string 0) #\%)
                  (characterp (char string 1)) (char= (char string 1) #\%))))

    (let* ((run (or run 
		    (loop
			for run in *taggers*
                        when (eq (first run) :tnt)
			return (second run))
		    "tnt -z100 /user/oe/src/tnt/models/wsj -"))
	   (command (format nil "exec ~a" run *taggers*))
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

