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

(defparameter *repp-debug-p* t)

(defparameter *repp* nil)

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

(defun read-repp (file &key (fspp (make-fspp) fsppp) (prefix ""))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (let* ((path (pathname file))
             (type (pathname-type path)))
        (format 
         t 
         "~&~aread-repp(): reading file `~a~@[.~a~]'.~%" 
         prefix (pathname-name path) type)
        (loop
            with separator = (ppcre:create-scanner "\\t+")
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
                    (setf (fspp-version fspp) 
                      (string-trim '(#\Space) version))))
                 ((char= c #\<)
                  (let* ((name (subseq line 1 end))
                         (file (or (probe-file name)
                                   (probe-file (merge-pathnames name path)))))
                    (if file
                      (read-repp
                       file :fspp fspp :prefix (format nil "~a  " prefix))
                        (format
                         t
                         "~aread-repp(): [~d] unable to include `~a'~%"
                         prefix n name))))
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (fspp-tokenizer fspp) tokenizer)))
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
                         "~aread-repp(): [~d] invalid pattern `~a'~%"
                         prefix n source)))
                    (format
                     t
                     "~aread-repp(): [~d] invalid `~a'~%"
                     prefix n line)))
                 (t
                  (format
                   t
                   "~aread-repp(): [~d] ~
                    prefix ignoring unknown rule type `~a'~%"
                   n c))))
            when (null line) do
              (unless fsppp
                (setf (fspp-global fspp)
                  (nreverse (fspp-global fspp)))
                (setf (fspp-local fspp)
                  (nreverse (fspp-local fspp))))
              (unless fsppp (format t "~a~%" fspp))
              (return (if fsppp fspp (setf *repp* fspp))))))))

(defun repp (string &key (repp *repp*) 
                         (globalp t) (tokenp t)
                         (verbose *repp-debug-p*)
                         (format :pet))

  (when (null repp)
    (return-from repp (and (eq format :lkb) string)))
  
  (let ((tokenizer (and repp (fspp-tokenizer repp)))
        (global (and repp (fspp-global repp)))
        (local (and repp (fspp-local repp)))
        (length 0)
        result)
    (when globalp
      (loop
          for rule in global
          for scanner = (fsr-scanner rule)
          for target = (fsr-target rule)
          for match = (ppcre:regex-replace-all scanner string target)
          when (and (eq verbose :trace) (not (string= string match))) do
            (format
             t
             "~&|~a|~%  |~a|~%  |~a|~%~%"
             (fsr-source rule) string match)
          do
            (setf string match)))
    (loop
        with tokens = (ppcre:split tokenizer string)
        for token in tokens
        unless (string= token "") do
          (incf length)
          (loop
              with extra = nil
              for rule in (when tokenp local)
              for type = (fsr-type rule)
              for scanner = (fsr-scanner rule)
              for target = (fsr-target rule)
              for match = (ppcre:regex-replace scanner token target)
              when (and (eq verbose :trace) (not (string= token match))) do
                (format
                 t
                 "~&|~a|~%  |~a|~%  |~a|~%~%"
                 (fsr-source rule) token match)
              when (eq type :substitute) do
                (setf token match)
              else unless (string= token match) do
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
                (push (list type (if (eq type :ersatz) token match)) extra)
                (when (eq type :ersatz) (setf token match))
                
              finally
                (push (cons token extra) result)))
    (loop
        with tokens = (nreverse result)
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
          (return
            (case format
              (:lkb
               (loop
                   for i = -1
                   for token in (nreverse result)
                   for start = (second token)
                   for form = (fourth token)
                   unless (= start i) collect form into forms
                   finally 
                     (return (values (format nil "~{~a~^ ~}" forms)
                                     (length forms)))))
              (:pet
               (loop
                   for (id start end form surface) in (nreverse result)
                   for token = (format 
                                nil 
                                "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
                                id start end 
                                (escape-string form) (escape-string surface))
                   collect token into tokens
                   finally 
                     (return
                       (values (format nil "~{~a~^ ~}" tokens) length))))
              (:list
               (values (nreverse result) length)))))))

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
  (setf *repp* nil))

(defun repp-for-pet (string &optional tagger)
  (if (and tagger (consp tagger) (keywordp (first tagger)))
    (multiple-value-bind (tokens length)
        (case (first tagger)
          (:tnt
           (apply 
            #'tnt
            (repp string :format :list :verbose nil)
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
    (repp string :format :pet :verbose nil)))

(defparameter *taggers*
  '((:tnt "tnt -z100 /user/oe/src/tnt/models/wsj -")))

(defun tnt (tokens &optional run &key (n 1))
  (labels ((commentp (string)
             (and (>= (length string) 2)
                  (characterp (char string 0)) (char= (char string 0) #\%)
                  (characterp (char string 1)) (char= (char string 1) #\%))))

    (let* ((run (or run 
		    (loop
			for run in *taggers*
		       when (eq (first run) :tnt)
			return (first (rest run)))
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

