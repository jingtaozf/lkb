;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: preprocess.lsp
;;;      module: input preprocessing mimicry (collection of rough utilities)
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

(in-package :lkb)

(defparameter *preprocessor* nil)

#+:null
(defparameter *punctuation-characters*
  (append
   '(#\! #\" #\& #\' #\(
     #\) #\* #\+ #\, #\- #\. #\/ #\: #\;
     #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
     #\_ #\` #\{ #\| #\} #\~)
   #+:ics
   '(#\ideographic_full_stop #\fullwidth_question_mark 
     #\horizontal_ellipsis #\fullwidth_full_stop
     #\fullwidth_exclamation_mark
     #\fullwidth_comma #\ideographic_space)))

#+:null
(defun punctuationp (thing)
  (let ((string (string thing)))
    (loop
        for c across string
        always (member c *punctuation-characters*))))

(defstruct fspp
  (tokenizer (ppcre:create-scanner "[ \\t]+"))
  global
  local)

(defstruct fsr
  type
  source
  scanner
  target)

(defun read-preprocessor (file)
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (loop
          with fspp = (make-fspp)
          with separator = (ppcre:create-scanner "\\t+")
          for n from 1
          for line = (read-line stream nil nil)
          for length = (length line)
          for c = (unless (zerop length) (char line 0))
          when (and c (not (char= c #\;))) do
            (multiple-value-bind (start end) (ppcre:scan separator line)
              (cond
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
                          (ppcre:create-scanner (if (eq type :replace)
                                                  source
                                                  (format nil "^~a$" source))))
                         (match (make-fsr :type type :source source
                                          :scanner scanner :target target)))
                    (if (eq type :replace)
                      (push match (fspp-global fspp))
                      (push match (fspp-local fspp))))
                  (format
                   t
                   "read-preprocessor(): [~d] invalid `~a'~%"
                   n line)))))
          when (null line) do
            (setf (fspp-global fspp)
              (nreverse (fspp-global fspp)))
            (setf (fspp-local fspp)
              (nreverse (fspp-local fspp)))
            (return fspp)))))

(defun preprocess (string &key (preprocessor *preprocessor*) verbose)

  (when (null preprocessor)
    (return-from preprocess string))
  
  (let ((tokenizer (and preprocessor (fspp-tokenizer preprocessor)))
        (global (and preprocessor (fspp-global preprocessor)))
        (local (and preprocessor (fspp-local preprocessor)))
        (length 0)
        result)
    (loop
        for rule in global
        for scanner = (fsr-source rule)
        for target = (fsr-target rule)
        do
          (setf string (ppcre:regex-replace-all scanner string target)))
    (loop
        with tokens = (ppcre:split tokenizer string)
        for token in tokens
        unless (string= token "") do
          (incf length)
          (loop
              with extra = nil
              for rule in local
              for type = (fsr-type rule)
              for scanner = (fsr-scanner rule)
              for target = (fsr-target rule)
              for match = (ppcre:regex-replace scanner token target)
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
        for start = i
        for end = (incf i)
        when verbose do
          (format t "  (~a) [~a:~a] `~a'" (incf id) start end form)
          (loop
              for foo in extra
              for type = (case (first foo)
                           (:substitute #\-)
                           (:augment #\+)
                           (:ersatz #\^))
              for form = (second foo)
              for original = (third foo)
              do (format t " {~c `~a'~@[ `~a'~]}" type form original)
              finally (format t "~%"))
        do
          (push (format 
                 nil 
                 "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
                 (incf id) start end form form)
                result)
          (loop
              for (type form) in extra
              unless (eq type :ersatz) do 
                (push (format 
                       nil 
                       "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\")" 
                       (incf id) start end form form)
                      result))
        finally 
          (return
            (values (format nil "~{~a~^ ~}" (nreverse result)) length)))))

