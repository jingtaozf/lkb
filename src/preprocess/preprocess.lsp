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

;;
;; (bmw) where is this code called from?
;;

(defun preprocess-for-pet (string &optional tagger)
  (if (and tagger (consp tagger) (keywordp (first tagger)))
    (multiple-value-bind (tokens length)
        (case (first tagger)
          (:tnt
           (apply 
            #'tag-tnt
            (preprocessor:preprocess string :format :list :verbose nil)
            (rest tagger))))
      (loop
          for (id start end form surface . tags) in tokens
          for token = (format 
                       nil 
                       "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\",~
                       ~{ ~s ~,4f~})" 
                       id start end 
                       (preprocessor::x-escape-string form) (preprocessor::x-escape-string surface) tags)
          collect token into tokens
          finally 
            (return (values (format nil "~{~a~^ ~}" tokens) length))))
    (preprocessor:preprocess string :format :pet :verbose nil)))

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

