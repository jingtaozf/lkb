(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

(defvar *rasp-use-pos-tags-p* t)

(defvar *rasp-loose-preprocessing-p* nil)

(defun rasp-read-sentences (file)
  (loop
      with file = (if (stringp file) file (namestring file))
      with stream = (ignore-errors (open file :direction :input))
      with result = nil
      with sentence = nil
      for token = (when stream (read-line stream nil nil))
      for line from 1
      when (or (null token) (char= (char token 0) #\^)) do
        (when (and sentence 
                   (or (rest sentence)
                       (not (member (first sentence)
                                    '("._." "!_!" "?_?" "(_(" ")_)""..._...")
                                    :test #'string=))))
          (push (nreverse sentence) result))
        (setf sentence nil)
      else do (push token sentence)
      when (null token) do
        (close stream)
        (return (nreverse result))))

(defun rewrite-rasp-token (token pos)
  (cond
   ((string-equal pos "nnp") "NameErsatz")
   ((member pos '("``" "''" "," "\"") :test #'string=) "")
   ((member token '("--" "...") :test #'string=) "")
   ((string-equal pos "-lrb-") "(")
   ((string-equal pos "-rrb-") ")")
   (t (if lkb::*preprocessor*
        (lkb::preprocess token :globalp nil :format :lkb :verbose nil)
        token))))

(defun extract-rasp-tokens (string)
  (with-input-from-string (stream string)
    (loop
        for token = (first (read-token stream))
        for break = (and token (position #\_ token :from-end t :test #'char=))
        for form = (and break (subseq token 0 break))
        for tag = (and break (subseq token (+ break 1)))
        while (and (stringp token) (not (string= token "")))
        unless (and form tag) do
	  (unless *rasp-loose-preprocessing-p*
	    (format
	     t
	     "extract-rasp-tokens(): ignoring invalid token `~a'.~%" token))
        else collect (list form tag))))

(defun rasp-preprocess (string 
                        &key (format :plain) (posp *ptb-use-pos-tags-p*))
  (let ((length 0)
        (result nil))
    (loop
        with tokens = (extract-rasp-tokens string)
        with i = 0
        with id = 41
        for token in tokens
        for raw = (first token)
        for pos = (second token)
        for form = (rewrite-rasp-token raw pos)
        unless (or (string-equal pos "-none-")
                   (and (eq format :raw) (string= raw ""))
                   (and (not (eq format :raw)) (string= form ""))) do
          (case format
           (:raw (push raw result))
           (:plain (push form result))
           (:pos (push pos result))
           (t
            (push (format 
                   nil 
                   "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\"~
                    ~:[~*~;, \"~a\" 1.00~])" 
                   (incf id) i (incf i) form raw posp pos)
                  result)))
          (incf length))
    (values (or 
	     (when result
	       (if (eq format :pos)
		 (nreverse result)
		 (format nil "~{~a~^ ~}" (nreverse result))))
	     (and *rasp-loose-preprocessing-p* (not (eq format :pos)) string))
            length)))

(defun rasp-preprocess-for-pet (string)
  (rasp-preprocess string :format :pet :posp t))

(defun rasp-tag (string)
  (rasp-preprocess string :format :pos))

;;;
;;; from here on, import support to read in a file of RASP parse trees and make
;;; a profile, including results, from it; optionally, run a hook on each tree,
;;; prior to storage in the profile, to pad the results (e.g. with RMRSs).
;;;
(defun read-items-from-rasp-file (file 
                                  &key (base 1)
                                       origin register difficulty 
                                       category comment 
                                       (trees-hook *tsdb-trees-hook*)
                                       (semantix-hook *tsdb-semantix-hook*)
                                       (run 0) meter)
  (declare (ignore meter))
  
  (let ((author (current-user))
        (date (current-time))
        (format "none")
        item parse result)
    (with-open-file (stream file :direction :input)
      (loop
          with i = 0
          with surface = nil
          with derivations = nil
          for form = (read stream nil nil)
          while form
          when (and (consp form) (stringp (first form))) do
            (when (and surface derivations)
              (push (pairlis '(:i-id 
                               :i-origin :i-register :i-format
                               :i-difficulty :i-category :i-input :i-wf
                               :i-length :i-comment :i-author :i-date)
                             (list (+ base i)
                                   origin register format
                                   difficulty category 
                                   (format nil "~{~a~^ ~}" surface)
                                   1 (length surface) comment author date))
                    item)
              (push (pairlis '(:run-id :parse-id :i-id 
                               :readings)
                             (list run (+ base i) (+ base i)
                                   (length derivations)))
                    parse)
              (let ((foo
                     (loop
                         for j from 0
                         for derivation in (nreverse derivations)
                         for string = (with-standard-io-syntax 
                                        (let ((*package* (find-package :tsdb)))
                                          (write-to-string derivation)))
                         for tree = (tsdb::call-hook trees-hook string)
                         for mrs = (tsdb::call-hook semantix-hook string)
                         collect (pairlis '(:parse-id :result-id 
                                            :derivation :tree :mrs)
                                          (list (+ base i) j 
                                                string tree mrs)))))
                (setf result
                  (nconc
                   (if *import-result-hook*
                     (call-hook *import-result-hook* foo)
                     foo)
                   result)))
              (incf i))
            (setf surface form)
            (setf derivations nil)
          else when (consp form) do (push form derivations)
          finally
            (when (and surface derivations)
              (push (pairlis '(:i-id 
                               :i-origin :i-register :i-format
                               :i-difficulty :i-category :i-input :i-wf
                               :i-length :i-comment :i-author :i-date)
                             (list (+ base i)
                                   origin register format
                                   difficulty category 
                                   (format nil "~{~a~^ ~}" surface)
                                   1 (length surface) comment author date))
                    item)
              (push (pairlis '(:run-id :parse-id :i-id 
                               :readings)
                             (list run (+ base i) (+ base i)
                                   (length derivations)))
                    parse)
              (let ((foo
                     (loop
                         for j from 0
                         for derivation in (nreverse derivations)
                         for string = (with-standard-io-syntax 
                                        (let ((*package* (find-package :tsdb)))
                                          (write-to-string derivation)))
                         for tree = (tsdb::call-hook trees-hook string)
                         for mrs = (tsdb::call-hook semantix-hook string)
                         collect (pairlis '(:parse-id :result-id 
                                            :derivation :tree :mrs)
                                          (list (+ base i) j 
                                                string tree mrs)))))
                (setf result
                  (nconc
                   (if *import-result-hook*
                     (call-hook *import-result-hook* foo)
                     foo)
                   result))))))
            
    (values (nreverse item) nil nil nil (nreverse parse) (nreverse result))))

         