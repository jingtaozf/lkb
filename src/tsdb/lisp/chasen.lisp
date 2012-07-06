(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 2000 -- 2008 Stephan Oepen (oe@ifi.uio.no)
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

(defparameter *chasen-application* 
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (if root
      (format nil "~a/bin/chasen" root)
      "chasen"))
  #-:logon
  "chasen")

(defparameter *chasen-debug-p* t)

(defparameter *chasen-readings* nil)

(defun chasen-preprocess (string
                          &key (verbose *chasen-debug-p*) posp)
  
  (let* ((string (string-trim '(#\space #\tab) string))
         #+:logon
         (logon (system:getenv "LOGONROOT"))
         ;;
         ;; in the self-contained LOGON philosophy, we assume people want to
         ;; use the ChaSen installation included in the tree (using UTF-8).
         ;;
         (command (format 
                   nil 
                   "~a~@[ -r '~a'~] -i ~a -F ~
                    '(\"%m\" \"%M\" \"%P-:%Tn-%Fn\" \"%y\")\\n'" 
                   *chasen-application*
                   #+:logon (and logon (format nil "~a/.chasenrc" logon))
                   #-:logon nil
                   #+:logon "w" #-:logon "e")))
    (setf *chasen-readings* nil)
    (multiple-value-bind (stream foo pid)
        (run-process
         command :wait nil
         :output :stream :if-output-exists :append 
         :input :stream :error-output nil)
      (declare (ignore foo #-:allegro pid))
      ;;
      ;; while we assert ChaSen to operate in EUC-JP mode, enforce the encoding
      ;; on the stream talking to the sub-process.  in the LOGON universe, on
      ;; the other hand, we control which dictionaries are in use, so there we
      ;; opt for modern UTF-8.
      ;;
      #+(and :allegro-version>= (version>= 6 0))
      (setf (stream-external-format stream)
        (excl:find-external-format #+:logon :utf-8 #-:logon :euc))
    
      (format stream "~a~%" string)
      (let* ((analyses (loop
                           for form = (read stream nil :eof)
                           until (or (eq form :eof) 
                                     (and (symbolp form)
                                          (eq (intern form :keyword) :eos)))
                           collect form))
	     (length 0)
             full)
        (close stream)
        #+:allegro 
        (loop for i from 0 while (< i 500) until (sys:os-wait nil pid))
        (loop
            initially (when verbose (format t "~&~%ChaSen output:~%~%"))
            with i = 0
            with id = 0
            for analysis in analyses
            for form = (first analysis)
            for pos = (third analysis)
            when verbose do
              (format 
               t 
               "  form: `~a'; stem: `~a'; analysis: `~a' ; reading : `~a'~%"
               form (second analysis) pos (fourth analysis))
            #+:lkb unless #+:lkb (lkb::punctuationp form) do
	      (incf length)
              (push
               (format 
                nil 
                "(~d, ~d, ~d, 1, \"~a\" \"~a\", 0, \"null\", \"~a\" 1.0)" 
                (incf id)
                i (incf i) form (second analysis) pos)
               full)
	      (push (fourth analysis) *chasen-readings*)
            finally
	      (setf *chasen-readings* (reverse *chasen-readings*))
	      (when verbose (format t "~%")))
        (values
         (if posp
           (format nil "~{~a~^ ~}" (reverse full))
           (#+:lkb lkb::punctuation-normalize-string #-:lkb string
            (format 
             nil 
             "~{~a~^ ~}" 
             (loop for analysis in analyses collect (first analysis)))))
         length)))))

;;;
;;; hook for [incr tsdb()] to call when preprocessing input (going to the PET
;;; parser or when counting `words' while importing test items from a file).
;;;
(defun chasen-for-pet (input &optional tagger)
  (declare (ignore tagger))
  (chasen-preprocess input :verbose nil :posp t))
