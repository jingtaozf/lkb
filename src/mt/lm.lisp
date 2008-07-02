(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2006 Erik Velldal (erikve@ifi.uio.no)
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *lm-binary*
  (format
   nil 
   "exec ~a"
   (namestring
    (make-pathname
     :directory (pathname-directory make::bin-dir) :name "evallm"))))

(defparameter *lm-options* 
  "-include_unks -backoff_from_unk_inc -backoff_from_ccs_inc")

(defparameter *lm-oovs* nil)

(defparameter *lm-model*
  #+:logon
  (namestring (make-pathname 
               :directory (namestring
                           (dir-append 
                            (get-sources-dir "mt") '(:relative "mt")))
               :name "bnc.blm"))
  #-:logon
  nil)

(defparameter *lm-input* nil)

(defparameter *lm-pid* nil)

(defparameter *lm-output* 
  (format nil "/tmp/.lm.io.~a.~a.out" (lkb::current-user) (lkb::current-pid)))

(defparameter *lm-punctuation-characters* nil)

(defparameter *lm-measure* :logprob)

(defparameter *scrub-binary* 
  #+:logon
  (namestring
   (merge-pathnames
    (dir-append (get-sources-dir "tsdb") '(:relative "mt"))
    (make-pathname :name "scrub" :type "pl")))
  #-:logon
  nil)

(defparameter *scrub-stream* nil)

(defparameter *scrub-pid* nil)

(defparameter *tm-binary*
  (format
   nil 
   "exec ~a"
   (namestring
    (make-pathname
     :directory (pathname-directory make::bin-dir) :name "evallm"))))

(defparameter *tm-options* 
  "-include_unks -backoff_from_unk_inc -backoff_from_ccs_inc")

(defparameter *tm-model*
  #+:logon
  (namestring (make-pathname 
               :directory (namestring
                           (dir-append 
                            (get-sources-dir "mt") '(:relative "mt")))
               :name "mrs.blm"))
  #-:logon
  nil)

(defparameter *tm-input* nil)

(defparameter *tm-pid* nil)

(defparameter *tm-output* 
  (format nil "/tmp/.tm.io.~a.~a.out" (lkb::current-user) (lkb::current-pid)))

(defparameter *tm-measure* :perplexity)

(defun initialize-mt ()
  (declare (special *utool-binary*))
  (setf *lm-binary*
    (format
     nil 
     "exec ~a"
     (namestring
      (make-pathname
       :directory (pathname-directory make::bin-dir) :name "evallm"))))
  (setf *tm-binary*
    (format
     nil 
     "exec ~a"
     (namestring
      (make-pathname
       :directory (pathname-directory make::bin-dir) :name "evallm"))))
  (setf *lm-output*
    (format
     nil
     "/tmp/.lm.io.~a.~a.out"
     (lkb::current-user) (lkb::current-pid)))
  (setf *tm-output* 
    (format
     nil
     "/tmp/.tm.io.~a.~a.out"
     (lkb::current-user) (lkb::current-pid)))
  (setf *lm-input* nil)
  (setf *lm-pid* nil)
  (setf *tm-input* nil)
  (setf *tm-pid* nil)
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (unless root
      (error "initialize-mt(): unable to determine $LOGONROOT."))
    (setf *lm-model*
      (merge-pathnames
       (dir-append (get-sources-dir "tsdb") '(:relative "mt"))
       (make-pathname :name "bnc" :type "blm")))
    (setf *scrub-binary*
      (namestring
       (merge-pathnames
        (dir-append (get-sources-dir "tsdb") '(:relative "mt"))
        (make-pathname :name "scrub" :type "pl"))))
    (setf *tm-model*
      (merge-pathnames
       (dir-append (get-sources-dir "tsdb") '(:relative "mt"))
       (make-pathname :name "mrs" :type "blm")))
    (setf *utool-binary* (format nil "exec ~a/bin/utool" root)))
  (setf *scrub-stream* nil)
  (setf *scrub-pid* nil)
  (initialize-smt))

(defun lm-normalize-string (string)
  (when string
    (loop
        with result = (make-array (length string)
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        unless (member c *lm-punctuation-characters* :test #'char=)
        do (vector-push (char-downcase c) result)
        finally (return result))))
      
(let ((lock (mp:make-process-lock)))

  (defun lm-initialize (&optional (model *lm-model*))
    (mp:with-process-lock (lock)
      (with-open-file (stream *lm-output*
                       :direction :output :if-exists :supersede))
    
      (let ((command (format 
                      nil 
                      "~a -binary '~a'"
                      *lm-binary* model))
            foo)
        
        (multiple-value-setq (*lm-input* foo *lm-pid*)
          (run-process 
           command
           :wait nil
           :output *lm-output* :if-output-exists :supersede
           :input :stream
           :error-output "/dev/null" :if-error-output-exists :append))
        (setf foo foo))))
  
  (defun lm-score-strings (strings &key (measure *lm-measure*))

    (mp:with-process-lock (lock)
      (when (null *lm-input*) (lm-initialize))
      (let (files oovs-files)
        
        (loop
            for string in (scrub-strings strings)
            for i from 0
            for file = (format 
                        nil 
                        "/tmp/.lm.io.~a.~a.~a" 
                        (lkb::current-user) (lkb::current-pid) i)
            for oovs = (when *lm-oovs*
                         (format nil "~a.oovs" file))
            do
              (with-open-file (stream file
                               :direction :output :if-exists :supersede)
                (format stream "<s> ~a </s>~%" (lm-normalize-string string)))
              (format 
               *lm-input*
               "perplexity ~a -text ~a ~@[-oovs ~a~]~%"
               *lm-options* file oovs)
              (force-output *lm-input*)
              (push file files)
              (when oovs (push oovs oovs-files))
            finally
              (close *lm-input*)
              (setf *lm-input* nil)
	      #+:allegro
              (sys:os-wait nil *lm-pid*))
        (let ((results
               (with-open-file (stream *lm-output* :direction :input)
                 (loop
                     for line = (read-line stream nil nil)
                     for perplexity 
                     = (multiple-value-bind (foo bar)
                           (ppcre::scan-to-strings 
                            "^Perplexity = ([0-9.]+)" line)
                         (setf foo foo)
                         (when (simple-vector-p bar)
                           (ignore-errors
                            (read-from-string (svref bar 0) nil nil))))
		     for score 
		     = (when (numberp perplexity)
                         (loop
                             for key
                             in (if (consp measure) measure (list measure))
                             for score
                             = (case key
                                 (:perplexity perplexity)
                                 (:entropy (log perplexity 2)) 
                                 (:logprob 
                                  (* (log perplexity 2)
                                     (+ 1 (count #\space (first strings)))))
                                 (t 
                                  (error 
                                   "lm-score-strings(): ~
                                    unknown score type ~a~%"
                                   key)))
                             collect score into scores
                             finally
                               (return
                                 (if (consp measure) scores (first scores)))))
		     when score
		     collect (cons (pop strings) score)
		     while strings))))

          #+:fad
          (when oovs-files
            ;;;; sort and merge out-of-vocabulary items
            (let ((tmp1 (format nil "/tmp/lm.io.~a.~a.oovs.tmp1"
                                (lkb::current-user) (lkb::current-pid)))
                  (tmp2 (format nil "/tmp/lm.io.~a.~a.oovs.tmp2"
                                (lkb::current-user) (lkb::current-pid))))
              (run-process
               (format 
                nil
                "find /tmp -name \".lm.io.~a.~a.*.oovs\" -maxdepth 1 ~
                  -exec cat '{}' \\; | sort -ud -o ~a\; ~
                  sort -um ~a ~@[~a~] -o ~a\; mv ~a ~a" 
                (lkb::current-user) (lkb::current-pid) 
                tmp1 tmp1 (cl-fad:file-exists-p *lm-oovs*) 
                tmp2 tmp2 *lm-oovs*))))
          
          (loop 
              for file in files when (probe-file file) 
              do (delete-file file))
          (loop 
              for file in oovs-files when (probe-file file) 
              do (delete-file file))
          (lm-initialize)
          results)))))

(let ((lock (mp:make-process-lock)))

  (defun scrub-shutdown ()
    (mp:with-process-lock (lock)
      (when *scrub-stream*
        (ignore-errors
         (close *scrub-stream*)
         (setf *scrub-stream* nil)))
      (when *scrub-pid*
        (ignore-errors
         (run-process "kill -HUP ~d" *scrub-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process "kill -TERM ~d" *scrub-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process "(when kill -QUIT ~d" *scrub-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null"))
        (sys:os-wait nil *scrub-pid*)
        (setf *scrub-pid* nil))))
  
  (defun scrub-initialize ()
    (mp:with-process-lock (lock)
  
      (when *scrub-stream* (scrub-shutdown))
      
      (let (foo)
        (multiple-value-setq (*scrub-stream* foo *scrub-pid*)
          (run-process 
           (format nil "~a" *scrub-binary*)
           :wait nil
           :output :stream :input :stream
           :error-output "/dev/null" :if-error-output-exists :append))
        (setf foo foo))))

  (defun scrub-strings (strings)
    (if *scrub-binary*
      (mp:with-process-lock (lock)
        (when (null *scrub-stream*) (scrub-initialize))
        (loop
            for string in strings
            do 
              (format *scrub-stream* "~a~%" string)
              (force-output *scrub-stream*)
              collect (read-line *scrub-stream* nil nil)))
      strings)))

(let ((lock (mp:make-process-lock)))

  (defun tm-initialize (&optional (model *tm-model*))
    (mp:with-process-lock (lock)
      (with-open-file (stream *tm-output*
                       :direction :output :if-exists :supersede))
    
      (let ((command (format 
                      nil 
                      "~a -binary '~a'"
                      *tm-binary* model))
            foo)
        
        (multiple-value-setq (*tm-input* foo *tm-pid*)
          (run-process 
           command
           :wait nil
           :output *tm-output* :if-output-exists :supersede
           :input :stream
           :error-output "/dev/null" :if-error-output-exists :append
           :if-error-output-does-not-exist :create))
        (setf foo foo))))
  
  (defun tm-score-mrss (mrss &key (model *tm-model*) (measure *tm-measure*))
    (mp:with-process-lock (lock)
      (when (null *tm-input*) (tm-initialize model))
      (let (files lengths)
        
        (loop
            with mrs::*eds-include-quantifiers-p* = t
            with mrs::*eds-include-vacuous-relations-p* = t
            for mrs in mrss
            for i from 0
            for file = (format 
                        nil 
                        "/tmp/.tm.io.~a.~a.~a" 
                        (lkb::current-user) (lkb::current-pid) i)
            do
              (with-open-file (stream file
                               :direction :output :if-exists :supersede)
                (push
                 (mrs::ed-output-psoa
                  mrs :format :triples :markp t :stream stream)
                 lengths))
              (format 
               *tm-input*
               "perplexity ~a -text ~a~%"
               *tm-options* file)
              (force-output *tm-input*)
              (push file files)
            finally
              (close *tm-input*)
              (setf *tm-input* nil)
	      #+:allegro
              (sys:os-wait nil *tm-pid*))
        (setf lengths (nreverse lengths))
        (let ((results
               (with-open-file (stream *tm-output* :direction :input)
                 (loop
                     with mrss = (copy-list mrss)
                     for line = (read-line stream nil nil)
                     for perplexity 
                     = (when line
                         (multiple-value-bind (foo bar)
                             (ppcre::scan-to-strings 
                              "^Perplexity = ([0-9.]+)" line)
                           (setf foo foo)
                           (when (simple-vector-p bar)
                             (ignore-errors
                              (read-from-string (svref bar 0) nil nil)))))
		     for score 
		     = (when (numberp perplexity)
                         (case measure
                           (:perplexity perplexity)
                           (:entropy (log perplexity 2)) 
                           (:logprob
                            (* (log perplexity 2) (* 5 (pop lengths))))
                           (t 
                            (error 
                             "tm-score-strings(): unknown score type ~a~%" 
                             measure))))
                     when (null line) return nil
		     when score
		     collect (cons (pop mrss) score)
		     while mrss))))
          (loop 
              for file in files when (probe-file file) 
              do (delete-file file))
          (or results
              (loop
                  for mrs in mrss collect (cons mrs 0))))))))
