(in-package :mt)

(defparameter *lm-binary* 
  (format
   nil 
   "exec ~a"
   (namestring
    (make-pathname
     :directory (pathname-directory make::bin-dir) :name "evallm"))))

(defparameter *lm-options* 
  "-include_unks -backoff_from_unk_inc -backoff_from_ccs_inc")

(defparameter *lm-oovs* nil 
  "File to report out-of-vocabulary items.")

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
                         (case measure
                           (:perplexity perplexity)
                           (:entropy (log perplexity 2)) 
                           (:logprob 
                            (* (log perplexity 2)
                               (+ 1 (count #\space (first strings)))))
                           (t 
                            (error 
                             "lm-score-strings(): unknown score type ~a~%" 
                             measure))))
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

;;;pick up. check/diff the oovs from the 2,3 and 4..
