(in-package :mt)

(defparameter *lm-binary* 
  (format
   nil 
   "exec ~a"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "evallm"))))

(defparameter *lm-options* 
   "-include_unks -backoff_from_unk_inc -backoff_from_ccs_inc")

(defparameter *lm-model*
  (namestring (make-pathname 
               :directory (namestring
                           (dir-append 
                            (get-sources-dir "mt") '(:relative "mt")))
               :name "bnc3.blm")))

(defparameter *lm-input* nil)

(defparameter *lm-pid* nil)

(defparameter *lm-output* 
  (format nil "/tmp/.lm.io.~a.out" (lkb::current-user)))


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


  (defun lm-score-strings (strings)
    (mp:with-process-lock (lock)
      (when (null *lm-input*) (lm-initialize))
      (let (files)
        (loop
            for string in strings
            for i from 0
            for file = (format nil "/tmp/.lm.io.~a.~a" (lkb::current-user) i)
            do
              (with-open-file (stream file
                               :direction :output :if-exists :supersede)
                (format stream "<s> ~a <\\s>~%" string))
              (format 
               *lm-input*
               "perplexity ~a -text ~a~%"
               *lm-options* file)
              (force-output *lm-input*)
              (push file files)
            finally
              (close *lm-input*)
              (setf *lm-input* nil)
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
                     when (numberp perplexity)
                     collect (cons (pop strings) perplexity)
                     while strings))))
          (loop 
              for file in files when (probe-file file) 
              do (delete-file file))
          (lm-initialize)
          results)))))
  