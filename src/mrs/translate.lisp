;;; Copyright (c) 1998--2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defparameter *generator-server* nil)

;;;
;;; transfer-based MT prototype from the Gothenburg -- Oslo train.  many things
;;; remain to be improved, including:
;;;
;;;   - an actual solution to IPC among LKB processes or a way of loading more
;;;     than one grammar into the same image;
;;;   - the MRS reader requires each relation to have a handel, even where it
;;;     is completely unspecific;
;;;   - without an input condition, a munging rule will never fire; currently,
;;;     always need to provide INPUT.RELS <! !> instead;
;;;

(defun transfer (&optional (edge (first *parse-record*))
                 &key (file (format nil "/tmp/.transfer.~a" (current-user))))
  (let* ((*package* (find-package :lkb))
         (rules (reverse *ordered-mrs-rule-list*))
         (input (or (edge-mrs edge) (mrs::extract-mrs edge)))
         (output (and input (mrs::munge-mrs-struct input rules))))
    (when output
      (with-open-file (stream file :direction :output
                       :if-exists :supersede)
        (mrs::output-mrs1 output 'mrs::simple stream))
      (mrs::browse-mrs output "Transfer Result"))))

(defun translate (&key serverp 
                       (file (format nil "/tmp/.transfer.~a" (current-user))))
  (when serverp 
    (when (probe-file file) (delete-file file))
    (loop until (probe-file file) do (sleep 1)))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (with-open-file (log (format nil "/tmp/generate.debug.~a" (current-user))
                       :direction :output 
                       :if-exists :append :if-does-not-exist :create)
        (let* ((log (make-broadcast-stream 
                     log (or #+:allegro excl:*initial-terminal-io* t)))
               (mrs (mrs::read-mrs-from-file file))
               (*bypass-equality-check* t))
          (cond
           ((mrs::psoa-p mrs)
            (format
             log
             "~&[~a] translate(): read one MRS (~a EP~p) as generator input.~%"
             (current-time :long :short)
             (length (mrs:psoa-liszt mrs)) (length (mrs:psoa-liszt mrs)))
            (force-output log)
            (multiple-value-bind (result condition)
                (ignore-errors 
                 ;;
                 ;; work around values() return from generate-from-mrs() ...
                 ;;
                 (let* ((stream (make-string-output-stream))
                        (*standard-output* stream)
                        (strings (generate-from-mrs mrs))
                        (output (get-output-stream-string stream)))
                   (when (and (stringp output) (not (string= output "")))
                     (format
                      log
                      "~&[~a] translate(): message `~a'.~%"
                      (current-time :long :short)
                      (normalize-string output)))
                   strings))
              (if condition
                (format
                 log
                 "~&[~a] translate(): error `~a'.~%"
                 (current-time :long :short)
                 (normalize-string (format nil "~a" condition)))
                (format
                 log
                 "~&[~a] translate(): ~a generation result~p.~%"
                 (current-time :long :short)
                 (length result) (length result)))
              (when result (show-gen-result))))
           (t
            (format
             log
             "~&[~a] translate(): ignoring null or illformed MRS.~%"
             (current-time :long :short))))
          (force-output log)))))
  (when serverp 
    (delete-file file)
    (translate :serverp serverp :file file)))

(defun start-generator-server (&optional (forkp t))
  (when (and *generator-server* forkp) 
    (stop-generator-server)
    (with-open-file (log (format nil "/tmp/generate.debug.~a" (current-user))
                     :direction :output :if-exists :supersede)))
  (if forkp
    (with-output-to-top ()
      #-:clisp
      (setf *generator-server*
        (mp:run-function "generator server" #'start-generator-server nil)))
    (translate :serverp t)))

(defun stop-generator-server ()
  (when *generator-server*
    (let ((process *generator-server*))
      (setf *generator-server* nil)
      #-:clisp
      (ignore-errors
       (mp:process-kill process)))))

(defun current-user ()
  (or #+(and :allegro-version>= (version>= 5 0)) 
      (sys:user-name)
      #+(and :allegro (not (and :allegro-version>= (version>= 5 0))))
      (system:getenv "USER")
      #+(and :mcl :powerpc) 
      (ccl:process-name ccl:*current-process*)
      #+:lucid 
      (lcl:environment-variable "USER")
      "nobody"))

;;; functions that allow for translation (using interlingua)
;;; and `translation'

;;; to add - specification of constraints on input and output 
;;; - e.g. formality, dialect etc

;;; eventually, the control of transfer stuff should also be here
;;; although actual transfer rules will be specific to e.g. MRS

#+:interlingua
(defun translate (sentence)
  (when (and sentence *source-language* *target-language*)
      (close-existing-chart-windows)
      (let ((existing-language *current-language*))
        (set-current-language *source-language*)
        (unwind-protect
            (progn
              (parse (split-into-words 
                      (preprocess-sentence-string 
                       (string-trim '(#\space #\tab #\newline) sentence))))
              (loop for parse-res in *parse-record*
                   do
                   (set-current-language *source-language*)
                   ;;; mrs extraction might be language specific
                   (let ((mrs (mrs::extract-mrs parse-res)))
                     (set-current-language *target-language*)
                     (multiple-value-bind
                         (strings unifs-tried unifs-failed active inactive)
                         (generate-from-mrs mrs)
                       (loop for string in strings
                            collect
                            (fix-spelling string))))))
          ;;; reset the current-language etc
          (set-current-language existing-language)))))


(defun set-current-language (language)
    (setf *current-language* language))
