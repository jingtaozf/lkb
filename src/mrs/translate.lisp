;;; Copyright (c) 1998--2002 
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(defparameter *generator-server* nil)

(defparameter *translate-grid* nil)

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


(defun translate (&key serverp (gcp t)
                       (file (merge-pathnames
			      (lkb-tmp-dir)
			      (format
			       nil
			       ".transfer.~a.~(~a~)"
			       (current-user) (first *translate-grid*)))))
  #-:allegro
  (declare (ignore gcp))
  (when serverp 
    (when (probe-file file) (delete-file file))
    (loop until (probe-file file) do (sleep 1)))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (with-open-file (log (merge-pathnames
			    (lkb-tmp-dir)
			    (format 
			     nil 
			     "generate.debug.~a.~(~a~)" 
			     (current-user) (first *translate-grid*)))
                       :direction :output 
                       :if-exists :append :if-does-not-exist :create)
        (let* ((log (make-broadcast-stream 
                     log
		     #+:sbcl excl:*initial-terminal-io*
		     #-:sbcl (or #+:allegro excl:*initial-terminal-io* t)))
               (mrss (mrs::read-mrss-from-file file)))
          (format
           log
           "~&[~a] translate(): read ~a MRS~p as generator input.~%"
           (current-time :long :short) (length mrss) (length mrss))
          (loop
              for i from 0
              for mrs in mrss do
                (cond
                 ((mrs::psoa-p mrs)
                  (format
                   log
                   "~&[~a] translate(): ~
                     processing MRS # ~a (~a EP~p).~%"
                   (current-time :long :short) i
                   (length (mrs:psoa-liszt mrs)) (length (mrs:psoa-liszt mrs)))
                  (force-output log)
                  (multiple-value-bind (result condition)
                      (ignore-errors 
                       ;;
                       ;; work around values() return from generate-from-mrs()
                       ;;
                       (let* ((stream (make-string-output-stream))
                              (*standard-output* stream)
                              (mrs::*mrs-raw-output-p* nil)
                              (*debugging* nil)
                              (strings (generate-from-mrs mrs :signal t))
                              (output (get-output-stream-string stream)))
                         (when (and (stringp output) (not (string= output "")))
                           (format
                            log
                            "~&[~a] translate(): message `~a'.~%"
                            (current-time :long :short)
                            (normalize-string output)))
                         strings))
                    (cond
                     (condition
                      (format
                       log
                       "~&[~a] translate(): error `~a'.~%"
                       (current-time :long :short)
                       (normalize-string (format nil "~a" condition))))
                     (t
                      (format
                       log
                       "~&[~a] translate(): ~a generation result~p.~%"
                       (current-time :long :short)
                       (length result) (length result))))
                    (force-output log)
                    (when result (show-gen-result))
                    (invalidate-marks)
                    (invalidate-visit-marks)
                    (loop
                        for item in %generator-lexical-items%
                        for tdfs = (mrs::found-lex-inst-fs item)
                        when (tdfs-p tdfs) 
                        do (compress-dag (tdfs-indef tdfs) :recursivep t))
                    (loop
                        for rule in (get-matching-lex-rules nil)
                        for tdfs = (rule-full-fs rule)
                        for dag = (tdfs-indef tdfs) do
                          (compress-dag dag :recursivep t))
                    (loop
                        for rule in (get-matching-rules nil nil)
                        for tdfs = (rule-full-fs rule)
                        for dag = (tdfs-indef tdfs) do
                          (compress-dag dag :recursivep t))
                    (ignore-errors
                     (if (listp *start-symbol*)
                       (loop
                           for root in *start-symbol* 
                           for tdfs = (get-tdfs-given-id root)
                           for dag = (and tdfs (tdfs-indef tdfs))
                           when (dag-p dag) 
                           do (compress-dag dag :recursivep t))
                       (let* ((tdfs (get-tdfs-given-id *start-symbol*))
                              (dag (and tdfs (tdfs-indef tdfs))))
                         (when (dag-p dag) (compress-dag dag :recursivep t)))))
                    (sleep 1)
                    #+:allegro
                    (when gcp (excl:gc))))
                 (t
                  (format
                   log
                   "~&[~a] translate(): ignoring null or illformed MRS.~%"
                   (current-time :long :short))))
                (force-output log))))))
  (when serverp 
    (delete-file file)
    (translate :serverp serverp :file file)))

(defun start-generator-server (&optional (forkp t) (gcp t))
  (when (and *generator-server* forkp) 
    (stop-generator-server)
    (with-open-file (log (merge-pathnames
			  (lkb-tmp-dir)
			  (format 
			   nil 
			   "generate.debug.~a.~(~a~)" 
			   (current-user) (first *translate-grid*)))
                     :direction :output :if-exists :supersede)))
  ;;
  ;; tune Allegro CL gc() performance to initially tenure everything (assuming
  ;; that is the grammar, mostly) and then keep everything in newspace.
  ;;
  #+:allegro
  (when gcp
    (excl:gc :tenure)
    (excl:gc)
    (excl:gc t)
    (setf (sys:gsgc-parameter :auto-step) nil))

  (if forkp
    (with-output-to-top ()
      #-:clisp
      (setf *generator-server*
        (mp:run-function "generator server" #'start-generator-server nil gcp)))
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
      #+:sbcl
      (sb-ext:posix-getenv "USER")
      "nobody"))

#+(and :allegro-version>= (version>= 5 0))
(ff:def-foreign-call 
    (current-pid "getpid")
    (:void)
  :returning :int)
#+(and :allegro-version>= (not (version>= 5 0)))
(ff:defforeign 
    'current-pid
    :entry-point "getpid"
    :arguments nil
    :return-type :integer)
#-:allegro
(defun current-pid () (random (expt 2 15)))

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
