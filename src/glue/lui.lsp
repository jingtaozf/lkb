;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: lui.lsp
;;;      module: Linguistic User Interface protocol
;;;     version: 0.0 (1-jun-03)
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

(defparameter *lui-application*
  (format
   nil 
   "exec ~a -p"
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "yzlui"))))

(defparameter *lui-hidden-features*
  '(ARG ARG0 ARG1 ARG2 ARG3 ARG4
    MARG L-INDEX R-INDEX L-HNDL R-HNDL L-HANDEL R-HANDEL 
    MAIN SUBORD ROLE HINST NHINST))

(defparameter *lui-debug-p* nil)

(defparameter %lui-stream% nil)

(defparameter %lui-pid% nil)

(defparameter %lui-process% nil)

(defparameter %lui-pending-events% nil)

(defparameter %lui-eoc% (format nil " ~a" #\page))

(defun lui-initialize (&optional runtimep)
  (lui-shutdown)
  (when runtimep
    (setf *lui-application*
      (format
       nil 
       "exec ~a -p"
       (namestring 
        (make-pathname 
         :directory (pathname-directory make::bin-dir) :name "yzlui")))))
  (let (foo)
    (multiple-value-setq (%lui-stream% foo %lui-pid%)
      #-:clisp
      (run-process *lui-application*
                   :wait nil
                   :output :stream :input :stream 
                   #-(or :sbcl :openmcl) :error-output 
		   #-(or :sbcl :openmcl) nil)
      #+:clisp
      (ext:make-pipe-io-stream *lui-application* :buffered nil))
                    
    (when foo (setf foo foo))
    #+:allegro
    (setf (stream-external-format %lui-stream%) 
      (excl:find-external-format :utf-8))
    (format
     %lui-stream%
     "parameter list-type ~a~a~%~
      parameter non-empty-list-type ~a~a~%~
      parameter empty-list-type ~a~a~%~
      parameter list-head ~a~a~%~
      parameter list-tail ~a~a~%~
      parameter type-font #F[Helvetica ~a roman blue]~a~%~
      parameter feature-font #F[Helvetica ~a roman black]~a~%~
      parameter path-font #F[Helvetica ~a roman black]~a~%~
      parameter big-tree-font #F[Helvetica ~a roman black]~a~%~
      parameter small-tree-font #F[Helvetica ~a roman black]~a~%~
      status ready~a~%"
     *list-type* %lui-eoc%
     *non-empty-list-type* %lui-eoc%
     *empty-list-type* %lui-eoc% 
     (first *list-head*) %lui-eoc%
     (first *list-tail*) %lui-eoc%
     *fs-type-font-size* %lui-eoc%
     *fs-type-font-size* %lui-eoc%
     *parse-tree-font-size* %lui-eoc%
     *parse-tree-font-size* %lui-eoc%
     *summary-tree-font-size* %lui-eoc%
     %lui-eoc%)
    (force-output %lui-stream%)
    #-:clisp
    (setf %lui-process%
      #+:sbcl 
      (sb-thread:make-thread 
       #'(lambda () (lsp-loop nil %lui-stream%)))
      #-:sbcl
      (mp:run-function '(:name "LUI") #'lsp-loop nil %lui-stream%))
    #+:clisp
    (lsp-loop nil %lui-stream%)
    #+:clisp
    (lui-shutdown)))

(defun lui-shutdown ()

  (when %lui-stream%
    (ignore-errors
     (format %lui-stream% "~%~%quit~a" %lui-eoc%)
     (force-output %lui-stream%))
    (sleep 2)
    (ignore-errors (close %lui-stream%))
    (setf %lui-stream% nil))
  #-:clisp
  (when %lui-pid%
    (ignore-errors
     (run-process "kill -HUP ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -TERM ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null")
     (run-process "kill -QUIT ~d" %lui-pid% 
                  :wait t :output "/dev/null" 
                  #-:clisp :error-output #-:clisp "/dev/null"))
    #+:allegro
    (sys:os-wait nil %lui-pid%)
    (setf %lui-pid% nil))
  #-:clisp
  (when %lui-process%
    (let ((process %lui-process%))
      (setf %lui-process% nil)
      (ignore-errors
       #+:sbcl (sb-thread:terminate-thread process)
       #-:sbcl (mp:process-kill process)))))

(defun send-to-lui (string &key (wait nil) recursive)
  (unless recursive
    (when *lui-debug-p*
      (format t "~&send-to-lui(): [send] `~a'.~%" string))
    (format %lui-stream% "~@[wait ~*~]~a~a" wait string %lui-eoc%)
    (force-output %lui-stream%))
  (unwind-protect 
      (when (or wait recursive)
        (let ((*package* (find-package :lkb))
              (form (when (streamp %lui-stream%) 
                      (lsp-parse-command nil (lsp-read nil %lui-stream%)))))
          (when *lui-debug-p*
            (format t "~&send-to-lui(): [return] `~s'.~%" form))
          (cond
           ((eq (first form) 'event)
            (when *lui-debug-p*
              (format
               t 
               "~&send-to-lui(): queueing intervening event `~s'.~%" 
               form))
            (setf %lui-pending-events% 
              (append %lui-pending-events% (list form)))
            (send-to-lui nil :recursive t))
           (t
            form))))))

(defun process-pending-events ()
  #+:debug
  (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
    (mp:process-add-arrest-reason %lui-process% :pending-events))
  (loop
      while (and (streamp %lui-stream%) %lui-pending-events%)
      do (lsp-process-event nil (pop %lui-pending-events%) %lui-stream%))
  #+:debug
  (when (and %lui-process% (not (eq mp:*current-process* %lui-process%)))
    (mp:process-revoke-arrest-reason %lui-process% :pending-events)))

(defun lui-status (string)
  (format %lui-stream% "message ~s~a~%" string %lui-eoc%))
  
(defun lui-show-parses (edges &optional (input *sentence*) 
                                        (chart (copy-array *chart*))
                                        (morphs (copy-array *morphs*)))
  (loop
      with stream = (make-string-output-stream)
      initially
        (format 
         stream 
         "parameter path-font #F[Helvetica ~a roman black]~a~%~
          parameter big-tree-font #F[Helvetica ~a roman black]~a~%~
          parameter small-tree-font #F[Helvetica ~a roman black]~a~%~
          group ~d ~s~a~%"
         *parse-tree-font-size* %lui-eoc%
         *parse-tree-font-size* %lui-eoc%
         *summary-tree-font-size* %lui-eoc%
         (length edges) 
         (if input
           (format
            nil
            "`~a' (~a Tree~p)"
            input (length edges) (length edges))
           "Processing Result(s)")
         %lui-eoc%)
      for i from 1
      for title = (format nil "`~a' Parse Tree # ~d" input i)
      for edge in edges 
      for top = (make-new-parse-tree edge 1)
      for tdfs = (get top 'edge-fs)
      for lspb = (make-lspb
                  :input input :morphs morphs :chart chart
                  :edge edge :dag tdfs)
      for id = (lsp-store-object nil lspb)
      do
        (format stream "tree ~d " id)
        (lui-show-tree top input :chart chart :morphs morphs :stream stream)
        (format stream " ~s~a~%" title %lui-eoc%)
      finally
        (format %lui-stream% "~a" (get-output-stream-string stream)))
  (force-output %lui-stream%))

(defun lui-show-tree (top &optional (input *sentence*)
                      &key morphs chart lspb (stream %lui-stream%))
  (let* ((edge (get top 'edge-record))
         (tdfs (get top 'edge-fs))
         (lspb (make-lspb
                :context lspb :input input :morphs morphs :chart chart
                :edge edge :dag tdfs))
         (daughters (get top 'daughters))
         (form (when (and daughters (null (rest daughters))
                          (null (get (first daughters) 'edge-record)))
                 (get-string-for-edge (first daughters))))
         (id (lsp-store-object nil lspb))
         (label (get-string-for-edge top))
         (n (edge-id edge))
         (rule (if (rule-p (edge-rule edge))
                 (rule-id (edge-rule edge))
                 (string-downcase (string (first (edge-lex-ids edge)))))))
    (format stream "#T[~a ~s ~s ~a ~a " id label form n rule)
    (loop
        for daughter in (if form (get (first daughters) 'daughters) daughters)
        do (lui-show-tree
            daughter input
            :morphs morphs :chart chart :lspb lspb :stream stream))
    (format stream "]")))

(defun lui-display-fs (tdfs title id &optional failures)
  (declare (ignore id))
  (let* ((id (lsp-store-object nil (make-lspb :dag tdfs)))
         (dag (tdfs-indef tdfs))
         (*package* (find-package :lkb)))
    (format
     %lui-stream%
     "parameter type-font #F[Helvetica ~a roman blue]~a~%~
      parameter feature-font #F[Helvetica ~a roman black]~a~%"
     *fs-type-font-size* %lui-eoc%
     *fs-type-font-size* %lui-eoc%)
    (let ((string (with-output-to-string (stream)
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %lui-stream% string))
    #+:null
    (format %lui-stream% " ~s~%" path)
    (format %lui-stream% " ~s~%" title)
    (format %lui-stream% "~@[[~{~s~^ ~}]~]" failures)
    (format %lui-stream% "~a" %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-show-chart (&optional (input *sentence*)
                                 (chart (copy-array *chart*))
                                 (morphs (copy-array *morphs*)))
  
  (let ((stream (make-string-output-stream))
        (nvertices (loop 
                       for i from 0 for foo across morphs 
                       when (null foo) return i)))
    (format 
     stream 
     "parameter chart-word-font #F[Helvetica ~a oblique black]~a~%~
      parameter chart-edge-font #F[Helvetica ~a roman black]~a~%~
      chart ~d ~d ~s~%"
     *parse-tree-font-size* %lui-eoc%
     *parse-tree-font-size* %lui-eoc%
     -1 nvertices
     (if input (format nil "`~a' (Chart)" input) "Chart View"))

    (loop
        for key downfrom -1
        for from from 0
        for morph across morphs
        while morph do
          (format 
           stream
           "  #E[~a ~a ~a ~a ~s \"\" []]~%"
           key key from #+:null (+ from 1) -1 (morph-edge-word morph)))

    ;;
    ;; given the (archaic) treatment of orthography-changing rules in the LKB,
    ;; some edges are more equal than others (i.e. not in the chart).
    ;;
    (loop
        for edge in *morph-records*
        for to = (edge-to edge)
        for from = (edge-from edge)
        for lspb = (make-lspb
                    :input input :morphs morphs :chart chart :edge edge)
        for key = (lsp-store-object nil lspb)
        for id = (edge-id edge)
        for name = (tree-node-text-string (find-category-abb (edge-dag edge)))
        for label = (typecase (edge-rule edge)
                      (string (first (edge-lex-ids edge)))
                      (symbol (edge-rule edge))
                      (rule (rule-id (edge-rule edge)))
                      (t :unknown))
        when (and (numberp from) (numberp to)) do
          (format 
           stream
           "  #E[~a ~:[-1~;~a~] ~a ~a ~s \"~(~a~)\" []"
           key id id from to name label)
          (loop
              for child in (edge-children edge)
              do (format stream " ~a" (edge-id child)))
          (when (edge-morph-history edge)
            (format stream " ~a"(edge-id (edge-morph-history edge))))
          ;;
          ;; for lexemes, generate pseudo daughters list in terms of token ids
          ;;
          (when (stringp (edge-rule edge))
            (loop
                for i from (- (+ from 1)) downto (- to)
                do (format stream " ~a" i)))
          (format stream "]~%"))
                    
    (loop
        for to from 0 to (min nvertices *chart-limit*)
        for configurations 
        = (let ((entry (aref chart to 0)))
            (when (chart-entry-p entry)
              (sort (copy-list (chart-entry-configurations entry))
                    #'(lambda (span1 span2)
                        (cond
                         ((eql (chart-configuration-begin span1)
                               (chart-configuration-begin span2))
                          (< (edge-id (chart-configuration-edge span1))
                             (edge-id (chart-configuration-edge span2))))
                         (t
                          (< (chart-configuration-begin span1)
                             (chart-configuration-begin span2))))))))
        do
          (loop
              for configuration in configurations
              for edge = (if (edge-p configuration) 
                           configuration
                           (chart-configuration-edge configuration))
              for from = (if (edge-p configuration)
                           (edge-from configuration)
                           (chart-configuration-begin configuration))
              for lspb = (make-lspb
                          :input input :morphs morphs :chart chart
                          :edge edge)
              for key = (lsp-store-object nil lspb)
              for id = (edge-id edge)
              for name 
              = (format 
                 nil 
                 "~a[~a]"
                 (tree-node-text-string (find-category-abb (edge-dag edge)))
                 id)
              for label = (typecase (edge-rule edge)
                            (string (first (edge-lex-ids edge)))
                            (symbol (edge-rule edge))
                            (rule (rule-id (edge-rule edge)))
                            (t :unknown))
              when (and (numberp from) (numberp to)) do
                (format 
                 stream
                 "  #E[~a ~:[-1~;~a~] ~a ~a ~s \"~(~a~)\" []"
                 key id id from to name label)
                (loop
                    for child in (edge-children edge)
                    do (format stream " ~a" (edge-id child)))
                (when (edge-morph-history edge)
                  (format stream " ~a"(edge-id (edge-morph-history edge))))
                ;;
                ;; for lexemes, generate pseudo daughters list again
                ;;
                (when (stringp (edge-rule edge))
                  (loop
                      for i from (- (+ from 1)) downto (- to)
                      do (format stream " ~a" i)))
                (format stream "]~%")))
    (format stream "~a" %lui-eoc%)
    (format %lui-stream% "~a" (get-output-stream-string stream)))
  (force-output %lui-stream%))

(defun lui-display-mrs (mrs)
  (let* ((id (lsp-store-object nil (make-lspb :mrs mrs)))
         (dag (mrs::psoa-to-dag mrs))
         (title "Simple MRS Display"))
    (let ((string (with-output-to-string (stream)
                    (when *lui-hidden-features*
                      (format 
                       stream 
                       "parameter+ collapsed-features ~a~a~%"
                       (first *lui-hidden-features*) %lui-eoc%)
                      (loop
                          for foo in (rest *lui-hidden-features*)
                          do
                            (format 
                             stream 
                             "parameter+ collapsed-features ~a~a~%"
                             foo %lui-eoc%)))
                    (format stream "avm ~d " id)
                    (display-dag1 dag 'linear stream))))
      (format %lui-stream% string))
    (format %lui-stream% " ~s~a~%" title %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-status-p (key)
  (when (and (streamp %lui-stream%) (open-stream-p %lui-stream%))
    (case key
      #-:null
      (:tree (streamp %lui-stream%))
      #-:null
      (:avm (streamp %lui-stream%))
      #-:null
      (:chart (streamp %lui-stream%))
      #-:null
      (:mrs (streamp %lui-stream%)))))

(defun copy-array (array)
  (let ((dimensions (array-dimensions array))
        (element-type (array-element-type array))
        (adjustable (adjustable-array-p array))
        (fill-pointer (when (array-has-fill-pointer-p array)
                        (fill-pointer array))))
    (let ((new-array
           (apply #'make-array
                  (list dimensions
                        :element-type element-type
                        :adjustable adjustable
                        :fill-pointer fill-pointer))))
      (dotimes (i (array-total-size array))
        (setf (row-major-aref new-array i)
          (row-major-aref array i)))
      new-array)))