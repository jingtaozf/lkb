;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2003--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

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
   #-(or :darwin :macosx)
   (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                              :name "yzlui"))
   #+(or :darwin :macosx)
   (namestring
    (make-pathname 
     :directory (pathname-directory
		 (dir-append
		  make::bin-dir '(:relative "yzlui.app" "Contents" "MacOS")))
     :name "yzlui"))))
   
(defparameter *lui-autonomy-p* t)

(defparameter *lui-debug-p* nil)

(defparameter %lui-stream% nil)

(defparameter %lui-socket% nil)

(defparameter %lui-pid% nil)

(defparameter %lui-process% nil)

(defparameter %lui-pending-events% nil)

(defparameter %lui-eoc% (format nil " ~a" #\page))

(defun lui-initialize (&key runtimep port lui)
  (declare (ignore runtimep))
  
  (lui-shutdown)
  #+:linux
  (let ((display (getenv "DISPLAY")))
    (when (and (null port) (or (null display) (equal display "")))
      (return-from lui-initialize)))
  (setf *lui-application*
    (let* ((directory 
            #-(or :darwin :macosx)
            (pathname-directory (pathname make::bin-dir))
            #+(or :darwin :macosx)
            (pathname-directory
             (dir-append
              make::bin-dir '(:relative "yzlui.app" "Contents" "MacOS"))))
           (lui
            (and (stringp lui) (make-pathname :directory directory :name lui)))
           (binary
            (if (and lui (probe-file lui))
              (namestring lui)
              (namestring
               (make-pathname :directory directory :name "yzlui")))))
      (format nil "exec ~a -p" binary)))

  (if port
    (let* ((socket (socket:make-socket :connect :passive :local-port port))
           (stream (socket:accept-connection socket :wait t))
           (address (socket:remote-host stream))
           (host (and address (socket:ipaddr-to-hostname address)))
           (port (socket:remote-port stream)))
      (format
       t
       "lui-initialize(): remote connection from `~a:~a'~%"
       host port)
      (setf %lui-socket% socket)
      (setf %lui-stream% stream)
      (setf %lui-pid% nil))
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
      (setf foo foo)))

  #+:allegro
  (setf (stream-external-format %lui-stream%) 
    (excl:find-external-format :utf-8))

  (lui-parameters :all)
  
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
  (lui-shutdown))

(defun lui-shutdown ()

  (when %lui-stream%
    (ignore-errors
     (format %lui-stream% "~%~%quit~a" %lui-eoc%)
     (force-output %lui-stream%))
    (sleep 2)
    (ignore-errors (close %lui-stream%))
    (setf %lui-stream% nil))
  (when %lui-socket%
    #-:acl-compat (ignore-errors (socket:shutdown %lui-socket% :output))
    #-:acl-compat (ignore-errors (socket:shutdown %lui-socket% :input))
    (ignore-errors (close %lui-socket%))
    (setf %lui-socket% nil))
  #-:clisp
  (when %lui-pid%
    (ignore-errors
     (run-process (format nil "kill -HUP ~d" %lui-pid%)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null")
     (run-process (format nil "kill -TERM ~d" %lui-pid%)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null")
     (run-process (format nil "kill -QUIT ~d" %lui-pid%)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null"))
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

(defun lui-parameters (&optional style)
  (when (or (null style) (eq style :all))
    (format
     %lui-stream%
     "parameter list-type ~a~a~%~
      parameter non-empty-list-type ~a~a~%~
      parameter empty-list-type ~a~a~%~
      parameter list-head ~a~a~%~
      parameter list-tail ~a~a~%~
      status ready~a~%"
     *list-type* %lui-eoc%
     *non-empty-list-type* %lui-eoc%
     *empty-list-type* %lui-eoc% 
     (first *list-head*) %lui-eoc%
     (first *list-tail*) %lui-eoc%
     %lui-eoc%))
  (unless *lui-autonomy-p*
    (when (smember style '(:avm :all))
      (format
       %lui-stream%
       "parameter avm-bar-font #F[Helvetica ~a roman blue]~a~%~
        parameter avm-type-font #F[Helvetica ~a roman blue]~a~%~
        parameter avm-feature-font #F[Helvetica ~a roman black]~a~%~
        parameter avm-tag-font #F[Helvetica ~a roman red]~a~%"
       *fs-type-font-size* %lui-eoc%
       *fs-type-font-size* %lui-eoc%
       *fs-type-font-size* %lui-eoc%
       *fs-type-font-size* %lui-eoc%))
    (when (smember style '(:tree :all))
      (format
       %lui-stream%
       "parameter tree-summary-bar-font #F[Helvetica ~a roman black]~a~%~
        parameter tree-summary-node-font #F[Helvetica ~a roman black]~a~%~
        parameter tree-summary-surface-font #F[Helvetica ~a roman black]~a~%~
        parameter tree-detail-bar-font #F[Helvetica ~a roman black]~a~%~
        parameter tree-detail-node-font #F[Helvetica ~a roman black]~a~%~
        parameter tree-detail-surface-font #F[Helvetica ~a roman black]~a~%"
       *summary-tree-font-size* %lui-eoc%
       *summary-tree-font-size* %lui-eoc%
       *summary-tree-font-size* %lui-eoc%
       *parse-tree-font-size* %lui-eoc%
       *parse-tree-font-size* %lui-eoc%
       *parse-tree-font-size* %lui-eoc%))
    (when (smember style '(:chart :all))
      (format
       %lui-stream%
       "parameter chart-bar-font #F[Helvetica ~a roman blue]~a~%~
        parameter chart-word-font #F[Helvetica ~a roman black]~a~%~
        parameter chart-word-font #F[Helvetica ~a roman black]~a~%"
       *parse-tree-font-size* %lui-eoc%
       *parse-tree-font-size* %lui-eoc%
       *parse-tree-font-size* %lui-eoc%)))
  
  (force-output %lui-stream%))

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
                                        (tchart (copy-array *tchart*))
                                        (chart (copy-array *chart*)))
  (loop
      with stream = (make-string-output-stream)
      initially
        (lui-parameters :tree)
        (format
         stream
         "group ~d ~s~a~%"
         (length edges)
         (if input
           (format
            nil
            "`~a' (~a Tree~p)"
            input (length edges) (length edges))
           "Analysis Result(s)")
         %lui-eoc%)
      for i from 1
      for title = (format nil "`~a' Parse Tree # ~d" input i)
      for edge in edges 
      for top = (make-new-parse-tree edge 1)
      for tdfs = (get top 'edge-fs)
      for lspb = (make-lspb
                  :input input :tchart tchart :chart chart
                  :edge edge :dag tdfs)
      for id = (lsp-store-object nil lspb)
      do
        (setf (lspb-id lspb) id)
        (format stream "tree ~d " id)
        (lui-show-tree
         top input :lspb lspb
         :tchart tchart :chart chart :stream stream)
        (format stream " ~s~a~%" title %lui-eoc%)
      finally
        (format %lui-stream% "~a" (get-output-stream-string stream)))
  (force-output %lui-stream%))

(defun lui-show-tree (top &optional (input *sentence*)
                      &key tchart chart lspb (stream %lui-stream%))
  (let* ((edge (get top 'edge-record))
         (tdfs (get top 'edge-fs))
         (lspb (make-lspb
                :context lspb :input input :tchart tchart :chart chart
                :edge edge :dag tdfs))
         (daughters (get top 'daughters))
         (form (when (and daughters (null (rest daughters))
                          (null (get (first daughters) 'edge-record)))
                 (get-string-for-edge (first daughters))))
         (id (or (lspb-id lspb) (let ((id (lsp-store-object nil lspb)))
                                  (setf (lspb-id lspb) id)
                                  id)))
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
            :tchart tchart :chart chart :lspb lspb :stream stream))
    (format stream "]")))

(defun lui-display-fs (tdfs title id &optional failures)
  (declare (ignore id))
  (let* ((tdfs (if (tdfs-p tdfs) tdfs (make-tdfs :indef tdfs)))
         (id (lsp-store-object nil (make-lspb :dag tdfs)))
         (dag (tdfs-indef tdfs))
         (*package* (find-package :lkb)))
    
    (lui-parameters :avm)
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
                                 (tchart (copy-array *tchart*))
                                 (chart (copy-array *chart*)))
  
  (let* ((stream (make-string-output-stream))
         (root (make-lspb :input input :tchart tchart :chart chart))
         (id (lsp-store-object nil root))
         (nvertices (max *tchart-max* *chart-max*)))
    (lui-parameters :chart)
    (format
     stream
     "chart ~d ~d ~s~%"
     id nvertices
     (if input (format nil "`~a' (Chart)" input) "Chart View"))

    (loop
        for key downfrom -1
        for from from 0 to (- nvertices 1)
        for token = (loop
                        for configuration in (aref *tchart* from 1)
                        for edge = (when (chart-configuration-p configuration)
                                     (chart-configuration-edge configuration))
                        when (token-edge-p edge) return edge)
        when token do
          (format 
           stream
           "  #E[~a ~a ~a ~a ~(~s~) \"\" []]~%"
           key key from #+:null (+ from 1) -1 (token-edge-word token)))

    (loop
        for to from 0 to (min nvertices *chart-limit*)
        for configurations 
        = (sort (copy-list (aref chart to 0))
                #'(lambda (span1 span2)
                    (cond
                     ((eql (chart-configuration-begin span1)
                           (chart-configuration-begin span2))
                      (< (edge-id (chart-configuration-edge span1))
                         (edge-id (chart-configuration-edge span2))))
                     (t
                      (< (chart-configuration-begin span1)
                         (chart-configuration-begin span2))))))
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
                          :input input :tchart tchart :chart chart
                          :edge edge)
              for key = (lsp-store-object nil lspb)
              for id = (edge-id edge)
              for name = (lui-chart-edge-name edge)
              for label = (lui-chart-edge-label edge)
              do (push key (lspb-children root))
              when (and (numberp from) (numberp to)) do
                (format 
                 stream
                 "  #E[~a ~:[-1~;~a~] ~a ~a ~s \"~(~a~)\" []"
                 key id id from to name label)
                (loop
                    for child in (edge-children edge)
                    do (format stream " ~a" (edge-id child)))
                ;;
                ;; for lexemes, generate pseudo daughters list again
                ;;
                (when (stringp (edge-rule edge))
                  (loop
                      for i from (- (+ from 1)) downto (- to)
                      do (format stream " ~a" i)))
                (format stream "]~%")))
    (format stream "~a" %lui-eoc%)
    (format %lui-stream% "~a" (get-output-stream-string stream))
    (force-output %lui-stream%)
    id))

(defun lui-chart-event (id format object)
  (when (and (lspb-p object) (edge-p (lspb-edge object))
             (numberp (edge-id (lspb-edge object))))
    (format
     %lui-stream%
     "chart ~d event ~(~a~) ~d~a~%"
     id (if (eq format :edges) :highlight :restrict)
     (edge-id (lspb-edge object)) %lui-eoc%)
    (force-output %lui-stream%)))

(defun lui-show-gen-result (&optional edges chart)

  (declare (special *gen-record* *gen-chart* 
                    *generator-input* %generator-condition%))
  
  (loop
      with edges = (or edges *gen-record*)
      with chart = (or chart (copy-tree *gen-chart*))
      with stream = (make-string-output-stream)
      with context = (make-lspb)
      with last = (first (last edges))
      for edge in edges 
      for string = (if (consp (g-edge-string edge))
                     (format nil "~{~a~^ ~}" (g-edge-string edge))
                     (g-edge-string edge))
      for lspb = (make-lspb
                  :chart chart :mrs *generator-input*
                  :edge edge :input string)
      initially
        (lsp-store-object nil context)
        (format stream "text ~a #X[" (lspb-id context))
        (unless edges
          (if (typep %generator-condition% 'error)
            (let ((condition (format nil "~a" %generator-condition%)))
              (format stream "\"~a\" ]" (normalize-string condition)))
            (format stream "\"no realizations found\"]")))
      do
        (push (lsp-store-object nil lspb) (lspb-children context))
        (format
         stream
         "  #X[~d \"~a\"] ~:[newline~;]~]~%"
         (lspb-id lspb) string (eq edge last))
      collect lspb into lspbs
      finally
        (loop
            for lspb in lspbs
            for id = (lspb-id lspb)
            do
              (format
               stream
               "  #M[\"Tree\" \"browse ~d ~d tree\"~%     ~
                     \"AVM\" \"browse ~d ~d avm\"~%     ~
                     \"Simple MRS\" \"browse ~d ~d mrs simple\" ~d~%     ~
                     \"Indexed MRS\" \"browse ~d ~d mrs indexed\" ~d~%     ~
                     \"Dependencies\" \"browse ~d ~d dependencies\" ~d]~%"
               id id id id id id id id id id id id id))
        (format stream "  ~s~a~%" "Realization Result(s)" %lui-eoc%)
        (format %lui-stream% "~a" (get-output-stream-string stream)))
  (force-output %lui-stream%))

(defun lui-display-mrs (mrs &optional title (format :simple))
  (let* ((id (lsp-store-object nil (make-lspb :mrs mrs)))
         (title (case format
                  (:simple 
                   (format nil "~@[~a ~]Simple MRS Display" title))
                  (:indexed 
                   (format nil "~@[~a ~]Indexed MRS Display" title))
                  (:dependencies
                   (format 
                    nil
                    "~@[~a ~]Dependencies Display" title))))
         (string (with-output-to-string (stream)
                   (case format
                     (:simple
                      (format
                       stream
                       "parameter avm-collapsed-types [u h i e x]~a~%"
                       %lui-eoc%)
                      (format stream "avm ~d " id)
                      (mrs::lui-dagify-mrs mrs :stream stream))
                     (:indexed
                      (format stream "text 42 ")
                      (mrs::lui-indexed-mrs mrs :stream stream))
                     (:dependencies
                      (format stream "text 42 ")
                      (mrs::ed-output-psoa
                       mrs :stream stream :format :lui))))))
    (format %lui-stream% string)
    (format %lui-stream% " ~s~a~%" title %lui-eoc%))
  (force-output %lui-stream%))

(defun lui-status-p (&optional key format)
  (when (and (streamp %lui-stream%) (open-stream-p %lui-stream%))
    (if (null key)
      (streamp %lui-stream%)
      (case key
        #-:null
        (:tree (streamp %lui-stream%))
        #-:null
        (:avm (streamp %lui-stream%))
        #-:null
        (:chart (streamp %lui-stream%))
        #-:null
        (:realization (streamp %lui-stream%))
        #-:null
        (:mrs (and (streamp %lui-stream%)
                   (or (null format)
                       (member format '(:simple :indexed :dependencies)))))))))

(defun lui-display-tree (tree
                         &optional input
                         &key title (stream %lui-stream%) size (n 0))

  (unless %lui-stream% (return-from lui-display-tree))
  (when (numberp size)
    (format
     stream
     "parameter tree-detail-bar-font #F[Helvetica ~a roman black]~a~%~
      parameter tree-detail-node-font #F[Helvetica ~a roman black]~a~%~
      parameter tree-detail-surface-font #F[Helvetica ~a roman black]~a~%"
     size %lui-eoc% size %lui-eoc% size %lui-eoc%))

  (labels ((label (tree)
             (and (null (rest (rest tree)))
                  (not (consp (first (rest tree))))
                  (first (rest tree))))
           (traverse (tree)
             (if (and (consp (first tree)) (null (rest tree)))
               ;;
               ;; accomodate extra level of embedding at the root of PTB trees
               ;;
               (traverse (first tree))
               (loop
                   initially
                     (format
                      stream " #T[~a ~s ~s ~a ? "
                      n (first tree) (label tree) n (first tree))
                     (incf n)
                   for daughter
                   in (and (consp (first (rest tree))) (rest tree))
                   do (traverse daughter)
                   finally (format stream "]")))))
    (let* ((title (or title (format nil "~@[`~a' ~]Parse Tree" input)))
           (lspb (make-lspb :input input))
           (id (lsp-store-object nil lspb)))
      (setf (lspb-id lspb) id)
      (format stream "tree ~d " id)
      (traverse tree)
      (format stream " ~s~a~%" title %lui-eoc%)
      (force-output stream))))

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
