;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "TSDB")

(defvar *tsdb-wish-pid* nil)

(defvar *tsdb-wish-process* nil)

(defvar *tsdb-wish-stream* nil)

(defparameter *tsdb-podium-home*
  (namestring (dir-append (get-sources-dir "tsdb") '(:relative "tsdb" "tcl"))))

(defparameter *tsdb-podium*
  (namestring (make-pathname 
               :directory *tsdb-podium-home*
               :name "podium.tcl")))

(defparameter *tsdb-wish-application*
  (format
    nil "exec ~a"
    (namestring (make-pathname :directory (pathname-directory make::bin-dir)
                               :name "swish++"))))

(defvar *tsdb-podium-windows* nil)

(defvar *tsdb-podium-debug* nil)

(defparameter %tsdb-podium-pending-events% nil)

(defvar %tsdb-podium-background-process% nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :process))

(defun init-podium ()
  (shutdown-podium)
  (let (foo)
    (setf *tsdb-podium-windows* nil)
    (setf %tsdb-podium-pending-events% nil)
    (multiple-value-setq (*tsdb-wish-stream* foo *tsdb-wish-pid*)
      (run-process *tsdb-wish-application*
                   :wait nil
                   :output :stream :input :stream :error-output nil))
    (format 
     *tsdb-wish-stream* 
     "set globals(podium_home) \"~a\"~%~
      set globals(home) \"~a\"~%~
      set globals(skeleton_directory) \"~a\"~%~
      set globals(exhaustive_p) ~:[0~;1~]~%~
      set globals(write_run_p) ~:[0~;1~]~%~
      set globals(write_parse_p) ~:[0~;1~]~%~
      set globals(write_result_p) ~:[0~;1~]~%~
      set globals(write_output_p) ~:[0~;1~]~%~
      set globals(write_syntax_chart_p) ~:[0~;1~]~%~
      set globals(write_lexicon_chart_p) ~:[0~;1~]~%~
      set globals(gc_p) ~(~a~)~%"
     *tsdb-podium-home* 
     *tsdb-home*
     *tsdb-skeleton-directory*
     *tsdb-exhaustive-p*
     *tsdb-write-run-p* *tsdb-write-parse-p* 
     *tsdb-write-result-p* *tsdb-write-output-p*
     *tsdb-write-syntax-chart-p* *tsdb-write-lexicon-chart-p*
     *tsdb-gc-p*)
    (tsdb-do-phenomena :stream *tsdb-wish-stream*)
    (format *tsdb-wish-stream* "source \"~a\"~%" *tsdb-podium*)
    (format *tsdb-wish-stream* 
            "set globals(data) \"~a\"~%" (if *tsdb-data* *tsdb-data* ""))
    (force-output *tsdb-wish-stream*))
  (setf *tsdb-wish-process*
    (mp:process-run-function (list :name "tsdb(1) podium")
                             #'podium-loop)))

(defun shutdown-podium ()
  (ignore-errors
   (mp:process-kill *tsdb-wish-process*))
  (ignore-errors
   (format *tsdb-wish-stream* "~%~%exit~%")
   (force-output *tsdb-wish-stream*)
   (close *tsdb-wish-stream*))
  (setf *tsdb-wish-stream* nil)
  (ignore-errors
   (run-process "kill -HUP ~d" *tsdb-wish-pid* 
                :wait t :output "/dev/null" :error-output "/dev/null")
   (run-process "kill -TERM ~d" *tsdb-wish-pid* 
                :wait t :output "/dev/null" :error-output "/dev/null")
   (run-process "kill -QUIT ~d" *tsdb-wish-pid* 
                :wait t :output "/dev/null" :error-output "/dev/null"))
  (when *tsdb-wish-pid*
    (sys:os-wait nil *tsdb-wish-pid*))
  (setf *tsdb-wish-pid* nil)
  (setf *tsdb-podium-windows* nil))

(defun reset-podium ()
  (mp:process-reset *tsdb-wish-process*))
  
(defun podium-loop ()
  (let ((*package* (find-package "TSDB")))
    (loop
        while (and *tsdb-wish-stream* (streamp *tsdb-wish-stream*))
        do (process-pending-events)
           (let ((form (read *tsdb-wish-stream* nil nil)))
             (if form
               (evaluate-remote-command form)
               (shutdown-podium))))))

(defun evaluate-remote-command (form)
  
  (when *tsdb-podium-debug*
    (format t "podium-loop(): ~s~%" form))
  (case (first form)
    
    (:interrupt
     (case (first (second form))
       (abort
        (process-kill %tsdb-podium-background-process%)
        (setf %tsdb-podium-background-process% nil))))
    
    (:event
     (unless (eq (first (second form)) 'quit) (busy))
     (unwind-protect
      (multiple-value-bind (value condition)
       (ignore-errors
       (let* ((command (second form))
              (action (first command))
              (arguments (rest command))
              (user (current-user))
              (file (format
                     nil "/tmp/.tsdb.podium.~a.~a"
                     user (string-downcase (string (gensym ""))))))
         (case action

           (set
            (apply #'set arguments)
            (when (eq (first arguments) '*tsdb-home*)
              (setf *tsdb-profile-cache* 
                (make-hash-table :size 42 :test #'equal))))

           (list
            (let* ((data (first arguments))
                   (index (second arguments))
                   (message 
                    (if (and data index)
                      (format nil "updating `~a' status ..." data)
                      "updating tsdb(1) database list ...")))
              (status :text message)
              (unless (and data index)
                (send-to-podium "catch {unset test_suites}" :wait t))
              (let ((dbs (with-output-to-string (stream)
                           (tsdb-do-list *tsdb-home* 
                                         :stream stream :format :tcl
                                         :name data :index index
                                         :meter (make-meter 0 1)))))
                (send-to-podium dbs :wait t)
                (send-to-podium "update_ts_list" :wait t))
              (status :text (format nil "~a done" message) :duration 5)))

           (skeletons
            (meter :value 0)
            (status :text "updating tsdb(1) skeleton list ...")
            (send-to-podium "catch {unset skeletons}" :wait t)
            (let ((skeletons 
                   (with-output-to-string (stream)
                     (tsdb-do-skeletons nil 
                                        :stream stream :format :tcl
                                        :meter (make-meter 0 1)))))
                (send-to-podium skeletons :wait t)
                (send-to-podium "update_skeleton_list" :wait t))
            (meter :value 1)
            (status :text "updating tsdb(1) skeleton list ... done" 
                    :duration 3))

           (create
            (busy :action :release)
            (let* ((data (first arguments))
                   (path (suggest-test-run-directory data))
                   (return 
                     (send-to-podium 
                      (format nil "input \"create:\" ~s ~s" path *tsdb-home*)
                      :wait t))
                   (path (when (and (eq (first return) :ok)
                                    (eql (second return) 0))
                           (let ((return 
                                   (send-to-podium 
                                    "format \"\\\"%s\\\"\" $globals(input)"
                                    :wait t)))
                             (when (eq (first return) :ok) 
                               (second return))))))
              (when path
                (let* ((path (string-right-trim (list *tsdb-slash*) path))
                       (parent (when (find *tsdb-slash* path)
                                 (subseq 
                                  path 
                                  0 (position *tsdb-slash* path :from-end t))))
                       (return (send-to-podium 
                                (format nil "file mkdir ~a" parent)
                                :wait t)))
                  (if (eq (first return) :ok)
                    (let* ((path (string-strip *tsdb-home* path))
                           (path (string-right-trim '(#\/) path)))
                      (meter :value 0.1)
                      (status :text (format nil "creating `~a' ..." path))
                      (let* ((result
                              (tsdb-do-create path data 
                                              :stream nil :create t
                                              :meter (make-meter 0.1 0.6)))
                             (message
                              (case result
                                (0 (format 
                                    nil 
                                    "created `~a' as default database"
                                    path))
                                (5 (format 
                                    nil 
                                    "database `~a' already exists"
                                    path))
                                (t (format 
                                    nil 
                                    "mysterious problem creating `~a'"
                                    path))))
                             (new (when (zerop result)
                                    (tsdb-do-list 
                                     *tsdb-home* :stream nil 
                                     :format :list :name path 
                                     :meter (make-meter 0.6 1.0)))))
                        (when (and new (= (length new) 1))
                          (send-to-podium
                           (format nil "update_ts_list add ~a" (first new))
                           :wait t))
                        (unless (zerop result) (beep))
                        (meter :value 1)
                        (status :text message :duration 10)))
                    (let ((message 
                           (format nil "unable to create `~a'" parent)))
                      (beep)
                      (status :text message :duration 10)))))))
           
           (purge
            (let* ((data (first arguments))
                   (action (third arguments)))
              (apply #'purge-test-run arguments)
              (when (eq action :purge)
                (send-to-podium "tsdb_update selection" :wait t))))
           
           (relations
            (let* ((data (first arguments))
                   (directory (find-tsdb-directory data))
                   (file (concatenate 'string directory "relations"))
                   (title 
                    (format nil "tsdb(1) `~a' Database Schema" data))
                   (message 
                    (format nil "retrieving `~a' database schema ..." data)))
              (status :text message)
              (let ((return 
                      (send-to-podium 
                       (format 
                        nil 
                        "show_text ~s \".~(~a~)\" ~s ~d ~d" 
                        file (gensym "") title 80 25)
                       :wait t)))
                (when (and (equal (first return) :ok) 
                           (equal (first (second return)) :text))
                  (push (append (second return)
                                (pairlis 
                                 (list :file 
                                       :command)
                                 (list file
                                       (cons action arguments))))
                        *tsdb-podium-windows*)))
              (status :text (format nil "~a done" message) :duration 3)))

           (select
            (let* ((data (first arguments))
                   (condition (fifth arguments))
                   (condition (if (equal condition "") nil condition))
                   (title 
                    (format nil "tsdb(1) `~a' Data~@[ (~a)~]" data condition))
                   (message "computing table layout and geometry ..."))
              (apply (symbol-function action)
                     (append (rest arguments)
                             (list data 
                                   :file file :format :tcl 
                                   :sort t :meter (make-meter 0 1))))
              (status :text message)
              (let ((return 
                      (send-to-podium 
                       (format 
                        nil 
                        "showtable ~s \".~(~a~)\" ~s ~s" 
                        file (gensym "") data title)
                       :wait t)))
                (when (and (equal (first return) :ok) 
                           (equal (first (second return)) :table))
                  (push (append (second return)
                                (pairlis 
                                 (list :file 
                                       :command)
                                 (list file
                                       (append (rest arguments)
                                               (list data :file file)))))
                        *tsdb-podium-windows*)))
              (status :text (format nil "~a done" message) :duration 2)))

           (vocabulary
            (let* ((data (first arguments))
                   (meter (list :meter (make-meter 0 1)))
                   (message 
                    (format nil "retrieving `~a' vocabulary ..." data)))
              (status :text message)
              (apply #' tsdb-do-vocabulary (append arguments meter))
              (status :text (format nil "~a done" message) :duration 3)))

           (process
            (let* ((data (first arguments))
                   (message (format nil "test run for `~a' running ..." data))
                   (name (format nil "tsdb(1) `~a' test run" data)))
              (setf %tsdb-podium-background-process%
                (mp:process-run-function 
                 (list :name name)
                 #'(lambda ()
                     (apply
                      #'tsdb-do-process
                      (append arguments '(:podium t)))
                     (send-to-podium (format 
                                      nil 
                                      "update_ts_list update ~a"
                                      data)
                                     :wait t)
                     (status :text (format nil "~a done" message) :duration 5)
                     (process-pending-events))))
              (status :text message)))

           ((analyze-competence analyze-performance)
            (let* ((performancep  (eq action 'analyze-performance))
                   (database (second command))
                   (code (if (eq action 'analyze-performance)
                           (second arguments)
                           (third arguments)))
                   (restrictor 
                    (when performancep
                      (case code
                        (:all nil)
                        (:positive 
                         #'(lambda (foo)
                             (not (= (get-field :i-wf foo) 1))))
                        (:negative 
                         #'(lambda (foo)
                             (not (= (get-field :i-wf foo) 0))))
                        (:analyzed 
                         #'(lambda (foo)
                             (< (get-field :readings foo) 1)))
                        (:unanalyzed 
                         #'(lambda (foo)
                             (not (= (get-field :readings foo) 0)))))))
                   (arguments 
                    (if performancep (delete code arguments) arguments))
                   (arguments (append arguments 
                                      (list :file file 
                                            :restrictor restrictor 
                                            :format :tcl
                                            :meter (make-meter 0 1))))
                   (title 
                    (if performancep
                      (format 
                       nil 
                       "tsdb(1) `~a' Performance Profile (~(~a~) items)"
                       database code)
                      (format 
                       nil 
                       "tsdb(1) `~a' ~:[Overgeneration~;Coverage~] Profile"
                       database (= code 1))))
                   (message "computing table layout and geometry ..."))
              (apply (symbol-function action) arguments)
              (when (probe-file file)
                (status :text message)
                (let ((return 
                        (send-to-podium 
                         (format 
                          nil 
                          "showtable ~s \".~(~a~)\" ~s ~s" 
                          file (gensym "") database title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :table))
                    (push (append (second return)
                                  (pairlis 
                                   (list :data 
                                         :command)
                                   (list database
                                         (cons action arguments))))
                          *tsdb-podium-windows*))))
              (status :text (format nil "~a done" message) :duration 2)))

           ((graph-words-first-total graph-words-etasks-stasks-ftasks)
            (let* ((database (first arguments))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' ~a Distribution"
                           database
                           (case action
                             (graph-words-etasks-stasks-ftasks
                              "Parser Tasks")
                             (graph-words-first-total
                              "Parsing Times"))))
                   (message "computing graph layout and geometry ..."))
              (apply (symbol-function action) 
                     (append arguments 
                             (list :file file :meter (make-meter 0 1))))
              (when (probe-file file)
                (status :text message)
                (let ((return 
                        (send-to-podium 
                         (format 
                          nil 
                          "showgraph ~s \".~(~a~)\" ~s ~s" 
                          file (gensym "") database title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :graph))
                    (push (append (second return)
                                  (pairlis 
                                   (list :data 
                                         :command)
                                   (list database
                                         (cons action arguments))))
                          *tsdb-podium-windows*))))
              (status :text (format nil "~a done" message) :duration 2)))

           (rules
            (let* ((database (first arguments))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' Rule Application Profile"
                           database))
                   (message (format nil "retrieving `~a' data ..." database))
                   (gmessage "computing graph layout and geometry ..."))
              (status :text message)
              (meter :value 0.1)
              (apply #'rule-statistics (append arguments (list :file file)))
              (run-meter 500)
              (when (probe-file file)
                (status :text gmessage)
                (let ((return
                        (send-to-podium
                         (format 
                          nil 
                          "showgraph ~a \".~(~a~)\" ~s ~s" 
                          file (gensym "") database title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :graph))
                    (push (append (second return)
                                  (pairlis 
                                   (list :data 
                                         :command)
                                   (list database
                                         (cons action arguments))))
                          *tsdb-podium-windows*)))
                (status :text (format nil "~a done" gmessage) :duration 2))))

           (close
            (if (second command)
              (let ((window (find-podium-window (second command))))
                (remove-podium-window window))
              (dolist (window *tsdb-podium-windows*)
                (let* ((properties (rest window))
                       (toplevel (get-field :toplevel properties))
                       (command (format nil "tsdb_close ~s" toplevel)))
                  (send-to-podium command :wait nil :quiet t)))))

           ((compare-competence compare-performance)
            (let* ((competence-p (eq action 'compare-competence))
                   (source (first arguments))
                   (target (second arguments))
                   (meter (make-meter 0 1))
                   (name (format nil "~a:~a" source target))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' vs. `~a' ~a Comparison" 
                           (if competence-p "Competence" "Performance")
                           source target))
                   (message "computing table layout and geometry ...")
                   (result (apply (symbol-function action)
                                  (append arguments
                                          (list :file file :format :tcl 
                                                :meter meter)))))
              (case result
                (1
                 (beep)
                 (status :text (format
                                nil
                                "`~a' and `~a' are incompatible ~
                                 (e.g. in phenomena classification)"
                                source target)
                         :duration 10))
                (t
                 (status :text message)
                 (let ((return 
                         (send-to-podium 
                          (format 
                           nil 
                           "showtable ~s \".~(~a~)\" ~s ~s" 
                           file (gensym "") name title)
                          :wait t)))
                   (status :text (format nil "~a done" message) :duration 2)
                   (when (and (equal (first return) :ok) 
                              (equal (first (second return)) :table))
                     (push (append (second return)
                                   (pairlis 
                                    (list :file 
                                          :command)
                                    (list file
                                          (append arguments
                                                  (list :file file)))))
                           *tsdb-podium-windows*)))))))
           
           (detail
            (let* ((source (first arguments))
                   (target (second arguments))
                   (meter (make-meter 0 1))
                   (name (format nil "~a:~a" source target))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' vs. `~a' Detail Comparison" 
                           source target))
                   (message "computing table layout and geometry ..."))
              (apply #'compare-in-detail
                     (append arguments
                             (list :file file :format :tcl :meter meter)))
              (status :text message)
              (let ((return 
                      (send-to-podium 
                       (format 
                        nil 
                        "showtable ~s \".~(~a~)\" ~s ~s" 
                        file (gensym "") name title)
                       :wait t)))
                (status :text (format nil "~a done" message) :duration 2)
                (when (and (equal (first return) :ok) 
                           (equal (first (second return)) :table))
                  (push (append (second return)
                                (pairlis 
                                 (list :file 
                                       :command)
                                 (list file
                                       (append (rest arguments)
                                               (list :file file)))))
                        *tsdb-podium-windows*)))))
           
           (latex
            (status :text "generating LaTeX output ...")
            (let* ((window (find-podium-window (second command)))
                   (properties (rest window))
                   (data (get-field :data properties))
                   (command (get-field :command properties))
                   (command (remove-key-argument :file command))
                   (command (remove-key-argument :format command))
                   (output (format nil "/tmp/~a.tex" (directory2file data))))
              (apply (symbol-function (first command)) 
                     (append (rest command)
                             (list :file output :format :latex)))
              (status :text (format nil "wrote `~a'" output) :duration 5)))

           (quit
            (format 
             *tsdb-io* 
             "~&podium-loop(): (remote) shutdown from podium.~%")
            (shutdown-podium)))))
       (when condition 
         (beep)
         (status :text "error processing tsdb(1) podium event" :duration 10)))
      (unless (eq (first (second form)) 'quit)
        (busy :action :release))))

    (t
     (format 
      *tsdb-io* 
      "~&podium-loop(): unexpected podium return `~s'.~%" 
      form))))

(defun send-to-podium (string &key (wait nil) (quiet nil) recursive)
  (unless recursive
    (when (not (eq mp:*current-process* *tsdb-wish-process*))
      (mp:process-add-arrest-reason *tsdb-wish-process* :send-to-podium))
    (format *tsdb-wish-stream* "evaluate {~a} ~d;~%" string (if quiet 1 0))
    (force-output *tsdb-wish-stream*))
  (unwind-protect 
      (when (or wait recursive)
        (let ((*package* (find-package "TSDB"))
              (form (when (streamp *tsdb-wish-stream*)
                      (read *tsdb-wish-stream* nil nil))))
          (when *tsdb-podium-debug*
            (format
             *tsdb-io* 
             "~&send-to-podium(): podium return `~s'.~%" 
             form))
          (cond
           ((eq (first form) :event)
            (when *tsdb-podium-debug*
            (format
             *tsdb-io* 
             "~&send-to-podium(): queueing intervening event `~s'.~%" 
             form))
            (setf %tsdb-podium-pending-events% 
              (append %tsdb-podium-pending-events% (list form)))
            (send-to-podium nil :recursive t))
           (t
            form))))
    (mp:process-revoke-arrest-reason *tsdb-wish-process* :send-to-podium)))

(defun process-pending-events ()
  (when (not (eq mp:*current-process* *tsdb-wish-process*))
    (mp:process-add-arrest-reason *tsdb-wish-process* :pending-events))
  (loop
      while (and *tsdb-wish-stream* (streamp *tsdb-wish-stream*)
                 %tsdb-podium-pending-events%)
      do (evaluate-remote-command (pop %tsdb-podium-pending-events%)))
  (mp:process-revoke-arrest-reason *tsdb-wish-process* :pending-events))

(defun find-podium-window (pattern)
  ;;;
  ;;; the find-if() variant for a mysterious reason gives a bus error when
  ;;; compiled in Allegro CL 4.3 on Solaris :-{.
  ;;;
  #+:mystery
  (find-if #'(lambda (window)
               (member pattern window :test #'equal))
           *tsdb-podium-windows*)
  #-:mystery
  (do* ((windows *tsdb-podium-windows* (rest windows))
        (window (first windows) (first windows)))
      ((or (null windows) (member pattern window :test #'equal)) window)))

(defun remove-podium-window (window)
  (setf *tsdb-podium-windows* (delete window *tsdb-podium-windows*)))

(defun meter (&key (value 0.0) (text ""))
  (send-to-podium (format nil "meter ~,3f ~s" value text) :wait t))

(defun meter-advance (increment)
  (send-to-podium (format nil "meter_advance ~,3f" increment) :wait t))

(defun run-meter (duration)
  (send-to-podium (format nil "run_meter ~d" duration) :wait t))

(defun status (&key (text "") (duration 0))
  (send-to-podium (format nil "status ~s ~d" text duration) :wait t))

(eval-when (:load-toplevel :execute)
  (let (previous)
    (defun busy (&key (action :hold) (cursor :watch) (toplevel "."))
      (when (streamp *tsdb-wish-stream*)
        (unless (eq action :restore)
          (let ((status 
                 (send-to-podium (format nil "isbusy ~a" toplevel) :wait t)))
            (if (and (eq (first status) :ok) 
                     (equal (second status) toplevel))
              (setf previous t)
              (setf previous nil))))
        (let* ((action (if (stringp action)
                         (intern action :keyword)
                         action))
               (cursor (if (stringp cursor)
                         (intern cursor :keyword)
                         cursor)))
          (case action
            (:hold
             (let ((return 
                     (send-to-podium 
                      (format nil "busy ~(~a ~a~)" action toplevel) :wait t)))
               (when (eq (first return) :ok)
                 (send-to-podium 
                  (format nil "busy config ~(~a -cursor ~a~)" toplevel cursor) 
                  :wait t))))
            (:release
             (let ((status 
                    (send-to-podium (format nil "isbusy ~a" toplevel) 
                                    :wait t)))
               (when (and (eq (first status) :ok) 
                          (equal (second status) toplevel))
                 (send-to-podium 
                  (format nil "busy ~(~a ~a~)" action toplevel) :wait t))))
            (:restore
             (if previous
               (let ((return (busy)))
                 (setf previous previous)
                 return)
               (let ((return (busy :action :release)))
                 (setf previous previous)
                 return)))))))))

(defun beep ()
  (send-to-podium "tsdb_beep" :wait t))
