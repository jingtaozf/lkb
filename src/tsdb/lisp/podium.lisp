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

#+:allegro
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
    ;;
    ;; this may seem silly: suppress compiler warning about unused .foo.
    ;;
    (when foo (setf foo foo))
    (when *tsdb-wish-pid* (enable-gc-cursor *tsdb-wish-pid*))
    (format 
     *tsdb-wish-stream* 
     "set globals(version) {~a}~%~
      set globals(application) \"~a\"~%~
      set globals(podium_home) \"~a\"~%~
      set globals(home) \"~a\"~%~
      set globals(skeleton_directory) \"~a\"~%~
      set globals(aggregate_dimension) \"~(~s~)\"~%~
      set globals(aggregate_size) \"~@[~d~]\"~%~
      set globals(aggregate_threshold) \"~@[~d~]\"~%~
      set globals(aggregate_lower) \"~@[~d~]\"~%~
      set globals(aggregate_upper) \"~@[~d~]\"~%~
      set globals(exhaustive_p) ~:[0~;1~]~%~
      set globals(write_run_p) ~:[0~;1~]~%~
      set globals(write_parse_p) ~:[0~;1~]~%~
      set globals(write_result_p) ~:[0~;1~]~%~
      set globals(write_output_p) ~:[0~;1~]~%~
      set globals(write_rule_p) ~:[0~;1~]~%~
      set globals(write_syntax_chart_p) ~:[0~;1~]~%~
      set globals(write_lexicon_chart_p) ~:[0~;1~]~%~
      set globals(gc_p) ~(~a~)~%~
      set globals(tenure_p) ~:[0~;1~]~%"
     *tsdb-version*
     (current-application)
     *tsdb-podium-home* 
     *tsdb-home*
     *tsdb-skeleton-directory*
     *statistics-aggregate-dimension*
     *statistics-aggregate-size*
     *statistics-aggregate-threshold*
     *statistics-aggregate-lower*
     *statistics-aggregate-upper*
     *tsdb-exhaustive-p*
     *tsdb-write-run-p* *tsdb-write-parse-p* 
     *tsdb-write-result-p* *tsdb-write-output-p*
     *tsdb-rule-statistics-p*
     *tsdb-write-syntax-chart-p* *tsdb-write-lexicon-chart-p*
     *tsdb-gc-p*
     *tsdb-tenure-p*)
    (tsdb-do-phenomena :stream *tsdb-wish-stream*)
    (format *tsdb-wish-stream* "source \"~a\"~%" *tsdb-podium*)
    (format *tsdb-wish-stream* 
            "set globals(data) \"~a\"~%" (or *tsdb-data* ""))
    (force-output *tsdb-wish-stream*))
  (setf *tsdb-wish-process*
    (mp:process-run-function '(:name "tsdb(1) podium")
                             #'podium-loop)))

(defun shutdown-podium ()

  (setf *tsdb-podium-windows* nil)
  (enable-gc-cursor -1)
  (when *tsdb-wish-stream*
    (ignore-errors
     (format *tsdb-wish-stream* "~%~%exit~%")
     (force-output *tsdb-wish-stream*)
     (sleep 2)
     (close *tsdb-wish-stream*)
     (setf *tsdb-wish-stream* nil)))
  (when *tsdb-wish-pid*
    (ignore-errors
     (run-process "kill -HUP ~d" *tsdb-wish-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -TERM ~d" *tsdb-wish-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -QUIT ~d" *tsdb-wish-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null"))
    (sys:os-wait nil *tsdb-wish-pid*)
    (setf *tsdb-wish-pid* nil))
  (when *tsdb-wish-process*
    (let ((process *tsdb-wish-process*))
      (setf *tsdb-wish-process* nil)
      (ignore-errors
       (mp:process-kill process)))))


(defun reset-podium ()
  #+:debug
  (mp:process-add-arrest-reason *tsdb-wish-process* :reset)
  (setf %tsdb-podium-pending-events% nil)
  #+:debug
  (mp:process-reset *tsdb-wish-process*))
  
(defun podium-loop ()
  (let ((*package* (find-package "TSDB")))
    (loop
        while (streamp *tsdb-wish-stream*)
        do (process-pending-events)
           (let ((*package* (find-package "TSDB"))
                 (form (read *tsdb-wish-stream* nil nil)))
             (if form
               (evaluate-remote-command form)
               (shutdown-podium))))))

(defun evaluate-remote-command (form)
  
  (when *tsdb-podium-debug*
    (format t "podium-loop(): ~s~%" form))
  (case (first form)
    
    (:interrupt
     (case (first (second form))
       (reset (reset-podium))))
    
    (:event
     (unless (eq (first (second form)) 'quit) (busy))
     (unwind-protect
      (multiple-value-bind (foo condition)
       (ignore-errors
        (let* ((command (second form))
               (action (first command))
               (arguments (rest command))
               (user (current-user))
               (file (format
                      nil "/tmp/.tsdb.podium.~a.~a.~a"
                      user (current-pid) 
                      (string-downcase (string (gensym ""))))))
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
                      "obtaining tsdb(1) database list ...")))
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
            (status :text "obtaining tsdb(1) skeleton list ...")
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
                      (format 
                       nil 
                       "input {create:} {~a} {~a} directory" 
                       path *tsdb-home*)
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
                           (path (string-right-trim (list *tsdb-slash*) path)))
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
           
           (import
            (let* ((type (intern (first arguments) :keyword))
                   (source (second arguments))
                   (target (third arguments))
                   (arguments (rest (rest (rest arguments))))
                   (imeter (make-meter 0 0.8))
                   (umeter (make-meter 0.8 1))
                   (message (format
                             nil
                             "importing ~a `~a' ..."
                             (case type
                               (:items "test items from")
                               (:database "tsdb(1) database")
                               (:skeleton "skeleton data from"))
                             source)))
              (status :text message)
              (case type
                (:items
                 (let* ((result (apply #'do-import-items 
                                      (append (list source target)
                                              arguments
                                              (list :meter imeter))))
                        (message 
                         (cond
                          ((consp result)
                           (status :text (format nil "~a done" message))
                           (format
                            nil
                            "inserted ~d test item~p into `~a'"
                            (length result) (length result) target))
                          (t
                           (beep)
                           (format 
                            nil 
                            "mysterious problem creating `~a'"
                            target)))))
                   (status :text message :duration 5)
                   (sleep 2)))
                (:database
                 (let* ((result (apply #'do-import-database 
                                      (append (list source target)
                                              arguments
                                              (list :meter imeter))))
                        (message 
                         (cond
                          ((consp result)
                           (status :text (format nil "~a done" message))
                           (format
                            nil
                            "inserted ~d relation~p into `~a'"
                            (length result) (length result) target))
                          (t
                           (beep)
                           (format 
                            nil 
                            "mysterious problem creating `~a'"
                            target)))))
                   (status :text message :duration 5)
                   (sleep 2))))
              (let* ((new (tsdb-do-list 
                           *tsdb-home* :stream nil
                           :format :list :name target 
                           :meter umeter)))
                (when (and new (= (length new) 1))
                  (send-to-podium
                   (format nil "update_ts_list add ~a" (first new))
                   :wait t)))))
           
           (purge
            (let* ((action (third arguments)))
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
                                 '(:file :command)
                                 (list file (cons action arguments))))
                        *tsdb-podium-windows*)))
              (status :text (format nil "~a done" message) :duration 3)))

           (select
            (let* ((data (first arguments))
                   (condition (fifth arguments))
                   (condition (if (equal condition "") nil condition))
                   (title 
                    (format nil "tsdb(1) `~a' Data~@[ [~a]~]" data condition))
                   (message "computing table layout and geometry ...")
                   (items (apply (symbol-function action)
                                 (append (rest arguments)
                                         (list data 
                                               :file file :format :tcl 
                                               :sort t
                                               :status t
                                               :meter (make-meter 0 1))))))
              (cond
               ((zerop items)
                (status :text (format 
                               nil 
                               "no data in `~a' matching TSQL query"
                               data)
                        :duration 10))
               (t
                (status :text message)
                (let ((return 
                        (send-to-podium 
                         (format 
                          nil 
                          "showtable ~s \".~(~a~)\" ~s {~a}" 
                          file (gensym "") data title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :table))
                    (push (append (second return)
                                  (pairlis 
                                   '(:file 
                                     :command)
                                   (list file
                                         (append (rest arguments)
                                                 (list data :file file)))))
                          *tsdb-podium-windows*)))
                (status :text (format nil "~a done" message) :duration 2)))))

           (schema
            (let* ((data (first arguments)))
              (let ((schema
                     (with-output-to-string (stream)
                       (tsdb-do-schema data :stream stream :format :tcl))))
                (send-to-podium schema :wait t))))
           
           (vocabulary
            (let* ((meter (make-meter 0 1))
                   (interrupt (install-interrupt-handler)))
              (apply #'tsdb-do-vocabulary 
                     (append arguments 
                             (list :meter meter
                                   :interrupt interrupt)))
              (delete-interrupt-handler interrupt)))

           (process
            (let* ((interrupt (install-interrupt-handler))
                   (data (first arguments))
                   (arguments (rest arguments))
                   (interactive (find-key-argument :interactive arguments))
                   (meter (make-meter 0 1)))

              (apply #'tsdb-do-process
                     (cons data 
                           (append arguments 
                                   (list :meter meter :podium t 
                                         :interrupt interrupt))))
              (unless interactive
                (sleep 1)
                (send-to-podium (format 
                                 nil 
                                 "update_ts_list update ~a"
                                 data)
                                :wait t))
              (delete-interrupt-handler interrupt)))

         ((analyze-competence analyze-performance)
          (let* ((data (first arguments))
                 (code (or (find-key-argument :wf arguments) 2))
                 (condition 
                  (when (and *statistics-select-condition*
                             (not (equal *statistics-select-condition* "")))
                    *statistics-select-condition*))
                 (title 
                  (format 
                   nil 
                   "tsdb(1) `~a' ~
                    ~[Overgeneration~;Coverage~;Performance~] Profile~
                    ~@[ [~a]~]"
                     data code condition))
                 (message "computing table layout and geometry ..."))
            (apply (symbol-function action)
                   (append arguments 
                           (list :file file 
                                 :format :tcl
                                 :meter (make-meter 0 1))))
            (when (probe-file file)
              (status :text message)
              (let ((return 
                      (send-to-podium 
                       (format 
                        nil 
                        "showtable ~s \".~(~a~)\" ~s {~a}" 
                        file (gensym "") data title)
                       :wait t)))
                (when (and (equal (first return) :ok) 
                           (equal (first (second return)) :table))
                  (push (append (second return)
                                (pairlis 
                                 '(:data :command)
                                 (list data (cons action arguments))))
                        *tsdb-podium-windows*))))
            (status :text (format nil "~a done" message) :duration 2)))

           ((graph chart)
            (let* ((database (first arguments))
                   (dimension (find-key-argument :dimension arguments))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' by `~(~a~)'~
                            ~@[ [~a]~]"
                           database dimension *statistics-select-condition*))
                   (message "computing graph layout and geometry ..."))
              (apply #'graph
                     (append arguments 
                             (list :file file :meter (make-meter 0 1))))
              (when (probe-file file)
                (status :text message)
                (let ((return 
                        (send-to-podium 
                         (format 
                          nil 
                          "showgraph ~s \".~(~a~)\" ~s {~a}" 
                          file (gensym "") database title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :graph))
                    (push (append (second return)
                                  (pairlis 
                                   '(:data :command)
                                   (list database (cons 'graph arguments))))
                          *tsdb-podium-windows*))))
              (status :text (format nil "~a done" message) :duration 2)))

           (rules
            (let* ((data (first arguments))
                   (view (find-key-argument :view arguments))
                   (meter (make-meter 0 1))
                   (title (format 
                           nil 
                           "tsdb(1) `~a' Rule Application Profile"
                           data)))
              (apply #'rule-statistics (append arguments (list :file file
                                                               :meter meter)))
              (when (probe-file file)
                (let ((return
                        (send-to-podium
                         (format 
                          nil 
                          "show~(~a~) ~a \".~(~a~)\" ~s {~a}" 
                          view file (gensym "") data title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :graph))
                    (push (append (second return)
                                  (pairlis 
                                   '(:data :command)
                                   (list data (cons action arguments))))
                          *tsdb-podium-windows*))))))
           
           (execute
            (let* ((tag (second arguments))
                   (tag (and tag (intern tag :keyword)))
                   (data (get :source tag))
                   (contrast (get :contrast tag))
                   (field (get :field tag))
                   (i-id (get :i-id tag))
                   (title (format 
                           nil 
                           "`~(~a~)' clashes for `~a' (vs. `~a)' [i-id == ~a]"
                           field data contrast i-id))
                   (message "computing table layout and geometry ..."))
              (apply #'execute-tag
                     (append arguments (list :file file)))
              (when (probe-file file)
                (status :text message)
                (let ((return 
                        (send-to-podium 
                         (format 
                          nil 
                          "showtable ~s \".~(~a~)\" ~s {~a}" 
                          file (gensym "") data title)
                         :wait t)))
                  (when (and (equal (first return) :ok) 
                             (equal (first (second return)) :table))
                    (push (append (second return)
                                  (pairlis 
                                   '(:data :command)
                                   (list data (cons 'execute-tag arguments))))
                          *tsdb-podium-windows*))))
              (status :text (format nil "~a done" message) :duration 2)))

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
                           source target
                           (if competence-p "Competence" "Performance")))
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
                                "one (or both) data sets ~
                                 in comparison are empty")
                         :duration 10))
                (2
                 (beep)
                 (status :text (format
                                nil
                                "profiles are incompatible ~
                                 (e.g. in phenomena classification)")
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
                                    '(:file 
                                     :data
                                     :command)
                                    (list file
                                          (format nil "~a@~a" source target)
                                          (cons action arguments))))
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
                                 '(:file 
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
              (run-meter 500)
              (status :text "generating LaTeX output ... done")
              (sleep 0.5)
              (status :text (format nil "wrote `~a'" output) :duration 5)))

           (quit
            (format 
             *tsdb-io* 
             "~&podium-loop(): (remote) shutdown from podium.~%")
            (shutdown-podium)))
         (when (and file (probe-file file) (null *tsdb-debug-mode-p*))
           (delete-file file))))
       (declare (ignore foo))
       (when condition 
         (beep)
         (status :text "error processing tsdb(1) podium event" :duration 10)
         (format *tsdb-io* "podium-loop(): ~a~%" condition)))
      (unless (eq (first (second form)) 'quit)
        (busy :action :release))))

    (t
     (format 
      *tsdb-io* 
      "~&podium-loop(): unexpected podium return `~s'.~%" 
      form)))
  (meter :hide t))

(defun send-to-podium (string &key (wait nil) (quiet nil) recursive)
  (unless recursive
    #+:debug
    (when (and *tsdb-wish-process*
               (not (eq mp:*current-process* *tsdb-wish-process*)))
      (mp:process-add-arrest-reason *tsdb-wish-process* :send-to-podium))
    (when *tsdb-podium-debug*
      (format *tsdb-io* "~&send-to-podium(): [send] `~a'.~%" string))
    (format *tsdb-wish-stream* "evaluate {~a} ~d;~%" string (if quiet 1 0))
    (force-output *tsdb-wish-stream*))
  (unwind-protect 
      (when (or wait recursive)
        (let ((*package* (find-package "TSDB"))
              (form (when (streamp *tsdb-wish-stream*)
                      (read *tsdb-wish-stream* nil nil))))
          (when *tsdb-podium-debug*
            (format *tsdb-io* "~&send-to-podium(): [return] `~s'.~%" form))
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
    #+:debug
    (when (and *tsdb-wish-process*
               (not (eq mp:*current-process* *tsdb-wish-process*)))
      (mp:process-revoke-arrest-reason *tsdb-wish-process* :send-to-podium))))

(defun process-pending-events ()
  #+:debug
  (when (and *tsdb-wish-process*
             (not (eq mp:*current-process* *tsdb-wish-process*)))
    (mp:process-add-arrest-reason *tsdb-wish-process* :pending-events))
  (loop
      while (and (streamp *tsdb-wish-stream*) %tsdb-podium-pending-events%)
      do (evaluate-remote-command (pop %tsdb-podium-pending-events%)))
  #+:debug
  (when (and *tsdb-wish-process*
             (not (eq mp:*current-process* *tsdb-wish-process*)))
    (mp:process-revoke-arrest-reason *tsdb-wish-process* :pending-events)))

(defun install-interrupt-handler (&optional (i 0))
  (delete-interrupt-handler)
  (let* ((user (current-user))
         (file (format
                nil "/tmp/.tsdb.interrupt.~a.~a.~a"
                user (current-pid) (string-downcase (string (gensym ""))))))
    (when (probe-file file) (delete-file file))
    (if (probe-file file)
      (when (< i 42)
        (install-interrupt-handler (+ i 1)))
      (let ((return (send-to-podium 
                     (format nil "install_interrupt_handler {~a}" file) 
                     :wait t)))
        (when (eq (first return) :ok) file)))))

(defun delete-interrupt-handler (&optional file)
  (when (and file (probe-file file)) (delete-file file))
  (send-to-podium "delete_interrupt_handler" :wait t))

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

(defun meter (&key (value 0.0) (text "") hide)
  (if hide
    (send-to-podium (format nil "meter hide") :wait t)  
    (send-to-podium (format nil "meter ~,3f ~s" value text) :wait t)))

(let ((pending 0))
  (defun meter-advance (increment)
    (incf pending increment)
    (when (>= pending 0.01)
      (send-to-podium (format nil "meter_advance ~,6f" pending) :wait t)
      (setf pending 0))))

(defun run-meter (duration)
  (send-to-podium (format nil "run_meter ~d" duration) :wait t))

(defun status (&key (text "") (duration 0))
  (send-to-podium (format nil "status {~a} ~d" text duration) :wait t))

(defun busy (&key (action :freeze) gc)
  (when (and (streamp *tsdb-wish-stream*)
             (not (find :slave *features*)))
    (cond
     ((eq gc :start)
      (gc_start 0))
     ((eq gc :end)
      (gc_end 0))
     ((eq action :freeze)
      (send-to-podium (format nil "tsdb_busy freeze") :wait t))
     ((eq action :release)
      (send-to-podium (format nil "tsdb_busy release") :wait t)))))

(defun beep ()
  (send-to-podium "tsdb_beep" :wait t))
