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

(defun tsdb-do-process (data
                        &key condition
                             run-id comment
                             (verbose t)
                             output
                             (cache *tsdb-cache-database-writes-p*)
                             (gc *tsdb-gc-p*)
                             (stream *tsdb-io*)
                             overwrite interactive 
                             meter podium interrupt)
  (declare (ignore podium))
  
  (initialize-tsdb)
  
  (when (and overwrite (not interactive))
    (purge-test-run data :action :purge))

  (let* ((stream (cond
                  (output (create-output-stream output))
                  (interactive (make-string-output-stream))
                  (t stream)))
         (*tsdb-gc-message-p* nil)
         (condition (if (equal condition "") nil condition))
         (imessage (format nil "preparing `~a' test run ..." data))
         (pmessage (format nil "processing `~a' test run ..." data))
         (imeter (when meter (madjust * meter 0.02)))
         (pmeter (when meter
                   (madjust + (madjust * meter 0.98) (mduration imeter))))
         items abort)

    (when meter
      (meter :value (get-field :start imeter))
      (status :text imessage))

    (when (setf items (retrieve condition data :verbose verbose))
      (let* ((cache (when (and cache (not interactive))
                      (create-cache data :verbose verbose)))
             (run-id 
              (or run-id 
                  (unless interactive
                    (+ (largest-run-id data :verbose verbose) 1))))
             (parse-id 
              (unless interactive
                (+ (largest-parse-id run-id data :verbose verbose) 1)))
             (runs (create-test-runs 
                    data run-id 
                    :comment comment :gc gc 
                    :tasks (when (find-symbol "*PVM-TASKS*")
                             (symbol-value (find-symbol "*PVM-TASKS*")))
                    :interactive interactive))
             (burst (and (not interactive)
                         (find :pvm *features*)
                         (every #'(lambda (run)
                                    (get-field :task run))
                                runs)))
             (trees-hook 
              (unless interactive
                (typecase *tsdb-trees-hook*
                  (null nil)
                  (string (symbol-function 
                           (read-from-string *tsdb-trees-hook*)))
                  (symbol (symbol-function *tsdb-trees-hook*))
                  (function *tsdb-trees-hook*))))
             (semantix-hook 
              (unless interactive
                (typecase *tsdb-semantix-hook*
                  (null nil)
                  (string (symbol-function 
                           (read-from-string *tsdb-semantix-hook*)))
                  (symbol (symbol-function *tsdb-semantix-hook*))
                  (function *tsdb-semantix-hook*))))
             (increment (when meter (/ (mduration pmeter) (length items)))))
        
        (when meter 
          (status :text pmessage)
          (meter :value (get-field :start pmeter)))
        
        (unwind-protect
            (ignore-errors
             (catch :break
               (loop
                   until (or (null runs)
                             (and (not burst) (null items))
                             (and (null items)
                                  (null (find-if #'consp runs 
                                                 :key #'run-status))))
                   with result = nil
                   with item = nil
                   for i = (or parse-id 0) then (+ i 1)
                   for run = (if burst
                               (find-if #'task-idle-p runs
                                        :key #'(lambda (run)
                                                 (get-field :task run)))
                               (first runs))
                                                            
                   do
                     (when (interrupt-p interrupt)
                       (setf abort t)
                       (format
                        stream
                        "do-process(): abort on external interrupt signal.~%")
                       (force-output stream)
                       (throw :break nil))
                     
                     (when (or (null run) (null items))
                       (let* ((status (process-queue runs :stream stream))
                              (corpses (get-field :corpses status))
                              (pending (get-field :pending status))
                              (ready (get-field :ready status))
                              (item (get-field :item status))
                              (result (get-field :result status)))
                         (when (and item result)
                           (when verbose
                             (print-item item :stream stream :result result)
                             (print-result result :stream stream))
                           (unless interactive
                             (store-result data result :cache cache))
                           (when ready (incf (get-field :items ready))))
                         (when corpses
                           (loop
                               for corpse in corpses
                               do (setf runs (delete corpse runs))))
                         (when pending
                           (setf items (append pending items)))
                         (if ready 
                           (setf run ready)
                           (throw :break nil))))

                     (setf item 
                       (when run
                         (enrich-item run (pop items)
                                      :verbose verbose :stream stream)))
                     (when item
                       (when (and verbose (not burst))
                         (print-item item :stream stream))
                       (setf result
                         (process-item item
                                       :run-id (get-field :run-id run)
                                       :parse-id i
                                       :trees-hook trees-hook 
                                       :semantix-hook semantix-hook
                                       :stream stream
                                       :task (get-field :task run)
                                       :interactive interactive))
                       (cond
                        ((eq result :ok))
                        ((eq result :error)
                         (setf runs (delete run runs)))
                        ((consp result)
                         (when verbose
                           (print-result result :stream stream))
                         (unless interactive
                           (store-result data result :cache cache))
                         (incf (get-field :items run)))))

                     (when increment (meter-advance increment)))))
                     
          (when interactive
            (format *tsdb-io* "~&~a" (get-output-stream-string stream)))
          (complete-test-runs data runs :cache cache :interactive interactive)
          (when cache (flush-cache cache :verbose verbose))
          (unless interactive (format stream "~&~%"))
          (if abort
            (status :text (format nil "~a interrupt" pmessage) :duration 5)  
            (when meter
              (status :text (format nil "~a done" pmessage) :duration 5)
              (meter :value (get-field :end meter)))))))))

(defun create-test-runs (data run-id &key comment gc 
                                          tasks interactive)
  (if tasks
    (let ((tasks (remove-if-not #'task-idle-p tasks))
          runs)
      (when tasks
        (loop
            for task in tasks
            for tid = (task-tid task)
            for i = run-id then (+ i 1)
            for status = (revaluate 
                          tid 
                          `(create-test-run 
                            ,data ,i 
                            :comment ,comment :gc ,gc 
                            :interactive nil)
                          nil
                          :key :create-test-runs)
            when (eq status :ok)
            do
              (setf (task-status task) :create)
              (push (list (cons :task task)) runs)
            else 
            do
              (setf (task-status task) :error)
              (setf tasks (delete task tasks))
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-test-runs(): ~
                  transmission error; disabling <~d>.~%"
                 tid)
                (force-output)))
        (loop
            while (and tasks
                       (not (find :ready tasks :key #'task-status)))
            finally (return runs)
            for message = (pvm_poll -1 -1 5)
            when (message-p message)
            do
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-test-runs(): got message:~% `~a'~%"
                 message)
                (force-output))
              (let* ((tag (message-tag message))
                     (remote (message-remote message))
                     (content (message-content message))
                     (task (find remote tasks :key #'task-tid)))
                (cond
                 ((eql tag %pvm_task_fail%)
                  (let* ((remote (message-corpse message))
                         (task (find remote tasks :key #'task-tid)))
                    (setf tasks (delete task tasks))))
                 
                 ((null task)
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-test-runs(): ~
                      ignoring message from alien <~d>.~%"
                     remote)
                    (force-output)))

                 ((eql tag %pvm_lisp_message%)
                  (if (and (eq (first content) :return)
                           (eq (second content) :create-test-runs))
                    (let ((stub (find task runs 
                                      :key #'(lambda (run)
                                               (get-field :task run))))
                          (run (third content)))
                      (when (and stub (consp run))
                        (nconc stub run)
                        (setf (task-status task) :ready)))
                    (when *pvm-debug-p*
                      (format
                       t
                       "~&create-test-runs(): ~
                        ignoring unexpected message from <~d>.~%"
                       remote)
                      (force-output))))

                 (t
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-test-runs(): ~
                      ignoring dubious message from <~d>.~%"
                     remote)
                    (force-output))))))))
    
    (list (create-test-run data run-id 
                           :comment comment :gc gc 
                           :interactive interactive))))

(defun create-test-run (data run-id &key comment gc interactive)
  (let* ((environment (initialize-test-run :interactive interactive))
         (gc-strategy (unless interactive (install-gc-strategy gc)))
         (gc (get-field :gc gc-strategy))
         (user (current-user))
         (comment (or comment "null"))
         (platform (current-platform))
         (grammar (current-grammar))
         (tsdb (current-tsdb))
         (host (current-host))
         (os (current-os))
         (start (current-time :long t))
         (run (pairlis (list :data :run-id :comment 
                             :platform :tsdb :grammar
                             :user :host :os :start :items
                             :environment :gc-strategy :gc)
                       (list data run-id comment 
                             platform tsdb grammar
                             user host os start 0
                             environment gc-strategy gc))))
    (append run (get-test-run-information))))

(defun enrich-item (run item &key verbose stream)
  
  (when item
    (let* ((i-id (get-field :i-id item)) 
           (i-input (get-field :i-input item))
           (i-wf (get-field :i-wf item))
           (o-ignore (get-field :o-ignore item))
           (o-ignore 
            (and o-ignore (not (equal o-ignore "")) o-ignore)))

      (cond 
       ((and o-ignore (tsdb-ignore-p))
        (when verbose
          (format
           stream
           "~&(~a) `~:[*~;~]~a' --- skip (`o-ignore' is `~a').~%"
           i-id (= i-wf 1) i-input o-ignore)
          (force-output stream))
        nil)
       (t
        (let* ((o-gc (get-field :o-gc item))
               (o-gc (cond ((eql o-gc 0) nil)
                           ((eql o-gc 1) :local)
                           ((eql o-gc 2) :global)))
               (gc (or (get-field :gc item) o-gc (get-field :gc run)))
               (o-edges (get-field :o-edges item))
               (edges (or (get-field :edges item)
                          (if (and o-edges (not (= o-edges -1)))
                            (floor (* *tsdb-edge-factor* o-edges))
                            *tsdb-maximal-number-of-edges*))))
          (append (pairlis '(:gc :edges) (list gc edges)) item)))))))

(defun print-item (item &key (stream *tsdb-io*) result)
  
  (let* ((i-id (get-field :i-id item)) 
         (i-input (get-field :i-input item))
         (i-wf (get-field :i-wf item))
         (gc (get-field :gc item))
         (edges (get-field :edges item))
         (host (get-field :host result))
         (host (if (and host (stringp host)) 
                 (subseq host 0 (position #\. host))
                 host))
         (load (get-field :a-load result)))

    (if (and host load)
      (format stream "~&[~a ~,1f] " host load)
      (format stream "~&"))
    (format 
     stream 
     "(~a) `~:[*~;~]~a' ~:[~;:~]~:[~;=~][~a]"
     i-id (= i-wf 1) i-input 
     (eq gc :local)  (eq gc :global) edges)
    (force-output stream)))

(defun process-item (item &key (run-id 0) (parse-id 0)
                               trees-hook semantix-hook 
                               (stream *tsdb-io*)
                               (verbose t)
                               task
                               interactive burst)

  (if task

    (let* ((tid (task-tid task))
           (status (revaluate 
                    tid 
                    `(process-item
                      (quote ,item)
                      :run-id ,run-id :parse-id ,parse-id
                      :trees-hook ,trees-hook :semantix-hook ,semantix-hook
                      :verbose nil :interactive nil :burst t)
                    nil
                    :key :process-item
                    :verbose nil)))
      (case status
        (:ok (setf (task-status task) item) :ok)
        (:error (setf (task-status task) :error) :error)))

    (let* ((i-id (get-field :i-id item)) 
           (i-input (get-field :i-input item))
           (gc (get-field :gc item))
           (edges (get-field :edges item))
           result gcs i-load)

      (case gc
        (:local #+:allegro (excl:gc))
        (:global #+:allegro (excl:gc) #+:allegro (excl:gc t)))
      (setf *tsdb-global-gcs* 0)
      (setf i-load (unless interactive 
                     (first (load-average))))
      (setf result (parse-item i-input :edges edges
                               :trace interactive
                               :exhaustive *tsdb-exhaustive-p*
                               :trees-hook trees-hook
                               :semantix-hook semantix-hook
                               :burst burst))
      (when (and (not *tsdb-minimize-gcs-p*) 
                 (not interactive)
                 (eq gc :global)
                 (>= *tsdb-global-gcs* 1)
                 (<= *tsdb-global-gcs* 3))
        (when verbose
          (format 
           stream
           " (~d gc~:p);~%" *tsdb-global-gcs*)
          (force-output stream))
        (setf gc :global)
        (setf (get-field :gc item) :global)
        #+:allegro (excl:gc t)
        (when verbose
          (print-item item :stream stream))
        (setf *tsdb-global-gcs* 0)
        (setf i-load (first (load-average)))
        (setf result (parse-item i-input :edges edges
                                 :trace interactive
                                 :exhaustive *tsdb-exhaustive-p*
                                 :trees-hook trees-hook
                                 :semantix-hook semantix-hook
                                 :burst burst)))
      (setf gcs *tsdb-global-gcs*)
      (setf *tsdb-global-gcs* 0)

      #+:allegro
      (when (and (= (get-field :readings result) -1)
                 (equal (class-of 
                         (get-field :condition result))
                        (find-class 'excl:interrupt-signal)))
        (when verbose
          (format 
           stream 
           "~&do-process(): abort on keyboard interrupt signal.~%")
          (force-output stream))
        (throw :break nil))

      (let* ((readings (get-field :readings result))
             (treal (/ (get-field+ :treal result 0) 1000))
             (timeup (get-field :timeup result))
             (loads (load-average))
             (a-load
              (cond ((null treal) (first loads))
                    ((<= treal 60) (first loads))
                    ((<= treal (* 5 60)) (second loads))
                    (t (third loads)))))
        (push (cons :i-load i-load) result)
        (push (cons :a-load a-load) result)
        (push (cons :parse-id parse-id) result)
        (push (cons :run-id run-id) result)
        (push (cons :i-id i-id) result)
        (push (cons :gc gc) result)
        (push (cons :gcs gcs) result)
        (when (and timeup (not (= readings -1)))
          (push (cons :error (if (stringp timeup) timeup "timeup")) result))
        #+:page
        (unless (or interactive (= readings -1))
          (let ((statistics (pg::summarize-rules)))
            (when statistics
              (push (cons :statistics statistics) result)))))
      result)))

(defun process-queue (runs &key (verbose t) (stream *tsdb-io*))

  (loop
      while runs
      with corpses = nil
      with pending = nil
      for message = (pvm_poll -1 -1 5)
      when (message-p message)
      do
        (when *pvm-debug-p*
          (format
           t
           "~&process-queue(): got message:~% `~a'~%"
           message)
          (force-output))
        (let* ((tag (message-tag message))
               (remote (message-remote message))
               (content (message-content message))
               (run (find remote runs 
                          :key #'(lambda (run) 
                                   (task-tid (get-field :task run)))))
               (task (get-field :task run))
               (item (when task (task-status task))))

        (cond
         ((eql tag %pvm_task_fail%)
          (let* ((remote (message-corpse message))
                 (run (find remote runs 
                            :key #'(lambda (run) 
                                     (task-tid (get-field :task run)))))
                 (task (get-field :task run))
                 (item (when task (task-status task))))
            (when (consp item) (push item pending))
            (when (and run task)
              (setf (task-status task) :exit)
              (push run corpses)
              (setf runs (delete run runs)))))
                 
         ((null run)
          (when verbose
            (format
             stream
             "~&process-queue(): ignoring message from alien <~d>.~%"
             remote)
            (force-output stream))
          (push message *pvm-pending-events*))
         
         ((eql tag %pvm_lisp_message%)
          (if (eq (first content) :return)
            (case (second content)
              (:process-item
               (let* ((result (third content))
                      (host (cpu-host (task-cpu task))))
                 (setf (task-status task) :ready)
                 (return-from process-queue
                   (pairlis '(:corpses :pending :ready 
                              :item :result)
                            (list corpses pending run 
                                  item (acons :host host result))))))
              (:create-test-runs
               (let ((ready (third content)))
                 (when (consp ready)
                   (setf (task-status task) :ready)
                   (return-from process-queue
                     (pairlis '(:corpses :pending :ready)
                              (list corpses pending (nconc run ready)))))))
              (t
               (when verbose
                 (format
                  stream
                  "~&process-queue(): ~
                   ignoring unexpected message from <~d>.~%"
                  remote)
                 (force-output stream))))
            (when verbose
              (format
               stream
               "~&process-queue(): ~
                ignoring dubious message from <~d>.~%"
               remote message)
              (force-output stream))))

         (t
          (when verbose
            (format
             stream
             "~&process-queue(): ~
             ignoring dubious message from <~d>.~%"
             remote message)))))))

(defun print-result (result &key (stream *tsdb-io*))
  (let* ((readings (get-field :readings result))
         (words (get-field :words result))
         (tcpu (/ (get-field+ :tcpu result 0) 1000))
         (tgc (/ (get-field+ :tgc result 0) 1000))
           (first (/ (get-field+ :first result 0) 1000))
         (total (/ (get-field+ :total result 0) 1000))
         (aedges (get-field :aedges result))
         (pedges (get-field :pedges result))
         (edges (if (and aedges pedges)
                  (+ aedges pedges)
                  -1))
         (timeup (get-field :timeup result))
         (unifications (get-field :unifications result))
         (copies (get-field :copies result))
         (gc (get-field :gc result))
         (gcs (get-field :gcs result)))

    (if readings
      (case readings
        (0 (format 
            stream 
            " ---~:[~; time up:~] ~
             (~,1f~:[~*~;:~,1f~]|~,1f s) ~
             <~d:~d>~
             ~:[ {~d:~d}~;~2*~] ~
             (~a) [~:[~;=~]~d].~%" 
            timeup 
            tcpu (>= tgc 0.1) tgc total
            words edges 
            (or (= unifications copies 0)
                (= unifications copies -1))
            unifications copies 
            (pprint-memory-usage result) gc gcs))
        (-1 (format 
             stream 
             " --- error: ~a.~%" 
             (get-field :error result)))
        (t (format 
            stream 
            " ---~:[~; time up:~] ~a ~
             (~,1f~:[~*~;:~,1f~]|~,1f:~,1f s) ~
             <~d:~d>~
             ~:[ {~d:~d}~;~2*~] ~
             (~a) [~:[~;=~]~d].~%" 
            timeup readings 
            tcpu (>= tgc 0.1) tgc first total 
            words edges 
            (or (= unifications copies 0)
                (= unifications copies -1))
            unifications copies 
            (pprint-memory-usage result) gc gcs)))
      (format stream ".~%"))
    (force-output stream)))

(defun store-result (data result &key cache)
  (let* ((parse-id (get-field :parse-id result))
         #+:page (statistics (get-field :statistics result)))
    
    (write-parse result data :cache cache)
    (unless (= (get-field :readings result) -1)
      (write-results
       parse-id 
       (get-field :results result) data
       :cache cache)
      #+:page
      (when statistics
        (write-rules
         parse-id
         statistics
         data
         :cache cache)))))

(defun complete-test-runs (data runs &key cache interactive)
  (loop
      for run in runs
      for end = (complete-test-run run)
      when (> (length run) 1)
      do
        (push (cons :end (or end (current-time :long t))) run)
        (unless interactive (write-run run data :cache cache))))

(defun complete-test-run (run)
  (when (> (length run) 1)
    (let ((environment (get-field :environment run))
          (gc-strategy (get-field :gc-strategy run))
          (task (get-field :task run)))
      (cond
       ((and task (task-p task))
        (let* ((tid (task-tid task))
               (status (revaluate 
                        tid 
                        `(complete-test-run
                          (quote ,(pairlis '(:environment :gc-strategy)
                                           (list environment gc-strategy))))
                        5
                        :key :complete-test-run
                        :verbose nil)))
          (if (stringp status)
            (setf (task-status task) :ready)
            (setf (task-status task) :error))
          status))
       (t
        (finalize-test-run environment)
        (when gc-strategy (restore-gc-strategy gc-strategy))
        (current-time :long t))))))

(defun interrupt-p (interrupt)
  (when (and interrupt (probe-file interrupt))
    (delete-file interrupt)
    t))
