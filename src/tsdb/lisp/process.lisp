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
                             vocabulary
                             (cache *tsdb-cache-database-writes-p*)
                             (gc *tsdb-gc-p*)
                             (stream *tsdb-io*)
                             overwrite interactive 
                             meter podium interrupt)
  (declare (ignore podium))
  
  (initialize-tsdb)
  
  (unless (or (null vocabulary) interactive
              (and (find :pvm *features*)
                   (find-symbol "*PVM-TASKS*")
                   (symbol-value (find-symbol "*PVM-TASKS*"))))
    (unless (tsdb-do-vocabulary data 
                                :condition condition :load :quiet 
                                :meter (make-meter 0 1)
                                :interrupt interrupt)
      (return-from tsdb-do-process)))
                   
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
             (runs (create-runs 
                    data run-id 
                    :comment comment :gc gc 
                    :tasks (when (find-symbol "*PVM-TASKS*")
                             (symbol-value (find-symbol "*PVM-TASKS*")))
                    :interactive interactive))
             (burst (and (not interactive)
                         (find :pvm *features*)
                         (every #'(lambda (run) (get-field :task run)) runs)))
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
          (status :text (format nil "~a done" imessage)))
        (sleep 0.5)
        (when meter
          (status :text pmessage)
          (meter :value (get-field :start pmeter)))

        (unwind-protect
            (ignore-errors
             (catch :break
               (loop
                   with result = nil
                   with item = nil
                   for busy = (loop
                                  for run in runs
                                  when (consp (run-status run))
                                  collect run)
                   for idle = (loop
                                  for run in runs
                                  when (eq (run-status run) :ready)
                                  collect run)
                   until (or (and (not burst) (null items))
                             (and burst (null busy) (null idle))
                             (and (null items) (null busy)))
                   for i = (or parse-id 0) then (+ i 1)
                   for run = (if burst (first idle) (first runs))
                                                            
                   do
                     (when (interrupt-p interrupt)
                       (format
                        stream
                        "do-process(): ~
                         received external interrupt signal~
                         ~:[~*~; (~d active task~:p)~].~%"
                        busy (length busy))
                       (when items
                         (loop
                             for run in runs
                             do (nconc run (acons :status :interrupt nil))))
                       (force-output stream)
                       (setf abort t)
                       (setf items nil))
                     
                     (when (and burst busy (or (null run) (null items)))
                       (let* ((status (process-queue runs :stream stream))
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
                           (when ready (incf (get-field :items ready)))
                           (when increment (meter-advance increment)))
                         (when pending
                           (unless abort (setf items (append pending items))))
                         (if ready 
                           (setf run ready)
                           (throw :break nil))))

                     (setf item 
                       (when run
                         (enrich-item run (pop items)
                                      :parse-id i
                                      :verbose verbose :stream stream)))
                     (when item
                       (when (and verbose (not burst))
                         (print-item item :stream stream))
                       (setf result
                         (process-item item
                                       :trees-hook trees-hook 
                                       :semantix-hook semantix-hook
                                       :stream stream
                                       :task (get-field :task run)
                                       :interactive interactive))
                       (cond
                        ((eq result :ok))
                        ((eq result :error)
                         (let ((task (get-field :task run))
                               (end (current-time :long t)))
                           (when task (setf (task-status task) :error))
                           (nconc run `((:end . ,end)))))
                        ((consp result)
                         (when verbose
                           (print-result result :stream stream))
                         (unless interactive
                           (store-result data result :cache cache))
                         (incf (get-field :items run))
                         (when increment (meter-advance increment))))))))
                     
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

(defun create-runs (data run-id &key comment 
                                     gc (tenure *tsdb-tenure-p*)
                                     tasks interactive)
  (if (and tasks (null interactive))
    (let ((tasks (remove-if-not #'task-idle-p tasks))
          runs)
      (when tasks
        (loop
            for task in tasks
            for tid = (task-tid task)
            for i = run-id then (+ i 1)
            for status = (if (eq (task-protocol task) :lisp)
                           (revaluate 
                            tid 
                            `(create-run 
                              ,data ,i 
                              :comment ,comment :gc ,gc :tenure ,tenure
                              :interactive nil)
                            nil
                            :key :create-run)
                           (create_run tid data i comment interactive))
            when (eq status :ok)
            do
              (setf (task-status task) :create)
              (push (enrich-run (list (cons :task task))) runs)
            else 
            do
              (setf (task-status task) :error)
              (setf tasks (delete task tasks))
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-runs(): ~
                  transmission error; disabling <~d>.~%"
                 tid)
                (force-output)))
        (loop
            while (and tasks
                       (not (find :ready tasks :key #'task-status)))
            finally (return runs)
            for message = (pvm_poll -1 -1 1)
            when (message-p message)
            do
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-runs(): got message:~% `~s'~%"
                 message)
                (force-output))
              (let* ((tag (message-tag message))
                     (load (message-load message))
                     (remote (if (eql tag %pvm_task_fail%)
                               (message-corpse message)
                               (message-remote message)))
                     (content (message-content message))
                     (task (find remote tasks :key #'task-tid)))
                (cond
                 ((eql tag %pvm_task_fail%)
                  (when task
                    (setf (task-status task) :exit)
                    (setf tasks (delete task tasks))
                    (when (and (task-p task) (cpu-p (task-cpu task)))
                      (format
                       *tsdb-io*
                       "~&create-runs(): client exit on `~a' <~a>.~%"
                       (cpu-host (task-cpu task)) remote)
                      (force-output *tsdb-io*))))
                 
                 ((null task)
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-runs(): ~
                      ignoring message from alien <~d>.~%"
                     remote)
                    (force-output)))

                 ((eql tag %pvm_lisp_message%)
                  (if (and (eq (first content) :return)
                           (eq (second content) :create-run))
                    (let* ((stub (find task runs 
                                       :key #'(lambda (run)
                                                (get-field :task run))))
                           (run (third content)))
                      (when (and stub (consp run))
                        (nconc stub run)
                        (setf (task-status task) :ready)
                        (setf (task-load task) load)))
                    (when *pvm-debug-p*
                      (format
                       t
                       "~&create-runs(): ~
                        ignoring unexpected message from <~d>.~%"
                       remote)
                      (force-output))))

                 (t
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-runs(): ~
                      ignoring dubious message from <~d>.~%"
                     remote)
                    (force-output))))))))
    
    (list (enrich-run (create-run data run-id 
                                  :comment comment :gc gc 
                                  :interactive interactive)))))

(defun create-run (data run-id &key comment 
                                    gc (tenure *tsdb-tenure-p*)
                                    interactive)
  
  (let* ((environment (initialize-test-run :interactive interactive))
         (start (current-time :long t))
         (gc-strategy (unless interactive 
                        (install-gc-strategy gc :tenure tenure)))
         (gc (get-field :gc gc-strategy))
         (user (current-user))
         (comment (or comment "null"))
         (platform (current-platform))
         (grammar (current-grammar))
         (host (current-host))
         (os (current-os))
         (run (pairlis (list :data :run-id :comment
                             :platform :grammar
                             :user :host :os :start
                             :environment :gc-strategy :gc)
                       (list data run-id comment
                             platform grammar
                             user host os start
                             environment gc-strategy gc))))
    (append run (get-test-run-information))))

(defun enrich-run (run)
  
  (let ((tsdb (current-tsdb)))
    (append (pairlis '(:items :tsdb) (list 0 tsdb)) run)))

(defun enrich-item (run item &key (parse-id 0) verbose stream)
  
  (when item
    (let* ((run-id (get-field :run-id run))
           (i-id (get-field :i-id item)) 
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
          (append 
           (pairlis '(:run-id :parse-id :gc :edges) 
                    (list run-id parse-id gc edges)) 
           item)))))))

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

    (if (or host load)
      (format stream "~&[~@[~a~]~@[ ~,1f~]] " host load)
      (format stream "~&"))

    (format 
     stream 
     "(~a) `~:[*~;~]~a' ~:[~;:~]~:[~;=~][~a]"
     i-id (= i-wf 1) i-input 
     (eq gc :local)  (eq gc :global) edges)
    (force-output stream)))

(defun process-item (item &key trees-hook semantix-hook 
                               (stream *tsdb-io*)
                               (verbose t)
                               task
                               (exhaustive *tsdb-exhaustive-p*)
                               interactive burst)

  (cond
   ((task-p task)
    (let* ((tid (task-tid task))
           (status (if (eq (task-protocol task) :lisp)
                     (revaluate 
                      tid 
                      `(process-item
                        (quote ,item)
                        :trees-hook ,trees-hook :semantix-hook ,semantix-hook
                        :verbose nil :interactive nil :burst t)
                      nil
                      :key :process-item
                      :verbose nil)
                     (process_item tid item exhaustive interactive))))
      (case status
        (:ok (setf (task-status task) item) :ok)
        (:error (setf (task-status task) :error) :error))))
  
   ((null task)
    (let* ((run-id (get-field :run-id item))
           (parse-id (get-field :parse-id item))
           (i-id (get-field :i-id item)) 
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
                               :exhaustive exhaustive
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
                                 :exhaustive exhaustive
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
      result))))

(defun process-queue (runs &key (verbose t) (stream *tsdb-io*))

  (loop
      while (find-if #'consp runs :key #'run-status)
      with pending = nil
      for message = (pvm_poll -1 -1 1)
      finally
        (return (pairlis '(:pending :ready) 
                         (list pending (find :ready runs :key #'run-status))))
      when (message-p message)
      do
        (when *pvm-debug-p*
          (format
           t
           "~&process-queue(): got message:~% `~s'~%"
           message)
          (force-output))
        (let* ((tag (message-tag message))
               (remote (if (eql tag %pvm_task_fail%)
                         (message-corpse message)
                         (message-remote message)))
               (load (message-load message))
               (content (message-content message))
               (run (find remote runs :key #'run-tid))
               (task (get-field :task run))
               (item (and task (task-status task)))
               (host (and (task-p task) (cpu-p (task-cpu task))
                          (cpu-host (task-cpu task)))))

        (cond
         ((eql tag %pvm_task_fail%)
          (when (consp item)
            (let* ((fail (pairlis '(:host :corpse) (list host remote))))
              (push item pending)
              (when verbose
                (print-item item :stream stream :result fail)
                (print-result fail :stream stream))))
          (when run
            (nconc run `((:end . ,(current-time :long t))))
            (when (task-p task)
              (setf (task-status task) :exit))))
                 
         ((null run)
          (when verbose
            (format
             stream
             "~&process-queue(): ignoring message from alien <~d>.~%"
             remote)
            (force-output stream))
          (push message *pvm-pending-events*))
         
         ((eql tag %pvm_lisp_message%)
          (when task (setf (task-load task) load))
          (if (eq (first content) :return)
            (case (second content)
              (:process-item
               (let* ((result (third content)))
                 (setf (task-status task) :ready)
                 (return-from process-queue
                   (pairlis '(:pending :ready 
                              :item :result)
                            (list pending run 
                                  item (acons :host host result))))))
              (:create-run
               (let ((ready (third content)))
                 (when (consp ready)
                   (setf (task-status task) :ready)
                   (return-from process-queue
                     (pairlis '(:pending :ready)
                              (list pending (nconc run ready)))))))
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
         (unifications (get-field+ :unifications result 0))
         (copies (get-field+ :copies result 0))
         (gc (get-field :gc result))
         (gcs (get-field :gcs result))
         (corpse (get-field :corpse result)))

    (cond
     ((eql readings 0)
      (format 
       stream 
       " ---~:[~; time up:~] ~
        (~,1f~:[~*~;:~,1f~]|~,1f s) ~
        <~d:~d>~
        ~:[ {~d:~d}~;~2*~] ~
        (~a)~
        ~:[~*~*~; [~:[~;=~]~d]~].~%" 
       timeup 
       tcpu (>= tgc 0.1) tgc total
       words edges 
       (or (= unifications copies 0)
           (= unifications copies -1))
       unifications copies 
       (pprint-memory-usage result) 
       gcs gc gcs))
     ((eql readings -1)
      (format 
       stream 
       " --- error: ~a.~%" 
       (get-field :error result)))
     ((and (integerp readings) (> readings 0))
      (format 
       stream 
       " ---~:[~; time up:~] ~a ~
        (~,1f~:[~*~;:~,1f~]|~,1f:~,1f s) ~
        <~d:~d>~
        ~:[ {~d:~d}~;~2*~] ~
        (~a)~
        ~:[~*~*~; [~:[~;=~]~d]~].~%" 
       timeup readings 
       tcpu (>= tgc 0.1) tgc first total 
       words edges 
       (or (= unifications copies 0)
           (= unifications copies -1))
       unifications copies 
       (pprint-memory-usage result) 
       gcs gc gcs))
     (corpse
      (format
       stream
       " --- task exit <~d>.~%"
       corpse))
     (t
      (format stream ".~%")))
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
      for end = (or (get-field :end run) (complete-test-run run))
      for status = (or (get-field :status run) (run-status run) :complete)
      when (> (length run) 1)
      do
        (push (cons :end end) run)
        (push (cons :status (format nil "~(~a~)" status)) run)
        (unless interactive (write-run run data :cache cache))))

(defun complete-test-run (run)
  (when (> (length run) 1)
    (let ((environment (get-field :environment run))
          (gc-strategy (get-field :gc-strategy run))
          (task (get-field :task run)))
      (cond
       ((task-p task)
        (let* ((tid (task-tid task))
               (status (if (eq (task-protocol task) :lisp)
                         (revaluate 
                          tid 
                          `(complete-test-run
                            (quote ,(pairlis '(:environment :gc-strategy)
                                             (list environment gc-strategy))))
                          1
                          :key :complete-test-run
                          :verbose nil)
                         (complete_test_run tid (get-field :run-id run) 1))))
          (cond
           ((stringp status)
            (setf (task-status task) :ready)
            status)
           (t
            (setf (task-status task) :error)
            (current-time :long t)))))
       (t
        (finalize-test-run environment)
        (when gc-strategy (restore-gc-strategy gc-strategy))
        (current-time :long t))))))

(defun interrupt-p (interrupt)
  (when (and interrupt (probe-file interrupt))
    (delete-file interrupt)
    t))
