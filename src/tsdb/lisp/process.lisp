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
                             (verbose *tsdb-verbose-processing-p*)
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
                   (find-symbol "*PVM-CLIENTS*")
                   (symbol-value (find-symbol "*PVM-CLIENTS*"))))
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
      (let* ((schema (read-database-schema data))
             (cache (when (and cache (not interactive))
                      (create-cache data 
                                    :schema schema :verbose verbose 
                                    :protocol cache)))
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
                    :clients (when (find-symbol "*PVM-CLIENTS*")
                               (symbol-value (find-symbol "*PVM-CLIENTS*")))
                    :interactive interactive :verbose verbose
                    :stream stream :interrupt interrupt))
             (burst (and (not interactive)
                         (find :pvm *features*)
                         (every #'(lambda (run)
                                    (get-field :client run)) runs)))
             (*tsdb-trees-hook*
              (unless interactive
                (if burst
                  *tsdb-trees-hook*
                  (ignore-errors
                   (typecase *tsdb-trees-hook*
                     (null nil)
                     (string (symbol-function 
                              (read-from-string *tsdb-trees-hook*)))
                     (symbol (symbol-function *tsdb-trees-hook*))
                     (function *tsdb-trees-hook*))))))
             (*tsdb-semantix-hook*
              (unless interactive
                (if burst
                  *tsdb-semantix-hook* 
                  (ignore-errors
                   (typecase *tsdb-semantix-hook*
                     (null nil)
                     (string (symbol-function 
                              (read-from-string *tsdb-semantix-hook*)))
                     (symbol (symbol-function *tsdb-semantix-hook*))
                     (function *tsdb-semantix-hook*))))))
             (*tsdb-result-hook*
              (unless interactive
                (ignore-errors
                 (typecase *tsdb-result-hook*
                   (null nil)
                   (string (symbol-function 
                            (read-from-string *tsdb-result-hook*)))
                   (symbol (symbol-function *tsdb-result-hook*))
                   (function *tsdb-result-hook*)))))
             (increment (when meter (/ (mduration pmeter) (length items)))))
        
        (when meter 
          (status :text (format nil "~a done" imessage)))
        (sleep 0.5)
        (when meter
          (status :text pmessage)
          (meter :value (get-field :start pmeter)))
        (when (and burst runs)
          (install-gc-strategy nil :tenure nil :verbose verbose))
        
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
                         ~:[~*~; (~d active client~:p)~].~%"
                        busy (length busy))
                       (force-output stream)
                       (loop
                           for run in runs
                           do (nconc run (acons :status :interrupt nil)))
                       (setf items nil)
                       (setf abort t))

                     (pre-process :burstp burst)
                     
                     (when (and burst busy (or (null run) (null items)))
                       (let* ((status (process-queue 
                                       runs 
                                       :stream stream
                                       :interrupt interrupt))
                              (pending (get-field :pending status))
                              (ready (get-field :ready status))
                              (item (get-field :item status))
                              (result (get-field :result status))
                              (interrupt (get-field :interrupt status)))
                         (when result
                           (setf result 
                             (enrich-result result :verbose verbose)))
                         (when (and item result)
                           (when verbose
                             (print-item item :stream stream :result result)
                             (print-result result :stream stream))
                           (unless interactive
                             (store-result data result :cache cache))
                           (when ready (incf (get-field :items ready)))
                           (when increment (meter-advance increment)))
                         (when (and pending (null abort) (null interrupt))
                           (setf items (append pending items)))
                         (cond
                          ((and interrupt abort)
                           (loop
                               with end = (current-time :long t)
                               for run in runs
                               when (get-field :status run) do
                                 (setf (get-field :status run) :abort)
                               else do
                                 (nconc run (acons :status :abort nil))
                               do
                                 (nconc run (acons :end end nil)))
                           (throw :break nil))
                          (interrupt
                           (loop
                               for run in runs
                               do (nconc run (acons :status :interrupt nil)))
                           (setf items nil)
                           (setf abort t))
                          ((null ready)
                           (throw :break nil))
                          (ready
                           (setf run ready)))))

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
                                       :trees-hook *tsdb-trees-hook* 
                                       :semantix-hook *tsdb-semantix-hook*
                                       :stream stream
                                       :client (get-field :client run)
                                       :interactive interactive))
                       (cond
                        ((eq result :ok))
                        ((eq result :error)
                         (let ((client (get-field :client run))
                               (end (current-time :long t)))
                           (when client (setf (client-status client) :error))
                           (nconc run `((:end . ,end)))))
                        ((consp result)
                         (setf result (enrich-result result :verbose verbose))
                         (when verbose
                           (print-result result :stream stream))
                         (unless interactive
                           (store-result data result :cache cache))
                         (incf (get-field :items run))
                         (when increment (meter-advance increment)))))
                     (post-process :burstp burst))))
                     
          (when interactive
            (format *tsdb-io* "~&~a" (get-output-stream-string stream)))
          (complete-runs 
           data runs 
           :cache cache :interactive interactive 
           :stream stream :interrupt interrupt)
          (when cache (flush-cache cache :verbose verbose))
          (unless interactive (format stream "~&~%"))
          (when meter
            (if abort
              (status :text (format nil "~a interrupt" pmessage) :duration 5)
              (status :text (format nil "~a done" pmessage) :duration 5))
            (meter :value (get-field :end meter))))))))

(defun create-runs (data run-id &key comment 
                                     gc (tenure *tsdb-tenure-p*)
                                     clients interactive verbose
                                     stream interrupt)
  (if (and clients (null interactive))
    (let ((clients (remove-if-not #'client-idle-p clients))
          runs)
      (when clients
        (loop
            for client in clients
            for tid = (client-tid client)
            for cpu = (client-cpu client)
            for custom = (and (cpu-p cpu) (cpu-create cpu))
            for i = run-id then (+ i 1)
            for status = (if (eq (client-protocol client) :lisp)
                           (revaluate 
                            tid 
                            `(create-run 
                              ,data ,i 
                              :comment ,comment :gc ,gc :tenure ,tenure
                              :interactive nil :verbose ,verbose)
                            nil
                            :key :create-run)
                           (create_run tid data i comment interactive custom))
            when (eq status :ok)
            do
              (setf (client-status client) :create)
              (push (enrich-run (list (cons :client client))) runs)
            else 
            do
              (setf (client-status client) :error)
              (setf clients (delete client clients))
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-runs(): ~
                  transmission error; disabling <~d>.~%"
                 tid)
                (force-output)))
        (loop
            while (and clients
                       (not (find :ready clients :key #'client-status)))
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
                     (client (find remote clients :key #'client-tid)))
                (cond
                 ((eql tag %pvm_task_fail%)
                  (when client
                    (setf (client-status client) :exit)
                    (setf clients (delete client clients))
                    (when (and (client-p client) (cpu-p (client-cpu client)))
                      (format
                       *tsdb-io*
                       "~&create-runs(): client exit on `~a' <~a>.~%"
                       (cpu-host (client-cpu client)) remote)
                      (force-output *tsdb-io*))))
                 
                 ((null client)
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
                    (let* ((stub (find client runs 
                                       :key #'(lambda (run)
                                                (get-field :client run))))
                           (run (third content)))
                      (when (and stub (consp run))
                        (nconc stub run)
                        (setf (client-status client) :ready)
                        (setf (client-load client) load)))
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
                    (force-output)))))
            else when (interrupt-p interrupt)
            do
              (format
               stream
               "create-runs(): ~
                received external interrupt signal.~%")
              (force-output stream)
              (return-from create-runs runs))))
    
    (list (enrich-run (create-run data run-id 
                                  :comment comment :gc gc 
                                  :interactive interactive 
                                  :verbose verbose)))))

(defun create-run (data run-id &key comment 
                                    gc (tenure *tsdb-tenure-p*)
                                    (exhaustive *tsdb-exhaustive-p*)
                                    interactive verbose)
  
  (let* ((environment (initialize-test-run :interactive interactive 
                                           :exhaustive exhaustive))
         (start (current-time :long t))
         (gc-strategy (unless interactive 
                        (install-gc-strategy 
                         gc :tenure tenure :verbose verbose)))
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
    (gc-statistics-reset :all)
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
           (o-ignore (get-field :o-ignore item)))

      (cond 
       ((and o-ignore (tsdb-ignore-p o-ignore))
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
                               client
                               (exhaustive *tsdb-exhaustive-p*)
                               (derivationp *tsdb-write-passive-edges-p*)
                               interactive burst)

  (cond
   ((and client (client-p client))
    (let* ((tid (client-tid client))
           (status (if (eq (client-protocol client) :lisp)
                     (revaluate 
                      tid 
                      `(process-item
                        (quote ,item)
                        :trees-hook ,trees-hook 
                        :semantix-hook ,semantix-hook
                        :derivationp ,derivationp
                        :verbose nil :interactive nil :burst t)
                      nil
                      :key :process-item
                      :verbose nil)
                     (process_item 
                      tid item exhaustive derivationp interactive))))
      (case status
        (:ok (setf (client-status client) item) :ok)
        (:error (setf (client-status client) :error) :error))))
  
   ((null client)
    (let* ((run-id (get-field :run-id item))
           (parse-id (get-field :parse-id item))
           (i-id (get-field :i-id item)) 
           (i-input (get-field :i-input item))
           (gc (get-field :gc item))
           (edges (get-field :edges item))
           result i-load)

      (case gc
        (:local #+:allegro (excl:gc))
        (:global #+:allegro (excl:gc t)))
      (gc-statistics-reset)
      (setf i-load (unless interactive #+:pvm (load_average) #-:pvm nil))
      (setf result (parse-item i-input 
                               :edges edges
                               :trace interactive
                               :exhaustive exhaustive
                               :trees-hook trees-hook
                               :semantix-hook semantix-hook
                               :derivationp derivationp
                               :burst burst))
      (when (and (not *tsdb-minimize-gcs-p*) (not (eq gc :global))
                 (not interactive)
                 (>= (gc-statistics :global) 1) (<= (gc-statistics :global) 3))
        (when verbose
          (format 
           stream
           " (~d gc~:p);~%" (gc-statistics :global))
          (force-output stream))
        (setf (get-field :gc item) :global)
        #+:allegro (excl:gc t)
        (when verbose
          (print-item item :stream stream))
        (gc-statistics-reset)
        (setf i-load #+:pvm (load_average) #-:pvm nil)
        (setf result (parse-item i-input :edges edges
                                 :trace interactive
                                 :exhaustive exhaustive
                                 :trees-hook trees-hook
                                 :semantix-hook semantix-hook
                                 :derivationp derivationp
                                 :burst burst)))

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
             (timeup (get-field :timeup result))
             (comment (get-field+ :comment result ""))
             (global (gc-statistics :global))
             (scavenge (gc-statistics :scavenge))
             (new (gc-statistics :new))
             (old (gc-statistics :old))
             (total (length (gc-statistics :efficiency)))
             (efficiency (round (average (gc-statistics :efficiency))))
             (comment (format 
                       nil 
                       "~a (:global ~d) (:scavenge ~d) ~
                        (:new ~d) (:old ~d) ~
                        (:efficiency ~d) (:total ~d)"
                       comment global scavenge new old efficiency total))
             (a-load #+:pvm (load_average) #-:pvm nil))
        (push (cons :i-load i-load) result)
        (push (cons :a-load a-load) result)
        (push (cons :parse-id parse-id) result)
        (push (cons :run-id run-id) result)
        (push (cons :i-id i-id) result)
        (push (cons :gc gc) result)
        (push (cons :gcs (+ global scavenge)) result)
        (push (cons :comment comment) result)
        (when (and timeup (not (= readings -1)))
          (push (cons :error (if (stringp timeup) timeup "timeup")) result))
        #+:page
        (unless (or interactive (= readings -1))
          (let ((statistics (pg::summarize-rules)))
            (when statistics
              (push (cons :statistics statistics) result)))))
      result))))

(defun process-queue (runs &key (verbose t) (stream *tsdb-io*) interrupt)

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
               (client (get-field :client run))
               (item (and client (client-status client)))
               (host (and (client-p client) (cpu-p (client-cpu client))
                          (cpu-host (client-cpu client)))))

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
            (when (client-p client)
              (setf (client-status client) :exit))))
                 
         ((null run)
          (when verbose
            (format
             stream
             "~&process-queue(): ignoring message from alien <~d>.~%"
             remote)
            (force-output stream))
          (push message *pvm-pending-events*))
         
         ((eql tag %pvm_lisp_message%)
          (when client (setf (client-load client) load))
          (if (eq (first content) :return)
            (case (second content)
              (:process-item
               (let* ((result (third content)))
                 (setf (client-status client) :ready)
                 (return-from process-queue
                   (pairlis '(:pending :ready 
                              :item :result)
                            (list pending run 
                                  item (acons :host host result))))))
              (:create-run
               (let ((ready (third content)))
                 (when (consp ready)
                   (setf (client-status client) :ready)
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
             remote message)))))
      else when (interrupt-p interrupt)
      do
        (let ((busy (loop 
                        for run in runs 
                        when (consp (run-status run)) collect run)))
          (format
           stream
           "process-queue(): ~
            received external interrupt signal~
            ~:[~*~; (~d active client~:p)~].~%"
           busy (length busy))
          (force-output stream))
        (return-from process-queue
          (pairlis '(:pending :interrupt)
                   (list pending t)))))


(defun enrich-result (result &key verbose)
  (declare (ignore verbose))
  (let* ((readings (get-field :readings result)))
    (if (and *tsdb-result-hook* (> readings 0))
      (multiple-value-bind (wealth condition)
          (ignore-errors (funcall *tsdb-result-hook* result))
        (or wealth (acons :error (format nil "~a" condition) result)))
      result)))

(defun print-result (result &key (stream *tsdb-io*))
  (let* ((readings (get-field :readings result))
         (words (get-field :words result))
         (tcpu (/ (get-field+ :tcpu result 0) 1000))
         (tgc (/ (get-field+ :tgc result 0) 1000))
         (first (/ (get-field+ :first result 0) 1000))
         (total (/ (get-field+ :total result 0) 1000))
         (aedges (get-field :aedges result))
         (pedges (get-field :pedges result))
         (edges (if (and (integerp pedges) (not (= pedges -1)))
                  (if (and (integerp aedges) (not (= aedges -1)))
                    (+ aedges pedges)
                    pedges)
                  (and (integerp aedges) (not (= aedges -1)) aedges)))
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
       (normalize-string (get-field :error result))))
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
       " --- client exit <~d>.~%"
       corpse))
     ((null readings)
      (format stream ".~%"))
     (t
      (format stream ".~%")))
    (force-output stream)))

(defun store-result (data result &key cache)
  (let* ((parse-id (get-field :parse-id result))
         (readings (get-field :readings result))
         (statistics (get-field :statistics result)))
    
    (write-parse result data :cache cache)
    (unless (or (null readings) (= readings -1))
      (write-results
       parse-id 
       (get-field :results result) data
       :cache cache)
      (when statistics
        (write-rules
         parse-id
         statistics
         data
         :cache cache)))))

(defun pre-process (&key burstp)
  (declare (ignore burstp)))

(defun post-process (&key burstp)
  (declare (ignore burstp))
  #+:allegro
  (sys:gsgc-step-generation)
  #+:gcdebug
  (let ((*terminal-io* excl:*initial-terminal-io*)
        (*standard-output* excl:*initial-terminal-io*))
    (sys:gsgc-parameters)
    (room) (excl:gc) (room)))

(defun complete-runs (data runs &key cache interactive stream interrupt)
  (loop
      for run in runs
      for completion = (unless (get-field :end run)
                         (complete-run run stream interrupt))
      for status = (or (get-field :status run) (run-status run) :complete)
      when (get-field :run-id run)
      do
        (push (cons :status (format 
                             nil "~(~a~)" 
                             (if (eq status :ready) :complete status)))
              completion)
        (unless interactive 
          (write-run (append completion run) data :cache cache))))

(defun complete-run (run &optional stream interrupt)
  (when (get-field :run-id run)
    (let ((environment (get-field :environment run))
          (gc-strategy (get-field :gc-strategy run))
          (client (get-field :client run)))
      (cond
       ((and client (client-p client))
        (let* ((tid (client-tid client))
               (cpu (client-cpu client))
               (custom (and (cpu-p cpu) (cpu-complete cpu)))
               (status (if (eq (client-protocol client) :lisp)
                         (revaluate 
                          tid 
                          `(complete-run
                            (quote ,(pairlis '(:environment :gc-strategy)
                                             (list environment gc-strategy))))
                          1
                          :key :complete-run
                          :verbose nil
                          :interrupt interrupt)
                         (complete_run 
                          tid (get-field :run-id run) custom 1 interrupt))))
          (cond
           ((and (consp status) (get-field :end status))
            (setf (client-status client) :ready)
            status)
           ((eq status :interrupt)
            (format
             stream
             "complete-run(): ~
              received external interrupt signal.~%")
            (force-output stream)
            (setf (client-status client) :interrupt)
            (acons :end (current-time :long t) nil))
           (t
            (setf (client-status client) :error)
            (acons :end (current-time :long t) nil)))))
       (t
        (let ((finalization (finalize-test-run environment)))
          (when gc-strategy (restore-gc-strategy gc-strategy))
          (cons (cons :end (current-time :long t)) finalization)))))))

(defun tsdb-ignore-p (o-ignore)
  (unless (or (null o-ignore) (equal o-ignore ""))
    o-ignore))

(defun call-hook (hook &rest arguments)
  (when hook
    (let* ((hook (typecase hook
                   (null nil)
                   (function hook)
                   (symbol (and (fboundp hook) (symbol-function hook)))
                   (string (ignore-errors 
                            (symbol-function (read-from-string hook))))))
           (result (ignore-errors (apply hook arguments))))
      (typecase result
        (null nil)
        (string result)
        (t (format nil "~a" result))))))

(defun result-hook (result)
  (loop
      for parse in (get-field :results result)
      for derivation = (get-field :derivation parse)
      for tree = (get-field :tree parse)
      for mrs = (get-field :mrs parse)
      for edge = (when (and derivation
                            (or (and *tsdb-trees-hook* (null tree))
                                (and *tsdb-semantix-hook* (null mrs))))
                   (reconstruct derivation))
      when edge do
        (let ((tree (call-hook *tsdb-trees-hook* edge))
              (mrs (call-hook *tsdb-semantix-hook* edge)))
          (when tree
            (setf (rest parse)
              (cons (cons :tree tree) (rest parse))))
          (when mrs
            (setf (rest parse)
              (cons (cons :mrs mrs) (rest parse))))))
  result)

(defun interrupt-p (interrupt)
  (when (and interrupt (probe-file interrupt))
    (delete-file interrupt)
    t))

(defun run-status (run)
  (let ((client (get-field :client run)))
    (and client (client-status client))))

(defun run-tid (run)
  (let ((client (get-field :client run)))
    (and client (client-tid client))))
