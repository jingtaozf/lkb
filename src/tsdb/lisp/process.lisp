;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

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

(defparameter %process-run-id% -1)

(defparameter %accumulated-mt-statistics% nil)

(defparameter %graft-aligned-generation-hack% nil)

(defun tsdb-do-process (data
                        &key condition
                             run-id comment
                             (verbose *tsdb-verbose-processing-p*)
                             output
                             vocabulary
                             (cache *tsdb-cache-database-writes-p*)
                             (gc *tsdb-gc-p*)
                             (stream *tsdb-io*)
                             (type *process-default-task*) gold
                             overwrite interactive 
                             meter podium interrupt
                             (fan *process-fan-out-log*)
                             (xml *process-fan-out-xml*))
  (declare (ignore podium))
  
  (initialize-tsdb)

  (when (< (profile-granularity data) 200509)
    (format
     stream
     "~%tsdb-do-process(): out-of-date profile `~a'.~%"
     data)
    (return-from tsdb-do-process))
  
  (unless (or (null vocabulary) 
              (smember type '(:transfer :generate :translate))
              interactive
              (and (find :pvm *features*)
                   (find-symbol "*PVM-CLIENTS*" :tsdb)
                   (symbol-value (find-symbol "*PVM-CLIENTS*" :tsdb))))
    (unless (tsdb-do-vocabulary data 
                                :condition condition :load :quiet 
                                :meter (make-meter 0 1)
                                :interrupt interrupt)
      (return-from tsdb-do-process)))

  (purge-profile-cache data)
  (when (and overwrite (not interactive))
    (purge-test-run data :action :purge))

  (let* ((stream (cond
                  (output (create-output-stream output))
                  (interactive (make-string-output-stream))
                  (t stream)))
         (fan (or fan
                  (when (eq type :translate)
                    (merge-pathnames
                     (user-homedir-pathname)
                     (make-pathname 
                      :name (directory2file data)
                      :type "fan")))))
         (fstream (and fan (create-output-stream fan)))
         (xml (or xml
                  #+:null
                  (when (eq type :translate)
                    (merge-pathnames
                     (user-homedir-pathname)
                     (make-pathname 
                      :name (directory2file data)
                      :type "xml")))))
         (xstream (and xml (create-output-stream xml))) 
         (*tsdb-gc-message-p* nil)
         (condition (if (equal condition "") nil condition))
         (imessage (format nil "preparing `~a' test run ..." data))
         (pmessage (format nil "processing `~a' test run ..." data))
         (imeter (when meter (madjust * meter 0.02)))
         (pmeter (when meter
                   (madjust + (madjust * meter 0.98) (mduration imeter))))
         items abort %accumulated-rule-statistics%)
    (declare (special %accumulated-rule-statistics%))

    (when (eq type :translate)
      (setf %accumulated-mt-statistics%
        (pairlis '(:total
                   :pcount :pfcount :tcount :tfcount :rcount :rfcount
                   :tfbleu :tbbleu)
                 (list 0 0 0 0 0 0 0 0 0))))

    (when meter
      (meter :value (get-field :start imeter))
      (status :text imessage))

    (when (setf items (retrieve 
                       condition data 
                       :mrs (when (smember type '(:transfer :generate)) gold)
                       :verbose verbose))
      (let* ((schema (read-database-schema data))
             (cache (when (and cache (not interactive))
                      (create-cache data 
                                    :schema schema :verbose verbose 
                                    :protocol cache)))
             (%process-run-id%
              (or run-id 
                  (if interactive
                    0
                    (+ (largest-run-id data :verbose verbose) 1))))
             (parse-id 
              (unless interactive
                (+ (largest-parse-id
                    %process-run-id% data :verbose verbose) 1)))
             (clients (when (find-symbol "*PVM-CLIENTS*" :tsdb)
                        (symbol-value (find-symbol "*PVM-CLIENTS*" :tsdb))))
             (runs (create-runs
                    data
                    :type type
                    :comment comment :gc gc :clients clients
                    :interactive interactive :verbose verbose
                    :stream stream :interrupt interrupt))
             (burst (and (not interactive)
                         (find :pvm *features*)
                         (every #'(lambda (run)
                                    (get-field :client run)) runs)))
             ;;
             ;; _fix_me_
             ;; for right now, hard-wire exhaustive input processing when doing
             ;; transfer.                                        (4-mar-04; oe)
             ;;
             (*process-exhaustive-inputs-p*
              (if (eq type :transfer) 200 *process-exhaustive-inputs-p*))
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
          (install-gc-strategy nil :tenure nil :burst t :verbose verbose))
        
        (when fstream
          (format
           fstream
           ";;;~%;;; `~a'~%;;; fan-out batch (~a@~a; ~a).~%;;;~%~%"
           data (current-user) (current-host) (current-time :long :pretty)))
        
        (when xstream (xmlify-run :stream xstream))
        (unwind-protect
            (#-:debug ignore-errors #+:debug progn
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
                   for i = (or parse-id 1) then (+ i 1)
                   for run = (if burst (first idle) (first runs))
                                                            
                   do
                     (when (interrupt-p interrupt)
                       (format
                        stream
                        "do-process(): ~
                         external interrupt signal~
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
                             (enrich-result result item :verbose verbose)))
                         (when (and item result)
                           (when verbose
                             (print-item
                              item 
                              :stream stream :result result
                              :interactive interactive)
                             (print-result result :stream stream :log fstream))
                           (when xstream (xmlify-item result :stream xstream))
                           (unless interactive
                             (store-result data result :cache cache))
                           (when ready (incf (get-field :items ready)))
                           (when increment (meter-advance increment)))
                         (when (and pending (null abort) (null interrupt))
                           (setf items (append pending items)))
                         (cond
                          ((and interrupt abort)
                           (loop
                               with end = (current-time :long :tsdb)
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
                           (pushnew ready runs)
                           (setf run ready)))))

                     (setf item 
                       (when run
                         (enrich-item run (pop items)
                                      :type type :parse-id i
                                      :verbose verbose :stream stream)))
                     (when item
                       (when (and verbose (not burst))
                         (print-item
                          item :stream stream
                          :interactive interactive))
                       (setf result
                         (process-item item
                                       :type type
                                       :trees-hook *tsdb-trees-hook* 
                                       :semantix-hook *tsdb-semantix-hook*
                                       :stream stream
                                       :client (get-field :client run)
                                       :interactive interactive))
                       (cond
                        ((eq result :ok))
                        ((eq result :error)
                         (let ((client (get-field :client run))
                               (end (current-time :long :tsdb)))
                           (when client (setf (client-status client) :error))
                           (nconc run `((:end . ,end)))))
                        ((consp result)
                         (setf result 
                           (enrich-result result item :verbose verbose))
                         (when verbose
                           (print-result result :stream stream :log fstream))
                         (when xstream (xmlify-item result :stream xstream))
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
          (when cache 
            (flush-cache cache :verbose verbose 
                         :sort *process-sort-profile-p*))
          (purge-profile-cache data)
          (unless interactive (format stream "~&"))
          (when (and (stringp output) (open-stream-p stream))
            (close stream))
          (when (and (stringp fan) (open-stream-p fstream))
            (close fstream))
          (when (and (stringp xml) (open-stream-p xstream))
            (close xstream))
          (when meter
            (if abort
              (status :text (format nil "~a interrupt" pmessage) :duration 5)
              (status :text (format nil "~a done" pmessage) :duration 5))
            (meter :value (get-field :end meter))))))))

#+:pvm
(defun tsdb-do-listen (data
                        &key comment overwrite
                             (verbose *tsdb-verbose-processing-p*)
                             (reset t)
                             output
                             (cache *tsdb-cache-database-writes-p*)
                             (stream *tsdb-io*)
                             (file (format 
                                    nil 
                                    "/tmp/pvm.debug.~a"
                                    (current-user)))
                             status interrupt)
  
  (initialize-tsdb)
  (when reset                           
    (pvm_quit)
    (pvm_start :user (current-user))
    (sleep 2)
    (setf *pvm-master* (pvm_register file *pvm-debug-p*))
    (setf *pvm-clients* nil))

  (purge-profile-cache data)
  (when overwrite (purge-test-run data :action :empty))

  (let* ((stream (if output (create-output-stream output) stream))
         (*tsdb-gc-message-p* nil)
         (schema (read-database-schema data))
         (cache (when cache (create-cache data 
                                          :schema schema :verbose verbose 
                                          :protocol cache)))
         (*tsdb-trees-hook*
          (ignore-errors
           (typecase *tsdb-trees-hook*
             (null nil)
             (string (symbol-function 
                      (read-from-string *tsdb-trees-hook*)))
             (symbol (symbol-function *tsdb-trees-hook*))
             (function *tsdb-trees-hook*))))
         (*tsdb-semantix-hook*
          (ignore-errors
           (typecase *tsdb-semantix-hook*
             (null nil)
             (string (symbol-function 
                      (read-from-string *tsdb-semantix-hook*)))
             (symbol (symbol-function *tsdb-semantix-hook*))
             (function *tsdb-semantix-hook*))))
         (*tsdb-result-hook*
          (typecase *tsdb-result-hook*
            (null nil)
            (string (symbol-function 
                     (read-from-string *tsdb-result-hook*)))
            (symbol (symbol-function *tsdb-result-hook*))
            (function *tsdb-result-hook*)))
         (i-id (largest-i-id data :verbose verbose))
         (%process-run-id% (+ (largest-run-id data :verbose verbose) 1))
         (parse-id (largest-parse-id %process-run-id% data :verbose verbose))
         (run (pairlis (list :data :run-id :comment
                             :user :host :os :start)
                       (list data %process-run-id% comment
                             nil nil nil (current-time :long :tsdb))))
         (run (enrich-run run))
         (mode "passive mode (<Control-G> to abort)")
         (nitems 0)
         (%accumulated-rule-statistics% nil))
    (declare (special %accumulated-rule-statistics%))
    
    (when status
      (status 
       :text (format nil "entering ~a: listening for incoming data" mode)))
  
    (unwind-protect
      (ignore-errors
       (pvm::pvm_announce "itsdb" (current-tsdb) (current-user))
       (loop
           for message = (receive-item nitems
                                       :interrupt interrupt 
                                       :stream stream :verbose verbose)
           while (and message (get-field :data message))
           do
             (incf nitems) (incf i-id) (incf parse-id)
             (let* ((content (get-field :data message))
                    (user (get-field :user content))
                    (host (get-field :host content))
                    (os (get-field :os content))
                    (environment (get-field :run content))
                    (status
                     (format
                      nil
                      "passive mode (<Control-G> to abort): ~
                       captured ~d item~:p from `~a@~a'"
                      nitems user host))
                    (i-input (get-field :i-input content))
                    (o-input 
                     (when (find-attribute-reader :i-input)
                       (funcall (find-attribute-reader :i-input) i-input)))
                    (i-length (get-field :i-length content))
                    (item (pairlis '(:i-id :i-input :o-input 
                                     :i-wf :i-length :i-origin)
                                   (list i-id i-input o-input 
                                         1 i-length "captured")))
                    (item (enrich-item run item
                                       :parse-id parse-id
                                       :verbose verbose :stream stream))
                    (result (append 
                             (pairlis '(:run-id 
                                        :parse-id :i-id)
                                      (list (get-field :run-id run) 
                                            (get-field :parse-id item) i-id))
                             content))
                    (result (enrich-result result item)))
               (when status (status :text status))
               (setf (get-field :items run) nitems)
               (setf (get-field :user run) user)
               (setf (get-field :host run) host)
               (setf (get-field :os run) os)
               (when (= nitems 1)
                 (nconc run environment))
               (print-item item :stream stream :result result)
               (print-result result :stream stream)
               ;;
               ;; _fix_me_ create store-item() to go through the write .cache.
               ;;
               (insert data "item" (list item))
               (store-result data result :cache cache)
               (when (get-field :interrupt message) 
                 (format
                  stream
                  "tsdb-do-listen(): external interrupt signal ~
                   (captured ~d item~:p).~%"
                  nitems)
                 (return nil)))))

      (let* ((user (get-field :user run))
             (host (get-field :host run))
             (status (format
                      nil
                      "leaving passive mode ~
                       (captured ~d item~:p ~:[~2*~;from `~a@~a'~]) ..."
                      nitems (and user host) user host))
             pending abort)
        (pvm::pvm_retract "itsdb")
        (when status
          (status :text status :duration 10) (meter :value 0))
        (loop
            for message = (receive-item nitems
                                        :wait 2
                                        :interrupt interrupt 
                                        :stream stream :verbose verbose)
            while message
            when (get-field :interrupt message) do
              (setf abort t)
            else do (push (get-field :data message) pending))
        (if abort
          (when status
            (beep)
            (status :text (format
                           nil
                           "leaving passive mode: ~
                            ignoring ~d item~:p ~:[~2*~;from `~a@~a'~]"
                           nitems (and user host) user host)
                    :duration 10)
            (meter :value 1)
            (sleep 0.5))
          (loop
              with npending = (length pending)
              with increment = (unless (zerop npending)
                                 (/ 1 npending))
              initially
                (setf status (format
                              nil
                              "leaving passive mode ~
                               (~d item~:p pending ~
                               ~:[~2*~;from `~a@~a'~]) ..."
                              npending (and user host) user host))
                (when status (status :text status) (meter :value 0))
              finally
                (when status 
                  (status :text (format nil "~a done" status))
                  (meter :value 1)
                  (sleep 0.5))
              for content in pending
              for i-input = (get-field :i-input content)
              for o-input = (when (find-attribute-reader :i-input)
                              (funcall (find-attribute-reader :i-input) 
                                       i-input))
              for i-length = (get-field :i-length content)
              for foo = (pairlis '(:i-id :i-input :o-input 
                                   :i-wf :i-length :i-origin)
                                 (list i-id i-input o-input 
                                       1 i-length "captured"))
              for item = (enrich-item run foo
                                      :parse-id parse-id
                                      :verbose verbose :stream stream)
              for bar = (append 
                         (pairlis '(:run-id :parse-id :i-id)
                                  (list (get-field :run-id run) 
                                        (get-field :parse-id item) i-id))
                         content)
              for result = (enrich-result bar item)
              do
                (incf nitems) (incf i-id) (incf parse-id)
                (when increment (meter-advance increment))
                (setf (get-field :items run) nitems)
                (print-item item :stream stream :result result)
                (print-result result :stream stream)
                (insert data "item" (list item))
                (store-result data result :cache cache))))

      (let* ((run (append (pairlis '(:end :status)
                                   (list (current-time :long :tsdb) :capture))
                          run)))
        (complete-runs 
         data (list run)
         :cache cache :stream stream :interrupt interrupt)
        (when cache (flush-cache cache :verbose verbose))
        (format stream "~&~%")))))

(defun create-runs (data &key comment type
                              gc (tenure *tsdb-tenure-p*)
                              clients interactive verbose
                              (protocol *pvm-protocol*)
                              stream interrupt)
  
  (let ((clients (unless interactive
                   (loop
                       for client in clients
                       for cpu = (client-cpu client)
                       when (and cpu (client-idle-p client)
                                 (or (null type) (null (cpu-task cpu))
                                     (eq (cpu-task cpu) type)
                                     (smember type (cpu-task cpu))))
                       collect client)))
        tids)
    (if clients
      (let (runs)
        (loop
            for client in clients
            for tid = (client-tid client)
            for cpu = (client-cpu client)
            for custom = (and (cpu-p cpu) (cpu-create cpu))
            for status = (if (eq (client-protocol client) :lisp)
                           (revaluate 
                            tid 
                            `(create-run 
                              ,data ,%process-run-id%
                              :comment ,comment :gc ,gc :tenure ,tenure
                              :interactive nil :verbose ,verbose 
                              :protocol ,protocol :custom ,custom)
                            nil
                            :key :create-run)
                           (create_run 
                            tid data %process-run-id%
                            comment interactive protocol custom))
            when (eq status :ok)
            do
              (setf (client-status client) :create)
              (push (enrich-run (list (cons :client client))) runs)
              (push tid tids)
              (incf %process-run-id%)
            else 
            do
              (setf (client-status client) :error)
              (setf clients (delete client clients))
              (when *pvm-debug-p*
                (format
                 t
                 "~&create-runs(): ~
                  transmission error; disabling <~x>.~%"
                 tid)
                (force-output)))
        (loop
            while (and clients
                       (not (find :ready clients :key #'client-status)))
            finally (return runs)
            for message = (loop
                              for tid in tids
                              thereis (pvm_poll tid -1 1))
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
                       "~&create-runs(): client exit on `~a' <~x>.~%"
                       (client-host client) remote)
                      (force-output *tsdb-io*))))
                 
                 ((null client)
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-runs(): ~
                      ignoring message from alien <~x>.~%"
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
                        ignoring unexpected message from <~x>.~%"
                       remote)
                      (force-output))))

                 (t
                  (when *pvm-debug-p*
                    (format
                     t
                     "~&create-runs(): ~
                      ignoring dubious message from <~x>.~%"
                     remote)
                    (force-output)))))
            else when (interrupt-p interrupt)
            do
              (format
               stream
               "create-runs(): ~
                external interrupt signal.~%")
              (force-output stream)
              (return-from create-runs runs)))
      
      (let ((run-id %process-run-id%))
        (incf %process-run-id%)
        (list (enrich-run (create-run
                           data run-id
                           :comment comment :gc gc :interactive interactive 
                           :verbose verbose :protocol protocol)))))))

(defun create-run (data run-id 
                   &key comment 
                        gc (tenure *tsdb-tenure-p*)
                        (exhaustive *tsdb-exhaustive-p*)
                        (nanalyses *tsdb-maximal-number-of-analyses*)
                        interactive (protocol *pvm-protocol*) custom
                        verbose)
  
  (let* ((run (initialize-run :interactive interactive 
                              :exhaustive exhaustive
                              :nanalyses nanalyses
                              :protocol protocol :custom custom))
         (start (current-time :long :tsdb))
         (gc-strategy (unless interactive 
                        (install-gc-strategy 
                         gc :tenure tenure :verbose verbose)))
         (gc (get-field :gc gc-strategy))
         (user (current-user))
         (comment (or comment "null"))
         (platform (current-platform))
         (grammar (current-grammar))
         (host (current-host))
         (os (current-os)))
    (gc-statistics-reset :all)
    (nconc (pairlis (list :data :run-id :comment
                          :platform :grammar
                          :user :host :os :protocol :start
                          :gc-strategy :gc)
                    (list data run-id comment
                          platform grammar
                          user host os protocol start
                          gc-strategy gc))
           run)))

(defun enrich-run (run)
  
  (let ((tsdb (current-tsdb)))
    (append (pairlis '(:items :tsdb) (list 0 tsdb)) run)))

(defun enrich-item (run item &key (parse-id 0) type verbose stream)
  
  ;;
  ;; _fix_me_
  ;; over the years, we have accumulated multiple fields to record variants of
  ;; the :i-input field.  from what i can re-construct, :o-input was part of
  ;; the YY days, where (in passive mode) we captured strings in YY format; to
  ;; convert these into `plain' strings, yy-read-input() was (globally) set as
  ;; the reader for :i-input.  quite similarly, using a profile with inputs in
  ;; YY format (which should be passed to the parser as is), a per-cpu reader
  ;; can be provided to have the same effect.
  ;;
  ;; another relevant field is :p-input, which holds the pre-processor output,
  ;; i.e. what is actually sent to the parser.  this is independent from what
  ;; is used for logging (i.e. the print-out while processing), but there are
  ;; various parameters in print-item() that determine which variant to use.
  ;;
  ;; it used to be that the `reader' was always applied to the original string,
  ;; but to make it possible to pretty-print the results of a pre-processor, i
  ;; am now re-ordering things, so that a pre-processor and reader can cascade.
  ;; so far, we had /either/ a reader /or/ a pre-processor, hence this change
  ;; should not have effects on existing users.                 (24-sep-08; oe)
  ;;
  ;; when looking more at the `statistics attribute reader' machinery, it turns
  ;; out :o-input pre-dates the YY days: it is the field where the result of
  ;; applying an :i-input reader (if defined) is recorded for display purposes,
  ;; i.e. for everyone calling select() with a non-nil :readerp argument (this
  ;; includes the ubiquitous analyze(), which calls select() internally).  not
  ;; that i see reason to worry here, as the above comments apply to the batch
  ;; processing unverse, where the latter only applies to profile analysis; for
  ;; i can see the two universes are clearly separated, so a bit of overloading
  ;; the :o-input field should do us no harm.                   (13-nov-08; oe)
  ;;
  (when item
    (let* ((run-id (get-field :run-id run))
           (i-id (get-field :i-id item)) 
           (i-input (get-field :i-input item))
           (i-wf (get-field :i-wf item))
           (parse-id (if (>= parse-id i-id) parse-id i-id))
           (client (get-field :client run))
           (cpu (and client (client-cpu client)))
	   (tagger (when (cpu-p cpu) (cpu-tagger cpu)))
           (p-input (cond
                     ((and (cpu-p cpu) (cpu-preprocessor cpu))
                      (call-hook 
		       (cpu-preprocessor cpu) i-input 
		       (when (consp tagger) tagger)))
                     (*tsdb-preprocessing-hook*
                      (call-hook 
		       *tsdb-preprocessing-hook* i-input
		       (when (consp tagger) tagger)))))
           (p-input (when (and (stringp p-input) (not (string= p-input "")))
                      p-input))
	   (tags (cond
		  ((and (cpu-p cpu) (cpu-tagger cpu)
			(not (consp (cpu-tagger cpu))))
		   (call-hook (cpu-tagger cpu) i-input))
		  (*tsdb-tagging-hook*
		   (call-hook *tsdb-tagging-hook* i-input))))
           (o-input (or (get-field :o-input item)
                        (when (and (cpu-p cpu) (cpu-reader cpu))
                          (call-hook (cpu-reader cpu) (or p-input i-input)))))
           (o-input (when (and (stringp o-input) (not (string= o-input "")))
                      o-input))
           (strikes (get-field+ :strikes item 0)))

      (loop
          for output in (get-field :outputs item)
          for ignore = (get-field :o-ignore output)
          when (and ignore (tsdb-ignore-p ignore))
          do
            (when verbose
              (format
               stream
               "~&(~a) `~:[*~;~]~a' --- skip (`o-ignore' is `~a').~%"
               i-id (= i-wf 1) i-input ignore)
              (force-output stream))
            (return-from enrich-item))

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
         (pairlis '(:run-id :parse-id :type :strikes
                    :gc :edges :o-input :p-input :tags) 
                  (list run-id parse-id type strikes
                        gc edges o-input p-input tags)) 
         item)))))

(defun print-item (item &key (stream *tsdb-io*) result interactive)
  (declare (ignore interactive))
  
  (let* ((i-id (get-field :i-id item)) 
         (i-input (or (when *process-pretty-print-trace-p*
                        (get-field :o-input item))
                      (when *process-raw-print-trace-p*
                        (get-field :i-input item))
                      (get-field :p-input item)
                      (get-field :i-input item)))
         (i-wf (get-field+ :i-wf item 1))
         (gc (get-field :gc item))
         (edges (get-field :edges item))
         (type (get-field :type item))
         (inputs (when (smember type '(:transfer :generate))
                   (get-field :results item)))
         (host (get-field :host result))
         (host (if (and host (stringp host)) 
                 (subseq host 0 (position #\. host))
                 host))
         (load (get-field :a-load result))
         (strikes (let ((foo (get-field :strikes item)))
                    (and (numberp foo) (> foo 0) foo))))

    (if (or host load)
      (format stream "~&[~@[~a~]~@[ ~,1f~]] " host load)
      (format stream "~&"))

    (format 
     stream 
     "(~a) `~:[*~;~]~a' ~:[~;:~]~:[~;=~][~@[~a~]]"
     i-id (= i-wf 1) i-input 
     (eq gc :local)  (eq gc :global) edges)
    (when inputs
      (format stream " {~a}" (length inputs)))
    (when strikes
      (format stream " <~a>" strikes))
    (force-output stream)))

(defun process-item (item &key trees-hook semantix-hook 
                               (type :parse)
                               (stream *tsdb-io*)
                               (verbose t)
                               client
                               (exhaustive *tsdb-exhaustive-p*)
                               (nanalyses *tsdb-maximal-number-of-analyses*)
                               (nresults 
                                (if *tsdb-write-passive-edges-p*
                                  -1
                                  *tsdb-maximal-number-of-results*))
                               (filter *process-suppress-duplicates*)
                               result-id
                               interactive burst)

  (let ((strikes (get-field :strikes item)))
    (when (and (numberp strikes) (numberp *process-client-retries*)
               (> strikes *process-client-retries*))
      (when (and verbose
                 client (client-p client)
                 (smember type '(:parse :generate :translate)))
        (print-item item :stream stream :interactive interactive))
      (return-from process-item
        (pairlis '(:readings :error)
                 (list -1
                       (format
                        nil
                        "maximum number of strikes exhausted (~a)"
                        strikes))))))
        
  (cond
   ((and client
         (smember type '(:parse :transfer :generate :translate))
         (client-p client))
    ;;
    ;; adjust resource limits recorded in .item. according to cpu definition
    ;;
    (let* ((cpu (client-cpu client))
           (edges (cpu-edges cpu)))
      (when (numberp edges)
        (if (get-field :edges item)
          (setf (get-field :edges item) edges)
          (nconc item (acons :edges edges nil)))))
    (let* ((nanalyses (if exhaustive 
                        0 
                        (if (or (and (integerp nanalyses) (>= nanalyses 1))
                                (and (eq type :translate) (stringp nanalyses)))
                          nanalyses
                          1)))
           (trees-hook (and *tsdb-write-tree-p* trees-hook))
           (semantix-hook (and *tsdb-write-mrs-p* semantix-hook))
           (tid (client-tid client))
           (reader (find-attribute-reader :mrs))
           (mrs (when (smember type '(:transfer :generate))
                  (let* ((id
                          (if (numberp result-id)
                            result-id
                            (unless *process-exhaustive-inputs-p*
                              (loop
                                  for rank in (get-field :ranks item)
                                  when (eql (get-field :rank rank) 1)
                                  return (get-field :result-id rank)))))
                         (result
                          (when id
                            (loop
                                for result in (get-field :results item)
                                when (eql (get-field :result-id result) id)
                                return result)))
                         (mrs (get-field :mrs result)))
                    (if (and reader (stringp mrs))
                      (funcall reader mrs)
                      mrs))))
           (mrs (when mrs
                  (typecase mrs
                    (string mrs)
                    #+:lkb
                    (mrs::psoa
                     (with-output-to-string (stream)
                       (mrs::output-mrs1 mrs 'mrs::simple stream))))))
           (custom (rest (assoc type *process-custom*)))
           (status (if (eq (client-protocol client) :lisp)
                     (revaluate 
                      tid 
                      `(process-item
                        (quote ,item)
                        :type ,type
                        :trees-hook ,trees-hook 
                        :semantix-hook ,semantix-hook
                        :exhaustive ,exhaustive :nanalyses ,nanalyses
                        :nresults ,nresults :filter (quote ,filter)
                        :verbose nil :interactive nil :burst t)
                      nil
                      :key :process-item
                      :verbose nil)
                     (process_item 
                      tid (nconc item (acons :mrs mrs nil))
                      nanalyses nresults interactive custom))))
      (case status
        (:ok 
         (setf (client-status client) (cons (get-universal-time) item))
         :ok)
        (:error (setf (client-status client) :error) :error))))
  
   ((null client)
    (let* ((trees-hook (if (eq trees-hook :local)
                         *tsdb-trees-hook*
                         trees-hook))
           (semantix-hook (if (eq semantix-hook :local)
                            *tsdb-semantix-hook*
                            semantix-hook))
           (run-id (get-field :run-id item))
           (parse-id (get-field :parse-id item))
           (i-id (get-field :i-id item))
           (i-wf (get-field :i-wf item))
           (i-length (get-field :i-length item))
           (i-input (or (and interactive (get-field :o-input item))
                        (get-field :p-input item)
                        (get-field :i-input item)))
           (reader (find-attribute-reader :mrs))
           (mrs (when (smember type '(:transfer :generate))
                  (let* ((id
                          (if (numberp result-id)
                            result-id
                            (unless *process-exhaustive-inputs-p*
                              (loop
                                  for rank in (get-field :ranks item)
                                  when (eql (get-field :rank rank) 1)
                                  return (get-field :result-id rank)))))
                         (result
                          (when id
                            (loop
                                for result in (get-field :results item)
                                when (eql (get-field :result-id result) id)
                                return result)))
                         (mrs (get-field :mrs result))
                         (derivation (get-field :derivation result))
                         (edge (and derivation
                                    (ignore-errors (reconstruct derivation)))))
                    (when edge (setf %graft-aligned-generation-hack% edge))
                    (if (and reader (stringp mrs))
                      (funcall reader mrs)
                      mrs))))
           (targets (when (smember type '(:translate))
                      (loop
                          for output in (get-field :outputs item)
                          for surface = (get-field :o-surface output)
                          when (and (stringp surface)
                                    (not (string= surface "")))
                          collect surface)))
           (gc (get-field :gc item))
           (edges (get-field :edges item))
           result i-load)

      (case gc
        (:local #+:allegro (excl:gc))
        (:global #+:allegro (excl:gc t)))
      (gc-statistics-reset)
      (setf i-load (unless interactive #+:pvm (load_average) #-:pvm nil))
      (setf result 
        (if (and (smember type '(:transfer :generate))
                 (null mrs))
          ;;
          ;; _fix_me_
          ;; there appears to be some duplication of the MRS determination code
          ;; a little up, and of some of the processing calls further down; try
          ;; to clean this up one day.                          (18-sep-05; oe)
          ;;
          (loop
              for inputs in (get-field :results item)
              for i from 1 to (if (numberp *process-exhaustive-inputs-p*)
                                *process-exhaustive-inputs-p*
                                (length inputs))
              for mrs = (let ((mrs (get-field :mrs inputs)))
                          (if (and reader (stringp mrs))
                            (funcall reader mrs)
                            mrs))
              for result =
                (case type
                  (:transfer
                   (transfer-item mrs 
                                  :string i-input
                                  :edges edges
                                  :trace interactive
                                  :exhaustive exhaustive
                                  :nanalyses nanalyses
                                  :trees-hook trees-hook
                                  :semantix-hook semantix-hook
                                  :nresults nresults :filter filter
                                  :burst burst))
                  (:generate
                   (generate-item mrs 
                                  :string i-input
                                  :edges edges
                                  :trace interactive
                                  :exhaustive exhaustive
                                  :nanalyses nanalyses
                                  :trees-hook trees-hook
                                  :semantix-hook semantix-hook
                                  :nresults nresults :filter filter
                                  :burst burst)))
              when (let ((readings (get-field :readings result)))
                     (and (numberp readings) (> readings 0)))
              return result
              else collect result into results
              finally (return (first results)))
          (case type
            (:parse
             (parse-item i-input 
                         :edges edges
                         :trace interactive
                         :exhaustive exhaustive
                         :nanalyses nanalyses
                         :trees-hook trees-hook
                         :semantix-hook semantix-hook
                         :nresults nresults :filter filter
                         :burst burst))
            (:transfer
             (transfer-item mrs 
                            :string i-input
                            :edges edges
                            :trace interactive
                            :exhaustive exhaustive
                            :nanalyses nanalyses
                            :trees-hook trees-hook
                            :semantix-hook semantix-hook
                            :nresults nresults :filter filter
                            :burst burst))
            (:generate
             (generate-item mrs 
                            :string i-input
                            :edges edges
                            :trace interactive
                            :exhaustive exhaustive
                            :nanalyses nanalyses
                            :trees-hook trees-hook
                            :semantix-hook semantix-hook
                            :nresults nresults :filter filter
                            :burst burst))
            (:translate
             (translate-item i-input
                             :id i-id :wf i-wf :length i-length
                             :edges edges
                             :trace interactive
                             :exhaustive exhaustive
                             :nanalyses nanalyses
                             :trees-hook trees-hook
                             :semantix-hook semantix-hook
                             :nresults nresults :filter filter
                             :burst burst
                             :targets targets)))))
      ;;
      ;; this is a bit archaic: when between one or three global gc()s occured
      ;; during processing, redo it (unless we were told not to).  this goes
      ;; back to the days, where post-gc() cpu time (rehashing) would show as
      ;; a significant skewing fact and inhibit reliable timing measures.
      ;;
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
          (print-item item :stream stream :interactive interactive))
        (gc-statistics-reset)
        (setf i-load #+:pvm (load_average) #-:pvm nil)
        (setf result 
          (case type
            (:parse
             (parse-item i-input :edges edges
                         :trace interactive
                         :exhaustive exhaustive
                         :nanalyses nanalyses
                         :trees-hook trees-hook
                         :semantix-hook semantix-hook
                         :nresults nresults :filter filter
                         :burst burst))
            (:transfer
             (transfer-item mrs
                            :string i-input 
                            :edges edges
                            :trace interactive
                            :exhaustive exhaustive
                            :nanalyses nanalyses
                            :trees-hook trees-hook
                            :semantix-hook semantix-hook
                            :nresults nresults :filter filter
                            :burst burst))
            (:generate
             (generate-item mrs
                            :string i-input 
                            :edges edges
                            :trace interactive
                            :exhaustive exhaustive
                            :nanalyses nanalyses
                            :trees-hook trees-hook
                            :semantix-hook semantix-hook
                            :nresults nresults :filter filter
                            :burst burst))
            (:translate
             (translate-item i-input
                             :id i-id :wf i-wf
                             :edges edges
                             :trace interactive
                             :exhaustive exhaustive
                             :nanalyses nanalyses
                             :trees-hook trees-hook
                             :semantix-hook semantix-hook
                             :nresults nresults :filter filter
                             :burst burst
                             :targets targets)))))

      #+:allegro
      (when (and (= (get-field+ :readings result -1) -1)
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
             (others (get-field :others result))
             (timeup (get-field :timeup result))
             (comment (get-field+ :comment result ""))
             (global (gc-statistics :global))
             (scavenge (gc-statistics :scavenge))
             (new (gc-statistics :new))
             (old (gc-statistics :old))
             (total (length (gc-statistics :efficiency)))
             (efficiency (round (average (gc-statistics :efficiency))))
             ;;
             ;; no point doing the gc() statistics in :translation mode, as it
             ;; will always dispatch all of the work to further PVM clients
             ;;
             (comment (if (eq type :translate)
                        comment
                        (format 
                         nil 
                         "~a (:global . ~d) (:scavenge . ~d) ~
                          (:new . ~d) (:old . ~d) ~
                          (:efficiency . ~d) (:total . ~d)"
                         comment global scavenge new old efficiency total)))
             (a-load #+:pvm (load_average) #-:pvm nil))
        (when (and (integerp others) (< others -1))
          (push (cons :others (+ (expt 2 32) others)) result))
        (push (cons :i-load i-load) result)
        (push (cons :a-load a-load) result)
        (push (cons :parse-id parse-id) result)
        (push (cons :run-id run-id) result)
        (push (cons :i-id i-id) result)
        (push (cons :gc gc) result)
        (push (cons :gcs (+ global scavenge)) result)
        (push (cons :comment comment) result)
        (when (and timeup (not (= readings -1)))
          (push (cons :error (if (stringp timeup) timeup "timeup")) result)))
      result))))

(defun process-queue (runs 
                      &key client (verbose t) (stream *tsdb-io*) interrupt)

  #+:debug
  (setf %runs runs)
  
  (loop
      with tid = (if client (client-tid client) -1)
      while (or (find-if #'consp runs :key #'run-status) 
                (and client (consp (client-status client))))
      with pending = nil
      for message = (or (pvm_poll tid -1 1) 
                        (unless (= tid -1) (pvm_poll -1 %pvm_task_fail% 1)))
      finally
        (return (pairlis '(:pending :ready) 
                         (list pending (find :ready runs :key #'run-status))))
      do (expire-clients (if client (list client) *pvm-clients*))
      when (message-p message)
      do
        (when *pvm-debug-p*
          (with-standard-io-syntax
            (format
             t
             "~&process-queue(): got message:~% `~s'~%"
             message))
          (force-output))
        (let* ((tag (message-tag message))
               (remote (if (eql tag %pvm_task_fail%)
                         (message-corpse message)
                         (message-remote message)))
               (load (message-load message))
               (content (message-content message))
               (run (find remote runs :key #'run-tid))
               (client (or client (get-field :client run)))
               (item (and client
                          (consp (client-status client))
                          (rest (client-status client))))
               (host (and (client-p client) (client-host client))))

        (cond
         ((eql tag %pvm_task_fail%)
          
          (when (consp item)
            (let* ((fail (pairlis '(:host :corpse) (list host remote))))
              (when (get-field :strikes item)
                (incf (get-field :strikes item)))
              (push item pending)
              (when verbose
                (print-item item :stream stream :result fail)
                (print-result fail :stream stream))))
          (when run
            (nconc run `((:end . ,(current-time :long :tsdb)))))
          (when (client-p client)
            (when (cpu-p (client-cpu client))
              (format
               stream
               "~&process-queue(): client exit on `~a' <~x>~%"
               (client-host client) remote))
            (setf (client-status client) :exit)
            (when (and *process-client-retries* client)
              (let* ((cpu (client-cpu client))
                     (clients (initialize-cpus
                               :cpus (list cpu) :reset nil :block t)))
                (when run
                  (nconc
                   runs
                   (create-runs
                    (get-field :data run)
                    :comment (get-field :comment run) :gc (get-field :gc run)
                    :protocol (get-field :protocol run) :clients clients)))))))
                 
         ((null client)
          (when verbose
            (format
             stream
             "~&process-queue(): ignoring message from alien <~x>.~%"
             remote)
            (force-output stream))
          (push message *pvm-pending-events*))
         
         ((eql tag %pvm_lisp_message%)
          (when client (setf (client-load client) load))
          (if (eq (first content) :return)
            (case (second content)
              (:process-item
               (let* ((run-id (get-field+ :run-id run -1))
                      (result (nconc (pairlis '(:host :run-id)
                                              (list host run-id))
                                     (third content))))
                      
                 (setf (client-status client) :ready)
                 ;;
                 ;; _fix_me_
                 ;; PET, for the time being, returns :score but not :flags.
                 ;; patch this up, for a transition period.     (20-nov-08; oe)
                 ;;
                 (let ((cpu (when (cpu-p (client-cpu client))
                              (client-cpu client))))
                   (when (and cpu
                              (or (null (cpu-task cpu))
                                  (eq (first (cpu-task cpu)) :parse)))
                     (loop
                         for result in (get-field :results result)
                         for score = (get-field :score result)
                         for flags = (get-field :flags result)
                         when (and score (null flags)) do
                           (let ((flags (acons :ascore score nil)))
                             (nconc result (acons :flags flags nil))))))
                 (return-from process-queue
                   (pairlis '(:pending :ready :item :result)
                            (list pending run item result)))))
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
                   ignoring unexpected message from <~x>.~%"
                  remote)
                 (force-output stream))))
            (when verbose
              (format
               stream
               "~&process-queue(): ~
                ignoring dubious message from <~x>.~%"
               remote message)
              (force-output stream))))

         (t
          (when verbose
            (format
             stream
             "~&process-queue(): ~
             ignoring dubious message from <~x>.~%"
             remote message)))))
      else when (interrupt-p interrupt)
      do
        (let ((busy (loop 
                        for run in runs 
                        when (consp (run-status run)) collect run)))
          (format
           stream
           "process-queue(): ~
            external interrupt signal~
            ~:[~*~; (~d active client~:p)~].~%"
           busy (length busy))
          (force-output stream))
        (return-from process-queue
          (pairlis '(:pending :interrupt)
                   (list pending t)))))


(defun receive-item (nitems &key wait stream verbose interrupt)
  (declare (ignore nitems))

  (loop
      for message = (pvm_poll -1 -1 1)
      for i from 1
      when (and (integerp wait) (>= i wait)) do (return nil)
      when (message-p message) do
        (when *pvm-debug-p*
          (format t "~&receive-item(): got message:~% `~s'~%" message)
          (force-output))
        (let* ((tag (message-tag message))
               (remote (message-remote message))
               (content (message-content message)))

          (cond
           ((and (eql tag %pvm_lisp_message%)
                 (eq (first content) :account)
                 (eq (second content) :item-summary))
            (return-from receive-item 
              (pairlis '(:data :interrupt)
                       (list (third content) (interrupt-p interrupt)))))
           (t
            (when verbose
              (format
               stream
               "~&receive-item(): ~
                ignoring unexpected message from <~x>.~%"
               remote)
              (force-output stream)))))
      when (interrupt-p interrupt) do (return (acons :interrupt t nil))))


(defun enrich-result (result item &key verbose)
  (declare (ignore verbose))
  
  ;;
  ;; in case the result returned from a client is defective (e.g. in case the
  ;; PVM buffer could not be read because of array size limitations :-{), fix
  ;; up .result. with information from .item.
  ;;
  (when (null (get-field :i-id result))
    (nconc result (acons :i-id (get-field :i-id item) nil)))
  (when (null (get-field :parse-id result))
    (nconc result (acons :parse-id (get-field :parse-id item) nil)))
  (when (null (get-field :run-id result))
    (nconc result (acons :run-id (get-field :run-id item) nil)))
  
  (let* ((readings (get-field :readings result))
         (results (get-field :results result)))

    (nconc result (acons :unique (length results) nil))

    ;;
    ;; _fix_me_
    ;; find a reasonably efficient way of constructing a `score' relation when
    ;; we have scores on the results; maybe extract scored results first, sort,
    ;; and rank.  parsers are free to not return scores at all or for a subset
    ;; of results only.                                            (4-feb-03)
    ;;
    #+:null
    (loop
        with i = 1
        with last = (get-field :score (first results))
        for j from 1
        for result in results
        for result-id = (get-field :result-id result)
        for score = (get-field :score result))
                    
    (when (and *tsdb-result-hook* (integerp readings) (> readings 0))
      (multiple-value-bind (wealth condition)
          (ignore-errors (funcall *tsdb-result-hook* result))
        (when (or wealth condition)
          (setf result 
            (or wealth (acons :error (format nil "~a" condition) result))))))
    (when (get-field :fan result)
      (accumulate-mt-statistics result))

    result))

(defun print-result (result &key (stream *tsdb-io*) format index log)

  (let* ((readings (get-field :readings result))
         (unique (or (get-field :unique result) (get-field :nresults result)))
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
         (nfragments (get-field :nfragments result))
         (gc (get-field :gc result))
         (gcs (get-field :gcs result))
         (error (get-field :error result))
         (corpse (get-field :corpse result)))

    (cond
     ((eql readings 0)
      (format 
       stream 
       " ~:[---~;&mdash;~]~:[~; time up:~] ~
        ~@[<a href=\"/view?item=~a\" target=\"_blank\" ~
              onclick=\"return __pageInitializedP__\">~]~
        (~,2f~:[~*~;:~,2f~]|~,2f s) ~
        <~@[~d~]:~@[~d~]>~
        ~:[ {~d:~d}~;~2*~] ~
        (~a)~
        ~:[~*~*~; [~:[~;=~]~d]~]~@[</a>~].~%"
       (eq format :html) timeup index
       tcpu (>= tgc 0.1) tgc total
       words edges 
       (or (= unifications copies 0)
           (= unifications copies -1))
       unifications copies 
       (pprint-memory-usage result) 
       gcs gc gcs index))
     ((or (and error (null readings))
          (and error (numberp unique) (zerop unique))
          (eql readings -1))
      (format 
       stream 
       " ~:[---~;&mdash;~] ~@[<a href=\"/view?item=~a\" target=\"_blank\" ~
                   onclick=\"return __pageInitializedP__\">~]~
       error: ~a~@[</a>~].~%" 
       (eq format :html) index 
       (normalize-string (get-field :error result)) index))
     ((and (integerp readings) (> readings 0))
      (format 
       stream 
       " ~:[---~;&mdash;~]~:[~; time up:~] ~
        ~@[<a href=\"/view?item=~a\" target=\"_blank\" ~
              onclick=\"return __pageInitializedP__\">~]~
        ~:[~;^~]~:[~*~a~;~a [~a]~] ~
        (~,2f~:[~*~;:~,2f~]|~,2f:~,2f s) ~
        <~@[~d~]:~@[~d~]>~
        ~:[ {~d:~d}~;~2*~] ~
        (~a)~
        ~:[~*~*~; [~:[~;=~]~d]~]~@[</a>~].~%" 
       (eq format :html) timeup index
       (and (integerp nfragments) (> nfragments 0))
       (and unique (not (eql unique readings))) unique readings 
       tcpu (>= tgc 0.1) tgc first total 
       words edges 
       (or (= unifications copies 0)
           (= unifications copies -1))
       unifications copies 
       (pprint-memory-usage result) 
       gcs gc gcs index))
     (corpse
      (format
       stream
       " ~:[---~;&mdash;~] client exit <~x>.~%"
       (eq format :html) corpse))
     ((null readings)
      (format stream ".~%"))
     (t
      (format stream ".~%")))
    (force-output stream)
    
    (when log
      (let ((trace (get-field :trace result))) 
        (when trace (format log "~a" trace))
        (when %accumulated-mt-statistics%
          (let ((pcount (get-field :pcount %accumulated-mt-statistics%))
                (pfcount (get-field :pfcount %accumulated-mt-statistics%))
                (tcount (get-field :tcount %accumulated-mt-statistics%))
                (tfcount (get-field :tfcount %accumulated-mt-statistics%))
                (rcount (get-field :rcount %accumulated-mt-statistics%))
                (rfcount (get-field :rfcount %accumulated-mt-statistics%))
                (total (get-field :total %accumulated-mt-statistics%))
                (tfbleu (get-field :tfbleu %accumulated-mt-statistics%))
                (tbbleu (get-field :tbbleu %accumulated-mt-statistics%)))
            (format
             log
             "|= ~a:~a of ~a {~,1f+~,1f}; ~
              ~a:~a of ~a:~a {~,1f ~,1f}; ~
              ~a:~a of ~a:~a {~,1f ~,1f} @ ~a of ~a {~,1f} ~
              <~,2f ~,2f|~,2f ~,2f>.~%"
             pcount pfcount total
             (per-cent pcount total) (per-cent pfcount total)
             tcount tfcount pcount pfcount
             (per-cent tcount pcount) (per-cent tfcount pfcount)
             rcount rfcount tcount tfcount
             (per-cent rcount tcount) (per-cent rfcount tfcount)
             (+ rcount rfcount) total (per-cent (+ rcount rfcount) total)
             (divide tfbleu total) (divide tfbleu (+ rcount rfcount))
             (divide tbbleu total) (divide tbbleu (+ rcount rfcount)))))
        (format log "~%"))
      (force-output log))))

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
      (write-edges
       parse-id 
       (get-field :chart result) data
       :cache cache)
      (when (and *tsdb-rule-statistics-p* statistics)
        (if (eq *tsdb-rule-statistics-p* :raw)
          (write-rules
           parse-id
           statistics
           data
           :cache cache)
          (accumulate-rules statistics))))))

(defun pre-process (&key burstp)
  (declare (ignore burstp)))

(defun post-process (&key burstp)
  (declare (ignore burstp))
  #+:gcdebug
  (let ((*terminal-io* excl:*initial-terminal-io*)
        (*standard-output* excl:*initial-terminal-io*))
    (sys:gsgc-parameters)
    (room) (excl:gc) (room)))

(defun complete-runs (data runs &key cache interactive stream interrupt)
  (declare (special %accumulated-rule-statistics%))
  (setf %graft-aligned-generation-hack% nil)
  (loop
      for run in runs
      for completion = (unless (get-field :end run)
                         (complete-run run 
                                       :stream stream :interrupt interrupt))
      for status = (or (get-field :status run) (run-status run) :complete)
      when (get-field :run-id run)
      do
        (push (cons :status (format 
                             nil "~(~a~)" 
                             (if (eq status :ready) :complete status)))
              completion)
        (unless interactive 
          (write-run (append completion run) data :cache cache)))
  (when (and (null interactive) %accumulated-rule-statistics%)
    (write-rules -1 %accumulated-rule-statistics% data :cache cache)))

(defun complete-run (run &key stream interrupt custom)
  (when (get-field :run-id run)
    (let ((id (get-field :run-id run))
	  (context (get-field :context run))
          (gc-strategy (get-field :gc-strategy run))
          (client (get-field :client run)))
      (cond
       ((and client (client-p client))
        (let* ((tid (client-tid client))
               (cpu (client-cpu client))
               (custom (or custom (and (cpu-p cpu) (cpu-complete cpu))))
               (status (if (eq (client-protocol client) :lisp)
                         (revaluate 
                          tid 
                          `(complete-run
                            (quote ,(pairlis '(:run-id :context :gc-strategy)
                                             (list id context gc-strategy)))
                            :custom ,custom)
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
              external interrupt signal.~%")
            (force-output stream)
            (setf (client-status client) :interrupt)
            (acons :end (current-time :long :tsdb) nil))
           (t
            (setf (client-status client) :error)
            (acons :end (current-time :long :tsdb) nil)))))
       (t
        (let ((finalization (finalize-run context :custom custom)))
          (when gc-strategy (restore-gc-strategy gc-strategy))
          (cons (cons :end (current-time :long :tsdb)) finalization)))))))

(defun tsdb-ignore-p (o-ignore)
  (unless (or (null o-ignore) (equal o-ignore ""))
    o-ignore))

(defun call-hook (hook &rest arguments)
  (when hook
    (let ((result (apply #'call-raw-hook hook arguments)))
      (typecase result
        (null nil)
        (string result)
        (t (write-to-string result :escape t))))))

(defun call-raw-hook (hook &rest arguments)
  (when hook
    (let* ((extra (and (consp hook) (rest hook)))
           (hook (if (consp hook) (first hook) hook))
           (hook (typecase hook
                   (null nil)
                   (function hook)
                   (symbol (and (fboundp hook) (symbol-function hook)))
                   (string (ignore-errors 
                            (symbol-function (read-from-string hook))))))
           (arguments (append arguments extra)))
      (when hook (ignore-errors (apply hook arguments))))))

(defun call-safe-hook (hook &rest arguments)
  (when hook
    (let* ((hook (typecase hook
                   (null nil)
                   (function hook)
                   (symbol (and (fboundp hook) (symbol-function hook)))
                   (string (ignore-errors 
                            (symbol-function (read-from-string hook)))))))
      (apply hook arguments))))

(defun result-hook (result)
  (when (and (null *tsdb-write-mrs-p*) (null *tsdb-write-tree-p*))
    (return-from result-hook))
  (loop
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      for parse in (or (get-field :results result)
                       ;;
                       ;; _fix_me_
                       ;; not sure why this function appears to be overloaded
                       ;; to either operate on a `complete' result or just the
                       ;; :results value; consider dropping this branch.
                       ;;                                      (31-aug-05; oe)
                       (and (consp (first (first result))) result))
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

(defun accumulate-results (results item &key type)
  (declare (ignore item type))
  (loop
      for result in results
      for readings = (get-field :readings result)
      when (and (numberp readings) (> readings 0))
      return result
      finally (return (first results)))
  #+:null
  (let (successes errors)
    (loop
        for result in results
        for readings = (get-field :readings result)
        when (or (not (numberp readings)) (<= readings 0))
        do (push result errors)
        else do (push result successes))
    (setf successes (nreverse successes))
    (setf errors (nreverse errors))
    (if successes
      (loop
          with result
          for success in successes
          do (adjoin-result success result)
          finally (return result))
      (first results))))

(defun adjoin-result (result results)
  (when (null results)
    (return-from adjoin-result
      (let ((n (loop
                   for foo in (get-field :results result)
                   for id = (get-field :result-id foo)
                   when id maximize id)))
        (acons :n n result))))
  (loop
      for key in '(:others :symbols :conses :treal :tcpu :tgc 
                   :readings :pedges)
      for value = (get-field key result)
      when (numberp value) do
        (let ((match (get-field :key results)))
          (if (numberp match)
            (incf (get-field :key results) value)
            (nconc results (acons key value nil)))))
  (let ((n (get-field :n results))
        (match (get-field :results results)))
    (if match
      (nconc 
       match
       (loop
           with baz = n
           with foo = (get-field :results result)
           for bar in foo
           do 
             (setf baz 
               (max baz (incf (get-field :result-id bar) n)))
           finally 
             (setf (get-field :n results) baz)
             (return results)))
      (let ((foo (get-field :results result)))
        (nconc results foo)
        (setf (get-field :n results)
          (loop
              for foo in (get-field :results result)
              for id = (get-field :result-id foo)
              when id maximize id))))))

(defun accumulate-rules (statistics)
  (declare (special %accumulated-rule-statistics%))
  (if %accumulated-rule-statistics%
    (loop
        for rule in statistics
        for name = (get-field+ :rule rule "")
        for filtered = (get-field :filtered rule)
        for executed = (get-field :executed rule)
        for successful = (get-field :successful rule)
        for actives = (get-field :actives rule)
        for passives = (get-field :passives rule)
        for arule = (loop
                        for arule in %accumulated-rule-statistics%
                        for aname = (get-field+ :rule arule "")
                        thereis (when (equal name aname) arule))
        for afiltered = (get-field :filtered arule)
        for aexecuted = (get-field :executed arule)
        for asuccessful = (get-field :successful arule)
        for aactives = (get-field :actives arule)
        for apassives = (get-field :passives arule)
        do
          (when (and filtered afiltered) 
            (incf (get-field :filtered arule) filtered))
          (when (and executed aexecuted)
            (incf (get-field :executed arule) executed))
          (when (and successful asuccessful)
            (incf (get-field :successful arule) successful))
          (when (and actives aactives)
            (incf (get-field :actives arule) actives))
          (when (and passives apassives)
            (incf (get-field :passives arule) passives)))
    (setf %accumulated-rule-statistics% statistics)))

(defun accumulate-mt-statistics (result)
  (when %accumulated-mt-statistics%
    (incf (get-field :total %accumulated-mt-statistics%))
    (let* ((fragmentp (let ((foo (get-field :nfragments result)))
                        (and (numberp foo) (> foo 0))))
           (fan (get-field :fan result))
           (nanalyses (first fan))
           (ntransfers (second fan))
           (nrealizations (third fan))
           (fbleu (get-field :fbleu result))
           (bbleu (get-field :bbleu result)))
      (when (numberp fbleu)
        (incf (get-field :tfbleu %accumulated-mt-statistics%) fbleu))
      (when (numberp bbleu)
        (incf (get-field :tbbleu %accumulated-mt-statistics%) bbleu))
      (when (and (numberp nanalyses) (numberp ntransfers)
                 (numberp nrealizations))
        (when (> nanalyses 0)
          (incf
           (get-field
            (if fragmentp :pfcount :pcount)
            %accumulated-mt-statistics%))
          (when (> ntransfers 0)
            (incf
             (get-field
              (if fragmentp :tfcount :tcount)
              %accumulated-mt-statistics%))
            (when (> nrealizations 0)
              (incf
               (get-field
                (if fragmentp :rfcount :rcount)
                %accumulated-mt-statistics%)))))))))
