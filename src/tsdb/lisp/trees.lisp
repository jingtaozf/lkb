(in-package :tsdb)


(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (let* ((condition (if condition
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items
          (if (stringp data) 
            (analyze data 
                     :condition condition :thorough '(:derivation)
                     :meter meter :message t)
            data))
         (message (format nil "generating `~a' tree view ..." data))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo)))))

    (when (functionp *statistics-result-filter*)
      (setf items
        (loop
            for item in items
            for result = (funcall *statistics-result-filter* item)
            when result collect result)))
    
    (when meter
      (status :text message)
      (meter :value 0))

    (loop
        with increment = (and meter (/ 1 (if items (length items) 1)))
        with indices = (loop
                           with indices
                           for field in '(:i-id :i-input :o-input 
                                          :readings :results)
                           for i from 0
                           do
                             (setf (getf indices field) i)
                           finally (return indices))
        with firstp = t
        with lkb::*parse-record*
        with frame = (clim:make-application-frame 'lkb::compare-frame)
        with client = nil
        initially 
          (setf (lkb::compare-frame-current-chart frame) nil)
          (setf (clim:frame-pretty-name frame) "[incr tsdb()] Tree Selection")
          (setf (lkb::compare-frame-controller frame) *current-process*)
        for item in items
        for values = (loop
                         with values = (make-array 5)
                         for pair in item
                         for key = (first pair)
                         when (getf indices key) do
                           (setf (aref values (getf indices key)) (rest pair))
                         finally (return values))
          
        for i-id = (aref values (getf indices :i-id))
        for i-input = (aref values (getf indices :i-input))
        for o-input = (aref values (getf indices :o-input))
        for readings = (aref values (getf indices :readings))
        for results = (aref values (getf indices :results))
        for trees = (loop
                        for result in results
                        for derivation = (get-field :derivation result)
                        for tree = (when derivation (reconstruct derivation))
                        when tree collect tree)
        while (or firstp (and (mp:process-p client)
                              (mp:process-active-p client)))
        do
          (when increment (meter-advance increment))
          (when trees 
            (setf lkb::*parse-record* trees)
            (setf (lkb::compare-frame-item frame) i-id)
            (lkb::set-up-compare-frame lkb::*parse-record* frame)
            (cond
             (firstp
              (setf firstp nil)
              (setf client
                (mp:run-function 
                 "[incr tsdb()] Tree Selection"
                 #'clim:run-frame-top-level frame)))
             (t
              (clim:redisplay-frame-panes frame :force-p t)))
            (process-add-arrest-reason *current-process* :wait))
        finally
          (clim:frame-exit frame)
          (when (mp:process-p client)
            (mp:process-kill client)))
    
    (when meter
      (status :text (format nil "~a done" message) :duration 10)
      (meter :value 1))))
