(in-package :tsdb)

(defun summarize (profiles &key file meter)
  (when meter (meter :value (get-field :start meter)))
  (loop
      with semi = (mt::make-semi)
      for profile in (if (consp profiles) profiles (list profiles))
      for message = (format nil "summarizing `~a' ..." profile)
      do
        (when meter (status :text message))
        (loop
            for item in (analyze profile :thorough '(:mrs))
            for results = (get-field :results item)
            do
              (loop
                  for result in results
                  for mrs = (get-field :mrs result)
                  when mrs do (mt::record-mrs mrs semi)))
      finally 
        (when meter (status :text (format nil "~a done" message) :duration 5))
        (let ((stream (create-output-stream file nil)))
          (mt::print-semi semi :stream stream)
          (when (stringp file) (close stream))))
  (when meter (meter :value (get-field :end meter))))

(defun translate-file (name &key (file *tsdb-io*) tee (verbose t))
  (labels ((read-string (stream)
             (loop
                 for line = (read-line stream nil nil)
                 while (and line (cl-ppcre:scan "^[ \\t]*;" line))
                 finally (return line)))
           (emptyp (string)
             (cl-ppcre:scan "^[ \\t]*$" string))
           (strip-identifier (string)
             (multiple-value-bind (start end) 
                 (cl-ppcre:scan "^[0-9]+[a-z]\\. " string)
               (declare (ignore start))
               
               (if (numberp end)
                 (subseq string end)
                 string))))
    (let* ((encoding :iso-8859-1)
           (output (create-output-stream file nil :encoding encoding))
           (tee (if (and tee (not (eq output *tsdb-io*)))
                  (make-broadcast-stream output *tsdb-io*)
                  output))
           (gc (install-gc-strategy 
                nil :tenure nil :burst t :verbosity nil :verbose verbose))
           (*tsdb-gc-message-p* nil))
      (with-open-file (input name :direction :input)
        (loop
            with i = 0
            with pcount = 0 with pfcount = 0
            with tcount = 0 with tfcount = 0
            with rcount = 0 with rfcount = 0
            for source = (read-string input)
            for targets = (unless (emptyp source)
                            (loop
                                for line = (read-string input)
                                while (and line (not (emptyp line)))
                                collect (strip-identifier line)))
            while source 
            unless (emptyp source)
            do
              (incf i)
              (let* ((result (translate-string
                              (strip-identifier source)
                              :id i :targets targets :stream tee))
                     (parsep (let ((foo (get-field :readings result)))
                               (and (numberp foo) (> foo 0))))
                     (fragmentp (let ((foo (get-field :fragments result)))
                                  (and (numberp foo) (> foo 0))))
                     (transfers (get-field :transfers result))
                     (transferp (loop
                                    for transfer in transfers
                                    for foo = (get-field :readings transfer)
                                    thereis (and (numberp foo) (> foo 0))))
                     (realizationp (get-field :translations result)))
                (when parsep
                  (if fragmentp (incf pfcount) (incf pcount))
                  (when transferp
                    (if fragmentp (incf tfcount) (incf tcount))
                    (when realizationp
                      (if fragmentp (incf rfcount) (incf rcount))))))
              (format
               tee
               "|= ~a:~a of ~a {~,1f ~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f} @ ~a of ~a {~,1f}.~%~%"
               pcount pfcount i
               (per-cent pcount i) (per-cent pfcount i)
               tcount tfcount pcount pfcount
               (per-cent tcount pcount) (per-cent tfcount pfcount)
               rcount rfcount tcount tfcount
               (per-cent rcount tcount) (per-cent rfcount tfcount)
               (+ rcount rfcount) i (per-cent (+ rcount rfcount) i))))
        
      (restore-gc-strategy gc)
      (when (stringp file) (close output)))))

(defun translate-string (input &key id targets (stream *tsdb-io*))

  (declare (ignore targets))
  (format
   stream
   "[~a]~@[ (~a)~] |~a|"
   (current-time :long :short) id input)

  (let ((start (get-internal-real-time))
        (parse (pvm-process input)))
    (print-result parse :stream stream)
    (loop
        with transfers
        with translations
        with ntranslations = 0
        for result in (get-field :results parse)
        for pid = (get-field :result-id result)
        for transfer = (pvm-process parse :transfer :result-id pid)
        for realizations = nil
        do
          (format
           stream
           "|~%|-[~a] # ~a"
           (current-time :long :since :treal start) pid)
          (print-result transfer :stream stream)
          (loop
              for result in (get-field :results transfer)
              for tid = (get-field :result-id result)
              for realization = 
                (pvm-process transfer :generate :result-id tid :rankp t)
              do
                (format
                 stream
                 "| |~%| |-[~a] # ~a"
                 (current-time :long :since :treal start) tid)
                (print-result realization :stream stream)
                (loop
                    for result in (get-field :results realization)
                    for rid = (get-field :result-id result)
                    for tree = (tsdb::get-field :tree result)
                    for score = (tsdb::get-field :score result)
                    do
                      (format
                       stream
                       "| |   |~a| [~a]~%"
                       tree score)
                    when (and (stringp tree) (numberp score))
                    do
                      (push (pairlis '(:pid :tid :rid :string :score)
                                     (list pid tid rid tree score))
                            translations)
                      (incf ntranslations))
                (push realization realizations))
          (push 
           (acons :realizations (nreverse realizations) transfer)
           transfers)
        finally
          ;;
          ;; now eliminate duplicates, making sure to keep outputs that were
          ;; found earlier in the process: since .translations. at this point
          ;; is in reverse order, discard everything from the front while there
          ;; is an equivalent output further down the list.
          ;;
          (setf translations
            (loop
                for translations on translations
                for translation = (first translations)
                unless (find 
                        (get-field :string translation) (rest translations)
                        :test #'string=
                        :key #'(lambda (foo) (get-field :string foo)))
                collect translation))
                        
          (setf translations
            (sort
             translations
             #'< :key #'(lambda (foo) (get-field :score foo))))
          (loop
              initially
                (let ((n (length translations)))
                  (format
                   stream
                   "|~%|< |~a|~@[ (~a)~] --- ~:[~*~a~;~a [~a]~]~%"
                   input id (not (= ntranslations n)) n ntranslations))
              for translation in translations
              do
                (format
                 stream
                 "|> |~@[~a~]| [~@[~,1f~]] (~a:~a:~a).~%"
                 (get-field :string translation) 
                 (get-field :score translation)
                 (get-field :pid translation)
                 (get-field :tid translation)
                 (get-field :rid translation)))
          (return (append (pairlis '(:transfers :translations)
                                   (list (nreverse transfers) translations))
                          parse)))))


          