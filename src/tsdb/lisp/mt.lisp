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

(defun translate-file (name &key (file *tsdb-io*) 
                                 xml tee (verbose t)
                                 (encoding :iso-8859-1))
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
    (let* ((*tsdb-io* (if (eq *tsdb-io* t) *terminal-io* *tsdb-io*))
           (output (create-output-stream file nil :encoding encoding))
           (capture (when (stringp xml)
                      (create-output-stream xml nil :format :xml)))
           (tee (if (and tee (not (eq output *tsdb-io*)))
                  (make-broadcast-stream output *tsdb-io*)
                  output))
           (gc (install-gc-strategy 
                nil :tenure nil :burst t :verbosity nil :verbose verbose))
           (*tsdb-gc-message-p* nil))
      (when tee
        (format
         tee
         ";;;~%;;; `fan-out' batch of `~a';~%;;; (~a@~a; ~a).~%;;;~%~%"
         name (current-user) (current-host) (current-time :long :pretty)))
      (when capture (xmlify-run :stream capture))
      (with-open-file (input name :direction :input)
        (loop
            with i = 0
            with pcount = 0 with pfcount = 0
            with tcount = 0 with tfcount = 0
            with rcount = 0 with rfcount = 0
            with tbleu = 0
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
                     (realizationp (get-field :translations result))
                     (bleu (get-field+ :bleu result 0)))

                (when capture (xmlify-item result :stream capture))

                (when parsep
                  (if fragmentp (incf pfcount) (incf pcount))
                  (when transferp
                    (if fragmentp (incf tfcount) (incf tcount))
                    (when realizationp
                      (if fragmentp (incf rfcount) (incf rcount))
                      (when bleu (incf tbleu bleu)))))
                
              (format
               tee
               "|= ~a:~a of ~a {~,1f+~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f} @ ~a of ~a {~,1f} <~,2f ~,2f>.~%~%"
               pcount pfcount i
               (per-cent pcount i) (per-cent pfcount i)
               tcount tfcount pcount pfcount
               (per-cent tcount pcount) (per-cent tfcount pfcount)
               rcount rfcount tcount tfcount
               (per-cent rcount tcount) (per-cent rfcount tfcount)
               (+ rcount rfcount) i (per-cent (+ rcount rfcount) i)
               (divide tbleu i) (divide tbleu (+ rcount rfcount))))))
        
      (restore-gc-strategy gc)
      (when (stringp file) (close output))
      (when capture 
        (format capture "~%</run>~%")
        (when (stringp xml) (close capture))))))

(defun translate-string (input &key id targets (stream *tsdb-io*))

  (format
   stream
   "[~a]~@[ (~a)~] |~a|"
   (current-time :long :short) id input)

  (let ((start (get-internal-real-time))
        (parse (pvm-process input :parse :wait 30)))
    (print-result parse :stream stream)
    (loop
        with analyses = (get-field :results parse)
        with transfers
        with translations
        with ntransfers = 0
        with nrealizations = 0
        with ntranslations = 0
        with best = 0
        for result in analyses
        for pid = (get-field :result-id result)
        for transfer = (pvm-process parse :transfer :result-id pid)
        for realizations = nil
        do
          (incf ntransfers (length (get-field :results transfer)))
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
                (incf nrealizations (length (get-field :results realization)))
                (format
                 stream
                 "| |~%| |-[~a] # ~a"
                 (current-time :long :since :treal start) tid)
                (print-result realization :stream stream)
                (loop
                    with results = (get-field :results realization)
                    for result in results
                    for rid = (get-field :result-id result)
                    for tree = (tsdb::get-field :tree result)
                    for score = (tsdb::get-field :score result)
                    for bleu = (first
                                (bleu-score-strings 
                                 (list tree)
                                 (when (eq result (first results)) targets)
                                 :source input))
                    when (numberp bleu) do (setf best (max best bleu))
                    do
                      (nconc result (acons :bleu bleu nil))
                      (format
                       stream
                       "| |   |~a| [~@[~,1f~]] <~@[~,2f~]>~%"
                       tree score bleu)
                    when (and (stringp tree) (numberp score))
                    do
                      (push (pairlis '(:pid :tid :rid :string :score :bleu)
                                     (list pid tid rid tree score bleu))
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
                   "|~%|< |~a|~@[ (~a)~] --- ~a x ~a x ~a = ~
                    ~:[~*~a~;~a [~a]~]~%"
                   input id 
                   (length analyses) ntransfers nrealizations
                   (not (= ntranslations n)) n ntranslations))
              for translation in translations
              do
                (format
                 stream
                 "|> |~@[~a~]| [~@[~,1f~]] <~@[~,2f~]> (~a:~a:~a).~%"
                 (get-field :string translation) 
                 (get-field :score translation) (get-field :bleu translation)
                 (get-field :pid translation)
                 (get-field :tid translation)
                 (get-field :rid translation)))
          (force-output stream)
          (return (append (pairlis '(:transfers :translations
                                     :bleu)
                                   (list (nreverse transfers) translations
                                         best))
                          parse)))))

(defun xmlify-run (&key (stream t) (prefix ""))
  (let* ((user (current-user))
         (date (current-time :long :pretty))
         (host (current-host))
         (os (current-os))
         (platform (current-platform))
         (logon (subseq mt::*version* 7 (- (length mt::*version*) 2)))
         (itsdb (current-tsdb))
         (lkb (subseq lkb::*cvs-version* 7 (- (length lkb::*cvs-version*) 2)))
         (grammar (current-grammar))
         (analysis (loop
                       for client in *pvm-clients*
                       for cpu = (client-cpu client)
                       when (smember :parse (cpu-task cpu))
                       return (cpu-grammar cpu)))
         (transfer (loop
                       for client in *pvm-clients*
                       for cpu = (client-cpu client)
                       when (smember :transfer (cpu-task cpu))
                       return (cpu-grammar cpu)))
         (realization (loop
                          for client in *pvm-clients*
                          for cpu = (client-cpu client)
                          when (smember :generate (cpu-task cpu))
                          return (cpu-grammar cpu))))
    (format stream "<run>~%~%")
    (format
     stream
     "~a<environment>~%~
      ~:[~2*~;~a  <user>~a</user>~%~]~
      ~:[~2*~;~a  <date>~a</date>~%~]~
      ~:[~2*~;~a  <host>~a</host>~%~]~
      ~:[~2*~;~a  <os>~a</os>~%~]~
      ~:[~2*~;~a  <platform>~a</platform>~%~]~
      ~:[~2*~;~a  <logon>~a</logon>~%~]~
      ~:[~2*~;~a  <itsdb>~a</itsdb>~%~]~
      ~:[~2*~;~a  <lkb>~a</lkb>~%~]~
      ~:[~2*~;~a  <grammar>~a</grammar>~%~]~
      ~:[~2*~;~a  <analysis>~a</analysis>~%~]~
      ~:[~2*~;~a  <transfer>~a</transfer>~%~]~
      ~:[~2*~;~a  <realization>~a</realization>~%~]~
      ~a</environment>~%~%"
     prefix 
     user prefix user
     date prefix date
     host prefix host
     os prefix os
     platform prefix platform
     logon prefix logon
     itsdb prefix itsdb
     lkb prefix lkb
     grammar prefix grammar
     analysis prefix analysis
     transfer prefix transfer
     realization prefix realization
     prefix)
    (force-output stream)))

(defun xmlify-item (item &key (stream t) (prefix ""))

  (let ((type (cond
               ((assoc :translations item) :analysis)
               ((assoc :realizations item) :transfer)
               (t :realization))))
    (format
     stream
     "~a<item type=\"~(~a~)\"~@[ id=\"~a\"~] ~
           readings=\"~a\"~@[ fragments=\"~a\"~]~@[ tcpu=\"~d\"~]>~%"
     prefix type
     (when (eq type :analysis) (get-field :i-id item))
     (get-field :readings item) 
     (get-field :fragments item) (get-field :tcpu item))

    ;;
    ;; first off, get the end-to-end summary out: the original string, followed
    ;; by the unique set of output strings and their (cross-perplexity) scores.
    ;;
    (when (eq type :analysis)
      (format
       stream
       "~a  <input>~a</input>~%"
       prefix (get-field :i-input item))
      (loop
          for translation in (get-field :translations item)
          for string = (get-field :string translation)
          for score = (get-field :score translation)
          for bleu = (get-field :bleu translation)
          do
            (format
             stream
             "~a  <output score=\"~,1f\"~@[ bleu=\"~,4f\"~]>~a</output>~%"
             prefix score bleu string)))

    ;;
    ;; in case there was a processing error recorded, get it out as a property
    ;; of this item.
    ;;
    (let ((error (get-field :error item)))
      (when (and (stringp error) (not (string= error "")))
        (format
         stream
         "~a  <error>~a</error>~%"
         prefix error)))
    
    ;;
    ;; now generate a tree of downstream processing outputs, one per result; 
    ;; try doing this in a way that allows calling ourselves recursively at the
    ;; various levels.
    ;;
    (loop
        with transfers = (get-field :transfers item)
        with realizations = (get-field :realizations item)
        for result in (get-field :results item)
        for id = (get-field :result-id result)
        for edges = (get-field :pedges item)
        for string = (when (eq type :realization) (get-field :tree result))
        for score = (get-field :score result)
        for transfer = (pop transfers)
        for realization = (pop realizations)
        for mrs = nil
                  #+:null
                  (let* ((string (get-field :mrs result))
                         (mrs (ignore-errors 
                               (mrs::read-mrs-from-string string))))
                    (when (mrs::psoa-p mrs) mrs))
        do 
          (format
           stream
           "~a  <result id=\"~a\"~@[ edges=\"~a\"~]~@[ score=\"~,1f\"~]>~%"
           prefix id edges score)
        when string
        do
          (format
           stream
           "~a    <output>~a</output>~%"
           prefix (normalize-string string) prefix)
        when mrs
        do
          (let ((output (with-output-to-string (stream)
                          (mrs::output-mrs1 mrs 'mrs::mrs-xml stream))))
            (with-input-from-string (tmp output)
              (loop
                  for line = (read-line tmp nil nil)
                  while line
                  do
                    (format stream "~a  ~a~%" prefix line))))
        when transfer do 
          (xmlify-item 
           transfer :stream stream :prefix (format nil "~a    " prefix))
        when realization do 
          (xmlify-item 
           realization :stream stream :prefix (format nil "~a    " prefix))
        do (format stream "~a  </result>~%" prefix))
    (format stream "~a</item>~%" prefix)
    (force-output stream)))

#+:clim
(let ((history '("Ask vil kunne reise.")))
  (defun mt::parse-interactively (&optional string)
    (ignore-errors
     (let* ((input
             (unless string
               (lkb::ask-for-strings-movable 
                "Parse Interactively" 
                `(("Input:" . ,(cons :typein-menu history))) 400)))
            (string (or string (when (stringp (first input)) (first input))))
            (item (and string (pvm-process string :parse :wait 5))))
       (when string
         (setf history
           (butlast
            (cons string (remove string history :test #'equal))
            (max 0 (- (length history) 15)))))
       (loop
           for result in (get-field :results item)
           for mrs = (get-field :mrs result)
           collect (when (stringp mrs) (mrs::read-mrs-from-string mrs))
           into mrss
           finally (mt::browse-mrss 
                    mrss 
                    (format nil "`~a' Parse Results" string)))))))

(defparameter *bleu-binary* 
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (when root
      (format nil "~a/ntnu/bleu/bleu.pl" root))))

(defparameter *bleu-punctuation-characters* '(#\. #\! #\? #\, #\: #\|))

(defparameter *bleu-stream* nil)

(defparameter *bleu-pid* nil)

(defun bleu-normalize-string (string)
  (when string
    (loop
        with result = (make-array (length string)
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        unless (member c *bleu-punctuation-characters* :test #'char=) 
        do (vector-push (char-downcase c) result)
        finally (return result))))

(let ((lock (mp:make-process-lock)))

  (defun bleu-shutdown ()
    (mp:with-process-lock (lock)
      (when *bleu-stream*
        (ignore-errors
         (close *bleu-stream*)
         (setf *bleu-stream* nil)))
      (when *bleu-pid*
        (ignore-errors
         (run-process "kill -HUP ~d" *bleu-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process "kill -TERM ~d" *bleu-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process "(when kill -QUIT ~d" *bleu-pid* 
                      :wait t :output "/dev/null" :error-output "/dev/null"))
        (sys:os-wait nil *bleu-pid*)
        (setf *bleu-pid* nil))))
  
  (defun bleu-initialize ()
    (mp:with-process-lock (lock)
  
      (when *bleu-stream* (bleu-shutdown))
      
      (let (foo)
        (multiple-value-setq (*bleu-stream* foo *bleu-pid*)
          (run-process 
           (format nil "~a" *bleu-binary*)
           :wait nil
           :output :stream :input :stream
           :error-output "/dev/null" :if-error-output-exists :append))
        (setf foo foo))))

  (defun bleu-score-strings (translations &optional references &key source)
    (mp:with-process-lock (lock)
      (when (null *bleu-stream*) (bleu-initialize))
      (let (scores)
        (when references
          (format *bleu-stream* "SOURCE~@[ ~a~]~%" source)
          (loop
              for reference in references 
              do (format
                  *bleu-stream*
                  "REF ~a~%"
                  (bleu-normalize-string reference))))
        (loop
            for translation in translations
            do 
              (format
               *bleu-stream*
               "TRANS ~a~%"
               (bleu-normalize-string translation))
              (force-output *bleu-stream*)
              (let ((score (read *bleu-stream* nil nil)))
                (if (numberp score)
                  (push score scores)
                  (return))))
        (nreverse scores)))))
