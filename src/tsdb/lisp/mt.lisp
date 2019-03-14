(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2018 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter *mt-engine* nil)

(defparameter *mt-analysis-weight* 0.1)

(defparameter *mt-transfer-weight* 0.1)

(defparameter *mt-realization-weight* 0.25)

(defparameter *mt-lm-weight* 0.25)

(defparameter *mt-distortion-weight* 0.1)

(defparameter *mt-lfn-weight* 0.2)

(defparameter *mt-lnf-weight* 0.0)

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
                  when mrs do (mt::record-mrs semi mrs)))
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
                 while (and line (ppcre:scan "^[ \\t]*;" line))
                 finally (return line)))
           (emptyp (string)
             (ppcre:scan "^[ \\t]*$" string))
           (strip-identifier (string)
             (or
              (multiple-value-bind (start end) 
                  (ppcre:scan "^[0-9]+[a-z]\\. " string)
                (declare (ignore start))
                (and (numberp end) (subseq string end)))
              (multiple-value-bind (start end) 
                  (ppcre:scan "^\\[[0-9]{4}[a-z]\\] " string)
                (declare (ignore start))
                (and (numberp end) (subseq string end)))
              string))
           (fragmentp (string)
             (multiple-value-bind (start end) 
                 (ppcre:scan "^[ ]*\\^" string)
               (declare (ignore start))
               (values (if (numberp end) (subseq string end) string) end))))
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
            with tfbleu = 0
            with tbbleu = 0
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
                              (fragmentp (strip-identifier source))
                              :id i :targets targets :stream tee))
                     (parsep (let ((foo (get-field :readings result)))
                               (and (numberp foo) (> foo 0))))
                     (fragmentp (let ((foo (get-field :nfragments result)))
                                  (and (numberp foo) (> foo 0))))
                     (transfers (get-field :transfers result))
                     (transferp (loop
                                    for transfer in transfers
                                    for foo = (get-field :readings transfer)
                                    thereis (and (numberp foo) (> foo 0))))
                     (realizationp (get-field :translations result))
                     (fbleu (get-field+ :fbleu result 0))
                     (bbleu (get-field+ :bbleu result 0)))

                (when capture (xmlify-item result :stream capture))

                (when parsep
                  (if fragmentp (incf pfcount) (incf pcount))
                  (when transferp
                    (if fragmentp (incf tfcount) (incf tcount))
                    (when realizationp
                      (if fragmentp (incf rfcount) (incf rcount))
                      (when fbleu (incf tfbleu fbleu))
                      (when bbleu (incf tbbleu bbleu)))))
                
              (format
               tee
               "|= ~a:~a of ~a {~,1f+~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f}; ~
                ~a:~a of ~a:~a {~,1f ~,1f} @ ~a of ~a {~,1f} ~
                <~,2f ~,2f|~,2f ~,2f>.~%~%"
               pcount pfcount i
               (per-cent pcount i) (per-cent pfcount i)
               tcount tfcount pcount pfcount
               (per-cent tcount pcount) (per-cent tfcount pfcount)
               rcount rfcount tcount tfcount
               (per-cent rcount tcount) (per-cent rfcount tfcount)
               (+ rcount rfcount) i (per-cent (+ rcount rfcount) i)
               (divide tfbleu i) (divide tfbleu (+ rcount rfcount))
               (divide tbbleu i) (divide tbbleu (+ rcount rfcount))))))
        
      (restore-gc-strategy gc)
      (when (stringp file) (close output))
      (when capture 
        (format capture "~%</run>~%")
        (when (stringp xml) (close capture))))))

(defun translate-string (input &key id (wf 1) targets nanalyses
                                    (stream *tsdb-io*)
                                    (format :ascii)
                                    (types '(:neva :bleu :torbjoern :wa))
                                    filter index)
  (declare (special %model%))
  
  ;;
  ;; in HTML mode, use extra cell (instead of a container and margins) to get
  ;; better dynamic rendering (in Mozilla, at least).          (25-jan-05; oe)
  ;;
  (case format
    (:ascii
     (format
      stream
      "[~a]~@[ (~a)~] ~:[~;*~]|~a|"
      (current-time :long :short) id (and (numberp wf) (zerop wf)) input))
    (:html
     (format
      stream
      "<table class=\"flow\" border=0>~%~
       <tr><td class=\"flowTopBorder\" colspan=5></td></tr>~%~
       <tbody id=\"fanOutTree\">~%~
       <tr><td class=\"flowLeftBorder\"></td>~
           <td class=\"flow\" colspan=3>[~a]~@[ (~a)~] ~:[~;*~]|~a|"
      (current-time :long :short) id (and (numberp wf) (zerop wf)) input)))
  (force-output stream)
  
  (let* ((start (get-internal-real-time))
         (nanalyses
          (if (stringp nanalyses)
            (loop
                with split
                = #+:ppcre (ppcre:split "[^0-9]" nanalyses) #-:ppcre nil
                for match = (pop split)
                for i = (or (ignore-errors (read-from-string match)) 0)
                repeat 4
                collect i)
            (and (numberp nanalyses) (> nanalyses 0)
                 (list nanalyses nanalyses nanalyses nanalyses))))
         (parse
          (pvm-process
           input :parse :wait 30 :filter filter
           :nanalyses (first nanalyses))))

    (print-result
     parse :stream stream :format format :index index :format format)
    (case format
      (:html
       (nconc parse (acons :www index nil)) (incf index)
       (format
        stream
        "</td><td class=flowRightBorder></td></tr>~%")))
    (force-output stream)
    
    (loop
        with amin with amax with tmin with tmax
        with rmin with rmax with lmin with lmax
        with nfmin with nfmax with fnmin with fnmax with dmin with dmax
        with analyses = (get-field :results parse)
        with nfragments = (get-field :nfragments parse)
        with n = (if (consp nanalyses) (first nanalyses) (length analyses))
        with transfers
        with translations
        with ntransfers = 0
        with nrealizations = 0
        with ntranslations = 0
        with first = 0
        with best = 0
        for i from 1 to n
        for result in analyses
        for aid = (get-field :result-id result)
        for aflags = (let* ((flags (get-field :flags result))
                            (score (get-field :score result)))
                       ;;
                       ;; _fix_me_
                       ;; PET (as of today) does not return a :flags field, but
                       ;; it does return the parse ranking score (though with
                       ;; only one significant bit, we neeed to fix that too).
                       ;; hence, convert from old-style conversions to the new
                       ;; :flags mechanism, but aim to update PET eventually.
                       ;;                                        (18-nov-08; oe)
                       (if (and (null flags) (numberp score))
                         (acons :ascore score nil)
                         flags))
        for ascore = (get-field :ascore aflags)
        for transfer
        = (pvm-process
           parse :transfer :result-id aid :filter filter
           :nanalyses (second nanalyses))
        for realizations = nil
        when (numberp ascore) do
          (when (or (null amin) (< ascore amin)) (setf amin ascore))
          (when (or (null amax) (> ascore amax)) (setf amax ascore))
        do
          
          (incf ntransfers (length (get-field :results transfer)))
          (case format
            (:ascii
             (format
              stream
              "|~%|-[~a] # ~a~@[ {~,2f}~]"
              (current-time :long :since :treal start) aid ascore))
            (:html
             (format
              stream
              "<tr><td class=\"flowLeftBorder\"></td>~
                   <td class=\"flowLeft\"></td><td colspan=2></td>~%~
                   <td class=\"flowRightBorder\"></td>~%~
               <tr><td class=\"flowLeftBorder\"></td>~
                   <td class=\"flowLeft\"><img src=\"1x20.jpg\"></td>~
                   <td class=\"flow\" colspan=2>[~a] # ~a~@[ {~,2f}~]"
               (current-time :long :since :treal start) aid ascore)))
          (print-result
           transfer :stream stream :format format :index index :format format)
          (case format
            (:html
             (nconc transfer (acons :www index nil)) (incf index)
             (format stream "</td><td class=\"flowRightBorder\"></td>~%")))
          (force-output stream)
          (loop
              with analyses = (get-field :results transfer)
              with n
              = (if (consp nanalyses) (second nanalyses) (length analyses))
              for j from 1 to n
              for result in (get-field :results transfer)
              for tid = (get-field :result-id result)
              for tflags = (get-field :flags result)
              for tscore = (get-field :tscore tflags)
              for realization
              = (pvm-process
                 transfer :generate :result-id tid :filter filter
                 :nanalyses (third nanalyses))
              when (numberp tscore) do
                (when (or (null tmin) (< tscore tmin)) (setf tmin tscore))
                (when (or (null tmax) (> tscore tmax)) (setf tmax tscore))
              do
                (incf nrealizations (length (get-field :results realization)))
                (case format
                  (:ascii
                   (format
                    stream
                    "| |~%| |-[~a] # ~a~@[ {~,2f}~]"
                    (current-time :long :since :treal start) tid tscore))
                  (:html
                   (format
                    stream
                    "<tr><td class=\"flowLeftBorder\"></td>~
                         <td class=\"flowLeft\"></td>~
                         <td class=\"flowLeft\"></td><td></td>~
                         <td class=\"flowRightBorder\"></td>~%~
                     <tr><td class=\"flowLeftBorder\"></td>~
                         <td class=\"flowLeft\"></td>~
                         <td class=\"flowLeft\"><img src=\"1x20.jpg\"></td>~
                         <td class=\"flow\">[~a] # ~a~@[ {~,2f}~]"
                    (current-time :long :since :treal start) tid tscore)))
                (print-result
                 realization :stream stream :index index :format format)
                (case format
                  (:html
                   (nconc realization (acons :www index nil)) (incf index)
                   (format 
                    stream
                    "</td><td class=\"flowRightBorder\"></td>~%")))
                (loop
                    with results = (get-field :results realization)
                    for result in results
                    for rid = (get-field :result-id result)
                    for surface = (get-field :surface result)
                    for rflags = (get-field :flags result)
                    for rscore = (or (get-field :score result)
                                     (when (consp rflags)
                                       (get-field :rscore rflags)))
                    for lm 
                    = (let ((lm (and (consp rflags) (get-field :lm rflags))))
                        (when (numberp lm) (- lm)))
                    for distortion
                    = (and (consp rflags) (get-field :distortion rflags))
                    for distance = (let ((foo (when (consp rflags)
                                                (get-field
                                                 :distance rflags))))
                                     (when (and (numberp foo) (> foo 0))
                                       foo))
                    for scores = (let ((scores
                                        (score-strings 
                                         (list surface) targets
                                         :source input :type types)))
                                   (loop
                                       for score in scores
                                       collect (first score)))
                    for bleu = (first scores)
                    for smt = #+:logon
                              (first
                               (mt::smt-score-strings
                                input (list surface)))
                              #-:logon
                              0
                    for lfn = (first smt) for lnf = (rest smt)
                    when (numberp bleu) do (setf best (max best bleu))
                    when (numberp rscore) do
                      (when (or (null rmin) (< rscore rmin))
                        (setf rmin rscore))
                      (when (or (null rmax) (> rscore rmax))
                        (setf rmax rscore))
                    when (numberp lm) do
                      (when (or (null lmin) (< lm lmin)) (setf lmin lm))
                      (when (or (null lmax) (> lm lmax)) (setf lmax lm))
                    when (numberp distortion) do
                      (when (or (null dmin) (< distortion dmin))
                        (setf dmin distortion))
                      (when (or (null dmax) (> distortion dmax))
                        (setf dmax distortion))
                    when (numberp lfn) do
                      (when (or (null fnmin) (< lfn fnmin)) (setf fnmin lfn))
                      (when (or (null fnmax) (> lfn fnmax)) (setf fnmax lfn))
                    when (numberp lnf) do
                      (when (or (null nfmin) (< lnf nfmin)) (setf nfmin lnf))
                      (when (or (null nfmax) (> lnf nfmax)) (setf nfmax lnf))
                    do
                      (let ((flags
                             (append
                              (pairlis types scores)
                              (pairlis '(:aid :tid :rid) (list aid tid rid))
                              (pairlis '(:lfn :lnf) (list lfn lnf))
                              aflags tflags rflags)))
                        (if (get-field :flags result)
                          (setf (get-field :flags result) flags)
                          (nconc result (acons :flags flags nil))))
                      (nconc result (pairlis '(:bleu) (list bleu)))
                      (case format
                        (:ascii
                         (format
                          stream
                          "| |   |~a|~@[ [~a]~] ~
                           {~@[~,2f~]|~@[~,2f~]~
                            |~@[~,2f~]|~@[~,2f~]|~@[~,2f~]} ~
                           <~@[~,2f~]>~%"
                          surface distance
                          lm rscore
                          distortion lfn lnf
                          bleu))
                        (:html
                         (format
                          stream
                          "<tr><td class=\"flowLeftBorder\"></td>~
                               <td class=\"flowLeft\"></td>~
                               <td class=\"flowLeft\"></td>~
                               <td class=\"flow\">    ~
                                 |~a|~@[ [~a]~] ~
                                 {~@[~,2f~]|~@[~,2f~]~
                                  |~@[~,2f~]|~@[~,2f~]|~@[~,2f~]}</td>~
                               <td class=\"flowRightBorder\"></td>~%"
                          surface 
                          distance lm rscore
                          distortion lfn lnf)))
                      (force-output stream)
                    when (and (stringp surface) (numberp rscore))
                    do
                      (let* ((flags (get-field :flags result))
                             (translation
                              (pairlis '(:aid :tid :rid :string :flags
                                         :ascore :tscore :rscore :lm
                                         :distortion :distance
                                         :lfn :lnf :bleu)
                                       (list aid tid rid surface flags
                                             ascore tscore rscore lm
                                             distortion distance
                                             lfn lnf bleu)))
                             (score (when (model-p %model%)
                                      (mem-score-result
                                       result %model% :normalizep :minmax))))
                        (when score
                          (nconc translation (acons :score score nil)))
                        (push translation translations))
                      (incf ntranslations))
                (force-output stream)
                (push realization realizations))
          (push 
           (acons :realizations (nreverse realizations) transfer)
           transfers)
        finally
          (when (eq format :html)
            (format
             stream
             "<tr><td class=\"flowLeftBorder\"></td>~
                  <td class=\"flow\" colspan=3>&nbsp;</td>~
                  <td class=\"flowRightBorder\"></td>~%~
              </tbody><tbody id=\"fanOutSummary\">~%"))

          ;;
          ;; now eliminate duplicates, making sure to preserve outputs with the
          ;; maximum aggregate score (however that was defined :-).  note that
          ;; .translations. at this point is in reverse chronological order, so
          ;; throwing away everything but the first output with maxium score is
          ;; an implicit way of keeping outputs found early on in fan-out.
          ;;
          (let ((map (make-hash-table :test #'equal))
                (scores (make-hash-table :test #'equal)))
            (loop
                with arange = (and (numberp amin) (numberp amax) (- amax amin))
                with trange = (and (numberp tmin) (numberp tmax) (- tmax tmin))
                with rrange = (and (numberp rmin) (numberp rmax) (- rmax rmin))
                with lrange = (and (numberp lmin) (numberp lmax) (- lmax lmin))
                with drange = (and (numberp dmin) (numberp dmax) (- dmax dmin))
                with fnrange
                = (and (numberp fnmin) (numberp fnmax) (- fnmax fnmin))
                with nfrange
                = (and (numberp nfmin) (numberp nfmax) (- nfmax nfmin))
                for translation in translations
                for string = (get-field :string translation)
                for ascore = (get-field+ :ascore translation 0)
                for tscore = (get-field+ :tscore translation 0)
                for rscore = (get-field+ :rscore translation 0)
                for lm = (get-field+ :lm translation 0)
                for distortion = (get-field+ :distortion translation 0)
                for lfn = (get-field+ :lfn translation 0)
                for lnf = (get-field+ :lnf translation 0)
                for distance = (get-field+ :distance translation 0)
                for score
                = (or (get-field :score translation)
                      (+ (* *mt-analysis-weight*
                            (if (and ascore arange)
                              (divide (- ascore amin) arange)
                              0))
                         (* *mt-transfer-weight*
                            (if (and tscore trange)
                              (divide (- tscore tmin) trange)
                              0))
                         (* *mt-realization-weight*
                            (if (and rscore rrange)
                              (divide (- rscore rmin) rrange)
                              0))
                         (* *mt-lm-weight*
                            (if (and lm lrange)
                              (divide (- lm lmin) lrange)
                              0))
                         (* *mt-distortion-weight*
                            (if (and distortion drange)
                              (divide (- distortion dmin) drange)
                              0))
                         (* *mt-lfn-weight*
                            (if (and lnf fnrange)
                              (divide (- lfn fnmin) fnrange)
                              0))
                         (* *mt-lnf-weight*
                            (if (and lnf nfrange)
                              (divide (- lnf nfmin) nfrange)
                              0))
                         (- distance)))
                unless (get-field :score translation)
                do (nconc translation (acons :score score nil))
                when (or (null (gethash string scores))
                         (> score (gethash string scores)))
                do
                  (setf (gethash string scores) score)
                  (setf (gethash string map) (acons :score score translation)))
            
            (setf translations
              (sort
               translations
               #'> :key #'(lambda (foo) (get-field :score foo))))
            (loop
                with last = (get-field :score (first translations))
                with i = 1 with j = 2
                for translation in translations
                for score = (get-field :score translation)
                for flags = (get-field :flags translation)
                unless (= score last) do
                  (setf i j) (setf last score) (incf j)
                do (nconc flags (pairlis '(:score :rank) (list score i))))
            (setf translations
              (loop
                  for translation being each hash-value in map
                  collect translation)))
                        
          (setf translations
            (sort
             translations
             #'> :key #'(lambda (foo) (get-field :score foo))))
          (loop
              with n
              = (if (consp nanalyses) (fourth nanalyses) (length translations))
              initially
                (let ((n (length translations)))
                  (case format
                    (:ascii
                     (format
                      stream
                      "|~%|< ~:[~;*~]|~a|~@[ (~a)~] --- ~
                       ~:[~;^~]~a x ~a x ~a = ~
                       ~:[~*~a~;~a [~a]~]~%"
                      (and (numberp wf) (zerop wf)) input id
                      (and (numberp nfragments) (> nfragments 0))
                      (length analyses) ntransfers nrealizations
                      (not (= ntranslations n)) n ntranslations)
                     (loop
                         for target in targets
                         do (format stream "|@ |~a|~%" target)))
                    (:html
                     (format
                      stream
                      "<tr><td class=\"flowLeftBorder\"></td>~
                           <td class=\"flow\" colspan=3>~
                             ~:[~;*~]<span class=\"flowInput\">~a</span>~
                             ~@[ (~a)~] &mdash; ~:[~;^~]~a x ~a x ~a = ~
                             ~:[~*~a~;~a [~a]~]</td>~
                           <td class=\"flowRightBorder\"></td>~%"
                      (and (numberp wf) (zerop wf)) input id 
                      (and (numberp nfragments) (> nfragments 0))
                      (length analyses) ntransfers nrealizations
                      (not (= ntranslations n)) n ntranslations))))
              for translation in translations
              for i from 1 to n
              when (eq translation (first translations))
              do (setf first (get-field :bleu translation))
              do
                (case format
                  (:ascii
                   (format
                    stream
                    "|> |~@[~a~]|~@[ [~a]~] ~
                     {~@[~,2e~]} <~@[~,2f~]> (~a:~a:~a).~%"
                    (get-field :string translation) 
                    (get-field :distance translation) 
                    (get-field :score translation) 
                    (get-field :bleu translation)
                    (get-field :aid translation)
                    (get-field :tid translation)
                    (get-field :rid translation)))
                  (:html
                   (format
                    stream
                    "<tr><td class=\"flowLeftBorder\"></td>~
                         <td class=\"flow\" colspan=3>~
                           <span class=\"flowOutput\">~@[~a~]</span>~
                           ~@[ [~a]~] {~@[~,2e~]} ~
                           (~a:~a:~a).</td>~
                         <td class=\"flowRightBorder\"></td>~%"
                    (get-field :string translation) 
                    (get-field :distance translation) 
                    (get-field :score translation) 
                    (get-field :aid translation)
                    (get-field :tid translation)
                    (get-field :rid translation)))))
          (case format
            (:html
             (format 
              stream 
              "<tr><td class=\"flowBottomBorder\" colspan=5></td></table~%~
               <script type=\"text/javascript\" language=\"javascript\">~
                 new Effect.ScrollTo('fanOutSummary');</script>~%")))

          (force-output stream)
          (return (append (pairlis '(:transfers :translations
                                     :fbleu :bbleu)
                                   (list (nreverse transfers) translations
                                         first best))
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
         (analysis (remote-grammar :parse))
         (transfer (remote-grammar :transfer))
         (realization (remote-grammar :generate)))
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
           readings=\"~a\"~@[ nfragments=\"~a\"~]~@[ tcpu=\"~d\"~]>~%"
     prefix type
     (when (eq type :analysis) (get-field :i-id item))
     (get-field :readings item) 
     (get-field :nfragments item) (get-field :tcpu item))

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
             "~a  <output score=\"~,2f\"~@[ bleu=\"~,4f\"~]>~a</output>~%"
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
        for string = (when (eq type :realization) (get-field :surface result))
        for score = (get-field :score result)
        for bleu = (get-field :bleu result)
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
           "~a  <result id=\"~a\"~@[ edges=\"~a\"~]~@[ score=\"~,4f\"~]>~%"
           prefix id edges score)
        when string
        do
          (format
           stream
           "~a    <output~@[ bleu=\"~,4f\"~]>~a</output>~%"
           prefix bleu (normalize-string string) prefix)
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


(defun translate-item (string
                       &key id (wf 1) length exhaustive nanalyses trace
                            edges derivations semantix-hook trees-hook
                            (filter *process-suppress-duplicates*)
                            burst (nresults 0) targets)
  (declare (ignore exhaustive derivations edges
                   semantix-hook trees-hook nresults))
  
  #+:logon
  (when *mt-engine*
    (return-from translate-item
      (www-translate-item
       string :engine *mt-engine* :id id :wf wf :targets targets
       :burst burst)))
  
  (let* ((length (or length (+ 1 (count #\space string))))
         (stream (make-string-output-stream))
         (log (make-string-output-stream))
         (*standard-output* 
          (if trace (make-broadcast-stream *standard-output* stream) stream))
         (start (get-internal-run-time)) stop)

    (multiple-value-bind (return condition)
        (ignore-errors
         (when (or (not (stringp string)) (string= string ""))
           (error "null or malformed input string"))
         (loop
             for task in '(:parse :transfer :generate)
             do
               (unless (loop
                           for client in *pvm-clients*
                           for cpu = (client-cpu client)
                           when (and (smember task (cpu-task cpu))
                                     (eq (client-status client) :ready))
                           return client)
                 (error "no ~(~a~) PVM client" task)))
         
         (let* ((item (translate-string
                       string :id id :wf wf :nanalyses nanalyses :stream log
                       :filter filter :targets targets))
                (tgc 0) (tcpu 0) (treal 0) (conses 0) (symbols 0) (others 0)
                (total 0) (readings 0) outputs (errors "") 
                (nparses 0) (ntransfers 0) (nrealizations 0)
                (nfragments (get-field :nfragments item))
                (ntranslations (length (get-field :translations item))))
           
           (setf stop (get-internal-run-time))
                
           (incf total (get-field+ :total item 0))
           (incf tgc (get-field+ :tgc item 0))
           (incf tcpu (get-field+ :tcpu item 0))
           (incf treal (get-field+ :treal item 0))
           (incf conses (get-field+ :conses item 0))
           (incf symbols (get-field+ :symbols item 0))
           (incf others (get-field+ :others item 0))
           
           (let ((error (get-field :error item)))
             (when (and error (not (equal error "")))
               (setf errors (format nil "[]:|~a|" error))))
           
           (loop
               for transfer in (get-field :transfers item)
               for result in (get-field :results item)
               for pid = (get-field :result-id result)
               do
                 (incf nparses)
                 (incf total (get-field+ :total transfer 0))
                 (incf tgc (get-field+ :tgc transfer 0))
                 (incf tcpu (get-field+ :tcpu transfer 0))
                 (incf treal (get-field+ :treal transfer 0))
                 (incf conses (get-field+ :conses transfer 0))
                 (incf symbols (get-field+ :symbols transfer 0))
                 (incf others (get-field+ :others transfer 0))
                 (let ((error (get-field :error transfer)))
                   (when (and error (not (equal error "")))
                     (setf errors 
                       (format nil "~a [~a]:|~a|" errors pid error))))
                 (loop
                     for realization in (get-field :realizations transfer)
                     for result in (get-field :results transfer)
                     for tid = (get-field :result-id result)
                     do
                       (incf ntransfers)
                       (incf total (get-field+ :total realization 0))
                       (incf tgc (get-field+ :tgc realization 0))
                       (incf tcpu (get-field+ :tcpu realization 0))
                       (incf treal (get-field+ :treal realization 0))
                       (incf conses (get-field+ :conses realization 0))
                       (incf symbols (get-field+ :symbols realization 0))
                       (incf others (get-field+ :others realization 0))
                       (let ((error (get-field :error realization)))
                         (when (and error (not (equal error "")))
                           (setf errors 
                             (format 
                              nil
                              "~a [~a:~a]:|~a|"
                              errors pid tid error))))
                       (loop
                           for result in (get-field :results realization)
                           for ratio 
                           = (let* ((surface (get-field :surface result))
                                    (olength (+ 1 (count #\space surface))))
                               (and surface (float (divide olength length))))
                           for flags = (and ratio (acons :ratio ratio nil))
                           do
                             (incf nrealizations)
                             (when flags
                               (if (get-field :flags result)
                                 (nconc (get-field :flags result) flags)
                                 (nconc result (acons :flags flags nil))))
                             (push (acons :result-id readings result) outputs)
                             (incf readings))))
           
           `((:others . ,others) (:symbols . ,symbols) (:conses . ,conses)
             (:treal . ,treal) (:tcpu . ,tcpu) (:tgc . ,tgc)
             (:readings . ,readings) (:total . ,total) (:error . ,errors)
             (:comment . ,(format 
                           nil
                           "(:nanalyses . ~s) (:ntransfers . ~a) ~
                            (:nrealizations . ~a) (:ntranslations . ~a)"
                           nparses ntransfers nrealizations ntranslations))
             (:trace . ,(get-output-stream-string log))
             (:results . ,outputs)
             (:nfragments . ,nfragments)
             (:fan . ,(list nparses ntransfers nrealizations ntranslations))
             (:fbleu . ,(get-field :fbleu item))
             (:bbleu . ,(get-field :bbleu item)))))
           
      (unless stop (setf stop (get-internal-run-time)))

      (append
       (when condition
         (let ((error (normalize-string 
                       (format nil "~a" condition)))
               (total (round 
                       (* (- stop start) 1000) 
                       internal-time-units-per-second)))
           (pairlis '(:readings :condition :error :total)
                    (list -1 (unless burst condition) error total))))
       return))))

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
                    (format nil "`~a' Parse Results" string)))
       (let ((error (get-field :error item)))
       (when (and error (not (equal error "")))
         (format
          #+:allegro excl:*initial-terminal-io* #-:allegro t
          "parse-interactively(): error `~a'.~%"
          (normalize-string error))))))))

(defparameter *string-similarity-punctuation-characters*
  '(#\. #\! #\? #\, #\: #\|))

(defun string-similarity-normalize (string)
  (when string
    (loop
        with result = (make-array (length string)
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        unless (member
                c *string-similarity-punctuation-characters*
                :test #'char=) 
        do (vector-push (char-downcase c) result)
        finally (return result))))

(defparameter *string-similarity-default* :bleu)

(defparameter *string-similarity-locks*
  (list
   (cons :torbjoern (mp:make-process-lock))
   (cons :wa (mp:make-process-lock))
   (cons :waft (mp:make-process-lock))))

;;;
;;; _fix_me_
;;; the actual values are reset in initialize-tsdb().            (8-may-07; oe)
;;;
(defparameter *string-similarity-binaries*
    (let* ((root (system:getenv "LOGONROOT"))
           (root (and root (namestring (parse-namestring root)))))
      (when root
        (list
         (cons :torbjoern (format nil "~a/ntnu/bleu/bleu.pl" root))
         (cons :wa (format nil "~a/ntnu/bleu/wa.pl" root))
         (cons :waft (format nil "~a/ntnu/bleu/wa.pl -t" root))))))

(defparameter *string-similarity-streams*
  (loop
      for measure in *string-similarity-binaries*
      collect (cons (first measure) nil)))

(defparameter *string-similarity-pids*
  (loop
      for measure in *string-similarity-binaries*
      collect (cons (first measure) nil)))

(defun string-similarity-shutdown
    (&optional (type *string-similarity-default*))
  (let ((lock (get-field type *string-similarity-locks*))
        (stream (get-field type *string-similarity-streams*))
        (pid (get-field type *string-similarity-pids*)))
    (mp:with-process-lock (lock)
      (when stream
        (ignore-errors
         (close stream)
         (setf (get-field type *string-similarity-streams*) nil)))
      (when pid
        (ignore-errors
         ;; JAC 29-Oct-2018: first 2 args in each of the following 3 calls were incorrect so
         ;; they would always have failed
         (run-process (format nil "kill -HUP ~d" pid)
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process (format nil "kill -TERM ~d" pid)
                      :wait t :output "/dev/null" :error-output "/dev/null")
         (run-process (format nil "kill -QUIT ~d" pid)
                      :wait t :output "/dev/null" :error-output "/dev/null"))
        (sys:os-wait nil pid)
        (setf (get-field type *string-similarity-pids*) nil)))))

(defun string-similarity-initialize
    (&optional (type *string-similarity-default*))
  (let ((lock (get-field type *string-similarity-locks*))
        (binary (get-field type *string-similarity-binaries*)))
    (when lock
      (mp:with-process-lock (lock)
      
        (when (get-field type *string-similarity-streams*) 
          (string-similarity-shutdown type))
        
        (multiple-value-bind (stream foo pid)
            (run-process 
             (format nil "~a" binary)
             :wait nil
             :output :stream :input :stream
             :error-output "/dev/null" :if-error-output-exists :append)
          (declare (ignore foo))
          (setf (get-field type *string-similarity-streams*) stream)
          (setf (get-field type *string-similarity-pids*) pid))))))

(defun score-strings (translations
                      &optional references
                      &key (type *string-similarity-default*) source scrub)
  (unless (consp translations) (setf translations (list translations)))
  (when (consp type)
    (return-from score-strings
      (loop
          for foo in type
          for scores = (score-strings
                        translations references :type foo
                        :source source :scrub scrub)
          collect scores)))
  (when (null references)
    (return-from score-strings
      (loop repeat (length translations) collect 0)))
  #-:logon
  (return-from score-strings
    (loop repeat (length translations) collect 0))
  #+:logon
  (case type
    ((:torbjoern :wa :waft)
     (let ((lock (get-field type *string-similarity-locks*)))
       (mp:with-process-lock (lock)
         (when (null (get-field type *string-similarity-streams*))
           (string-similarity-initialize type))
         (let ((scores)
               (stream (get-field type *string-similarity-streams*)))
           (when references
             (format stream "SOURCE~@[ ~a~]~%" source)
             (loop
                 for reference
                 in (if scrub (mt::scrub-strings references) references)
                 do
                   (format
                    stream
                    "REF ~a~%"
                    (string-similarity-normalize reference))))
           (loop
               for translation
               in (if scrub (mt::scrub-strings translations) translations)
               do 
                 (format
                  stream
                  "TRANS ~a~%"
                  (string-similarity-normalize translation))
                 (force-output stream)
                 (let ((score (read stream nil nil)))
                   (if (numberp score) (push score scores) (return))))
           (nreverse scores)))))
    ((:bleu :nist :neva)
     (loop 
         for translation 
         in (if scrub (mt::scrub-strings translations) translations)
         collect (score-string 
                  (string-similarity-normalize translation)
                  (loop
                      for reference in references
                      collect (string-similarity-normalize reference))
                  :type type)))
    (t (error "score-strings(): unrecognized measure type: ~a" type))))

(defun score-string (test references
                      &key (type :bleu)
                           (sl "NO") (tl "EN") (did "D0"))

  (labels ((make-file (strings &key (type :ref) name)
             (let* ((strings (if (consp strings) strings (list strings)))
                    (tag (case type 
                           (:ref "refset")
                           (:tst "tstset")
                           (:src "srcset")
                           (t (error
                               "score-string(): unrecognized type `~a'"
                               type))))
                    (name (or name
                              (format
                               nil "/tmp/.mteval.~(~a~).~a.~a.sgm"
                               type (current-user) (current-pid)))))
               (with-open-file (stream name :direction :output 
                                :if-exists :supersede :if-not-exists :create)
                 (format
                  stream
                  "<~a setid=\"FOO\" srclang=\"~a\" trglang=\"~a\">~%"
                  tag sl tl)
                 (loop
                     for string in strings
                     for i from 0
                     do
                       (format
                        stream
                        "  <doc docid=\"~a\" sysid=\"~a~a\">~%    ~
                             <seg>~a</seg>~%  ~
                           </doc>~%"
                        did type i (xml-escape-string string)))
                 (format stream "</~a>~%" tag))
               name)))

    (let* ((ofile
            (format 
             nil 
             "/tmp/.mteval.out.~a.~a" 
             (current-user) (current-pid)))
           (sfile (make-file "dummy" :type :src))
           (rfile (make-file references :type :ref))
           (tfile (make-file test :type :tst))
           (command
            (let ((root (namestring 
                         (parse-namestring 
                          (system:getenv "LOGONROOT")))))
              (unless root
                (error
                 "mteval-string(): LOGONROOT env variable not specified"))
              (case type 
                ((:bleu :nist)
                 (format
                  nil
                  "perl ~a/nist/mteval-v11b.pl ~:[~;-b~]"
                  root (eq type :bleu)))
                ((:neva)
                 (format nil "perl ~a/nist/neva.pl -b" root))
                (t
                 (error "mteval-string(): unrecognized measure `~a'" type)))))
           (command (format
                     nil
                     "~a -r '~a' -s '~a' -t '~a'"
                     command rfile sfile tfile)))
      (when command
        (run-process
         command :wait t :output ofile :if-output-exists :supersede))
      (with-open-file (stream ofile :direction :input)
        (loop
            for line = (read-line stream nil nil)
            for score
            = (multiple-value-bind (match capture)
                  (ppcre::scan-to-strings "score = ([0-9.]+)" line)
                (declare (ignore match))
                (when capture (read-from-string (aref capture 0))))
            when (or score (null line)) return score)))))

(defun mteval (data
               &key items condition (type :first) enhancers
                    oracle filter h r
                    (sl "NO") (tl "EN") (sid "LOGON") (did "D0")
                    (output t) (verbose t))
  (declare (special %model%))
  (let* ((uscanner (ppcre:create-scanner "/([^/]*)/"))
         (fscanner (ppcre:create-scanner " \\|\\|"))
         (total (if items (length items) 0))
         (items (if items
                  items
                  (loop
                      with items
                      = (let ((items (analyze
                                      data :condition condition
                                      :output t :thorough '(:surface :flags))))
                          (loop
                              for enhancer in enhancers
                              do
                                (loop
                                    for item in items
                                    do (call-raw-hook enhancer item)))
                          items)
                      for item in items
                      for results
                      = (loop
                            for result in (get-field :results item)
                            for surface = (get-field :surface result)
                            for output
                            = (ppcre:regex-replace-all
                               uscanner
                               (ppcre:regex-replace-all fscanner surface "")
                               "\\1")
                            for flags = (get-field :flags result)
                            when (and flags (not (consp flags)))
                            do 
                              (setf (get-field :flags result)
                                (ignore-errors (read-from-string flags)))
                            collect (acons :surface output result))
                      for source = (get-field :i-input item)
                      for references
                      = (loop
                            for output in (get-field :outputs item)
                            collect (get-field :o-surface output))
                      for test
                      = (cond
                         ((null results) nil)
                         ((eq type :random)
                          (let* ((n (random (length results)))
                                 (current (nth n results)))
                            (get-field :surface current)))
                         ((eq type :first)
                          (loop
                              with current = (first results)
                              with id = (get-field :result-id current)
                              for result in results
                              when (< (get-field :result-id result) id)
                              do
                                (setf current result)
                                (setf id (get-field :result-id result))
                              finally (return (get-field :surface current))))
                         ((eq type :top)
                          (loop
                              with current = (first results)
                              with rank
                              = (get-field :rank (get-field :flags current))
                              for result in results
                              for match
                              = (get-field :rank (get-field :flags result))
                              when (and rank match (< match rank))
                              do (setf current result) (setf rank match)
                              finally
                                (when rank
                                  (return (get-field :surface current)))))
                         ((smember type '(:neva :wa :bleu :torbjoern))
                          (loop
                              with current = (first results)
                              with value
                              = (get-field type (get-field :flags current))
                              for result in results
                              for match
                              = (get-field type (get-field :flags result))
                              when (and value match (> match value))
                              do (setf current result) (setf value match)
                              finally
                                (when value
                                  (return (get-field :surface current)))))
                         ((and (eq type :oracle) (hash-table-p oracle))
                          (let* ((input (get-field :i-input item))
                                 (match (and oracle (gethash input oracle)))
                                 (output
                                  (loop
                                      for result in results
                                      for output = (get-field :surface result)
                                      when (equal match output)
                                      return output)))
                            (if output
                              output
                              (format
                               t
                               "mteval(): oracle miss |~a|.~%" input))))
                         ((eq type :mem)
                          (loop
                              with current with best
                              for result in results
                              for score = (mem-score-result
                                           result %model%
                                           :normalizep :minmax)
                              collect score into scores
                              when (or (null best) (< best score))
                              do
                                (setf current result)
                                (setf best score)
                              finally
                                (return
                                  (if (or h r)
                                    (let* ((output
                                            (get-field :surface current))
                                           (scores (sort scores #'>))
                                           (n (length scores))
                                           (probabilities
                                            (scores-to-probabilities scores))
                                           (entropy
                                            (divide (entropy probabilities)
                                                    (sqrt n)))
                                           (ratio
                                            (divide
                                             (first probabilities)
                                             (second probabilities)))
                                           (neva (first
                                                  (score-strings 
                                                   output
                                                   references :type :neva))))
                                      (when (eq verbose :entropy)
                                        (format
                                         t
                                         "[~a] H = ~,4f R = ~,2f <~,4f>~%"
                                         (get-field :i-id item)
                                         entropy ratio neva))
                                      (when (or (null (rest scores))
                                                (and h (< entropy h))
                                                (and r (> ratio r)))
                                        output))
                                    (get-field :surface current))))))
                      do (incf total)
                      when (or test (null filter))
                      collect (list* source (or test "") references))))
         (sfile (format
                 nil
                 "/tmp/.mteval.~a.~a.source"
                 (current-user) (current-pid)))
         (rfile (format
                 nil
                 "/tmp/.mteval.~a.~a.references"
                 (current-user) (current-pid)))
         (tfile (format
                 nil
                 "/tmp/.mteval.~a.~a.test"
                 (current-user) (current-pid)))
         (ofile (format
                 nil
                 "/tmp/.mteval.~a.~a.output"
                 (current-user) (current-pid)))
         (command
          (let* ((root (system:getenv "LOGONROOT"))
                 (root (and root (namestring (parse-namestring root)))))
            (when root
              (format
               nil
               "perl ~a/nist/mteval-v11b.pl -s '~a' -r '~a' -t '~a'"
               root sfile rfile tfile))))
         (ndocuments
          (loop for foo in items maximize (length (rest (rest foo)))))
         (documents (make-array ndocuments)))
    (with-open-file (sstream sfile :direction :output :if-exists :supersede)
      (with-open-file (rstream rfile :direction :output :if-exists :supersede)
        (with-open-file (tstream tfile
                         :direction :output :if-exists :supersede)
        (format
         sstream
         "<srcset setid=\"S0\" srclang=\"~a\">~%<doc docid=\"~a\">~%"
         sl did)
        (format
         rstream
         "<refset setid=\"R0\" srclang=\"~a\" trglang=\"~a\">~%" sl tl)
        (format
         tstream
         "<tstset setid=\"T0\" srclang=\"~a\" trglang=\"~a\">~%~
          <doc docid=\"~a\" sysid=\"~a\">~%"
         sl tl did sid)
        (loop
            for (source test . references) in items
            do
              (format sstream "  <seg>~a</seg>~%" (xml-escape-string source))
              (format tstream "  <seg>~a</seg>~%" (xml-escape-string test))
              (loop
                  for i from 0 to (- ndocuments 1)
                  for reference = (or (nth i references) "")
                  do (push reference (aref documents i))))
        (loop
            for i from 0
            for document across documents
            do
              (format rstream "<doc docid=\"~a\" sysid=\"~a\">~%" did i)
              (loop
                  for reference in (nreverse document)
                  do
                    (format
                     rstream
                     "  <seg>~a</seg>~%"
                     (xml-escape-string reference)))
              (format rstream "</doc>~%"))
        (format sstream "</doc>~%</srcset>~%")
        (format rstream "</refset>~%")
        (format tstream "</doc>~%</tstset>~%"))))
    (when command
      (run-process
       command :wait t :output ofile :if-output-exists :supersede
       :error-output "/dev/null" :if-error-output-exists :append))
    (when output
      (with-open-file (stream ofile :direction :input)
        (loop
            with scanner
            = (ppcre::create-scanner
               "NIST score = ([0-9.]+) +BLEU score = ([0-9.]+) for system")
            with bleu = 0 with nist = 0
            for i from 0
            for line = (read-line stream nil nil)
            while line
            do
              (multiple-value-bind (start end starts ends)
                  (ppcre:scan scanner line)
                (when (and start end)
                  (let ((foo (subseq line (aref starts 0) (aref ends 0)))
                        (bar (subseq line (aref starts 1) (aref ends 1))))
                    (setf nist (ignore-errors (read-from-string foo)))
                    (setf bleu (ignore-errors (read-from-string bar))))))
            when (and verbose (> i 1) (< i 25))
            do (format output "~a~%" line)
            finally (return (pairlis
                             '(:total :active :bleu :nist)
                             (list total (length items) bleu nist))))))))

(defun read-oracle (file &key (oracle (make-hash-table :test #'equal))
                              verbose)
  (with-open-file (stream file)
    (loop
        with iscanner = (ppcre:create-scanner
                         "^\\|<[ \\*]+\\|(.*)\\| \\(([0-9]+)\\) --- ")
        with oscanner = (ppcre:create-scanner
                         "^x\\|> \\|(.*)\\|.+\\{([0-9.-]+)\\} <[0-9.]+>")
        with current
        for line = (read-line stream nil nil)
        while line
        do
          (multiple-value-bind (start end starts ends)
              (ppcre:scan iscanner line)
            (if (and start end)
              (let ((input (subseq line (aref starts 0) (aref ends 0)))
                    (id (subseq line (aref starts 1) (aref ends 1))))
                (setf current input)
                (when verbose (format t "< |~a| (~a)~%" input id)))
              (multiple-value-bind (start end starts ends)
                  (ppcre:scan oscanner line)
                (when (and start end)
                  (let ((output (subseq line (aref starts 0) (aref ends 0)))
                        (match (gethash current oracle)))
                    (when (and match (not (equal match output)))
                      (format
                       t
                       "read-oracle(): unexpected collision:~%  ~
                        |~a|~%   |~a|~%   |~a|~%~%"
                       current match output))
                    (setf (gethash current oracle) output)
                    (when verbose (format t "> |~a|~%" output)))))))))
  oracle)

#+:logon
(let ((vurl "http://www.gramtrans.com")
      (vscanner (ppcre:create-scanner "<blockquote[^<]*><br/>([^<]+)"))
      (nurl "http://babel.hf.ntnu.no/babelcgi/translate/translate.cgi/right")
      (nscanner (ppcre:create-scanner
                 "translation</[hH]1><[bB]>([^<]+)" :case-insensitive-mode t))
      (iurl "http://www.tranexp.com:2000/Translate/result.shtml")
      (iscanner "\"translation\" *>([^<]+)")
      (gurl "http://www.google.com/translate_t?sl=no&tl=en")
      (gscanner "<div id=\"?result_box\"?[^>]*>([^<]+)"))
  (defun www-translate-item (input
                             &key (engine (or *mt-engine* :google))
                                  (format :ascii)
                                  (source :no) (target :en)
                                  id (wf 1) exhaustive nanalyses trace
                                  edges derivations semantix-hook trees-hook
                                  filter (nresults 0) targets burst)
    (declare (ignore exhaustive nanalyses edges derivations
                     semantix-hook trees-hook filter nresults))
    
    (let* ((stream (make-string-output-stream))
           (log (make-string-output-stream))
           (*standard-output* 
            (if trace (make-broadcast-stream *standard-output* stream) stream))
           (ua (or (and (consp engine) (get-field :ua engine))
                   "LOGON MT Syndicator"))
           (source (or source
                       (and (consp engine) (get-field :source engine))))
           (target (or target
                       (and (consp engine) (get-field :target engine))))
           (delay (and (consp engine) (get-field :delay engine)))
           (engine (if (consp engine) (get-field :engine engine) engine))
           start stop)

      (when (consp delay) 
        (sleep (+ (max (first delay) (random (rest delay)))
                  (if (eq engine :oa) 30 0))))
      
      (setf start (get-internal-real-time))
      (case format
        (:ascii
         (format
          log
          "[~a]~@[ (~a)~] ~:[~;*~]|~a|"
          (current-time :long :short) id (and (numberp wf) (zerop wf)) input))
        (:html
         (format
          log
          "<table class=\"flow\" border=0>~%~
           <tr><td class=\"flowTopBorder\" colspan=5></td>~%~
           <tr><td class=\"flowLeftBorder\"></td>~
               <td class=\"flow\" colspan=3>[~a]~@[ (~a)~] ~:[~;*~]|~a|"
          (current-time :long :short)
          id (and (numberp wf) (zerop wf)) input)))

      (multiple-value-bind (return condition)
          (ignore-errors
           (when (or (not (stringp input)) (string= input ""))
             (error "null or malformed input string"))
           
           (let ((output
                  (case engine
                    #+:drakma
                    (:visl
                     (let ((drakma::*drakma-default-external-format* :utf-8))
                       (multiple-value-bind (body status headers uri stream)
                           (drakma:http-request
                            vurl
                            :method :post :user-agent ua
                            :parameters `(("pair" . "nor2eng")
                                          ("input" . ,input)))
                         (declare (ignore headers uri stream))
                         (when (= status 200)
                           (multiple-value-bind (start end starts ends)
                               (ppcre:scan vscanner body)
                             (when (and start end starts ends)
                               (subseq
                                body
                                (aref starts 0) (aref ends 0))))))))
                    #+:drakma
                    (:oa
                     (let ((drakma::*drakma-default-external-format*
                            :iso-8859-1))
                       (multiple-value-bind (body status headers uri stream)
                           (drakma:http-request
                            nurl
                            :method :post :user-agent ua
                            :parameters `(("name" . ,input)))
                         (declare (ignore headers uri stream))
                         (when (= status 200)
                           (multiple-value-bind (start end starts ends)
                               (ppcre:scan nscanner body)
                             (when (and start end starts ends)
                               (subseq
                                body
                                (aref starts 0) (aref ends 0))))))))
                    
                    #+:drakma
                    (:it
                     (let ((drakma::*drakma-default-external-format*
                            :iso-8859-1))
                       (multiple-value-bind (body status headers uri stream)
                           (drakma:http-request
                            iurl
                            :method :post :user-agent ua
                            :parameters `(("from" . "nor") ("to" . "eng")
                                          ("text" . ,input)))
                         (declare (ignore headers uri stream))
                         (when (= status 200)
                           (multiple-value-bind (start end starts ends)
                               (ppcre:scan iscanner body)
                             (when (and start end starts ends)
                               (subseq
                                body
                                (aref starts 0) (aref ends 0))))))))
                    #+:drakma
                    (:google
                     (let ((drakma::*drakma-default-external-format*
                            :utf-8)
                           (source (string-downcase (string source)))
                           (target (string-downcase (string target))))
                       (multiple-value-bind (body status headers uri stream)
                           (drakma:http-request
                            gurl
                            :method :post :user-agent ua
                            :parameters `(("sl" . ,source) ("tl" . ,target)
                                          ("ie" . "UTF-8")
                                          ("text" . ,input)))
                         (declare (ignore headers uri stream))
                         (when (= status 200)
                           (multiple-value-bind (start end starts ends)
                               (ppcre:scan gscanner body)
                             (when (and start end starts ends)
                               (subseq
                                body
                                (aref starts 0) (aref ends 0))))))))
                     
                    (:smt
                     (first (mt::smt-translate-strings (list input)))))))

             (setf stop (get-internal-real-time))

             (let* ((treal (round (* (- stop start) 1000) 
                                  internal-time-units-per-second))
                    (readings (if output 1 -1))
                    (bleu (if (and output targets)
                            (first
                             (score-strings 
                              (list output)
                              targets :source input :type :bleu))
                            0))
                    (result
                     (when output
                       (pairlis '(:result-id :surface :bleu :flags)
                                (list 0 output bleu (acons :bleu bleu nil))))))
               
               `((:treal . ,treal) (:total . ,treal) (:tcpu . ,treal)
                 (:readings . ,readings)
                 (:results . ,(list result))
                 (:fbleu . ,bleu) (:bbleu . ,bleu)))))
           
        (unless stop (setf stop (get-internal-real-time)))

        (when (and return log)
          (print-result return :stream log)
          (let* ((readings (get-field :readings return))
                 (readings
                  (if (and (numberp readings) (> readings 0)) readings 0))
                 (result (first (get-field :results return)))
                 (output (get-field :surface result))
                 (bleu (get-field :bleu result)))
            (case format
              (:ascii
               (format
                log
                "|~%|< ~:[~;*~]|~a|~@[ (~a)~] --- ~a x ~a x ~a = ~a~%"
                (and (numberp wf) (zerop wf)) input id
                readings readings readings readings)
               (loop
                   for target in targets
                   do (format stream "|@ |~a|~%" target))
               (format
                log
                "|> |~@[~a~]| {} <~@[~,2f~]> (1:1:1).~%"
                output bleu))
              (:html
               (format
                log
                "<tr><td class=\"flowLeftBorder\"></td>~
                     <td class=\"flow\" colspan=3>&nbsp;</td>~
                     <td class=\"flowRightBorder\"></td>~%~
                 <tr><td class=\"flowLeftBorder\"></td>~
                     <td class=\"flow\" colspan=3>~
                       |< ~:[~;*~]|~a|~@[ (~a)~] ~
                       --- ~a x ~a x ~a = ~a</td>~
                     <td class=\"flowRightBorder\"></td>~%"
                        (and (numberp wf) (zerop wf)) input id 
                        readings readings readings readings)
               (format
                log
                "<tr><td class=\"flowLeftBorder\"></td>~
                     <td class=\"flow\" colspan=3>~
                       |> |~@[~a~]| {} <~@[~,2f~]> (1:1:1).</td>~
                     <td class=\"flowRightBorder\"></td>~%"
                output bleu)))))
          
        (nconc
         (when condition
           (let ((error (normalize-string 
                         (format nil "~a" condition))))
             (pairlis '(:readings :condition :error)
                      (list -1 (unless burst condition) error))))
         (let ((total (round (* (- stop start) 1000)
                             internal-time-units-per-second)))
           (pairlis '(:trace :total)
                    (list (get-output-stream-string log) total)))
         return)))))


(defun mt-optimize-weights (data &key condition)
  (let ((items
         (loop
             with items
             = (analyze data :condition condition :thorough '(:flags))
             for item in items
             for results = (get-field :results item)
             when results do
               (loop
                   for result in results
                   for flags
                   = (let ((flags (get-field :flags result)))
                       (if (stringp flags) (read-from-string flags) flags))
                   for bleu = (get-field :bleu flags)
                   for ascore = (get-field :ascore flags)
                   for tscore = (get-field :tscore flags)
                   for lm = (- (get-field :lm flags))
                   for perplexity = (- (get-field :perplexity flags))
                   for distortion = (get-field+ :distortion flags 0)
                   for rscore = (get-field :rscore flags)
                   for lfn = (get-field :lfn flags)
                   for lnf = (get-field :lnf flags)
                   do (setf (get-field :flags result) flags)
                   maximize bleu into bmax
                   minimize ascore into amin maximize ascore into amax
                   minimize tscore into tmin maximize tscore into tmax
                   minimize lm into lmin maximize lm into lmax
                   minimize perplexity into pmin maximize perplexity into pmax
                   minimize distortion into dmin maximize distortion into dmax
                   minimize rscore into rmin maximize rscore into rmax
                   minimize lfn into fnmin maximize lfn into fnmax
                   minimize lnf into nfmin maximize lnf into nfmax
                   finally
                     (let ((arange (- amax amin))
                           (trange (- tmax tmin))
                           (lrange (- lmax lmin))
                           (prange (- pmax pmin))
                           (drange (- dmax dmin))
                           (rrange (- rmax rmin))
                           (fnrange (- fnmax fnmin))
                           (nfrange (- nfmax nfmin)))
                       (loop
                           for result in results
                           for flags = (get-field :flags result)
                           for distance = (get-field+ :distance flags 0)
                           for ascore
                           = (divide (- (get-field :ascore flags) amin) arange)
                           for tscore
                           = (divide (- (get-field :tscore flags) tmin) trange)
                           for lm
                           = (divide (- (- (get-field :lm flags)) lmin) lrange)
                           for perplexity
                           = (divide
                              (- (- (get-field :perplexity flags)) pmin)
                              prange)
                           for distortion
                           = (divide
                              (- (get-field+ :distortion flags 0) dmin) drange)
                           for rscore
                           = (divide (- (get-field :rscore flags) rmin) rrange)
                           for lfn
                           = (divide (- (get-field :lfn flags) fnmin) fnrange)
                           for lnf
                           = (divide (- (get-field :lnf flags) nfmin) nfrange)
                           for scores
                           = (pairlis '(:ascore :tscore :lm :perplexity
                                        :distortion :rscore :lfn :lnf
                                        :distance)
                                      (list ascore tscore lm perplexity
                                            distortion rscore lfn lnf
                                            distance))
                           do (nconc result (acons :scores scores nil)))
                     (nconc
                      item
                      (pairlis '(:bbleu :amin :arange :tmin :trange
                                 :lmin :lrange :pmin :prange
                                 :dmin :drange :rmin :rrange
                                 :fnmin :fnrange :nfmin :nfrange)
                               (list bmax amin arange tmin trange
                                     lmin lrange pmin prange
                                     dmin drange rmin rrange
                                     fnmin fnrange nfmin nfrange)))))
             finally
               (return items))))
    (macrolet ((gridify (parameters &body body)
                 (if (null (rest parameters))
                   `(dolist ,(first parameters) ,@body)
                   `(dolist ,(first parameters)
                      (gridify ,(rest parameters) ,@body)))))
      (let ((values (loop for w from 0 to 1 by 0.05 collect w))
            (best 0) active (i -1))
        (gridify ((aw values) (tw values) (lw values)
                  (dw values) (rw values) (fnw values) (nfw values))
          (unless (> (+ aw tw lw dw rw fnw nfw) 1)
            (loop
                for item in items
                for length = (get-field :i-length item)
                for bleu
                = (loop
                      with bvalue = 0 with bbleu = 0
                      for result in (get-field :results item)
                      for scores = (get-field :scores result)
                      for bleu = (get-field :bleu (get-field :flags result))
                      for value
                      = (+ (* aw (get-field :ascore scores))
                           (* tw (get-field :tscore scores))
                           (* lw (get-field :lm scores))
                           (* dw (get-field :distortion scores))
                           (* rw (get-field :rscore scores))
                           (* fnw (get-field :lfn scores))
                           (* nfw (get-field :lnf scores))
                           (- (get-field :distance scores)))
                      when (> value bvalue) do (setf bbleu bleu)
                      finally (return bbleu))
                sum (* bleu length) into tbleu sum length into tlength
                finally
                  (let ((bleu (/ tbleu tlength)))
                    (format
                     t
                     "{~d} [~,1f ~,1f ~,1f ~,1f ~,1f ~,1f ~,1f]: ~,8f ~
                      (~,8f @ [~{~,1f~^ ~}]).~%"
                     (incf i) aw tw lw dw rw fnw nfw bleu best
                     (first active))
                    (when (> bleu best)
                      (setf best bleu)
                      (setf active (list (list aw tw lw dw rw fnw nfw))))
                    (when (= bleu best)
                      (push (list aw tw lw dw rw fnw nfw) active))))))
        active))))
