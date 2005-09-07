(in-package :tsdb)

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

;;;
;;; ToDo
;;;
;;; - no need to save yield (`value') for discriminants; recompute from start
;;;   and end indices into full string;
;;; - protect against saving with an empty `decision' set;
;;; - fill in `preference' relation;
;;; - confidence menu;
;;; - `Reset' button: re-instantiate original, preset state;
;;; - reorder trees: active at top;
;;; - pairwise comparison of trees;
;;; - highlighting of discriminants on tree select;
;;; - highlighting of trees on discriminant select;
;;; - utilize status vector to, e.g. fast-forward to first unannotated;
;;; - record all :select decisions, valid at `Save' time;
;;; - add print button: include edge id in display and print out;
;;;

(defparameter *redwoods-semantix-hook* nil)

(defparameter *redwoods-trees-hook* nil)

(defparameter *redwoods-record-void-discriminants-p* nil)

(defparameter *redwoods-trace* nil)

(defparameter *redwoods-item-enhancer* nil)

(defparameter *redwoods-reconstruct-mode* :word)

(defparameter %redwoods-items-increment% 100)

(defparameter %redwoods-items-percentile% 20)

(defparameter %model% nil)

(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          gold strip inspect 
                          (bestp *redwoods-thinning-normalize-p*)
                          (exactp *redwoods-update-exact-p*)
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t) interactive
                          (stream *tsdb-io*)
                          (runp t) interrupt meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (initialize-tsdb)
  (when strip
    (unless (do-import-database (find-tsdb-directory data) strip 
                                :meter (when meter (make-meter 0 1))
                                :except (append
                                         '("tree" "decision" "preference")
                                         (and bestp '("result"))))
      (return-from browse-trees nil)))

  (let* ((condition (if (and condition (not (equal condition "")))
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items
          (if (stringp data) 
            (analyze data 
                     :condition condition :meter meter :message t)
            data))
         (message (format 
                   nil 
                   "~a `~a' trees ..." 
                   (cond
                    (strip "normalizing")
                    (gold "updating")
                    (t "browsing"))
                   data))
         (items (sort (copy-list items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (schema (read-database-schema data))
         (cache (when cache
                  (create-cache (or strip data)
                                :schema schema :verbose verbose 
                                :protocol cache)))
         (gc-strategy 
          (unless (or interactive (null runp))
            (install-gc-strategy 
             nil :tenure *tsdb-tenure-p* :burst t :verbose t)))
	 (display (let ((foo (getenv "DISPLAY")))
		    (and (stringp foo) (not (string= foo "")) foo)))
         (frame (unless #-:expand strip #+:expand nil
                  (if (and runp display)
                    (clim:make-application-frame 'lkb::compare-frame)
                    (clim:make-application-frame 
                     'lkb::compare-frame :frame-manager nil))))
                    
         %client%)
    (declare (special %client%))

    #+:debug
    (setf lkb::%frame% frame)
    (when frame 
      (setf (lkb::compare-frame-ids frame) 
        (loop for item in items collect (get-field :i-id item))))
    
    (when meter
      (status :text message)
      (meter :value 0))
    (when runp
      (loop
          with last = nil
          with increment = (and meter (/ 1 (if items (length items) 1)))
          with title = (format 
                        nil 
                        "[incr tsdb()] Tree ~a (`~a'~
                         ~:[~*~; from `~a'~])~@[ @ `~a'~]" 
                        (if gold "Update" "Annotation")
                        data gold gold condition)
          with nitems = (length items)
          with annotated = (make-array nitems :initial-element 0)
          with position = 0
          initially
            (when frame
              (setf (lkb::compare-frame-chart frame) nil)
              (setf (clim:frame-pretty-name frame) title)
              (setf (lkb::compare-frame-controller frame) *current-process*))
          for item = (when position (nth position items))
          for i-id = (get-field :i-id item)
          for status = (when (integerp i-id) 
                         (or 
                          #+:allegro
                          (when *tsdb-tenure-p*
                            (excl:tenuring
                             (browse-tree 
                              data i-id frame 
                              :gold gold :strip strip :bestp bestp 
                              :inspect inspect :exactp exactp
                              :cache cache :title title :display display
                              :verbose verbose :stream stream)))
                          (browse-tree 
                           data i-id frame 
                           :gold gold :strip strip :bestp bestp 
                           :inspect inspect :exactp exactp
                           :cache cache :title title :display display
                           :verbose verbose :stream stream)))
          for action = (get-field :status status)
          for offset = (or (get-field :offset status) 1)
          while (and status (not (eq action :close)) (numberp position))
          do 
            (when (and (eq action :save) increment 
                       (zerop (aref annotated position)))
              (meter-advance increment))
            (case action
              (:first (setf position 0))
              (:previous 
               (decf position offset)
               (close-connections :data data))
              ((:skip :null :flag)
               (if (eq last :previous) (decf position) (incf position))
               (setf action last))
              ((:next :save) 
               (when (eq action :save) (incf (aref annotated position)))
               (incf position offset))
              (:last (setf position (- nitems 1))))
            (setf last action)
            (when (or (>= position nitems) (< position 0))
              (setf position nil))
            (purge-profile-cache data :expiryp nil)
            (when gold (purge-profile-cache gold  :expiryp nil))
          when (interrupt-p interrupt) do
            (format 
             stream
             "browse-trees(): external interrupt signal~%")
            (force-output stream)
            (return))

      (when frame 
        ;;
        ;; according to section 9.6 of the CLIM User Guide, frame-exit() cannot
        ;; have an effect unless called from the process running the top-level
        ;; of that frame.
        ;;
        (clim:frame-exit frame)
        #+:null
        (clim:destroy-frame frame))
      (when (mp:process-p %client%)
        (mp:process-kill %client%))

      (when meter
        (status :text (format nil "~a done" message) :duration 10)
        (meter :value 1)))
  
    (when cache (flush-cache cache :verbose verbose))
    (when gc-strategy (restore-gc-strategy gc-strategy))
    (purge-profile-cache data :expiryp nil)
    
    (or frame t)))

(defun browse-tree (data i-id frame &key gold strip bestp 
                                         inspect exactp subset
                                         title cache verbose 
                                         (runp t) display stream)
  
  (declare (special %client%))
  
  #+:debug
  (setf lkb::%frame% frame)

  (when (or (null runp)
            (null %client%)
            (and (mp:process-p %client%) (mp:process-active-p %client%)))
    #+:allegro
    (format
     excl:*initial-terminal-io*
     "~&[~a] browse-tree(): `~a' ~@[(~a) ~]--- item # ~a~%"
     (current-time :long :short) data gold i-id)

    (let* ((lkb::*chart-packing-p* nil)
           (*reconstruct-cache* (make-hash-table :test #'eql))
           (lkb::*tree-update-match-hook* #'update-match-p)
           (lkb::*tree-automatic-update-p* 
            (when gold lkb::*tree-automatic-update-p*))
           (condition (format nil "i-id = ~a" i-id))
           (items (let ((*package* (find-package lkb::*lkb-package*)))
                    (analyze 
                     data :thorough '(:derivation :mrs) 
                     :commentp t :taggingp t :condition condition)))
           (item (and (null (rest items)) (first items)))
           (input (or (get-field :o-input item) (get-field :i-input item)))
	   (tags (get-field :tags item))
           (i-id (get-field :i-id item))
           (i-length (get-field :i-length item))
           (i-comment (get-field :i-comment item))
           (readings (get-field :readings item))
           (parse-id (get-field :parse-id item))
           (results (get-field :results item))
           (trees (when parse-id
                    #+:allegro
                    (format
                     excl:*initial-terminal-io*
                     "~&[~a] browse-tree(): ~
                      retrieved item # ~a (~a parse~p).~%"
                     (current-time :long :short) i-id 
                     (length results)(length results))
                    (select '("parse-id" "t-version" "t-active" "t-confidence" 
                              "t-author" "t-start" "t-end" "t-comment")
                            '(:integer :integer :integer :integer 
                              :string :date :date :string)
                            "tree" 
                            (format nil "parse-id == ~a" parse-id) 
                            data
                            :sort :parse-id)))
           (version (loop
                        for tree in trees
                        maximize (get-field :t-version tree)))
           (trees (loop
                      for tree in trees
                      when (eq version (get-field :t-version tree))
                      collect tree))
           (user (get-field :t-author (first trees)))
           (date (get-field :t-end (first trees)))
           (confidence (let* ((foo (get-field :t-confidence (first trees))))
                         (if (and (integerp foo) (>= foo 0) (<= foo 3))
                           foo
                           3)))
           (history (let* ((foo (get-field :t-confidence (first trees)))
                           (confidence 
                            (if (and (integerp foo) (>= foo 0) (<= foo 3))
                              (aref #("zero" "low" "fair" "high") foo)
                              "unknown")))
                      (if (and (>= version 0) user date)
                        (format
                         nil
                         "(~a) ~a on ~a: ~a (~a)"
                         version user date confidence foo)
                        "")))
           (edges (unless (or #-:expand strip #+:expand (null trees))
                    #+:allegro
                    (format
                     excl:*initial-terminal-io*
                     "~&[~a] browse-tree(): retrieved ~a tree record~p.~%"
                     (current-time :long :short) (length trees) (length trees))
                    (loop
                        with edges
                        with mode = 
                          (if (eq (lkb::compare-frame-mode frame) :modern)
                            t *redwoods-reconstruct-mode*)
                        for result in results
                        for id = (get-field :result-id result)
                        for derivation = (get-field :derivation result)
                        for mrs = (get-field :mrs result)
                        for edge = (when (or (null subset) (member id subset))
                                     (if (and derivation 
                                              (not (equal derivation "")))
                                       (reconstruct derivation mode)
                                       (when mrs
                                         (lkb::make-edge
                                          :id id :from 0 :to i-length))))
                        ;;
                        ;; _fix_me_
                        ;; this seems overly robust: issue a warning message
                        ;; whenever we fail to reconstruct an edge.
                        ;;                                      7-jun-04; oe)
                        ;;
                        when edge do 
                          (setf (lkb::edge-foo edge) id)
                          (setf (lkb::edge-bar edge) derivation)
                          (setf (lkb::edge-mrs edge) mrs)
                          (push edge edges)
                        finally
                          #+:allegro
                          (format
                           excl:*initial-terminal-io*
                           "~&[~a] browse-tree(): reconstructed ~a edge~p.~%"
                           (current-time :long :short) 
                           (length edges) (length edges))
                          (return (nreverse edges)))))
           (edges (sort edges #'< :key #'lkb::edge-foo))
           (foo (first edges))
           (start (and foo (lkb::edge-from foo)))
           (end (and foo (lkb::edge-to foo)))
           (decisions (when (and parse-id version)
                        (select '("parse-id" "t-version"
                                  "d-state" "d-type" "d-key" "d-value" 
                                  "d-start" "d-end" "d-date")
                                '(:integer :integer
                                  :integer :integer :string :string 
                                  :integer :integer :date)
                                "decision" 
                                (format 
                                 nil 
                                 "parse-id == ~a && t-version == ~a" 
                                 parse-id version) 
                                data)))
           (discriminants (unless #-:expand strip #+:expand nil
                            #+:allegro
                            (format
                             excl:*initial-terminal-io*
                             "~&[~a] browse-tree(): retrieved ~a decision~p.~%"
                             (current-time :long :short)
                             (length decisions) (length decisions))
                            (reconstruct-discriminants decisions)))
           (greadings (when (and gold parse-id (null strip))
                        (let ((items (select 
                                      '("readings") '(:integer) "parse" 
                                      (format nil "i-id == ~a" i-id)
                                      gold)))
                          (when (= (length items) 1)
                            (get-field :readings (first items))))))
           (gtrees (when (and gold parse-id (null strip))
                     (select '("parse-id" "t-version" 
                               "t-active" "t-author" "t-end")
                             '(:integer :integer 
                               :integer :string :date)
                             "tree" 
                             (format nil "parse-id == ~a" parse-id) 
                             gold
                             :sort :parse-id)))
           (gversion (loop
                         for tree in gtrees
                         maximize (get-field :t-version tree)))
           (gtrees (loop
                       for tree in gtrees
                       when (eq gversion (get-field :t-version tree))
                       collect tree))
           (gactive (when (= (length gtrees) 1)
                      (let ((gactive (get-field :t-active (first gtrees))))
                        (unless (minus-one-p gactive) gactive))))
           (gitem (when (and gactive (or exactp (= readings 1)))
                    (first
                     (analyze 
                      gold :thorough '(:derivation) :condition condition))))
           (gpreferences (when (and gitem (= (length gtrees) 1))
                           (select '("parse-id" "t-version" "result-id")
                                   '(:integer :integer :integer)
                                   "preference" 
                                   (format 
                                    nil 
                                    "parse-id == ~a && t-version == ~a" 
                                    parse-id gversion) 
                                   gold)))
           (gderivation (when (= (length gpreferences) 1)
                          (loop
                              with gpreference = (first gpreferences)
                              with key = (get-field :result-id gpreference)
                              for result in (get-field :results gitem)
                              for id = (get-field :result-id result)
                              thereis (when (= id key)
                                        (get-field :derivation result)))))
           (ghistory (when (and (integerp greadings) (integerp gactive))
                       (let* ((guser (get-field :t-author (first gtrees)))
                              (gdate (get-field :t-end (first gtrees))))
                         (format 
                          nil 
                          "(~a) ~a on ~a; [~a : ~a] active"
                          gversion guser gdate 
                          gactive (- greadings gactive)))))
           (gdecisions (when (and gold gversion)
                         #+:allegro
                         (format
                          excl:*initial-terminal-io*
                          "~&[~a] browse-tree(): retrieved ~a gold tree~p.~%"
                          (current-time :long :short) 
                          (length gtrees) (length gtrees))
                         (unless exactp
                           (select '("parse-id" "t-version"
                                     "d-state" "d-type" "d-key" "d-value" 
                                     "d-start" "d-end" "d-date")
                                   '(:integer :integer
                                     :integer :integer :string :string 
                                     :integer :integer :date)
                                   "decision" 
                                   (format 
                                    nil 
                                    "parse-id == ~a && t-version == ~a" 
                                    parse-id gversion) 
                                   gold))))
           (gdiscriminants (when gdecisions
                             #+:allegro
                             (format
                              excl:*initial-terminal-io*
                              "~&[~a] browse-tree(): ~
                               retrieved ~a gold decision~p.~%"
                              (current-time :long :short) 
                              (length gdecisions) (length gdecisions))
                             (reconstruct-discriminants gdecisions)))
           (version (max (if version version 0) (if gversion gversion 0)))
           (lkb::*parse-record* edges))
      (declare (ignore active))

      (when strip
        #-:expand
        (loop
            with preferences = (select '("parse-id" "t-version" "result-id")
                                       '(:integer :integer :integer)
                                       "preference" 
                                       (format 
                                        nil 
                                        "parse-id == ~a && t-version == ~a" 
                                        parse-id version) 
                                       data)
            for preference in preferences
            do 
              (write-preference strip preference :cache cache)
            finally
              (when (and trees bestp)
                ;;
                ;; _fix_me_
                ;; in :random mode, we need to also adjust the preference
                ;; relation accordingly.                       (26-apr-04; oe)
                ;;
                (let* ((ids (if (eq bestp :random)
                              (random-sample 1 readings (length preferences))
                              (loop
                                  for preference in preferences
                                  collect (get-field :result-id preference))))
                       (condition (format
                                   nil
                                   "parse-id == ~a~
                                    ~@[ && (~{result-id == ~a~^ ||~})~]"
                                   parse-id ids))
                       (schema (read-database-schema data))
                       (relation (loop
                                     for (relation . structure) in schema
                                     when (string= relation "result")
                                     return structure))
                       (fields (loop
                                   for field in relation 
                                   collect (first field)))
                       (types (loop
                                  for field in relation 
                                  collect (second field)))
                       (results 
                        (when ids
                          (select fields types "result" condition data))))
                  (when (or *redwoods-semantix-hook* *redwoods-trees-hook*)
                    (loop
                        for result in results
                        for derivation = (get-field :derivation result)
                        for edge = (when derivation (reconstruct derivation))
                        for mrs = (when (and edge *redwoods-semantix-hook*)
                                    (call-hook *redwoods-semantix-hook* edge))
                        for tree = (when (and edge *redwoods-trees-hook*)
                                     (call-hook *redwoods-trees-hook* edge))
                        when mrs do (setf (get-field :mrs result) mrs)
                        when tree do (setf (get-field :tree result) tree))
                    (setf lkb::*cached-category-abbs* nil))
                  (write-results parse-id results strip :cache cache))))
        (if trees
          (write-tree strip (first trees) :cache cache)
          (let* ((user (current-user))
                 (time (current-time :long :tsdb))
                 (tree (pairlis '(:parse-id 
                                  :t-version :t-active :t-confidence
                                  :t-author :t-start :t-end :t-comment)
                                (list parse-id 
                                      0 -1 -1
                                      user time time ""))))
            (write-tree strip tree :cache cache)))
        (loop
            for decision in decisions
            do (write-decision strip decision :cache cache))
        #-:expand
        (return-from browse-tree (acons :status :save nil)))

      (when (null edges)
        (when verbose
          (format
           stream
           "browse-tree(): failed to reconstruct item # ~d (parse # ~d).~%"
           i-id parse-id))
        (return-from browse-tree (acons :status :null nil)))
      
      (setf (lkb::compare-frame-edges frame) nil)
      (setf (lkb::compare-frame-input frame) input)
      (setf (lkb::compare-frame-tags frame) tags)
      (setf (lkb::compare-frame-item frame) i-id)
      (setf (lkb::compare-frame-start frame) start)
      (setf (lkb::compare-frame-end frame) end)
      (setf (lkb::compare-frame-derivations frame) 
        (loop
            for result in results collect (get-field :derivation result)))
      (setf (lkb::compare-frame-version frame) history)
      (setf (lkb::compare-frame-comment frame) i-comment)
      (setf (lkb::compare-frame-confidence frame) confidence)
      (setf (lkb::compare-frame-preset frame) discriminants)
      (setf (lkb::compare-frame-gold frame) gdiscriminants)
      (setf (lkb::compare-frame-gversion frame) ghistory)
      (setf (lkb::compare-frame-gactive frame) gactive)
      (setf (lkb::compare-frame-gderivation frame) gderivation)
      (setf (lkb::compare-frame-inspect frame) inspect)
      (setf (lkb::compare-frame-update frame)
        (when (and gactive greadings)
          (pairlis '(:parse-id :u-gin :u-gout)
                   (list parse-id gactive (- greadings gactive)))))

      (when exactp
        (loop
            with gderivation = (if (stringp gderivation)
                                 (ignore-errors
                                  (read-from-string gderivation nil nil))
                                 gderivation)
            initially (setf (lkb::compare-frame-exact frame) nil)
            for edge in lkb::*parse-record*
            for derivation = (lkb::edge-bar edge)
            when (derivation-equal derivation gderivation) do
              (push edge (lkb::compare-frame-exact frame))))

      (when (and runp display (null %client%))
        (setf %client%
          (mp:run-function 
           (or title "[incr tsdb()] Tree Selection")
           #'lkb::run-compare-frame frame)))

      (let ((status (lkb::set-up-compare-frame 
                     frame lkb::*parse-record* :runp runp :display display)))

        ;;
        ;; _fix_me_
        ;; grey out `Save' button in compare frame, when we have a read-only
        ;; cache, i.e. (:protocol . :ro).                     (26-jan-04; oe)
        ;;
        #+:expand
        (lkb::record-decision (lkb::make-decision :type :save) frame)
        #-:expand
        (unless (or (eq status :skip) (null runp))
          (process-add-arrest-reason *current-process* :wait)))
      
      (when runp
        (let* ((decisions (lkb::compare-frame-decisions frame))
               (status (lkb::decision-type (first decisions)))
               (recent (second decisions)))

          (when (and (eq status :flag) (null trees) *redwoods-update-flag-p*)
            (let* ((user (current-user))
                   (time (current-time :long :tsdb))
                   (tree (pairlis '(:parse-id 
                                    :t-version :t-active :t-confidence
                                    :t-author :t-start :t-end :t-comment)
                                  (list parse-id 
                                        0 -1 -1
                                        user time time ""))))
              (write-tree strip tree :cache cache)))

          (when (eq status :save)
            (let* ((version (if version 
                              #-:expand (incf version) #+:expand version
                              1))
                   (edges (lkb::compare-frame-in frame))
                   (active (length edges))
                   (foo (lkb::compare-frame-confidence frame))
                   (confidence (if (and (integerp foo)
                                        (>= foo 0) (<= foo 3))
                                 foo
                                 -1))
                   (t-author (current-user))
                   (t-start (let* ((start (first (last decisions)))
                                   (start (when (lkb::decision-p start)
                                            (lkb::decision-time start))))
                              (if start 
                                (decode-time start :long :tsdb)
                                (current-time :long :tsdb))))
                   (t-end (let* ((end (first decisions))
                                 (end (when (lkb::decision-p end)
                                        (lkb::decision-time end))))
                            (if end 
                              (decode-time end :long :tsdb)
                              (current-time :long :tsdb)))))
              #-:expand
              (write-tree data (pairlis '(:parse-id 
                                          :t-version :t-active :t-confidence
                                          :t-author :t-start :t-end :t-comment)
                                        (list parse-id 
                                              version active confidence
                                              t-author t-start t-end ""))
                          :cache cache)

              (loop
                  for edge in edges
                  for id = (when (lkb::edge-p edge) (lkb::edge-foo edge))
                  do
                    (write-preference data
                                      (pairlis '(:parse-id 
                                                 :t-version :result-id)
                                               (list parse-id
                                                     version id))
                                      :cache cache)))

            #-:expand
            (when (and (lkb::decision-p recent)
                       (member (lkb::decision-type recent) '(:reject :select)))
              (let* ((version (or version 1))
                     (state (encode-discriminant-state recent))
                     (type (encode-discriminant-type recent))
                     (start (lkb::compare-frame-start frame))
                     (end (lkb::compare-frame-end frame))
                     (time (let ((time (lkb::decision-time recent)))
                             (if time
                               (decode-time time :long :tsdb)
                               (current-time :long :tsdb)))))
                (write-decision data 
                                (pairlis '(:parse-id :t-version 
                                           :d-state :d-type :d-key :d-value 
                                           :d-start :d-end :d-date)
                                         (list parse-id version 
                                               state type nil nil 
                                               start end time))
                                :cache cache)))
            #-:expand
            (loop
                with version = (or version 1)
                for discriminant in (lkb::compare-frame-discriminants frame)
                for state = (encode-discriminant-state discriminant)
                for type = (encode-discriminant-type discriminant)
                for key = (lkb::discriminant-key discriminant)
                for value = (lkb::discriminant-value discriminant)
                for start = (lkb::discriminant-start discriminant)
                for end = (lkb::discriminant-end discriminant)
                for time = (let ((time (lkb::discriminant-time discriminant)))
                             (if time
                               (decode-time time :long :tsdb)
                               (current-time :long :tsdb)))
                unless (or (lkb::discriminant-tag discriminant)
                           (and (null *redwoods-record-void-discriminants-p*)
                                (= state 5)))
                do
                  (write-decision data 
                                  (pairlis '(:parse-id :t-version 
                                             :d-state :d-type :d-key :d-value 
                                             :d-start :d-end :d-date)
                                           (list parse-id version 
                                                 state type key value 
                                                 start end time))
                                  :cache cache))
            #-:expand
            (let* ((update (lkb::compare-frame-update frame))
                   (discriminants (lkb::compare-frame-discriminants frame))
                   (decisions 
                    (loop
                        for foo in discriminants
                        for bar = (lkb::discriminant-toggle foo)
                        count (and (null (lkb::discriminant-gold foo))
                                   (not (eq bar :unknown)))))
                   (in (length (lkb::compare-frame-in frame)))
                   (out (length (lkb::compare-frame-out frame))))
              (when (and update (>= (profile-granularity data) 0210))
                (write-update 
                 data (append
                       (pairlis '(:t-version :u-new :u-in :u-out)
                                (list (or version 1) decisions in out))
                       update)
                 :cache cache))))

          (pairlis '(:status) (list status)))))))

(defun encode-discriminant-state (discriminant)
  (cond
   ((lkb::discriminant-p discriminant)
    (let ((toggle (lkb::discriminant-toggle discriminant))
          (state (lkb::discriminant-state discriminant)))
      (cond
       ((eq toggle t) 1)
       ((null toggle) 2)
       ((eq state t) 3)
       ((null state) 4)
       (t 5))))
   ((lkb::decision-p discriminant)
    -1)
   (t -1)))

(defun encode-discriminant-type (discriminant)
  (cond
   ((lkb::discriminant-p discriminant)
    (case (lkb::discriminant-type discriminant)
      (:relation 1)
      (:type 2)
      (:constituent 3)
      (:ed 6)
      (t 0)))
   ((lkb::decision-p discriminant)
    (case (lkb::decision-type discriminant)
      (:select 4)
      (:reject 5)
      (t -1)))
   (t -1)))

(defun reconstruct-discriminants (decisions)
  (loop
      for decision in decisions
      for state = (get-field :d-state decision)
      for type = (get-field :d-type decision)
      for key = (get-field :d-key decision)
      for value = (get-field :d-value decision)
      for start = (get-field :d-start decision)
      for end = (get-field :d-end decision)
      for discriminant = (and state type key value start end
                              (not (minus-one-p type))
                              (reconstruct-discriminant 
                               state type key value start end))
      when discriminant collect discriminant))

(defun reconstruct-discriminant (istate type key value start end)
  (let* ((type (cond 
                ((eql type 1) :relation)
                ((eql type 2) :type)
                ((eql type 3) :constituent)
                ((eql type 4) :select)
                ((eql type 5) :reject)
                ((eql type 6) :ed)
                (t nil)))
         (toggle :unknown)
         (state :unknown))
    (cond
     ((eql istate 1) (setf toggle t) (setf state t))
     ((eql istate 2) (setf toggle nil) (setf state nil))
     ((eql istate 3) (setf state t))
     ((eql istate 4) (setf state nil)))
    (lkb::make-discriminant :type (intern type :keyword) 
                            :key key :value value 
                            :start start :end end
                            :toggle toggle :state state)))

(defun analyze-trees (&optional (data *tsdb-data*)
                      &key (condition *statistics-select-condition*)
                           file append (format :latex)
                           meter)
  (let* ((stream (create-output-stream file append))
         (items (if (stringp data) 
                  (analyze-aggregates data :condition condition :trees t
                                      :meter meter :format format) 
                  data))
         (averages 
          (summarize-competence-parameters items))
         (averages (remove 0 averages 
                           :key #'(lambda (foo)
                                    (get-field :results (rest foo)))))
         (naggregates (- (length averages) 1))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (ncolumns 17)
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty))))
    
    (when (or (null items) (null averages))
      (return-from analyze-trees 1))
    
    (case format
      (:tcl
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "flags 2~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 7 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 10 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 13 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 16 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
        (+ naggregates 2) (+ naggregates 3))
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         region 1 1 2 1 -contents {~a} -format title ~
           -hor_justify left -ver_justify center~%~
         region 1 2 1 4 -contents {all results} ~
           -format title -hor_justify center~%~
         region 1 5 1 7 -contents {t-active = 0} ~
           -format title -hor_justify center~%~
         region 1 8 1 10 -contents {t-active = 1} ~
           -format title -hor_justify center~%~
         region 1 11 1 13 -contents {t-active > 1} ~
           -format title -hor_justify center~%~
         region 1 14 1 16 -contents {unannotated} ~
           -format title -hor_justify center~%"
        alabel alabel)
       (loop
           for i from 2 to 14 by 3
           do
             (loop
                 for label in '("items\\n#" "words\\n\\330" "trees\\n\\330")
                 for j from 0
                 for k = (+ i j)
                 do
                   (format
                    stream
                    "cell 2 ~d -contents \"~a\" -format title~%~
                     region 2 ~d 2 ~d -contents \"~a\" -format title ~
                       -hor_justify center~%"
                    k label k k label))))
      (:html
       (format stream "<table  cellspacing=0>~%")
       (html-output "redwoods-annotations-header.html" :stream stream
                    :values (list alabel))))
    (loop
        with i = 2
        for aggregate in (remove :all items :key #'first)
        for data = (rest (assoc (first aggregate) averages))
        for name = (if (eq format :latex)
                     (latexify-string (second aggregate))
                     (second aggregate))
        when data do
          (incf i)
          (case format
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~,2f -format data~%~
               cell ~d 4 -contents ~,1f -format data~%~
               cell ~d 5 -contents ~d -format data~%~
               cell ~d 6 -contents ~,2f -format data~%~
               cell ~d 7 -contents ~,1f -format data~%~
               cell ~d 8 -contents ~d -format data~%~
               cell ~d 9 -contents ~,2f -format data~%~
               cell ~d 10 -contents ~,1f -format data~%~
               cell ~d 11 -contents ~d -format data~%~
               cell ~d 12 -contents ~,2f -format data~%~
               cell ~d 13 -contents ~,1f -format data~%~
               cell ~d 14 -contents ~d -format data~%~
               cell ~d 15 -contents ~,2f -format data~%~
               cell ~d 16 -contents ~,1f -format data~%"
              i name
              i (get-field :results data) i (get-field :i-length data)
              i (get-field :analyses data)
              i (get-field :rresults data) i (get-field :rlength data)
              i (get-field :ranalyses data)
              i (get-field :uresults data) i (get-field :ulength data)
              i (get-field :uanalyses data)
              i (get-field :aresults data) i (get-field :alength data)
              i (get-field :aanalyses data)
              i (get-field :sresults data) i (get-field :slength data)
              i (get-field :sanalyses data)))
            (:html
             (html-output
              (if (= i 3)
                "redwoods-annotations-body-first.html"
                "redwoods-annotations-body.html")
              :stream stream
              :values (list 
                       name
                       (get-field :results data) (get-field :i-length data)
                       (get-field :analyses data)
                       (get-field :rresults data) (get-field :rlength data)
                       (get-field :ranalyses data)
                       (get-field :uresults data) (get-field :ulength data)
                       (get-field :uanalyses data)
                       (get-field :aresults data) (get-field :alength data)
                       (get-field :aanalyses data)
                       (get-field :sresults data) (get-field :slength data)
                       (get-field :sanalyses data))))))
    (let* ((total (rest (assoc :total averages)))
           (name "Total")
           (n (+ naggregates 3)))
      (case format
        (:tcl
         (format
          stream
          "cell ~d 1 -contents {~a} -format aggregate~%~
           cell ~d 2 -contents ~d -format data~%~
           cell ~d 3 -contents ~,2f -format data~%~
           cell ~d 4 -contents ~,1f -format data~%~
           cell ~d 5 -contents ~d -format data~%~
           cell ~d 6 -contents ~,2f -format data~%~
           cell ~d 7 -contents ~,1f -format data~%~
           cell ~d 8 -contents ~d -format data~%~
           cell ~d 9 -contents ~,2f -format data~%~
           cell ~d 10 -contents ~,1f -format data~%~
           cell ~d 11 -contents ~d -format data~%~
           cell ~d 12 -contents ~,2f -format data~%~
           cell ~d 13 -contents ~,1f -format data~%~
           cell ~d 14 -contents ~d -format data~%~
           cell ~d 15 -contents ~,2f -format data~%~
           cell ~d 16 -contents ~,1f -format data~%"
          n name
          n (get-field :results total) n (get-field :i-length total)
          n (get-field :analyses total)
          n (get-field :rresults total) n (get-field :rlength total)
          n (get-field :ranalyses total)
          n (get-field :uresults total) n (get-field :ulength total)
          n (get-field :uanalyses total)
          n (get-field :aresults total) n (get-field :alength total)
          n (get-field :aanalyses total)
          n (get-field :sresults total) n (get-field :slength total)
          n (get-field :sanalyses total)))
        (:html
         (html-output
          "redwoods-annotations-total.html" :stream stream
          :values (list 
                   name
                   (get-field :results total) (get-field :i-length total)
                   (get-field :analyses total)
                   (get-field :rresults total) (get-field :rlength total)
                   (get-field :ranalyses total)
                   (get-field :uresults total) (get-field :ulength total)
                   (get-field :uanalyses total)
                   (get-field :aresults total) (get-field :alength total)
                   (get-field :aanalyses total)
                   (get-field :sresults total) (get-field :slength total)
                   (get-field :sanalyses total)))
         (format
          stream
          "<tr>~%  ~
           <td class=\"ItsdbCaption\" colspan=~a align=right>~%    ~
           ~a~%  </td>~%</tr>~%</table>~%"
          ncolumns caption)))

      #+:debug
      (format
       t
       "`~a'~%~%  ~
        ~d items; ~d results; ~
        ~,2f tokens; ~,2f words (~,2f); ~,2f readings;~%    ~
        rejected: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
        unambiguous: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
        ambiguous: ~d [~,2f ~,2f (~,2f) ~,2f]~%    ~
        unannotated: ~d [~,2f ~,2f (~,2f) ~,2f]~%"
       data
       (get-field :items total)
       (get-field :results total)
       (get-field :i-length total)
       (get-field :words total)
       (divide (get-field :words total) (get-field :i-length total))
       (get-field :analyses total)
       (get-field :rresults total)
       (get-field :rlength total)
       (get-field :rwords total)
       (divide (get-field :rwords total) (get-field :rlength total))
       (get-field :ranalyses total)
       (get-field :uresults total)
       (get-field :ulength total)
       (get-field :uwords total)
       (divide (get-field :uwords total) (get-field :ulength total))
       (get-field :uanalyses total)
       (get-field :aresults total)
       (get-field :alength total)
       (get-field :awords total)
       (divide (get-field :awords total) (get-field :alength total))
       (get-field :aanalyses total)
       (get-field :sresults total)
       (get-field :slength total)
       (get-field :swords total)
       (divide (get-field :swords total) (get-field :slength total))
       (get-field :sanalyses total)))

    
    (when (or (stringp file) (stringp append)) (close stream))
    0))

(defun analyze-update (&optional (data *tsdb-data*)
                       &key (condition *statistics-select-condition*)
                            file append (format :latex)
                            meter)
  (declare (ignore meter))

  (let* ((stream (create-output-stream file append))
         (averages (summarize-update data :condition condition :format format))
         (naggregates (length averages))
         (alabel "Aggregate")
         (ncolumns 11)
         #+:latex
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty))))
    
    (when (or (null averages) (= naggregates 1))
      (return-from analyze-update 1))
    
    (case format
      (:tcl
       (when *statistics-tcl-formats* 
         (format stream *statistics-tcl-formats*))
       (format
        stream
        "layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 2 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 6 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 8 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col 9 -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout col ~a -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%"
        ncolumns
        (+ naggregates 1) (+ naggregates 2))
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         region 1 1 2 1 -contents {~a} -format title ~
           -hor_justify left -ver_justify center~%~
         region 1 3 1 4 -contents {gold} ~
           -format title -hor_justify center~%~
         region 1 5 1 6 -contents {matches} ~
           -format title -hor_justify center~%~
         region 1 7 1 8 -contents {update} ~
           -format title -hor_justify center~%~
         region 1 9 1 9 -contents {} ~
           -format title -hor_justify center~%~
         region 1 10 1 11 -contents {final} ~
           -format title -hor_justify center~%"
        alabel alabel)
       (loop
           for i from 2 to 11
           for label in '("items\\n#" "in\\n\\330" "out\\n\\330"
                          "yes\\n\\330" "no\\n\\330"
                          "in\\n\\330" "out\\n\\330"
                          "new\\n\\330"
                          "in\\n\\330" "out\\n\\330")
           do
             (format
              stream
              "cell 2 ~d -contents \"~a\" -format title~%~
               region 2 ~d 2 ~d -contents \"~a\" -format title ~
                 -hor_justify center~%"
              i label i i label))))
    (loop
        with i = 2
        for aggregate in (remove :total averages :key #'first)
        for data = (rest (rest aggregate))
        for name = (second aggregate)
        do
          (incf i)
          (case format
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~,1f -format data~%~
               cell ~d 4 -contents ~,1f -format data~%~
               cell ~d 5 -contents ~,1f -format data~%~
               cell ~d 6 -contents ~,1f -format data~%~
               cell ~d 7 -contents ~,1f -format data~%~
               cell ~d 8 -contents ~,1f -format data~%~
               cell ~d 9 -contents ~,1f -format data~%~
               cell ~d 10 -contents ~,1f -format data~%~
               cell ~d 11 -contents ~,1f -format data~%"
              i name i (get-field :nitems data)
              i (get-field :gin data) i (get-field :gout data)
              i (get-field :matches data) i (get-field :mismatches data)
              i (get-field :pin data) i (get-field :pout data)
              i (get-field :new data)
              i (get-field :in data) i (get-field :out data)))))
    (let* ((data (rest (rest (assoc :total averages))))
           (name "Total")
           (i (+ naggregates 2)))
      (case format
        (:tcl
         (format
          stream
          "cell ~d 1 -contents {~a} -format aggregate~%~
           cell ~d 2 -contents ~d -format data~%~
           cell ~d 3 -contents ~,1f -format data~%~
           cell ~d 4 -contents ~,1f -format data~%~
           cell ~d 5 -contents ~,1f -format data~%~
           cell ~d 6 -contents ~,1f -format data~%~
           cell ~d 7 -contents ~,1f -format data~%~
           cell ~d 8 -contents ~,1f -format data~%~
           cell ~d 9 -contents ~,1f -format data~%~
           cell ~d 10 -contents ~,1f -format data~%~
           cell ~d 11 -contents ~,1f -format data~%"
          i name i (get-field :nitems data)
          i (get-field :gin data) i (get-field :gout data)
          i (get-field :matches data) i (get-field :mismatches data)
          i (get-field :pin data) i (get-field :pout data)
          i (get-field :new data)
          i (get-field :in data) i (get-field :out data)))))
    
    (when (or (stringp file) (stringp append)) (close stream))
    0))

(defun summarize-update (data &key condition format)
  (loop
      with result = nil
      with tnitems = 0
      with tmatches = 0
      with tmismatches = 0
      with tnew = 0
      with tgin = 0
      with tgout = 0
      with tpin = 0
      with tpout = 0
      with tin = 0
      with tout = 0
      with items = (select '("parse-id" "t-version" 
                             "u-matches" "u-mismatches" "u-new"
                             "u-gin" "u-gout" "u-pin" "u-pout"
                             "u-in" "u-out")
                           '(:integer :integer
                             :integer :integer :integer 
                             :integer :integer :integer :integer  
                             :integer :integer)
                           "update" condition data :sort :parse-id)
      with aggregates = (aggregate-by-classes 
                         items '(0 1 2) 
                         :dimension :u-new :format format)
      for aggregate in aggregates
      for data = (rest (rest aggregate))
      for anitems = (length data)
      for amatches = 0
      for amismatches = 0
      for anew = 0
      for agin = 0
      for agout = 0
      for apin = 0
      for apout = 0
      for ain = 0
      for aout = 0
      do
        (loop
            for item in data
            do
              (incf amatches (get-field :u-matches item))
              (incf amismatches (get-field :u-mismatches item))
              (incf anew (get-field :u-new item))
              (incf agin (get-field :u-gin item))
              (incf agout (get-field :u-gout item))
              (incf apin (get-field :u-pin item))
              (incf apout (get-field :u-pout item))
              (incf ain (get-field :u-in item))
              (incf aout (get-field :u-out item)))
        (push (append
               (list (first aggregate) (second aggregate))
               (pairlis '(:nitems :matches :mismatches :new 
                          :gin :gout :pin :pout :in :out)
                        (list anitems 
                              (divide amatches anitems)
                              (divide amismatches anitems)
                              (divide anew anitems)
                              (divide agin anitems)
                              (divide agout anitems)
                              (divide apin anitems)
                              (divide apout anitems)
                              (divide ain anitems)
                              (divide aout anitems))))
              result)
        (incf tnitems anitems)
        (incf tmatches amatches) (incf tmismatches amismatches)
        (incf tnew anew)
        (incf tgin agin) (incf tgout agout)
        (incf tpin apin) (incf tpout apout)
        (incf tin ain) (incf tout aout)
      finally (return 
                (nreverse
                 (cons
                  (append 
                   (list :total "Total")
                   (pairlis '(:nitems :matches :mismatches :new
                              :gin :gout :pin :pout :in :out)
                            (list tnitems 
                                  (divide tmatches tnitems)
                                  (divide tmismatches tnitems)
                                  (divide tnew tnitems)
                                  (divide tgin tnitems)
                                  (divide tgout tnitems)
                                  (divide tpin tnitems)
                                  (divide tpout tnitems)
                                  (divide tin tnitems)
                                  (divide tout tnitems))))
                  result)))))

(defun update-match-p (frame)
  ;;
  ;; during updates, a `save' match is indicated by the following conditions:
  ;;
  ;;   - the current item has not been tree annotated already;
  ;;   - the number of active trees in the current set equals the number of
  ;;     active trees in the gold set;
  ;;   - either the current item has more than one reading, or that single one
  ;;     reading has the exact same derivation as the preferred tree from the
  ;;     gold set.
  ;;   - also, when in `exact-match' update mode, be content if there is at
  ;;     least one unique result.
  ;;
  (or (and (lkb::compare-frame-exact frame)
           (lkb::compare-frame-in frame)
           #+:null
           (null (rest (lkb::compare-frame-in frame))))
      (and (or (null (lkb::compare-frame-version frame))
               (equal(lkb::compare-frame-version frame) ""))
           (integerp (lkb::compare-frame-gactive frame))
           (= (length (lkb::compare-frame-in frame)) 
              (lkb::compare-frame-gactive frame))
           (or (not (= (length (lkb::compare-frame-edges frame)) 1))
               (derivation-equal 
                (lkb::compare-frame-gderivation frame)
                (loop
                    with id = (lkb::edge-id 
                               (first (lkb::compare-frame-in frame)))
                    for derivation in (lkb::compare-frame-derivations frame)
                    thereis (when (= (derivation-id derivation) id)
                              derivation)))))))

(defun export-trees (data &key (condition *statistics-select-condition*)
                               path prefix interrupt meter 
                               (compressor "gzip -c -9") (suffix "gz")
                               (stream *tsdb-io*))
  
  (loop
      with offset = (cond
                     ((search "vm6" data) 60000)
                     ((search "vm13" data) 130000)
                     ((search "vm31" data) 310000)
                     ((search "vm32" data) 320000)
                     ((search "ecoc" data) 1000000)
                     ((search "ecos" data) 2000000)
                     ((search "ecpa" data) 3000000)
                     ((search "ecpr" data) 4000000)
                     (t 0))
      with target = (format 
                     nil 
                     "~a/~a"
                     (or path "/lingo/oe/tmp") (directory2file data))
      with lkb::*chart-packing-p* = nil
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze
                    data :thorough '(:derivation :mrs)
                    :condition condition :commentp t)
      with increment = (when (and meter items)
                         (/ (- (get-field :end meter) (get-field :start meter))
                            (length items) 1))
      with gc-strategy = (install-gc-strategy 
                          nil :tenure *tsdb-tenure-p* :burst t :verbose t)

      initially
        #+:allegro (ignore-errors (mkdir target))
        (when meter (meter :value (get-field :start meter)))
      for item in items
      for i-wf = (get-field :i-wf item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for i-comment = (get-field :i-comment item)
      for parse-id = (get-field :parse-id item)
      for results = (let ((results (get-field :results item)))
                      (sort (copy-list results) #'< 
                            :key #'(lambda (foo) (get-field :result-id foo))))
      for trees = (select '("t-active" "t-version") '(:integer :integer) 
                          "tree" 
                          (format nil "parse-id == ~a" parse-id) 
                          data)
      for version = (when trees
                      (loop
                          for tree in trees
                          maximize (get-field :t-version tree)))
      for active = (when version
                     (let ((foo (select '("result-id") '(:integer) 
                                        "preference" 
                                        (format 
                                         nil 
                                         "parse-id == ~a && t-version == ~d" 
                                         parse-id version) 
                                        data)))
                       (loop 
                           for bar in foo 
                           collect (get-field :result-id bar))))
      for file = (format 
                  nil 
                  "~a/~@[~a.~]~d~@[.~a~]" 
                  target prefix (+ parse-id offset) suffix)
      when results do
        (format 
         stream 
         "[~a] export-trees(): [~a] ~a active tree~:[~;s~] (of ~d).~%" 
         (current-time :long :short)
         (+ parse-id offset)
         (if version (length active) "all")
         (or (null version) (> (length active) 1))
         (length results))
        (clrhash *reconstruct-cache*)
        
        #+:allegro
        (multiple-value-bind (stream foo pid)
            (run-process
             compressor :wait nil :input :stream
             :output file :if-output-exists :supersede
             :error-output nil)
          (declare (ignore foo #-:allegro pid))

          (format
           stream
           ";;;~%;;; Redwoods export of `~a';~%;;; (~a@~a; ~a).~%;;;~%~%"
           data (current-user) (current-host) (current-time :long :pretty))
          (format 
           stream
           "[~d] (~a of ~d) {~d} `~a'~@[ [~a]~]~%~a~%"
           (+ parse-id offset)
           (if version (length active) "all") (length results) i-wf
           input i-comment #\page)
          
          (export-tree item active :offset offset :stream stream)
          (unless *redwoods-thinning-export-p*
            (export-tree
             item active :complementp t :offset offset :stream stream))

          (force-output stream)
          (close stream)
          (sys:os-wait nil pid))
        
        (when increment (meter-advance increment))
      when (interrupt-p interrupt) do
        (format 
         stream
         "[~a] export-trees(): external interrupt signal~%"
         (current-time :long :short))
        (force-output stream)
        (return)
      finally
        (when meter (meter :value (get-field :end meter)))
        (when gc-strategy (restore-gc-strategy gc-strategy))))

(defun export-tree (item active 
                    &key complementp (offset 0) (stream *tsdb-io*))

  #+:debug
  (setf %item% item %active% active)
  (loop
      with *package* = (find-package :lkb)
      with lkb::*deleted-daughter-features* = 
        (if (or (eq *redwoods-export-values* :all)
                (smember :avm *redwoods-export-values*))
          nil
          lkb::*deleted-daughter-features*)
      with i-input = (get-field :i-input item)
      with i-id = (get-field :i-id item)
      with i-comment = (get-field :i-comment item)
      with parse-id = (get-field :parse-id item)
      with results = (get-field :results item)
      for i from 1
      for result in results
      for result-id = (get-field :result-id result)
      for derivation = (when (if complementp
                               (not (member result-id active :test #'eql))
                               (member result-id active :test #'eql))
                         (get-field :derivation result))
      for edge = (and derivation (reconstruct derivation))
      for tree = (when (and edge
                            (or (eq *redwoods-export-values* :all)
                                (smember :tree *redwoods-export-values*)))
                   (let ((tree (ignore-errors
                                (lkb::parse-tree-structure edge))))
                     (unless tree
                       (format 
                        stream 
                        "[~a] export-trees(): [~a] ~
                         error() labeling tree # ~a.~%" 
                        (current-time :long :short)
                        (+ parse-id offset)
                        result-id))
                     tree))
      for dag = (and edge (let ((tdfs (lkb::edge-dag edge)))
                            (and (lkb::tdfs-p tdfs)
                                 (lkb::tdfs-indef tdfs))))
      for mrs = (or (get-field :mrs result)
                    (and edge (mrs::extract-mrs edge)))
      for ident = (format nil "~a @ ~a~@[ @ ~a~]" i-id result-id i-comment)
      when (zerop (mod i 100)) do (clrhash *reconstruct-cache*)
      when (or dag mrs) do
        (format 
         stream 
         "[~d:~d] ~:[(active)~;(inactive)~]~%~%" 
         (+ parse-id offset) result-id complementp)
        (setf lkb::*cached-category-abbs* nil)
        (when (or (eq *redwoods-export-values* :all)
                  (smember :derivation *redwoods-export-values*))
          (let ((*package* (find-package :tsdb)))
            (format stream "~s~%~%~%" derivation)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :tree *redwoods-export-values*))
          (if tree
            (format stream "~a~%~%" tree)
            (format stream "()~%~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :avm *redwoods-export-values*))
          (lkb::display-dag1 dag 'lkb::compact stream)
          (format stream "~%~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :mrs *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::simple stream))
        (when (and (not (eq *redwoods-export-values* :all))
                   (smember :indexed *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::indexed stream))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :prolog *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::prolog stream)
          (format stream "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :rmrs *redwoods-export-values*))
          (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::compact stream)
          (format stream "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :xml *redwoods-export-values*))
          (mrs::output-rmrs1
           (mrs::mrs-to-rmrs mrs)
           'mrs::xml stream nil nil i-input ident)
          (format stream "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dependencies *redwoods-export-values*))
          (mrs::ed-output-psoa mrs :stream stream))
        #|
        (when (smember :qa *redwoods-export-values*)
          (mrs::output-rmrs-from-fine-system
           (+ parse-id offset) 
           (or (get-field :o-input item) (get-field :i-input item))
           mrs))
           |#
        (format stream "~c~%" #\page)))

(defun semantic-equivalence (data &key condition (file "/tmp/equivalences"))
  
  (loop
      with stream = (open file :direction :output :if-exists :supersede)
      with lkb::*chart-packing-p* = nil
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with items = (analyze data :thorough '(:derivation) 
                            :condition condition :readerp nil)
      for item in items
      for i-id = (get-field :i-id item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for results = (nreverse (copy-list (get-field :results item)))
      do
        (clrhash *reconstruct-cache*)
        (format t "~a: [~a] `~a'~%" i-id (length results) input)
        (format stream "~a: [~a] `~a'~%" i-id (length results) input)
        (loop
            with *package* = (find-package :lkb)
            for result in results
            for derivation = (get-field :derivation result)
            for edge = (when derivation (reconstruct derivation))
            for id = (when edge (lkb::edge-id edge))
            for mrs = (when edge (mrs::extract-mrs edge))
            do (nconc result (pairlis '(:id :mrs) (list id mrs))))
        (loop
            for result = (pop results)
            for id1 = (get-field :id result)
            for mrs1 = (get-field :mrs result)
            while result do
              (format stream "~a:"id1)
              (loop
                  for foo in results
                  for id2 = (get-field :id foo)
                  for mrs2 = (get-field :mrs foo)
                  when (apply #'mrs::mrs-equalp mrs1 mrs2 '(t nil)) do
                    (format stream " ~a" id2))
              (format stream "~%"))
        (format stream "~a~%" #\page)
      finally (close stream)))

(defun analyze-scores (data 
                       &optional (gold data)
                       &key (condition *statistics-select-condition*)
                            spartanp (scorep t) (n 1)  test loosep
                            file append (format :latex)
                            meter)

  (let* ((stream (create-output-stream file append))
         (aggregates (summarize-scores
                      data gold :condition condition 
                      :spartanp spartanp :scorep scorep :n n
                      :test test :loosep loosep
                      :format format :meter meter))
         (aggregates (nreverse aggregates))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty)))
         (n (if (and n (> n 1)) (- n 1) 0))
         (ncolumns (+ n 
                      (if loosep 8 7) 
                      (if *redwoods-score-similarity-p* 1 0)))
         (i 2))

    ;;
    ;; _fix_me_
    ;; the :random values appear bogus; debug this further.     (13-may-05; oe)
    ;;
    #+:null
    (let ((total (rest (rest (find :total aggregates :key #'first)))))
      (format
       t
       "~,1f of ~a = ~,2f~%"
       (get-field :random total) (get-field :scores total)
       (* 100 (divide (get-field :random total) (get-field :scores total)))))
    
    (case format
      (:latex
        (format
         stream
         "\\begin{tabular}{@{}|l|c|c|c|c|c|~:[~;c|~]~:[~;c|~]c|@{}}~%  ~
          \\hline~%  ~
          \\multicolumn{~d}{|c|}~%    {\\bf `~a' ~a Profile}\\\\~%  ~
          \\hline\\hline~%  ~
          & {\\bf  total} & {\\bf total} & {\\bf word} ~
            & {\\bf parser}~%    ~
            & {\\bf exact} & {\\bf near}~:[~; & {\\bf loose}~]~
            ~:[~; & {\\bf simlarity}~]~
            & {\\bf overall}\\\\~%  ~
          {\\bf ~a} & {\\bf items} & {\\bf scores} & {\\bf string} ~
            & {\\bf analyses}~%    ~
            & {\\bf matches} & {\\bf matches}~:[~; & {\\bf matches}~]~%    ~
            ~:[~; & {\\bf matches}~]~
            & {\\bf accuracy}\\\\~%  ~
          & $\\sharp$ & $\\sharp$ & $\\phi$ & $\\phi$~%    ~
            & $\\sharp$ & $\\sharp$~:[~; & $\\sharp$~] & $\\%$\\\\~%  ~
          \\hline~%  ~
          \\hline~%"
         loosep *redwoods-score-similarity-p*
         ncolumns
         (if (stringp data) data "Some") "Parse Selection"
         loosep 
         alabel loosep *redwoods-score-similarity-p*
         loosep *redwoods-score-similarity-p*))
      (:tcl
       (format stream *statistics-tcl-formats*)
       (format
        stream
        "flags 2~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%"
        ncolumns)
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"total\\nscores\\n#\" -format title~%~
         cell 1 4 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 5 -contents \"parser\\nanalyses\\n\\330\" -format title~%~
         cell 1 6 -contents \"exact\\nmatches\\n#\" -format title~%~
         ~:[~*~;cell 1 ~d -contents \"loose\\nmatches\\n#\" -format title~%~]~
         ~:[~*~;cell 1 ~d -contents \"similar\\nmatches\\n#\" ~
                  -format title~%~]~
         cell 1 ~d -contents \"overall\\naccuracy\\n%\" -format title~%"
        alabel 
        loosep (+ 7 n) 
        *redwoods-score-similarity-p* (+ 8 n) ncolumns)
       (unless (zerop n)
         (loop
             for j from 0 to (- n 2)
             do
               (format
                stream
                "layout col ~d -m1 5 -r 0 -m2 5 -c black -j right~%"
                (+ 7 j)))

         (format
          stream
          "~:[~;cell 1 7 -contents \"near\\nmatches\\n#\" -format title~%~]~
           region 1 7 1 ~d -contents \"near\\nmatches\\n#\" -format title ~
           -hor_justify center -ver_justify center~%"
          (= n 1) (+ 7 (- n 1))))
       (format stream "~%")))

    (loop
        for (id foo . data) in aggregates
        for name = (if (eq format :latex) (latexify-string foo) foo)
        for items = (get-field :items data)
        for scores = (get-field :scores data)
        for length = (get-field+ :i-length data 0)
        for analyses = (get-field :analyses data)
        for exact = (get-field :exact data)
        for near = (get-field :near data)
        for successes = (get-field :successes data)
        for loose = (and loosep (get-field :loose data))
        for similarity = (let ((similarity (get-field+ :similarity data 0)))
                           (if (zerop scores)
                             100
                             (divide similarity scores)))
        for accuracy = (if (zerop scores)
                         100
                         (* (divide (+ exact near) scores) 100))
        unless (or (smember id '(:all :total)) (zerop scores)) do
          (setf id id)
          (case format
            (:latex
             (format
              stream
              "  ~a & ~d & ~d & ~,2f & ~,2f ~
               & ~,1f & ~,1f~@[ & ~d~] & ~,2f\\\\~%"
              name items scores length analyses 
              exact near loose accuracy))
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~d -format data~%~
               cell ~d 4 -contents ~,2f -format data~%~
               cell ~d 5 -contents ~,2f -format data~%~
               cell ~d 6 -contents ~,1f -format data~%~
               ~:[~*~*~*~;cell ~d ~d -contents ~d -format data~%~]~
               ~:[~*~*~*~;cell ~d ~d -contents ~,4f -format data~%~]~
               cell ~d ~d -contents ~,2f -format data~%"
              i name
              i items
              i scores
              i length
              i analyses
              i exact
              loosep i (+ 7 n) loose
              *redwoods-score-similarity-p* i (+ 8 n) similarity 
              i ncolumns accuracy)
             (unless (zerop n)
               (loop
                   for j from 0 to (- n 1)
                   for k = (aref successes (+ j 1))
                   do
                     (format
                      stream
                      "cell ~d ~d -contents ~,1f -format data~%"
                      i (+ 7 j) k)))
             (format stream "~%")))
          (incf i))
    
    (let* ((data (rest (rest (assoc :total aggregates))))
           (name "Total")
           (items (get-field :items data))
           (scores (get-field :scores data))
           (length (get-field+ :i-length data 0))
           (analyses (get-field :analyses data))
           (exact (get-field :exact data))
           (near (get-field :near data))
           (successes (get-field :successes data))
           (loose (and loosep (get-field :loose data)))
           (similarity (let ((similarity (get-field+ :similarity data 0)))
                         (if (zerop scores)
                           100
                           (divide similarity scores))))
           (accuracy (if (zerop scores)
                       100
                       (* (divide (+ exact near) scores) 100))))
      (case format
        (:latex
         (format
          stream
          "~:[~;  \\hline~%  \\hline~%~]  ~
           {\\bf ~a} & {\\bf ~d} & {\\bf ~d} & {\\bf ~,2f} & {\\bf ~,2f}~%    ~
           & {\\bf ~,1f} & {\\bf ~,1f}~@[ & {\\bf ~d}~] & {\\bf ~,2f}\\\\~%  ~
           \\hline~%"
          (= i 1)
          name items scores length analyses exact near loose accuracy)
         (format
          stream
          "  \\multicolumn{~d}{r}{\\tiny ~%    ~a}~%~
           \\end{tabular}~%"
          ncolumns caption))
        (:tcl
         (format
          stream
          "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%~
           cell ~d 1 -contents {~a} -format total~%~
           cell ~d 2 -contents ~d -format total~%~
           cell ~d 3 -contents ~d -format total~%~
           cell ~d 4 -contents ~,2f -format total~%~
           cell ~d 5 -contents ~,2f -format total~%~
           cell ~d 6 -contents ~,1f -format total~%~
           ~:[~*~*~*~;cell ~d ~d -contents ~d -format total~%~]~
           ~:[~*~*~*~;cell ~d ~d -contents ~,4f -format total~%~]~
           cell ~d ~d -contents ~,2f -format total~%"
          (- i 1) i
          i name
          i items
          i scores
          i length
          i analyses
          i exact
          loosep i (+ 7 n) loose
          *redwoods-score-similarity-p* i (+ 8 n) similarity
          i ncolumns accuracy)
         (unless (zerop n)
           (loop
               for j from 0 to (- n 1)
               for k = (aref successes (+ j 1))
               do
                 (format
                  stream
                  "cell ~d ~d -contents ~,1f -format total~%"
                  i (+ 7 j) k)))
         (format stream "~%"))))
    (when (or (stringp file) (stringp append)) (close stream))))
 
(defun summarize-scores (data &optional (gold data)
                         &key (condition *statistics-select-condition*)
                              spartanp (scorep t) (n 1) test loosep
                              (trace *redwoods-trace*)
                              (format :latex) meter)
  (declare (ignore meter))
  
  ;;
  ;; score results in .data. against ground truth in .gold.  operates in
  ;; several slightly distinct modes: (i) using the implicit parse ranking in
  ;; the order of `results' or (ii) using an explicit ranking from the `score'
  ;; relation; an orthogonal dimension of variation is (a) scoring by result
  ;; identifier (e.g. within the same profile or against one that is comprised
  ;; of identical results) vs. (b) scoring by derivation equivalence (e.g.
  ;; when comparing best-first parser output against a gold standard).
  ;;
  (let* ((thorough (when (eq test :derivation) '(:derivation)))
         ;;
         ;; _fix_me_
         ;; ideally, score-item() should not have to look at anything from the
         ;; `result' relation but the result identifier; the following for now
         ;; includes the `tree' field, so we can perform a string similarity
         ;; comparison on generator outputs.                   (28-oct-04; oe)
         ;;
         (thorough (if (or trace *redwoods-score-similarity-p*)
                     (cons :tree thorough)
                     thorough))
         (items (if (stringp data)
                  (analyze (if spartanp gold data)
                           :thorough (or thorough spartanp)
                           :condition condition 
                           :score (if scorep data t) :scorep t
                           :readerp (eq test :derivation))
                  data))
         (aggregates (aggregate items :format format))
         (gitems (if (stringp gold)
                   (analyze gold
                            :thorough thorough
                            :condition condition :gold gold 
                            :readerp (eq test :derivation))
                   gold))
         (gaggregates (aggregate-by-analogy gitems aggregates :loosep t))
         results)

    #+:debug
    (setf %items items %aggregates aggregates
          %gitems gitems %gaggregates gaggregates)

    (loop
        with tnitems = 0
        with tnscores = 0
        with tlength = 0
        with treadings = 0
        with texact = 0
        with tnear = 0
        with tloose = 0
        with tsimilarity = 0
        with tsuccesses = (and n (make-array n :initial-element 0))
        with trandom = 0
        for (id name . data) in aggregates
        for gaggregate = (when (equal (first (first gaggregates)) id)
                           (pop gaggregates))
        for gdata = (rest (rest gaggregate))
        when gdata do
          (loop
              with anitems = 0
              with anscores = 0
              with alength = 0
              with areadings = 0
              with aexact = 0
              with anear = 0
              with aloose = 0
              with asimilarity = 0
              with asuccesses = (and n (make-array n :initial-element 0))
              with arandom = 0
              for item in data
              for i-id = (get-field :i-id item)
              for length = (get-field :i-length item)
              for readings = (get-field :readings item)
              for gitem = (loop
                              for gitem = (first gdata)
                              while (and gitem
                                         (< (get-field :i-id gitem) i-id))
                              do (pop gdata)
                              finally
                                (return 
                                  (let ((i (get-field :i-id (first gdata))))
                                    (when (and i (= i i-id))
                                      (pop gdata)))))
                          
              when gitem do
                (multiple-value-bind (i score loosep similarity)
                    (score-item item gitem 
                                :test test :n n :loosep loosep)
                  
                  (when (and trace (open-stream-p trace) i)
                    (format
                     trace
                     "[~a] <~,1f~@[:~,2f~]~@[ @ ~a~]> |~a|~%"
                     i-id score 
                     (and (zerop i) similarity)
                     (and (> i 0) i) (get-field :i-input gitem))
                    (loop
                        with granks = (get-field :ranks gitem)
                        for i from 1 to n
                        do
                          (loop
                              for grank in granks
                              when (= (get-field :rank grank) i)
                              do
                                (format
                                 trace
                                 "  < [~a] {~a} |~a|~%"
                                 (get-field :result-id grank)
                                 i (get-field :tree grank))))
                    (loop
                        for rank in (get-field :ranks item)
                        for score = (let ((foo (get-field :score rank)))
                                      (if (stringp foo)
                                        (read-from-string foo)
                                        foo))
                        do
                          (format
                           trace
                           "  > [~a] {~a} ~@[<~,6f>~] |~a|~%"
                           (get-field :result-id rank)
                           (get-field :rank rank)
                           score
                           (get-field :tree rank)))
                    (format trace "~%")
                    (force-output trace))
                  
                  (incf anitems)
                  (when i (incf arandom (divide 1 areadings)))
                  (cond
                   ((null i))
                   ((zerop i)
                    (incf anscores) 
                    (incf alength length) (incf areadings readings)
                    (when (numberp similarity) (incf asimilarity similarity)))
                   (t
                    (incf anscores) 
                    (incf alength length) (incf areadings readings)
                    (when (<= i n)
                      (if (= i 1) (incf aexact score) (incf anear score)))
                    (when loosep (incf aloose))
                    (incf asimilarity (or similarity 1))
                    (when asuccesses 
                      (incf (aref asuccesses (- i 1)) score)))))
              finally
                (incf tnitems anitems) (incf tnscores anscores)
                (incf tlength alength) (incf treadings areadings) 
                (incf texact aexact) (incf tnear anear) (incf tloose aloose)
                (incf tsimilarity asimilarity)
                (loop
                    for i from 0
                    for j across asuccesses
                    do
                      (incf (aref tsuccesses i) j))
                (incf trandom arandom)
                (push (nconc (list id name)
                             (pairlis '(:items :scores 
                                        :i-length 
                                        :analyses
                                        :exact :near :loose 
                                        :successes :similarity :random)
                                      (list anitems anscores 
                                            (divide alength anscores)
                                            (divide areadings anscores)
                                            aexact anear aloose 
                                            asuccesses asimilarity arandom)))
                      results))
        finally
          (push (nconc (list :total "Total")
                       (pairlis '(:items :scores 
                                  :i-length
                                  :analyses
                                  :exact :near :loose 
                                  :successes :similarity :random)
                                (list tnitems tnscores 
                                      (divide tlength tnscores)
                                      (divide treadings tnscores)
                                      texact tnear tloose 
                                      tsuccesses tsimilarity trandom)))
                results))
    (when (eq test :derivation)
      (purge-profile-cache data :expiryp nil)
      (unless (equal data gold) (purge-profile-cache gold :expiryp nil)))

    results))

(defun score-item (item gold &key test (n 1) (loosep t) errorp)
  
  #+:debug
  (setf %item% item %gold% gold)
  
  (let ((ranks (get-field :ranks item))
        (granks (get-field :ranks gold))
        (test (cond
               ((functionp test) test)
               ((or (null test) (eq test :id))
                #'(lambda (old new)
                    (let ((foo (get-field :result-id old))
                          (bar (get-field :result-id new)))
                      (and foo bar (= foo bar)))))
               ((eq test :derivation)
                #'(lambda (old new)
                    (let ((foo (get-field :derivation old))
                          (bar (get-field :derivation new)))
                      (and foo bar (derivation-equal foo bar))))))))

    ;;
    ;; check calling context: both the test item and ground truth needs to come
    ;; with ranks; all ranks in the ground truth are 1 (since they came from
    ;; annotations in a treebank); and unless .loosep. is on, there must not be
    ;; more than one gold target.
    (cond
     ((or (null ranks) (null granks)) nil)
     ((loop
          for grank in granks
          for rank = (get-field :rank grank)
          thereis (or (not (integerp rank)) (not (= rank 1))))
      nil)
     ((and (rest granks) (null loosep)) nil)
     
     ;;
     ;; now do up to three nested searches, aiming to find the best .match. for
     ;; one of the gold targets, i.e. the one with the smallest rank (when
     ;; ranking within an n-best beam of more than 1 is enabled) or otherwise
     ;; the one with the smallest set of ties.  for each .match. find the set
     ;; of results that the model put at a lower or the same rank, ignoring the
     ;; ones that are a gold target themselves.
     ;;
     (t
      (loop
          with result = nil with best = nil
          for grank in granks
          for match = (loop
                          for rank in ranks
                          for i = (get-field :rank rank)
                          while (or (null n) errorp (<= i n))
                          thereis (and (funcall test rank grank) rank))
          for i = (get-field :rank match)
          for matches = (when i
                          (loop
                              for rank in ranks
                              for j = (get-field :rank rank)
                              while (<= j i)
                              when (and (= i j)
                                        (not (loop
                                                 for grank in granks
                                                 thereis (funcall 
                                                          test rank grank))))
                              collect rank))
          for errors = (when errorp
                         (if i
                           (loop
                               for rank in ranks
                               for j = (get-field :rank rank)
                               while (< j i) collect rank)
                           ranks))
          when (and (numberp i)
                    (or (null best)
                        (< i (get-field :rank best))
                        (and (= i (get-field :rank best)) 
                             (< (length matches) (length result)))))
          do
            (setf best match)
            (setf result matches)
          finally 
            (let ((similarity
                   (when *redwoods-score-similarity-p*
                     (loop
                         with sum = 0 with n = 0
                         with ranks = (loop
                                          for rank in ranks
                                          while (= (get-field :rank rank) 1)
                                          collect rank)
                         with granks = (loop
                                           for rank in granks
                                           while (= (get-field :rank rank) 1)
                                           collect rank)
                         for rank in ranks
                         do
                           ;;
                           ;; _fix_me_
                           ;; this is not quite what is common in MT usages of
                           ;; BLEU and related measures: a more charitable way
                           ;; of computing the overall score here would be to
                           ;; maximize over all references.    (10-may-05; oe)
                           ;;
                           (loop
                               for grank in granks
                               for score = (call-raw-hook 
                                            *redwoods-score-similarity-p* 
                                            rank grank)
                               when (numberp score) do 
                                 (incf sum score) (incf n))
                         finally
                           (return (divide sum n))))))
              (return (values (if best (get-field :rank best) 0)
                              (if best (divide 1 (+ (length result) 1)) 0)
                              (rest granks) similarity
                              errors match (delete match result)))))))))

(defun bleu-similarity (rank grank)
  (let ((tree (get-field :tree rank))
        (gtree (get-field :tree grank)))
    (first (bleu-score-strings (list tree) (list gtree)))))

(defun analyze-errors (data 
                       &optional (gold data)
                       &key (condition *statistics-select-condition*)
                            spartanp (scorep t) (n 1)  test loosep
                            file append (format :tcl)
                            meter)
  (declare (ignore meter))

  (let* ((errors (summarize-errors data gold :condition condition
                                   :spartanp spartanp :scorep scorep :n n
                                   :test test :loosep loosep))
         (stream (create-output-stream file append)))
    (when (listp errors)
      (case format
        (:tcl
         (when *statistics-tcl-formats* 
           (format stream *statistics-tcl-formats*))
         (format
          stream
          "flags 0~%~
           layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
           layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
           layout col 0 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 1 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 2 -m1 5 -r 2 -m2 5 -c black -j left~%~
           layout col 4 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout col 7 -m1 5 -r 2 -m2 5 -c black -j right~%~
           layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row 2 -m1 5 -r 2 -m2 5 -c black -j center~%")
         (format
          stream
          "cell 1 1 -contents {i-id} -format title~%~
           region 1 1 2 1 -contents {i-id} ~
             -format title -hor_justify center~%~
           cell 1 2 -contents {i-input} -format title~%~
           region 1 2 2 2 -contents {i-input} ~
             -format title -hor_justify center~%~
           cell 1 3 -contents {xxxx} -format title~%~
           cell 1 4 -contents {xxxx} -format title~%~
           region 1 3 2 4 -contents {readings} ~
             -format title -hor_justify center~%")
         (format
          stream
          "region 1 5 1 7 -contents {scores} ~
             -format title -hor_justify center~%~
           cell 2 5 -contents {<} -format title~%~
           cell 2 6 -contents {H(p)} -format title~%~
           region 2 6 2 6 -contents {H(p)} ~
             -format title -hor_justify center~%~
           cell 2 7 -contents {=} -format title~%")))
      (loop
          for (item gitem rank errors match others entropy) in errors
          for i-id = (get-field :i-id item)
          for i-input = (or (get-field :o-input item)
                            (get-field :i-input item))
          for greadings = (get-field :readings gitem)
          for readings = (get-field :readings item)
          for tag = (intern (gensym "") :keyword)
          with i = 3
          when (zerop (mod (- i 2) 10))
          do 
            (case format
              (:tcl
               (format
                stream
                "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                i)))
          do
            ;;
            ;; _fix_me_ this creates a potential memory leak: as soon as the
            ;; window for this table is destroyed, there will be no further
            ;; reference to the (tag) symbols used to store data on the lisp
            ;; side.  yet, the values associated with the symbol properties
            ;; will never become unbound.                         (16-feb-03)
            ;;
            (setf rank rank)
            (setf (get :source tag) gold)
            (setf (get :i-id tag) i-id)
            (setf (get :i-input tag) i-input)
            (setf (get :match tag) match)
            (setf (get :errors tag) errors)
            (setf (get :others tag) others)
            (format
             stream
             "cell ~d 1 -contents {~a} -format data~%~
              cell ~d 2 -contents {~a} -format data -key ~d -source {~a}~%~
              cell ~d 3 -contents {~a} -format data~%~
              cell ~d 4 -contents {~a} -format data~%~
              cell ~d 5 -contents {~a} -format data~%~
              cell ~d 6 -contents {~,4f} -format data ~
                -action inspect -tag ~a~%~
              cell ~d 7 -contents {~a} -format data~%"
             i i-id 
             i i-input i-id data
             i greadings
             i readings
             i (length errors)
             i entropy tag
             i (length others))
            (incf i)
          finally
            (when (> i 3)
              (case format
                (:tcl
                 (format
                  stream
                  "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
                   cell ~d 1 -contents {~a} -format total~%~
                   layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%"
                  (- i 1) i (- i 3) i))))))
    
    (when (or (stringp file) (stringp append)) (close stream))
    (if (listp errors) 0 -1)))

(defun summarize-errors (data 
                         &optional (gold data)
                         &key (condition *statistics-select-condition*)
                              spartanp (scorep t) (n 1) test loosep
                              meter)
  (declare (ignore meter))
  
  (let* ((thorough (when (eq test :derivation) '(:derivation)))
         (items (if (stringp data)
                   (analyze (if spartanp gold data)
                            :thorough (or thorough spartanp)
                            :condition condition :score (if scorep data t)
                            :readerp (eq test :derivation) :scorep t)
                  data))
         (items (sort (copy-list items) #'< 
                      :key #'(lambda (foo) (get-field :i-id foo))))
         (gitems (if (stringp gold)
                   (analyze gold
                            :thorough thorough
                            :condition condition :gold gold 
                            :readerp (eq test :derivation) :scorep t)
                   gold))
         (gitems (sort (copy-list gitems) #'< 
                       :key #'(lambda (foo) (get-field :i-id foo))))
         result)

    (loop
        for item in items
        for gitem in gitems
        for i-id = (get-field :i-id item)
        for gi-id = (get-field :i-id gitem)
        when (or (not (numberp i-id)) (not (numberp gi-id))
                 (not (= i-id gi-id))) 
        do
          (setf result :error)
          (return)
        else do
          (multiple-value-bind (rank foo bar baz errors match others)
              (score-item item gitem :test test :n n :loosep loosep :errorp t)
            (declare (ignore foo bar baz))
            (when (and rank (or (zerop rank) (> rank n) others))
              (let* ((ranks (when (or errors others)
                              (get-field :ranks item)))
                     (scores (loop 
                                 for rank in ranks
                                 collect (get-field :score rank)))
                     (probabilities (scores-to-probabilities scores))
                     (entropy (entropy probabilities)))
                (push (list item gitem rank errors match others entropy)
                      result)))))
    (if (listp result) (nreverse result) result)))

(defun analyze-agreement (data1 data2
                          &key (condition *statistics-select-condition*)
                               (test '(:edge :discriminant))
                               file append (format :tcl)
                               meter)
  
  (let* ((stream (create-output-stream file append))
         (aggregates (summarize-agreement
                      data1 data2 :condition condition 
                      :test test :format format :meter meter))
         (aggregates (nreverse aggregates))
         (alabel (if (eq *statistics-aggregate-dimension* :phenomena)
                   "Phenomenon"
                   "Aggregate"))
         (caption (format 
                   nil "(generated by ~a at ~a)"
                   *tsdb-name* (current-time :long :pretty)))
         (ncolumns 12)
         (i 2))
    (declare (ignore caption))
    
    #+:debug
    (setf %aggregates aggregates)
    
    (case format
      (:tcl
       (format stream *statistics-tcl-formats*)
       (format
        stream
        "flags 0~%~
         layout col def -m1 5 -r 1 -m2 5 -c black -j right~%~
         layout row def -m1 5 -r 0 -m2 5 -c black -j center~%~
         layout col 0 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col 1 -m1 5 -r 2 -m2 5 -c black -j left~%~
         layout col ~d -m1 5 -r 2 -m2 5 -c black -j right~%~
         layout row 0 -m1 5 -r 2 -m2 5 -c black -j center~%~
         layout row 1 -m1 5 -r 2 -m2 5 -c black -j center~%"
        ncolumns)
       (format
        stream
        "cell 1 1 -contents {~a} -format title~%~
         cell 1 2 -contents \"total\\nitems\\n#\" -format title~%~
         cell 1 3 -contents \"word\\nstring\\n\\330\" -format title~%~
         cell 1 4 -contents \"parser\\nanalyses\\n\\330\" -format title~%~
         cell 1 5 -contents \"random\\nbase\\n%\" -format title~%~
         cell 1 6 -contents \"exact\\nmatch\\n%\" -format title~%~
         cell 1 7 -contents \"distinct\\nnodes\\n\\330\" -format title~%~
         cell 1 8 -contents \"node\\nmatch\\n%\" -format title~%~
         cell 1 9 -contents \"distinct\\nlabels\\n\\330\" -format title~%~
         cell 1 10 -contents \"label\\nmatch\\n%\" -format title~%~
         cell 1 11 -contents \"distinct\\nbrackets\\n\\330\" -format title~%~
         cell 1 12 -contents \"bracket\\nmatch\\n%\" -format title~%"
        alabel)))

    (loop
        for (id foo . data) in aggregates
        for name = (if (eq format :latex) (latexify-string foo) foo)
        for items = (get-field :items data)
        for length = (get-field+ :i-length data 0)
        for analyses = (get-field :analyses data)
        for random = (get-field :random data)
        for exact = (get-field :exact data)
        for nodes1 = (get-field :nodes1 data)
        for nodes2 = (get-field :nodes2 data)
        for ids = (get-field :ids data)
        for labels = (get-field :labels data)
        for brackets = (get-field :brackets data)
        for cids = (get-field :cids data)
        for clabels = (get-field :clabels data)
        for cbrackets = (get-field :cbrackets data)
        unless (or (smember id '(:all :total)) (zerop items)) do
          (case format
            (:tcl
             (format
              stream
              "cell ~d 1 -contents {~a} -format aggregate~%~
               cell ~d 2 -contents ~d -format data~%~
               cell ~d 3 -contents ~,2f -format data~%~
               cell ~d 4 -contents ~,2f -format data~%~
               cell ~d 5 -contents ~,2f -format data~%~
               cell ~d 6 -contents ~,2f -format data~%~
               cell ~d 7 -contents ~,2f -format data~%~
               cell ~d 8 -contents ~,2f -format data~%~
               cell ~d 9 -contents ~,2f -format data~%~
               cell ~d 10 -contents ~,2f -format data~%~
               cell ~d 11 -contents ~,2f -format data~%~
               cell ~d 12 -contents ~,2f -format data~%"
              i name
              i items
              i length
              i analyses
              i (* (divide random items) 100)
              i (* (divide exact items) 100)
              i ids
              i (* (divide cids ids) 100)
              i labels
              i (* (divide (* 2 (/ clabels nodes1) (/ clabels nodes2))
                           (+ (/ clabels nodes1) (/ clabels nodes2)))
                   100)
              #+:null (* (divide clabels labels) 100)
              i brackets
              i (* (divide cbrackets brackets) 100))))
          (incf i))
    
    (let* ((data (rest (rest (assoc :total aggregates))))
           (name "Total")
           (items (get-field :items data))
           (length (get-field+ :i-length data 0))
           (analyses (get-field :analyses data))
           (random (get-field :random data))
           (exact (get-field :exact data))
           (nodes1 (get-field :nodes1 data))
           (nodes2 (get-field :nodes2 data))
           (ids (get-field :ids data))
           (labels (get-field :labels data))
           (cids (get-field :cids data))
           (clabels (get-field :clabels data))
           (brackets (get-field :brackets data))
           (cbrackets (get-field :cbrackets data)))
      (case format
        (:tcl
         (format
          stream
          "layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~
           layout row ~d -m1 5 -r 2 -m2 5 -c black -j center~%~%~
           cell ~d 1 -contents {~a} -format total~%~
           cell ~d 2 -contents ~d -format total~%~
           cell ~d 3 -contents ~,2f -format total~%~
           cell ~d 4 -contents ~,2f -format total~%~
           cell ~d 5 -contents ~,2f -format total~%~
           cell ~d 6 -contents ~,2f -format total~%~
           cell ~d 7 -contents ~,2f -format total~%~
           cell ~d 8 -contents ~,2f -format total~%~
           cell ~d 9 -contents ~,2f -format total~%~
           cell ~d 10 -contents ~,2f -format total~%~
           cell ~d 11 -contents ~,2f -format total~%~
           cell ~d 12 -contents ~,2f -format total~%"
          (- i 1) i
          i name
          i items
          i length
          i analyses
          i (* (divide random items) 100)
          i (* (divide exact items) 100)
          i ids
          i (* (divide cids ids) 100)
          i labels
          i (* (divide (* 2 (/ clabels nodes1) (/ clabels nodes2))
                           (+ (/ clabels nodes1) (/ clabels nodes2)))
               100)
          #+:null (* (divide clabels labels) 100)
          i brackets
          i (* (divide cbrackets brackets) 100)))))
    (when (or (stringp file) (stringp append)) (close stream))
    (if (listp aggregates) 0 -1)))

(defun summarize-agreement (data1 data2
                            &key (condition *statistics-select-condition*)
                                 (test '(:edge :discriminant)) 
                                 (format :latex) meter)
  (declare (ignore meter))
  
  (let* ((thorough (when (smember :edge test) '(:derivation)))
         (items1 (if (stringp data1)
                   (analyze data1
                            :thorough thorough
                            :condition condition
                            :readerp (smember :derivation thorough)
                            :gold data1)
                  data1))
         (items1 (sort (copy-list items1) #'< 
                       :key #'(lambda (foo) (get-field :i-id foo))))
         (items2 (if (stringp data2)
                   (analyze data2
                            :thorough thorough
                            :condition condition
                            :readerp (smember :derivation thorough)
                            :gold data2)
                  data2))
         (items2 (sort (copy-list items2) #'< 
                       :key #'(lambda (foo) (get-field :i-id foo))))
         items results)

    ;;
    ;; now we need to do the traditional alignment of two sorted sets of data:
    ;; iterate through one, search for match in the other as long as id is less
    ;; than or equal to current position in first set.
    ;;
    (loop
        for item1 in items1
        for id1 = (get-field :i-id item1)
        for input = (get-field :i-input item1)
        for length = (get-field :i-length item1)
        for pid = (get-field :parse-id item1)
        for readings = (get-field :readings item1)
        for item2 = (when (numberp id1)
                      (loop
                          for id2 = (get-field :i-id (first items2))
                          while (and (numberp id2) (<= id2 id1)) do
                            (let ((item2 (pop items2)))
                              (when (and (numberp id2) (= id2 id1)) 
                                (return item2)))))
        for id2 = (get-field :i-id item2)
        while (numberp id1)
        when (numberp id2) 
        do
          (let ((item (pairlis '(:i-id :i-input :i-length
                                 :parse-id :readings)
                               (list id1 input length
                                     pid readings)))
                collectp)
            (when (smember :edge test)
              (let* (;;
                     ;; _fix_me_
                     ;; currently, assume we are operating on thinned profiles,
                     ;; such that we only have active derivations available; we
                     ;; need to fix the :random normalize first, before the 
                     ;; general code will work.                 (27-apr-04; oe)
                     ;;
                     (derivations1 #+:null
                                   (loop
                                       for result in (get-field :ranks item1)
                                       when (= (get-field :rank result) 1)
                                       collect (get-field :derivation result))
                                   (loop
                                       for result in (get-field :results item1)
                                       collect (get-field :derivation result)))
                     (derivations2 #+:null
                                   (loop
                                       for result in (get-field :ranks item2)
                                       when (= (get-field :rank result) 1)
                                       collect (get-field :derivation result))
                                   (loop
                                       for result in (get-field :results item2)
                                       collect (get-field :derivation result)))
                     ;;
                     ;; _fix_me_
                     ;; generalize for not fully disambiguated items
                     ;;
                     (agreement 
                      (when (and derivations1
                                 (null (rest derivations1))
                                 derivations2
                                 (null (rest derivations2)))
                        (if (and *redwoods-agreement-exact-p*
                                 (= (derivation-id (first derivations1))
                                    (derivation-id (first derivations2))))
                          (acons :exact t nil)
                          (summarize-derivation-agreement
                                     (first derivations1) 
                                     (first derivations2))))))
                (when agreement
                  (nconc item (acons :edge agreement nil))
                  (setf collectp t))))
            (when collectp (push item items))))

    (let* ((aggregates (aggregate items :format format)))

      (loop
          with tnitems = 0
          with tlength = 0
          with treadings = 0
          with turandom = 0
          with tuexact = 0
          with tunodes1 = 0
          with tunodes2 = 0
          with tuids = 0
          with tulabels = 0
          with tubrackets = 0
          with tucids = 0
          with tuclabels = 0
          with tucbrackets = 0
          for (id name . data) in aggregates
          do
          (loop
              with anitems = 0
              with alength = 0
              with areadings = 0
              with aurandom = 0
              with auexact = 0
              with aunodes1 = 0
              with aunodes2 = 0
              with auids = 0
              with aulabels = 0
              with aubrackets = 0
              with aucids = 0
              with auclabels = 0
              with aucbrackets = 0
              for item in data
              for length = (get-field :i-length item)
              for readings = (get-field :readings item)
              for edge = (get-field :edge item)
              when (and edge (get-field :exact edge)) do
                (incf anitems)
                (incf alength length) (incf areadings readings)
                (incf aurandom (divide 1 readings))
                (incf auexact)
              else when edge do
                (incf anitems)
                (incf alength length) (incf areadings readings)
                (incf aurandom (divide 1 readings))
                (incf aunodes1 (get-field :nodes1 edge))
                (incf aunodes2 (get-field :nodes2 edge))
                (incf auids (get-field :ids edge))
                (incf aulabels (get-field :labels edge))
                (incf aubrackets (get-field :brackets edge))
                (incf aucids (get-field :cids edge))
                (incf auclabels (get-field :clabels edge))
                (incf aucbrackets (get-field :cbrackets edge))
              finally
                (incf tnitems anitems)
                (incf tlength alength)
                (incf treadings areadings)
                (incf turandom aurandom)
                (incf tuexact auexact)
                (incf tunodes1 aunodes1) (incf tunodes2 aunodes2)
                (incf tuids auids) (incf tulabels aulabels)
                (incf tucids aucids) (incf tuclabels auclabels)
                (incf tubrackets aubrackets) (incf tucbrackets aucbrackets)
                (push (nconc (list id name)
                             (pairlis '(:items
                                        :i-length 
                                        :analyses
                                        :random
                                        :exact
                                        :nodes1 :nodes2
                                        :ids :labels :brackets
                                        :cids :clabels :cbrackets)
                                      (let ((nedges (- anitems auexact)))
                                        (list anitems 
                                              (divide alength anitems)
                                              (divide areadings anitems)
                                              aurandom
                                              auexact
                                              (divide aunodes1 nedges)
                                              (divide aunodes2 nedges)
                                              (divide auids nedges)
                                              (divide aulabels nedges)
                                              (divide aubrackets nedges)
                                              (divide aucids nedges)
                                              (divide auclabels nedges)
                                              (divide aucbrackets nedges)))))
                      results))
          finally
            (push (nconc (list :total "Total")
                         (pairlis '(:items
                                    :i-length
                                    :analyses
                                    :random
                                    :exact
                                    :nodes1 :nodes2
                                    :ids :labels :brackets
                                    :cids :clabels :cbrackets)
                                  (let ((nedges (- tnitems tuexact)))
                                    (list tnitems
                                          (divide tlength tnitems)
                                          (divide treadings tnitems)
                                          turandom
                                          tuexact
                                          (divide tunodes1 nedges)
                                          (divide tunodes2 nedges)
                                          (divide tuids nedges)
                                          (divide tulabels nedges)
                                          (divide tubrackets nedges)
                                          (divide tucids nedges)
                                          (divide tuclabels nedges)
                                          (divide tucbrackets nedges)))))
                  results)))
    (when (smember :edge test)
      (purge-profile-cache data1 :expiryp nil)
      (purge-profile-cache data2 :expiryp nil))
    results))

(defun summarize-derivation-agreement (derivation1 derivation2
                                       &key (test '(:id :label :bracket)))
  ;;
  ;; for now, make sure we do not count word nodes themselves, i.e. the ones
  ;; that have no edge id; this should deflate the agreement measure somewhat.
  ;;
  (let* ((nodes1 (delete nil (derivation-nodes derivation1) :key #'first))
         (nodes2 (delete nil (derivation-nodes derivation2) :key #'first))
         (cids (when (smember :id test)
                 (intersection nodes1 nodes2 :key #'first)))
         (clabels (when (smember :label test)
                    (intersection 
                     nodes1 nodes2
                     :test #'(lambda (foo bar)
                               (and (eql (fourth foo) (fourth bar))
                                    (eql (fifth foo) (fifth bar))
                                    (equal (second foo) (second bar)))))))
         (cbrackets (when (smember :label test)
                      (intersection 
                       nodes1 nodes2
                       :test #'(lambda (foo bar)
                                 (and (eql (fourth foo) (fourth bar))
                                      (eql (fifth foo) (fifth bar)))))))
          
         (ids (union nodes1 nodes2 :key #'first))
         (labels (union 
                  nodes1 nodes2
                  :test #'(lambda (foo bar)
                            (and (eql (fourth foo) (fourth bar))
                                 (eql (fifth foo) (fifth bar))
                                 (equal (second foo) (second bar))))))
         (brackets (union 
                    nodes1 nodes2
                    :test #'(lambda (foo bar)
                              (and (eql (fourth foo) (fourth bar))
                                   (eql (fifth foo) (fifth bar)))))))
    (pairlis '(:nodes1 :nodes2
               :ids :labels :brackets :cids :clabels :cbrackets) 
             (list 
               (length nodes1) (length nodes2)
              (length ids) (length labels) (length brackets)
              (length cids) (length clabels) (length cbrackets)))))

(defun train (sources file &key (condition *statistics-select-condition*)
                                (type :mem) model (estimatep t) (recursep t)
                                (verbose t) (stream t)
                                interrupt meter)
  (declare (ignore interrupt))

  (cond
   ((consp sources)
    (loop
        with *tsdb-connection-expiry* = 50
        with model = (or model 
                         (case type
                           ((:mem :tag)
                            (let ((model (make-mem)))
                              (initialize-mem model)
                              model))))
        with gc = (install-gc-strategy 
                   nil :tenure *tsdb-tenure-p* :burst t :verbose verbose)
        with condition = (if (eq type :tag)
                           condition
                           (if (and condition (not (equal condition "")))
                             (format nil "t-active >= 1 && (~a)" condition)
                             "t-active >= 1"))
        with meter = (when meter (madjust / meter (length sources)))
        with duration = (when meter (mduration meter))
        for i from 0
        for sources on sources
        for rmeter = (when duration (madjust + meter (* duration i)))
        do
          (train (first sources) nil :condition condition :type type
                 :model model :estimatep (null (rest sources))
                 :verbose verbose :stream stream :meter rmeter)
          (purge-profile-cache (first sources))
        finally
          (when verbose
            (format stream "train(): exporting ~a~%" model))
          (print-mem model :file file :format :export)
          (restore-gc-strategy gc)
          (setf %model% model)
          (return model)))
   (recursep
    (format 
     t 
     "[~a] train(): reading `~a'~%" 
     (current-time :long :short) sources)
    (loop
        with items = (select "i-id" :integer "item" nil sources :sort :i-id)
        with first = (get-field :i-id (first items))
        with last = (get-field :i-id (first (last items)))
        with delta = (if (numberp %redwoods-items-percentile%)
                       (or
                        (loop
                            with i = (ceiling
                                      (length items)
                                      %redwoods-items-percentile%)
                            for item in items
                            when (<= (decf i) 0)
                            return (max %redwoods-items-increment%
                                        (- (get-field :i-id item) first)))
                        %redwoods-items-increment%)
                       %redwoods-items-increment%)
        with n = (max (ceiling (- last first) delta) 1)
        with increment = (when meter (/ (mduration meter) n))
        for i from 1 to n
        for low = (+ first (* (- i 1) delta))
        for high = (+ first (* i delta))
        for message = (format
                       nil
                       "training `~a' [~a - ~a| ..."
                       sources low high)
        for foo = (format 
                   nil 
                   "i-id >= ~d && i-id < ~d~@[ && (~a)~]"
                   low high condition)
        initially (when meter (meter :value (get-field :start meter)))
        do
          (when meter 
            (meter-advance increment)
            (status :text message))
          (train sources nil :condition foo :type type
                 :model model :estimatep (and estimatep (= i n))
                 :recursep nil :verbose verbose :stream stream)
        finally (when meter 
                  (status :text (format nil "~a done" message) :duration 3)
                  (meter :value (get-field :end meter)))))
   (t
    ;;
    ;; _fix_me_
    ;; for the realization ranking experiments, always retrieve :tree (where we
    ;; record the surface string right now), but we should make that an extra
    ;; field in the `result' relation.                          (28-oct-04; oe)
    ;;
    (let ((items (analyze sources :gold sources :condition condition
                          :thorough '(:derivation :tree) :readerp nil)))
      (purge-profile-cache sources :expiryp nil)

      (when *redwoods-item-enhancer*
        (loop
            for item in items
            do (call-raw-hook *redwoods-item-enhancer* item)))
      
      (when (or items estimatep)
        (case type
          (:mem
           (estimate-mem 
            items :model model :estimatep estimatep :stream stream))
          (:tag
           (estimate-tagger 
            items :model model :estimatep estimatep :stream stream))))))))

(defun rank-profile (source 
                     &optional (target source)
                     &key (condition *statistics-select-condition*) data
                          (nfold 10) (type :mem) model
                          (stream *tsdb-io*) (cache :raw) (verbose t)
                          interrupt meter)  
  (format
   stream
   "~&[~a] rank-profile:() `~a' -->~%                           `~a'~%"
   (current-time :long :short) source target)

  (purge-test-run target :action :score)
  (loop
      with gc = (install-gc-strategy 
                 nil :tenure *tsdb-tenure-p* :burst t :verbose t)
      with cache = (create-cache target :verbose verbose :protocol cache)
      with condition = (if (and condition (not (equal condition "")))
                         (format nil "t-active >= 1 && (~a)" condition)
                         "t-active >= 1")
      with data = (or data
                      (let ((data
                             ;;
                             ;; _fix_me_
                             ;; for the realization ranking experiments, always
                             ;; retrieve :tree (where we record the surface 
                             ;; string right now), but we should make that an 
                             ;; extra field in the `result' relation. 
                             ;;                               (28-oct-04 ; oe)
                             (analyze 
                              source 
                              :thorough (if (eq type :ngram)
                                          '(:tree)
                                          '(:derivation :tree))
                              :condition condition :gold source
                              :readerp nil :message meter)))
                        (when *redwoods-item-enhancer*
                          (loop
                              for item in data
                              do (call-raw-hook 
                                  *redwoods-item-enhancer* item)))
                        data))
      with sets = (when *redwoods-use-item-sets-p*
                    (let ((sets (select '("i-id" "s-id") '(:integer :integer)
                                        "item-set" nil source :sort :s-id)))
                      (loop
                          with current
                          with group
                          for set in sets
                          for i-id = (get-field :i-id set)
                          for s-id = (get-field :s-id set)
                          when (or (null i-id) (null s-id)) return nil
                          unless (or (null current) (= current s-id))
                          collect (sort group #'<) into groups
                          and do (setf group nil)
                          do (push i-id group) (setf current s-id)
                          finally 
                            (return (nconc groups (list (sort group #'<)))))))
      with boundaries = (loop
                            for set in sets
                            collect (first (last set))
                            into boundaries
                            finally 
                              (return (sort boundaries #'<)))
      with nboundaries = (length boundaries)
      with increment = (when meter (/ (mduration meter) nfold))
      ;;
      ;; for some externally acquired models, never do more than one iteration
      ;;
      with nfold = (if (or model (smember type '(:ngram :chance)))
                     1
                     (min (length data) nfold))
      with high
      initially 
        #+:debug (setf %data% data)
        (when meter (meter :value (get-field :start meter)))
      for i from 1 
      to (if (>= nfold 1) nfold (if boundaries nboundaries 1))
      when (interrupt-p interrupt) do
        (format 
         stream
         "[~a] rank-profile(): external interrupt signal~%"
         (current-time :long :short))
        (flush-cache cache :verbose verbose)
        (restore-gc-strategy gc)
        (return)
      do
        (multiple-value-bind (test train) 
            (ith-nth data i (if (zerop nfold) 
                              (if boundaries nboundaries 10)
                              nfold))
          ;;
          ;; when requested, fit data division to item set boundaries
          ;;
          (when (and (not (= nfold 1)) test boundaries)
            ;;
            ;; we know .result. and .complement. are sorted at this point
            ;;
            (let* ((last (get-field :i-id (first (last test))))
                   (low (or high (- (get-field :i-id (first test)) 1))))
              ;;
              ;; fit the upper limit to the nearest boundary, below or above;
              ;; to make sure no item can be in .test. twice, also keep track
              ;; of .high. across iterations.
              ;;
              (setf high
                (if (<= last (first boundaries))
                  (first boundaries)
                  (if (= nfold 0)
                    (loop
                        for boundary in boundaries
                        when (> boundary low) return boundary)
                    (loop
                        for boundaries on boundaries
                        for this = (first boundaries)
                        for next = (first (rest boundaries))
                        when (and (<= this last) (< last next))
                        return next))))
              ;;
              ;; for the last iteration, make sure to extend the .test. subset
              ;; to score all of the items, in the end.
              ;;
              (when (= i nfold) (setf high last))
              (setf test nil) (setf train nil)
              
              (loop
                  for item in data
                  for id = (get-field :i-id item)
                  when (and (> id low) (<= id high)) collect item into foo
                  else collect item into bar
                  finally (setf test foo) (setf train bar))))
            
          (when (and (null train) (not (smember type '(:ngram :chance))))
            (setf train test))
          (when (and test (or train (= nfold 1)))
            (format
             stream
             "~&[~a] rank-profile:() iteration # ~d (~d against ~d)~%"
             (current-time :long :short) i (length test) (length train))
            (when meter 
              (status :text
                      (format
                       nil
                       "~a-fold cross-validation: ~
                        iteration # ~d (~d against ~d)"
                       (if (and (zerop nfold) boundaries)
                         nboundaries
                         nfold)
                       i (length test) (length train))))
            (loop
                with items = (train-and-rank 
                              train test :type type :model model)
                for item in items
                for parse-id = (get-field :parse-id item)
                for ranks = (get-field :ranks item)
                do
                  (loop
                      for foo in ranks
                      for result-id = (get-field :result-id foo)
                      for score = (let ((score (get-field :score foo)))
                                    (if score (format nil "~,16f" score) ""))
                      for rank = (get-field :rank foo)
                      do
                        #+:debug
                        (format
                         stream
                         "  parse: ~a; result: ~d; rank: ~d; score: ~a~%"
                         parse-id result-id rank score)
                        (write-score target (pairlis '(:parse-id :result-id
                                                       :rank :score)
                                                     (list parse-id result-id
                                                           rank score))
                                     :cache cache)))
            (when meter (meter-advance increment))))
      finally 
        (when meter
          (meter :value (get-field :end meter))
          (status :text ""))
        (flush-cache cache :verbose verbose)
        (restore-gc-strategy gc)
        (purge-profile-cache source :expiryp nil)
        (purge-profile-cache target)))

(defun train-and-rank (train test 
                       &key (type :mem) model (stream *tsdb-io*))

  #+:debug
  (setf %train train %test test)
  (loop
      with model = (or model
                       (case type
                         (:pcfg (estimate-cfg train))
                         (:mem (estimate-mem train))
                         (:ngram "string n-grams")
                         (:tag (estimate-tagger train))
                         (:chance "chance")))
      for item in test
      for iid = (get-field :i-id item)
      for readings = (get-field :readings item)
      for results = (get-field :results item)
      for ranks = nil
      initially 
        (format
         stream
         "~&[~a] train-and-rank(): using ~a;~%"
         (current-time :long :short)  model)
        (setf %model% model)
      when (and (integerp readings) (> readings 1)) do
        (format 
         stream
         "~&[~a] train-and-rank(): item # ~a (~a reading~p);~%"
         (current-time :long :short) iid readings readings)
        (if (eq type :ngram)
          ;;
          ;; mostly for better efficiency (given our current, say, limited way
          ;; of re-starting evallm(1) for each call to lm-score-strings(), use
          ;; a slightly differenct control strategy for n-gram ranking.
          ;;
          (loop
              for result in results
              for string = (get-field :tree result)
              collect string into strings
              finally
                (loop
                    for score in (mt::lm-score-strings strings)
                    for result in results
                    for id = (get-field :result-id result)
                    do
                      (push (nconc (pairlis '(:result-id :score)
                                            (list id (- (rest score))))
                                   result)
                            ranks)))
          (loop
              with lkb::*chart-packing-p* = nil
              with *reconstruct-cache* = (make-hash-table :test #'eql)
              for result in results
              for id = (get-field :result-id result)
              for derivation = (get-field :derivation result)
              for edge = (unless (eq type :chance)
                           (reconstruct derivation nil))
              for lm = (get-field :lm result)
              for score = (and (case type (eq type :chance)
                                 (:pcfg (pcfg-score-edge edge model))
                                 (:mem (mem-score-edge edge model :lm lm))
                                 (:tag (tag-score-edge edge model))
                                 (:chance 0.0)))
              when (and (null edge) (not (eq type :chance))) do 
                (format 
                 stream
                 "~&[~a] train-and-rank(): ignoring this item (no edge);~%"
                 (current-time :long :short))
                (setf ranks nil) (return)
              else do 
                (push (nconc (pairlis '(:result-id :score)
                                      (list id score))
                             result)
                      ranks)))
      and collect 
        (let* ((ranks (sort ranks #'> 
                            :key #'(lambda (foo) (get-field :score foo))))
               (ranks (loop
                          with last = (get-field :score (first ranks))
                          with i = 1
                          for rank in ranks
                          for j from 1
                          for score = (get-field :score rank)
                          unless (= score last) do
                            (setf i j) (setf last score)
                          collect (acons :rank i rank))))
          (nconc (acons :ranks ranks nil) item))))

(defun kappa (actual expected)
  (/ (- actual expected) (- 100 expected)))

(defstruct experiment
  (source *tsdb-gold*)
  (skeleton *tsdb-default-skeleton*)
  (target *tsdb-data*)
  (type :mem) 
  (nfold 10)
  environment
  scores)

(defun run-experiment (experiment)
  (when (stringp (experiment-target experiment))
    (tsdb
     :create (experiment-target experiment)
     :skeleton (experiment-skeleton experiment))
    (let (save)
      (loop
          with environment = (copy-list (experiment-environment experiment))
          for symbol = (pop environment)
          for value = (pop environment)
          while symbol do
                (when (boundp symbol)
              (push (cons symbol (symbol-value symbol)) save))
                (set symbol value))
      (rank-profile
       (or (experiment-source experiment) (experiment-target experiment))
       (experiment-target experiment)
       :nfold (experiment-nfold experiment)
       :type (experiment-type experiment))
      (setf (experiment-scores experiment)
        (summarize-scores
         (experiment-target experiment)
         (or (experiment-source experiment)
             (experiment-target experiment))
         :n 1 :test :id :spartanp t :loosep t))
      (loop
          for (symbol . value) in save
          do (set symbol value)))
    (purge-profile-cache (experiment-source experiment))
    (purge-profile-cache (experiment-target experiment))
    experiment))

#+:null
(loop
    with experiments
    for variance in '(1e0 1e1 1e2 1e3 1e4 1e5)
    do
      (loop
          for tolerance in '(1e-8 1e-7 1e-6 1e-5 1e-4 1e-3 1e-2)
          do
            (loop
                for ngrams in '(0 1 2 3)
                for target = (format
                              nil
                              "mem.10.~e.~e.~a"
                              tolerance variance ngrams)
                for experiment
                = (make-experiment
                   :source "gold/lkb.g.s" :skeleton "hike"
                   :target target
                   :nfold 10
                   :environment `(*maxent-active-edges-p* nil
                                  *maxent-relative-tolerance* ,tolerance
                                  *maxent-variance* ,variance
                                  *maxent-ngram-size* ,ngrams))
                do
                  (run-experiment experiment)
                  (push experiment experiments)
                  (let ((clone (copy-experiment experiment)))
                    (setf
                        (experiment-target clone)
                      (format nil "~a.ae" (experiment-target experiment)))
                    (setf
                        (experiment-environment clone)
                      (copy-list (experiment-environment experiment)))
                    (setf (second (experiment-environment clone)) t)
                    (run-experiment clone)
                    (push clone experiments))))
    finally (return experiments))
#+:null
(loop
    with experiments
    for variance in '(1e2)
    do
      (loop
          for tolerance in '(1e-7 1e-8 1e-6)
          do
            (loop
                for ngrams in '(0 1 2 3)
                for target = (format
                              nil
                              "mem.100.~e.~e.~a"
                              tolerance variance ngrams)
                for experiment
                = (make-experiment
                   :source "gold/lkb.g.s" :skeleton "hike"
                   :target target
                   :nfold 100
                   :environment `(*maxent-active-edges-p* nil
                                  *maxent-relative-tolerance* ,tolerance
                                  *maxent-variance* ,variance
                                  *maxent-ngram-size* ,ngrams))
                do
                  (run-experiment experiment)
                  (push experiment experiments)
                  (let ((clone (copy-experiment experiment)))
                    (setf
                        (experiment-target clone)
                      (format nil "~a.ae" (experiment-target experiment)))
                    (setf
                        (experiment-environment clone)
                      (copy-list (experiment-environment experiment)))
                    (setf (second (experiment-environment clone)) t)
                    (run-experiment clone)
                    (push clone experiments))))
    finally (return experiments))

#+:null
(loop
    with experiments
    for variance in '(1e1 1e2 1e3)
    do
      (loop
          for tolerance in '(1e-8 1e-7 1e-6)
          do
            (loop
                for ngrams in '(0)
                for target = (format
                              nil
                              "mem.10.~e.~e.~a"
                              tolerance variance ngrams)
                for experiment
                = (make-experiment
                   :source "gold/hike.oct-04.4-apr" :skeleton "hike"
                   :target target
                   :nfold 10
                   :environment 
                   `(*maxent-active-edges-p* nil
                     *maxent-relative-tolerance* ,tolerance
                     *maxent-variance* ,variance
                     *maxent-ngram-size* ,ngrams
                     *maxent-lm-p* 100
                     *redwoods-item-enhancer* #'mem-item-enhancer))
                do
                  (run-experiment experiment)
                  (push experiment experiments)
                  (let ((clone (copy-experiment experiment)))
                    (setf
                        (experiment-target clone)
                      (format nil "~a.ae" (experiment-target experiment)))
                    (setf (experiment-environment clone)
                      (copy-list (experiment-environment experiment)))
                    (setf (second (experiment-environment clone)) t)
                    (run-experiment clone)
                    (push clone experiments))))
    finally (return experiments))

#+:null
(loop
    with experiments
    with active-edges-p = t
    with ngram-backkoff-p = t
    for variance in '(1e1 1e2 1e3 1e4 1e5)
    do
      (loop
          for tolerance in '(1e-7 1e-6 1e-5 1e-4)
          do
            (loop
                for ngrams in '(0)
                do
                  (loop
                      for grandparenting in '(0 1)
                      do
                        (loop
                            for lm in '(0)
                            for target = (format
                                          nil
                                          "mem.10.~a.~a.~:[0~;1~].~e.~e.~ 
                                          ~:[0~;1~].~a"
                                          lm ngrams ngram-backkoff-p
                                          tolerance variance
                                          ngram-backkoff-p grandparenting)
                            for experiment
                            = (make-experiment
                               :source "gold/hike.oct-04.g"
                               :skeleton "hike"
                               :target target
                               :nfold 10
                               :environment 
                               `(*maxent-active-edges-p* ,active-edges-p
                                 *maxent-grandparenting* ,grandparenting
                                 *maxent-relative-tolerance* ,tolerance
                                 *maxent-variance* ,variance
                                 *maxent-ngram-size* ,ngrams
                                 *maxent-ngram-back-off-p* ,ngram-backkoff-p
                                 *maxent-lm-p* ,lm))
                            do
                              (run-experiment experiment)
                              (push experiment experiments)))))
    finally (return experiments))

#+:null
(with-open-file (stream "/tmp/scores.r"
                 :direction :output :if-exists :supersede)
  (loop
      with gold = "gold/hike.oct-04.g"
      with *redwoods-score-similarity-p* = #'bleu-similarity
      for db in (find-tsdb-directories *tsdb-home* :pattern "^mem")
      for name = (let ((name (get-field :database db)))
                   (unless (string= name gold) name))
      for fields = (and name (cl-ppcre:split "\\." name))
      for folds = (second fields)
      for lm = (third fields)
      for ngrams = (fourth fields)
      for threshold = (sixth fields)
      for variance = (eighth fields)
      for aep = (ninth fields)
      when name do
        (let* ((scores (summarize-scores
                        name gold
                        :n 1 :test :id :spartanp t :loosep t))
               (total (rest (rest (find :total scores :key #'first))))
               (nitems (get-field :items total))
               (nscores (get-field :scores total))
               (exact (get-field :exact total))
               (similarity (get-field :similarity total)))
          (purge-profile-cache gold)
          (purge-profile-cache name)
          (format
           stream
           "~,2f ~,4f `~a' ~a ~a ~a ~:[0~;1~] ~a ~a~%"
           (* 100 (divide exact nscores)) (divide similarity nscores)
           name
           folds lm ngrams aep threshold variance)
          (force-output stream))))
