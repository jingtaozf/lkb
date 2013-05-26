(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2005 -- 2006 Erik Velldal (erikve@ifi.uio.no)
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

(defparameter *redwoods-shift* nil)

(defparameter *redwoods-record-void-discriminants-p* nil)

(defparameter *redwoods-trace* nil)

(defparameter *redwoods-reconstruct-mode* :word)

(defparameter *redwoods-composite-export-p* nil)

(defparameter *redwoods-index-export-p* nil)

(defparameter *redwoods-train-percentage* 100)

(defparameter *redwoods-task* :rank)

(defparameter *models* nil)

(defparameter %model% nil)

(defparameter %redwoods-items-increment% #-:64bit 500 #+:64bit 2000)

(defparameter %redwoods-items-percentile% 20)

(defstruct fc
  file db (strikes 0) cache)

(defun browse-trees (&optional (data *tsdb-data*)
                     &key (condition *statistics-select-condition*)
                          gold scores
                          (shift *redwoods-shift*) strip inspect blindp
                          (bestp *redwoods-thinning-normalize-p*)
                          (exactp *redwoods-update-exact-p*)
                          (cache *tsdb-cache-database-writes-p*)
                          (verbose t) interactive
                          (stream *tsdb-io*)
                          (runp t) interrupt meter)

  (declare (optimize (speed 3) (safety 0) (space 0)))

  (initialize-tsdb)
  (when strip
  
    (when (< (profile-granularity data) 200509)
      (format
       stream
       "~%browse-trees(): out-of-date profile `~a'.~%"
       data)
      (return-from browse-trees))
  
    (unless (do-import-database (find-tsdb-directory data) strip 
                                :meter (when meter (make-meter 0 1))
                                :uncompress t
                                :except (append
                                         '("tree" "decision" "preference")
                                         (and bestp '("result"))))
      (return-from browse-trees nil)))

  (let* (#+:null
         (*tsdb-connection-expiry* 200)
         (condition (if (and condition (not (equal condition "")))
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (gold (unless (and (stringp gold) (string= gold "")) gold))
         (items
          (if (stringp data) 
            (analyze data :condition condition :meter meter :message meter)
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
         (frame (unless strip
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
                              :gold gold :scores scores :shift shift
                              :strip strip :bestp bestp :blindp blindp
                              :inspect inspect :exactp exactp
                              :cache cache :title title :display display
                              :verbose verbose :stream stream)))
                          (browse-tree 
                           data i-id frame 
                           :gold gold :scores scores :shift shift
                           :strip strip :bestp bestp :blindp blindp
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

(defun browse-tree (data i-id frame &key gold scores shift strip bestp 
                                         inspect exactp subset blindp
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
           (lkb::*edge-registry* nil)
           (*reconstruct-cache* (make-hash-table :test #'eql))
           (lkb::*tree-update-match-hook* #'update-match-p)
           (lkb::*tree-automatic-update-p* 
            (when gold lkb::*tree-automatic-update-p*))
           (condition (format nil "i-id = ~a" i-id))
           (thorough (cons :derivation (unless blindp '(:mrs))))
           (items (let ((*package* (find-package lkb::*lkb-package*)))
                    (analyze 
                     data :thorough thorough 
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
           (edges (unless strip
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
                        for mrs = (let ((mrs (get-field :mrs result)))
                                    (and mrs (not (equal mrs "")) mrs))
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
           (discriminants (unless strip
                            #+:allegro
                            (format
                             excl:*initial-terminal-io*
                             "~&[~a] browse-tree(): retrieved ~a decision~p.~%"
                             (current-time :long :short)
                             (length decisions) (length decisions))
                            (reconstruct-discriminants decisions)))
           (gi-id (if (functionp shift) (funcall shift i-id) i-id))
           ;;
           ;; _fix_me_
           ;; in the following two s-expressions, the when() used to also test
           ;; (null strip).  in hacking a thinning update mode that will find
           ;; the top-ranked result that aligns with the tokenization of the
           ;; gold-standard parse(s), we want to combine .gold. and .strip.  i
           ;; cannot quite how this combination would have been invoked before,
           ;; so hope that relaxing these when()s will not have side effects.
           ;;                                                   (31-jan-12; oe)
           (greadings (when gold
                        (let ((items (select 
                                      '("readings") '(:integer) "parse" 
                                      (format nil "i-id == ~a" gi-id)
                                      gold)))
                          (when (= (length items) 1)
                            (get-field :readings (first items))))))
           (gtrees (when gold
                     (select '("parse-id" "t-version" 
                               "t-active" "t-author" "t-end")
                             '(:integer :integer 
                               :integer :string :date)
                             "tree" 
                             (format nil "i-id == ~a" gi-id) 
                             gold :sort :parse-id)))
           (gversion (loop
                         for tree in gtrees
                         maximize (get-field :t-version tree)))
           (gtrees (loop
                       for tree in gtrees
                       when (eq gversion (get-field :t-version tree))
                       collect tree))
           (gactive (when (and (consp gtrees) (null (rest gtrees)))
                      (let ((gactive (get-field :t-active (first gtrees))))
                        (unless (minus-one-p gactive) gactive))))
           ;;
           ;; _fix_me_
           ;; i added the final element in the or() to enable the combination
           ;; of .gold. and .strip. discussed above; however, not quite sure
           ;; just now why .readings. should always be 1 ordinarily?
           ;;                                                   (31-jan-12; oe)
           (gitem (when (and gactive
                             (or exactp (= readings 1) (and strip bestp)))
                    (first
                     (analyze 
                      gold :thorough '(:derivation) :condition condition))))
           (gpreferences (when (and gitem gtrees (null (rest gtrees)))
                           (select '("parse-id" "t-version" "result-id")
                                   '(:integer :integer :integer)
                                   "preference" 
                                   (format 
                                    nil 
                                    "i-id == ~a && t-version == ~a" 
                                    gi-id gversion) 
                                   gold)))
           (gderivation (when (and gpreferences (null (rest gpreferences)))
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
                                    "i-id == ~a && t-version == ~a" 
                                    gi-id gversion) 
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
            do (write-preference strip preference :cache cache)
            finally
              (when (or (and trees bestp)
                        (smember bestp '(:top :random :tokenization)))
                ;;
                ;; _fix_me_
                ;; in :random mode, we need to also adjust the preference
                ;; relation accordingly.                        (26-apr-04; oe)
                ;; with the additions of other modes that bypass the preferred
                ;; tree annotation (if any), the same possibly also holds for
                ;; :tokenization and :top modes now.             (4-dec-12-oe)
                ;;
                (let* ((ids 
                        (case bestp
                          (:random
                           (random-sample 1 readings (length preferences)))
                          (:tokenization
                           (loop
                               for result in results
                               for derivation = (get-field :derivation result)
                               when (derivation-equal
                                     derivation gderivation :tokenization)
                               return (list (get-field :result-id result))))
                          (:top
                           (list
                            (loop
                                for result in results
                                minimize (get-field :result-id result))))
                          (t
                           (or
                            (loop
                                for preference in preferences
                                collect (get-field :result-id preference))
                            (list
                             (loop
                                 for result in results
                                 minimize (get-field :result-id result)))))))
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
        (let ((gderivation (if (stringp gderivation)
                             (ignore-errors
                              (read-from-string gderivation nil nil))
                             gderivation)))
          (setf (lkb::compare-frame-exact frame) nil)
          (case exactp
            (:inclusion
             ;;
             ;; against a lexical chart (the lattice resulting from lexical-
             ;; only parsing), label those maximal sub-trees that participate
             ;; in the 'gold' derivation.
             ;;
             (let* ((n (loop for edge in edges maximize (lkb::edge-to edge)))
                    (chart
                     (make-array (list (+ n 1) (+ n 1)) :initial-element nil)))
               (labels ((process (derivation)
                          (when (derivation-daughters derivation)
                            (let* ((from (derivation-start derivation))
                                   (to (derivation-end derivation))
                                   (edge
                                    (find
                                     derivation (aref chart from to)
                                     :key #'lkb::edge-bar
                                     :test #'derivation-equal)))
                              (if edge
                                (push edge (lkb::compare-frame-exact frame))
                                (loop
                                    for daughter
                                    in (derivation-daughters derivation)
                                    do (process daughter)))))))
                 (loop
                     for edge in edges
                     for from = (lkb::edge-from edge)
                     for to = (lkb::edge-to edge)
                     do 
                       (push edge (aref chart from to)))
                 (process gderivation))))
            (t
             (loop
                 for edge in edges
                 for derivation = (lkb::edge-bar edge)
                 when (derivation-equal gderivation derivation) do
                   (push edge (lkb::compare-frame-exact frame)))))))

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
                              (incf version)
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
      (:twig 7)
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

(defun analyze-trees (&optional (profile *tsdb-data*)
                      &key (condition *statistics-select-condition*)
                           file append (format :latex)
                           meter)
  (let* ((stream (create-output-stream file append))
         (items (if (stringp profile)
                  (analyze-aggregates profile :condition condition :trees t
                                      :meter meter :format format) 
                  profile))
         (averages (summarize-competence-parameters items))
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
                       (get-field :sanalyses data))))
            (:csv
             (format
              stream
              "~a,~a,~a,~
               ~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f~%"
              profile name (get-field :items data)
              (get-field :results data) (get-field :i-length data)
              (get-field :analyses data)
              (get-field :rresults data) (get-field :rlength data)
              (get-field :ranalyses data)
              (get-field :uresults data) (get-field :ulength data)
              (get-field :uanalyses data)
              (get-field :aresults data) (get-field :alength data)
              (get-field :aanalyses data)
              (get-field :sresults data) (get-field :slength data)
              (get-field :sanalyses data)))))

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
          ncolumns caption))
        (:csv
         (format
          stream
          "~a,~a,~a,~
           ~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f,~,2f~%"
          profile name (get-field :items total)
          (get-field :results total) (get-field :i-length total)
          (get-field :analyses total)
          (get-field :rresults total) (get-field :rlength total)
          (get-field :ranalyses total)
          (get-field :uresults total) (get-field :ulength total)
          (get-field :uanalyses total)
          (get-field :aresults total) (get-field :alength total)
          (get-field :aanalyses total)
          (get-field :sresults total) (get-field :slength total)
          (get-field :sanalyses total))))

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
       profile
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
                               path prefix
                               (index *redwoods-index-export-p*)
                               (compositep *redwoods-composite-export-p*)
                               interrupt meter 
                               (compressor "gzip -c -9") (suffix "gz")
                               blindp (offset 0)
                               (stream *tsdb-io*))
  
  (loop
      with target = (format 
                     nil 
                     "~a~@[/~a~]"
                     (or path (tmp :redwoods))
                     (unless compositep (directory2file data)))
      with lkb::*chart-packing-p* = nil
      with *reconstruct-cache* = (make-hash-table :test #'eql)
      with *derivations-print-lexical-type-p* = t
      with items = (analyze
                    data :thorough (cons :derivation (unless blindp '(:mrs)))
                    :condition condition :commentp t :inputp t)
      with increment = (when (and meter items)
                         (/ (- (get-field :end meter) (get-field :start meter))
                            (length items) 1))
      with gc-strategy = (install-gc-strategy 
                          nil :tenure nil :burst t :verbose t)
      with firstp = t with out with pid with foo
      initially
        #+:allegro (ignore-errors (mkdir target))
        (when meter (meter :value (get-field :start meter)))
        (when compositep
          (let ((file (format 
                       nil "~a/~@[~a.~]~a~@[.~a~]" 
                       target prefix (directory2file data) suffix)))
            (multiple-value-setq (out foo pid)
              (run-process
               compressor :wait nil :input :stream
               :output file :if-output-exists :supersede
               :error-output nil))
            (setf foo foo)))
        (when index
          (let ((base (format
                       nil "~a/~a"
                       target (if compositep (directory2file data) "Index"))))
            (ignore-errors (mkdir base))
            (setf index (acons :base base nil))))
      for item in items
      for i-wf = (get-field :i-wf item)
      for input = (or (get-field :o-input item) (get-field :i-input item))
      for i-comment = (let ((foo (get-field :i-comment item)))
                        (unless (or (string-equal foo "")
                                    (string-equal foo "nil"))
                          foo))
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
      for active = (if version
                     (let ((foo (select '("result-id") '(:integer) 
                                        "preference" 
                                        (format 
                                         nil 
                                         "parse-id == ~a && t-version == ~d" 
                                         parse-id version) 
                                        data)))
                       (loop 
                           for bar in foo 
                           collect (get-field :result-id bar)))
                     (list (get-field :result-id (first results))))
      when results do
        (format 
         stream 
         "[~a] export-trees(): [~a] ~a active tree~:[~;s~] (of ~d).~%" 
         (current-time :long :short)
         (+ parse-id offset) (length active)
         (or (null version) (> (length active) 1))
         (length results))
        (clrhash *reconstruct-cache*)
        #+:lkb (lkb::release-temporary-storage)

        (if compositep
          (unless firstp (format out "~c~%" (code-char 4)))
          (let ((file (format 
                       nil "~a/~@[~a.~]~d~@[.~a~]" 
                       target prefix (+ parse-id offset) suffix)))
            (multiple-value-setq (out foo pid)
              (run-process
               compressor :wait nil :input :stream
               :output file :if-output-exists :supersede
               :error-output nil))
            (format
             out
             ";;;~%;;; Redwoods export of `~a';~%;;; (~a@~a; ~a).~%;;;~%~%"
             data (current-user)
             (current-host) (current-time :long :pretty))))

        (format 
         out
         "[~d] (~a of ~d) {~d} `~a'~@[ [~a]~]~%"
         (+ parse-id offset)
         (length active) (length results) i-wf
         input i-comment)

        (export-tree
         item active :offset offset :out out :stream stream :index index)
        (unless *redwoods-thinning-export-p*
          (export-tree
           item active :complementp t :offset offset
           :out out :stream stream :index index))

        (force-output out)
        (unless compositep
          (close out)
          #+:allegro
          (sys:os-wait nil pid)
          (setf out nil pid nil))

        (setf firstp nil)
        ;;
        ;; _fix_me_
        ;; while TITAN limits our jobs to a maximum of 1024 open files, make
        ;; sure to close all streams for each new item.        (28-nov-10; oe)
        ;;
        (when index
          (loop
              for entry in index
              when (and (streamp (rest entry)) (open-stream-p (rest entry)))
              do
                (close (rest entry))
                (setf (rest entry) nil)))
        (when increment (meter-advance increment))
      when (interrupt-p interrupt) do
        (when out (close out))
        #+:allegro
        (when pid (sys:os-wait nil pid))
        (format 
         stream
         "[~a] export-trees(): external interrupt signal~%"
         (current-time :long :short))
        (force-output stream)
        (return)
      finally
        (when compositep
          (close out)
          #+:allegro
          (sys:os-wait nil pid))
        (when index
          (loop
              for entry in index
              for stream = (rest entry)
              when (and (streamp stream) (open-stream-p stream))
              do (close stream)))
        (when meter (meter :value (get-field :end meter)))
        (when gc-strategy (restore-gc-strategy gc-strategy))))

(defun export-tree (item active 
                    &key complementp (offset 0) (out t)
                         index (stream *tsdb-io*))

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
      with i-comment = (let ((foo (get-field :i-comment item)))
                         (unless (or (string-equal foo "")
                                     (string-equal foo "nil"))
                           foo))
      with parse-id = (get-field :parse-id item)
      with results = (get-field :results item)
      for i from 1
      for result in results
      for result-id = (get-field :result-id result)
      for activep = (if complementp
                      (not (member result-id active :test #'eql))
                      (member result-id active :test #'eql))
      for derivation = (and activep (get-field :derivation result))
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
      for mrs = (or (let ((mrs (get-field :mrs result)))
                      (mrs::read-mrs-from-string mrs))
                    (and edge (mrs::extract-mrs edge)))
      for ident = (format nil "~a @ ~a~@[ @ ~a~]" i-id result-id i-comment)
      when (zerop (mod i 100)) do (clrhash *reconstruct-cache*)
      when (and activep (or dag mrs)) do
        (format 
         out 
         "~c~%[~d:~d] ~:[(active)~;(inactive)~]~%~%" 
         #\page (+ parse-id offset) result-id complementp)
        ;;
        ;; first, the actual parser inputs (initial and internal tokens)
        ;;
        (when (or (eq *redwoods-export-values* :all)
                  (smember :input *redwoods-export-values*))
          (let ((input (get-field :p-input item))
                (tokens (get-field :p-tokens item)))
            (format out "<~%")
            (when (consp input)
              (loop
                  for token in input
                  do
                    (yy-print-token token :prefix "  " :stream out)
                    (terpri out)))
            (format out ">~%~%<~%")
            (when (consp tokens)
              (loop
                  for token in tokens
                  do
                    (yy-print-token token :prefix "  " :stream out)
                    (terpri out)))
            (format out ">~%~%")))
        (setf lkb::*cached-category-abbs* nil)
        (when index
          (labels ((register (entity start end &optional cache)
                     (let ((stream (and cache (get-field entity index)))
                           (stringp t))
                       (unless stream
                         (let* ((base (get-field :base index))
                                (file (format nil "~a/~(~a~)" base entity)))
                           (setf stream
                             (open
                              file :direction :output
                              :if-exists :append :if-does-not-exist :create))
                           (when cache (set-field entity stream index))))
                       (format
                        stream "~a ~a ~a ~a ~a~@[ |~a|~] ~a~%"
                        i-id parse-id result-id start end 
                        (and stringp i-input)
                        (if complementp 0 1))
                       (unless cache (close stream)))))
            (let* ((sponsor (derivation-sponsor derivation))
                   (nodes (derivation-nodes derivation))
                   (yield (derivation-yield derivation)))
              (when sponsor
                (register
                 sponsor
                 (derivation-start derivation) (derivation-end derivation)
                 t))
              (loop
                  for node in nodes
                  for entity = (second node)
                  for start = (fourth node)
                  for end = (fifth node)
                  for preterminalp = (smember entity yield)
                  when preterminalp
                  do (register (type-of-lexical-entry entity) start end t)
                  else do (register entity start end (not preterminalp))))))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :derivation *redwoods-export-values*))
          (let ((*package* (find-package :tsdb)))
            (pprint-derivation derivation :stream out)
            (format out "~%~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :tree *redwoods-export-values*))
          (if tree
            (format out "~a~%~%" tree)
            (format out "()~%~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :avm *redwoods-export-values*))
          (lkb::display-dag1 dag 'lkb::compact out)
          (format out "~%~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :mrs *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::simple out))
        (when (and (not (eq *redwoods-export-values* :all))
                   (smember :indexed *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::indexed out))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :prolog *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::prolog out)
          (format out "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :mrx *redwoods-export-values*))
          (mrs::output-mrs1 mrs 'mrs::mrs-xml out)
          (format out "~%"))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :rmrs *redwoods-export-values*))
	  (ignore-errors
	   (mrs::output-rmrs1 (mrs::mrs-to-rmrs mrs) 'mrs::compact out)
	   (format out "~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :rmrx *redwoods-export-values*))
	  (ignore-errors
	   (mrs::output-rmrs1
	    (mrs::mrs-to-rmrs mrs)
	    'mrs::xml out nil nil i-input ident)
	   (format out "~%")))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :eds *redwoods-export-values*)
                  (smember :dependencies *redwoods-export-values*))
          (ignore-errors (mrs::ed-output-psoa mrs :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :triples *redwoods-export-values*))
          (ignore-errors
           (mrs::ed-output-psoa
            mrs :format :triples :cargp nil :markp nil :lnkp nil
            :collocationp t :abstractp t :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :mtriples *redwoods-export-values*))
          (ignore-errors
           (mrs::ed-output-psoa
            mrs :format :triples :cargp nil :markp t :lnkp nil
            :collocationp t :abstractp t :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :ltriples *redwoods-export-values*))
          (ignore-errors
           (mrs::ed-output-psoa
            mrs :format :triples  :cargp t :markp nil :lnkp t
            :collocationp nil :abstractp nil :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :striples *redwoods-export-values*))
          (ignore-errors
           (mrs::ed-output-psoa
            mrs :format :triples  :cargp t :markp nil :lnkp t :propertyp nil
            :collocationp nil :abstractp nil :sortp t :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dtriples *redwoods-export-values*))
          (ignore-errors
           (mrs::ed-output-psoa
            mrs :format :triples  :cargp t :markp nil :lnkp t :propertyp nil
            :collocationp nil :abstractp nil :sortp t :dmrsp t :stream out)))
        (when (or (eq *redwoods-export-values* :all)
                  (smember :dmrx *redwoods-export-values*))
          (ignore-errors
           (mrs::output-dmrs1
            (mrs::rmrs-to-dmrs (mrs::mrs-to-rmrs mrs))
            'mrs::dxml out)
           (format out "~%")))
        ;;
        ;; _fix_me_
        ;; it appears this function is just called for its side effect, as it
        ;; is not given access to the .out. stream?             (2-jun-10; oe)
        ;;
        #+:cambridge
        (when (smember :qa *redwoods-export-values*)
          (mrs::output-rmrs-from-itsdb
           (+ parse-id offset) 
           (or (get-field :o-input item) (get-field :i-input item))
           mrs))))

(defun semantic-equivalence (data
                             &key condition
                                  (file (format
                                         nil "~a/equivalences"
                                         (tmp :redwoods))))
  
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
                            (trace *redwoods-trace*)
                            meter)

  (let* (;;
         ;; _fix_me_
         ;; we changed the format for string similarity as returned from
         ;; summarize-scores(), now returning an a-list with multiple scores
         ;; (now averaged already).  until we adapt the output generation code,
         ;; effectively disable all of it.               (25-jan-06; oe & erik)
         ;;
         (*redwoods-score-similarities* nil)
         (*redwoods-trace*
          (typecase trace
            (pathname (create-output-stream trace))
            (string (create-output-stream trace))
            (stream trace)))
         (stream (create-output-stream file append))
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
         (ncounts (length *redwoods-score-counts*))
         (ncolumns (+ (if loosep 8 7) n ncounts))
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

    (labels ((name (type)
               (case type
                 (:ta "tagging\\naccuracy")
                 (:parseval "labeled\\nbrackets")
                 (t (format nil "~a\\nscore" type)))))
      (case format
        (:latex
          (format
           stream
           "\\begin{tabular}{@{}|l|c|c|c|c|c|~:[~;c|~]c|@{}}~%  ~
            \\hline~%  ~
            \\multicolumn{~d}{|c|}~%    {\\bf `~a' ~a Profile}\\\\~%  ~
            \\hline\\hline~%  ~
            & {\\bf  total} & {\\bf total} & {\\bf word} ~
              & {\\bf parser}~%    ~
              & {\\bf exact} & {\\bf near}~:[~; & {\\bf loose}~]~
              & {\\bf overall}\\\\~%  ~
            {\\bf ~a} & {\\bf items} & {\\bf scores} & {\\bf string} ~
              & {\\bf analyses}~%    ~
              & {\\bf matches} & {\\bf matches}~:[~; & {\\bf matches}~]~
              & {\\bf accuracy}\\\\~%  ~
            & $\\sharp$ & $\\sharp$ & $\\phi$ & $\\phi$~%    ~
              & $\\sharp$ & $\\sharp$ & $\\%$\\\\~%  ~
            \\hline~%  ~
            \\hline~%"
           loosep
           ncolumns
           (if (stringp data) data "Some") "Parse Accuracy"
           loosep 
           alabel loosep
           loosep))
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
           cell 1 ~d -contents \"overall\\naccuracy\\n%\" -format title~%"
          alabel 
          loosep (+ 7 n) 
          (- ncolumns ncounts))
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
         (loop
             for type in *redwoods-score-counts*
             for j from (+ (- ncolumns ncounts) 1)
             do
               (format
                stream
                "cell 1 ~d -contents \"~a\\n~c\" -format title~%"
                j (name type)
                (code-char (if *redwoods-score-microaverage-p* #x03c6 #x03a6))))
         (format stream "~%"))
        (:ascii
         (format stream "`~a' # `~a' Parse Accuracy~%" data gold))))

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
        for accuracy = (if (zerop scores)
                         100
                         (* (divide (+ exact near) scores) 100))
        for tcounts = (get-field :tcounts data)
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
               cell ~d ~d -contents ~,2f -format data~%"
              i name
              i items
              i scores
              i length
              i analyses
              i exact
              loosep i (+ 7 n) loose
              i (- ncolumns ncounts) accuracy)
             (unless (zerop n)
               (loop
                   for j from 0 to (- n 1)
                   for k = (aref successes (+ j 1))
                   do
                     (format
                      stream "cell ~d ~d -contents ~,1f -format data~%"
                      i (+ 7 j) k)))
             (loop
                 for type in *redwoods-score-counts*
                 for j from (+ (- ncolumns ncounts) 1)
                 for record = (rest (assoc type tcounts))
                 for score = (if *redwoods-score-microaverage-p*
                               (or (first record) 0)
                               (apply #'f-one (rest record)))
                 do
                   (format
                    stream "cell ~d ~d -contents ~,1f -format data~%"
                    i j (* score 100)))))
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
           (accuracy (if (zerop scores)
                       100
                       (* (divide (+ exact near) scores) 100)))
           (tcounts (get-field :tcounts data)))
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
           cell ~d ~d -contents ~,2f -format total~%"
          (- i 1) i
          i name
          i items
          i scores
          i length
          i analyses
          i exact
          loosep i (+ 7 n) loose
          i (- ncolumns ncounts) accuracy)
         (unless (zerop n)
           (loop
               for j from 0 to (- n 1)
               for k = (aref successes (+ j 1))
               do
                 (format
                  stream
                  "cell ~d ~d -contents ~,1f -format total~%"
                  i (+ 7 j) k)))
         (loop
             for type in *redwoods-score-counts*
             for j from (+ (- ncolumns ncounts) 1)
             for record = (rest (assoc type tcounts))
             for score = (if *redwoods-score-microaverage-p*
                           (or (first record) 0)
                           (apply #'f-one (rest record)))
             do
               (format
                stream "cell ~d ~d -contents ~,1f -format data~%"
                i j (* score 100))))
        (:ascii
         (format
          stream
          "  ~a item~p; ~a score~p; ~a robust;~%  ~
             exact match: ~a (~,2f%);~%"
          items items scores scores (get-field :robust data)
          exact (* (/ exact items) 100))
         (loop
             for type in *redwoods-score-counts*
             for record = (rest (assoc type tcounts))
             for test = (second record)
             for gold = (third record)
             for correct = (fourth record)
             do
               (format
                stream
                "  [~a] ~a test; ~a gold; ~a correct: ~
                 P=~,1f; R=~,1f; F=~,1f~%"
                type test gold correct
                (* (divide correct test) 100) (* (divide correct gold) 100)
                (* (f-one test gold correct) 100)))))
      (format stream "~%")
      (force-output stream))
    (when (stringp trace) (close *redwoods-trace*))
    (when (or (stringp file) (stringp append)) (close stream))))
 
(defun summarize-scores (data &optional (gold data)
                         &key (condition *statistics-select-condition*)
                              spartanp (scorep t) (n 1) test loosep analogon
                              accumulation
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
  ;; when comparing best-first parser output against a gold standard).  to make
  ;; the picture even more complex, scores can be read either (1) from .data.
  ;; itself or (2) from an `overlay' profile (which we call spartan, seeing it
  ;; only holds a subset of relations, e.g. `item', `score', and `fold').
  ;;
  (let* ((thorough (if *redwoods-score-counts*
                     '(:derivation :mrs)
                      (when (eq test :derivation) '(:derivation))))
         ;;
         ;; _fix_me_
         ;; ideally, score-item() should not have to look at anything from the
         ;; `result' relation but the result identifier; the following for now
         ;; includes the `surface' field, so we can perform a string similarity
         ;; comparison on generator outputs.                   (28-oct-04; oe)
         ;;
         (thorough (if *redwoods-score-similarities*
                     (cons :surface thorough)
                     thorough))
         (items (if (stringp data)
                  (analyze (if spartanp gold data)
                           :thorough (or thorough spartanp)
                           :condition condition 
                           :score (if scorep data t) :scorep t
                           :readerp (or (eq test :derivation)
                                        (not (null *redwoods-score-counts*))))
                  data))
         (gitems (if (stringp gold)
                   (analyze gold
                            :thorough thorough
                            :condition condition :gold gold 
                            :readerp (or (eq test :derivation)
                                         (not (null *redwoods-score-counts*))))
                   gold))
         (aggregates (if analogon
                       (aggregate-by-analogy items analogon)
                       (aggregate items :format format)))
         (gaggregates (aggregate-by-analogy gitems aggregates :loosep t))
         #+:lkb
         (lkb::%failures% nil)
         results)

    (loop
        with nsimilarities = (length *redwoods-score-similarities*)
        with ncounts = (length *redwoods-score-counts*)
        with total = (rest (rest (get-field :total accumulation)))
        with tnitems = (get-field+ :item total 0)
        with tnscores = (get-field+ :scores total 0)
        with tlength = (* (get-field+ :i-length total 0) tnscores)
        with treadings = (* (get-field+ :analyses total 0) tnscores)
        with texact = (get-field+ :exact total 0)
        with tnear = (get-field+ :near total 0)
        with tloose = (get-field+ :loose total 0)
        with trobust = (get-field+ :robust total 0)
        with ttsimilarities = (make-array nsimilarities :initial-element 0)
        with tnsimilarities = (make-array nsimilarities :initial-element 0)
        with ttcounts = (make-array ncounts)
        with tncounts = (make-array ncounts)
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
              with arobust = 0
              with atsimilarities
              = (make-array nsimilarities :initial-element 0)
              with ansimilarities
              = (make-array nsimilarities :initial-element 0)
              with atcounts = (make-array ncounts)
              with ancounts = (make-array ncounts)
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
                #+:lkb
                (setf lkb::%failures% nil)
                (multiple-value-bind (i score loosep similarities counts)
                    ;;
                    ;; there are four possible outcomes of score-item():
                    ;;   nil: incomparable items (e.g. missing :ranks)
                    ;;   (= i 1): exact match at the top-ranked result
                    ;;   (<= i n): exact match at the .i.th result
                    ;;   (= i 0): no exact match within beam of size .n. 
                    ;;
                    (score-item item gitem :test test :n n :loosep loosep)
                  
                  ;;
                  ;; first, print per-item trace information when requested
                  ;;
                  (when (and trace (open-stream-p trace))
                    (if (numberp i)
                      (format trace "[~a] <~a ~,1f" i-id i score)
                      (format trace "[~a] <null" i-id))
                    (format
                     trace "~@[ : ~{~,2f~^ ~}~]"
                     (loop
                         for similarity in similarities
                         collect (second similarity)))
                    (loop
                        for count in counts
                        when (eq count (first counts)) do
                          (format trace " #")
                        do
                          (format
                           trace " ~(~a~): ~,1f ~{~d~^ ~}"
                           (first count) (* (first (second count)) 100)
                           (rest (second count))))
                    (format
                     trace "> {~a} |~a|~%"
                     (length (get-field :ranks gitem))
                     (get-field :i-input gitem))
                    #+:null
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
                                 "  < [~a] {~a}~@[ |~a|~]~%"
                                 (get-field :result-id grank)
                                 i (get-field- :surface grank))))
                    #+:null
                    (loop
                        for rank in (get-field :ranks item)
                        for score = (let ((foo (get-field :score rank)))
                                      (if (stringp foo)
                                        (read-from-string foo)
                                        foo))
                        do
                          (format
                           trace
                           "  > [~a] {~a} ~@[<~,6f>~]~@[ |~a|~]~%"
                           (get-field :result-id rank)
                           (get-field :rank rank)
                           score
                           (get-field- :surface rank)))
                    (format trace "~%")
                    (force-output trace))
                  
                  ;;
                  ;; now accumulate the various scores for this aggregate
                  ;;
                  (incf anitems)
                  #+:lkb 
                  (when lkb::%failures% (incf arobust))
                  (when (or (numberp i) *redwoods-score-all-p*)
                    (incf arandom (divide 1 areadings))
                    (loop
                        for i from 0
                        for (tag top nbest) in similarities
                        do
                          (setf tag tag)
                          (incf (aref atsimilarities i) top)
                          (incf (aref ansimilarities i) nbest))
                    (loop
                        for i from 0
                        for (tag top nbest) in counts
                        do
                          (setf tag tag)
                          (if (null (aref atcounts i))
                            (setf (aref atcounts i) (copy-list top))
                            (let ((foo (aref atcounts i)))
                              (incf (first foo) (first top))
                              (incf (second foo) (second top))
                              (incf (third foo) (third top))
                              (incf (fourth foo) (fourth top))))
                          (if (null (aref ancounts i))
                            (setf (aref ancounts i) (copy-list nbest))
                            (let ((foo (aref ancounts i)))
                              (incf (first foo) (first nbest))
                              (incf (second foo) (second nbest))
                              (incf (third foo) (third nbest))
                              (incf (fourth foo) (fourth nbest)))))
                    (incf anscores)
                    (incf alength length)
                    (incf areadings readings)
                    (unless (or (null i) (zerop i))
                      (when (<= i n)
                        (if (= i 1) (incf aexact score) (incf anear score)))
                      (when loosep (incf aloose))
                      (when asuccesses 
                        (incf (aref asuccesses (- i 1)) score)))))
              finally
                (incf tnitems anitems) (incf tnscores anscores)
                (incf tlength alength) (incf treadings areadings) 
                (incf texact aexact) (incf tnear anear) (incf tloose aloose)
                (loop
                    for i from 0
                    for atsimilarity across atsimilarities
                    do (incf (aref ttsimilarities i) atsimilarity))
                (loop
                    for i from 0
                    for ansimilarity across ansimilarities
                    do (incf (aref tnsimilarities i) ansimilarity))
                (loop
                    for i from 0
                    for ncount across ancounts
                    when ncount do
                      (if (null (aref tncounts i))
                        (setf (aref tncounts i) (copy-list ncount))
                        (let ((foo (aref tncounts i)))
                          (incf (first foo) (first ncount))
                          (incf (second foo) (second ncount))
                          (incf (third foo) (third ncount))
                          (incf (fourth foo) (fourth ncount)))))
                (loop
                    for i from 0
                    for tcount across atcounts
                    when tcount do
                      (if (null (aref ttcounts i))
                        (setf (aref ttcounts i) (copy-list tcount))
                        (let ((foo (aref ttcounts i)))
                          (incf (first foo) (first tcount))
                          (incf (second foo) (second tcount))
                          (incf (third foo) (third tcount))
                          (incf (fourth foo) (fourth tcount)))))
                (loop
                    for i from 0
                    for j across asuccesses
                    do (incf (aref tsuccesses i) j))
                (incf trandom arandom)
                (incf trobust arobust)
                (push (nconc (list id name)
                             (pairlis
                              '(:items :scores 
                                :i-length 
                                :analyses
                                :exact :near :loose :robust
                                :successes
                                :tsimilarities :nsimilarities
                                :tcounts :ncounts
                                :random)
                              (list anitems anscores 
                                    (divide alength anscores)
                                    (divide areadings anscores)
                                    aexact anear aloose arobust
                                    asuccesses
                                    (loop
                                        for tag
                                        in *redwoods-score-similarities*
                                        for similarity across atsimilarities
                                        for average
                                        = (divide similarity anscores)
                                        collect (cons tag average))
                                    (loop
                                        for tag
                                        in *redwoods-score-similarities*
                                        for similarity across ansimilarities
                                        collect (cons
                                                 tag
                                                 (divide similarity anscores)))
                                    (loop
                                        for tag in *redwoods-score-counts*
                                        for counts across atcounts
                                        unless counts do
                                          (setf counts (list 0 0 0 0))
                                        do (setf (first counts)
                                             (divide (first counts) anscores))
                                        collect (cons tag counts))
                                    (loop
                                        for tag in *redwoods-score-counts*
                                        for counts across ancounts
                                        unless counts do
                                          (setf counts (list 0 0 0 0))
                                        do (setf (first counts)
                                             (divide (first counts) anscores))
                                        collect (cons tag counts))
                                    arandom)))
                      results))
        finally
          (push (nconc (list :total "Total")
                       (pairlis
                        '(:items :scores 
                          :i-length
                          :analyses
                          :exact :near :loose :robust
                          :successes
                          :tsimilarities :nsimilarities
                          :tcounts :ncounts
                          :random)
                        (list tnitems tnscores 
                              (divide tlength tnscores)
                              (divide treadings tnscores)
                              texact tnear tloose trobust
                              tsuccesses
                              (loop
                                  for tag in *redwoods-score-similarities*
                                  for similarity across ttsimilarities
                                  collect (cons
                                           tag
                                           (divide similarity tnscores)))
                              (loop
                                  for tag in *redwoods-score-similarities*
                                  for similarity across tnsimilarities
                                  collect (cons
                                           tag
                                           (divide similarity tnscores)))
                              (loop
                                  for tag in *redwoods-score-counts*
                                  for counts across ttcounts
                                  unless counts do (setf counts (list 0 0 0 0))
                                  do (setf (first counts)
                                       (divide (first counts) tnscores))
                                  collect (cons tag counts))
                              (loop
                                  for tag in *redwoods-score-counts*
                                  for counts across tncounts
                                  unless counts do (setf counts (list 0 0 0 0))
                                  do (setf (first counts)
                                       (divide (first counts) tnscores))
                                  collect (cons tag counts))
                        trandom)))
                results))
    ;;
    ;; _fix_me_
    ;; this seems a tad overly defensive, nowadays: with expensive metrics like
    ;; EDM, it might be good to hold on to these profiles.      (24-jan-12; oe)
    ;;
    (when (eq test :derivation)
      (purge-profile-cache data :expiryp nil)
      (unless (equal data gold) (purge-profile-cache gold :expiryp nil)))
    #+:null
    (when *redwoods-score-counts*
      (lkb::release-temporary-storage :task :reconstruct))
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
                      (and foo bar (derivation-equal foo bar)))))))
        (cache (pairlis '(:mrs :edm)
                        (list (make-hash-table :test #'eq)
                              (make-hash-table :test #'equal)))))
    ;;
    ;; check calling context: both the test item and ground truth need to come
    ;; with ranks; all ranks in the ground truth are 1 (since they came from
    ;; annotations in a treebank); and unless .loosep. is on, there must not be
    ;; more than one gold target.
    ;;
    (cond
     ((or (null granks)
          (loop
              for grank in granks
              for rank = (get-field :rank grank)
              thereis (or (not (integerp rank)) (not (= rank 1))))
          (and (rest granks) (null loosep)))
      nil)
     ((null ranks)
      (values
       nil 0 nil nil
       (when *redwoods-score-counts*
         ;;
         ;; to include, say, ParsEval or EDM counts in the global statistics,
         ;; even where an item failed to parse, we still need to obtain counts
         ;; of 'gold' elements; result-similarity() must be robust to an empty
         ;; argument, and F1 scores will always be 0 in these cases.  hence, in
         ;; case there are multiple golds (and following the 'friendly' spirit
         ;; applied to multi-gold scoring below), look for the counts with the
         ;; smallest number of gold elements.
         ;;
         (loop
             for tag in *redwoods-score-counts*
             for best = nil
             do
               (loop
                   for grank in granks
                   for score
                   = (result-similarity nil grank :type tag :cache cache)
                   when (or (null best) (< (third score) (third best)))
                   do (setf best score))
             collect (list tag best best)))))
                        
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
            (let (similarities counts)
              ;;
              ;; from the realization ranking days, we already support a notion
              ;; of string similarity functions (e.g. BLEU or word accuracy).
              ;; to make these well-defined where there can both be multiple
              ;; gold-standard results and multiple (tied) candidate results,
              ;; we maximize, i.e. look for the highest-scoring test.  one may
              ;; argue that this approach over-estimates similarities (as it
              ;; fails to penalize ties), but it has the great advantage that
              ;; it straightforwardly extends to n-best scoring, i.e. looking
              ;; for the best result among the top-n candidates.
              ;;
              (when (or *redwoods-score-similarities* *redwoods-score-counts*)
                (loop
                    with foo = (length *redwoods-score-similarities*)
                    with tscores = (make-array foo :initial-element 0)
                    with nscores = (make-array foo :initial-element 0)
                    with bar = (length *redwoods-score-counts*)
                    with tcounts = (make-array bar)
                    with ncounts = (make-array bar)
                    with ranks
                    = (loop
                          for rank in ranks
                          for i = (get-field :rank rank)
                          while (<= i n) collect rank)
                    with granks = (loop
                                      for rank in granks
                                      while (= (get-field :rank rank) 1)
                                      collect rank)
                    for rank in ranks
                    do
                      (loop
                          for grank in granks
                          do
                            (loop
                                for i from 0
                                for tag in *redwoods-score-similarities*
                                for score
                                = (string-similarity
                                   rank grank :type tag)
                                when (numberp score) do
                                  (setf (aref nscores i)
                                    (max (aref nscores i) score))
                                  (when (= (get-field :rank rank) 1)
                                    (setf (aref tscores i)
                                      (max (aref tscores i) score))))
                            (loop
                                for i from 0
                                for tag in *redwoods-score-counts*
                                for score
                                = (result-similarity
                                   rank grank :type tag :cache cache)
                                for key = (first score)
                                when (and (numberp key)
                                          (or (null (aref ncounts i))
                                              (< (first (aref ncounts i)) key)))
                                do
                                  (setf (aref ncounts i) score)
                                  (when (= (get-field :rank rank) 1)
                                    (setf (aref tcounts i) score))))
                    finally
                      (setf similarities
                        (loop
                            for tag in *redwoods-score-similarities*
                            for top across tscores
                            for nbest across nscores
                            collect (list tag top nbest)))
                      (setf counts
                        (loop
                            for tag in *redwoods-score-counts*
                            for top across tcounts
                            for nbest across ncounts
                            collect (list tag top nbest)))))
              (return (values (if best (get-field :rank best) 0)
                              (if best (divide 1 (+ (length result) 1)) 0)
                              (rest granks) similarities counts
                              errors match (delete match result)))))))))

(defun string-similarity (rank grank &key (type :bleu) scrub)
  (let ((*string-similarity-punctuation-characters* nil)
        (string (get-field :surface rank))
        (gstring (get-field :surface grank)))
    (first (score-strings
            (list string) (list gstring) :type type :scrub scrub))))

(defun result-similarity (rank grank &key (type :parseval) cache)
  (labels ((get-mrs (result)
             (or (and cache (gethash result (get-field :mrs cache)))
                 (let ((mrs (get-field :mrs result)))
                   (unless (mrs::psoa-p mrs)
                     (if (and (stringp mrs) (not (string= mrs "")))
                       (setf mrs (mrs::read-mrs-from-string mrs))
                       (let ((edge (get-field :edge result)))
                         (unless (lkb::edge-p edge)
                           (let ((derivation (get-field :derivation result)))
                             (when derivation
                               (setf edge (reconstruct derivation)))))
                         (when edge (setf mrs (mrs::extract-mrs edge))))))
                   (if cache
                     (setf (gethash result (get-field :mrs cache))
                       (if (mrs::psoa-p mrs) mrs :fail))
                     (if (mrs::psoa-p mrs) mrs :fail))))))
    (cond
     ((smember type '(:ta :parseval))
      (let* ((reader (find-attribute-reader :derivation))
             (derivation (let ((value (get-field :derivation rank)))
                           (when (and (stringp value) reader)
                             (setf value
                               (ignore-errors (funcall reader value)))
                             (when value (set-field :derivation value rank)))
                           value))
             (gderivation (let ((value (get-field :derivation grank)))
                            (when (and (stringp value) reader)
                              (setf value
                                (ignore-errors (funcall reader value)))
                              (when value (set-field :derivation value grank)))
                            value))
             (counts (case type
                       (:ta (tagging-accuracy derivation gderivation))
                       (:parseval (parseval derivation gderivation))))
             (test (get-field :test counts))
             (gold (get-field :gold counts))
             (correct (get-field :correct counts)))
        (list (f-one test gold correct) test gold correct)))
     ((smember type '(:edm :edmna :edmap :edmn :edma :edmp))
      (let* ((mrs (let ((foo (get-mrs rank))) (and (mrs::psoa-p foo) foo)))
             (gmrs (let ((foo (get-mrs grank))) (and (mrs::psoa-p foo) foo)))
             (edm (when gmrs
                    (let ((key (cons mrs gmrs)))
                      (if cache
                        (or (gethash key (get-field :edm cache))
                            (setf (gethash key (get-field :edm cache))
                              (mrs::edm mrs gmrs)))
                        (mrs::edm mrs gmrs))))))
        (if (null edm)
          (list 0 0 0 0)
          (let* (test gold correct)
            (case type
              (:edm
               (push :tn test) (push :ta test) (push :tp test)
               (push :gn gold) (push :ga gold) (push :gp gold)
               (push :cn correct) (push :ca correct) (push :cp correct))
              (:edmna
               (push :tn test) (push :ta test)
               (push :gn gold) (push :ga gold)
               (push :cn correct) (push :ca correct))
              (:edmap
               (push :ta test) (push :tp test)
               (push :ga gold) (push :gp gold)
               (push :ca correct) (push :cp correct))
              (:edmn
               (push :tn test) (push :gn gold) (push :cn correct))
              (:edma
               (push :ta test) (push :ga gold) (push :ca correct))
              (:edmp
               (push :tp test) (push :gp gold) (push :cp correct)))
            (setf test (loop for key in test sum (get-field key edm)))
            (setf gold (loop for key in gold sum (get-field key edm)))
            (setf correct (loop for key in correct sum (get-field key edm)))
            (list (f-one test gold correct) test gold correct))))))))

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
          (multiple-value-bind (rank foo bar baz fee errors match others)
              (score-item item gitem :test test :n n :loosep loosep :errorp t)
            (declare (ignore foo bar baz fee))
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

(defun operate-on-profiles
    (profiles
     &key (condition *statistics-select-condition*) scores
          (recursep t) internalp (stream *tsdb-io*) (verbose t) (task :fc)
          (model (if (eq task :pcfg) (make-cfg) (make-model))) (purgep t)
          target initialp finalp firstp lastp
          (resolvedp t)
          (increment %redwoods-items-increment%) cache interrupt meter)

  (declare (ignore interrupt meter)
           (special *feature-item-enhancers* *feature-flags*
                    *feature-grandparenting* *feature-ngram-size*))


  ;;
  ;; invoke various memory-intensive operations successively on sub-sets of
  ;; items from .profiles.  go through three states: (a) the top-level call 
  ;; must always provide a list of profiles and dispatches recursively for
  ;; each individual profile; (b) when operating on a single profile, we work
  ;; out a suitable sub-division of items, construct a conditon, and pass that
  ;; into yet another recursive call; finally, (c) the target task is executed.
  ;;
  ;; there are four relevant boundary conditions: working on the first or last
  ;; profile and, within each profile, working on the first or last increment.
  ;; .initialp. and .finalp. are profile-level, .first. and .lastp. increment-
  ;; level indicators of where we are in the calling chain.
  ;;
  (unless (or internalp (consp profiles))
    (error "operate-on-profiles(): non-list argument `~a'." profiles))

  (cond
   ((consp profiles)
    (loop
        with lkb::*edge-registry* = nil
        with *tsdb-connection-expiry* = 200
        with gc = (install-gc-strategy nil :tenure nil :verbose verbose)
        with condition
        = (case task
            ((:fc :rank :pcfg)
             (if (eq *redwoods-task* :classify)
               condition
               (let ((n (if (eq task :pcfg) 0 1)))
                 (if resolvedp
                   (format
                    nil "t-active > 0 && readings > ~a~@[ && (~a)~]"
                    n (unless (equal condition "") condition))
                   (format
                    nil "readings > ~a~@[ && (~a)~]"
                    n (unless (equal condition "") condition))))))
            (t condition))
        for remaining on profiles
        for active = (first remaining)
        for virtualp = (virtual-profile-p active)
        when (find-tsdb-directory active :test t)
        do
          (operate-on-profiles
           active :scores scores
           :condition condition :model model :task task :stream stream
           :initialp (eq remaining profiles) :finalp (null (rest remaining))
           :target target :internalp t :resolvedp resolvedp)
          (when (and purgep (not virtualp)) (purge-profile-cache active))
        else do
          (format 
           stream
           "operate-on-profiles(): invalid `~a'.~%"
           active)  
          finally
          (restore-gc-strategy gc))
    ;;
    ;; _fix_me_
    ;; when operating on a group of profiles, there is no clear notion of which
    ;; should be home to the `model' file (including the symbol table, counts,
    ;; and maybe other relevant information).  rethink?   (1-feb-06; erik & oe)
    ;;
    (when (eq task :fc)
      (let ((embassador (first profiles)))
        (print-model
         model :file (profile-find-model embassador) :format :freeze)))
    model)

   ((virtual-profile-p profiles)
    (loop
        with profiles = (virtual-profile-components profiles)
        for remaining on profiles
        for active = (first remaining)
        do
          (operate-on-profiles
           active :scores scores
           :condition condition :model model :task task :stream stream
           :initialp (and initialp (eq remaining profiles))
           :finalp (and finalp (null (rest remaining)))
           :target target :internalp t :resolvedp resolvedp)
          (when purgep
            (purge-profile-cache active)
            #+:allegro
            (progn
              (let ((*tsdb-gc-debug* nil))
                (excl:print-type-counts :new) (excl:print-type-counts :old)
                (excl:gc :tenure)
                (excl:print-type-counts :new) (excl:print-type-counts :old)
                (excl:gc t)
                (excl:print-type-counts :old)
                (excl:print-type-counts :holes))))))
   
   (recursep
    (when (eq task :unfc)
      (let ((fc (profile-find-feature-cache profiles)))
        (ignore-errors (delete-file (fc-file fc))))
      (return-from operate-on-profiles))
    
    (when verbose
      (format 
       stream
       "[~a] operate-on-profiles(): reading `~a'~%" 
       (current-time :long :short) profiles))
    (loop
        with items = (select "i-id" :integer "item" nil profiles :sort :i-id)
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
                            return (max increment
                                        (- (get-field :i-id item) first)))
                        increment)
                       increment)
        with n = (max (ceiling (- last first) delta) 1)
        with cache 
        = (when (eq task :rank)
            (when initialp
              (purge-test-run target :action :score))
            (let ((target (or target profiles)))
              (create-cache target :verbose verbose :protocol :raw)))
        for i from 1 to n
        for low = (+ first (* (- i 1) delta))
        for high = (+ first (* i delta))
        for foo = (format 
                   nil 
                   "i-id >= ~d && i-id < ~d~@[ && (~a)~]"
                   low high condition)
        do
          (when verbose
            (format
             stream
             "[~a] operate-on-profiles(): running `~a' [~a - ~a|.~%"
             (current-time :long :short) profiles low high))
          (operate-on-profiles
           profiles :scores scores
           :condition foo :model model :task task :stream stream
           :internalp t :recursep nil 
           :initialp initialp :finalp finalp :firstp (= i 1) :lastp (= i n)
           :target target :cache cache :resolvedp resolvedp)
          ;;
          ;; do not expire the DB yet, while running sub-sets of items from it
          ;;
          (purge-profile-cache profiles :expiryp nil)
        finally (when cache (flush-cache cache :verbose verbose))))
   
   (t
    (let* ((thorough
            (append
             (when (or (eq task :pcfg)
                       (>= *feature-grandparenting* 0)
                       (> *feature-ngram-size* 0))
               '(:derivation))
             (and *feature-flags* '(:flags))
             (and (eq *redwoods-task* :rank) '(:surface))
             (when (and (eq task :score) *redwoods-score-counts*)
               '(:derivation :mrs))))
           (thorough (remove-duplicates thorough))
           (data (analyze
                  profiles :thorough thorough :condition condition
                  :gold (and (eq *redwoods-task* :rank) profiles)
                  :commentp t :tokensp (eq *redwoods-task* :classify))))
      (case task
        (:fc
         (loop
             for enhancer in *feature-item-enhancers*
             do
               (loop
                   for item in data
                   do (call-raw-hook enhancer item)))
         (when (eq *redwoods-task* :rank)
           (setf data
             (loop  
                 for item in data
                 for readings = (get-field :readings item)
                 for ranks = (length (get-field :ranks item))
                 unless (or (= readings ranks) (null ranks)) collect item)))
         (cache-features
          data model :createp firstp
          :stream stream :verbose verbose))
        ;;
        ;; _fix_me_
        ;; this bit is hacky and MEM-specific for now.    (4-apr-06; erik & oe)
        ;;
        (:rank
         (loop
             for enhancer in *feature-item-enhancers*
             do
               (loop
                   for item in data
                   do (call-raw-hook enhancer item)))
         (loop
             with target = (or target profiles)
             for item in data
             for readings = (get-field :readings item)
             for parse-id = (get-field :parse-id item)
             for results = (get-field :results item)
             for nresults = (length results)
             for nranks = (length (get-field :ranks item))
             unless (= readings nranks)
             do
               (format
                stream
                "~&[~a] operate-on-profiles(): ~
                 scored item # ~d (~d @ ~d).~%"
                (current-time :long :short)
                (get-field :i-id item) nresults nranks)
               
               (let* ((ranks
                       (loop
                           for result in results
                           for rid = (get-field :result-id result)
                           for score
                           = (mem-score-result result (or model %model%))
                           collect (pairlis '(:parse-id :result-id :score)
                                            (list parse-id rid score))))
                      (ranks (sort
                              ranks #'>
                              :key #'(lambda (foo) (get-field :score foo))))
                      (ranks (loop
                                 with last = (get-field :score (first ranks))
                                 with i = 1
                                 with j = 2
                                 for rank in ranks
                                 for score = (get-field :score rank)
                                 unless (= score last) do
                                   (setf i j) (setf last score) (incf j)
                                 collect (acons :rank i rank))))
                 
                 (loop
                     for score in ranks
                     do (write-score target score :cache cache)))))
        (:pcfg
         (when data
           (format
            stream
            "~&[~a] operate-on-profiles(): ~
           ~a PCFG item~p [~a -- ~a].~%"
            (current-time :long :short)
            (length data) (length data)
            (get-field :i-id (first data))
            (get-field :i-id (first (last data))))
           (estimate-cfg data :cfg model :estimate (and lastp finalp))
           (when (and lastp finalp (or (stringp target) (streamp target)))
             (print-cfg model :stream target :format :export)))))))))

(defun train (source file
              &key (condition *statistics-select-condition*)
                   (type :mem) (fcp t) (ccp t) (identity (current-pid)) target
                   (resolvedp t) normalizep
                   (verbose t) (stream t)
                   interrupt meter)

  (declare (ignore meter)
           (special *feature-item-enhancers*))

  (format 
   t 
   "[~a] train(): reading `~a'~%" 
   (current-time :long :short) source)
  
  ;;
  ;; _fix_me_
  ;; it looks like we cannot call train() with :fcp nil and expect to have our
  ;; `model' (feature table with counts) read from .source.  do we need to
  ;; make-model() here, actually?                        (5-apr-06; oe & erik)
  ;;
  (let ((*maxent-debug-p* t)
        (model (if fcp (make-model) (read-model (profile-find-model source)))))
    (declare (special *maxent-debug-p*))
    (when fcp
      (operate-on-profiles
       (list source) :condition condition
       :task :fc :model model :resolvedp resolvedp
       :verbose verbose :stream stream
       :interrupt interrupt))
    (rank-profile
     source target
     :nfold 1 :recache ccp :model model :type type :identity identity
     :resolvedp resolvedp :normalizep normalizep
     :enhancers (unless resolvedp *feature-item-enhancers*)
     :verbose verbose :stream stream
     :interrupt interrupt
     :condition condition)

    (let ((parameters 
           (case type
             (:mem (model-parameters model))
             ;;
             ;; _fix_me_
             ;; the script should probably reside in the `bin' sub-directory,
             ;; side-by-side with the other SVM binaries.       (12-mar-07; oe)
             ;;
             (:svm (let* ((output (format
                                   nil "~a/.model.~a.~a.svm_weights"
                                   (tmp :redwoods)
                                   (current-user) (current-pid)))

                          ;;
                          ;; _fix_me_
                          ;; svm2weights.pl will only work for linear kernels
                          ;;                                  (15-feb-07; erik)
                          (command (format
                                    nil "~a/uio/svm2weights.pl ~a"
                                    (getenv "LOGONROOT")
                                    (model-parameters model))))
                     (unless (and (zerop 
                                   (run-process
                                    command :wait t :output output 
                                    :if-output-exists :supersede))
                                  (probe-file output))
                       (format
                        t
                        "train(): unable to compute weights from SVM model.")
                       (return-from train)))))))
    
      (unless (probe-file parameters)
        (format t "train(): unable to read MLM parameters.")
        (return-from train))
      (read-weights  model parameters))
    (when file (print-model model :file file :format :export))
    model))

(defun rank-profile (source 
                     &optional target
                     &key (condition *statistics-select-condition*) data
                          (nfold 10) (niterations nfold) (type :mem) model
                          (percentage *redwoods-train-percentage*)
                          (identity (current-pid))
                          (stream *tsdb-io*) (cache :raw) (verbose t)
                          (overwrite t)
                          interrupt meter
                          (resolvedp t) recache enhancers normalizep)

  (declare (special *feature-flags*))
  
  (format
   stream
   "~&[~a] rank-profile():~%  `~a'~%~@[   --> `~a'~%~]"
   (current-time :long :short) source target)

  (when (and overwrite target) (purge-test-run target :action :score))
  
  (let* ((lkb::*edge-registry* nil)
         (gc (install-gc-strategy 
              nil :tenure *tsdb-tenure-p* :burst nil :verbose t))
         (condition
          (if resolvedp
            (if (and condition (not (equal condition "")))
              (format nil "t-active > 0 && readings > 1 && (~a)" condition)
              "t-active > 0 && readings > 1")
            (if (and condition (not (equal condition "")))
              (format nil "readings > 1 && (~a)" condition)
              "readings > 1")))
         ;;
         ;; _fix_me_
         ;; in principle, most experiments should be able to make do without
         ;; any data from the `result' relation (it can be huge and thus slow
         ;; to retrieve and join from the DB); this is not finished, though.
         ;;                                                     (23-nov-07; oe)
         (thorough (append
                    (and #+:null (eq type :ngram) '(:surface))
                    (and *feature-flags* '(:flags))))
         (gold (or data
                   (analyze 
                    source 
                    :thorough thorough :condition condition :gold source
                    :readerp nil :burst t :purge :db :message meter)))
         (nsifted 0)
         (data (loop
                   initially
                     (loop
                         for enhancer in enhancers
                         do
                           (loop
                               for item in gold
                               do (call-raw-hook enhancer item)))
                   for item in gold
                   for readings = (get-field :readings item)
                   for ranks = (length (get-field :ranks item))
                   unless (= readings ranks)
                   collect (copy-tree item)
                   else do (incf nsifted))))

    (when verbose
      (format
       stream
       "~&[~a] rank-profile(): using ~d items (ignoring ~d).~%"
       (current-time :long :short) (length data) nsifted))

    (when (smember type '(:mem :perf :svm))
      
      ;;
      ;; as of february 2006, whenever there is a feature cache there also has
      ;; to be a serialized partial model, recording the full symbol table and
      ;; frequency counts (later used in restricting the context cache).
      ;;
      (unless (model-p model)
        (setf model (read-model (profile-find-model source))))
      (unless (model-p model)
        (format
         t
         "~&[~a] rank-profile(): no MLM for `~a'.~%"
         (current-time :long :short) source)
        (return-from rank-profile))

      ;;
      ;; when requested, re-build the context cache
      ;;
      (when recache 
        (cache-contexts
         data model identity :format type :normalizep normalizep)))
    
    (loop
        with folds
        with cache = (when target
                       (create-cache target :verbose verbose :protocol cache))
        with sets = (let ((sets (when *redwoods-use-item-sets-p*
                                  (select-item-sets source))))
                      ;;
                      ;; when there are no item sets, manufacture a dummy list
                      ;; of singleton sets, one per item.
                      ;;
                      ;; _fix_me_
                      ;; for combinations of profiles where some provide item
                      ;; sets while others do not, the code below breaks.  it
                      ;; would seem we need to make sure to manufacture dummy
                      ;; sets for all items not found in an item set.
                      ;;                                        (23-oct-10; oe)
                      (if sets
                        (loop
                            for set in sets
                            for items
                            = (loop
                                  while data
                                  for id in set
                                  for item
                                  = (when (= (get-field :i-id (first data)) id)
                                      (pop data))
                                  when item collect item)
                            when items collect items)
                        (loop for item in data collect (list item))))
        with increment = (when meter (/ (mduration meter) nfold))
        ;;
        ;; for some model types, never do more than one iteration; also, if
        ;; there are fewer item sets than folds, we can maximally do one fold
        ;; per set.
        ;;
        with nfold
        = (if (smember type '(:ngram :chance :oracle)) 
            1
            (min (length sets) nfold))

        initially 
          #+:debug (setf %data% data)
          (when meter (meter :value (get-field :start meter)))

        for i from 1 to niterations

        when (interrupt-p interrupt)
        do
          (format 
           stream
           "[~a] rank-profile(): external interrupt signal~%"
           (current-time :long :short))
          (flush-cache cache :verbose verbose)
          (restore-gc-strategy gc)
          (return)

        do
          (multiple-value-bind (stest strain) (ith-nth sets i nfold)
            
            (let ((test (apply #'append stest))
                  (train (apply #'append strain)))
              
              ;;
              ;; for .nfold. == 1, train and test on the same data set
              ;;
              (when (and (null train) 
                         (not (smember type '(:ngram :chance :oracle))))
                (setf train test))
              
              (when (and (numberp percentage)
                         (> percentage 0) (< percentage 100))
                (setf train
                   (i-jth-nth train 1 percentage 100)))
              
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
                           nfold i (length test) (length train))))
                (multiple-value-bind (items fold)
                    (train-and-rank 
                     train (when target test)
                     :type type
                     :identity identity :model model :id i)
                  (loop
                      for item in items
                      for parse-id = (get-field :parse-id item)
                      for ranks = (get-field :ranks item)
                      when ranks do
                        (loop
                            for foo in ranks
                            for result-id = (get-field :result-id foo)
                            for score
                            = (let ((score (get-field :score foo)))
                                (if score (format nil "~,16f" score) ""))
                            for rank = (get-field :rank foo)
                            do
                              #+:debug
                              (format
                               stream
                               "  parse: ~a; result: ~d; rank: ~d; score: ~a~%"
                               parse-id result-id rank score)
                              (when target
                                (write-score target
                                             (pairlis '(:parse-id :result-id
                                                        :rank :score)
                                                      (list parse-id result-id
                                                            rank score))
                                             :cache cache)))
                      finally
                        (let* ((scores
                                (summarize-scores
                                 items gold :condition nil
                                 :n 5 :test :id :spartanp t :loosep t))
                               (total (rest (rest (assoc :total scores))))
                               (nscores (get-field :scores total))
                               (exact (get-field :exact total))
                               (near (get-field :near total))
                               (similarities
                                (let ((top (get-field :tsimilarities total))
                                      (nbest (get-field :nsimilarities total)))
                                  (append
                                   (and top (acons :tsimilarities top nil))
                                   (and nbest
                                        (acons :nsimilarities nbest nil)))))
                               (accuracy
                                (if exact (* 100 (divide exact nscores)) 0.0))
                               (naccuracy 
                                (if (and exact near)
                                  (float 
                                   (* 100 (divide (+ exact near) nscores)))
                                  0.0)))
                          (if (get-field :f-extras fold)
                            (nconc (get-field :f-extras fold) similarities)
                            (nconc fold (acons :f-extras similarities nil)))
                          (nconc (get-field :f-extras fold)
                            (acons :naccuracy naccuracy nil))
                          (nconc fold (acons :f-accuracy  accuracy nil)))
                        (push (acons :test test fold) folds))))))

          (when meter (meter-advance increment))

        finally 
          (when meter
            (meter :value (get-field :end meter))
            (status :text ""))
          (when target
            (loop
                for fold in folds
                do
                  (write-fold target fold :cache cache))
            (flush-cache cache :sort t :verbose verbose))
          
          (restore-gc-strategy gc))))

(defun train-and-rank (train test
                       &key (type :mem) (identity -1)
                            model id (stream *tsdb-io*))

  (declare (ignore stream))
  #+:debug
  (setf %train train %test test)
  (let* ((trains (let ((stream (make-string-output-stream)))
                   (loop
                       for item in train
                       do (format stream " ~a" (get-field :i-id item)))
                   (get-output-stream-string stream)))
         (tests (let ((stream (make-string-output-stream)))
                  (loop
                      for item in test
                      do (format stream " ~a" (get-field :i-id item)))
                  (get-output-stream-string stream)))
         (f-environment (case type
                          ((:svm :perf :mem) (feature-environment))))
         (l-environment (case type
                          ((:svm :perf) (svm-environment))
                          ((:mem) (mem-environment))))
         (environment (format nil "~a ~a" 
                              f-environment l-environment))
         (fold (pairlis '(:f-id :f-train :f-trains :f-test :f-tests
                          :f-environment :f-user :f-host :f-start)
                        (list (or id -1)
                              (length train) trains
                              (length test) tests
                              environment
                              (current-user) (current-host)
                              (current-time :long :tsdb))))
         (model (case type
                  (:pcfg (estimate-cfg train))
                  ((:mem :svm :perf) 
                   (estimate-model
                    train :identity identity :fold fold
                    :type type :model model))
                  (:ngram "string n-grams")
                  (:chance "chance")
                  (:oracle "oracle")))
         (ranks
          (when test
            (case type
              ((:svm :perf :mem)
               (learner-rank-items 
                test model :identity identity :fold fold :type type))
              (:chance
               (chance-rank-items test :fold fold))
              (:oracle
               (oracle-rank-items test :fold fold))
              #+:logon
              (:ngram (ngram-rank-items test :fold fold))))))
    (nconc fold (pairlis '(:f-end) (list (current-time :long :tsdb))))
    (values ranks fold)))

#+:logon
(defun ngram-rank-items (items &key fold)
  (declare (ignore fold))
  (loop
      for item in items
      for results = (get-field :results item)
      for ranks 
      = (loop
            for result in results 
            for string = (get-field :surface result)
            collect string into strings
            finally
              (return 
                (loop
                    for result in (get-field :results item)
                    for score in (mt::lm-score-strings strings)
                    for rid = (get-field :result-id result)
                    collect
                      (pairlis '(:result-id :score)
                               (list rid (- (cdr score)))))))
      do 
        (let* ((ranks (sort
                       ranks #'> 
                       :key #'(lambda (foo) (get-field :score foo))))
               (ranks (loop
                          with last = (get-field :score (first ranks))
                          with i = 1
                          with j = 2
                          for rank in ranks
                          for score = (get-field :score rank)
                          unless (= score last) do
                            (setf i j) (setf last score) (incf j)
                          collect (acons :rank i rank))))

          (if (get-field :ranks item)
              (setf (get-field :ranks item) ranks)
            (nconc item (acons :ranks ranks nil)))))
  items)
            
(defun chance-rank-items (test &key fold)
  (declare (ignore fold))
  (loop
      for item in test
      for results = (get-field :results item)
      for nresults = (length results)
      for random = (make-array nresults)
      collect
        (acons
         :ranks
         (loop
             for result in (get-field :results item)
             for rid = (get-field :result-id result)
             for rank
             = (loop
                   for i = (random nresults)
                   unless (aref random i)
                   do (setf (aref random i) i) and return (+ i 1))
             collect (pairlis '(:result-id :rank :score) (list rid rank 0.0))
             into ranks
             finally
               (return (sort
                        ranks #'<
                        :key #'(lambda (rank) (get-field :rank rank)))))
         item)))

(defun oracle-rank-items (test &key fold)
  (declare (ignore fold))
  (loop
      for item in test
      collect
        (acons
         :ranks
         (loop
             for result in (get-field :results item)
             for rid = (get-field :result-id result)
             with active = (loop for rank in (get-field :ranks item)
                               collect (get-field :result-id  rank))
             for activep = (if (member rid active :test #'=) 1 0)
             collect (pairlis '(:result-id :rank :score) 
                              (list rid activep activep)))
         item)))

(defun kappa (actual expected)
  (/ (- actual expected) (- 100 expected)))

(defun answer-enrich-mrs (edge &key (format :string))
  #+:lkb
  (let ((mrs (typecase edge
               (lkb::edge (mrs::extract-mrs edge))
               (mrs::psoa edge))))
    (when (mrs::psoa-p mrs)
      (setf (mrs::psoa-vcs mrs)
        (loop
            for edge in (mt::transfer-mrs mrs :filter nil :task :trigger)
            for mtr = (mt::edge-rule edge)
            for id = (mt::mtr-trigger mtr)
            when (and id (not (smember id lkb::*duplicate-lex-ids*)))
            collect id))
      (case format
        (:string
         (with-output-to-string (stream)
           (mrs::output-mrs1 mrs 'mrs::simple stream)))
        (:raw
         mrs)))))

;;;
;;; generate summary statistics for a set of treebanked profiles
;;;
#+:null
(loop
    with *phenomena* = nil
    with *statistics-aggregate-dimension* = :phenomena
    with *statistics-all-rejections-p* = t
    with *tsdb-home* = (logon-directory "lingo/terg/tsdb/gold" :string)
    initially 
      (purge-profile-cache :all)
      (when (probe-file "/tmp/redwoods.csv") (delete-file "/tmp/redwoods.csv"))
    for db in (find-tsdb-directories)
    for name = (get-field :database db)
    do (analyze-trees name :append "/tmp/redwoods.csv" :format :csv))

#|
for i in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21; do
  mkdir wsj${i}.1; 
  for j in wsj${i}?.1; do echo '"'$j'"'; done > wsj${i}.1/virtual; 
done
|#

#+:null
(loop
    with *phenomena* = nil
    with *statistics-aggregate-dimension* = :phenomena
    with *statistics-all-rejections-p* = t
    with *tsdb-home* = (logon-directory "coli/deepbank/tsdb/home" :string)
    initially
      (purge-profile-cache :all)
      (when (probe-file "/tmp/deepbank.csv") (delete-file "/tmp/deepbank.csv"))
    for name in (loop
                    for i from 0 to 21
                    collect (format nil "wsj~2,'0d.1" i))
    do (analyze-trees name :append "/tmp/deepbank.csv" :format :csv))
