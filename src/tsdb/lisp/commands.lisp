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

(defun retrieve (&optional condition (data *tsdb-data*)
                 &key mrs
                      (output (not *tsdb-ignore-output-p*))
                      (stream *tsdb-io*) (verbose t) meter)

  (initialize-tsdb)
  (when meter
    (status :text (format nil "retrieving `~a' data ..." data)))
  (let* ((imeter (if output
                   (madjust * meter 0.5)
                   meter))
         (ometer (when output
                   (madjust + (madjust * meter 0.5) (mduration imeter))))
         (granularity (profile-granularity data))
         (fields '("i-id" "i-wf" "i-length" "i-input" "i-comment"))
         (fields (if (< granularity 201011) fields (cons "i-tokens" fields)))
         (types '(:integer :integer :integer :string :string))
         (types (if (< granularity 201011) types (cons :string types)))
         (items (select fields types "item" (unless mrs condition) data
                        :unique nil :sort :i-id :meter imeter))
         (outputs (when output
                    (select '("i-id" "o-ignore" "o-surface"
                              "o-wf" "o-gc" "o-edges")
                            '(:integer :string :string
                              :integer :integer :integer)
                        "output"
                        (unless mrs condition)
                        data
                        :unique nil :sort :i-id
                        :meter ometer)))
         (condition (if mrs
                      (if (or (null condition) (equal condition ""))
                        "readings >= 1"
                        (format nil "(~a) && (readings >= 1)" condition))
                      condition))
         (results (when mrs
                    (select '("i-id" "parse-id" "result-id" "derivation" "mrs")
                            '(:integer :integer :integer :string :string)
                            '("parse" "result")
                            condition
                            mrs
                            :unique nil :sort :i-id)))
         (all (loop
                  for item in items
                  for iid = (get-field :i-id item)
                  for length = (get-field+ :i-length item 0)
                  for ogc = -2 for oedges = -2
                  for matches
                  = (when output
                      (loop
                          for record in (member
                                         iid outputs
                                         :key #'(lambda (foo)
                                                  (get-field :i-id foo)))
                          while (= (get-field :i-id record) iid)
                          do
                            (setf ogc (max ogc (get-field+ :o-gc record -1)))
                            (setf oedges
                              (max oedges (get-field+ :o-edges record -1)))
                          collect record))
                  do
                    (let* ((tokens (get-field :i-tokens item))
                           (n (when (stringp tokens) (- (length tokens) 1))))
                      (when (and n (< 3 n)
                                 (char= (schar tokens 0) #\()
                                 (char= (schar tokens 1) #\()
                                 (char= (schar tokens (- n 1)) #\))
                                 (char= (schar tokens n) #\)))
                        (let ((tokens (ignore-errors 
                                        (read-from-string tokens))))
                          (when tokens (set-field :i-tokens tokens item)))))
                    (let* ((comment (get-field :i-comment item))
                           (n (when (stringp comment) (- (length comment) 1))))
                      (when (and n (< 3 n)
                                 (char= (schar comment 0) #\()
                                 (char= (schar comment 1) #\()
                                 (char= (schar comment (- n 1)) #\))
                                 (char= (schar comment n) #\)))
                        (let ((comment (ignore-errors 
                                        (read-from-string comment))))
                          (nconc item comment))))
                    (nconc
                     item
                     (pairlis '(:o-gc :o-edges :outputs)
                              (list (and (> ogc -2) ogc)
                                    (and (> oedges 2) oedges)
                                    matches)))
                  when (> length 0) collect item)))
    (when results
      (setf all
        (loop
            for item in all
            for key = (get-field :i-id item)
            for matches =
              (when (eql key (get-field :i-id (first results)))
                (loop
                    for result = (first results)
                    while (and result 
                               (eql key (get-field :i-id result)))
                    collect (pop results)))
            when matches
            collect (nconc item 
                           (pairlis '(:parse-id 
                                      :results)
                                    (list (get-field :parse-id (first matches))
                                          matches)))))
      (setf all (sort all #'< 
                      :key #'(lambda (foo) (get-field :parse-id foo))))
      (rank-items items :gold mrs :sloppyp t))
    (when verbose
      (format
       stream 
       "~&retrieve(): found ~a item~:p (~a output specification~:p).~%" 
       (length all) (length outputs)))
    (when meter (meter :value (get-field :end meter)))
    (when meter
      (status :text (format nil "retrieving `~a' data ... done" data)))
    all))

(defun tsdb-do-vocabulary (language &key condition (load :warn) 
                                         (stream *tsdb-io*)
                                         meter interrupt)
  
  (initialize-tsdb)
  (let* ((*tsdb-gc-message-p* nil)
         (condition (if (equal condition "") nil condition))
         (loadp (not (member load (list nil :off :no :none))))
         (whitespace '(#\Space #\Newline #\Tab))
         (garbage (append whitespace 
                          (loop
                              for string in *tsdb-tokens-to-ignore* 
                              when (and (stringp string) (= (length string) 1))
                              collect (schar string 0)
                              else when (characterp string)
                              collect string)))
         (imeter (madjust * meter 0.1))
         (wmeter (madjust + (madjust * meter 0.9) (mduration imeter)))
         (items (retrieve condition language))
         (strings 
          (loop
              for item in items
              for i-input = (get-field :i-input item)
              for p-input = (call-hook *tsdb-preprocessing-hook* i-input)
              collect (or p-input i-input)))
         (frequencies (make-hash-table :test #'equal))
         (lexicon (make-hash-table :test #'equal ))
         (lstasks (make-hash-table :test #'equal ))
         (maximal-frequency 0)
         (message (format nil "retrieving `~a' vocabulary ..." language))
         words unknown-words)
    (when meter
      (status :text message)
      (meter :value (get-field :start meter)))
    (when (eq load :collect) (setf %tsdb-lexical-preterminals% nil))
    (when strings
      (format stream "~%")
      (dolist (string strings words)
        (do* ((i (position-if #'(lambda (c) (member c whitespace)) string)
                 (position-if #'(lambda (c) (member c whitespace)) string))
              (word (when i (subseq string 0 i))
                    (when i (subseq string 0 i)))
              (word (string-downcase (string-trim garbage word))
                    (string-downcase (string-trim garbage word)))
              (n (gethash word frequencies)
                 (gethash word frequencies))
              (string (if i (subseq string i) string)
                      (if i (subseq string i) string))
              (string (string-left-trim whitespace string)
                      (string-left-trim whitespace string)))
            ((not i)
             (let* ((word (string-downcase (string-trim garbage string)))
                    (n (gethash word frequencies)))
               (when (and word (> (length word) 0))
                 (setf (gethash word frequencies) (+ (or n 0) 1))
                 (setf (gethash word lexicon) 0)
                 (setf (gethash word lstasks) 0)
                 (setf maximal-frequency 
                   (max maximal-frequency (+ (or n 0) 1)))
                (pushnew word words :test #'equal))))
          (when (and word (> (length word) 0))
            (setf (gethash word frequencies) (+ (or n 0) 1))
            (setf (gethash word lexicon) 0)
            (setf (gethash word lstasks) 0)
            (setf maximal-frequency (max maximal-frequency (+ (or n 0) 1)))
            (pushnew word words :test #'equal))))
      (let* ((width (apply #'max (map 'list #'length words)))
             (tabulation (format 
                          nil
                          "~~~d,0t| ~~~dd"
                          (+ width 2 1) 
                          (length (format nil "~d" maximal-frequency))))
             (increment (when wmeter
                          (/ (mduration wmeter) (length words)))))
        (do* ((words (sort (copy-seq words) #'string-lessp) (rest words))
              (word (first words) (first words)))
            ((null words) unknown-words)
          (when (and interrupt (probe-file interrupt))
            (delete-file interrupt)
            (format
             stream
             "do-vocabulary(): received external interrupt signal.~%")
            (when meter 
              (status :text (format nil "~a interrupt" message) :duration 5))
            (return-from tsdb-do-vocabulary))
          (when (and loadp 
                     (not (member word *tsdb-tokens-to-ignore* 
                                  :test #'string-equal)))
            (let ((entries (parse-word word :load load)))
              (setf (gethash word lexicon) (get-field :words entries))
              (setf (gethash word lstasks) (get-field :l-stasks entries))
              (unless entries (push word unknown-words)))
            (format 
             stream 
             "~&  ~a ~@? reference(s)~:[~; | [~d + ~d] lexical entrie(s)~];~%" 
             word tabulation (gethash word frequencies)
             loadp (or (gethash word lexicon) -1) 
             (or (gethash word lstasks) -1)))
          (when increment (meter-advance increment)))))
    (format stream "~&~%")
    (when meter 
      (meter :value (get-field :end meter))
      (status :text (format nil "~a done" message) :duration 5))
    (length items)))

(defun tsdb (&optional action argument 
             &key condition run skeleton load gold host task wait quantum
                  (file nil filep) (reset nil resetp) count target error)
  
  (unless (and action (keywordp action)
               (let ((action (string action)))
                 (string-equal
                  "ini" (subseq action 0 (min (length action) 3)))))
    (initialize-tsdb))
  (if (stringp action)
    (let* ((result (call-tsdb action (or argument *tsdb-data*))))
      (when (and result (not (zerop (length result))))
        (format *tsdb-io* "~&~%~a~%" result)))
    (let ((action (string action)))
      (case (intern (subseq action 0 (min (length action) 3)) :keyword)

        ((:initialize :ini)
         (setf *tsdb-initialized-p* nil)
         (initialize-tsdb argument :pattern load :background run)
         (tsdb :info)
         (format *tsdb-io* "~&~%"))

        ((:podium :pod :po)
         (init-podium)
         (when argument
           (load-cache :pattern load :background t)))
        
        ((:cpus :cpu :cpu :cp)
         (format *tsdb-io* "~&~%")
         (cond
          ((null argument)
           (tsdb-do-cpus
            :action :active :host host :task task :stream *tsdb-io*))
          ((member argument '(:active :list :kill :shutdown))
           (tsdb-do-cpus
            :action argument :host host :task task :stream *tsdb-io*))
          (t
           (let ((clients
                  (cond
                   ((and filep resetp)
                    (initialize-cpus
                     :classes argument :count count :wait wait
                     :host host :task task :quantum quantum
                     :file file :reset reset :stream *tsdb-io* :prefix "  "))
                   (filep 
                    (initialize-cpus
                     :classes argument :count count :wait wait
                     :host host :task task :quantum quantum
                     :file file :stream *tsdb-io* :prefix "  "))
                   (resetp
                    (initialize-cpus
                     :classes argument :count count :wait wait
                     :host host :task task :quantum quantum
                     :reset reset :stream *tsdb-io* :prefix "  "))
                   (t
                    (initialize-cpus
                     :classes argument :count count :wait wait
                     :host host :task task :quantum quantum
                     :stream *tsdb-io* :prefix "  ")))))
             (when (and (eq error :exit) (< (length clients) (or count 1)))
               #+:allegro
               (excl:exit 0 :no-unwind t)
               #+:lispworks
               (lw:quit :ignore-errors-p t)
               #-(or :allegro :lispworks)
               (error
                "no known mechanism to shutdown Lisp (see `commands.lisp'")))))
         (format *tsdb-io* "~&~%"))
         
        ((:info :inf)
         (tsdb-do-status :all :stream *tsdb-io*))
        
        ((:home :hom :ho)
         (if (stringp argument)
           (tsdb-do-set (quote *tsdb-home*) argument)
           (tsdb-do-status :home :stream *tsdb-io*)))
        
        ((:default :def :de :d)
         (if (stringp argument)
           (tsdb-do-set (quote *tsdb-data*) argument)
           (tsdb-do-status :default :stream *tsdb-io*)))
        
        ((:list :lis :li :l)
         (tsdb-do-list (or argument *tsdb-home*)))
        
        ((:skeletons :ske :sk :s)
         (cond
          ((stringp argument)
           (tsdb-do-set (quote *tsdb-skeleton-directory*) argument)
           (tsdb-do-skeletons nil :format :short))
          (t
           (format *tsdb-io* "~&~%")
           (tsdb-do-skeletons nil))))
        
        ((:create :cre :cr :c)
         (format *tsdb-io* "~&~%")
         (tsdb-do-create argument (or skeleton *tsdb-default-skeleton*))
         (format *tsdb-io* "~&~%"))
        
        ((:process :pro :pr)
         (format *tsdb-io* "~&~%")
         (tsdb-do-process (if (or (null argument)
                                  (member argument (list nil t "")))
                            *tsdb-data*
                            argument)
                          :condition condition :gold gold
                          :run-id run :overwrite t))
        
        ((:vocabulary :voc :vo :v)
         (format *tsdb-io* "~&~%")
         (tsdb-do-vocabulary (if (or (null argument)
                                     (member argument (list nil t "")))
                               *tsdb-data*
                               argument) 
                             :condition condition :load (or load :quiet)))
        
        ((:compress :com :co)
         (format *tsdb-io* "~&~%")
         (tsdb-do-compress
          (if (or (null argument) (member argument (list nil t "")))
            *tsdb-data*
            argument)
          target))
        
        ((:help :hel :he)
         (format *tsdb-io* "~&~%")
         (tsdb-do-help (if argument
                         (intern (subseq (string argument) 0 3) :keyword)
                         :all)))
        
        (t 
         (format 
          *tsdb-io* 
          "~&~%  tsdb(): invalid or ambiguous command `~(~:a~)'; ~
           try `(tsdb :help)'.~%~%"
          action))))))

(defun tsdb-do-set (variable value)
  (format *tsdb-io* "~%")
  (let ((value (case variable
                 ((*tsdb-home* *tsdb-skeleton-directory*) 
                  (namestring (make-pathname :directory value)))
                 (t value))))
    (set variable value)))

(defun tsdb-do-status (name &key (stream *tsdb-io*) (prefix "  "))

  (format stream "~&~%")
  (when (member name (list :all))
    (format 
     stream 
     "~atsdb(1) application: `~a';~%" 
     prefix 
     *tsdb-application*))
  (when (member name (list :all :home))
    (format 
     stream 
     "~atsdb(1) database root: `~a';~%" 
     prefix 
     *tsdb-home*))
  (when (member name (list :all :default))
    (format 
     stream 
     "~adefault test suite database `~a'~%" 
     prefix 
     *tsdb-data*))
  (when (member name (list :all))
    (format 
     stream
     "~askeletons directory: `~a';~%"
     prefix *tsdb-skeleton-directory*)
    (format
     stream
     "~adefault test suite skeleton `~a';~%"
     prefix *tsdb-default-skeleton*))
  (when (member name (list :all))
    (format 
     stream 
     "~awrite run: ~:[no~;yes~]; write parse: ~:[no~;yes~]; ~
      write result: ~:[no~;yes~];~%"
     prefix
     *tsdb-write-run-p* *tsdb-write-parse-p* *tsdb-write-result-p*))
  (when (member name (list :all))
    (format 
     stream 
     "~awrite output: ~:[no~;yes~];~%"
     prefix
     *tsdb-write-output-p*))
  (when (member name (list :all))
    (format 
     stream 
     "~acache database writes ~:[no~;yes~]; flush cache threshold ~a~%"
     prefix
     *tsdb-cache-database-writes-p* *tsdb-flush-cache-threshold*))
  (when (member name (list :all))
    (format 
     stream 
     "~atrees hook: ~:[none~;`~(~a~)()'~];~%~
      ~asemantix hook: ~:[none~;`~(~a~)()'~];~%"
     prefix *tsdb-trees-hook* *tsdb-trees-hook* 
     prefix *tsdb-semantix-hook* *tsdb-semantix-hook*))
  (when (member name (list :all))
    (format 
     stream 
     "~aexhaustive search: ~:[no~;yes~]; ~
     maximal number of edges ~a; item factor: ~a.~%"
     prefix
     *tsdb-exhaustive-p* *tsdb-maximal-number-of-edges* *tsdb-edge-factor*))
  (format stream "~%"))

(defun find-tsdb-directories (&optional (home *tsdb-home*) 
                              &key name pattern trace meter)
  (declare (ignore trace))
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((prefix (length *tsdb-home*))
         (length (length home))
         (absolute (not (equal home *tsdb-home*)))
         (directories (subdirectories home))
         (directories
          (loop
              for directory in directories
              for suffix = (if (and (<= prefix length)
                                    (string=
                                     *tsdb-home* directory :end2 prefix))
                             (subseq directory prefix)
                             directory)
              when (and suffix
                        (not (search "/.svn" suffix))
                        (not (search "/.cvs" suffix))
                        (or (null name) (string= name suffix))
                        (or (null pattern) (ppcre::scan pattern suffix)))
              collect suffix))
         (increment (when (and directories meter)
                      (/ (mduration meter) (+ (length directories) 1))))
         (databases
          (loop
              initially (when increment (meter-advance increment))
              for directory in directories
              for status = (verify-tsdb-directory directory :absolute absolute)
              when status collect status
              when increment do (meter-advance increment))))
    (when meter (meter :value (get-field :end meter)))
    databases))

(defun tsdb-do-list (home &key (stream *tsdb-io*) 
                               (prefix "  ")
                               (format :ascii)
                               (indentation 0)
                               name pattern meter index)
  
  (when stream (format stream "~%"))
  (loop
      with dbs = (sort (find-tsdb-directories home :name name :pattern pattern
                                              :meter (madjust * meter 0.95))
                       #'string< :key #'(lambda (foo) 
                                          (get-field :database foo)))
      with result = nil
      initially
        (case format
          (:html
           (format 
            stream 
            "~v,0t<table>~%~
             ~v,0t  <tr><th>&nbsp;</th>~
             <th align=center>Test Suite Instance</th>~
             <th align=center>Items</th><th align=center>Parses</th>~
             <th align=center>Options</th></tr>~%"
            indentation indentation)))
      for i from 0
      for db in dbs
      do
        (case format
          (:ascii
           (format 
            stream 
            "~a`~a'~@[ (~(~a~))~]: ~a items; ~a parses;~%"
            prefix
            (get-field :database db) (get-field :status db) 
            (get-field :items db) (get-field :parses db)))
          (:tcl
           (format 
            stream 
            "set test_suites(~d) {~s \"~(~a~)\" ~d ~d ~
             ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~]};~%"
            (if index (+ index i) i)
            (get-field :database db) (get-field :status db) 
            (get-field :items db) (get-field :parses db)
            (get-field :resultp db) (get-field :edgep db) (get-field :rulep db) 
            (get-field :treep db) (get-field :scorep db)
            (get-field :fcp db)))
          (:list
           (push (format 
                  nil 
                  "{~s \"~(~a~)\" ~d ~d ~
                   ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~] ~:[0~;1~]}"
                  (get-field :database db) (get-field :status db) 
                  (get-field :items db) (get-field :parses db)
                  (get-field :resultp db) (get-field :edgep db)
                  (get-field :rulep db)
                  (get-field :treep db) (get-field :scorep db)
                  (get-field :fcp db))
                 result))
          (:html
           (format
            stream
            "~v,0t  <tr>~%~
             ~v,0t    <td><input type=radio name=data value=\"~a\"></td>~%~
             ~v,0t    <td align=left>~a</td><td align=center>~a</td>~
             <td align=center>~a</td><td align=center>~a~a~a~a~a~a</td>~%~
             ~v,0t  </tr>~%"
            indentation 
            indentation (get-field :database db)
            indentation (get-field :database db) (get-field :items db)
            (get-field :parses db)
            (if (get-field :resultp db) "r" "-")
            (if (get-field :rulep db) "r" "-")
            (if (get-field :edgep db) "e" "-")
            (if (get-field :treep db) "t" "-")
            (if (get-field :scorep db) "s" "-")
            (if (get-field :fcp db) "f" "-")
            indentation)))
      finally
        (case format
          (:html
           (format stream "~v,0t</table>~%" indentation)))
        (when (and stream dbs) (format stream "~%"))
        (when meter (meter :value (get-field :end meter)))
        (return result)))

(defun find-skeleton (name)
  (let* ((name (if (keywordp name) (string-downcase name) name)))
     (loop
         with agenda = (copy-list *tsdb-skeletons*)
         for skeleton = (pop agenda)
         while skeleton
         when (and (equal (get-field :path skeleton) name)
                   (null (get-field :daughters skeleton)))
         return skeleton
         do (loop
                for daughter in (get-field :daughters skeleton)
                do (push daughter agenda)))))

(defun find-skeleton-directory (skeleton)
  (let* ((path (if (consp skeleton) (get-field :path skeleton) skeleton))
         (folder (when (consp skeleton) (get-field :folder skeleton)))
         (path (dir-append (namestring *tsdb-skeleton-directory*)
                           (cons :relative (append folder (list path))))))
    (namestring path)))

(defun read-skeleton-index (&optional folder)
  (let* ((base (if (pathnamep *tsdb-skeleton-directory*)
                 (pathname-directory *tsdb-skeleton-directory*)
                 (pathname-directory 
                  (make-pathname :directory *tsdb-skeleton-directory*))))
         (base (append base folder))
         (file (make-pathname :directory base :name *tsdb-skeleton-index*))
         (skeletons (when (probe-file file)
                      (with-open-file (stream file :direction :input)
                        (read stream nil nil)))))
    (loop
        for skeleton in skeletons
        for path = (get-field :path skeleton)
        for file = (make-pathname
                    :directory (append base (list path))
                    :name *tsdb-skeleton-index*)
        for daughters
        = (when (probe-file file)
            (read-skeleton-index (append folder (list path))))
        when daughters do
          (nconc skeleton (acons :daughters daughters nil))
        when folder do (nconc skeleton (acons :folder folder nil)))
    (sort
     skeletons
     #'(lambda (foo bar)
         (let* ((fdaughters (get-field :daughters foo))
                (bdaughters (get-field :daughters bar))
                (fpath (get-field :path foo))
                (bpath (get-field :path bar)))
           (or (and fdaughters (not bdaughters))
               (and (or (and fdaughters bdaughters)
                        (and (not fdaughters) (not bdaughters)))
                    fpath bpath (string< fpath bpath))))))))

(defun count-skeletons (&optional (skeletons *tsdb-skeletons*))
  (loop
      for skeleton in skeletons
      for daughters = (get-field :daughters skeleton)
      when daughters sum (count-skeletons daughters)
      else sum 1))

(defun tsdb-do-skeletons (source &key (stream *tsdb-io*) 
                                      (prefix "  ")
                                      (format :ascii)
                                      (index 0) meter)
  
  (when meter (meter :value (get-field :start meter)))
  (setf *tsdb-skeletons* (read-skeleton-index))
  (let* ((directory (if (pathnamep *tsdb-skeleton-directory*)
                      (pathname-directory *tsdb-skeleton-directory*)
                      (pathname-directory 
                       (make-pathname :directory *tsdb-skeleton-directory*))))
         (increment (when (and meter *tsdb-skeletons*)
                      (/ (mduration meter) (length *tsdb-skeletons*)))))
    (labels ((print-skeleton (skeleton prefix)
               (let ((daughters (get-field :daughters skeleton))
                     (content  (get-field :content skeleton))
                     (path (get-field :path skeleton)))
                 (if daughters
                   (loop
                       initially
                         (case format
                           (:ascii
                            (format  
                             stream 
                             "~a~a (`~a'): ~a skeleton~p:~%"
                             prefix content path
                             (length daughters) (length daughters)))
                           (:tcl
                            (format  
                             stream 
                             "set skeletons(~a) {0 \"~{~a~^.~}\" ~s ~s ~d};~%"
                             index (get-field :folder skeleton)
                             path content (length daughters))))
                         (incf index)
                       with indentation = (format nil "~a| " prefix)
                       for daughter in daughters
                       do (print-skeleton daughter indentation))
                   (let* ((suffix
                           (append (get-field :folder skeleton) (list path)))
                          (name (make-pathname 
                                 :directory (append directory suffix)))
                          (name (namestring name))
                          (status (verify-tsdb-directory
                                   name :absolute t :skeletonp t))
                          (items (get-field :items status)))
                     (when status
                       (nconc skeleton (acons :items items nil))
                       (case format
                         (:ascii
                          (format  
                           stream 
                           "~a~a (`~a'): ~a item~p;~%"
                           prefix content path items items))
                         (:tcl
                          (format  
                           stream 
                           "set skeletons(~a) {1 \"~{~a~^.~}\" ~s ~s ~d};~%"
                           index (get-field :folder skeleton)
                           path content items)))
                       (incf index)))))))
      (if source
        (when (find-skeleton (string-downcase source))
          (setf *tsdb-default-skeleton* (string-downcase source)))
        (loop
            for skeleton in *tsdb-skeletons*
            when increment do (meter-advance increment)
            do
              (print-skeleton skeleton prefix))))
    (when (eq format :short)
      (let ((n (count-skeletons)))
        (format
         stream "~a~a skeleton~p in `~a'."
         prefix n n 
         (namestring (make-pathname :directory directory)))))
    (when meter (meter :value (get-field :end meter)))
    (when (and *tsdb-skeletons* (not (eq format :short)))
      (format stream "~%"))))

(defun tsdb-do-phenomena (&key (stream *tsdb-io*) 
                               (prefix "  ")
                               (format :tcl))
  (declare (ignore prefix)
           (special *phenomena*))
  
  (do ((phenomena *phenomena* (rest phenomena))
       (i 0 (+ i 1)))
      ((null phenomena))
    (case format
      (:tcl
       (format 
        stream 
        "set phenomena(~d) ~s;~%"
        i (first phenomena))))))

(defun tsdb-do-cpus (&key (action :list) (format :ascii) host task
                          (stream *tsdb-io*) (prefix "  "))
  (declare (ignore host task))
  
  (case action
    (:list
     (loop 
         for cpu in (sort 
                     (copy-list *pvm-cpus*) #'string< 
                     :key #'(lambda (cpu) 
                              (let ((class (cpu-class cpu)))
                                (symbol-name
                                 (typecase class
                                   (cons (first class))
                                   (t class))))))
                    
         for host = (cpu-host cpu)
         for task = (cpu-task cpu)
         for spawn = (cpu-spawn cpu)
         for options = (cpu-options cpu)
         for class = (cpu-class cpu)
         do
           (case format
             (:ascii
              (format
               stream
               "~&~%~a- `~(~:[~a~;~{~a~^ | ~}~]~)' (`~a') ~
                [~(~:[~a~;~{~a~^ | ~}~]~)]~%~
                ~a  command: `~a';~%~
                ~a  options: `~{~a~^ ~}';~%~%"
               prefix (consp class) class host 
               (consp task) task
               prefix spawn
               prefix options)))))
    (:kill
     (let ((n (length *pvm-clients*)))
       (format stream "~atsdb(): shutting down ~a client~p ..." prefix n n))
     (loop
         for client in *pvm-clients*
         do (kill-client client :prefix prefix))      
     (sleep 1)
     (setf *pvm-clients* nil)
     (pvm_quit)
     (format stream " done; no active PVM clients.~%~%"))
    (:shutdown
     (format stream "~atsdb(): performing full PVM reset ..." prefix)
     (pvm_halt :user (current-user))
     (sleep 1)
     (setf *pvm-clients* nil)
     (format stream " done; no active PVM clients.~%~%"))
    (:active
     (if (null *pvm-clients*)
       (format
        stream
        "~atsdb(): no active PVM clients ~
         (you could always try `(tsdb :help :cpu)').~%~%"
        prefix)
       (loop 
           for client in (sort 
                          (copy-list *pvm-clients*) #'string<
                          :key #'(lambda (client) 
                                   (cpu-host (client-cpu client))))
           for cpu = (client-cpu client)
           for host = (cpu-host cpu)
           for spawn = (cpu-spawn cpu)
           for type = (cpu-task cpu)
           for task = (client-task client)
           for tid = (get-field :tid task)
           for pid = (get-field :pid task)
           for status = (client-status client)
           for protocol = (client-protocol client)
           finally (format t "~%")
           do
             (format
                 stream
                 "~&~%~a- `~(~a~)' (pid: ~a --- tid: <~x> [~d]) ~
                  [~(~:[~a~;~{~a~^ | ~}~]~)]~%~
                  ~a  command: `~a';~%~
                  ~a  status: ~(~a~) --- protocol: ~(~a~);~%"
                 prefix host pid tid tid
                 (consp type) type
                 prefix spawn
                 prefix status protocol))))))

(defun tsdb-do-schema (data &key (stream *tsdb-io*) (format :tcl))
  
  (let* ((schema (read-database-schema data))
         (relations (map 'list #'first schema))
         (fields (map 'list #'rest schema))
         (fields  (reduce #'(lambda (foo bar)
                              (union foo bar 
                                     :key #'first 
                                     :test #'string=))
                          fields))
         (attributes (map 'list #'first fields)))
    (case format
      (:tcl
       (format
        stream
        "set globals(relations) ~a;~%set globals(attributes) ~a;~%"
        (list2tcl relations) (list2tcl attributes))))))

(defun tsdb-do-create (name skeleton-name 
                       &key (stream *tsdb-io*) 
                            create meter)
  (declare (ignore create))
  
  (when meter (meter :value (get-field :start meter)))
  (if (null name)
    (if stream
      (format 
       stream
       "tsdb(): new database name required as argument for `:create'.~%")
      1)
      (let* ((name (if (equal name t) "" name))
             (name (string name))
             (new (if (or (equal name "")
                          (equal (elt name (- (length name) 1)) #\/))
                    (let ((date (current-time :long :usa)))
                      (find-tsdb-directory (format nil "~a~a" name date)))
                    (find-tsdb-directory name)))
             (skeleton (and skeleton-name (find-skeleton skeleton-name)))
             (old (when skeleton (find-skeleton-directory skeleton))))
        (when (probe-file new)
          (when stream
            (format 
             stream
             "tsdb(): database `~a' already exists.~%"
             (string-trim '(#\/) (string-strip *tsdb-home* new))))
          (return-from tsdb-do-create 5))
        (mkdir new :parentp t)
        (cond
         ((null skeleton-name)
          ;;
          ;; create an emtpy (`null') skeleton, typically for capturing mode
          ;;
          (when meter (meter :value (get-field :start meter)))
          (let ((relations (make-pathname 
                            :directory (namestring *tsdb-skeleton-directory*)
                            :name *tsdb-relations-skeleton*))
                (target (make-pathname :directory new :name "relations")))
            (cp relations target))
          (let ((imeter (madjust / meter 2)))
            (when imeter (meter :value (get-field :end imeter)))
            (select "i-id" :integer "item" nil new :absolute t)
            (when (verify-tsdb-directory new :absolute t)
              (setf *tsdb-data* 
                (string-trim '(#\/) (string-strip *tsdb-home* new)))
              (when meter (meter :value (get-field :end meter)))
              (if stream
                (format 
                 stream
                 "tsdb(): `~a' created as new default test suite.~%"
                 *tsdb-data*)
                0))))
         ((null skeleton)
          (if stream
            (format 
             stream
             "tsdb(): unknown test suite skeleton `~a'.~%"
             skeleton-name)
            2))
         ((not (probe-file old))
          (if stream
            (format 
             stream
             "tsdb(): no skeleton directory `~a'.~%"
             old)
            4))
         (t
          (when meter (meter :value (get-field :start meter)))
          (let ((status
                 (ignore-errors
                  (loop
                      with target = (pathname new)
                      for file in (directory 
                                   (make-pathname :directory old :name :wild))
                      for name = (pathname-name file) 
                      unless (member
                              name
                              '("CVS" "LVS" ".svn")
                              :test #'string=)
                      do (cp file (merge-pathnames target file))
                      finally (return t)))))
            (if status
              (let ((imeter (madjust / meter 2)))
                (when imeter (meter :value (get-field :end imeter)))
                (when (select "i-id" :integer "item" nil new :absolute t)
                  (setf *tsdb-data* 
                    (string-trim '(#\/) (string-strip *tsdb-home* new)))
                  (when meter (meter :value (get-field :end meter)))
                  (if stream
                    (format 
                     stream
                     "tsdb(): `~a' created as new default test suite.~%"
                     *tsdb-data*)
                    0)))
              (if stream
                (format 
                 stream
                 "tsdb(): mysterious problems creating database `~a'.~%"
                 (string-trim '(#\/) (string-strip *tsdb-home* new)))
                42))))))))

(defun tsdb-do-compress (source target &key (stream *tsdb-io*))
  (let* ((source (find-tsdb-directory source))
         (cpp target)
         (target (if target (find-tsdb-directory target) source))
         (target (ignore-errors (mkdir target :parentp t))))
    (if (pathnamep target)
      (loop
          with zipper = "gzip -9 -f"
          for old in (directory (make-pathname :directory source :name :wild))
          for name = (pathname-name old)
          for type = (pathname-type old)
          for new = (merge-pathnames target old)
          unless (directoryp old)
          do 
            (when cpp (cp old new))
            (unless (or (equal name "relations")
                        (zerop (file-size old))
                        (equal type "gz"))
              (format
               stream
               "tsdb(): compressing `~a'.~%"
               (string-strip *tsdb-home* (namestring new)))
              (run-process 
               (format nil "~a '~a'" zipper (namestring new))
               :wait t)))
      (format
       stream
       "tsdb(): invalid target directory `~a'.~%"
       target))))

(defun tsdb-do-help (command)
  
  (when (equal command :all)
    (format
     *tsdb-io*
     "  The `tsdb' command encapsulates all interaction with the test suite
  database tsdb(1) and profiling machinery; it has the general synopsis:

    tsdb _action_ [ _argument_ ] [ :key _option_ ]+

  where _action_ can be one of the following:~%~%"))
  
  (when (member command (list :all :info :inf))
    (format
     *tsdb-io*
     "    - :info

        display summary of tsdb(1) global parameters and options; see below
        for commands that query and set individual parameters;~%~%"))
  
  (when (member command (list :all :home :hom :ho))
    (format
     *tsdb-io*
     "    - :home [ _string_ ]

        query or set tsdb(1) databases root directory;~%~%"))
  
  (when (member command (list :all :default :def :de :d))
    (format
     *tsdb-io*
     "    - :default [ _string_ ]

        query or set tsdb(1) default test suite database;~%~%"))
  
  (when (member command (list :all :initialize :ini))
    (format
     *tsdb-io*
     "    - :initialize [ _file_ ] [ :load _string_ ] [ :run _bool_ ]

        reload _file_ argument or `.tsdbrc' (from user home directory) and 
        rescan the test suite skeleton directory; optional :cache argument,
        loads tsdb(1) profile cache for databases matching _string_; :run t
        background the cache load; finally, display resulting status;~
      ~%~%"))
  
  (when (member command (list :all :list :lis :li :l))
    (format
     *tsdb-io*
     "    - :list [ _string_ ]

        search directory _string_ (tsdb(1) home by default) for well-formed
        tsdb(1) databases; display database status and size;~%~%"))

  (when (member command (list :all :skeletons :ske :sk :s))
    (format
     *tsdb-io*
     "    - :skeletons [ _string_ ]

        query or set tsdb(1) skeleton root directory;;~%~%"))

  (when (member command (list :all :create :cre :cr))
    (format
     *tsdb-io*
     "    - :create [ _string_ ] [ :skeleton _string_ ]

        create new database called _string_ (as a subdirectory in the tsdb(1)
        home directory) from the skeleton database (or the default skeleton if
        the :skeleton option ist omitted; if the name for the new database has
        a trailing `/' and is an existing directory, a new name will be
        generated from the current date;)~%~%"))

  (when (member command (list :all :vocabulary :voc :vo :v))
    (format
     *tsdb-io*
     "    - :vocabulary [_string_ ] [ :condition _string_ ] [ :load _keyword_ ]

        extract alphabetically sorted list of vocabulary used in database 
        _string_ (use :condition options as with :process below); the :load 
        option determines whether lexical entries are expanded as needed and
        whether the loading is quiet or verbose; _keyword_ can be one of
        :off, :quiet (default), or :verbose;~%~%"))

  (when (member command (list :all :process :pro :pr))
    (format
     *tsdb-io*
     "    - :process [ _string_ ] [ :condition _string_ ] [ :run _integer_ ]

        select test data from database called _string_ (or the default db);
        use optional (T)SQL condition to constrain the selection (e.g. use
        `i-wf = 1 && i-length >= 10' for longish grammatical sentences) and
        number the test run _integer_ (a new unique identifier will be used
        without the :run option);~%~%"))
  
  (when (member command (list :all :podium :pod :po))
    (format
     *tsdb-io*
     "    - :podium [ :cache _string_ ]

        start up tsdb(1) podium (the graphical user interface); see :initialize
        above for the optional :cache argument;~%~%"))

  (when (member command (list :all :cpus :cpu :cp))
    (format
     *tsdb-io*
     "    - :cpus [ _name_ ] [ :file _string_ ] [ :reset _bool_ ] ~
                  [ :count _n_ ]

        list or activate [incr tsdb()] cpus; _keyword_ is a class name (used
        in the cpu definition) that identifies which client(s) to start; write
        client output to file _string_ (defaults to `/tmp/pvm.debug.user' --- 
        where `user' is the active account name; `t' as the :file argument
        means client output goes to standard out); :reset defaults to `t' and
        shuts down all existing cpus before initialization; :count defaults to
        `1' and determines the number of instances of the cpu to be started;
        no _keyword_ argument (or `:active') lists currently active clients;
        `:list' provides a summary of all available client definitions; and
        `:kill' shuts down all existing clients.~%~%")) 
  
  (when (member command (list :all :help :hel :he))
    (format
     *tsdb-io*
     "    - :help [ _keyword_ ]

        display help on tsdb(1) command _keyword_ or all commands.~%~%"))
  
  (when (equal command :all)
    (format
     *tsdb-io*
     "  _action_ may be abbreviated as long as the prefix is unambiguous; any of
  the options can only be specified when all optional arguments are present;
  option keys cannot be abbreviated.~%~%")))


