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

(defparameter *tsdb-cache-connections-p* nil)

(defparameter *tsdb-connection-expiry* 200)

(defparameter *tsdb-maximal-number-of-connections* 20)

(defparameter %tsdb-connection-cache% nil)

(defstruct connection
  data absolute path unique ro
  stream pid
  (count 0))

(defun cache-connection (data &key absolute unique ro verbose)

  (declare (ignore verbose))
  #+:debug
  (when absolute (break))
  (loop
      with path = (if absolute (namestring data) (find-tsdb-directory data))
      for connection in (copy-list %tsdb-connection-cache%)
      when (and (equal (connection-path connection) path)
                #+:null
                (equal (connection-data connection) data)
                #+:null
                (equal (connection-absolute connection) absolute)
                (equal (connection-unique connection) unique)
                (equal (connection-ro connection) ro))
      do
        (incf (connection-count connection))
        (if (and (numberp *tsdb-connection-expiry*)
                 (>= (connection-count connection) *tsdb-connection-expiry*))
          (close-connection connection :verbose t)
          (return connection))
      finally
        (let ((command (format
                        nil 
                        "~a -home=~a -uniquely-project=~:[off~;on~] ~
                         -quiet~:[~; -read-only~] -eof=\":eof\" ~
                         -string-escape=lisp -pager=null -max-results=0"
                        *tsdb-application* path unique ro)))
          (multiple-value-bind (stream foo pid)
            (run-process
             command :wait nil
             :output :stream :input :stream
             :error-output nil)
            (declare (ignore foo))
            (push (make-connection :data data :absolute absolute :path path
                                   :unique unique :ro ro
                                   :stream stream :pid pid)
                  %tsdb-connection-cache%)))
        (return (first %tsdb-connection-cache%))))

(defun close-connections (&key data absolute (verbose t))
  (loop
      with path = (when data
                    (if absolute (namestring data) (find-tsdb-directory data)))
      for connection in %tsdb-connection-cache%
      when (or (null path) (equal path (connection-path connection))) do
        (close-connection connection :deletep nil :verbose verbose)
      else collect connection into result
      finally (setf %tsdb-connection-cache% result)))

(defun close-connection (connection &key (deletep t) (verbose t))
  (ignore-errors
   (close (connection-stream connection))
   #+:allegro
   (sys:os-wait nil (connection-pid connection)))
  (when deletep
    (setf %tsdb-connection-cache%
      (delete connection %tsdb-connection-cache%)))
  (when verbose
    (format
     *tsdb-io*
     "close-connection(): `~a' expiry.~%"
     (connection-data connection))))

(defun data-hook (old new)
  (when *tsdb-data-hook* (call-hook *tsdb-data-hook* old new)))

(defun gold-hook (old new)
  (when *tsdb-gold-hook* (call-hook *tsdb-gold-hook* old new)))

(defun initialize-tsdb (&optional cache
                        &key rc action background name pattern)
  
  (declare (special *tsdb-podium-home* *tsdb-podium* *tsdb-wish-application*
                    *statistics-readers* *statistics-browsers* 
                    *statistics-predicates*))
  
  (unless (and *tsdb-initialized-p* (null action) (null rc))
    (let* ((*tsdb-initialized-p* t)
           (tsdbrc (or rc (dir-and-name (user-homedir-pathname) ".tsdbrc")))
           (index (make-pathname :directory  *tsdb-skeleton-directory*
                                 :name *tsdb-skeleton-index*)))
      (when (and (or (null action) (member action '(:paths :all))))
        (setf *tsdb-application*
          (format
           nil "exec ~a"
           (namestring (make-pathname
                        :directory (pathname-directory make::bin-dir)
                        :name "tsdb"))))
        (setf *tsdb-home* 
          (namestring (dir-append
                       (get-sources-dir "tsdb") '(:relative "tsdb" "home"))))
        (setf *tsdb-skeleton-directory* 
          (namestring (dir-append 
                       (get-sources-dir "tsdb")
                       '(:relative "tsdb" "skeletons" "english"))))
        (setf *tsdb-podium-home*
          (namestring (dir-append
                       (get-sources-dir "tsdb") '(:relative "tsdb" "tcl"))))
        (setf *tsdb-podium*
          (namestring (make-pathname 
                       :directory *tsdb-podium-home*
                       :name "podium.tcl")))
        (setf *tsdb-wish-application*
          (format
           nil 
           "exec ~a"
           (namestring (make-pathname 
                        :directory (pathname-directory make::bin-dir)
                        :name "swish++")))))
      (when (and (or (null action) (member action '(:tsdbrc :all)))
                 (probe-file tsdbrc))
        (load tsdbrc))
      (let* ((home (if (stringp *tsdb-home*)
                     (make-pathname :directory *tsdb-home*)
                     *tsdb-home*)))
        (setf *tsdb-home* (when home (namestring home))))
      (when (and (or (null action) (member action '(:skeletons :all)))
                 (probe-file index))
        (setf *tsdb-skeletons* (with-open-file (stream index 
                                                :direction :input)
                                 (read stream nil nil))))
      (when (find-package :mrs)
        (when (null (gethash :mrs *statistics-readers*))
          (setf (gethash :mrs *statistics-readers*) 
            "mrs::read-mrs-from-string"))
        (when (null (gethash :mrs *statistics-browsers*))
          (setf (gethash :mrs *statistics-browsers*) 
            (if (and (find-package :mt) (find-symbol "BROWSE-MRSS" :mt))
              "mt::browse-mrss"
              "mrs::browse-mrs")))
        (setf (gethash :mrs *statistics-predicates*) "mrs::safe-mrs-unequalp"))
      (when (and (or (null action) (member action '(:cache :all))) cache)
        (load-cache :background background :name name :pattern pattern))))
  (unless action (setf *tsdb-initialized-p* t)))

(defun call-tsdb (query language
                  &key (format :string)
                       cache absolute unique quiet ro)

  (if *tsdb-server-mode-p*
    #+:server (call-tsdbd query language) #-:server nil
    (if cache
      (cache-query query language cache)
      (let* ((user (current-user))
             (file (format
                    nil "/tmp/.tsdb.io.~a.~a.~a"
                    user (current-pid) (string-downcase (string (gensym "")))))
             (data (if absolute 
                     (namestring language) 
                     (find-tsdb-directory language)))
             (command (format
                       nil 
                       "~a -home=~a -uniquely-project=~:[off~;on~]~
                       ~:[~; -quiet~]~:[~; -read-only~] -eof=\":eof\" ~
                        -string-escape=lisp -pager=null -max-results=0"
                       *tsdb-application* data unique quiet ro))
             (command (format
                       nil "~a -query='do \"~a\"'"
                       command file))
             (query (string-trim '(#\Space #\Tab #\Newline) query))
             (query (if (equal (elt query (- (length query) 1)) #\.)
                      (subseq query 0 (- (length query) 1))
                      query))
             (output (when (eq format :file)
                       (format
                        nil "/tmp/.tsdb.data.~a.~a.~a"
                        user (current-pid) 
                        (string-downcase (string (gensym ""))))))
             (redirection 
              (if output (concatenate 'string " > \"" output "\"") ""))
             (query (concatenate 'string query redirection "."))
             (connection (when (and *tsdb-cache-connections-p* 
                                    (eq format :lisp))
                           (cache-connection language 
                                             :absolute absolute
                                             :unique unique :ro ro))))

        (when (and output (probe-file output)) (delete-file output))
        (with-open-file (stream file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
          (format stream "~a~%" query)
          (force-output stream))
        (multiple-value-bind (stream foo pid)
          (if connection
            (let* ((command (format nil "do \"~a\".~%" file))
                   (stream (connection-stream connection)))
              (format stream command)
              (force-output stream)
	      ;;
	      ;; _fix_me_
	      ;; after a fair amount of tinkering, it seems reading over this
	      ;; line is required on Linux at least to skip the echo of what we
	      ;; just wrote to the stream; although we are running in --quiet
	      ;; mode, the termios magic apparently fails to have the desired
	      ;; effect.  but it does on MacOS X, as it should.  i should take
	      ;; the time to look into the Linux behavior one day soon.
              ;;                                                 (4-nov-05; oe)
              ;; hmm, after recompiling the tsdb(1) binary on `mt' (RHEL3), i
              ;; now get the same effect on Linux hosts (as i should); disable
              ;; the mysterious read-line() globally then.      (12-dec-05; oe)
              ;; 
              #+:null
              (read-line stream)
              (values stream nil nil))
            (run-process
             command :wait nil
             :output (if output "/dev/null" :stream)
             :if-output-exists :append
             :input "/dev/null" :error-output nil))
          (declare (ignore foo #-:allegro pid))
          (if output
            ;;
            ;; we are in a rather intricate situation here: possibly the child
            ;; has not yet been created (os-wait() returns nil nil) or is still
            ;; running (os-wait() returns nil .pid.); then, in turn, something
            ;; may have gone wrong in the process creation; in this case we
            ;; better not block eternally.  without os-specific functions (like
            ;; os-wait in Allegro CL) there is no way to handle this situation.
            ;;
            #+:allegro
            (multiple-value-bind (status epid) 
                (sys:os-wait t pid)
              (when (null status)
                (loop
                    with i = 0
                    do
                      (sleep 0.5)
                      (multiple-value-setq (status epid) (sys:os-wait t pid))
                      (when (null epid) (incf i))
                    while (and (< i 100) (not status))
                    finally
                      (when (>= i 100)
                        (format
                         *tsdb-io*
                         "call-tsdb(): failed to salvage tsdb(1) child ~d~%"
                         pid))))
              (when (probe-file output)
                (unless *tsdb-debug-mode-p*
                  (delete-file file))
                output))
            #-:allegro
            (error "call-tsdb(): cannot process :file output format; ~
                    see `tsdb.lisp'.")
            (case format
              (:string
               (let ((result (make-array
                              32768
                              :element-type 'character
                              :adjustable t             
                              :fill-pointer 0)))
                 (loop
                     for c = (read-char stream nil :eof)
                     until (eq c :eof)
                     do
                       (vector-push-extend c result 32768))
                 (close stream)
                 #+:allegro (sys:os-wait nil pid)
                 (unless *tsdb-debug-mode-p*
                   (delete-file file))
                 result))
              (:lisp
               (let ((result (loop
                                 for form = (read stream nil :eof)
                                 until (eq form :eof)
                                 collect form)))
                 (when pid 
                   (close stream)
                   #+:allegro
                   (loop until (sys:os-wait nil pid)))
                 (unless *tsdb-debug-mode-p* (delete-file file))
                 result)))))))))

(defun create-cache (data &key (protocol :cooked) (verbose t) schema allp)

  ;;
  ;; _fix_me_
  ;; make sure to not create a write cache (and thus no new files) on read-only
  ;; profiles: add call to verify-tsdb-directory() here and inspect status.
  ;;                                                           (27-oct-03; oe)
  (let ((status (verify-tsdb-directory data)))
    (when (eq (get-field :status status) :ro)
      (when verbose
        (format 
         *tsdb-io*
         "~&create-cache(): `~a' is read-only.~%"
         data))
      (return-from create-cache
        (pairlis '(:database :count :protocol) (list data 0 :ro)))))
  (case protocol
    (:raw
     (loop 
         with cache = (pairlis '(:database :count :protocol)
                               (list data 0 protocol))
         with path = (find-tsdb-directory data)
         with schema = (or schema (read-database-schema data))
         initially (when verbose
                     (format 
                      *tsdb-io*
                      "~&create-cache(): write-through mode for `~a'.~%"
                      data)
                     (force-output *tsdb-io*))
         for file in (if allp
                       (loop 
                           for relation in schema
                           collect (first relation))
                       *tsdb-profile-files*)
         for key = (intern (string-upcase file) :keyword)
         when (assoc file schema :test #'string=) do
           (let ((stream (open (make-pathname :directory path :name file) 
                               :direction :output 
                               :if-exists :append 
                               :if-does-not-exist :create)))
             (push (cons key stream) cache))
         finally (return cache)))
    (:cooked
     (let* ((user (current-user))
            (file (format
                   nil "/tmp/.tsdb.cache.~a.~a.~a"
                   user (current-pid) (string-downcase (string (gensym "")))))
            (stream (open file 
                          :direction :output 
                          :if-exists :supersede :if-does-not-exist :create)))
       (when stream
         (format stream "set implicit-commit \"exit\".~%")
         (when verbose
           (format 
            *tsdb-io*
            "~&create-cache(): tsdb(1) write cache in `~a'.~%"
            file)
           (force-output *tsdb-io*)))
       (pairlis '(:database :file :stream :count :protocol)
                (list data file stream 0 protocol))))))

(defun cache-query (query data cache)
  (let* ((database (get-field :database cache))
         (stream (get-field :stream cache))
         (query (string-trim '(#\Space #\Tab #\Newline) query))
         (query (if (equal (elt query (- (length query) 1)) #\.)
                  query
                  (concatenate 'string query "."))))
    (if (string-equal data database)
      (when stream 
        (format stream "~a~%" query)
        (force-output stream)
        (incf (get-field :count cache))
        (when (>= (get-field :count cache) *tsdb-flush-cache-threshold*)
          (flush-cache cache :verbose *tsdb-verbose-cache-flush-p*)
          (setf (get-field :count cache) 0)
          (let ((stream 
                 (open (get-field :file cache)
                       :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)))
            (format stream "set implicit-commit \"exit\".~%")
            (setf (get-field :stream cache) stream))))
      (format
       *tsdb-io*
       "~&cache-query() ignoring query to `~a' (on `~a' cache).~%"
       data database))))

(defun flush-cache (cache
                    &key sort (verbose t) relations exceptions)
  (let ((database (get-field :database cache))
        (protocol (get-field :protocol cache)))
    (when verbose
      (format 
       *tsdb-io*
       "~&flush-cache(): flushing `~a' cache ..."
       database)
      (force-output *tsdb-io*))
    (case protocol
      (:raw
       (loop
           for file in *tsdb-profile-files*
           for key = (intern (string-upcase file) :keyword)
           for stream = (get-field key cache)
           when (and stream
                     (or (null relations) (member key relations))
                     (or (null exceptions) (not (member key exceptions))))
           do
             (force-output stream)
             (close stream)))
      (:cooked
       (let ((file (get-field :file cache))
             (stream (get-field :stream cache)))
         (format stream "~&commit.~%")
         (force-output stream)
         (close stream)
         (let* ((query (format nil "do \"~a\"" file)))
           (call-tsdb query database))
         (unless *tsdb-debug-mode-p*
           (delete-file file)))))
    (when (and sort (not (eq protocol :ro)))
      ;;
      ;; for improved tsdb(1) efficiency, make an attempt at sorting the files.
      ;;
      (loop
          with path = (find-tsdb-directory database)
          for file in '("parse" "result" "score")
          for source = (namestring (make-pathname :directory path :name file))
          for target = (format nil "~a-" source)
          when (probe-file source) do
            (when (probe-file target) (delete-file target))
            (run-process (format nil "exec mv -f '~a' '~a'" source target)
                         :output "/dev/null" :if-output-exists :append
                         :input "/dev/null" :error-output "/dev/null"
                         :if-error-output-exists :append) 
            (run-process (format nil "exec sort -n '~a'" target)
                         :output source :if-output-exists :supersede
                         :input "/dev/null" :error-output "/dev/null"
                         :if-error-output-exists :append)
            (when (probe-file target) (delete-file target))))
    (when verbose
      (format *tsdb-io* " done.~%")
      (force-output *tsdb-io*))))

(defun largest-result-key (&optional (data *tsdb-data*)
                           &key (verbose t))
  (let* ((query "select c-id from csli")
         (result (call-tsdb query data)))
    (with-input-from-string (stream result)
      (do ((c-ids nil c-ids)
           (c-id (read stream nil :eof) (read stream nil :eof)))
        ((equal c-id :eof)
         (let ((c-id (if c-ids (apply #'max c-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-result-key(): largest `c-id' is ~a.~%"
              c-id)
             (force-output *tsdb-io*))
           c-id))
        (when (integerp c-id) (push c-id c-ids))))))

(defun largest-i-id (&optional (data *tsdb-data*)
                     &key (verbose t))     
  (let* ((query "select i-id from item")
         (result (call-tsdb query data))
         (i-id 
          (with-input-from-string (stream result)
            (loop
                for i-id = (read stream nil :eof)
                until (equal i-id :eof)
                maximize i-id))))
    (when verbose
      (format *tsdb-io* "~&largest-i-id(): largest `i-id' is ~a.~%" i-id))
    i-id))

(defun largest-run-id (&optional (data *tsdb-data*)
                       &key (verbose t))     
  (let* ((query "select run-id from run")
         (result (call-tsdb query data)))
    (with-input-from-string (stream result)
      (do ((run-ids nil run-ids)
           (run-id (read stream nil :eof) (read stream nil :eof)))
        ((equal run-id :eof)
         (let ((run-id (if run-ids (apply #'max run-ids) 0)))
           (when verbose
             (format
              *tsdb-io* 
              "~&largest-run-id(): largest `run-id' is ~a.~%"
              run-id))
           run-id))
        (when (integerp run-id) (push run-id run-ids))))))

(defun largest-parse-id (run-id &optional (language *tsdb-data*)
                         &key (verbose t))
  (let* ((data (select "parse-id" :integer "parse"
                       (format nil "run-id = ~d" run-id) language))
         (parse-ids (map 'list #'(lambda (foo) 
                                   (get-field :parse-id foo)) data))
         (parse-id (if parse-ids (apply #'max parse-ids) 0)))
    (when verbose
      (format
       *tsdb-io* 
       "~&largest-parse-id(): largest `parse-id' (for `run' ~d) is ~a.~%"
       run-id parse-id))
    parse-id))

(defun largest-tree-id (&optional (language *tsdb-data*)
                        &key (verbose t))
  (let* ((data (select "tree-id" :integer "tree" nil language))
         (tree-id (loop
                      for record in data
                      maximize (get-field+ :tree-id record 0))))
    (when verbose
      (format
       *tsdb-io* 
       "~&largest-tree-id(): largest `tree-id' is ~a.~%"
       tree-id))
    tree-id))

(defun select-derivations (language &key item)
  (let* ((condition (when item (format nil "i-id = ~d" item)))
         (derivations 
          (select "derivation" :string "result" condition language))
         (derivations
          (and derivations
               (map 'list #'(lambda (tuple) (get-field :derivation tuple))
                    derivations))))
    (and derivations
         (map 'list (lambda (string) (read-from-string string nil))
              derivations))))

(defun select-phenomena (data &key (format :tcl) (stream *tsdb-io*))
  (declare (ignore format stream))
  (let* ((phenomena (select '("p-id" "p-name")
                            '(:integer :string)
                            "phenomenon"
                            nil data)))
    (sort phenomena #'string< 
          :key #'(lambda (tuple)
                   (get-field :p-name tuple)))))

(defun analyze-phenomena (phenomena &optional prefix)
  (loop
      with result
      for tuple = (pop phenomena)
      for name = (get-field :p-name tuple)
      for next = (get-field :p-name (first phenomena))
      for common = (and next (mismatch name next))
      do
        (cond
         ((and common (null prefix))
          (multiple-value-bind (analysis residuum)
              (analyze-phenomena phenomena common)
            (push (cons common analysis) result)
            (setf phenomena residuum)))
         ((and common prefix (< (mismatch common prefix) (length prefix))))
         (t
          (push tuple result)))
      finally (return (nreverse result))))
(defun select-item-sets (data)
  (let ((sets (select '("i-id" "s-id") '(:integer :integer)
                      "item-set" nil data :sort :s-id)))
    (when sets
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
          finally (return (nconc groups (list (sort group #'<))))))))

(defun read-database-schema (data &key absolute)
  
  (let* ((relations (call-tsdb "info relations" data :ro t :absolute absolute))
         schema)
    (with-input-from-string (stream relations)
      (do* ((line (read-line stream nil nil) (read-line stream nil nil)))
          ((null line) schema)
        (let* ((line (string-trim '(#\Space #\Tab) line))
               (colon (position #\: line)))
          (unless (or (string= line "") (null colon))
            (let ((relation (subseq line 0 colon))
                  fields)
              (do* ((line (read-line stream nil nil) 
                          (read-line stream nil nil)))
                  ((or (null line) 
                       (string= (string-trim '(#\Space #\Tab #\Newline) line) 
                                "")))
                (let* ((line (string-trim '(#\Space #\Tab) line))
                       (space (or (position #\Space line) (length line))))
                  (unless (or (string= line "") (null space))
                    (let* ((name (subseq line 0 space))
                           (flags (subseq line space))
                           (type (cond 
                                  ((search "integer" flags) :integer)
                                  ((search "string" flags) :string)
                                  ((search "date" flags) :date)
                                  ((search "position" flags) :position)))
                           (field 
                            (append (list name type) 
                                    (and (search "key" flags) '(:key))
                                    (and (search "unique" flags) '(:unique)))))
                      (push field fields)))))
              (push (cons relation (nreverse fields)) schema))))))))

(defun concatenate-profiles (profiles target
                             &key (condition *statistics-select-condition*))
  (declare (special *statistics-select-condition*))
  
  ;;
  ;; _fix_me_
  ;; make an attempt to sort source profiles into ascending order, such that
  ;; the output has a chance of coming out in ascending sorted order (assuming
  ;; each input profile was properly sorted); our poor little tsdb(1) engine
  ;; is so much more efficient with sorted input files.       (15-feb-03; oe)
  ;;
  (when (loop
            with base = (profile-granularity (first profiles))
            for profile in (rest profiles)
            for granularity = (profile-granularity profile)
            thereis (not (= base granularity)))
    (return-from concatenate-profiles))
  
  (let ((target (find-tsdb-directory target)))
    #+:allegro
    (when (probe-file target) (excl:delete-directory-and-files target))
    (ignore-errors (mkdir target))
    (let ((source (find-tsdb-directory (first profiles))))
      (cp (make-pathname :directory source :name "relations")
          (make-pathname :directory target :name "relations"))))
  
  (loop
      with *print-circle* = nil 
      with *print-level* = nil 
      with *print-length* = nil
      with ofs = *tsdb-ofs*
      with gc = (install-gc-strategy 
                 nil :tenure *tsdb-tenure-p* :burst t :verbose t)
      with schema = (read-database-schema (first profiles))
      with cache = (create-cache 
                    target :protocol :raw :verbose t :schema schema :allp t)
      with special = '(:i-id :run-id :parse-id)
      for profile in profiles
      for i from 1000000 by 10000
      for offset = (cond
                    ((search "vm6" profile) 60000)
                    ((search "vm13" profile) 130000)
                    ((search "vm31" profile) 310000)
                    ((search "vm32" profile) 320000)
                    (t i))
      for schema = (read-database-schema profile)
      do
        (loop
            for (relation . structure) in schema
            for key = (first (find :key structure :key #'third))
            for fields = (loop 
                             for foo in structure collect (first foo))
            for types = (loop 
                            for foo in structure collect (second foo))
            for data = (select 
                        fields types relation condition profile
                        :sort (and key (intern (string-upcase key) :keyword)))
            do
              (loop
                  with foo = (intern (string-upcase relation) :keyword)
                  with stream = (get-field foo cache)
                  for tuple in data
                  do
                    (loop
                        with firstp = t
                        for field in fields
                        for type in types
                        for key = (intern (string-upcase field) :keyword)
                        for value = (if (smember key special)
                                      (+ (get-field key tuple) offset)
                                      (get-field key tuple))
                        when firstp do 
                          (setf firstp nil) 
                        else do 
                           (write-char ofs stream)
                        when (smember type '(:string :date)) do
                          (let ((value (normalize-string 
                                        value :escape t :normalize nil)))
                            (write-string value stream))
                        else do 
                          (write value :stream stream))
                    (terpri stream)
                    (force-output stream)
                    (incf (get-field :count cache))))
        (purge-profile-cache profile)
      finally 
        (flush-cache cache :verbose t)
        (restore-gc-strategy gc)))

(defun write-run (result language &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-run(): write attempt on read-only cache; see `tsdb.lisp'"))
  
  (when *tsdb-write-run-p*
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (*print-escape* t)
           (rawp (and cache (eq (get-field :protocol cache) :raw)))
           (run-id (get-field+ :run-id result -1))
           (comment 
            (normalize-string (get-field :comment result) :escape rawp))
           (platform 
            (normalize-string (get-field :platform result) :escape rawp))
           (tsdb (normalize-string (get-field :tsdb result) :escape rawp))
           (application 
            (normalize-string (get-field :application result) :escape rawp))
           (environment 
            (normalize-string (get-field :environment result) :escape rawp))
           (grammar 
            (normalize-string (get-field :grammar result) :escape rawp))
           (avms (get-field+ :avms result -1))
           (sorts (get-field+ :sorts result -1))
           (templates (get-field+ :templates result -1))
           (lexicon (get-field+ :lexicon result -1))
           (lrules (get-field+ :lrules result -1))
           (rules (get-field+ :rules result -1))
           (user (normalize-string (get-field+ :user result "") :escape rawp))
           (host (normalize-string (get-field+ :host result "") :escape rawp))
           (os (normalize-string (get-field+ :os result "") :escape rawp))
           (start (get-field :start result))
           (end (get-field :end result))
           (items (get-field+ :items result -1))
           (status 
            (normalize-string (get-field+ :status result "") :escape rawp)))
      (if rawp
        (let ((stream (get-field :run cache))
              (ofs *tsdb-ofs*))
          (write run-id :stream stream) (write-char ofs stream)
          (write-string comment stream) (write-char ofs stream)
          (write-string platform stream) (write-char ofs stream)
          (write-string tsdb stream) (write-char ofs stream)
          (write-string application stream) (write-char ofs stream)
          (write-string environment stream) (write-char ofs stream)
          (write-string grammar stream) (write-char ofs stream)
          (write avms :stream stream) (write-char ofs stream)
          (write sorts :stream stream) (write-char ofs stream)
          (write templates :stream stream) (write-char ofs stream)
          (write lexicon :stream stream) (write-char ofs stream)
          (write lrules :stream stream) (write-char ofs stream)
          (write rules :stream stream) (write-char ofs stream)
          (write-string user stream) (write-char ofs stream)
          (write-string host stream) (write-char ofs stream)
          (write-string os stream) (write-char ofs stream)
          (write-string start stream) (write-char ofs stream)
          (write-string end stream) (write-char ofs stream)
          (write items :stream stream) (write-char ofs stream)
          (write-string status stream) 
          (terpri stream)
          (force-output stream)
          (incf (get-field :count cache)))
        (let* ((query
                (format
                 nil
                 "insert into run values ~
                  ~d ~s ~
                  ~s ~s ~s ~s ~s ~
                  ~d ~d ~d ~d ~d ~d ~
                  ~s ~s ~s ~a ~a ~d ~s"
                 run-id comment 
                 platform tsdb application environment grammar
                 avms sorts templates lexicon lrules rules
                 user host os start end items status)))
          (call-tsdb query language :cache cache))))))

(defun write-parse (result language &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-parse(): write attempt on read-only cache; see `tsdb.lisp'"))
  
  (when *tsdb-write-parse-p*
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (*print-escape* t)
           (rawp (and cache (eq (get-field :protocol cache) :raw)))
           (parse-id (get-field+ :parse-id result -1))
           (run-id (get-field+ :run-id result -1))
           (i-id (get-field+ :i-id result -1))
           (readings (get-field+ :readings result -1))
           (first (round (get-field+ :first result -1)))
           (total (round (get-field+ :total result -1)))
           (tcpu (round (get-field+ :tcpu result -1)))
           (tgc (round (get-field+ :tgc result -1)))
           (treal (round (get-field+ :treal result -1)))
           (words (get-field+ :words result -1))
           (l-stasks (get-field+ :l-stasks result -1))
           (p-ctasks (get-field+ :p-ctasks result -1))
           (p-ftasks (get-field+ :p-ftasks result -1))
           (p-etasks (get-field+ :p-etasks result -1))
           (p-stasks (get-field+ :p-stasks result -1))
           (aedges (get-field+ :aedges result -1))
           (pedges (get-field+ :pedges result -1))
           (raedges (get-field+ :raedges result -1))
           (rpedges (get-field+ :rpedges result -1))
           (unifications (get-field+ :unifications result -1))
           (copies (get-field+ :copies result -1))
           (conses (get-field+ :conses result -1))
           (symbols (get-field+ :symbols result -1))
           (others (get-field+ :others result -1))
           (gcs (get-field+ :gcs result -1))
           (i-load (get-field :i-load result))
           (i-load (if i-load (round (* 100 i-load)) -1))
           (a-load (get-field :a-load result))
           (a-load (if a-load (round (* 100 a-load)) -1))
           (date (current-time :long t))
           (error 
            (normalize-string (get-field+ :error result "") :escape rawp))
           (comment 
            (normalize-string (get-field+ :comment result "") :escape rawp)))
      (if rawp
        (let ((stream (get-field :parse cache))
              (ofs *tsdb-ofs*))
          (write parse-id :stream stream) (write-char ofs stream)
          (write run-id :stream stream) (write-char ofs stream)
          (write i-id :stream stream) (write-char ofs stream)
          (write readings :stream stream) (write-char ofs stream)
          (write first :stream stream) (write-char ofs stream)
          (write total :stream stream) (write-char ofs stream)
          (write tcpu :stream stream) (write-char ofs stream)
          (write tgc :stream stream) (write-char ofs stream)
          (write treal :stream stream) (write-char ofs stream)
          (write words :stream stream) (write-char ofs stream)
          (write l-stasks :stream stream) (write-char ofs stream)
          (write p-ctasks :stream stream) (write-char ofs stream)
          (write p-ftasks :stream stream) (write-char ofs stream)
          (write p-etasks :stream stream) (write-char ofs stream)
          (write p-stasks :stream stream) (write-char ofs stream)
          (write aedges :stream stream) (write-char ofs stream)
          (write pedges :stream stream) (write-char ofs stream)
          (write raedges :stream stream) (write-char ofs stream)
          (write rpedges :stream stream) (write-char ofs stream)
          (write unifications :stream stream) (write-char ofs stream)
          (write copies :stream stream) (write-char ofs stream)
          (write conses :stream stream) (write-char ofs stream)
          (write symbols :stream stream) (write-char ofs stream)
          (write others :stream stream) (write-char ofs stream)
          (write gcs :stream stream) (write-char ofs stream)
          (write i-load :stream stream) (write-char ofs stream)
          (write a-load :stream stream) (write-char ofs stream)
          (write-string date stream) (write-char ofs stream)
          (write-string error stream) (write-char ofs stream)
          (write-string comment stream)
          (terpri stream)
          (force-output stream)
          (incf (get-field :count cache)))
        (let* ((query "insert into parse values")
               (query
                (format
                 nil
                 "~a ~d ~d ~d ~
                  ~d ~d ~d ~d ~d ~d ~
                  ~d ~d ~
                  ~d ~d ~d ~d ~
                  ~d ~d ~d ~d ~
                  ~d ~d ~
                  ~d ~d ~d ~
                  ~d ~d ~d ~
                  ~a ~s ~s"
                 query
                 parse-id run-id i-id
                 readings first total tcpu tgc treal
                 words l-stasks
                 p-ctasks p-ftasks p-etasks p-stasks
                 aedges pedges raedges rpedges
                 unifications copies
                 conses symbols others
                 gcs i-load a-load
                 date error comment)))
          (call-tsdb query language :cache cache))))))

(defun write-results (parse-id results
                      &optional (language *tsdb-data*)
                      &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error 
     "write-results(): write attempt on read-only cache; see `tsdb.lisp'"))

  (when *tsdb-write-result-p*
    (loop
        with *print-circle* = nil
        with *print-level* = nil
        with *print-length* = nil
        with rawp = (and cache (eq (get-field :protocol cache) :raw))
        for result in results
        for id from 0
        for result-id = (or (get-field :result-id result) id)
        for time = (round (get-field+ :time result -1))
        for r-ctasks = (get-field+ :r-ctasks result -1)
        for r-ftasks = (get-field+ :r-ftasks result -1)
        for r-etasks = (get-field+ :r-etasks result -1)
        for r-stasks = (get-field+ :r-stasks result -1)
        for size = (get-field+ :size result -1)
        for r-aedges =  (get-field+ :r-aedges result -1)
        for r-pedges = (get-field+ :r-pedges result -1)
        for derivation = (normalize-string 
                          (get-field :derivation result) :escape rawp)
        for surface = (normalize-string
                       (get-field :surface result) :escape rawp)
        for tree = (normalize-string (get-field :tree result) :escape rawp)
        for mrs = (normalize-string (get-field :mrs result) :escape rawp)
        for grade = (normalize-string (get-field :grade result) :escape rawp)
        do
          (if rawp
            (let ((stream (get-field :result cache))
                  (ofs *tsdb-ofs*))
              (write parse-id :stream stream) (write-char ofs stream)
              (write result-id :stream stream) (write-char ofs stream)
              (write time :stream stream) (write-char ofs stream)
              (write r-ctasks :stream stream) (write-char ofs stream)
              (write r-ftasks :stream stream) (write-char ofs stream)
              (write r-etasks :stream stream) (write-char ofs stream)
              (write r-stasks :stream stream) (write-char ofs stream)
              (write size :stream stream) (write-char ofs stream)
              (write r-aedges :stream stream) (write-char ofs stream)
              (write r-pedges :stream stream) (write-char ofs stream)
              (write-string derivation stream) (write-char ofs stream)
              (write-string surface stream) (write-char ofs stream)
              (write-string tree stream) (write-char ofs stream)
              (write-string mrs stream)  (write-char ofs stream)
              (write-string grade stream) 
              (terpri stream)
              (force-output stream)
              (incf (get-field :count cache)))
            (let* ((query (format
                           nil
                           "insert into result values ~
                            ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ~s ~s ~s ~s ~s"
                           parse-id result-id
                           time
                           r-ctasks r-ftasks r-etasks r-stasks
                           size r-aedges r-pedges
                           derivation surface tree mrs grade)))
              (call-tsdb query language :cache cache))))))

(defun write-edges (parse-id edges
                    &optional (language *tsdb-data*)
                    &key cache)
  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-edges(): write attempt on read-only cache; see `tsdb.lisp'"))

  (when *tsdb-write-edge-p*
    (loop
        with *print-circle* = nil
        with *print-level* = nil
        with *print-length* = nil
        with rawp = (and cache (eq (get-field :protocol cache) :raw))
        for edge in edges
        for e-id = (get-field+ :id edge -1)
        for e-label =  (normalize-string 
                        (get-field+ :label edge "") :escape rawp)
        for e-score = (get-field+ :score edge "")
        for e-start = (get-field+ :start edge -1)
        for e-end = (get-field+ :end edge -1)
        for e-status = (get-field+ :status edge -1)
        for e-daughters = (normalize-string 
                           (get-field+ :daughters edge "") :escape rawp)
        for e-parents = (normalize-string 
                         (get-field+ :parents edge "") :escape rawp)
        for e-alternates = (normalize-string 
                         (get-field+ :alternates edge "") :escape rawp)
        do
          (if rawp
            (let ((stream (get-field :edge cache))
                  (ofs *tsdb-ofs*))
              (write e-id :stream stream) (write-char ofs stream)
              (write parse-id :stream stream) (write-char ofs stream)
              (write-string e-label stream) (write-char ofs stream)
              (write-string e-score stream) (write-char ofs stream)
              (write e-start :stream stream) (write-char ofs stream)
              (write e-end :stream stream) (write-char ofs stream)
              (write e-status :stream stream) (write-char ofs stream)
              (write-string e-daughters stream) (write-char ofs stream)
              (write-string e-parents stream) (write-char ofs stream)
              (write-string e-alternates stream)
              (terpri stream)
              (force-output stream)
              (incf (get-field :count cache)))
            (let* ((query (format
                           nil
                           "insert into edge values ~
                            ~d ~d ~s ~s ~d ~d ~d ~s"
                           e-id parse-id e-label e-score 
                           e-start e-end e-status e-daughters)))
              (call-tsdb query language :cache cache))))))

(defun write-rules (parse-id statistics
                    &optional (language *tsdb-data*)
                    &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-rules(): write attempt on read-only cache; see `tsdb.lisp'"))

  (when *tsdb-rule-statistics-p*
    (loop 
        with *print-circle* = nil
        with *print-level* = nil
        with *print-length* = nil
        with rawp = (and cache (eq (get-field :protocol cache) :raw))
        for rule in statistics
        for name = (normalize-string (get-field+ :rule rule "") :escape rawp)
        for filtered = (get-field+ :filtered rule -1)
        for executed = (get-field+ :executed rule -1)
        for successful = (get-field+ :successful rule -1)
        for actives = (get-field+ :actives rule -1)
        for passives = (get-field+ :passives rule -1)
        do
          (if rawp
            (let ((stream (get-field :rule cache))
                  (ofs *tsdb-ofs*))
              (write parse-id :stream stream) (write-char ofs stream)
              (write-string name stream) (write-char ofs stream)
              (write filtered :stream stream) (write-char ofs stream)
              (write executed :stream stream) (write-char ofs stream)
              (write successful :stream stream) (write-char ofs stream)
              (write actives :stream stream) (write-char ofs stream)
              (write passives :stream stream)
              (terpri stream)
              (force-output stream)
              (incf (get-field :count cache)))
            (let ((query (format
                          nil
                          "insert into rule values ~d ~s ~d ~d ~d ~d ~d"
                          parse-id name filtered executed successful
                          actives passives)))
              (call-tsdb query language :cache cache))))))

(defun write-output (i-id application 
                     tree mrs tasks 
                     user date
                     language
                     &key cache)
  (when (and cache (eq (get-field :protocol cache) :ro))
    (error 
     "write-output(): write attempt on read-only cache; see `tsdb.lisp'"))

  (when *tsdb-write-output-p*
    (when (and cache (eq (get-field :protocol cache) :raw))
      (error "write-output(): unable to write to raw cache; see `tsdb.lisp'"))
    (let* ((*print-circle* nil)
           (*print-level* nil)
           (*print-length* nil)
           (rawp nil)
           (tree (normalize-string tree :escape rawp))
           (mrs (normalize-string mrs :escape rawp))
           (query
            (format
             nil
             "insert into output values ~a ~d ~s ~s ~s ~d ~s ~a"
             i-id application
             tree mrs tasks
             user date)))
      (call-tsdb query language :cache cache)))) 

(defun write-tree (data record &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-tree(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (parse-id (get-field :parse-id record))
         (version (get-field :t-version record))
         (active (get-field :t-active record))
         (confidence (get-field :t-confidence record))
         (t-author (get-field :t-author record))
         (t-start (get-field :t-start record))
         (t-end (get-field :t-end record))
         (t-comment (get-field :t-comment record)))
    (if rawp
      (let ((stream (get-field :tree cache))
            (ofs *tsdb-ofs*))
        (write parse-id :stream stream) (write-char ofs stream)
        (write version :stream stream) (write-char ofs stream)
        (write active :stream stream) (write-char ofs stream)
        (write confidence :stream stream) (write-char ofs stream)
        (write-string t-author stream) (write-char ofs stream)
        (write-string t-start stream) (write-char ofs stream)
        (write-string t-end stream) (write-char ofs stream)
        (write-string t-comment stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into tree values ~d ~d ~d ~d ~s ~a ~a ~s"
                    parse-id version active confidence
                    t-author t-start t-end t-comment)))
        (call-tsdb query data :cache cache)))))

(defun write-decision (data record &key cache)
  
  (when (and cache (eq (get-field :protocol cache) :ro))
    (error 
     "write-decision(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (parse-id (get-field :parse-id record))
         (t-version (get-field :t-version record))
         (d-state (get-field :d-state record))
         (d-type (get-field :d-type record))
         (d-key
          (normalize-string (or (get-field :d-key record) "") :escape rawp))
         (d-value
          (normalize-string (or (get-field :d-value record) "") :escape rawp))
         (d-start (get-field :d-start record))
         (d-end (get-field :d-end record))
         (d-date (get-field :d-date record)))
    (if rawp
      (let ((stream (get-field :decision cache))
            (ofs *tsdb-ofs*))
        (write parse-id :stream stream) (write-char ofs stream)
        (write t-version :stream stream) (write-char ofs stream)
        (write d-state :stream stream) (write-char ofs stream)
        (write d-type :stream stream) (write-char ofs stream)
        (write-string (or d-key "") stream) (write-char ofs stream)
        (write-string (or d-value "") stream) (write-char ofs stream)
        (write d-start :stream stream) (write-char ofs stream)
        (write d-end :stream stream) (write-char ofs stream)
        (write-string (or d-date "") stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into decision values ~d ~d ~d ~s ~s ~d ~d ~a"
                    parse-id t-version d-type (or d-key "") (or d-value "") 
                    d-start d-end (or d-date ""))))
        (call-tsdb query data :cache cache)))))

(defun write-preference (data record &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error 
     "write-preference(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (parse-id (get-field :parse-id record))
         (t-version (get-field :t-version record))
         (result-id (get-field :result-id record)))
    (if rawp
      (let ((stream (get-field :preference cache))
            (ofs *tsdb-ofs*))
        (write parse-id :stream stream) (write-char ofs stream)
        (write t-version :stream stream) (write-char ofs stream)
        (write result-id :stream stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into preference values ~d ~d ~d"
                    parse-id t-version result-id)))
        (call-tsdb query data :cache cache)))))

(defun write-update (data record &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error 
     "write-update(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (parse-id (get-field :parse-id record))
         (t-version (get-field :t-version record))
         (u-matches (get-field+ :u-matches record -1))
         (u-mismatches (get-field+ :u-mismatches record -1))
         (u-new (get-field+ :u-new record -1))
         (u-gin (get-field+ :u-gin record -1))
         (u-gout (get-field+ :u-gout record -1))
         (u-pin (get-field+ :u-pin record -1))
         (u-pout (get-field+ :u-pout record -1))
         (u-in (get-field+ :u-in record -1))
         (u-out (get-field+ :u-out record -1)))
    (if rawp
      (let ((stream (get-field :update cache))
            (ofs *tsdb-ofs*))
        (write parse-id :stream stream) (write-char ofs stream)
        (write t-version :stream stream) (write-char ofs stream)
        (write u-matches :stream stream) (write-char ofs stream)
        (write u-mismatches :stream stream) (write-char ofs stream)
        (write u-new :stream stream) (write-char ofs stream)
        (write u-gin :stream stream) (write-char ofs stream)
        (write u-gout :stream stream) (write-char ofs stream)
        (write u-pin :stream stream) (write-char ofs stream)
        (write u-pout :stream stream) (write-char ofs stream)
        (write u-in :stream stream) (write-char ofs stream)
        (write u-out :stream stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into update values ~
                     ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d ~d"
                    parse-id t-version u-matches u-mismatches u-new
                    u-gin u-gout u-pin u-pout u-in u-out)))
        (call-tsdb query data :cache cache)))))

(defun write-fold (data record &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-fold(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (f-id (get-field+ :f-id record -1))
         (f-train (get-field+ :f-train record -1))
         (f-trains (get-field+ :f-trains record ""))
         (f-test (get-field+ :f-test record -1))
         (f-tests (get-field+ :f-tests record ""))
         (f-events (get-field+ :f-events record -1))
         (f-features (get-field+ :f-features record -1))
         (f-environment (normalize-string
                         (get-field+ :f-environment record "") :escape rawp))
         (f-iterations (get-field+ :f-iterations record -1))
         (f-etime (get-field+ :f-etime record -1))
         (f-estimation (normalize-string
                        (get-field+ :f-estimation record "") :escape rawp))
         (f-accuracy (let ((foo (get-field+ :f-accuracy record "")))
                       (if (stringp foo) foo (format nil "~f" foo))))
         (f-extras (normalize-string
                    (get-field+ :f-extras record "")))
         (f-user (get-field+ :f-user record ""))
         (f-host (get-field+ :f-host record ""))
         (f-start (get-field+ :f-start record ""))
         (f-end (get-field+ :f-end record ""))
         (f-comment (normalize-string
                     (get-field+ :f-comment record ""))))
    (if rawp
      (let ((stream (get-field :fold cache))
            (ofs *tsdb-ofs*))
        (write f-id :stream stream) (write-char ofs stream)
        (write f-train :stream stream) (write-char ofs stream)
        (write-string f-trains stream) (write-char ofs stream)
        (write f-test :stream stream) (write-char ofs stream)
        (write-string f-tests stream) (write-char ofs stream)
        (write f-events :stream stream) (write-char ofs stream)
        (write f-features :stream stream) (write-char ofs stream)
        (write-string f-environment stream) (write-char ofs stream)
        (write f-iterations :stream stream) (write-char ofs stream)
        (write f-etime :stream stream) (write-char ofs stream)
        (write-string f-estimation stream) (write-char ofs stream)
        (write-string f-accuracy stream) (write-char ofs stream)
        (write-string f-extras stream) (write-char ofs stream)
        (write-string f-user stream) (write-char ofs stream)
        (write-string f-host stream) (write-char ofs stream)
        (write-string f-start stream) (write-char ofs stream)
        (write-string f-end stream) (write-char ofs stream)
        (write-string f-comment stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into score values ~
                     ~d ~d ~s ~d ~s ~d ~d ~s ~d ~d ~s ~s ~s ~s ~s ~s ~s ~s"
                    f-id f-train f-trains f-test f-tests f-events f-features
                    f-environment f-iterations f-etime f-estimation f-accuracy
                    f-extras f-user f-host f-start f-end f-comment)))
        (call-tsdb query data :cache cache)))))

(defun write-score (data record &key cache)

  (when (and cache (eq (get-field :protocol cache) :ro))
    (error "write-score(): write attempt on read-only cache; see `tsdb.lisp'"))

  (let* ((*print-circle* nil)
         (*print-level* nil)
         (*print-length* nil)
         (rawp (and cache (eq (get-field :protocol cache) :raw)))
         (parse-id (get-field :parse-id record))
         (result-id (get-field :result-id record))
         (rank (get-field :rank record))
         (score (get-field+ :score record "")))
    (if rawp
      (let ((stream (get-field :score cache))
            (ofs *tsdb-ofs*))
        (write parse-id :stream stream) (write-char ofs stream)
        (write result-id :stream stream) (write-char ofs stream)
        (write rank :stream stream) (write-char ofs stream)
        (write-string score stream)
        (terpri stream)
        (force-output stream)
        (incf (get-field :count cache)))
      (let ((query (format
                    nil
                    "insert into score values ~
                     ~d ~d ~d ~s"
                    parse-id result-id rank score)))
        (call-tsdb query data :cache cache)))))
