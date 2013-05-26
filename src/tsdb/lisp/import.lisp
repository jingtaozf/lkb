;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

;;;
;;; Copyright (c) 1996 -- 2000 Stephan Oepen (oe@coli.uni-sb.de)
;;; Copyright (c) 2001 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;; Copyright (c) 2007 -- 2010 Stephan Oepen (oe@ifi.uio.no)
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


(in-package :tsdb)

(defparameter *import-tsnlp-style-p* nil)

(defparameter *import-phenomena-separator* ";;;")

(defparameter *import-separator* ";;")

(defparameter *import-punctuation* 
  '(#\, #\. #\! #\? #\; #\: #\- #\" #\' #\` #\( #\)))

(defparameter *import-origin* "unknown")

(defparameter *import-register* "formal")

(defparameter *import-difficulty* 1)

(defparameter *import-category* "S")

(defparameter *import-marks*
  '((#\* . :illformed) 
    (#\+ . :incomplete) 
    (#\^ . :mispeled)
    (#\# . :td)))

(defparameter *import-email-headers*
  '("From " "From:" "To:" "Subject:" 
    "MIME-Version:" "Content-Type:" "Content-Transfer-Encoding:"
    "Message-ID:"))

(defparameter *import-result-hook* nil)

(defparameter *import-normalize-p* t)

(defun do-import-database (source target
                                  &key absolute meter except uncompress)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((tpath (if absolute (namestring target) (find-tsdb-directory target)))
         (sstatus (verify-tsdb-directory source :absolute t))
         (tstatus (verify-tsdb-directory tpath :absolute t)))
    (cond
     ((null sstatus)
      (format
       *tsdb-io*
       "import-database(): invalid source database `~a'.~%"
       source)
      -1)
     (tstatus
      (format
       *tsdb-io*
       "import-database(): target `~a' exists.~%"
       target
       -2))
     (t
      (unless (purge-directory tpath)
        (when (probe-file tpath) (delete-file tpath))
        (mkdir tpath))
      (let* ((schema (read-database-schema source :absolute t))
             (increment (when meter (/ (mduration meter) (length schema)))))
        (loop
            for (relation) in schema
            for compressed = (format nil "~a.gz" relation)
            do
              (let ((sfile (make-pathname :directory source :name relation))
                    (tfile (make-pathname :directory tpath :name relation)))
                (when (probe-file sfile)
                  (if (member relation except :test #'equal)
                    (touch tfile)
                    (cp sfile tfile))))
              (let ((sfile (make-pathname :directory source :name compressed))
                    (tfile (make-pathname :directory tpath :name compressed)))
                (when (probe-file sfile)
                  (cond
                   ((member relation except :test #'equal)
                    (touch (make-pathname :directory tpath :name relation)))
                   (t
                    (cp sfile tfile)
                    (when uncompress
                      (run-process 
                       (format nil "~a '~a'" "gzip -d -f" (namestring tfile))
                       :wait t))))))
              (when increment (meter-advance increment))
            finally
              (let ((sfile 
                     (make-pathname :directory source :name "relations")))
                (when (probe-file sfile)
                  (cp sfile 
                      (make-pathname :directory tpath :name "relations")))))
        (when meter (meter :value (get-field :end meter)))
        schema)))))

(defun do-import-items 
    (file data &key 
               (format :ascii)
               absolute
               target
               low high
               (create t)
               (overwrite t)
               (origin (or *import-origin* "unknown"))
               (register (or *import-register* "formal"))
               (difficulty (or *import-difficulty* 1))
               (category (or *import-category* ""))
               comment shift
               (separator (or *import-separator* ";;"))
               (pseparator (or *import-phenomena-separator* ";;;"))
               encoding meter)
  
  (when meter (meter :value (get-field :start meter)))
  (unless absolute (purge-profile-cache data))
  (let* ((path (if absolute (namestring data) (find-tsdb-directory data)))
         (ifile (make-pathname :directory path :name "item"))
         (status (verify-tsdb-directory path :absolute t))
         (rmeter (when meter (madjust * meter 0.4)))
         (imeter (when meter
                   (madjust + (madjust * meter 0.3) (mduration rmeter))))
         (ipmeter (when meter 
                    (madjust + (madjust * meter 0.3) 
                             (+ (mduration rmeter) (mduration imeter)))))
         (tpath (when target
                  (if absolute
                    (namestring target)
                    (find-tsdb-directory target))))
         (tifile (and tpath (make-pathname :directory tpath :name "item")))
         (tisfile
          (and tpath (make-pathname :directory tpath :name "item-set")))
         (tstatus (and tpath (verify-tsdb-directory tpath :absolute t))))

    (cond
     ((null (probe-file file))
      (format
       *tsdb-io*
       "import-items(): unable to open input file `~a'.~%"
       file)
      1)
     ((and (null status) (null create))
      (format *tsdb-io* "import-items(): invalid database `~a'.~%" data)
      2)
     ((and (eq (get-field :status status) :ro) (not (eq create :purge)))
      (format *tsdb-io* "import-items(): read-only database `~a'.~%" data)
      3)
     ((and (null tstatus) (null create))
      (format *tsdb-io* "import-items(): invalid database `~a'.~%" target)
      12)
     ((and (eq (get-field :status tstatus) :ro) (not (eq create :purge)))
      (format *tsdb-io* "import-items(): read-only database `~a'.~%" target)
      13)
     (t
      (when (and create 
                 (or (null status) (eq create :purge)
                     (not (find "item" (read-database-schema path :absolute t)
                                :test #'string= :key #'first))))
        (unless (purge-directory path)
          (when (probe-file path) (delete-file path))
          (mkdir path :parentp t))
        (let ((relations 
               (make-pathname :directory (namestring *tsdb-skeleton-directory*)
                              :name *tsdb-relations-skeleton*))
              (target (make-pathname :directory path :name "relations")))
          (unless (cp relations target)
            (format
             *tsdb-io*
             "import-items(): invalid skeleton root; check `~a'.~%"
             (namestring *tsdb-skeleton-directory*))
            (return-from do-import-items 4)))
          
        (select "i-id" :integer "item" nil path :absolute t))
      (when (and create tpath
                 (or (null tstatus) (eq create :purge)
                     (not (find "item" (read-database-schema tpath :absolute t)
                                :test #'string= :key #'first))))
        (unless (purge-directory tpath)
          (when (probe-file tpath) (delete-file tpath))
          (mkdir tpath :parentp t))
        (let ((relations 
               (make-pathname :directory (namestring *tsdb-skeleton-directory*)
                              :name *tsdb-relations-skeleton*))
              (target (make-pathname :directory tpath :name "relations")))
          (unless (cp relations target)
            (format
             *tsdb-io*
             "import-items(): invalid skeleton root; check `~a'.~%"
             (namestring *tsdb-skeleton-directory*))
            (return-from do-import-items 14)))
          
        (select "i-id" :integer "item" nil tpath :absolute t))
      
      (when overwrite 
        (with-open-file (foo ifile :direction :output
                         :if-does-not-exists :create 
                         :if-exists :supersede))
        (when tifile
          (with-open-file (foo tifile :direction :output
                           :if-does-not-exists :create 
                           :if-exists :supersede)))
        (when tisfile
          (with-open-file (foo tisfile :direction :output
                           :if-does-not-exists :create 
                           :if-exists :supersede))))
      (let* ((ids (select "i-id" :integer "item" nil path :absolute t))
             (ids (map 'list #'(lambda (foo) (get-field :i-id foo)) ids))
             (ids (remove nil ids))
             (base (if ids (+ 1 (apply #'max ids)) 1)))
        (multiple-value-bind (item phenomenon item-phenomenon output
                              parse result
                              titem foo bar tis)
            (case format
              (:ascii
               (read-items-from-ascii-file
                file
                :base base :origin origin :register register
                :difficulty difficulty :category category
                :comment comment :shift shift
                :separator separator :pseparator pseparator
                :encoding encoding :meter rmeter))
              (:bitext
               (read-items-from-bitext-file
                file
                :base base :origin origin :register register
                :difficulty difficulty :category category
                :comment comment :shift shift
                :separator separator :pseparator pseparator
                :encoding encoding :meter rmeter))
              (:ptb
               (read-items-from-ptb-directory file :base base)) 
              (:conll
               (read-items-from-conll-file file :shift shift)) 
              (:rasp
               (read-items-from-rasp-file file
                                          :base base
                                          :origin origin
                                          :register register
                                          :difficulty difficulty
                                          :category category
                                          :comment comment
                                          :encoding encoding
                                          :meter rmeter)))
          (declare (ignore foo bar))
          (cond
           (item
            (when (and (numberp low) (numberp high))
              (setf item
                (loop
                    for i from 0
                    for foo in item
                    when (and (<= low i) (< i high))
                    collect foo)))
            (insert
             path "item" item :absolute t 
             :normalize (and *import-normalize-p*
                             (not (smember format '(:conll))))
             :meter (if item-phenomenon 
                      imeter
                      (when (and imeter ipmeter)
                        (make-meter (get-field :start imeter) 
                                    (get-field :end ipmeter)))))
            (when (and phenomenon item-phenomenon)
              (insert path "phenomenon" phenomenon :absolute t)
              (insert path "item-phenomenon" item-phenomenon 
                      :absolute t :meter ipmeter))
            (when output
              (insert path "output" output :absolute t))
            (when parse (insert path "parse" parse :absolute t))
            (when result (insert path "result" result :absolute t))
            (when (and titem tpath)
              (insert tpath "item" titem :absolute t))
            (when (and tis tpath)
              (insert tpath "item-set" tis :absolute t))
            item)
           (t 42))))))))

(defun read-items-from-ascii-file (file &key (base 1)
                                             (origin "unknown")
                                             (register "formal")
                                             (difficulty 1)
                                             (category "")
                                             comment 
                                             shift
                                             (separator ";;")
                                             (pseparator ";;;")
                                             (p-id 1)
                                             encoding
                                             meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((lines (and meter (get-field :lines (wc file))))
         (increment (when (and lines (> lines 0))
                      (/ (mduration meter) lines)))
         (format "none")
         (author (current-user))
         (date (current-time))
         (index 0)
         stream pid foo item phenomenon item-phenomenon)
    (setf foo foo)
    (if (ppcre:scan "\\.gz$" (namestring file))
      (let ((file (probe-file file)))
        (when file
          (multiple-value-setq (stream foo pid)
            (run-process
             (format nil "gzip -d -c '~a'" (namestring file))
             :wait nil :output :stream :input "/dev/null"
             :error-output "/dev/null" :if-error-output-exists :append))))
      (setf stream (open file :direction :input :if-does-not-exist :error)))
    (when stream
      #+:allegro
      (when encoding (setf (stream-external-format stream) encoding))
      (decf p-id)
      (loop
          with context = :newline
          for i from 1
          for line = (read-line stream nil nil)
          while line
          for string = (string-trim '(#\Space #\Tab #\Return) line)
          for commentp = (let ((n (position #\; string)))
                           (and (numberp n) (zerop n)))
          for length = (if commentp 0 (length string))
          for pseparation = (search pseparator string)
          when increment do (meter-advance increment)
          when (and pseparation (zerop pseparation)) do
            (let* ((string (subseq string (length pseparator)))
                   (p-name (string-trim '(#\Space #\Tab) string)))
              (unless (zerop (length p-name))
                (incf p-id)
                (push (pairlis '(:p-id :p-name :p-author :p-date) 
                               (list p-id p-name author date))
                      phenomenon)))
            (setf context :phenomenon)
          else when (zerop length) do
            (setf context :newline)
          else unless (zerop length) do
            (multiple-value-bind (string id) (strip-identifier string)
              (multiple-value-bind (offset extras) (classify-item string)
                (when (and (eq (get-field :header extras) :envelope)
                           (smember context '(:newline :phenomenon)))
                  (incf index 1000)
                  (setf base 1))
                (unless (and (numberp id) (>= id (+ index base)))
                  (setf id (+ index base)))
                (when (get-field :author extras)
                  (setf author (get-field :author extras)))
                (when (get-field :date extras)
                  (setf date (get-field :date extras)))
                (let ((foo (get-field :index extras)))
                  (when (and (numberp foo) (not (= foo index)))
                    (format
                     t
                     "read-items-from-ascii-file(): ~
                      [~d] index mismatch (~d vs. ~d).~%"
                     i foo index)))
                (let* ((string (subseq string offset))
                       (break (when separator 
                                (search separator string 
                                        :test #'string= :from-end t)))
                       (comment 
                        (format
                         nil
                         "~@[~a~]~:[~; ~]~@[~a~]"
                         comment comment
                         (when break
                           (normalize-string 
                            (subseq string (+ break (length separator)))))))
                       (comment
                        (let ((n (- (length comment) 1)))
                          (or 
                           (when (and (< 0 n)
                                      (char= (schar comment 0) #\()
                                      (char= (schar comment n) #\)))
                             (ignore-errors (read-from-string comment)))
                           (unless (string= comment "") comment))))
                       (string (if break
                                 (subseq string 0 (max break 0)) 
                                 string))
                       (input (if *import-normalize-p*
                                (string-trim '(#\Space #\Tab) string)
                                string))
                       (oinput 
                        (when (find-attribute-reader :i-input)
                          (funcall (find-attribute-reader :i-input) input))))
                  (multiple-value-bind (ninput length tokens)
                      (if (get-field :header extras)
                        (values input -1)
                        (normalize-item (or oinput input)))
                    (let ((n (get-field :i-length extras)))
                      (when (integerp n)
                        (setf length n)))
                    (unless (or (get-field :paragraph extras)
                                (zerop length))
                      (let* ((category (get-field+ :category extras category))
                             (wf (cond
                                  ((get-field :header extras) -1)
                                  ((get-field :illformed extras) 0)
                                  (t 1))))
                        (when (functionp shift)
                          (setf id (funcall shift id)))
                        (push
                         (pairlis '(:i-id :i-origin :i-register
                                    :i-format :i-difficulty :i-category
                                    :i-input :i-wf :i-length :i-comment
                                    :i-tokens :i-author :i-date)
                                  (list id origin register
                                        format difficulty category 
                                        (if oinput input ninput)
                                        wf length
                                        (if (stringp comment)
                                          comment
                                          (when comment
                                            (write-to-string
                                             comment 
                                             :case :downcase)))
                                        (if (stringp tokens)
                                          tokens
                                          (when tokens
                                            (write-to-string
                                             tokens 
                                             :case :downcase)))
                                        author date))
                         item)
                        (when phenomenon
                          (push (pairlis '(:ip-id :i-id :p-id
                                           :ip-author :ip-date)
                                         (list base id p-id
                                               author date))
                                item-phenomenon))
                        (incf base)))))))
            (setf context nil))

      (close stream)
      #+:allegro
      (when (integerp pid) (sys:os-wait nil pid))
      (when meter (meter :value (get-field :end meter)))
      (values 
       (nreverse item) (nreverse phenomenon) (nreverse item-phenomenon)))))

(defun read-items-from-bitext-file (file &key (base 1)
                                              (origin "unknown")
                                              (register "formal")
                                              (difficulty 1)
                                              (category "")
                                              comment
                                              shift
                                              (separator ";;")
                                              (pseparator ";;;")
                                              (p-id 1)
                                              encoding
                                              meter)

  (when meter (meter :value (get-field :start meter)))
  (let* ((lines (and meter (get-field :lines (wc file))))
         (increment (when (and lines (> lines 0))
                      (/ (mduration meter) lines)))
         (format "none")
         (author (current-user))
         (date (current-time))
         (index 0)
         (delta 0)
         stream pid foo item phenomenon item-phenomenon output titem tis)
    (setf foo foo)
    (if (ppcre:scan "\\.gz$" (namestring file))
      (let ((file (probe-file file)))
        (when file
          (multiple-value-setq (stream foo pid)
            (run-process
             (format nil "gzip -d -c '~a'" (namestring file))
             :wait nil :output :stream :input "/dev/null"
             :error-output "/dev/null" :if-error-output-exists :append))))
      (setf stream (open file :direction :input :if-does-not-exist :error)))
    (when stream
      #+:allegro
      (when encoding (setf (stream-external-format stream) encoding))
      (decf p-id)
      (loop
          with context = :newline
          with rid with rinput
          with sid with last
          for i from 1
          for line = (read-line stream nil nil)
          while line
          for string = (string-trim '(#\Space #\Tab #\Return) line)
          for commentp = (let ((n (position #\; string)))
                           (and (numberp n) (zerop n)))
          for length = (length string)
          for pseparation = (search pseparator string)
          when increment do (meter-advance increment)
          when (and pseparation (zerop pseparation)) do
            (let* ((string (subseq string (length pseparator)))
                   (p-name (string-trim '(#\Space #\Tab) string)))
              (unless (zerop (length p-name))
                (incf p-id)
                (push (pairlis '(:p-id :p-name :p-author :p-date) 
                               (list p-id p-name author date))
                      phenomenon)))
            (setf context :phenomenon)
            (setf delta 0)
          else when (zerop length) do
            (setf context :newline)
            (setf rid nil) (setf rinput nil)
            (setf delta 0)
          else unless commentp do
            (multiple-value-bind (string id)
                (strip-identifier string)
              (unless (numberp id)
                (setf id (if (eq context :item)
                           (+ (* index 10) (incf delta))
                           (* (incf index) 10))))
              (when (and (numberp last) (<= id last))
                (setf id (+ last 1)))
              (setf last id)
              (when (smember context '(:newline :phenomenon))
                (setf sid id)
                (setf rid id))
              (multiple-value-bind (offset extras)
                  (classify-item string)
                (when (get-field :paragraph extras) (setf rid nil))
                (when rid
                  (let* ((string (subseq string offset))
                         (break (when separator 
                                  (search
                                   separator string 
                                   :test #'string= :from-end t)))
                         (comment 
                          (format
                           nil
                           "~@[~a~]~@[ ~a~]"
                           comment
                           (when break
                             (normalize-string 
                              (subseq string (+ break (length separator)))))))
                         (string (if break
                                   (subseq string 0 (max break 0)) 
                                   string))
                         (input (string-trim '(#\Space #\Tab) string))
                         (oinput 
                          (when (find-attribute-reader :i-input)
                            (funcall (find-attribute-reader :i-input) input))))
                    (multiple-value-bind (ninput length)
                        (normalize-item (or oinput input))
                      (unless (zerop length)
                        (let* ((category
                                (get-field+ :category extras category))
                               (wf (if (get-field :illformed extras) 0 1))
                               (targetp (not (zerop (mod id 10)))))
                          (when (functionp shift)
                            (setf id (funcall shift id)))
                          (cond
                           (targetp
                            (push (pairlis '(:i-id 
                                             :i-origin :i-register :i-format
                                             :i-difficulty :i-category :i-input
                                             :i-wf :i-length :i-comment
                                             :i-author :i-date)
                                           (list id
                                                 origin register format
                                                 difficulty category 
                                                 (if oinput input ninput) wf
                                                 length (or rinput comment)
                                                 author date))
                                  titem)
                            (push (pairlis '(:i-id :s-id :polarity)
                                           (list id sid -1))
                                  tis)
                            (push (pairlis '(:i-id :o-surface)
                                           (list rid
                                                 (if oinput input ninput)))
                                  output))
                           (t
                            (push (pairlis '(:i-id 
                                             :i-origin :i-register :i-format
                                             :i-difficulty :i-category :i-input
                                             :i-wf
                                             :i-length :i-comment :i-author
                                             :i-date)
                                           (list id
                                                 origin register format
                                                 difficulty category 
                                                 (if oinput input ninput) wf
                                                 length comment author date))
                                  item)
                            (setf rinput (if oinput input ninput)))))
                        (when phenomenon
                          (push (pairlis '(:ip-id :i-id :p-id
                                           :ip-author :ip-date)
                                         (list base id p-id
                                               author date))
                                item-phenomenon)
                          (incf base))))))))
            (setf context :item))

      (close stream)
      #+:allegro
      (when (integerp pid) (sys:os-wait nil pid))
      (when meter (meter :value (get-field :end meter)))
      (values
       ;;
       ;; _fix_me_
       ;; until we keep track of phenomena data for the target too, no point in
       ;; returning the partial truth.                           (5-jun-05; oe)
       ;;
       (nreverse item) nil nil (nreverse output) nil nil
       (nreverse titem) nil nil (nreverse tis)))))

(defun normalize-item (string)
  (flet ((punctuationp (char)
           (and *import-tsnlp-style-p*
                (member char *import-punctuation* :test #'char=)))
         (whitespacep (char)
           (member char '(#\Space #\Tab #\Newline) :test #'char=)))
    (if *import-normalize-p*
      (let ((string (string-trim '(#\Space #\Tab #\Newline) string))
            (result (make-array 
                     4096 :element-type 'character
                     :adjustable t :fill-pointer 0))
            (n 0)
            (previous :whitespace))
        (loop
            for i from 0 to (- (length string) 1) by 1
            for current = (char string i)
            do
              (cond
               ((and (whitespacep current) (eq previous :regular))
                (vector-push-extend #\Space result 1024)
                (setf previous :whitespace)
                (incf n))
               ((and (whitespacep current) (eq previous :whitespace)))
               ((and (punctuationp current) (eq previous :regular))
                (vector-push-extend #\Space result 1024)
                (vector-push-extend current result 1024)
                (vector-push-extend #\Space result 1024)
                (setf previous :whitespace)
                (incf n))
               ((and (punctuationp current) (eq previous :whitespace))
                (vector-push-extend current result 1024)
                (vector-push-extend #\Space result 1024)
                (setf previous :whitespace))
               (t
                (vector-push-extend current result 1024)
                (setf previous :regular))))
        (when (eq previous :regular) (incf n))
        (let ((result (string-right-trim '(#\Space) result))
              tokens)
          (when *tsdb-preprocessing-hook*
            (multiple-value-setq (tokens n) 
              (call-safe-hook *tsdb-preprocessing-hook* result))
            (unless (numberp n) (setf n (length tokens))))
          (values result (or n -1) tokens)))
      (let ((n (length (ppcre:split "[ \\t]+" string)))
            tokens)
        (when *tsdb-preprocessing-hook*
          (multiple-value-setq (tokens n) 
            (call-safe-hook *tsdb-preprocessing-hook* string))
          (unless (numberp n) (setf n (length tokens))))
        (values string n tokens)))))

(defun strip-identifier (string)
  (multiple-value-bind (start end) 
      (ppcre:scan "^[0-9]+[a-z]\\. " string)
    (unless start
      (multiple-value-setq (start end)
        (ppcre:scan "^\\[[0-9]{4,5}[a-z]\\]( |$)" string))
      (when (numberp start) (incf start)))
    (unless start
      (multiple-value-setq (start end)
        (ppcre:scan "^\\[[0-9]+\\]( |$)" string))
      (when (numberp start) (incf start)))
    (if (numberp end)
      (let ((suffix (subseq string end))
            (id (or (parse-integer string :start start :junk-allowed t) 0))
            (offset (case (when (>= end 3) (char string (- end 3)))
                      (#\a 0)
                      (#\b 1)
                      (#\c 2)
                      (#\d 3)
                      (#\e 4)
                      (#\f 5)
                      (#\g 6)
                      (#\h 7)
                      (#\i 8)
                      (#\j 9))))
        (values suffix (if offset (+ (* id 10) offset) id)))
      string)))

(defun classify-item (string)
  (let ((result nil)
        (offset 0))
    (unless (string= string "")
      (multiple-value-bind (start end starts ends) 
          (ppcre:scan "^ *<([0-9]+)>[^|]*\\|" string)
        (when (and start end)
          (let ((n (ignore-errors
                    (parse-integer
                     string :start (svref starts 0) :end (svref ends 0)))))
            (when n (push (cons :i-length n) result))
            (setf offset (+ (svref ends 0) 1)))))
      (loop
          for stablep = t
          when (char= (char string offset) #\space)
          do (incf offset)
          when (char= (char string offset) #\|)
          do (incf offset) (setf stablep t)
          else do
            (loop
                for (mark . key) in *import-marks*
                when (char= (char string offset) mark) do
                  (push (cons key t) result)
                  (incf offset)
                  (setf stablep nil))
          until stablep)
      (when (or (string-equal (subseq string offset) "<p>")
                (string-equal (subseq string offset) "<tr>"))
        (setf result (acons :paragraph t nil)))
      (loop
          with length = (length string)
          for key in *import-email-headers*
          for end = (max (- (min length (+ offset (length key))) 1) 0)
          when (and (< offset end)
                    (string= string key :start1 offset :end1 end)) do
            (push (cons :category "EH") result)
            (push (cons :header t) result)
            (when (string-equal key "From " :start1 offset :end1 end)
              (push (cons :header :envelope) result))
            (when (string-equal key "From:" :start1 offset :end1 end)
              (push (cons :header :from) result)
              (let ((value (subseq string (+ offset 5))))
                (push (cons :author (normalize-string value)) result)))
            (when (string-equal key "Subject:" :start1 offset :end1 end)
              (push (cons :header :subject) result)
              (let* ((start (position #\# string :from-end t))
                     (index (when start
                              (read-from-string 
                               string nil nil :start (+ start 1)))))
                (when (integerp index)
                  (push (cons :index index) result)))))
      (unless (get-field :category result)
        (cond
         ((get-field :illformed result)
          (push (cons :category "") result))
         ((get-field :incomplete result)
          (push (cons :category "XP") result))
         ((get-field :td result)
          (push (cons :category "TD") result)))))
    (values offset result)))
