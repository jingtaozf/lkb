;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

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
    (#\^ . :mispeled)))

(defparameter *import-email-headers*
  '("From " "From:" "To:" "Subject:" 
    "MIME-Version:" "Content-Type:" "Content-Transfer-Encoding:"
    "Message-ID:"))

(defparameter *import-result-hook* nil)

(defun do-import-database (source target &key absolute meter except)
  
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
                  (if (member relation except :test #'equal)
                    (touch tfile)
                    (cp sfile tfile))))
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
               (create t)
               (overwrite t)
               (origin (or *import-origin* "unknown"))
               (register (or *import-register* "formal"))
               (difficulty (or *import-difficulty* 1))
               (category (or *import-category* ""))
               comment 
               (separator (or *import-separator* ";;"))
               (pseparator (or *import-phenomena-separator* ";;;"))
               meter)
  
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
                             (+ (mduration rmeter) (mduration imeter))))))

    (cond
     ((null (probe-file file))
      (format
       *tsdb-io*
       "import-items(): unable to open input file `~a'.~%"
       file)
      1)
     ((and (null status) (null create))
      (format
       *tsdb-io*
       "import-items(): invalid database `~a'.~%"
       data)
      2)
     ((eq (get-field :status status) :ro)
      (format
       *tsdb-io*
       "import-items(): read-only database `~a'.~%"
       data)
      3)
     (t
      (when (and create 
                 (or (null status)
                     (not (find "item" (read-database-schema path :absolute t)
                                :test #'string= :key #'first))))
        (unless (purge-directory path)
          (when (probe-file path) (delete-file path))
          (mkdir path))
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
      (when overwrite 
        (with-open-file (foo ifile :direction :output
                         :if-does-not-exists :create 
                         :if-exists :supersede)))
      (let* ((ids (select "i-id" :integer "item" nil path :absolute t))
             (ids (map 'list #'(lambda (foo) (get-field :i-id foo)) ids))
             (ids (remove nil ids))
             (base (if ids (+ 1 (apply #'max ids)) 1)))
        (multiple-value-bind (item phenomenon item-phenomenon parse result)
            (case format
              (:ascii
               (read-items-from-ascii-file file
                                           :base base
                                           :origin origin
                                           :register register
                                           :difficulty difficulty
                                           :category category
                                           :comment comment
                                           :separator separator
                                           :pseparator pseparator
                                           :meter rmeter))
              (:rasp
               (read-items-from-rasp-file file
                                          :base base
                                          :origin origin
                                          :register register
                                          :difficulty difficulty
                                          :category category
                                          :comment comment
                                          :meter rmeter)))
          
          (cond
           (item
            (insert path "item" item 
                    :absolute t 
                    :meter (if item-phenomenon 
                             imeter
                             (when (and imeter ipmeter)
                               (make-meter (get-field :start imeter) 
                                           (get-field :end ipmeter)))))
            (when (and phenomenon item-phenomenon)
              (insert path "phenomenon" phenomenon :absolute t)
              (insert path "item-phenomenon" item-phenomenon 
                      :absolute t :meter ipmeter))
            (when parse (insert path "parse" parse :absolute t))
            (when result (insert path "result" result :absolute t))
            item)
           (t 4))))))))

(defun read-items-from-ascii-file (file &key (base 1)
                                             (origin "unknown")
                                             (register "formal")
                                             (difficulty 1)
                                             (category "")
                                             comment
                                             (separator ";;")
                                             (pseparator ";;;")
                                             (p-id 1)
                                             meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((lines (and meter (get-field :lines (wc file))))
         (increment (when (and lines (> lines 0))
                      (/ (mduration meter) lines)))
         (stream (open file :direction :input :if-does-not-exist :error))
         (format "none")
         (author (current-user))
         (date (current-time))
         (index 0)
         item phenomenon item-phenomenon)
    (when stream
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
            (multiple-value-bind (offset extras)
                (classify-item line)
              (when (and (eq (get-field :header extras) :envelope)
                         (smember context '(:newline :phenomenon)))
                (incf index 1000)
                (setf base 1))
              (when (get-field :author extras)
                (setf author (get-field :author extras)))
              (when (get-field :date extras)
                (setf author (get-field :date extras)))
              (let ((foo (get-field :index extras)))
                (when (and (numberp foo) (not (= foo index)))
                  (format
                   t
                   "read-items-from-ascii-file(): ~
                    [~d] index mismatch (~d vs. ~d).~%"
                   i foo index)))
              (let* ((string (subseq line offset))
                     (break (when separator 
                              (search separator string 
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
                    (if (get-field :header extras)
                      (values input -1)
                      (normalize-item (or oinput input)))
                  (unless (zerop length)
                    (let* ((category (get-field+ :category extras category))
                           (wf (cond
                                ((get-field :header extras) -1)
                                ((get-field :illformed extras) 0)
                                (t 1))))
                      (push (pairlis '(:i-id 
                                       :i-origin :i-register :i-format
                                       :i-difficulty :i-category :i-input :i-wf
                                       :i-length :i-comment :i-author :i-date)
                                     (list (+ index base)
                                           origin register format
                                           difficulty category 
                                           (if oinput input ninput) wf
                                           length comment author date))
                          item)
                    (when phenomenon
                      (push (pairlis '(:ip-id :i-id :p-id :ip-author :ip-date)
                                     (list base base p-id author date))
                            item-phenomenon))
                    (incf base))))))
            (setf context nil))

      (close stream)
      (when meter (meter :value (get-field :end meter)))
      (values 
       (nreverse item) (nreverse phenomenon) (nreverse item-phenomenon)))))

(defun normalize-item (string)
  (flet ((punctuationp (char)
           (and *import-tsnlp-style-p*
                (member char *import-punctuation* :test #'char=)))
         (whitespacep (char)
           (member char '(#\Space #\Tab #\Newline) :test #'char=)))
    (let ((string (string-trim '(#\Space #\Tab #\Newline) string))
          (result (make-array 4096
                              :element-type 'character
                              :adjustable t :fill-pointer 0))
          (n 0)
          (previous :whitespace)
          foo)
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
      (when *tsdb-preprocessing-hook*
        (multiple-value-setq (foo n) 
          (call-safe-hook *tsdb-preprocessing-hook* string)))
      (setf foo foo)
      (values (string-right-trim '(#\Space) result) (or n -1)))))

(defun classify-item (string)
  (let ((result nil)
        (offset 0))
    (loop
        for stablep = t
        do
          (loop
              for (mark . key) in *import-marks*
              when (char= (char string offset) mark) do
                (push (cons key t) result)
                (incf offset)
                (setf stablep nil))
        until stablep)
    (loop
        with length = (length string)
        for key in *import-email-headers*
        for end = (min length (+ offset (length key)))
        when (string= string key :start1 offset :end1 end) do
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
        (push (cons :category "XP") result))))
    (values offset result)))
