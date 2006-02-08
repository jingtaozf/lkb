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
               target
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
     ((eq (get-field :status status) :ro)
      (format *tsdb-io* "import-items(): read-only database `~a'.~%" data)
      3)
     ((and (null tstatus) (null create))
      (format *tsdb-io* "import-items(): invalid database `~a'.~%" target)
      12)
     ((eq (get-field :status tstatus) :ro)
      (format *tsdb-io* "import-items(): read-only database `~a'.~%" target)
      13)
     (t
      (when (and create 
                 (or (null status)
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
                 (or (null tstatus)
                     (not (find "item" (read-database-schema tpath :absolute t)
                                :test #'string= :key #'first))))
        (unless (purge-directory tpath)
          (when (probe-file tpath) (delete-file tpath))
          (mkdir tpath))
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
              (:bitext
               (read-items-from-bitext-file
                file
                :base base :origin origin :register register
                :difficulty difficulty :category category
                :comment comment :separator separator :pseparator pseparator
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
          (declare (ignore foo bar))
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
            (when output
              (insert path "output" output :absolute t))
            (when parse (insert path "parse" parse :absolute t))
            (when result (insert path "result" result :absolute t))
            (when (and titem tpath)
              (insert tpath "item" titem :absolute t))
            (when (and tis tpath)
              (insert tpath "item-set" tis :absolute t))
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

(defun read-items-from-bitext-file (file &key (base 1)
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
         (delta 0)
         item phenomenon item-phenomenon output titem tis)
    (when stream
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
            (setf delta 0)
          else when (zerop length) do
            (setf context :newline)
            (setf rid nil) (setf rinput nil)
            (setf delta 0)
          else unless (zerop length) do
            (multiple-value-bind (string id)
                (strip-identifier string)
              (unless (numberp id)
                (setf id (if (eq context :item)
                           (+ (* index 10) (incf delta))
                           (* (incf index) 10))))
              (when (and (numberp last) (<= id last))
                (setf id (+ last 1)))
              (setf last id)
              (when (eq context :newline) 
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
                               (wf (cond
                                    ((get-field :illformed extras) 0)
                                    (t 1)))
                               (targetp (not (zerop (mod id 10)))))
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
                            ;;the polarity field is hard-coded for now.
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

(defun strip-identifier (string)
  (multiple-value-bind (start end) 
      (cl-ppcre:scan "^[0-9]+[a-z]\\. " string)
    (unless start
      (multiple-value-setq (start end)
        (cl-ppcre:scan "^\\[[0-9]{4}[a-z]\\]( |$)" string))
      (when (numberp start) (incf start)))
    (if (numberp end)
      (let ((suffix (subseq string end))
            (id (or (parse-integer string :start start :junk-allowed t) 0))
            (offset (or (case (when (>= end 3) (char string (- end 3)))
                          (#\a 0)
                          (#\b 1)
                          (#\c 2)
                          (#\d 3)
                          (#\e 4)
                          (#\f 5)
                          (#\g 6)
                          (#\h 7)
                          (#\i 8)
                          (#\j 9))
                        0)))
        (values suffix (+ (* id 10) offset)))
      string)))

(defun classify-item (string)
  (let ((result nil)
        (offset 0))
    (unless (string= string "")
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
      (when (string-equal (subseq string offset) "<p>")
        (setf result (acons :paragraph t nil)))
      (loop
          with length = (length string)
          for key in *import-email-headers*
          for end = (max (- (min length (+ offset (length key))) 1) 0)
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
          (push (cons :category "XP") result)))))
    (values offset result)))
