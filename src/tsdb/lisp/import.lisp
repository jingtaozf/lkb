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

(defparameter *import-separator* ";;")

(defparameter *import-punctuation* 
  '(#\, #\. #\! #\? #\; #\: #\- #\" #\' #\` #\( #\)))

(defparameter *import-origin* "unknown")

(defparameter *import-register* "formal")

(defparameter *import-difficulty* 1)

(defparameter *import-category* "")

(defun do-import-database (source target &key absolute meter)
  
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
              (let ((sfile (make-pathname :directory source :name relation)))
                (when (probe-file sfile)
                  (cp sfile 
                      (make-pathname :directory tpath :name relation))))
              (let ((sfile (make-pathname :directory source :name compressed)))
                (when (probe-file sfile)
                  (cp sfile 
                      (make-pathname :directory tpath :name compressed))))
              (when increment (meter-advance increment))
            finally
              (let ((sfile 
                     (make-pathname :directory source :name "relations")))
                (when (probe-file sfile)
                  (cp sfile 
                      (make-pathname :directory tpath :name "relations")))))
        (when meter (meter :value (get-field :end meter)))
        schema)))))

(defun do-import-items (file data &key 
                                  absolute
                                  (create t)
                                  (overwrite t)
                                  (origin (or *import-origin* "unknown"))
                                  (register (or *import-register* "formal"))
                                  (difficulty (or *import-difficulty* 1))
                                  (category (or *import-category* ""))
                                  comment 
                                  (separator (or *import-separator* ";;"))
                                  meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((path (if absolute (namestring data) (find-tsdb-directory data)))
         (ifile (make-pathname :directory path :name "item"))
         (status (verify-tsdb-directory path :absolute t))
         (rmeter (madjust * meter 0.4))
         (imeter (madjust + (madjust * meter 0.6) (mduration rmeter))))
    (cond
     ((null (probe-file file))
      (format
       *tsdb-io*
       "import-items(): unable to open item file `~a'.~%"
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
          (cp relations target))
        (select "i-id" :integer "item" nil path :absolute t))
      (when overwrite 
        (with-open-file (foo ifile :direction :output
                         :if-does-not-exists :create 
                         :if-exists :supersede)))
      (let* ((ids (select "i-id" :integer "item" nil path :absolute t))
             (ids (map 'list #'(lambda (foo) (get-field :i-id foo)) ids))
             (ids (remove nil ids))
             (base (if ids (+ 1 (apply #'max ids)) 1))
             (items (read-items-from-ascii-file file
                                                :base base
                                                :origin origin
                                                :register register
                                                :difficulty difficulty
                                                :category category
                                                :comment comment
                                                :separator separator
                                                :meter rmeter)))
        (if items
          (insert path "item" items :absolute t :meter imeter)
          4))))))

(defun read-items-from-ascii-file (file &key (base 1)
                                             (origin "unknown")
                                             (register "formal")
                                             (difficulty 1)
                                             (category "")
                                             comment
                                             (separator ";;")
                                             meter)
  
  (when meter (meter :value (get-field :start meter)))
  (let* ((stream (open file :direction :input :if-does-not-exist :error))
         (format "none")
         (author (current-user))
         (date (current-time))
         items)
    (when stream
      (loop
          for line = (read-line stream nil nil)
          while line
          for string = (string-trim '(#\Space #\Tab) line)
          unless (zerop (length string))
          do
            (let* ((start (if (char= (char string 0) #\") 1 0))
                   (wf (if (char= (char string start) #\*) 0 1))
                   (string (subseq string (if (zerop wf) (+ start 1) start)))
                   (break (and separator 
                               (search separator string 
                                       :test #'string= :from-end t)))
                   (comment 
                    (or comment
                        (when break
                          (string-trim 
                           (list #\Space #\Tab) 
                           (subseq string (+ break (length separator)))))
                        ""))
                   (string (if break
                             (subseq string 0 (max break 0)) 
                             string))
                   (string (string-trim '(#\Space #\Tab) string))
                   (length (- (length string) 1))
                   (end (when (and (not (zerop start))
                                   (char= (char string length) #\"))
                          length))
                   (string (subseq string 0 end))
                   (input (string-trim '(#\Space #\Tab) string)))
              (multiple-value-bind (input length)
                  (normalize-item input)
                (unless (zerop length)
                  (push (pairlis '(:i-id :i-origin :i-register :i-format
                                   :i-difficulty :i-category :i-input :i-wf
                                   :i-length :i-comment :i-author :i-date)
                                 (list base origin register format
                                       difficulty category input wf
                                       length comment author date))
                        items)
                  (incf base)))))
      (close stream)
      (when meter (meter :value (get-field :end meter)))
      (nreverse items))))

(defun normalize-item (string)
  (flet ((punctuation-p (char)
           (member char *import-punctuation* :test #'char=))
         (whitespace-p (char)
           (member char '(#\Space #\Tab #\Newline) :test #'char=)))
    (let ((string (string-trim '(#\Space #\Tab #\Newline) string))
          (result (make-array 4096
                              :element-type 'character
                              :adjustable t :fill-pointer 0))
          (n 0)
          (previous :whitespace))
      (loop
          for i from 0 to (- (length string) 1) by 1
          for current = (char string i)
          do
            (cond
             ((and (whitespace-p current) (eq previous :regular))
              (vector-push-extend #\Space result 1024)
              (setf previous :whitespace)
              (incf n))
             ((and (whitespace-p current) (eq previous :whitespace)))
             ((and (punctuation-p current) (eq previous :regular))
              (vector-push-extend #\Space result 1024)
              (vector-push-extend current result 1024)
              (vector-push-extend #\Space result 1024)
              (setf previous :whitespace)
              (incf n))
             ((and (punctuation-p current) (eq previous :whitespace))
              (vector-push-extend current result 1024)
              (vector-push-extend #\Space result 1024)
              (setf previous :whitespace))
             (t
              (vector-push-extend current result 1024)
              (setf previous :regular))))
      (when (eq previous :regular) (incf n))
      (values (string-right-trim '(#\Space) result) n))))
            