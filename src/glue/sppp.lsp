;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-


;;; Copyright (c) 2000--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: sppp.lsp
;;;      module: simple preprocessor protocol
;;;     version: 0.0 (27-feb-03)
;;;  written by: oe, celi torino
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; ToDo
;;;
;;; - respect `encoding' header in incoming message: read in 8-bit mode and use
;;;   XML header to explicitly convert strings; arguably the XML parser might
;;;   be expected to do this for us, but it seems to not respect the encoding
;;;   declaration and just return what was read.
;;; - experiment with generic lexical entries that are activaed on the basis of
;;;   POS assignments.
;;; - write DTD and experiment with validating parser; the way we dissect the
;;;   parsed XML feels overly clumsy for the time being.
;;; - add more tracing and debugging support; currently grammarians need to
;;;   know to inspect *tchart* and run the preprocessor stand-alone to debug
;;;   the integration.
;;; - investigate ways of allowing tokenization ambiguity; presumably, this 
;;;   would have to be part of a full refactoring of the LKB preprocessing and
;;;   lexical look-up machinery, and ann would want to take part in that.
;;;

(in-package :lkb)

(defvar *sppp-pid* nil)

(defvar *sppp-stream* nil)

(defparameter *sppp-application* nil)

(defparameter *sppp-input-buffer* 
  (make-array 2048 :element-type 'character :adjustable nil :fill-pointer 0))

(defparameter *sppp-input-chart*
  (make-array '(2048 2)))

(defvar *sppp-debug-p* nil)

(defun initialize-sppp ()

  (when *sppp-application*
    (shutdown-sppp)
    (let (foo)
      (multiple-value-setq (*sppp-stream* foo *sppp-pid*)
        (run-process *sppp-application*
                     :wait nil
                     :output :stream :input :stream :error-output nil))
      ;;
      ;; this may seem silly: suppress compiler warning about unused .foo.
      ;;
      (when foo (setf foo foo)))))

(defun shutdown-sppp ()

  (when *sppp-stream*
    (close *sppp-stream*)
    (setf *sppp-stream* nil))
  (when *sppp-pid*
    (ignore-errors
     (run-process "kill -HUP ~d" *sppp-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -TERM ~d" *sppp-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null")
     (run-process "kill -QUIT ~d" *sppp-pid* 
                  :wait t :output "/dev/null" :error-output "/dev/null"))
    #+:allegro
    (sys:os-wait nil *sppp-pid*)
    (setf *sppp-pid* nil)
    (setf *morph-option* :default)))

(defun sppp-setup-morphs (tokens)
  (loop
      for i from 0
      for token in tokens
      for form = (rest (assoc :form token))
      for start = (or (rest (assoc :start token)) -1)
      for end = (or (rest (assoc :end token)) -1)
      for from = (or (rest (assoc :from token)) -1)
      for to = (or (rest (assoc :to token)) -1)
      do (add-token-edge form (string-upcase form) start end from to)
      do
        (loop
            for analysis in (rest (assoc :analyses token))
            for stem = (string-upcase (rest (assoc :stem analysis)))
            for inflection = (rest (assoc :inflection analysis))
            for rules = (rest (assoc :rules analysis))
            when rules do
              (add-morpho-stem-edge
               stem
               (loop
                   for rule in rules
                   collect (list (rest (assoc :id rule))
                                 (rest (assoc :form rule))))
               i (+ 1 i) form form start end from to)
            else do
              (let ((irule (unless (or (null inflection)
                                       (string= inflection "zero"))
                             (intern (format 
                                      nil 
                                      "~@:(~a~)~a" 
                                      inflection *lex-rule-suffix*)
                                     :lkb))))
                (add-morpho-stem-edge
                 stem (when irule (list (list irule form)))
                 i (+ 1 i) form form start end from to)))))

(defun sppp (text &key (stream *sppp-stream*))

  (when (streamp stream)
    (when (output-stream-p stream)
      (setf (stream-external-format stream) (excl:find-external-format :utf-8))
      (format
       stream
       "<?xml version='1.0' encoding='utf-8'?><text>~a</text>~%~a~%"
       (xml-escape-string text) #\page)
      (force-output stream))
    (let ((n (array-dimension *sppp-input-chart* 0)))
      (when (>= (length text) n)
        (setf *sppp-input-chart* 
          (adjust-array *sppp-input-chart* (list n 2)))))
    (setf (stream-external-format stream) (excl:find-external-format :utf-8))
    (let ((*package* (find-package :lkb))
          (n (loop
                 with size = (array-dimension *sppp-input-buffer* 0)
                 initially (setf (fill-pointer *sppp-input-buffer*) 0)
                 for n from 1
                 for c = (read-char stream nil nil)
                 when (null c) do 
                   (format
                    t
                    "sppp(): premature end of file (after ~a characters)~%" 
                    n)
                   (return)
                 when (= n size) do
                   (incf size size)
                   (setf *sppp-input-buffer* 
                     (adjust-array *sppp-input-buffer* size))
                 when (char= c #\page) do
                   (return n)
                 while c do (vector-push c *sppp-input-buffer*))))
      (when (and (numberp n) (> n 1))
        (multiple-value-bind (xml condition) 
            (ignore-errors
             (xml:parse-xml (string-trim '(#\newline) *sppp-input-buffer*)))
          (if condition
            (format
             t
             "sppp(): error parsing XML (~a characters)~%"
             n)
            (when (eq (first (second xml)) '|segment|)
              (sppp-process-segment (rest (second xml))))))))))

(defun sppp-process-segment (segment)
  (loop
      for i from 0 to (- (array-dimension *sppp-input-chart* 0) 1)
      do
        (setf (aref *sppp-input-chart* i 0) nil)
        (setf (aref *sppp-input-chart* i 1) nil))
  (let (tokens)
    (loop
        for element in segment
        for token = (when (consp element) (sppp-process-token element))
        for form = (rest (assoc :form token))
        for from = (rest (assoc :from token))
        for to = (rest (assoc :to token))
        when (and from to (not (punctuationp form))) do 
          (push token (aref *sppp-input-chart* from 0))
          (push token (aref *sppp-input-chart* to 1)))
    (loop
        with n = 0
        for i from 0 to (- (array-dimension *sppp-input-chart* 0) 1)
        for foo = (aref *sppp-input-chart* i 0)
        when foo do
          (loop
              for token in foo
              do
                (push (nconc (pairlis '(:start :end) (list n (+ n 1))) token)
                      tokens))
          (incf n))
    (nreverse tokens)))

(defun sppp-process-token (token)
  (loop
      with analyses = nil
      with base = (first token)
      with from = (sppp-xml-get base '|from| :type :number)
      with to = (sppp-xml-get base '|to| :type :number)
      with form = (sppp-xml-get base '|form|)
      for element in (rest token)
      when (consp element) do
        (push (sppp-process-analysis element) analyses)
      finally (return (pairlis '(:form :from :to :analyses)
                               (list form from to (nreverse analyses))))))

(defun sppp-process-analysis (analysis)
  (let* ((base (first analysis))
         (stem (sppp-xml-get base '|stem|))
         (inflection (sppp-xml-get base '|inflection|))
         rules)
    (loop
        for element in (rest analysis)
        when (consp element) do
          (let* ((base (first element))
                 (id (let ((foo (sppp-xml-get base '|id|)))
                       (when (stringp foo)
                         (intern (string-upcase foo) :lkb)))) 
                 (form (sppp-xml-get base '|form|)))
            (push (pairlis '(:id :form) (list id form)) rules)))
    (when (and inflection rules)
      (format
       t
       "sppp-process-analysis(): ignoring superfluous `inflection' value.~%"))
    (pairlis '(:stem :inflection :rules)
             (list stem (unless rules inflection) (nreverse rules)))))

(defun sppp-xml-get (element attribute &key type)
  (loop
      for attributes = (rest element) then (rest (rest attributes))
      while attributes
      when (eq (first attributes) attribute) do
        (return
          (case type
            (:number
             (read-from-string (second attributes) nil nil))
            (t
             (second attributes))))))

(defun xml-escape-string (string)
  (if (and string (stringp string))
    (loop
        with padding
        = (loop
              for c across string
              when (char= c #\&) sum 4
              else when (or (char= c #\<) (char= c #\>)) sum 3
              else when (or (char= c #\') (char= c #\")) sum 5)
        with result = (make-string (+ (length string) padding))
        with i = -1
        for c of-type character across (the string string)
        when (char= c #\&) do
          (setf (schar result (incf i)) #\&)
          (setf (schar result (incf i)) #\a)
          (setf (schar result (incf i)) #\m)
          (setf (schar result (incf i)) #\p)
          (setf (schar result (incf i)) #\;)
        else when (char= c #\<) do
          (setf (schar result (incf i)) #\&)
          (setf (schar result (incf i)) #\l)
          (setf (schar result (incf i)) #\t)
          (setf (schar result (incf i)) #\;)
        else when (char= c #\>) do
          (setf (schar result (incf i)) #\&)
          (setf (schar result (incf i)) #\g)
          (setf (schar result (incf i)) #\t)
          (setf (schar result (incf i)) #\;)
        else when (char= c #\') do
          (setf (schar result (incf i)) #\&)
          (setf (schar result (incf i)) #\a)
          (setf (schar result (incf i)) #\p)
          (setf (schar result (incf i)) #\o)
          (setf (schar result (incf i)) #\s)
          (setf (schar result (incf i)) #\;)
        else when (char= c #\") do
          (setf (schar result (incf i)) #\&)
          (setf (schar result (incf i)) #\q)
          (setf (schar result (incf i)) #\u)
          (setf (schar result (incf i)) #\o)
          (setf (schar result (incf i)) #\t)
          (setf (schar result (incf i)) #\;)
        else do
          (setf (schar result (incf i)) c)
        finally
          (return result))
    string))
