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
;;;   know to inspect *morphs* and run the preprocessor stand-alone to debug
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

#+:allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :process))

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
    (sys:os-wait nil *sppp-pid*)
    (setf *sppp-pid* nil)))

(defun sppp-setup-morphs (tokens)
  (loop
      for i from 0
      for token in tokens
      for form = (rest (assoc :form token))
      for analyses = 
        (loop
            for analysis in (rest (assoc :analyses token))
            for stem = (string-upcase (rest (assoc :stem analysis)))
            for inflection = (rest (assoc :inflection analysis))
            for irule = (unless (string= inflection "zero")
                          (intern (format 
                                   nil 
                                   "~@:(~a~)~a" 
                                   inflection *lex-rule-suffix*)
                                  :lkb))
            collect (cons stem (when irule (list (list irule form)))))
      for edge = (make-morph-edge :id i :word form :morph-results analyses)
      do
        (setf (aref *morphs* i) edge)))

(defun sppp (text &key (stream *sppp-stream*))

  (when (streamp stream)
    (when (output-stream-p stream)
      (setf (stream-external-format stream) (excl:find-external-format :utf-8))
      (format
       stream
       "<?xml version='1.0' encoding='utf-8'?><text>~a</text>~%~a~%"
       (xml-escape-string text) #\page)
      (force-output stream))
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
                   (setf *sppp-input-chart* 
                     (adjust-array *sppp-input-chart* (list size 2)))
                 when (char= c #\page) do
                   (return n)
                 while c do (vector-push c *sppp-input-buffer*))))
      (when (and (numberp n) (> n 1))
        (multiple-value-bind (pxml condition) 
            (ignore-errors
             (xml:parse-xml (string-trim '(#\newline) *sppp-input-buffer*)))
          (if condition
            (format
             t
             "sppp(): error parsing XML (~a characters)~%"
             n)
            (when (eq (first (second pxml)) '|segment|)
              (sppp-process-segment (rest (second pxml))))))))))

(defun sppp-process-segment (segment)
  (loop
      for i from 0 to (- (array-dimension *sppp-input-buffer* 0) 1)
      do
        (setf (aref *sppp-input-chart* i 0) nil)
        (setf (aref *sppp-input-chart* i 1) nil))
  (let (tokens)
    (loop
        for element in segment
        for token = (when (consp element) (sppp-process-token element))
        for from = (rest (assoc :from token))
        for to = (rest (assoc :to token))
        when (and from to) do 
          (push token (aref *sppp-input-chart* from 0))
          (push token (aref *sppp-input-chart* to 1)))
    (loop
        with n = 0
        for i from 0 to (- (array-dimension *sppp-input-buffer* 0) 1)
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
        (let* ((base (first element))
               (stem (sppp-xml-get base '|stem|))
               (inflection (sppp-xml-get base '|inflection|)))
          (push (pairlis '(:stem :inflection) (list stem inflection)) 
                analyses))
      finally (return (pairlis '(:form :from :to :analyses)
                               (list form from to (nreverse analyses))))))

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
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        when (member c '(#\& #\< #\> #\' #\") :test #'char=) do
          (vector-push #\& result)
          (case c
            (#\&
             (vector-push #\a result)
             (vector-push #\m result)
             (vector-push #\p result)
             (decf padding 4))
            (#\<
             (vector-push #\l result)
             (vector-push #\t result)
             (decf padding 3))
            (#\>
             (vector-push #\g result)
             (vector-push #\t result)
             (decf padding 3))
            (#\'
             (vector-push #\a result)
             (vector-push #\p result)
             (vector-push #\o result)
             (vector-push #\s result)
             (decf padding 5))
            (#\"
             (vector-push #\q result)
             (vector-push #\u result)
             (vector-push #\o result)
             (vector-push #\t result)
             (decf padding 5)))
          (vector-push #\; result)
          (when (<= padding 0)
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
        else do
          (vector-push c result)
        finally
          (return result))
    string))
