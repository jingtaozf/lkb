;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB; Coding: utf-8; -*-


;;; Copyright (c) 2000--2018 Stephan Oepen; see `LICENSE' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: repp.lsp
;;;      module: regular expression-based preprocessor (some rough utilities)
;;;     version: 0.0 (30-jan-03)
;;;  written by: oe, csli stanford
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
;;; rework to implement the following specification:
;;;
;;; @ version
;;;
;;; : token separator (applied once string rewriting has saturated)
;;;
;;; ! replace
;;;
;;; #42
;;;
;;; # grouping: name set of rules # 42; group is not executed unless called
;;;
;;; >42 group call; groups can be recursive and stop when there was no match
;;;
;;; </foo/bar file inclusion: `/foo/bar' is read at this point
;;;

(in-package :lkb)

(defparameter *repp-characterize-p* t)

(defparameter *repp-characterization-beam* 2)

(defparameter *repp-characterization-range* 500)

(defparameter *repp-unique-p* t)

(defparameter *repp-debug-p* t)

(defparameter *repps* nil)

(defparameter *repp-calls* nil)

(defparameter *repp-interactive* nil)

(defparameter *repp-iterations* 4711)

(defstruct repp
  id
  version
  tokenizer
  rules
  groups)

(defmethod print-object ((object repp) stream)
  (format 
   stream 
   "#[REPP ~(~a~) (~d rules~@[ @ `~a'~])]"
   (repp-id object)
   (length (repp-rules object))
   (repp-tokenizer object)))

(defstruct fsr
  type
  source
  scanner
  target)

(defstruct token
  id form stem from to (start -1) (end -1) tags ersatz)

(defun read-repp (file &key id (repp (make-repp) reppp) (prefix ""))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (let* ((path (pathname file))
             (type (pathname-type path))
             (name (pathname-name path))
             (id (or id (intern (string-upcase name) :keyword))))
        (unless reppp (setf (repp-id repp) id))
        (format 
         t 
         "~&~aread-repp(): reading file `~a~@[.~a~]'.~%" 
         prefix name type)
        (loop
            with separator = (ppcre:create-scanner "\\t+")
            with repps = (list repp)
            for n from 1
            for line = (read-line stream nil nil)
            for length = (length line)
            for c = (unless (zerop length) (char line 0))
            when (and c (not (char= c #\;))) do
              (multiple-value-bind (start end) (ppcre:scan separator line)
                (cond
                 ((char= c #\@)
                  (let* ((version (subseq line 1 end))
                         (version (if (string= version "$Date: " :end1 7)
                                    (subseq version 7 (- (length version) 2))
                                    version)))
                    (setf (repp-version (first repps)) 
                      (string-trim '(#\Space) version))))
                 ((char= c #\<)
                  (let* ((name (subseq line 1 end))
                         (file (or (probe-file name)
                                   (probe-file (merge-pathnames name path)))))
                    (if file
                      (read-repp
                       file :repp repp :prefix (format nil "~a  " prefix))
                      (format
                       t
                       "~aread-repp(): [~d] unable to include `~a'~%"
                       prefix n name))))
                 ((char= c #\>)
                  (let* ((name (subseq line 1 end))
                         (id (or (parse-integer name :junk-allowed t)
                                 (intern (string-upcase name) :keyword)))
                         (fsr (make-fsr :type :call :source id)))
                    (push fsr (repp-rules (first repps)))))
                 ((char= c #\:)
                  (let ((tokenizer (subseq line 1 end)))
                    (setf (repp-tokenizer (first repps)) tokenizer)))
                 ((char= c #\#)
                  (if (ppcre:scan "^#[ \\t]*$" line)
                    (if (rest repps)
                      (let ((repp (pop repps)))
                        (setf (repp-rules repp) (nreverse (repp-rules repp)))
                        (push repp (repp-groups (first repps))))
                      (format
                       t
                       "~aread-repp(): [~d] spurious group close~%"
                       prefix n))
                    (let ((id (parse-integer line :start 1 :junk-allowed t)))
                      (if (numberp id)
                        (push (make-repp :id id) repps)
                        (format
                         t
                         "~aread-repp(): [~d] invalid group identifier `~a'~%"
                         prefix n (subseq line 1))))))
                 ((member c '(#\! #\- #\+ #\^) :test #'char=)
                  (if (and start end)
                    (let* ((type (case c
                                   (#\! :replace)))
                           (source (subseq line 1 start))
                           (target (subseq line end))
                           (scanner
                            (ignore-errors
                             (ppcre:create-scanner 
                              source)))
                           (match (make-fsr :type type :source source
                                            :scanner scanner :target target)))
                      (if scanner
                        (push match (repp-rules (first repps)))
                        (format
                         t
                         "~aread-repp(): [~d] invalid pattern `~a'~%"
                         prefix n source)))
                    (format
                     t
                     "~aread-repp(): [~d] invalid `~a'~%"
                     prefix n line)))
                 (t
                  (format
                   t
                   "~aread-repp(): [~d] ignoring unknown rule type `~a'~%"
                   prefix n c))))
            when (null line) do
              ;;
              ;; _fix_me_
              ;; check for left-over groups, i.e. ones not closed off by `#'
              ;;
              (unless reppp
                (setf (repp-rules repp)
                  (nreverse (repp-rules repp))))
              (unless reppp
              #+:debug
                (format t "~a~%" repp)
                (push repp *repps*))
              (return repp))))))

(defun repp (input &key (repp (first *repps*))
                        (calls *repp-calls*)
                        (verbose *repp-debug-p*)
                        (format :pet))
  
  (when (keywordp repp)
    (setf repp
      (loop
          for foo in *repps*
          when (eq (repp-id foo) repp) return foo)))
  (when (null repp)
    (return-from repp (and (eq format :string) input)))
  
  (let ((*repp-characterize-p*
         (unless (smember format '(:string :lkb)) *repp-characterize-p*))
        (string input)
        (length 0)
        tokens)
    (loop
        with rules = (copy-list (repp-rules repp))
        with repps = (list repp)
        with matchp with counts
        for rule = (pop rules)
        while rule
          #+:debug do
          #+:debug
          (format
           t
           "~&[~@[+~]~a ~a] ~a~%  :~(~a~) ~a~%~%"
           matchp n (length rules) (first repps)
           (fsr-type rule) (fsr-source rule))
        when (eq (fsr-type rule) :call) do
          (let* ((id (fsr-source rule))
                 (repp
                  (loop
                      for repp 
                      in (if (numberp id) (repp-groups (first repps)) *repps*)
                      when (eql id (repp-id repp)) return repp)))
            (unless repp
              (error "repp(): invalid call target `~(~a~)'" id))
            (when (or (numberp id)
                      (and calls (symbolp calls))
                      (smember id calls))
              (push (make-fsr :type :pop) rules)
              (setf rules (append (repp-rules repp) rules))
              (push repp repps)
              (push 0 counts)
              (setf matchp nil)
              (when (eq verbose :trace) (format t "~&>~(~a~)~%~%" id))))
        else when (eq (fsr-type rule) :pop) do
          ;;
          ;; _fix_me_
          ;; i think we should not call external groups iteratively, they are
          ;; primarily there to modularize the rule set; if we actually wanted
          ;; to iterate an external group, the group call could be put into an
          ;; internal, numeric group.                            (3-feb-09; oe)
          ;;
          (cond
           ((and matchp
                 (numberp (first counts))
                 (or (null *repp-iterations*)
                     (< (first counts) *repp-iterations*)))
            (push (make-fsr :type :pop) rules)
            (setf rules (append (repp-rules (first repps)) rules))
            (incf (first counts))
            (when (eq verbose :trace)
              (format t ">~(~a~)~%~%" (repp-id (first repps)))))
           (t
            (when (eq verbose :trace)
              (format t "<~(~a~)~%~%" (repp-id (first repps))))
            (pop repps)
            (pop counts)))
          (setf matchp nil)
        else do
          (let* ((scanner (fsr-scanner rule))
                 (target (fsr-target rule))
                 (match (ppcre:regex-replace-all scanner string target)))
            (unless (string= string match)
              (setf matchp t)
              (when (eq verbose :trace)
                (format
                 t
                 "~&|~a|~%  |~a|~%  |~a|~%~%"
                 (fsr-source rule) string match))
              (setf string match))))
    (setf tokens
      (loop
          with result
          with i = 0
          with id = 0
          for form in (ppcre:split (repp-tokenizer repp) string)
          unless (string= form "")
          do
            (let* ((from i) (to (incf i))
                   (token
                    (make-token :id id :form form :from from :to to)))
              (incf length)
              (incf id)
              (push token result)
              (when verbose
                (format t "  (~a) [~a:~a] |~a|~%" id from to form)))
          finally (return (nreverse result))))
    
    (when *repp-characterize-p*
      (loop
          with alignment = (repp-align input tokens)
          with last = 0
          with offset = 0
          for token in tokens
          for n = (length (token-form token))
          for start = nil for end = nil
          do
            (loop
                for i from offset to (+ offset n -1)
                for index = (aref alignment i)
                when index do
                  (unless start (setf start index))
                  (setf end (+ index 1)))
            (when (null start) (setf start end))
            (when (null start) (setf start last end last))
            (setf (token-start token) start)
            (setf (token-end token) end)
            (setf last end)
            (incf offset n)))
    
    (values (repp-format tokens format) length)))

(defun repp-align (string tokens &optional (start 0) solutions (space 0))
  (unless (stringp tokens)
    (let ((forms (loop for token in tokens collect (token-form token))))
      (setf tokens (format nil "~{~a~}" forms)))
    ;;
    ;; each solution is a vector, with one cell per character in the .tokens.
    ;; sequences (i.e. no cells corresponding to whitespace in the original
    ;; .string.), plus two extra cells at the end for bookkeeping: the total
    ;; count of alignments at position .n., and the highest index into .string.
    ;; at position .n. + 1.
    ;;
    (let* ((n (length tokens))
           (solution (make-array (+ n 2))))
      (setf (aref solution n) 0)
      (setf solutions (list solution))))
  (let ((n (length tokens)))
    (if (>= start (length tokens))
      (loop
          with top = (first solutions)
          for solution in (rest solutions)
          when (> (aref solution n) (aref top n))
          do (setf top solution)
          finally (return top))
      (let ((l (length string))
            matches)
        (when (and (< start l) (smember (char string start) '(#\space #\tab)))
          (incf space))
        (loop
            with b = (char tokens start)
            with range = (when (numberp *repp-characterization-range*)
                           (round *repp-characterization-range* 2))
            for i from (if range (max 0 (+ (- start range) space)) 0)
            to (if range (min (+ start range space) (- l 1)) (- l 1))
            for c = (char string i)
            when (or (char= b c)
                     (and (char= c #\")
                          (or (char= b #-openmcl #\“ #+openmcl #\U+201C)
                              (char= b #-openmcl #\” #+openmcl #\U+201D)))
                     (and (char= c #\')
                          (or (char= b #-openmcl #\‘ #+openmcl #\U+2018)
                              (char= b #-openmcl #\’ #+openmcl #\U+2019)))
                     (and (char= c #\`)
                          (char= b #-openmcl #\‘ #+openmcl #\U+2018)))
            do
              (loop
                  for solution in solutions
                  when (or (null (aref solution (+ n 1)))
                           (< (aref solution (+ n 1)) i))
                  do
                    ;;
                    ;; we assume that .solutions. are ordered according to how
                    ;; many alignment points they contain, with larger
                    ;; alignments at the top of the list.  hence, if there is
                    ;; an earlier match with the same alignment point for the
                    ;; current token index (i.e. the same .i. at position
                    ;; .start.), then we are guaranteed to find larger matches
                    ;; before smaller ones; for example #(0 1 ...) prior to 
                    ;; #(- 1 ...).  avoid finding new solutions for the same 
                    ;; alignment point that are inferior to an existing 
                    ;; solution; it cannot possibly end up better.
                    ;;
                    (unless (loop
                                for match in matches
                                thereis (and (eql (aref match start) i)
                                             (> (aref match n)
                                                (aref solution n))))
                      (let ((copy (copy-seq solution)))
                        (setf (aref copy start) i)
                        (incf (aref copy n))
                        (setf (aref copy (+ n 1)) i)
                        (push copy matches)))))
        (labels ((n (solution) (aref solution n)))
          (let* ((solutions (sort (nconc matches solutions) #'> :key #'n))
                 ;;
                 ;; *repp-characterization-beam* controls how far we search 
                 ;; into the space of candidate but (at present) inferior 
                 ;; solutions.  in principle, it may yield a better alignment
                 ;; to skip over a sub-sequence, at some point, leaving 
                 ;; characters unaligned.  a beam size of five, say, will keep
                 ;; solutions available that are up to five alignments worse
                 ;; than the current best one.
                 ;;
                 (beam
                  (loop
                      with top = (- (aref (first solutions) n)
                                    *repp-characterization-beam*)
                      for solution in solutions
                      while (>= (aref solution n) top)
                      collect solution)))
            (repp-align string tokens (incf start) beam space)))))))

(defun repp-unique (tokens)
  (let ((tokens (sort tokens #'< :key #'token-id))
        result)
    (labels ((token= (foo bar)
               (and (equal (token-form foo) (token-form bar))
                    (equal (token-stem foo) (token-stem bar))
                    (eql (token-from foo) (token-from bar))
                    (eql (token-to foo) (token-to bar))
                    (eql (token-start foo) (token-start bar))
                    (eql (token-end foo) (token-end bar))
                    (equal (token-tags foo) (token-tags bar)))))
      (loop
          for token in tokens
          unless (member token result :test #'token=)
          do (push token result)))
    (nreverse result)))

(defun repp-format (tokens format &key (uniquep *repp-unique-p*))
  (when uniquep (setf tokens (repp-unique tokens)))
  (case format
    (:raw
     tokens)
    (:list
     (loop
         for token in tokens
         collect
           (pairlis '(:id :start :end :form :stem :tag)
                    (list (token-id token)
                          (token-start token) (token-end token)
                          (token-form token)
                          (or (token-stem token) (token-form token))
                          (first (token-tags token))))))
    ((:string :lkb)
     (let ((forms (loop
                      for token in tokens
                      collect (or (token-ersatz token) (token-form token)))))
       (if (eq format :string)
         (format nil "~{~a~^ ~}" forms)
         forms)))
    (:pet
     (loop
         for token in tokens
         for form = (token-form token)
         for ersatz = (token-ersatz token)
         for yy = (format 
                   nil 
                   "(~d, ~d, ~d, ~:[~*~*~;<~a:~a>, ~]~
                     1, \"~a\"~@[ \"~a\"~], 0, \"null\"~
                     ~@[,~{ ~s ~,4f~}~])" 
                   (token-id token)
                   (token-from token) (token-to token)
                   *repp-characterize-p* 
                   (token-start token) (token-end token)
                   (escape-string (or ersatz form))
                   (when ersatz (escape-string form))
                   (token-tags token))
         collect yy into result
         finally (return (format nil "~{~a~^ ~}" result))))
    (:sppp
     (loop 
         for token in tokens
         for form = (or (token-ersatz token) (token-form token))
         for from = (token-from token)
         for to = (token-to token)
         for start = (token-start token)
         for end = (token-end token)
         collect (pairlis '(:start :end :from :to :form)
                          (list from to start end form))))))

(defun escape-string (string &key (syntax :c))
  (declare (ignore syntax))
           
  (if string
    (loop
        with padding = 128
        with length = (+ (length string) padding)
        with result = (make-array length
                                  :element-type 'character
                                  :adjustable nil :fill-pointer 0)
        for c across string
        when (or (char= c #\") (char= c #\\)) do
          (vector-push #\\ result)
          (vector-push c result)
          (when (zerop (decf padding))
            (setf padding 42)
            (incf length padding)
            (setf result (adjust-array result length)))
        else do
          (vector-push c result)
        finally
          (return result))
    ""))

(defun clear-repp ()
  (setf *repps* nil))

(defun repp-for-pet (string &optional tagger &rest arguments)
  (let* ((*repp-calls* (or (getf arguments :calls) *repp-calls*))
         (repp (or (getf arguments :repp) (first *repps*)))
         (raw (getf arguments :raw))
         (format (if raw :raw (or (getf arguments :format) :pet)))
         (verbose (getf arguments :verbose))
         (tokens (repp string :repp repp :format :raw :verbose verbose))
         (length (length tokens))
         (result
          (repp-format
           (if (or (keywordp tagger)
                   (and (consp tagger) (keywordp (first tagger))))
             (case (if (keywordp tagger) tagger (first tagger))
               (:tnt
                (apply 
                 #'tnt tokens
                 (unless (keywordp tagger) (rest tagger)))))
             tokens)
           format)))
    (let ((stream (getf arguments :stream)))
      (cond
       ((or (streamp stream) (eq stream t))
        (format stream "~a~%" result))
       ((stringp stream)
        (with-open-file (stream stream
                         :direction :output :if-exists :supersede)
          (format stream "~a~%" result)))
       (t                 
        (values result length))))))

(defparameter *taggers*
  #+:logon
  '((:tnt "${LOGONROOT}/bin/tnt -z100 ${LOGONROOT}/coli/tnt/models/wsj -" 2))
  #-:logon
  '((:tnt "tnt -z100 /user/oe/src/tnt/models/wsj -")))

(defun tnt (tokens &optional run &key (n 1 np))
  (unless (or run np)
    (loop
        for run in *taggers*
        when (eq (first run) :tnt)
        do (setf n (third run))))
  (labels ((commentp (string)
             (and (>= (length string) 2)
                  (characterp (char string 0)) (char= (char string 0) #\%)
                  (characterp (char string 1)) (char= (char string 1) #\%))))

    (let* ((tmp
            (let* ((tmp (or (getenv "TNTTMP") #+:logon (getenv "LOGONTMP")))
                   (tmp (and tmp (namestring (parse-namestring tmp)))))
              (or tmp "/tmp")))
           (run (or run 
		    (loop
			for run in *taggers*
                        when (eq (first run) :tnt)
			return (second run))
		    "tnt -z100 /user/oe/src/tnt/models/wsj -"))
	   (command (format nil "exec ~a" run *taggers*))
	   (input (format
                   nil "~a/.tnt.in.~a.~a"
                   tmp (current-user) (current-pid)))
	   (output (format
                    nil "~a/.tnt.out.~a.~a"
                    tmp (current-user) (current-pid)))
	   (length 0) analyses)
      (with-open-file (stream input :direction :output :if-exists :supersede)
        ;;
        ;; _fix_me_
        ;; this is unnecessary complicated, nowadays: REPP no longer creates
        ;; token-level ambiguity, hence we are guaranteed a linear sequence of
        ;; tokens (at least until i change my mind about REPP once again :-).
        ;;                                                      (25-jan-10; oe)
        (loop
            with i = -1
            for token in tokens
            for from = (token-from token)
            for form = (or (token-ersatz token) (token-form token))
            unless (= i from) do
              (setf form
                (cond
                 ((string= form "“") "``")
                 ((string= form "”") "''")
                 ((string= form "‘") "`")
                 ((string= form "’") "'")
                 ((string= form "…") "...")
                 ((string= form "—") "---")
                 ((string= form "–") "--")
                 (t form)))
              (setf i from)
              (incf length)
              (format stream "~a~%" form)
            finally (format stream "~%~%")))
      (run-process
       command :wait t 
       :input input :output output :if-output-exists :supersede
       :error-output "/dev/null" :if-error-output-exists :append)
      (with-open-file (stream output :direction :input)
        (loop
            with buffer = (make-array 512 
                                      :element-type 'character
                                      :adjustable t :fill-pointer 0)
            with i = 0
            for string = (read-line stream nil nil)
            while (and string (not (string= string "")))
            unless (commentp string) do 
              (incf i)
              (loop
                  with foo = nil
                  with n = 0
                  initially (setf (fill-pointer buffer) 0)
                  for c across string
                  when (char= c #\tab) do
                    (when (not (zerop (fill-pointer buffer)))
                      (push (if (and (evenp n) (not (zerop n)))
                              (read-from-string (copy-seq buffer))
                              (copy-seq buffer))
                            foo)
                      (setf (fill-pointer buffer) 0)
                      (incf n))
                  else do
                    (vector-push c buffer)
                  finally
                    (when (not (zerop (fill-pointer buffer)))
                      (push (read-from-string (copy-seq buffer)) foo))
                    (when foo
                      (push (nreverse foo) analyses)))))
      (loop
          with tags = (make-array (length analyses))
          for analysis in (nreverse analyses)
          for i from 0
          do (setf (aref tags i) (rest analysis))
          finally
            (loop
                for token in tokens
                for from = (token-from token)
                for analysis = (aref tags from)
                do (setf (token-tags token)
                     (loop
                         with n = (* 2 n)
                         for foo in analysis
                         while (< 0 n) collect foo do (decf n)))))
      (values tokens length))))

