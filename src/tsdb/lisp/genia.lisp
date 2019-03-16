(in-package :itsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 2010 -- 2010 Stephan Oepen (oe@ifi.uio.no)
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

(defparameter *genia-tokenize-p* t)

(defparameter *genia-binary*
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (if root 
      (format nil "~a/bin/geniatagger~:[ -nt~;~]" root *genia-tokenize-p*)
      (format nil "geniatagger~:[ -nt~;~]" *genia-tokenize-p*)))
  #-:logon
  (format nil "geniatagger~:[ -nt~;~]" *genia-tokenize-p*))

(defparameter *genia-stream* nil)

(defparameter *genia-pid* nil)

(defparameter *genia-ne-p* t)

(defun genia-initialize ()
  (unless (and (streamp *genia-stream*) (open-stream-p *genia-stream*)
               (integerp *genia-pid*) 
               #+:null (probe-file (format nil "/proc/~a/stat" *genia-pid*)))
    (genia-shutdown)
    (setf *genia-binary*
      #+:logon
      (let* ((root (system:getenv "LOGONROOT"))
             (root (and root (namestring (parse-namestring root)))))
        (if root 
          (format nil "~a/bin/geniatagger~:[ -nt~;~]" root *genia-tokenize-p*)
          (format nil "geniatagger~:[ -nt~;~]" *genia-tokenize-p*)))
      #-:logon
      (format nil "geniatagger~:[ -nt~;~]" *genia-tokenize-p*))
    (let (foo)
      (multiple-value-setq (*genia-stream* foo *genia-pid*)
        (run-process (format nil "exec ~a" *genia-binary*)
         :wait nil :output :stream :input :stream 
         #-(or :sbcl :openmcl) :error-output #-(or :sbcl :openmcl) "/dev/null"
         #-(or :sbcl :openmcl) :if-error-output-exists 
         #-(or :sbcl :openmcl) :append))
      (setf foo foo))))

(defun genia-shutdown ()
  (ignore-errors
   (when (and (streamp *genia-stream*) (open-stream-p *genia-stream*))
     (force-output *genia-stream*)

     (close *genia-stream*)))
  (setf *genia-stream* nil)
  (when *genia-pid*
    (ignore-errors
     (run-process (format nil "kill -HUP ~d" *genia-pid*)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null")
     (run-process (format nil "kill -TERM ~d" *genia-pid*)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null")
     (run-process (format nil "kill -QUIT ~d" *genia-pid*)
                  :wait t :output "/dev/null" 
                  #-(or :sbcl :openmcl) :error-output
		  #-(or :sbcl :openmcl) "/dev/null"))
    #+:allegro
    (sys:os-wait nil *genia-pid*)
    (setf *genia-pid* nil)))

(let ((scanner (ppcre:create-scanner "\\t")))
  (defun genia (string)
    (genia-initialize)
    (if (stringp string)
      ;;
      ;; GENIA normalizes towards PTB conventions, i.e. disambiguates straight
      ;; double quotes into LaTeX-style opening or closing quotes.
      ;;
      (labels ((match (token string &optional (i 0))
                 (when (and token (numberp i))
                   (or (search token string :start2 i)
                       (when (string= token "``")
                         (search "\"" string :start2 i))
                       (when (string= token "''")
                         (search "\"" string :start2 i))
                       (when (string= token "`")
                         (search "'" string :start2 i))))))
        (format *genia-stream* "~a~%" string)
        (loop
            for i from 0
            for line = (read-line *genia-stream* nil nil)
            for fields = (and line (ppcre:split scanner line))
            for token = (first fields)
            for start = (match token string) then (match token string end)
            for end = (when start
                        (let ((length
                               (if (char= (schar string start) (schar token 0))
                                 (length token)
                                 1)))
                          (+ start length)))
            while fields
            collect 
            (pairlis '(:id :start :end :form :stem :tag :chunk :ne)
                     (list i start end (first fields) (second fields)
                           (third fields) (fourth fields) (fifth fields)))))
      (loop
          initially
            (loop
                for token in string
                for form = (ptb-escape (get-field :form token))
                do
                  (format
                   *genia-stream* "~@[ ~*~]~a"
                   (not (eq token (first string))) form))
            (format *genia-stream* "~%")
            (force-output *genia-stream*)
            (setf string (copy-list string))
            
          for line = (read-line *genia-stream* nil nil)
          for token = (pop string)
          for tag = (get-field :tag token)
          for fields = (and line (ppcre:split scanner line))
          when (and (null fields) string)
          do (error "something unexpected happened in genia()")
          while fields
          do
            (unless (and (stringp tag) (ppcre:scan "^NNP" tag)
                         (ppcre:scan "^NN" (third fields)))
              (setf (get-field :stem token) (second fields))
              (setf (get-field :tag token) (third fields)))
          collect token))))

(defun genia+tagger-for-pet (string &optional tagger &rest arguments)
  (let* ((format (getf arguments :format :pet))
         (nep (getf arguments :nep *genia-ne-p*))
         (stream (let ((stream (getf arguments :stream)))
                   (remf arguments :stream)
                   stream))
         (arguments (append arguments '(:raw t)))
         (tokens (apply #'lkb::repp-for-pet string tagger arguments))
         (length (length tokens))
         (id (loop for token in tokens maximize (lkb::token-id token)))
         (genia (genia string))
         (map (make-array (list (+ (length string) 1) 2)))
         block new)
    ;;
    ;; index all non-epsilon REPP tokens by start and end positions
    ;;
    (loop
        for i from 0
        for token in tokens
        for start = (lkb::token-start token)
        for end = (lkb::token-end token)
        when (and (numberp start) (numberp end) (not (= start end)))
        do
          (setf (aref map (lkb::token-start token) 0) token)
          (setf (aref map (lkb::token-end token) 1) token))
    (labels ((synthesize (start end category)
               #-:null
               (declare (ignore category))
               (let ((left (aref map start 0))
                     (right (aref map end 1)))
                 (when (and left right (not (eq left right)))
                   (let* ((start (lkb::token-start left))
                          (end (lkb::token-end right))
                          ;;
                          ;; _fix_me_
                          ;; until we work out how to properly integrate these
                          ;; in token mapping (and add `X/Y' +POSITION syntax 
                          ;; for filtering rules to match an `embedding' token
                          ;; as context), force a PoS tag that triggers a plain
                          ;; generic name (which, it turns out, can combine
                          ;; with determiners and form plurals). (4-mar-10; oe)
                          ;;
                          #+:null
                          (category (format nil "GENIA:~*" category))
                          (category "NNP"))
                     (when (and (numberp start) (numberp end))
                       ;;
                       ;; _fix_me_
                       ;; for completeness, we should find a lemma, maybe the
                       ;; stem from the final token in this span, i.e. assuming
                       ;; these NEs are head-final.             (22-mar-10; oe)
                       ;;
                       (push
                        (lkb::make-token
                         :id (incf id) :form (subseq string start end)
                         :from (lkb::token-from left) :to (lkb::token-to right)
                         :start start :end end :tags (list category 1.0))
                        new)
                       ;;
                       ;; the GENIA NE support can work in one of two ways: 
                       ;; either just augmenting the token lattice (creating
                       ;; multi-word tokens in addition to the basic tokens),
                       ;; or letting the NE tokens `obstruct' their component
                       ;; parts.
                       ;;
                       (unless (eq nep :add)
                         (loop
                             for token in (member left tokens :test #'eq)
                             until (eq token right) do (push token block)
                             finally (push right block)))))))))
      ;;
      ;; overwrite TnT tag assignments from corresponding GENIA tokens, where
      ;; applicable; do not squash NNP assignments for GENIA NN or NNS, though.
      ;; at the same time, pick up lemma from GENIA and downcase, unless we are
      ;; looking at a name.
      ;;
      (loop
          for token in genia
          for start = (get-field :start token)
          for end = (get-field :end token)
          for tag = (get-field :tag token)
          for match = (aref map start 0)
          when (and tag match (= (lkb::token-end match) end)) do
            (unless (and (ppcre:scan "^NNP" (first (lkb::token-tags match)))
                         (ppcre:scan "^NN" tag))
              (setf (lkb::token-tags match) (list tag 1.0)))
            (let ((stem (get-field :stem token)))
              (when stem 
                (setf (lkb::token-stem match)
                  (if (eql (search "NNP" (first (lkb::token-tags match))) 0)
                    stem
                    (string-downcase stem))))))
      ;;
      ;; look for GENIA named entities, possibly spanning multiple tokens
      ;;
      (when nep
        (loop
            with start with end with category
            for token in genia
            for ne = (get-field :ne token)
            when (and (char= (char ne 0) #\B)
                      (aref map (get-field :start token) 0))
            do
              (when start (synthesize start end category))
              (setf start (get-field :start token))
              (setf end (get-field :end token))
              (setf category (subseq ne 2))
            when (and start (char= (char ne 0) #\I))
            do
              (setf end (get-field :end token))
            when (and start (char= (char ne 0) #\O))
            do
              (synthesize start end category)
              (setf start nil)
            finally
              (when (and start end) (synthesize start end category)))))
    (nconc tokens new)
    (setf tokens
      (sort (set-difference tokens block) #'< :key #'lkb::token-start))
    (let ((result (lkb::repp-format tokens format)))
      (cond
       ((or (streamp stream) (eq stream t))
        (format stream "~a~%" result))
       ((stringp stream)
        (with-open-file (stream stream
                         :direction :output :if-exists :supersede)
          (format stream "~a~%" result))))
      (values result length))))

(defun read-items-from-genia-file (file)
  (with-open-file (stream file)
    (loop
        with pmid with items
        with author = (current-user) with date = (current-time)
        for line = (read-line stream nil nil)
        while line do
          (multiple-value-bind (start end starts ends)
              (ppcre:scan "<PMID>([0-9]+)</PMID>" line)
            (declare (ignore start end))
            (when (and starts ends)
              (setf pmid
                (parse-integer
                 line :start (aref starts 0) :end (aref ends 0)))))
          (multiple-value-bind (start end starts ends)
              (ppcre:scan
               "(<sentence[^>]*>)((?:(?!</sentence>).)*)</sentence>" line)
            (declare (ignore start end))
            (when (and starts ends)
              (let* ((tag (subseq line (aref starts 0) (aref ends 0)))
                     (id (search "id=\"S" tag))
                     (id (parse-integer tag :start (+ id 5) :junk-allowed t))
                     (body (subseq line (aref starts 1) (aref ends 1)))
                     (length (ppcre:all-matches "<tok[^>]*>" body))
                     (length (/ (length length) 2)))
                (push
                 (pairlis '(:i-id :i-category :i-wf :i-length :i-input
                            :i-origin :i-register :i-difficulty 
                            :i-author :i-date)
                          (list (+ (* pmid 100) id) "S" 1 length body
                                "GENIA Treebank" "formal" 1 author date))
                 items))))
        finally (return (nreverse items)))))

(defun genia-for-pet (string &optional tagger &rest arguments)
  (let* ((*package* (find-package :tsdb))
         (format (getf arguments :format :pet))
         (xml (with-input-from-string (stream string)
                (net.xml.parser:parse-xml stream)))
         (start 0) (from 0) (id 41)
         tokens)
    (labels ((process (xml &optional stringp)
               (if (and (consp xml) (consp (first xml)))
                 (case (first (first xml))
                   (|tok|
                    (let* ((tag (third (first xml)))
                           (form (second xml)))
                      (if (not stringp)
                        (push
                         (lkb::make-token
                          :id (incf id) :from from :to (incf from)
                          :start start :end (incf start (length form))
                          :form form :tags (list tag 1))
                         tokens)
                        (push form tokens))))
                   (|cons|
                    (loop for foo in (rest xml) do (process foo stringp))))
                 (when (stringp xml)
                   (if (not stringp)
                     (incf start (length xml))
                     (push xml tokens))))))
      (process (first xml) tagger)
      (setf tokens (nreverse tokens))
      (if tagger
        (let ((string (format nil "~{~a~}" tokens)))
          (apply #'genia+tagger-for-pet string tagger arguments))
        (values (lkb::repp-format tokens format) (length tokens))))))

#+:null
(defun genia-blazing-hook (frame &key (port 8765) (host "localhost"))
  (setf %frame frame)
  (let* ((id (lkb::compare-frame-item frame))
         (input (lkb::compare-frame-input frame))
         (input (format nil "<item>~a</item>~%" input))
         (discriminants
          (loop
              for discriminant in (lkb::compare-frame-discriminants frame)
              for start = (lkb::discriminant-start discriminant)
              for end = (lkb::discriminant-end discriminant)
              for edge = (lkb::discriminant-top discriminant)
              for derivation = (and edge (lkb::edge-bar edge))
              for from = (and derivation (derivation-from derivation start))
              for to = (and derivation (derivation-to derivation end))
              for i from 0
              collect
                (format
                 nil
                 "<discriminant id=\"~a\" type=\"~(~a~)\" ~
                  start=\"~a\" end=\"~a\"~@[ from=\"~a\"~]~@[ to=\"~a\"~] ~
                  key=\"~a\">~a</discriminant>~%"
                 i (xml-escape-string (lkb::discriminant-type discriminant))
                 start end from to
                 (xml-escape-string (lkb::discriminant-key discriminant))
                 (xml-escape-string (lkb::discriminant-value discriminant)))))
         (call (net.xml-rpc:encode-xml-rpc-call
                "treeblaze" id 0 input discriminants))
         (url (format nil "http://~a:~a/" host port)))
    (handler-case
        (let ((result (net.xml-rpc:xml-rpc-call call :url url)))
          (pprint result)
          (loop
              for state in result
              collect (if (stringp state)
                        (let ((c (schar state 0)))
                          (case c (#\+ t) (#\- nil) (t :unknown)))
                        :unknown)))
                        
      (condition (condition)
        (format
         *error-output* "genia-blazing-hook(): error `~a'.~%"
         (normalize-string (format nil "~a" condition)))))))

#+:null
(loop
    with items with n = 0 with id = 0
    with files = (loop
                     for file in (directory "~/src/conll10/genia/xml/")
                     when (equal (pathname-type file) "xml") collect file)
    for file in (sort
                 files #'<
                 :key #'(lambda (file) (parse-integer (pathname-name file))))
    do
      (let* ((foo (read-items-from-genia-file file))
             (k (length foo)))
        (when (> (+ n k) 1000)
          (let ((profile (format nil "genia/gtb~2,'0d" id)))
            (format t "creating `~a'~%" profile)
            (incf id)
            (insert profile "item" items :normalize t)
            (setf items nil)
            (setf n 0)))
        (setf items (nconc items foo))
        (incf n k))
    finally
      (let ((profile (format nil "genia/gtb~2,'0d" id)))
        (format t "creating `~a'~%" profile)
        (incf id)
        (insert profile "item" items :normalize t)))
