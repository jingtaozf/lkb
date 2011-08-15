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

(defparameter *yy-k2y-ra-threshold* nil)

(defparameter *yy-rts-ra-threshold* nil)

(defparameter *yy-k2y-rts-ra-ratio* nil)

(defun yy-read-input (string &key (format :string))
  (let* ((i 0)
         (length (length string))
         (whitespace '(#\space #\tab #\newline)))
    (labels ((skip (characters)
               (loop
                   while (member (schar string i) characters :test #'char=)
                   do (incf i)))
             (skip-to (character)
               (loop
                   with escape = nil with quote = nil
                   while (< i length)
                   when (and (char= (schar string i) #\") (not escape))
                   do (setf quote (not quote))
                   else when (and (char= (schar string i) #\\) (not escape))
                   do (setf escape t)
                   else when (and (char= (schar string i) character) 
                                  (not quote))
                   return i
                   else do (setf escape nil)
                   do (incf i)))
             (seek-character (character)
               (when (< i length)
                 (skip whitespace)
                 (char= (schar string i) character)))
             (read-character (character)
               (when (seek-character character)
                 (incf i)))
             (read-form ()
               (when (< i length)
                 (multiple-value-bind (form rest)
                     (let ((*package* (find-package :tsdb)))
                       (read-from-string string nil nil 
                                         :start i :preserve-whitespace t))
                   (when form
                     (setf i rest)
                     form))))
             (read-characterization ()
               (when (seek-character #\<)
                 (let ((*readtable* (copy-readtable))
                       from to)
                   (set-syntax-from-char #\: #\" *readtable*)
                   (set-syntax-from-char #\, #\" *readtable*)
                   (set-syntax-from-char #\> #\" *readtable*)
                   (read-character #\<)
                   (setf from (read-form))
                   (when (numberp from)
                     (read-character #\:)
                     (setf to (read-form))
                     (read-character #\>)
                     (read-character #\,)
                     (and (numberp to) (values from to))))))
             (read-token ()
               (ignore-errors
                (let (id start end from to form surface inflection tags)
                  (read-character #\()
                  (setf id (read-form))
                  (read-character #\,)
                  (setf start (read-form))
                  (read-character #\,)
                  (setf end (read-form))
                  (read-character #\,)
                  (multiple-value-setq (from to) (read-characterization))
                  ;;
                  ;; skip over the path(s) this token is a member of; we are
                  ;; assuming everything is connected with everything.
                  ;;
                  (skip-to #\,) (read-character #\,)
                  (setf form (read-form))
                  (when (seek-character #\") (setf surface (read-form)))
                  (read-character #\,)
                  ;;
                  ;; skip over the inflection position
                  ;;
                  (skip-to #\,) (read-character #\,)
                  ;;
                  ;; _fix_me_
                  ;; i guess inflection information can be a list, separating
                  ;; elements by whitespact
                  ;;
                  (setf inflection (read-form))
                  (when (seek-character #\,)
                    (read-character #\,)
                    (setf tags
                      (loop
                          while (not (seek-character #\)))
                          collect (read-form))))
                  (skip-to #\))
                  (when (read-character #\))
                    (pairlis '(:id :start :end :from :to 
                               :form :surface :inflection :tags)
                             (list id start end from to
                                   form surface inflection tags)))))))
      (let ((tokens (loop for token = (read-token) while token collect token)))
        (setf tokens
          (sort tokens #'< :key #'(lambda (token) (get-field :id token))))
        (case format
          (:raw
           tokens)
          (:string
           (loop
               with result =
                 (make-array length
                  :element-type 'character :adjustable nil :fill-pointer 0)
               with positions = nil
               with ntokens = 0
               for token in tokens
               for start = (get-field :start token)
               for word = (or (get-field :surface token)
                              (get-field :form token))
               unless (member start positions :test #'=)
               do 
                  (push start positions)
                  (unless (zerop ntokens) (vector-push #\space result))
                  (loop for c across word do (vector-push c result))
                  (incf ntokens)
               finally (return (unless (equal result "") result)))))))))

(labels ((|[|-reader (stream char)
           (declare (ignore char))
           (read-delimited-list #\] stream t))
         (|{|-reader (stream char)
           (declare (ignore char))
           (read-delimited-list #\} stream t))
         (|<|-reader (stream char)
           (declare (ignore char))
           (read-delimited-list #\> stream t)))
  (defun yy-read-k2y (string)
    (unless (or (null string) (equal string ""))
      (let ((*readtable* (copy-readtable))
            (*package* (find-package :tsdb)))
        (setf (readtable-case *readtable*) :preserve)
        (set-macro-character #\[ #'|[|-reader nil *readtable*)
        (set-macro-character #\] (get-macro-character #\)) nil *readtable*)
        (set-macro-character #\{ #'|{|-reader nil *readtable*)
        (set-macro-character #\} (get-macro-character #\)) nil *readtable*)
        (set-macro-character #\< #'|<|-reader nil *readtable*)
        (set-macro-character #\> (get-macro-character #\)) nil *readtable*)
        (set-syntax-from-char #\; #\space *readtable*)
        (set-syntax-from-char #\, #\space *readtable*)
        (with-input-from-string (stream string)
          (loop
              with k2y = (ignore-errors (read stream nil nil))
              with span = (let* ((span (first k2y)))
                            (when span 
                              (substitute #\space #\- (symbol-name span)
                                          :test #'char=)))
              with start = (if span (read-from-string span) -1)
              with end = (if span 
                           (with-input-from-string (stream span)
                             (read stream nil nil)
                             (read stream nil -1))
                           -1)
              with size = (let ((size (fifth k2y)))
                            (if (listp size) 
                              (list (first size) (third size))
                              size))
              with header = (pairlis '(:span :reading :parse :size)
                                     (list (cons start end) (second k2y) 
                                           (fourth k2y) size))
              with body = (nthcdr (if (listp size) 5 6) k2y)
              for predicate = (pop body)
              for arguments = (pop body)
              while (and predicate arguments)
              collect (cons predicate arguments) into relations
              finally (return (acons :relations relations header))))))))

(defun yy-k2y-size (k2y)
  (loop
      with raw-atoms
      with relations = (get-field :relations k2y)
      for relation in relations
      do
        (loop
            for field = (pop relation)
            while field
            thereis (when (and (symbolp field)
                               (member (symbol-name field) '("RA" "IDS")
                                       :test #'string=))
                      (loop
                          for ra in (pop relation) do
                            (pushnew ra raw-atoms))))
      finally (return (sort raw-atoms #'<))))

(defun yy-browse-k2y (k2y i-input &key (format :tcl) file)
  (when k2y
    (let* ((user (current-user))
           (file (or file
                     (format
                      nil "/tmp/.tsdb.podium.~a.~a.~a"
                      user (current-pid) 
                      (string-downcase (string (gensym ""))))))
           (title (format nil "K2Y view for `~a'" i-input)))
      (loop
          with stream = (if (stringp file)
                          (open file :direction :output 
                                :if-exists :supersede 
                                :if-does-not-exist :create)
                          file)
          with span = (get-field :span k2y)
          with reading = (get-field :reading k2y)
          with parse = (get-field :parse k2y)
          with size = (get-field :size k2y)
          with relations = (get-field :relations k2y)
          initially 
            (case format
              (:tcl
               (format
                stream
                "header {~a--~a; ~a of ~a; ~a relations}~%"
                (first span) (rest span) reading parse size))
              (:html
               (format
                stream
                "<table border=2 cellpadding=5><tr><td align=center>~%~
                 <table border=0 class=k2y cellspacing=0>~%")))
          for i from 1
          for relation in relations
          for predicate = (first relation)
          for arguments = (rest relation)
          do
            (case format
              (:tcl
               (format stream "relation ~a {~a} { " i predicate))
              (:html
               (format
                stream 
                "<tr class=k2y>~%~
                 <td class=k2y align=right><b>~a&nbsp;</b></td>~%~
                 <td class=k2y align=left>~%  <b>[</b>&nbsp;~%"
                predicate)))
            (loop
                for attribute = (pop arguments)
                for value = (pop arguments)
                while (and attribute value)
                when (member (symbol-name attribute) '("RA" "IDS") 
                             :test #'string=)
                do 
                  (case format
                    (:tcl
                     (format 
                      stream 
                      "~a {~{~a~^ ~}} " 
                      (symbol-name attribute) value))
                    (:html
                     (format
                      stream
                      "     ~a &lt;<i>~{~a~^ ~}</i>&gt;;&nbsp;~%"
                      (symbol-name attribute) value)))
                else when (string= (symbol-name attribute) "CVALUE") do
                  (case format
                    (:tcl
                     (format 
                      stream 
                      "~a ~a " 
                      (symbol-name attribute) value))
                    (:html
                     (format
                      stream
                      "     ~a <i>~a</i>;&nbsp;~%"
                      (symbol-name attribute) value)))
                else do
                  (case format
                    (:tcl
                     (format
                      stream 
                      "~a ~:[~a~;{{~{~a~^, ~}}}~] " 
                      (symbol-name attribute)
                      (listp value) 
                      (if (not (listp value))
                        (symbol-name value)
                        (loop for foo in value collect (symbol-name foo)))))
                    (:html
                     (format
                      stream
                      "     ~a ~
                       ~:[<i>~a</i>~;&lt;~{<i>~a</i>~^, ~}&gt;~];&nbsp;~%" 
                      (symbol-name attribute)
                      (listp value) 
                      (if (not (listp value))
                        (symbol-name value)
                        (loop for foo in value collect (symbol-name foo)))))))
            (case format
              (:tcl
               (format stream "}~%"))
              (:html 
               (format stream "     <b>]</b>~%</td>~%</tr>~%")))
          finally
            (case format
              (:html
               (format stream "</table></td></tr>~%</table>~%")))
            (when (stringp file) (close stream))
            (case format
              (:tcl
               (let ((return 
                       (send-to-podium 
                        (format 
                         nil 
                         "showk2y ~s \".~(~a~)\" {~a}" 
                         file (gensym "") title)
                        :wait t)))
                 (when (and (equal (first return) :ok) 
                            (equal (first (second return)) :k2y))
                   (push (append (second return)
                                 (pairlis '(:file) (list file)))
                         *tsdb-podium-windows*)))
               (when (and file (probe-file file) (null *tsdb-debug-mode-p*))
                 (delete-file file))))))))

(labels ((|[|-reader (stream char)
           (declare (ignore char))
           (read-delimited-list #\] stream t))
         (|{|-reader (stream char)
           (declare (ignore char))
           (read-delimited-list #\} stream t))
         (|;|-reader (stream char)
             (declare (ignore stream char))
             #\page)
         (transform (intermediate)
           (loop
               with foo = nil
               while intermediate
               for token = (pop intermediate)
               when (equal token #\page) 
               collect (nreverse foo) and do (setf foo nil)
               else do (push token foo))))
  (defun yy-read-rts (string)
    (unless (or (null string) (equal string ""))
      (let ((*readtable* (copy-readtable))
            (*package* (find-package :tsdb)))
        (setf (readtable-case *readtable*) :preserve)
        (set-macro-character #\[ #'|[|-reader nil *readtable*)
        (set-macro-character #\] (get-macro-character #\)) nil *readtable*)
        (set-macro-character #\{ #'|{|-reader nil *readtable*)
        (set-macro-character #\} (get-macro-character #\)) nil *readtable*)
        (set-macro-character #\; #'|;|-reader nil *readtable*)
        (set-syntax-from-char #\# #\- *readtable*)
        (set-syntax-from-char #\: #\- *readtable*)
        (with-input-from-string (stream string)
          (loop
              for intermediate = (ignore-errors (read stream nil nil))
              for table = (transform intermediate)
              while table collect table))))))

(defun yy-rts-size (rts)
  (labels ((strip-raw-atom (string)
             (when (stringp string)
               (let* ((colon (position #\: string :from-end t))
                      (suffix (when colon (subseq string (+ colon 1))))
                      (raw-atom (when suffix 
                                  (ignore-errors (read-from-string suffix)))))
                 (when (integerp raw-atom) raw-atom)))))
    (loop
        with raw-atoms
        for rt in rts
        do
          (loop
              for role in rt
              for marker = (first role)
              for modifiers = (second role)
              for head = (third role)
              do
                (pushnew (strip-raw-atom marker) raw-atoms)
                (loop
                    for modifier in modifiers
                    do (pushnew (strip-raw-atom modifier) raw-atoms))
                (pushnew (strip-raw-atom head) raw-atoms))
        finally (return (sort (remove nil raw-atoms) #'<)))))

(defun yy-browse-rts (rts i-input &key (format :tcl) file)
  (when rts
    (let* ((user (current-user))
           (file (or file
                     (format
                      nil "/tmp/.tsdb.podium.~a.~a.~a"
                      user (current-pid) 
                      (string-downcase (string (gensym ""))))))
           (title (format nil "Role Table view for `~a'" i-input)))
      (loop
          with stream = (if (stringp file)
                          (open file :direction :output 
                                :if-exists :supersede 
                                :if-does-not-exist :create)
                          file)
          initially
            (case format
              (:html
               (format 
                stream 
                "<table border=2 cellpadding=5>~%")))

          for i from 1
          for rt in rts
          do
            (loop
                initially
                  (case format
                    (:tcl
                     (format stream "table ~d~%" i))
                    (:html
                     (format 
                      stream 
                      "<tr><td align=center>~
                       <table border=0 class=rt cellspacing=0>~%")))
                finally
                  (case format
                    (:html
                     (format stream "</table></td></tr>~%")))
                for j from 1
                for (role modifiers value) in rt
                do
                  (case format
                    (:tcl
                     (format
                      stream
                      "role ~d {~s {~{~s~^ ~}} ~@[~s~]}~%" 
                      j
                      (string role)
                      (loop 
                          for modifier in modifiers 
                          collect (string modifier))
                      (and value (string value))))
                    (:html
                     (format
                      stream
                      "<tr class=rt>~%~
                       <td class=rt align=right><b>~a&nbsp;</b></td>~%~
                       <td class=rt align=left>~%  <b>[</b>"
                      (string role))
                     (loop
                         for modifier in modifiers
                         do
                           (format stream "&nbsp;<i>~a</i>" (string modifier)))
                     (format
                      stream
                      "&nbsp;<b>]</b>&nbsp;~%  <i>~@[~a~]</i></td>~%</tr>~%"
                      (and value (string value))))))
          finally
            (case format
              (:tcl
               (when (stringp file) (close stream))
               (let ((return 
                       (send-to-podium 
                        (format 
                         nil 
                         "showrt ~s \".~(~a~)\" {~a}" 
                         file (gensym "") title)
                        :wait t)))
                 (when (and (equal (first return) :ok) 
                            (equal (first (second return)) :rt))
                   (push (append (second return)
                                 (pairlis '(:file) (list file)))
                         *tsdb-podium-windows*)))
               (when (and file (probe-file file) (null *tsdb-debug-mode-p*))
                 (delete-file file)))
              (:html
               (format stream "</td></tr>~%</table>~%")
               (when (stringp file) (close stream))))))))

(defun yy-result-filter (item)
  (if (and (or (null *yy-k2y-ra-threshold*) (zerop *yy-k2y-ra-threshold*))
           (or (null *yy-rts-ra-threshold*) (zerop *yy-rts-ra-threshold*))
           (or (null *yy-k2y-rts-ra-ratio*) (zerop *yy-k2y-rts-ra-ratio*)))
    item
    (let* ((i-length (get-field :i-length item))
           (readings (get-field :readings item))
           (results (get-field :results item))
           (result (or (when readings
                         (loop
                             for result in results
                             for id = (get-field :result-id result)
                             thereis (when (equal id readings) result)))
                       (first results)))
           (k2y (get-field :mrs result))
           (k2y (unless (equal k2y "") k2y))
           (k2y-size (length (yy-k2y-size k2y)))
           (rts (get-field :tree result))
           (rts (unless (equal rts "") rts))
           (rts-size (length (yy-rts-size rts)))
           (return
             (or 
              (when (and (integerp i-length)
                         (numberp *yy-k2y-ra-threshold*) 
                         (not (zerop *yy-k2y-ra-threshold*))
                         (< k2y-size 
                            (* i-length *yy-k2y-ra-threshold* 0.01)))
                item)
              (when (and (integerp i-length)
                         (numberp *yy-rts-ra-threshold*)
                         (not (zerop *yy-rts-ra-threshold*))
                         (< rts-size 
                            (* i-length *yy-rts-ra-threshold* 0.01)))
                item)
              (when (and (numberp *yy-k2y-rts-ra-ratio*)
                         (not (zerop *yy-k2y-rts-ra-ratio*)))
                (when (or (> rts-size k2y-size)
                          (and (not (zerop k2y-size)) 
                               (< (* (/ rts-size k2y-size) 100) 
                                  *yy-k2y-rts-ra-ratio*)))
                  item)))))
      (when return
        #+:fdebug
        (format 
         t 
         "yy-result-filter(): [~d] ~d tokens; ~d K2Y RAs; ~d RT RAs~%"
         (get-field :i-id item) i-length k2y-size rts-size)
        return))))

(defun yy-k2y-equal (old new)
  (let* ((symbols (make-hash-table)))
    (labels ((sort-key (relation)
               (let* ((name (first relation))
                      (name (if (symbolp name)
                              (string-downcase (symbol-name name))
                              (format nil "~(~a~)" name)))
                      (relation (rest relation))
                      clause ras)
                 (loop
                     for key = (pop relation)
                     while key
                     when (eq key 'clause) do
                       (let ((clause (first relation)))
                         (setf clause (if (stringp clause)
                                        clause
                                        (format nil "~(~a~)" clause))))
                     when (eq key 'ra) do
                       (let ((ras (first relation)))
                         (setf ras (format nil "~{~a~^_~}" ras))))
                 (concatenate 'string name "_" clause "_" ras)))
             (lookup (symbol)
               (let ((symbol (typecase symbol
                               (symbol symbol)
                               (string (intern symbol :tsdb))
                               (t (intern (format nil "~a" symbol) :tsdb)))))
                 (or (gethash symbol symbols)
                     (setf (gethash symbol symbols) (gensym "")))))
             
             (ssort (list)
               (sort (copy-seq list) #'string< :key #'symbol-name))
             (nsort (list)
               (sort (copy-seq list) #'<))
             (normalize-relation (relation)
               (cons (symbol-name (first relation))
                     (loop
                         with relation = (rest relation)
                         with result
                         for key = (pop relation)
                         for value = (pop relation)
                         while key
                         do
                           (push key result)
                           (case key
                             ((id clause arg argof objof var)
                              (push (lookup value) result))
                             (conjuncts
                              (let ((conjuncts (loop
                                                   for foo in value
                                                   collect (lookup foo))))
                                (push (ssort conjuncts) result)))
                             (ra
                              (push (nsort value) result))
                             (t
                              (push value result)))
                         finally (return (nreverse result)))))
             (normalize-k2y (k2y)
               (let* ((*gensym-counter* 0)
                      (relations (get-field :relations k2y))
                      (sorted (sort (copy-seq relations) 
                                    #'string< :key #'sort-key)))
                 (loop
                     for relation in sorted
                     collect (normalize-relation relation)))))
      (let ((osize (get-field :size old))
            (nsize (get-field :size new)))
        (if (listp osize)
          (if (listp nsize)
            (unless (equal osize nsize)
              (return-from yy-k2y-equal nil))
            (unless (equal (first osize) nsize)
              (return-from yy-k2y-equal nil)))
          (if (listp nsize)
            (unless (equal osize (first nsize))
              (return-from yy-k2y-equal nil))
            (unless (equal osize nsize)
              (return-from yy-k2y-equal nil))))
        (let ((old (normalize-k2y old))
              (new (normalize-k2y new)))
          (equal old new))))))

(defun yy-export-results (data
                          &key (condition *statistics-select-condition*)
                               (directory "/var/www/html/rte/lib")
                               meter)

  (when meter (meter :value (get-field :start meter)))
  (let* ((thorough '(:mrs :tree))
         (condition (if condition
                      (concatenate 'string "(readings >= 1) && " condition)
                      "readings >= 1"))
         (items (when (stringp data) (analyze data)))
         (items (sort (copy-seq items) 
                      #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (ritems (if (stringp data) 
                   (analyze data 
                            :condition condition :thorough thorough)
                   data))
         (ritems (sort (copy-seq ritems) 
                       #'< :key #'(lambda (foo) (get-field :i-id foo))))
         (pattern (make-pathname :directory directory :name :wild))
         (contents (directory pattern)))
    
    (loop
        for file in contents do (delete-file file))

    (when (functionp *statistics-result-filter*)
      (setf ritems
        (loop
            for item in ritems
            for result = (funcall *statistics-result-filter* item)
            when result collect result)))
    
    (loop
        with index = (create-output-stream 
                      (make-pathname :directory directory :name "Index"))
        with increment = (when (and meter items) (/ (mduration meter)
                                                    (length items)))
        for item in items
        for i-id = (get-field :i-id item)
        for i-input = (or (get-field :o-input item) (get-field :i-input item))
        for ritem = (when (equal i-id (get-field :i-id (first ritems)))
                      (pop ritems))
        for results = (when ritem (get-field :results ritem))
        for result = (loop
                         for result in results
                         thereis (when (and (get-field :mrs result)
                                            (get-field :tree result))
                                   result))
        for k2y = (get-field :mrs result)
        for rt = (get-field :tree result)
        for i from 1
        do
          (when increment (meter-advance increment))
          (format index "~d@@@@@~%" i)
          (with-open-file (stream (make-pathname 
                                   :directory directory
                                   :name (format nil "~d.item" i))
                           :direction :output :if-exists :supersede)
            (format stream "~d@-1@~a~%" i-id i-input))
          (when k2y
            (yy-browse-k2y 
             k2y i-input 
             :file  (namestring (make-pathname 
                                 :directory directory
                                 :name (format nil "~d.k2y" i)))
             :format :html))
          (when rt
            (yy-browse-rts 
             rt i-input 
             :file  (namestring (make-pathname 
                                 :directory directory
                                 :name (format nil "~d.rt" i)))
             :format :html))
          
        finally (close index)))
  (when meter (meter :value (get-field :end meter))))

(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (let ((reader
         #'(lambda (string)
             (let ((*package* (find-package :tsdb)))
               (yy-read-input string :format :raw)))))
    (setf (gethash :p-input *statistics-readers*) reader)
    (setf (gethash :p-tokens *statistics-readers*) reader)))

#+:null
(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf (gethash :i-input *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (yy-read-input string))))
  (setf (gethash :mrs *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (yy-read-k2y string))))
  (setf (gethash :mrs *statistics-browsers*) #'yy-browse-k2y)
  (setf (gethash :tree *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (yy-read-rts string))))
  (setf (gethash :tree *statistics-browsers*) #'yy-browse-rts)
  (setf (gethash :mrs *statistics-predicates*) 
    #'(lambda (gold blue) (not (yy-k2y-equal gold blue))))
  (setf *statistics-result-filter* #'yy-result-filter))
