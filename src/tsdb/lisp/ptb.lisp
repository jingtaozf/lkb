(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2007 Stephan Oepen (oe@ifi.uio.no)
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

(defvar *ptb-use-pos-tags-p* t)

(defun read-items-from-ptb-directory (path &key (base 1) (offset 20000000))
  (loop
      with id = base
      for set in (read-ptb-directory path)
      append
        (loop
            with current with range
            for (file . item) in set
            for ptb = (first (read-ptb-from-string item))
            for length = (length (extract-ptb-leaves ptb))
            for category = (ignore-errors (first ptb))
            for origin = (format nil "~a" file)
            unless (string= current file) do 
              (setf range
                (parse-integer (ppcre:scan-to-strings "[0-9]+" file)))
              (setf id 1)
              (setf current file)
            when (and ptb category)
            collect (pairlis '(:i-id :i-origin :i-category
                               :i-wf :i-length :i-input)
                             (list (+ offset (* range 1000) id)
                                   origin category
                                   1 length (format nil "(~a)" ptb)))
            and do (incf id))))

(defun read-token (stream &optional breaks)
  (loop
      with length = 2048
      with result = (make-array length
                                :element-type 'character
                                :adjustable nil :fill-pointer 0)
      with whitespace = '(#\space #\tab #\newline #\page)
      with prefixp = t
      for c = (read-char stream nil nil)
      when (member c whitespace :test #'eql) do
        (unless prefixp
          (setf c (peek-char t stream nil nil))
          (when (member c breaks :test #'eql) (read-char stream nil nil))
          (return (cons result c)))
      else when (or (member c breaks :test #'eql) (null c)) do
        (return (cons result c))
      else do
        (setf prefixp nil)
        (vector-push-extend c result length)))

(defun read-ptb-directory (path)
  (let* ((path (if (stringp path) path (namestring path)))
         (pattern (make-pathname :directory path :name :wild))
         (files (directory pattern))
         (files (sort files #'string< :key #'namestring))
         (buffer (make-array 4096
                             :element-type 'character
                             :adjustable t :fill-pointer 0))
         result)
      (loop
          for file in files
          for name = (pathname-name (pathname file))
          when (probe-file file) do
            (format t "~&now opening `~a':~%" file)
            (with-open-file (stream file :direction :input)
              (loop
                  with foo = nil
                  with open = 0
                  with last = #\(
                  for (token . break) = (read-token stream '(#\( #\)))
                  when (and (or (null break) (zerop open))
                            (> (fill-pointer buffer) 0)) 
                  do
                    (push (cons name (copy-seq buffer)) foo)
                    (setf (fill-pointer buffer) 0)
                    (setf last #\()
                  when (eql break #\() do
                    (incf open)
                  when (eql break #\)) do
                    (decf open)
                  while break do
                    (loop 
                        for c across token
                        do
                          (vector-push-extend c buffer 4096))
                    (cond
                     ((eql break #\()
                      (unless (and (zerop (length token))
                                   (eql last break))
                        (vector-push-extend #\space buffer 4096))
                      (vector-push-extend break buffer 4096))
                     ((eql break #\))
                      (vector-push-extend break buffer 4096))
                     (t
                      (vector-push-extend #\space buffer 4096)))
                    (setf last break)
                  finally (push (nreverse foo) result))))
      (nreverse result)))

(defun read-ptb-from-string (string)
  (with-input-from-string (stream string)
    (read-ptb-from-stream stream)))

(defun read-ptb-from-stream (stream &optional recursionp)
  (when (or recursionp
            (let ((foo (read-token stream '(#\( #\)))))
              (and (equal (first foo) "") (eql (rest foo) #\())))
    (loop
        for (token . break) = (read-token stream '(#\( #\)))
        when (> (length token) 0) collect token into result
        when (eql break #\() 
        collect (read-ptb-from-stream stream t) into result
        when (or (null break) (eql break #\))) do (return result))))

(defun extract-ptb-leaves (tree)
  (cond
   ((and tree (listp tree) (every #'stringp tree)) (list tree))
   ((listp tree)
    (loop for node in tree append (extract-ptb-leaves node)))
   (t nil)))

(defun rewrite-ptb-token (token pos)
  (declare (ignore pos))
  (cond
   #+:null
   ((string-equal pos "-lrb-") "(")
   #+:null
   ((string-equal pos "-rrb-") ")")
   ((string-equal token "``") "“")
   ((string-equal token "''") "”")
   ((string-equal token "`") "‘")
   ((string-equal token "'") "’")
   (t token)))

(defun ptb-preprocess (string 
                       &key rawp (plainp t) (posp *ptb-use-pos-tags-p*)
                            (characterize t))
  (let ((length 0)
        (result nil))
    (loop
        with tree = (read-ptb-from-string string)
        with leaves = (extract-ptb-leaves tree)
        with i = 0
        with id = 41
        for leaf in leaves
        for pos = (first leaf)
        for raw = (second leaf)
        for form = (rewrite-ptb-token raw pos)
        unless (or (string-equal pos "-none-")
                   (and rawp (string= raw ""))
                   (and (not rawp) (string= form ""))) do
          (cond
           (rawp (push raw result))
           (plainp (push form result))
           (t
            (push (format 
                   nil 
                   "(~d, ~d, ~d, ~:[~*~*~;<~a:~a>, ~]~
                     1, \"~a\" \"~a\", 0, \"null\"~
                    ~:[~*~;, \"~a\" 1.00~])" 
                   (incf id) i (+ i 1)
                   characterize i (+ i 1) form raw posp pos)
                  result)
            (incf i)))
          (incf length))
    (values (and result (format nil "~{~a~^ ~}" (nreverse result))) length)))

(defun ptb-for-pet (string &optional tagger)
  (declare (ignore tagger))
  (ptb-preprocess string :rawp nil :plainp nil :posp t))

#+:null         
(eval-when #+:ansi-eval-when (:load-toplevel :execute)
	   #-:ansi-eval-when (load eval)
  (setf (gethash :i-input *statistics-readers*)
    #'(lambda (string)
        (let ((*package* (find-package :tsdb)))
          (ptb-preprocess string :plainp t)))))

#+:null
(loop
    for i in '("00" "01" "02" "03" "04" "05" "06" "07"
               "08" "09" "10" "11" "12" "13" "14" "15"
               "16" "17" "18" "19" "20" "21" "22" "23" "24")
    do (do-import-items
         (format nil "/home/oe/src/ptb/mrg/~a" i)
         (format nil "test/wsj~a" i) :format :ptb))

#+:null
(loop
    for i from 2 to 21
    for shift = #'(lambda (id) (+ id 20000000 (* i 100000)))
    do (do-import-items
         (format nil "/home/oe/src/conll09/09~2,'0d.txt" i)
         (format nil "test/conll~2,'0d" i) :format :conll :shift shift))
