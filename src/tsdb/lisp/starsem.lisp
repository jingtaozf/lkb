;;; -*- mode: common-lisp; coding: utf-8; package: tsdb -*-

(in-package :tsdb)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 2008 -- 2009 Stephan Oepen (oe@ifi.uio.no)
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

(defun starsem-summarize-tokens (tokens)
  (labels ((anchor (string token)
             (let* ((form (get-field :form token))
                    (length (length form))
                    (start (search string form)))
               (if start
                 (let* ((base (* (get-field :id token) 100))
                        (end (+ start (length string)))
                        (affix
                         (cond
                          ((and (zerop start) (= end length)) :form)
                          ((zerop start) :prefix)
                          ((= end length) :suffix)
                          (t :infix))))
                   (list (+ base start) (+ base end) affix))
                 (error 
                  "summarize-starsem(): unable to anchor `~a' in ~a."
                  string token))))
           (collect (index)
             (loop
                 for token in tokens
                 for string = (nth index (get-field :starsem token))
                 unless (or (null string)
                            (string= string "_")
                            (string= string "***"))
                 collect (anchor string token))))
    (loop
        with n = (loop
                     for token in tokens
                     maximize (length (get-field :starsem token)))
        with cues with scopes with events
        for base from 0 to (- n 1) by 3
        for i from 0
        for cue = (collect base)
        for scope = (collect (+ base 1))
        for event = (collect (+ base 2))
        when cue do (push (cons i cue) cues)
        when scope do (push (cons i scope) scopes)
        when event do (push (cons i event) events)
        finally
          (labels ((massage (entities)
                     (loop
                         for entity in (nreverse entities)
                         for id = (acons :id (first entity) nil)
                         for span = (combine-spans (rest entity))
                         collect (nconc id (acons :span span nil)))))
            (return
              (when (or cues scope events)
                (pairlis '(:ncues :nscopes :nevents)
                         (list (massage cues)
                               (massage scopes)
                               (massage events)))))))))

(defun starsem-output (tokens
                       &optional cues scopes events
                       &key (stream t) (format :string))

  (when (get-field :i-tokens tokens)
    (setf tokens (get-field :i-tokens tokens))
    (setf cues (get-field :ncues tokens))
    (setf scopes (get-field :nscopes tokens))
    (setf events (get-field :nevents tokens)))
  (cond
   ((stringp stream)
    (with-open-file (stream stream :direction :output :if-exists :supersede)
      (starsem-output tokens cues scopes events :stream stream)))
   ((null stream)
    (with-output-to-string (stream)
      (starsem-output tokens cues scopes events :stream stream)))
   (t
    (if (smember format '(:string :xml))
      (let* ((starts (make-hash-table :test #'eql))
             (ends (make-hash-table :test #'eql)))
        (labels ((index (id type span)
                   (let ((entry (cons id (cons type span))))
                     (push entry (gethash (first span) starts))
                     (push entry (gethash (second span) ends))))
                 (record (id type spans)
                   (loop for span in spans do (index id type span)))
                 (open-span (id type)
                   (declare (ignore id))
                   (case format
                     (:string
                      (write-char
                       (case type (:cue #\<) (:scope #\{) (:event #\|))
                       stream))))
                 (close-span (id type)
                   (declare (ignore id))
                   (case format
                     (:string
                      (write-char
                       (case type (:cue #\>) (:scope #\}) (:event #\|))
                       stream)))))
          ;;
          ;; index scope and cue elements by start and end character positions
          ;;
          (loop
              for cue in cues
              do (record (get-field :id cue) :cue (get-field :span cue)))
          (loop
              for scope in scopes 
              do (record (get-field :id scope) :scope (get-field :span scope)))
          (loop
              for event in events
              do (record (get-field :id event) :event (get-field :span event)))
          ;;
          ;; for each position, sort elements starting or ending here according
          ;; to `size', with larger scopes `nesting around' smaller ones and
          ;; scopes nesting around events nesting around cues.
          ;;
          (loop
              for start being each hash-key in starts
              do
                (setf (gethash start starts)
                  (sort
                   (gethash start starts)
                   #'(lambda (spana spanb)
                       (let ((enda (fourth spana))
                             (endb (fourth spanb))
                             (typea (second spana)))
                         (or (> enda endb)
                             (and (= enda endb)
                                  (eq typea :scope))
                             (and (= enda endb)
                                  (eq typea :event)
                                  (eq (second spanb) :cue))))))))
          (loop
              for end being each hash-key in ends
              do
                (setf (gethash end ends)
                  (sort
                   (gethash end ends)
                   #'(lambda (spana spanb)
                       (let ((starta (third spana))
                             (startb (third spanb))
                             (typea (second spana)))
                         (or (> starta startb)
                             (and (= starta startb)
                                  (eq typea :cue))
                             (and (= starta startb)
                                  (eq typea :event)
                                  (eq (second spanb) :scope))))))))
          ;;
          ;; finally, write out the annotated string, one token at a time and,
          ;; (in string-based mode) within each token, one character at a time
          ;;
          (loop
              for token in tokens
              for form = (get-field :form token)
              for id = (get-field :id token)
              do
                (write-char #\space stream)
                (loop
                    for i from 0 to (- (length form) 1)
                    do
                      (loop
                          for span in (gethash (+ (* id 100) i) starts)
                          do (open-span (first span) (second span)))
                      (write-char (schar form i) stream)
                      (loop
                          for span in (gethash (+ (* id 100) i 1) ends)
                          do (close-span (first span) (second span)))))))
      (labels ((intersect (token spans type)
                 (loop
                     with span
                     = (list (get-field :start token) (get-field :end token))
                     for foo in spans
                     for intersection
                     = (first (span-intersection (get-field :span foo) span))
                     when intersection
                     collect
                       (let ((start (mod (first intersection) 100))
                             (end (mod (second intersection) 100)))
                         (list (get-field :id foo) type start end)))))
        (loop
            with n
            = (max (loop for cue in cues maximize (get-field :id cue))
                   (loop for scope in scopes maximize (get-field :id scope))
                   (loop for event in events maximize (get-field :id event)))
            for token in tokens
            for form = (get-field :form token)
            for cspans = (intersect token cues :cue)
            for sspans = (intersect token scopes :scope)
            for espans = (intersect token events :event)
            do
              (format
               stream "~{~a~^	~}"
               (loop
                   for key in '(:chapter :sentence :id :form :stem :ppos :ptb)
                   collect (get-field key token)))
              (if (and (null cues) (null scopes) (null events))
                (format stream "	***")
                (loop
                    for i from 0 to n
                    for cspan = (find i cspans :key #'first)
                    for sspan = (find i sspans :key #'first)
                    for espan = (find i espans :key #'first)
                    do
                      (format
                       stream "	~a	~a	~a"
                       (if cspan
                         (subseq form (third cspan) (fourth cspan))
                         "_")
                       (if sspan
                         (subseq form (third sspan) (fourth sspan))
                         "_")
                       (if espan
                         (subseq form (third espan) (fourth espan))
                         "_"))))
              (format stream "~%"))
        (format stream "~%"))))))

(defun span-intersection (one two &optional (base 1))
  (if (consp (first one))
    (loop for span in one append (span-intersection span two))
    (if (consp (first two))
      (loop for span in two append (span-intersection one span))
      (let* ((ostart (truncate (first one) base))
             (oend (truncate (second one) base))
             (tstart (truncate (first two) base))
             (tend (truncate (second two) base))
             (from (max ostart tstart))
             (to (min oend tend)))
        (and (<= from to) (list (list from to)))))))

(defun span-inclusion (one two)
  (if (consp (first one))
    (loop for span in one always (span-inclusion span two))
    (if (consp (first two))
      (loop for span in two thereis (span-inclusion one span))
      (let* ((base 1)
             (ostart (truncate (first one) base))
             (oend (truncate (second one) base))
             (tstart (truncate (first two) base))
             (tend (truncate (second two) base)))
        (and (>= ostart tstart) (<= oend tend))))))

(defun span-equal (one two)
  (if (consp (first one))
    (when (consp (first two))
      (loop
          for one in one for two in two always (span-equal one two)))
    (unless (consp (first two))
      (let* ((base 1)
             (ostart (truncate (first one) base))
             (oend (truncate (second one) base))
             (tstart (truncate (first two) base))
             (tend (truncate (second two) base)))
        (and (= ostart tstart) (= oend tend))))))

(defun combine-spans (spans)
  (labels ((reduce (spans)
             (if (or (null spans) (null (rest spans)))
               spans
               (let ((combination (combine (first spans) (second spans))))
                 (if combination
                   (reduce (cons combination (rest (rest spans))))
                   (cons (first spans) (reduce (rest spans)))))))
           (combine (left right)
             (when (and (numberp (first left)) (numberp (first right)))
               (if (equal left right)
                 right
                 (when (and (= (+ (truncate (second left) 100) 1)
                               (truncate (first right) 100))
                            (or (null (third left))
                                (smember (third left) '(:suffix :form)))
                            (or (null (third right))
                                (smember (third right) '(:prefix :form))))
                   (let ((type 
                          (cond
                           ((and (or (null (third left))
                                     (eq (third left) :form))
                                 (or (null (third right))
                                     (eq (third right) :form)))
                            :form)
                           ((or (null (third right)) (eq (third right) :form))
                            :suffix)
                           ((or (null (third left)) (eq (third left) :form))
                            :prefix)
                           (t
                            :infix))))
                     (list (first left) (second right) type)))))))
    (reduce
     (sort
      (copy-list spans)
      #'(lambda (left right)
          (or (< (first left) (first right))
              (and (= (first left) (first right))
                   (< (second left) (second right)))))))))

#+:null
(let* ((root "~/src/starsem/")
       (wisteria (format
                  nil "~aprimary/~a"
                  root "SEM-2012-SharedTask-CD-SCO-dev-09032012.txt"))
       (baskervilles (format
                      nil "~aprimary/~a"
                      root "SEM-2012-SharedTask-CD-SCO-training-09032012.txt"))
       (*conll-type* :starsem))

  (do-import-items wisteria "tmp/ssd" :format :conll)
  (purge-profile-cache :all)
  (with-open-file (stream (format nil "~a/ssd.txt" root)
                   :direction :output :if-exists :supersede)
    (loop
        for item in (analyze "tmp/ssd" :tokensp t :commentp t)
        when (get-field :ncues item)
        do
          (format stream "[~a] " (get-field :i-id item))
          (starsem-output
           (get-field :i-tokens item)
           (get-field :ncues item)
           (get-field :nscopes item)
           (get-field :nevents item)
           :stream stream :format :string)
          (terpri stream)))
  
  (do-import-items baskervilles "tmp/sst" :format :conll)
  (purge-profile-cache :all)
  (with-open-file (stream (format nil "~a/sst.txt" root)
                   :direction :output :if-exists :supersede)
    (loop
        for item in (analyze "tmp/sst" :tokensp t :commentp t)
        when (get-field :ncues item)
        do
          (format stream "[~a] " (get-field :i-id item))
          (starsem-output
           (get-field :i-tokens item)
           (get-field :ncues item)
           (get-field :nscopes item)
           (get-field :nevents item)
           :stream stream :format :string)
          (terpri stream))))

#+:null
(let* ((root "~/src/starsem/")
       (circle (format
                nil "~aprimary/~a"
                root "SEM-2012-SharedTask-CD-SCO-test-circle-GOLD.txt"))
       (cardboard (format
                   nil "~aprimary/~a"
                   root "SEM-2012-SharedTask-CD-SCO-test-cardboard-GOLD.txt"))
       (*conll-type* :starsem))

  (do-import-items circle "tmp/ssi" :format :conll)
  (purge-profile-cache :all)
  (with-open-file (stream (format nil "~a/ssi.txt" root)
                   :direction :output :if-exists :supersede)
    (loop
        for item in (analyze "tmp/ssi" :tokensp t :commentp t)
        when (get-field :ncues item)
        do
          (format stream "[~a] " (get-field :i-id item))
          (starsem-output
           (get-field :i-tokens item)
           (get-field :ncues item)
           (get-field :nscopes item)
           (get-field :nevents item)
           :stream stream :format :string)
          (terpri stream)))
  (do-import-items cardboard "tmp/ssa" :format :conll)
  (purge-profile-cache :all)
  (with-open-file (stream (format nil "~a/ssa.txt" root)
                   :direction :output :if-exists :supersede)
    (loop
        for item in (analyze "tmp/ssa" :tokensp t :commentp t)
        when (get-field :ncues item)
        do
          (format stream "[~a] " (get-field :i-id item))
          (starsem-output
           (get-field :i-tokens item)
           (get-field :ncues item)
           (get-field :nscopes item)
           (get-field :nevents item)
           :stream stream :format :string)
          (terpri stream))))
