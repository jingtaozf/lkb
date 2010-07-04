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

(defparameter *genia-binary*
  #+:logon
  (let* ((root (system:getenv "LOGONROOT"))
         (root (and root (namestring (parse-namestring root)))))
    (if root (format nil "~a/bin/geniatagger" root) "geniatagger"))
  "geniatagger")

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
          (format nil "~a/bin/geniatagger" root)
          "geniatagger"))
      #-:logon
      "geniatagger")
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
    (format *genia-stream* "~a~%" string)
    ;;
    ;; GENIA normalizes towards PTB conventions, i.e. disambiguates straight
    ;; double quotes into LaTeX-style opening or closing quotes.
    ;;
    (labels ((match (token string &optional (i 0))
               (when (and token (numberp i))
                 (or (search token string :start2 i)
                     (when (string= token "``") (search "\"" string :start2 i))
                     (when (string= token "''") (search "\"" string :start2 i))
                     (when (string= token "`")
                       (search "'" string :start2 i))))))
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
                         (third fields) (fourth fields) (fifth fields)))))))

(defun genia-for-pet (string &optional tagger &rest arguments)
  (let* ((format (or (getf arguments :format) :pet))
         (nep (or (getf arguments :nep) *genia-ne-p*))
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
                 (when (and left right)
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
                        new)))
                   (loop
                       for token in (member left tokens :test #'eq)
                       until (eq token right) do (push token block)
                       finally (push right block))))))
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
              (setf start nil))))
    (nconc tokens new)
    (setf tokens
      (sort (set-difference tokens block) #'< :key #'lkb::token-start))
    (values (lkb::repp-format tokens format) length)))
