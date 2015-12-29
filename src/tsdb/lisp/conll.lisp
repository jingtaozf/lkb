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

(defparameter *conll-type* :starsem)

(defun read-items-from-conll-file (file
                                   &key (base 1) (offset 0) shift cycle
                                        (type *conll-type*) fields rawp (log t))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (loop
          with identifier = nil
          with id = base
          with input = nil
          for line = (read-line stream nil nil)
          while line
          when (string= line "#SDP 2015")
          do (format log "SDP 2015 file format detected.~%")
          else when (ppcre:scan "^#[0-9]+$" line)
          do (setf identifier (parse-integer line :start 1))
          else when (string= line "")
          collect
            (let* ((i (or identifier id))
                   (length (length input))
                   (string (format nil "~{~a~^~%~}" (nreverse input)))
                   (tokens (conll-preprocess
                            string :type type :fields fields :format :raw))
                   (negations
                    (when (eq type :starsem)
                      (handler-case (starsem-summarize-tokens tokens)
                        (condition (condition)
                          (warn (format nil "~a" condition))
                          nil)))))
              (when (smember type '(:dtm :sdp :sdp+))
                (cond
                 ((null tokens)
                  (format
                   log
                   "read-items-from-conll-file(): empty graph in item #~a.~%"
                   i))
                 (t
                  (setf tokens (ignore-errors (conll-expand-graph tokens)))
                  (if (null tokens)
                    (format
                     log
                     "read-items-from-conll-file(): graph error in item #~a.~%"
                     i)
                    (when cycle
                      (multiple-value-bind (token path) (conll-cyclic-p tokens)
                        (when token
                          (format
                           log 
                           "read-items-from-conll-file(): cycle in item #~a ~
                            (~a: ~{~a~^.~}).~%"
                           i (get-field :id token) path)
                          (setf tokens nil))))))))
              (setf identifier nil)
              (if (eq type :starsem)
                (labels ((segment (chapter)
                           (cond
                            ;;
                            ;; based on chapter names, carve up the identifier
                            ;; space as follows, one segment per chapter:
                            ;; [0-1] wisteria
                            ;; [2-17] baskervilles
                            ;;
                            ((search "wisteria" chapter :end2 8)
                             (- (parse-integer chapter :start 8) 1))
                            ((search "baskervilles" chapter :end2 12)
                             (+ (parse-integer chapter :start 12) 1))
                            ((search "cardboard" chapter :end2 9)
                             16)
                            ((search "circle" chapter :end2 6)
                             (+ (parse-integer chapter :start 6) 16)))))
                  (let* ((token (first tokens))
                         (segment (or (segment (get-field :chapter token)) 0))
                         (sentence (get-field :sentence token)))
                    (setf i (+ 10000 (* segment 1000) sentence))))
                (when (functionp shift) (setf i (funcall shift i))))
              (incf id)
              (when (and (consp tokens) (null rawp))
                (setf tokens (write-to-string tokens :case :downcase)))
              (when (and (consp negations) (null rawp))
                (setf negations (write-to-string negations :case :downcase)))
              (setf input nil)
              (pairlis '(:i-id :i-wf :i-length :i-input :i-tokens :i-comment)
                       (list (+ offset i) 1 length string tokens negations)))
          else do (push line input)))))

(defun conll-preprocess (string &key (format :string) fields
                                     gaps (mode :erg) type)
  (let ((tokens
         (loop
             with lines = (ppcre:create-scanner "\\n")
             with columns = (ppcre:create-scanner "\\t")
             for token in (ppcre:split lines string)
             for conll = (ppcre:split columns token)
             when (consp fields)
             collect
               ;;
               ;; a custom format, with a list of column labels provided
               ;;
               (loop
                   for key in fields
                   for values on conll
                   for value = (first values)
                   when (smember key '(:id :head))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else when key collect (cons key value))
             else when (eq type :dtm)
             collect
               ;;
               ;; DELPH-IN syntactic and semantic bi-lexical dependencies (DTM)
               ;;
               (loop
                   for key in '(:id :form :lemma :pos :let nil nil nil
                                :head :label :pred :args)
                   for values on conll
                   while (or (null fields) (>= (decf fields) 0))
                   for value = (first values)
                   when (smember key '(:id :head))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else when (eq key :args)
                   collect (cons key values)
                   else when key collect (cons key value))
             else when (eq type :sdp)
             collect
               ;;
               ;; SemEval 2014 Semantic Depedency Parsing
               ;;
               (loop
                   for key in '(:id :form :lemma :pos :root :pred :args)
                   for values on conll
                   while (or (null fields) (>= (decf fields) 0))
                   for value = (first values)
                   when (smember key '(:id))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else when (smember key '(:root :pred))
                   do (setf value (string= value "+"))
                   and collect (cons key value)
                   else when (eq key :args)
                   collect (cons key values)
                   else when key collect (cons key value))
             else when (eq type :sdp+)
             collect
               ;;
               ;; SemEval 2015 Semantic Depedency Parsing
               ;;
               (loop
                   for key in '(:id :form :lemma :pos :root :pred :sense :args)
                   for values on conll
                   while (or (null fields) (>= (decf fields) 0))
                   for value = (first values)
                   when (smember key '(:id))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else when (smember key '(:root :pred))
                   do (setf value (string= value "+"))
                   and collect (cons key value)
                   else when (eq key :args)
                   collect (cons key values)
                   else when key collect (cons key value))
             else when (eq type :starsem)
             collect
               ;;
               ;; the 2012 *SEM pseudo-CoNLL format
               ;;
               (loop
                   for key in '(:chapter :sentence :id :form :stem :ppos :ptb)
                   for value = (pop conll)
                   when (smember key '(:sentence :id))
                   collect (cons key (parse-integer value :junk-allowed t))
                   into token
                   else collect (cons key value) into token
                   finally
                     (let* ((id (get-field :id token))
                            (length (length (get-field :form token))))
                       (set-field :start (* id 100) token)
                       (set-field :end (+ (* id 100) length) token))
                     (return (acons :starsem conll token)))
             else when (eq type :tt)
             collect
               ;;
               ;; a house-internal convention: tokens, lemmas, and tags
               ;;
               (loop
                   for key in '(:id :form :lemma :pos)
                   for value in conll
                   when (smember key '(:id))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else when key collect (cons key value))
             else when (= (length conll) 10)
             collect
               ;;
               ;; the earlier, CoNLL 2007 format (CoNLL-X)
               ;;
               (loop
                   for key in '(:id :form :stem
                                :cpos :pos :feat
                                :head :deprel :phead :pdeprel)
                   for value in conll
                   while (or (null fields) (>= (decf fields) 0))
                   when (smember key '(:id :head :phead))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else collect (cons key value))
             else collect
               ;;
               ;; the richer, CoNLL 2009 format
               ;;
               (loop
                   for key in '(:id :form :stem :plemma
                                :pos :ppos :feat :pfeat
                                :head :phead :deprel :pdeprel
                                :fillpred :pred :apreds)
                   for value in conll
                   while (or (null fields) (>= (decf fields) 0))
                   when (smember key '(:id :head :phead))
                   collect (cons key (parse-integer value :junk-allowed t))
                   else collect (cons key value)))))
    (case format
      (:raw tokens)
      (:string
       (let ((forms
              (loop
                  for token in tokens
                  unless (and (null gaps) (eq mode :srg)
                              (string= (get-field :stem token) "_")
                              (string= (get-field :pos token) "p"))
                  collect (get-field :form token))))
         (format nil "~{~a~^ ~}" forms))))))

(defun conll-for-pet (string &optional tagger
                      &key gold (characterize t) (mode :erg)
                           type stream)
  (declare (ignore tagger))
  (loop
      with result
      with i = 0
      with start = 0 
      with end = 1
      for token in (conll-preprocess string :format :raw :type type)
      for id = (get-field :id token)
      for form = (rewrite-conll-token (get-field :form token))
      for plemma = (get-field :plemma token)
      for lemma = (if (or gold (not plemma)) (get-field :stem token) plemma)
      for cpos = (get-field :cpos token)
      for pos 
      = (or cpos (if gold (get-field :pos token) (get-field :ppos token)))
      for feat = (if (or gold (not plemma))
                   (get-field :feat token)
                   (get-field :pfeat token))
      for yy 
      = (case mode
          (:jacy
           (let* ((feat (substitute #\+ #\| feat))
                  (feat (substitute #\- #\= feat)))
             (unless #+:lkb (lkb::punctuationp form) #-:lkb nil
               (format 
                nil 
                "(~d, ~d, ~d, ~:[~*~*~;<~a:~a>, ~]~
                  1, ~s, 0, \"null\"~@[, \"~a+~a\" 1.0~])"
                id i (+ i 1) characterize start end form pos feat))))
          (:srg
           (unless (and (string= lemma "_") (string= pos "p"))
             (loop
                 for tag in (conll-to-parole pos feat)
                 for stem 
                 = (if (ppcre:scan "^(?:[zwf]|np|ao)" tag)
                     (cond
                      ((and (char= (char tag 0) #\z)
                            (member form '("un" "una" "uno") :test #'string=))
                       form)
                      (t tag))
                     lemma)
                 for yy
                 = (format 
                    nil 
                    "(~d, ~d, ~d, ~:[~*~*~;<~a:~a>, ~]~
                     1, ~s \"~a\", 0, \"$~a\"~@[, ~s 1.0~])"
                    id i (+ i 1) characterize start end stem form tag pos)
                 when tag collect yy into result
                 finally
                   (when (and (null result) (not (string= pos "f")))
                     (format
                      t
                      "~&conll-for-pet(): ~
                       ignoring token #~a (`~a' `~a' `~a')~%"
                      id form pos feat))
                   (return result))))
          (t
           (let ((start (or (get-field :start token) i))
                 (end (or (get-field :end token) (+ i 1))))
             (format 
              nil 
              "(~d, ~d, ~d, ~:[~*~*~;<~a:~a>, ~]~
               1, ~s, 0, \"null\"~@[, ~s 1.0~])"
              id i (+ i 1) characterize start end form pos))))
      when (consp yy) do (setf result (nconc yy result)) (incf i)
      when (stringp yy) do (push yy result) (incf i)
      when yy do (setf start end) (setf end (+ start 1))
      else do (incf end)
      finally
        (setf result (nreverse result))
        (cond
         ((or (streamp stream) (eq stream t))
          (format stream "~{~a~^ ~}" (nreverse result)))
         ((stringp stream)
          (with-open-file (stream stream
                           :direction :output :if-exists :supersede)
            (format stream "~{~a~^ ~}" result))))
        (return (values (format nil "~{~a~^ ~}" result) i))))

(defun rewrite-conll-token (token &optional pos)
  (let ((token (rewrite-ptb-token token pos)))
    ;;
    ;; it would seem that, the CoNLL 2007 data at least, has braces instead of
    ;; parentheses.  i dimly recall reading something about this problem in an
    ;; early README file (maybe for the second PTB release), but i wonder why
    ;; CoNLL in 2007 should have this problem?                  (3-nov-10; oe)
    ;;
    (cond
     ((string-equal pos "{") "(")
     ((string-equal pos "}") ")")
     (t token))))

(defun conll-expand-graph (tokens)
  (let ((n 0))
    (loop
        for token in tokens
        for pred = (get-field :pred token)
        when (or (equal pred "_") (null pred))
        do (set-field :pred nil token)
        else do (incf n))
    (let ((predicates (make-array n :initial-element nil)))
      (loop
          for token in tokens
          for pred = (get-field :pred token)
          for args = (get-field :args token)
          when (stringp pred) do
            (set-field :opred pred token)
            (cond
             ((and (not (zerop (length pred))) (char= (char pred 0) #\^))
              (set-field :root t token)
              (set-field :pred (subseq pred 1) token))
             (t
              (set-field :root nil token)))
          do
            (loop
                for i from 0
                for arg in args
                unless (string= arg "_")
                do (push (cons arg token) (aref predicates i))))
      (loop
          with i = 0
          for token in tokens
          for pred = (get-field :pred token)
          for args = (and pred (aref predicates i))
          when args do (set-field :pred pred token)
          else do (set-field :pred nil token)
          do
            (set-field :edges args token)
            (delete :args token :key #'first)
            (when pred (incf i)))
      tokens)))

(defun conll-cyclic-p (tokens)
  (labels ((walk (token &optional history path)
             (if (smember token history)
               (nreverse path)
               (loop
                   for (role . value) in (get-field :edges token)
                   thereis
                     (walk value (cons token history) (cons role path))))))
    (loop
        for token in tokens
        for path = (walk token)
        when path return (values token path))))

(defun conll-analyze (tokens)
  (let ((mark (gensym ""))
        (edges 0)
        roots singletons roles)
    (labels ((walk (token &optional recursep)
               (let ((edges (get-field :edges token)))
                 (unless (or (eq (get-field :mark token) mark)
                             (and (null edges) (null recursep)))
                   (set-field :mark mark token)
                   (loop
                       for (label . token) in edges
                       do
                         (pushnew label roles :test #'string-equal)
                         (walk token t))))))
      (loop for token in tokens do (walk token))
      (loop
          for token in tokens
          do (incf edges (length (get-field :edges token)))
          when (get-field :root token)
          do (push token roots)
          else unless (eq (get-field :mark token) mark)
          do (push token singletons))
      (values
       (conll-cyclic-p tokens)
       edges
       (nreverse roots) (nreverse singletons)
       (sort roles #'string<)))))

(defun conll-output (item &key (type :sdp) (stream t) header footer sense)

  (unless (smember type '(:sdp :latex))
    (error "conll-output(): unknown output format `~(~a~)'." type))

  (when header
    (if (stringp header)
      (write-string header stream)
      (case type
        (:latex
         (format 
          stream
          "\\documentclass[10pt,a1paper,landscape]{article}~%~
           \\usepackage[T1]{fontenc}~%~
           \\usepackage[utf8]{inputenc}~%~
           \\usepackage{tikz-dependency}~%~
           \\usepackage[a1paper,landscape]{geometry}~%~%~
           \\begin{document}~%~%")))))

  (let ((n 0)
        (tokens (get-field :i-tokens item)))
    ;;
    ;; _hack_
    ;; see whether the tokens string looks much like an association list; if
    ;; so, parse that list.
    ;;
    (let* ((n (when (stringp tokens) (- (length tokens) 1))))
      (when (and n (< 3 n)
                 (char= (schar tokens 0) #\()
                 (char= (schar tokens 1) #\()
                 (char= (schar tokens (- n 1)) #\))
                 (char= (schar tokens n) #\)))
        (setf tokens (ignore-errors (read-from-string tokens)))))
    (loop
        for token in tokens
        when (get-field :edges token) do
          (set-field :rank n token)
          (incf n))
    (loop for token in tokens do (set-field :predicates (make-array n) token))
    (loop
        for token in tokens
        for rank = (get-field :rank token)
        for edges = (get-field :edges token)
        do
          (loop
              for (role . target) in edges
              do (setf (aref (get-field :predicates target) rank) role)))
    (case type
      (:sdp
       (format stream "#~a~%" (get-field :i-id item)))
      (:latex
       (format
        stream
        "\\begin{dependency}[edge above, edge slant=0.15ex, ~
           edge unit distance=2ex]~%  ~
           \\begin{deptext}[column sep=1ex]~%    ")
       (loop
           for token in tokens
           unless (eq token (first tokens)) do (format stream " \\amp ")
           do 
             (format 
              stream "~a"
              (latex-escape-string (get-field :form token) :escape '(#\&))))
       (format stream "\\\\~%")
       (when sense
         (loop
             for token in tokens
             unless (eq token (first tokens)) do (format stream " \\amp ")
             do 
               (format 
                stream "~a"
                (latex-escape-string 
                 (get-field :sense token) :escape '(#\& #\\))))
         (format stream "\\\\~%"))
       (format stream "\\end{deptext}~%")))
    (loop
        for token in tokens
        for predicates = (get-field :predicates token)
        for i from 1
        do
          (case type
            (:sdp
             (format
              stream "~a~c~a~c~a~c~a~c~:[-~;+~]~c~:[-~;+~]~c~a"
              i #\tab (get-field :form token) #\tab
              (or (get-field :lemma token) (get-field :stem token))
              #\tab (get-field :pos token) #\tab
              (get-field :root token) #\tab
              (get-field :pred token) #\tab
              (or (get-field :sense token) "_"))
             (loop
                 for i from 0 below n
                 do (format stream "~c~a" #\tab (or (aref predicates i) "_")))
             (terpri stream))
            (:latex
             (when (get-field :root token)
               (format
                stream "\\deproot{~a}{top}~%" (get-field :id token)))
             (loop
                 for (label . target) in (get-field :edges token)
                 do
                   (format
                    stream "\\depedge{~a}{~a}{~a}~%"
                    (get-field :id token) (get-field :id target) 
                    (latex-escape-string label))))))
    (case type
      (:sdp
       (terpri stream))
      (:latex
       (format stream "\\end{dependency}~%"))))

  (when footer
    (if (stringp footer)
      (write-string footer stream)
      (case type
        (:latex
         (format stream "\\end{document}~%"))))))

(defun tt-output (item &key (type :tt) (stream t))
  (unless (eq type :tt)
    (error "tt-output(): unknown output format `~(~a~)'." type))
  (format stream "#~a~%" (get-field :i-id item))
  (loop
      for token in (get-field :i-tokens item)
      for i from 1
      do
        (format
         stream "~a~c~a~c~a~c~a~%"
         i #\tab (get-field :form token) #\tab
         (get-field :lemma token) #\tab (get-field :pos token)))
  (terpri stream))

(defparameter *conll-parole-a-map*
  '((("postype=ordinal" . "o") ("postype=qualificative" . "q") 0)
    (0)
    (("gen=f" . "f") ("gen=m" . "m") ("gen=c" . "c") ("gen=c" . "0") 0)
    (("num=s" . "s") ("num=p" . "p") ("num=c" . "0") ("num=c" . "n") 0)
    (("posfunction=participle" . "p") 0)))

(defparameter *conll-parole-c-map*
  '((("postype=coordinating" . "c") ("postype=subordinating" . "s") 0)))

(defparameter *conll-parole-d-map*
  '((("postype=article" . "a") ("postype=demonstrative" . "d")
     ("postype=exclamative" . "e") ("postype=indefinite" . "i")
     ("postype=numeral" . "n") ("postype=possessive" . "p")
     ("postype=interrogative" . "t") 0)
    (("person=1" . "1") ("person=2" . "2") ("person=3" . "3") 0)
    (("gen=f" . "f") ("gen=m" . "m") ("gen=c" . "c") ("gen=c" . "n")
     ("gen=c" . "0") 0)
    (("num=s" . "s") ("num=p" . "p") ("num=c" . "0") ("num=c" . "n") 0)
    (("possessornum=s" . "s") ("possessornum=c" . "c")
     ("possessornum=p" . "p") 0)))
  
(defparameter *conll-parole-f-map*
  '((("punct=exclamationmark" . "a") ("punct=colon" . "d")
     ("punct=quotation" . "e") ("punct=hyphen" . "g") ("punct=slash" . "h")
     ("punct=etc" . "s") ("punct=semicolon" . "x") ("punct=mathsign" . "z") 0)
    (("punctenclose=open" . "a") ("punctenclose=close" . "t")
     ("punctenclose=close" . "c") 0)))

(defparameter *conll-parole-n-map*
  '((("postype=common" . "c") 0)
    (("gen=f" . "f") ("gen=m" . "m") ("gen=c" . "c") ("gen=c" . "0") 0)
    (("num=s" . "s") ("num=p" . "p") ("num=c" . "n") ("num=c" . "0") 0)
    (0)
    (0)
    (0)))

(defparameter *conll-parole-p-map*
  '((("postype=personal" . "p") ("postype=demonstrative" . "d")
     ("postype=possessive" . "x") ("postype=indefinite" . "i")
     ("postype=interrogative" . "t") ("postype=relative" . "r")
     ("postype=numeral" . "n") ("postype=exclamative" . "e") 0)
    (("person=1" . "1") ("person=2" . "2") ("person=3" . "3") 0)
    (("gen=f" . "f") ("gen=m" . "m") ("gen=c" . "0") ("gen=c" . "c")
     ("gen=c" . "n") 0)
    (("num=s" . "s") ("num=p" . "p") ("num=c" . "0") ("num=c" . "n") 0)
    (("case=nominative" . "n") ("case=accusative" . "a") ("case=dative" . "d")
     ("case=oblique" . "o") 0)
    (("possessornum=s" . "s") ("possessornum=p" . "p") ("possessornum=c" . "c")
     0)
    (("polite=yes" . "p") 0)))

(defparameter *conll-parole-v-map*
  '((("postype=auxiliary" . "a") ("postype=main" . "m")
     ("postype=semiauxiliary" . "s") 0)
    (("mood=indicative" . "i") ("mood=subjunctive" . "s")
     ("mood=imperative" . "m") ("mood=infinitive" . "n") ("mood=gerund" . "g")
     ("mood=pastparticiple" . "p") 0)
    (("tense=present" . "p") ("tense=imperfect" . "i") ("tense=future" . "f")
     ("tense=past" . "s") ("tense=conditional" . "c") 0)
    (("person=1" . "1") ("person=2" . "2") ("person=3" . "3") 0)
    (("num=s" . "s") ("num=p" . "p") ("num=c" . "0") 0)
    (("gen=f" . "f") ("gen=m" . "m") ("gen=c" . "0") 0)))

 
(defparameter *conll-parole-z-map*
  '((("postype=currency" . "m") ("postype=percentage" . "p") 0)))

(defun conll-to-parole (pos features &key (filter t))
  (labels ((cross-product (lists)
             (if (null (rest lists))
               (loop
                   for foo in (first lists) collect (list foo))
               (loop
                   with rests = (cross-product (rest lists))
                   for foo in (first lists)
                   nconc (loop
                             for bar in rests
                             collect (cons foo bar)))))
           (fields (map)
             (let ((fields
                    (loop
                        for field in map
                        collect
                          (loop
                              for entry in field
                              when (and (consp entry)
                                        (search
                                         (first entry)
                                         features :test #'string=))
                              collect (rest entry) into matches
                              finally (return (or matches (last field)))))))
               (loop
                   for values in (cross-product fields)
                   for tag = (format nil "~a~{~a~}" pos values)
                   when (or (null filter)
                            #+:lkb
                            (let ((symbol (intern (string-upcase tag) :lkb)))
                              (gethash symbol lkb::*lexical-rules*))
                            #-:lkb t)
                   collect tag))))
    (cond
     ((string= pos "a") (fields *conll-parole-a-map*))
     ((string= pos "c") (fields *conll-parole-c-map*))
     ((string= pos "d")
      (cond
       ((search "postype=numeral" features) (list "z"))
       (t (fields *conll-parole-d-map*))))
     ((string= pos "f")
      (cond
       ((string= features "punct=period") (list "fp"))
       ((string= features "punct=bracket|punctenclose=open")
        (list "fpa"))
       ((string= features "punct=bracket|punctenclose=close")
        (list "fpt"))
       ((string= features "punct=questionmark|punctenclose=open")
        (list "fia"))
       ((string= features "punct=questionmark|punctenclose=close")
        (list "fit"))
       ((string= features "punct=comma") (list "fc"))
       ((string= features "punct=bracket|punctenclose=open") (list "fca"))
       ((string= features "punct=bracket|punctenclose=close") (list "fct"))
       (t (fields *conll-parole-f-map*))))
     ((string= pos "i") (list "i"))
     ((string= pos "n")
      (cond
       ((string= features "postype=proper|gen=c|num=c") (list "np00000"))
       (t (fields *conll-parole-n-map*))))
     ((string= pos "p") (fields *conll-parole-p-map*))
     ((string= pos "r")
      (cond
       ((string= features "postype=negative") (list "rn"))
       ((string= features "_") (list "rg"))))
     ((string= pos "s")
      (cond
       ((string= features "postype=preposition|gen=m|num=s|contracted=yes")
        (list "spcms"))
       #+:null
       ((string= features "postype=preposition|gen=m|num=p|contracted=yes")
        (list "spcmp"))
       ((string= features "postype=preposition|gen=c|num=c")
        (list "sps00"))))
     ((string= pos "v") (fields *conll-parole-v-map*))
     ((string= pos "w") (list "w"))
     ((string= pos "z") (fields *conll-parole-z-map*)))))

(defun item-to-conll (item &key (stream t) gold result-id)
  (when (stringp stream)
    (return-from item-to-conll
      (with-open-file (stream stream :direction :output :if-exists :supersede)
        (item-to-conll item :stream stream :gold gold :result-id result-id))))
  (when (stringp gold)
    (return-from item-to-conll
      (with-open-file (gold gold :direction :output :if-exists :supersede)
        (item-to-conll item :stream stream :gold gold :result-id result-id))))
  
  (labels ((heads (node &optional heads)
             (if (null (node-daughters node))
               (let* ((head (first heads)))
                 (list (cons node (if (eq head node) (second heads) head))))
               (loop
                   with head = (node-head node)
                   with heads = (if (eq head (first heads))
                                  heads
                                  (cons head heads))
                   for node in (node-daughters node)
                   append (heads node heads)))))
    (let* ((input (get-field :i-input item))
           (conll (or (get-field :conll item)
                      (let ((conll (conll-preprocess input :format :raw)))
                        (nconc item (acons :conll conll nil))
                        conll)))
           (results (get-field :results item))
           (result (when (numberp result-id)
                     (loop
                         for result in results
                         when (eql (get-field :result-id result) result-id)
                         return result)))
           (result (or result (first results)))
           (derivation (let ((derivation (get-field :derivation result))
                             (*package* (find-package :tsdb)))
                         (when (stringp derivation)
                           (setf derivation (read-from-string derivation))
                           (setf (get-field :derivation result) derivation))
                         derivation))
           (node (let ((*derivations-ignore-tokens-p* nil))
                   (derivation-to-node derivation)))
           (heads (heads node))
           (total 0)
           (correct 0))
      (labels ((nucleus (node)
                 (setf (node-nucleus node)
                   (if (= (+ (node-from node) 1) (node-to node))
                     (node-to node)
                     (loop
                         with punctuation
                         = '(#\` #\' #\" #\( #\) #\[ #\] #\{ #\}
                             #\. #\? #\. #\, #\; #\:)
                         for i from (node-from node) to (- (node-to node) 1)
                         for token = (nth i conll)
                         for form = (get-field :form token)
                         unless (loop
                                    for c across form
                                    always (member
                                            c punctuation :test #'char=))
                         return (+ i 1)
                         finally (return (node-to node)))))))
        (loop
            for (dependent . head) in heads
            unless (node-nucleus dependent) do (nucleus dependent)
            unless (or (null head) (node-nucleus head)) do (nucleus head)))
      (loop
          for token in conll
          for id = (get-field :id token)
          for nodes = (let ((node (first (first heads))))
                        (if (and node
                                 (> id (node-from node))
                                 (<= id (node-to node)))
                          (pop heads)
                          nodes))
          for head = (if (rest nodes) (node-nucleus (rest nodes)) 0)
          do
            ;;
            ;; the scorer, it seems, uses the HEAD column; for easy eyeballing
            ;; of results, put the original HEAD into the PHEAD column.
            ;;
            (when stream
              (format
               stream
               "~a~{	~a~}	~a	~a	_	_	~
                ~a	_	_~%"
               id
               (loop
                   for key in '(:form :stem :plemma :pos :ppos :feat :pfeat)
                   collect (get-field key token))
               head (get-field :head token) 
               (get-field :fillpred token)))
            (when (streamp gold)
              (format
               gold
               "~a~{	~a~}	~a	~a~{	~a~}~%"
               id
               (loop
                   for key in '(:form :stem :plemma :pos :ppos :feat :pfeat)
                   collect (get-field key token))
               (get-field :head token) (get-field :phead token)
               (loop
                   for key in '(:deprel :pdeprel :fillpred :pred :apreds)
                   collect (get-field key token))))    
            (incf total)
            (when (equal head (get-field :head token)) (incf correct)))
      (when stream (terpri stream))
      (when (streamp gold) (terpri gold))
      (pairlis '(:total :head) (list total correct)))))

(defun conll-item-enhancer (item)
  (nconc
   item
   (loop
       with ranks
       for result in (get-field :results item)
       for id = (get-field :result-id result)
       for score = (let* ((score (item-to-conll
                                  item :stream nil :result-id id))
                          (total (get-field :total score))
                          (head (get-field :head score)))
                     (when (and total head)
                       (divide head total)))
       unless (numberp score) do
         (format
          t
          "conll-item-enhancer(): no score on item # ~a (result # ~a).~%"
          (get-field :i-id item) (get-field :result-id result))
       and return nil
       else do
         (push (acons :asu score result) ranks)
       finally
         (return
           (let ((ranks (sort ranks #'>
                         :key #'(lambda (result) (get-field :asu result)))))
             (acons
              :ranks
              (loop
                  with top = (get-field :asu (first ranks))
                  for rank in ranks
                  while (= (get-field :asu rank) top)
                  collect (acons :rank 1 rank))
              nil))))))

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
         (format nil "/home/oe/src/conll09/en/09~2,'0d.txt" i)
         (format nil "test/conll~2,'0d" i) :format :conll :shift shift))

#+:null
(loop
    for i from 2 to 24
    for shift = #'(lambda (id) (+ id 20000000 (* i 100000)))
    do (do-import-items
         (format nil "/home/oe/src/conll07/09~2,'0d.txt" i)
         (format nil "test/conll~2,'0d" i) :format :conll :shift shift))
