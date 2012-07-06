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
                                   &key (base 1) (offset 0) shift
                                        (type *conll-type*))
  (when (probe-file file)
    (with-open-file (stream file :direction :input)
      (loop
          with id = base
          with input = nil
          for line = (read-line stream nil nil)
          while line
          when (string= line "")
          collect
            (let* ((i id)
                   (length (length input))
                   (string (format nil "~{~a~^~%~}" (nreverse input)))
                   (tokens (conll-preprocess string :type type :format :raw))
                   (negations
                    (when (eq type :starsem)
                      (handler-case (starsem-summarize-tokens tokens)
                        (condition (condition)
                          (warn (format nil "~a" condition))
                          nil)))))
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
              (when (consp tokens)
                (setf tokens (write-to-string tokens :case :downcase)))
              (when (consp negations)
                (setf negations (write-to-string negations :case :downcase)))
              (setf input nil)
              (pairlis '(:i-id :i-wf :i-length :i-input :i-tokens :i-comment)
                       (list (+ offset i) 1 length string tokens negations)))
          else do (push line input)))))

(defun conll-preprocess (string &key (format :string) gaps (mode :erg) type)
  (let ((tokens
         (loop
             for token in (ppcre:split "\\n" string)
             for conll = (ppcre:split "\\t" token)
             when (eq type :starsem)
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
             else when (= (length conll) 10)
             collect
               ;;
               ;; the earlier, CoNLL 2007 format
               ;;
               (loop
                   for key in '(:id :form :stem
                                :cpos :pos :feat
                                :head :deprel :phead :pdeprel)
                   for value in conll
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
