;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; ANNLT specific

;;; called from comp.lisp - data structures within trees

(defun daughter-nodes-p (node)
  (listp node))

(defun get-rule-name (node)
  (string (first node)))

(defun get-dtr-nodes (node)
  (rest node))

(defun get-lexical-tag (node)
  (let* ((str (de-xml-str (string node)))
         (uscore-pos (position #\_ str)))
    (subseq str (+ 1 uscore-pos))))    

(defun get-lexeme (node)
  (let* ((xml-str (string node))
	 (str (de-xml-str xml-str))
         (uscore-pos (position #\_ str))
         (notag (subseq str 0 uscore-pos))
         (tag (subseq str uscore-pos))
         (colon-pos (position #\: notag :from-end t))
         (suffix-pos (position #\+ notag)))
    (make-word-info
      :lemma 
      (if suffix-pos
          (subseq notag 0 suffix-pos)
        (if (and colon-pos (> uscore-pos (+ 1 colon-pos)))
            (subseq notag 0 colon-pos)
          notag))
      :pos
      (tag-letters tag)
      :from (get-cfrom xml-str)
      :to (get-cto xml-str))))


(defun tag-letters (tag)
  ;;; e.g., _NP1
  (cond ((and (eql (elt tag 1) #\N) (eql (elt tag 2) #\P)) tag)
        ;;; various sorts of NPs - will correspond to named_rel etc
        ;;; in ERG
        ((eql (elt tag 1) #\N) "N")
        ((eql (elt tag 1) #\V) "V")
        ((eql (elt tag 1) #\J) "J")
        ((eql (elt tag 1) #\R) "R")
        ((eql (elt tag 1) #\P) "P")
        (t NIL)))


(defun de-xml-str (str)
  ;;; <w S='Y' C='W'>He:1_PPHS1</w>
  ;;; to He:1_PPHS1
  (let* ((first-end (position #\> str))
         (after-tag (subseq str (+ 1 first-end)))
         (second-first (position #\< after-tag)))
    (subseq after-tag 0 second-first)))

(defun get-cfrom (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 19
  (let ((first-s (position #\s str)))
    (if (and (char= (elt str (+ 1 first-s)) #\=)
	     (char= (elt str (+ 2 first-s)) #\"))
	(parse-integer (subseq str (+ 3 first-s)) :junk-allowed t)
      nil)))

(defun get-cto (str)
  ;;; <w s="19" e="24">bark+ed_VVD</w>
  ;;; extract 24
  (let ((first-e (position #\e str)))
    (if (and (char= (elt str (+ 1 first-e)) #\=)
	     (char= (elt str (+ 2 first-e)) #\"))
	(parse-integer (subseq str (+ 3 first-e)) :junk-allowed t)
      nil)))


  
;;; top level call
;;; multiple files

(defun extract-qa-file-identifier (namestring)
  ;;; e.g. top_docs.1.parses.gz
  ;;; returns "1"
  (let* ((dot-pos (position #\. namestring))
        (dot-pos2 (if dot-pos
                      (position #\. namestring :start (+ 1 dot-pos)))) 
        (qno (if (and dot-pos dot-pos2)
                 (subseq namestring (+ 1 dot-pos) dot-pos2)
               namestring)))
    qno))
  

(defun process-rasp-files nil
 (clear-rule-record)
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram14.1.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex14.1.rmrs")
 (let* ((ifiles
         ;;; (directory "~aac10/lingo/newlkb/src/rmrs/annlt-test/jan28/*"))
         ;;; (directory "/local/scratch/sht25/parses/*"))
         (directory "/local/scratch/aac10/qatest/parses/*"))
        (ofiles (directory "/local/scratch/aac10/qatest/rmrs/*"))
        (ofile-qnos (loop for ofile in ofiles
                        collect
                          (extract-qa-file-identifier 
                           (file-namestring ofile)))))
    (loop for ifile in ifiles
        do
          (let* ((namestring (file-namestring ifile))
                 (qno (extract-qa-file-identifier namestring)))
            (format t "~%Processing file ~A" namestring)
            (when
                (and (not (member qno ofile-qnos
                              :test #'string-equal))
                     (equal (subseq namestring 
                                    (- (length namestring) 2))
                            "gz"))
              (excl::shell 
               (concatenate 
                   'string "gunzip -c < " 
                   ;;; "/local/scratch/sht25/parses/"
                   "/local/scratch/aac10/qatest/parses/"
                   namestring "> /tmp/pfile"))
              (let ((new-file (concatenate 'string 
                                "/local/scratch/aac10/qatest/rmrs/"
                                "top_docs."
                                qno "." "rmrs"))
                    (err-file (concatenate 'string 
                                "/local/scratch/aac10/qatest/rmrs-errs/" 
                                "top_docs."
                                qno "." "errors")))
                (rmrs-from-xmlified-file "/tmp/pfile" 
                                         "/tmp/rfile" t)
                (excl::shell "rm /tmp/pfile")
                (when (probe-file "/tmp/rfile")
                  ;; change the dtd to the right thing
                  (excl::shell 
                   (concatenate 'string  
                     "/homes/sht25/Clconversion/chg_dtd.p \"/homes/sht25/QA/unified\" \"/usr/groups/mphil/qa03/dtd/analysis\" CORPUS CORPUS /tmp/rfile > " new-file))
                  (excl::shell 
                   (concatenate 'string
                     "xmlnorm -Vs " new-file " 2>| " err-file))
                  ;;; note we're redirecting std err
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file)))
                (excl::shell "rm /tmp/rfile")))))))

(defun process-rasp-specific-file (ifile)
 (clear-rule-record)
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram14.1.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex14.1.rmrs")
 (let* ((namestring (file-namestring ifile))
        (qno (extract-qa-file-identifier namestring)))
   (format t "~%Processing file ~A" namestring)
   (when
       (equal (subseq namestring 
                           (- (length namestring) 2))
                   "gz")
     (excl::shell 
      (concatenate 
          'string "gunzip -c < " 
          "/local/scratch/aac10/parses/"
          namestring "> /tmp/pfile"))
     (let ((new-file (concatenate 'string 
                       "/local/scratch/aac10/trec8qa/rmrs/"
                       "top_docs."
                       qno "." "rmrs")))
       (rmrs-from-xmlified-file "/tmp/pfile" 
                                "/tmp/rfile" t)
       (excl::shell "rm /tmp/pfile")
       (when (probe-file "/tmp/rfile")
         ;; change the dtd to the right thing
         (excl::shell 
          (concatenate 'string  
            "/homes/sht25/Clconversion/chg_dtd.p \"/homes/sht25/QA/unified\" \"/usr/groups/mphil/qa03/dtd/analysis\" CORPUS CORPUS /tmp/rfile > " new-file)))
       (excl::shell "rm /tmp/rfile")))))

#|
(simple-process-rasp-file 
 (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "rasp.out")
 "xxx")
|#

(defun simple-process-rasp-file (ifile ofile)
  (clear-rule-record)
 (read-rmrs-grammar 
  (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "gram14.1.rmrs"))
 (read-rmrs-tag-templates 
  (make-pathname :device "d"
		 :directory "/lingo/lkb/src/rmrs/annlt-test/"
		 :name "lex14.1.rmrs"))
  (rmrs-from-xmlified-file ifile ofile t))

(defun simple-process-rasp-non-xml-file (ifile ofile)
 (clear-rule-record)
 (read-rmrs-grammar 
  (make-pathname 
   :device "d"
   :directory "/lingo/lkb/src/rmrs/annlt-test/"
   :name "gram14.1.rmrs"))
 (read-rmrs-tag-templates 
  (make-pathname :device "d"
		 :directory "/lingo/lkb/src/rmrs/annlt-test/"
		 :name "lex14.1.rmrs"))
 (rmrs-from-file ifile ofile))

(defun revalidate-rmrs-files nil
  (let* ((ifiles
          (directory "/local/scratch/aac10/trec8qa/rmrs/*")))
    (loop for new-file in ifiles
        do
          (let* ((namestring (file-namestring new-file))
                 (qno (extract-qa-file-identifier namestring))
                 (err-file2 (concatenate 'string 
                                "/local/scratch/aac10/trec8qa/rmrs-errs2/" 
                                "top_docs."
                                qno "." "errors")))
             (when (equal (subseq namestring 
                                  (- (length namestring) 2))
                          "gz")
               (excl::shell 
               (concatenate 
                   'string "gunzip -c < "
                   "/local/scratch/aac10/trec8qa/rmrs/"
                   namestring "> /tmp/tfile"))
               (excl::shell 
                 (concatenate 'string
                   "xmlnorm -Vs /tmp/tfile 2>| " err-file2))
                  ;;; note we're redirecting std err
               (excl::shell "rm /tmp/tfile"))))))

(defun process-question-file nil
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram14.1.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex14.1.rmrs")
 (rmrs-from-xmlified-file 
  "~aac10/lingo/newlkb/src/rmrs/annlt-test/qa-data/questions.rasp" 
  "~aac10/lingo/newlkb/src/rmrs/annlt-test/qa-data/questions.rmrs"
  nil))

;;; File wrapper - note use of handler-case

(defun rmrs-from-file (filename output)
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (format ostream "<?xml version='1.0'?> <!DOCTYPE rmrs-list SYSTEM \"/homes/aac10/lingo/newlkb/src/rmrs/rmrs.dtd\" >")
      (format ostream "~%<rmrs-list>")
      (loop (let* ((original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (declare (ignore id))
              (unless tree
                (return))
              (when original
                #|
                blank lines in RASP cause the following
                () 0 ; ()
                
                (X)
                so we ignore cases where there's no sentence
                |#
                  (handler-case
                      (progn
                        (unless (equal tree '(X))
                          (construct-sem-for-tree tree ostream))
                        (finish-output ostream))
                    (storage-condition (condition)
                      (format ostream "~%Memory allocation problem: ~A~%" condition))
                    (error (condition)
                      (format ostream "~%Error: ~A~%" condition))
                    (serious-condition (condition)
                      (format ostream "~%Something nasty: ~A~%" condition))))))
      (format ostream "~%</rmrs-list>~%"))))

(defun rmrs-from-xmlified-file (filename output xml-p)
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (loop (let* ((markup (read-xml-characters istream xml-p))
                   ;; read in XML
                   (original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (declare (ignore id))
              (output-rmrs-file-markup ostream markup)
              ;; output XML unchanged (except for whitespace)
              (unless tree
                (unless (and markup
                             (dolist (char (coerce markup 'list))
                               (unless (whitespacep char)
                                 (return t))))
                  ;; hack round lack of markup at end when RASP
                  ;; misbehaves
                  (format ostream "~%</P>~%</TEXT>~%</DOC>~%</CORPUS>~%"))
                (return))
              (when original
                #|
                blank lines in RASP cause the following
                () 0 ; ()
                
                (X)
                so we ignore cases where there's no sentence
                |#
                  (format ostream
                          "~%<S>")
                  (format ostream
                          "~%<string>~%~S~%</string>" original)
                  (format ostream
                          "~%<tree>~%~S~%</tree>"
                          tree)
                  (handler-case
                      (progn
                        (unless (equal tree '(X))
                          (construct-sem-for-tree tree ostream))
                        (finish-output ostream))
                    (storage-condition (condition)
                      (format ostream "~%Memory allocation problem: ~A~%" condition))
                    (error (condition)
                      (format ostream "~%Error: ~A~%" condition))
                    (serious-condition (condition)
                      (format ostream "~%Something nasty: ~A~%" condition)))
                  (format ostream
                          "~%</S>")))))))
              

#|
;;; old sentence handling
              (if original
                  (format ostream
                          "~%<s>~%~A~{ ~A~}~A~%</s>"
                          (car original) (cdr original)
                          (if (member (car (last original)) 
                                      '("." "?" "!") :test #'equal)
                              ""
                            " ."))
                (format ostream
                          "~%<s></s>"))
              ;;; put spaces in between words but not at end.
              ;;;
              ;;; put a full stop at the end unless there's already
              ;;; some end of sentence punctuation there
|#
              

(defun read-xml-characters (istream xml-p)
  ;;; allow for arbitrary xml stuff in between what we care about
  ;;; if xml-p is nil, this is a noop
  ;;; otherwise we scan forward looking for the first
  ;;; P> followed by ( - maybe with whitespace
  (if xml-p
      (let* ((stuff nil)
             (next-char (peek-char t istream nil nil)))
        (if (eql next-char #\()
            nil
          (progn 
            (loop
              (let ((input-char1 (read-char istream nil nil)))
                (unless input-char1 (return))
                (push input-char1 stuff)
                (when (eql input-char1 #\P)
                  (let ((input-char2 (read-char istream nil nil)))
                    (push input-char2 stuff)
                    (when (eql input-char2 #\>)
                      (let ((paren-test
                             (loop (let ((input-char-inner 
                                          (peek-char nil istream nil nil)))
                                     (cond ((null input-char-inner) (return :eof))
                                           ((eql input-char-inner #\()
                                            (return :read))
                                           ((whitespacep input-char-inner)
                                            (read-char istream nil nil)
                                            (push input-char-inner stuff))
                                           (t (read-char istream nil nil)
                                              (push input-char-inner stuff)
                                              (return nil)))))))
                        (if (or (eql paren-test :read)
                                (eql paren-test :eof))
                            (return))))))))
              (coerce (nreverse stuff) 'string))))
        ))

(defun output-rmrs-file-markup (ostream markup)
  (when markup
    (format ostream "~A" markup)))


#|
(construct-sem-for-tree
'(|T/txt-sc1/----|
 (|S/np_vp| |There:1_EX|
  (|V1/be_np/---| |be+:2_VBR|
   (|NP/n1-plu|
    (|N1/np-num_n1| (|NP/num| |two:3_MC|)
     (|N1/n_pp-of| |aspect+s:4_NN2|
      (|PP/p1|
       (|P1/p_np| |of:5_IO|
        (|NP/det_n1| |the:6_AT|
         (|N1/n-name_n1| |Margaret:7_NP1|
          (|N1/n_n1/-| |Thatcher:8_NN1|
           (|N1/n_s|
            (|T/n_lmta-r| |phenomenon:9_NN1|
             (|Tacl/dash+/----| |-:10_-|
              (|S/np_vp|
               (|NP/np-pro_pp-of| |neither:11_DD1|
                (|PP/p1| (|P1/p_np-pro| |of:12_IO| |which:13_DDQ|)))
               (|V1/vp_pp|
                (|V1/be_ppart/-| |be+s:14_VBZ| (|V1/v| |address+ed:15_VVN|))
                (|PP/p1|
                 (|P1/p_np| |in:16_II|
                  (|NP/det_n1| |this:17_DD1|
                   (|N1/ap_n1/-| (|AP/a1| (|A1/a| |splendid:18_JJ|))
                    (|N1/ap_n1/-| (|AP/a1| (|A1/a| |British:19_JJ|))
                     (|N1/n| |biography:20_NN1|))))))))
              |-:21_-|))
            (|S/comp| |that:22_CST|
             (|S/np_vp|
              (|NP/det_n1| |every:23_AT1|
               (|N1/ap_n1/-| (|AP/a1| (|A1/a| |thinking:24_JJ|))
                (|N1/n| |American:25_NN1|)))
              (|V1/v_inf| |need+s:26_VVZ|
               (|V1/to_bse/-| |to:27_TO|
                (|V1/v_pp_pp| |keep:28_VV0|
                 (|PP/p1| (|P1/p_n1| |in:29_II| (|N1/n| |mind:30_NN1|)))
                 (|PP/p1|
                  (|P1/p_ing| |when:31_CS|
                   (|V1/v_np| |assess+ing:32_VVG|
                    (|NP/det_n1| |the:33_AT|
                     (|N1/n_pp-of| |achievement+s:34_NN2|
                      (|PP/p1|
                       (|P1/p_np| |of:35_IO|
                        (|NP/det_n1| |this:36_DD1|
                         (|N1/ap_n1/-| (|AP/a1| (|A1/a| |remarkable:37_JJ|))
                                       (|N1/n| |woman:38_NN1|)))))))))))))))))))))))))))
t)

|#