(in-package :mrs)

;;; for annlt - start ANNLT specific

(defun daughter-nodes-p (node)
  (listp node))

(defun get-rule-name (node)
  (string (first node)))

(defun get-dtr-nodes (node)
  (rest node))

(defun get-lexical-tag (node)
  (let* ((str (string node))
         (uscore-pos (position #\_ str)))
    (subseq str (+ 1 uscore-pos))))    

#|
(defun get-lexeme (node)
  (let* ((str (string node))
         (uscore-pos (position #\_ str))
         (notag (subseq str 0 uscore-pos))
         (suffix-pos (position #\+ notag)))
    (concatenate 'string
      (if suffix-pos
          (subseq notag 0 suffix-pos)
        notag)
      "_rel")))
|#

(defun get-lexeme (node)
  (let* ((str (string node))
         (uscore-pos (position #\_ str))
         (notag (subseq str 0 uscore-pos))
         (colon-pos (position #\: notag :from-end t))
         (suffix-pos (position #\+ notag)))
    (concatenate 'string
      (if suffix-pos
          (subseq notag 0 suffix-pos)
        (if (and colon-pos (> uscore-pos (+ 1 colon-pos)))
            (subseq notag 0 colon-pos)
          notag))
      "_rel")))


;;; multiple files

(defun process-rasp-files nil
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex.rmrs")
 (let* ((ifiles (directory "/usr/groups/mphil/qa02/trec8qa/parses/*"))
        (ofiles (directory "/local/scratch/aac10/trec8qa/rmrs/*"))
        (ofile-qnos (loop for ofile in ofiles
                        collect
                        (let* ((namestring (file-namestring ofile))
                               (dot-pos (position #\. namestring)))
                 (if dot-pos (subseq namestring 0 dot-pos))))))      
    (loop for ifile in ifiles
        do
          (let* ((namestring (file-namestring ifile))
                 (dot-pos (position #\. namestring))
                 (qno (if dot-pos (subseq namestring 0 dot-pos)))
                 (frest (if dot-pos (subseq namestring (+ 1 dot-pos))))
                 (fdot-pos (if frest (position #\. frest)))
                 (ftype (if fdot-pos (subseq frest 0 fdot-pos))))
            (when
                (and (not (member qno ofile-qnos
                              :test #'string-equal))
                     (equal ftype "parses")
                     (equal (subseq namestring 
                                    (- (length namestring) 2))
                            "gz"))
              (excl::shell 
               (concatenate 
                   'string "gunzip -c < " 
                   "/usr/groups/mphil/qa02/trec8qa/parses/"
                   namestring "> /tmp/pfile"))
              (let ((new-file (concatenate 'string 
                                "/local/scratch/aac10/trec8qa/rmrs/" 
                                qno "." "rmrs")))
                (rmrs-from-file "/tmp/pfile" 
                                new-file)
                (when (probe-file new-file)
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file)))))))))

(defun process-rasp-files2 nil
 (read-rmrs-grammar "~aac10/lingo/newlkb/src/rmrs/annlt-test/gram.rmrs")
 (read-rmrs-tag-templates "~aac10/lingo/newlkb/src/rmrs/annlt-test/lex.rmrs")
 (let* ((ifiles (directory "/usr/groups/mphil/qa02/trec8qa/parses2/*"))
        (ofiles (directory "/local/scratch/aac10/trec8qa/rmrs2/*"))
        (ofile-qnos (loop for ofile in ofiles
                        collect
                        (let* ((namestring (file-namestring ofile))
                               (dot-pos (position #\. namestring)))
                 (if dot-pos (subseq namestring 0 dot-pos))))))      
    (loop for ifile in ifiles
        do
          (let* ((namestring (file-namestring ifile))
                 (dot-pos (position #\. namestring))
                 (qno (if dot-pos (subseq namestring 0 dot-pos)))
                 (frest (if dot-pos (subseq namestring (+ 1 dot-pos))))
                 (fdot-pos (if frest (position #\. frest)))
                 (ftype (if fdot-pos (subseq frest 0 fdot-pos))))
            (when
                (and (not (member qno ofile-qnos
                              :test #'string-equal))
                     (equal ftype "parses")
                     (equal (subseq namestring 
                                    (- (length namestring) 2))
                            "gz"))
              (excl::shell 
               (concatenate 
                   'string "gunzip -c < " 
                   "/usr/groups/mphil/qa02/trec8qa/parses2/"
                   namestring "> /tmp/pfile"))
              (let ((new-file (concatenate 'string 
                                "/local/scratch/aac10/trec8qa/rmrs2/" 
                                qno "." "rmrs")))
                (rmrs-from-file "/tmp/pfile" 
                                new-file)
                (when (probe-file new-file)
                  (excl::shell (concatenate 'string "gzip " 
                                            new-file)))))))))

;;; File wrapper - note use of handler-case

(defun rmrs-from-file (filename output)
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (loop (let* ((original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (declare (ignore id))
              (unless tree (return))
              (format ostream "~%<sentence>")
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
              (format ostream
                      "~%<tree>~%~S~%</tree>"
                      tree)
              (handler-case
                  (progn
                    (unless (equal tree '(X))
                      (construct-sem-for-tree tree ostream))
                    (format ostream "</sentence>")
                    (finish-output ostream))
                (storage-condition (condition)
                  (format ostream "~%Memory allocation problem: ~A~%" condition))
                (error (condition)
                  (format ostream "~%Error: ~A~%" condition))
                (serious-condition (condition)
                  (format ostream "~%Something nasty: ~A~%" condition))))))))
               

(defun rmrs-from-file1 (filename)
  ;;; as above, no output file
  (with-open-file (istream filename :direction :input)
      (loop (let* ((original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (unless tree (return))
              (format t
                      "~%~S ~A"
                      original id)
              (handler-case
                  (construct-sem-for-tree tree t)
                (storage-condition (condition)
                  (format t "~%Memory allocation problem: ~A~%" condition))
                (error (condition)
                  (format t "~%Error: ~A~%" condition))
                (serious-condition (condition)
                  (format t "~%Something nasty: ~A~%" condition)))))))

;;; end ANNLT tree specific
