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


;;; File wrapper - note use of handler-case

(defun rmrs-from-file (filename output)
  (with-open-file (istream filename :direction :input)
    (with-open-file (ostream output :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (loop (let* ((original (read istream nil nil))
                   (id (read istream nil nil))
                   (tree (read istream nil nil)))
              (unless tree (return))
              (format ostream
                      "~%~S ~A"
                      original id)
              (handler-case
                  (construct-sem-for-tree tree ostream)
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
