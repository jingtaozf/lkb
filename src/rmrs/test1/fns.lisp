(in-package :mrs)


(defun daughter-nodes-p (node)
  (listp (second node)))

(defun get-rule-name (node)
  (first node))

(defun get-dtr-nodes (node)
  (rest node))

(defun get-lexical-tag (node)
  (first node))

(defun get-lexeme (node)
  (concatenate 'string (second node) "_rel"))

(defun rmrs-from-file (filename)
  (with-open-file (istream filename)
    (loop (let* ((tree (read istream nil nil)))
            (unless tree (return))
            (construct-sem-for-tree tree)))))
