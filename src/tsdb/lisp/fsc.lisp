;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ITSDB -*-

(in-package :itsdb)

(defun fsc-read-input (string)
  (let* ((string (string-trim '(#\space #\newline) string))
         (xml (ignore-errors (s-xml:parse-xml-string string)))
         (text (rest (assoc :|text| (rest (first (rest xml)))))))
    (or text string)))
