;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

;;; Utility functions for use with xml package

(defun whitespacep (char)
  (member char '(#\space #\tab #\newline #\page #\return #\linefeed)))

(defun xml-whitespace-string-p (str)
  (every 
   #'(lambda (char) 
       (whitespacep char))
   (coerce str 'list)))

;;; Minimal error checking because we assume that the
;;; XML has been validated syntactically

(defun read-rmrs-simple (expected-tag content)
  ;;; for the simple case
  ;;; (tag "str")
  (unless (eql (car content) expected-tag)
    (error "~A expected and not found" expected-tag))
  (cadr content))

#+:xml
(defun parse-xml-removing-junk (istream)
  ;;; parser insists on tree of `proper' elements
  ;;; so we just need to find this
  (let ((raw-xml (xml:parse-xml istream)))
    (dolist (xml-el raw-xml)
      (unless (member (car xml-el) '(:XML :DOCTYPE :COMMENT))
        (return xml-el)))))

          
    