;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :mrs)

;;; Utility functions for use with xml package
;;; Only loaded when this has been loaded, please
;;; don't add #+:xml compiler macros since I'm trying
;;; to keep rmrs as free of compiler macros as possible

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
  (let ((str  (cadr content)))
    (unless (stringp str)
      (error "string expected but ~A found" str))
    (string-trim  
     '(#\space #\tab #\newline #\page #\return #\linefeed)
     str)))

(defun parse-xml-removing-junk (istream)
  ;;; parser insists on tree of `proper' elements
  ;;; so we just need to find this
  (let ((raw-xml (xml:parse-xml istream)))
    (dolist (xml-el raw-xml)
      (unless (member (car xml-el) '(:XML :DOCTYPE :COMMENT))
        (return xml-el)))))


(defun remove-xml-whitespace-elements (content)
  ;;; sl revised version of fn contributed by Fabre
  (loop for x in content
      unless (and (stringp x) (xml-whitespace-string-p x))
      collect
      (cond ((or (symbolp x) (stringp x)) x)
	    ((listp x) (remove-xml-whitespace-elements x))
	    (t (error 
		"Unexpected element in remove-xml-whitespace-elements")))))
