;;; Copyright (c) 2006
;;;  John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

;;
;; [miscellaneous XML-related functions]
;;

(defun pprint-xml (x)
  (pretty-print-xml x))

;; input xml must have NO superfluous space
(defun pretty-print-xml (xml)
  (coerce
   (cdr ;; remove initial newline introduced by code below
    (loop
       with i = -1
	with last
	with last2
	for c across xml
	if (and (char= #\< last) (char= #\/ c)) do (decf i)
	if (and (char= #\< last) (or (null last2) (char= #\> last2)))
	append (list #\Newline) into x and
	append (loop for n from 1 to i
		   collect #\Space) into x
	if (and (char= #\< last) (char= #\/ c)) do (decf i)
	if (and (char= #\< last) (char= #\! c)) do (decf i)
	if (and (char= #\/ last) (char= #\> c)) do (decf i)
	if (char= #\< c) do (incf i)
	if last append (list last) into x
	do (setf last2 last) (setf last c)
	finally (return (append x (list c)))))
    'string))


;; escape string for use as XML text
(defun xml-escape (str)
  (coerce 
   (loop
       for c across str
       if (char= #\" c) append '(#\& #\q #\u #\o #\t #\;)
       else if (char= #\' c) append '(#\& #\a #\p #\o #\s #\;)
       else if (char= #\& c) append '(#\& #\a #\m #\p #\;)
       else if (char= #\< c) append '(#\& #\l #\t #\;)
       else if (char= #\> c) append '(#\& #\g #\t #\;)
       else append (list c))
   'string))

(defun whitespace-p (str)
  (and (stringp str)
       (every #'whitespace-char-p
	      (coerce str 'list))))

(defun whitespace-char-p (c)
  (or (char= #\Space c)
      (char= #\Newline c)))

