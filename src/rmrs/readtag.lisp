;;; Copyright (c) 2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

(defparameter *tag-templates* nil)

(defparameter *unknown-tags* nil)

(defun show-unknown-tags nil
  (show-unknown *unknown-tags*))

;;; Reading

(defun read-rmrs-tag-templates (file-name)
  ;;; <!ELEMENT lex (le)*>
  (setf *tag-templates* nil)
  (with-open-file (istream file-name :direction :input)
    (let ((templates (parse-xml-removing-junk istream)))
      (unless (equal (car templates) '|lex|)
        (error "~A is not a valid lexical tags file" file-name))
      (loop for template in (cdr templates)
          do
            (unless (xml-whitespace-string-p template)
            (let ((next-tag-template
                   (read-rmrs-tag-template template)))
              (when next-tag-template
              (add-rmrs-tag-template next-tag-template)))))))
  (setf *tag-templates*
    (nreverse *tag-templates*))
  nil)

(defun add-rmrs-tag-template (tag-template)
  (push tag-template *tag-templates*))

(defun get-tag-template (tag-name)
  (or 
   (find tag-name *tag-templates*
         :test #'string-equal :key #'rmrs-tag-template-name)
   (progn (push tag-name *unknown-tags*)
          nil)))

(defun read-rmrs-tag-template (real-xml)
;;;  <!ELEMENT le (tag, semstruct)>
  (let* ((tag (car real-xml)))
    (if (eq tag '|le|)
	(let ((name nil) 
	      (semstruct nil))
              (loop for next-el in (cdr real-xml)
                 do
                 (unless (xml-whitespace-string-p next-el)
                   (let ((next-tag (car next-el)))    
                     (ecase next-tag
		       (|tag| (setf name (cadr next-el)))
                       (|semstruct| 
                           (setf semstruct 
                            (read-rmrs-semstruct (cdr next-el))))))))
	  (make-rmrs-tag-template :name name
				  :semstruct semstruct)))))

;;; output

(defun write-rmrs-tags (filename)
  (with-open-file (ostream filename :direction :output
                   :if-exists :supersede)
    (format ostream "~%<lex>")
    (loop for tag in *tag-templates*
        do
          (output-rmrs-tag tag ostream))
    (format ostream "~%</lex>~%")))

(defun output-rmrs-tag (tag ostream)
  (let ((semstruct (rmrs-tag-template-semstruct tag)))
    (format ostream "~%<le>")
    (format ostream "~%<tag>~A</tag>" (rmrs-tag-template-name tag))
    (when semstruct
      (output-rmrs-semstruct semstruct ostream))
    (format ostream "~%</le>~%")))

