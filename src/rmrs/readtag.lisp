(in-package :mrs)

(defparameter *tag-templates* nil)

(defparameter *unknown-tags* nil)

(defun show-unknown-tags nil
  (show-unknown *unknown-tags*))

;;; Reading

(defun read-rmrs-tag-templates (file-name)
  (setf *tag-templates* nil)
  (let ((*readtable* (make-xml-break-table)))
    (with-open-file (istream file-name :direction :input)
      (loop
        (let ((next-tag-template (read-rmrs-tag-template istream)))
          (when (null next-tag-template) (return))
          (add-rmrs-tag-template next-tag-template)))))
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

(defun read-rmrs-tag-template (istream)
  #|
<le>
<tag>N</tag>
<semstruct>
<hook><index>x</index></hook>
<ep><pred></pred><arg>x</arg></ep> 
</semstruct>
</le>
|#
  (let ((tag (read-next-tag istream)))
    (if (eq tag 'LE)
	(let ((name nil) 
	      (semstruct nil))
	  (loop
	    (let  
		((next-tag (read-next-tag istream)))
	      (ecase next-tag
		(TAG (setf name (read-rmrs-tag-name istream)))
		(SEMSTRUCT (setf semstruct (read-rmrs-semstruct istream)))
		(/LE (return)))))
	  (make-rmrs-tag-template :name name
				  :semstruct semstruct)))))

(defun read-rmrs-tag-name (istream)
;;;  <tag>N</tag>
  (let ((tag
	 (read istream)))
    (check-for-end-tag 'tag istream)
    (string tag)))


;;; output

(defun write-rmrs-tags (filename)
  (with-open-file (ostream filename :direction :output
                   :if-exists :supersede)
    (loop for tag in *tag-templates*
        do
          (output-rmrs-tag tag ostream))))

(defun output-rmrs-tag (tag ostream)
  (let ((semstruct (rmrs-tag-template-semstruct tag)))
    (format ostream "~%<le>")
    (format ostream "~%<tag>~A</tag>" (rmrs-tag-template-name tag))
    (when semstruct
      (output-rmrs-semstruct semstruct ostream))
    (format ostream "~%</le>~%")))

