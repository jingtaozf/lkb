(in-package :mrs)

(defparameter *tag-templates* nil)

(defparameter *unknown-tags* nil)

(defun show-unknown-tags nil
  (show-unknown *unknown-tags*))

;;; Reading

(defun read-rmrs-tag-templates (file-name)
  (setf *tag-templates* nil)
  (with-open-file (istream file-name :direction :input)
    (loop
      (let ((next-tag-template (read-rmrs-tag-template istream)))
	(when (null next-tag-template) (return))
	(add-rmrs-tag-template next-tag-template)))))

(defun add-rmrs-tag-template (tag-template)
  (push tag-template *tag-templates*))

(defun get-tag-template (tag-name)
  (or 
   (find tag-name *tag-templates*
         :test #'equal :key #'rmrs-tag-template-name)
   (progn (push tag-name *unknown-tags*)
          nil)))

(defun read-rmrs-tag-template (istream)
  #|
<le>
<tag> N
<semstruct>
 <hook><index> x
 <ep> <pred><arg>x</ep> 
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
	      (case next-tag
		(TAG (setf name (read-rmrs-tag-name istream)))
		(SEMSTRUCT (setf semstruct (read-rmrs-semstruct istream)))
		(t (return)))))
	  (make-rmrs-tag-template :name name
				  :semstruct semstruct)))))

(defun read-rmrs-tag-name (istream)
;;;  <tag> N
  (read-string-to-tag-no-whitespace istream))
