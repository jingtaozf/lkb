(in-package :mrs)

(defun define-break-characters (char-list)
   (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char char-list)
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(defun make-xml-break-table nil 
  (define-break-characters '(#\< #\>)))

(defun read-next-tag (istream)
  (let ((token (peek-char t istream nil nil))
	(tag nil))
    (when (eql token #\<) 
      (read-char istream)
      (setf tag (read istream))
      (setf token (peek-char t istream nil nil))
      (unless (eql token #\>)
        (error "> expected and not found"))
      (read-char istream)
      tag)))
		
(defun check-for-end-tag (expected-tag istream)
  (let ((tag (read-next-tag istream)))
    (unless tag (error "end tag ~A expected and not found" expected-tag))
    (let ((str (string tag)))
      (unless (and (char-equal (elt str 0) #\/)
                   (string-equal (subseq str 1) (string expected-tag)))
        (error "end tag ~A expected and not found" expected-tag)))))
        
(defun read-string-to-tag (istream)
  ;;; reads characters up until a < as a string
  (let ((letter-bag nil))
    (loop 
      (let ((next-char (peek-char nil istream nil nil)))
	(when (or (null next-char)
		  (char-equal next-char #\<))
	  (return))
	(push next-char letter-bag)
	(read-char istream)))
    (coerce (nreverse letter-bag) 'string)))

(defun read-symbols-to-tag (istream)
  ;;; reads symbols up until a < - returns a list
  (let ((symbols nil))
    (loop 
      (let ((next-char (peek-char t istream nil nil)))
	(when (or (null next-char)
		  (char-equal next-char #\<))
	  (return))
	(push (read istream nil nil) symbols)))
    (nreverse symbols)))

