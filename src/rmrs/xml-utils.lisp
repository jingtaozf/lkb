(in-package :mrs)

(defun read-next-tag (istream)
  (let ((token (peek-char t istream nil nil))
	(tag-chars nil))
    (when (eql token #\<)
      (read-char istream)
      (loop 
	(let 
	  ((next-char (read-char istream)))
	  (when (or (null next-char)
		    (eql next-char #\>)) 
	    (return))
	  (push next-char tag-chars)))
      (intern 
       (string-upcase 
	(coerce (nreverse tag-chars) 'string))))))
		


(defun check-for-end-tag (expected-tag istream)
    (let ((token (peek-char t istream nil nil))
	(tag-chars nil))
      (unless (eql token #\<) (error "End tag ~A expected" expected-tag))
      (read-char istream)
      (unless (eql (read-char istream) #\/) 
	(error "End tag ~A expected" expected-tag))
      (loop 
	(let 
	  ((next-char (read-char istream)))
	  (when (or (null next-char)
		    (eql next-char #\>)) 
	    (return))
	  (push next-char tag-chars)))
      (unless (eql expected-tag
		   (intern 
		    (string-upcase 
		     (coerce (nreverse tag-chars) 'string))))
	(error "End tag ~A expected" expected-tag))))

(defun is-tag (symbol)
  (let ((str (string symbol)))
    (char-equal (elt str 0) #\<)))

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

(defun read-string-to-tag-no-whitespace (istream)
  ;;; reads characters up until a < as a string, ignoring whitespace
  (let ((letter-bag nil))
    (loop 
      (let ((next-char (peek-char t istream nil nil)))
	(when (or (null next-char)
		  (char-equal next-char #\<))
	  (return))
	(push next-char letter-bag)
	(read-char istream)))
     (coerce (nreverse letter-bag) 'string)))

