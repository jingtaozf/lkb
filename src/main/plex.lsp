;;; Copyright (c) 1998--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

(defclass plob-lex-database (lex-database)
  ((initialized-p :initform nil)
   (lexical-entries :initform nil)
   (psorts :initform (make-hash-table :test #'eq))
   (stored-psorts :initform nil)))

(setf *lexicon* (make-instance 'plob-lex-database))

(defmethod lexicon-loaded-p ((lexicon plob-lex-database))
  (slot-value lexicon 'initialized-p))

;; FIX
(defmethod read-cached-lex ((lexicon plob-lex-database) filenames)
  (declare (ignore filenames))
  (prog1
      (cond ((and (p-find-symbol 'lexicon)
		  (p-boundp 'lexicon)
		  (consp (p-symbol-value 'lexicon)))
	     (format t "~%Reading in cached lexicon")
	     t)
	    (t
	     (format t "~%Cached lexicon missing or out-of-date: reading lexicon source files")
	     nil))
    (open-plob-lexicon lexicon)))

;; FIX
(defmethod store-cached-lex ((lexicon plob-lex-database))
  (store-object (p-find-symbol 'lexicon) :deep)
  t)

(defmethod clear-lex ((lexicon plob-lex-database) &rest rest)
  (declare (ignore rest))
  (when (slot-value lexicon 'initialized-p)
    (clrhash (slot-value lexicon 'lexical-entries))
    (clrhash (slot-value lexicon 'psorts))
    (clrhash (slot-value lexicon 'stored-psorts)))
  (when (fboundp 'clear-lexicon-indices)
    (funcall 'clear-lexicon-indices)))

(defmethod collect-expanded-lex-ids ((lexicon plob-lex-database))
  ;; useful for creating a subset of a lexicon which corresponds to a
  ;; particular test suite
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (if (and value
                          (lex-entry-full-fs value))
                     (push id ids)))
	     (slot-value lexicon 'psorts))
    ids))

(defmethod store-psort ((lexicon plob-lex-database) id entry &optional orth)
  (with-slots (initialized-p stored-psorts) lexicon
    (unless initialized-p
      (open-plob-lexicon lexicon))
    (when (gethash id stored-psorts)
      (format t "~%Redefining ~A" id))
    (setf (gethash id stored-psorts) entry)))

(defmethod read-psort ((lexicon plob-lex-database) id)
  (with-slots (psorts stored-psorts) lexicon
    (cond ((gethash id psorts))
	  (t (let ((stored-entry (gethash id stored-psorts)))
	       (when stored-entry
		 (setf (gethash id psorts) 
		   (copy-lex-entry stored-entry))))))))

(defmethod unexpand-psort ((lexicon plob-lex-database) id)
  (setf (gethash id (slot-value lexicon 'psorts)) nil))

(defmethod collect-psort-ids ((lexicon plob-lex-database))
  (let ((ids nil))
    (maphash 
     #'(lambda (name val)
	 (declare (ignore val))
	 (push name ids))
     (slot-value lexicon 'stored-psorts))
    ids))

;; Utilities for plob lexical database

(defun open-plob-lexicon (lexicon)
  (with-slots (initialized-p lexical-entries stored-psorts) lexicon
    (cond ((and (p-find-symbol 'lexicon)
		(p-boundp 'lexicon)
		(consp (p-symbol-value 'lexicon)))
	   (setf lexical-entries (car (p-symbol-value 'lexicon)))
	   (setf stored-psorts (cdr (p-symbol-value 'lexicon))))
	  (t
	   (p-intern 'lexicon)
	   (setf lexical-entries (make-hash-table :test #'equal))
	   (setf stored-psorts (make-hash-table :test #'eq))
	   (setf (p-symbol-value 'lexicon) 
	     (cons lexical-entries stored-psorts))))
    (setf (slot-value lexicon 'initialized-p) t)))
