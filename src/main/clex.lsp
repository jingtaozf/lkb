;;; Copyright (c) 1999--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


(in-package :lkb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to an off-line hash table lexical cache
;;;

(defclass cdb-lex-database (lex-database)
  ((psort-db :initform nil :accessor psort-db)
   (orth-db :initform nil :accessor orth-db)))

(setf *lexicon* (make-instance 'cdb-lex-database))

(defmethod lookup-word ((lexicon cdb-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (with-slots (orth-db) lexicon
  (unless (stringp orth)
    (error "~a is not a string." orth))
  (when (and (null orth-db) *psorts-temp-index-file*)
    (setf orth-db (cdb:open-read *psorts-temp-index-file*)))
  (when orth-db
    (loop
     for record in (cdb:read-record orth-db orth)
     collect (intern record :lkb)))))

(defmethod lexicon-loaded-p ((lexicon cdb-lex-database))
  (not (null (psort-db lexicon))))

(defmethod lex-words ((lexicon cdb-lex-database))
  (unless (orth-db lexicon)
    (setf (orth-db lexicon) 
      (cdb:open-read *psorts-temp-index-file*)))
  (cdb:all-keys (orth-db lexicon)))

(defmethod collect-psort-ids ((lexicon cdb-lex-database) &key (recurse t))
  (declare (ignore recurse))
  (unless (psort-db lexicon)
    (setf (psort-db lexicon) 
      (cdb:open-read *psorts-temp-file*)))
;bmw
  (let ((res (cdb:all-keys (psort-db lexicon))))
    (unless (equal res '("NIL"))
      res)))

(defmethod read-cached-lex ((lexicon cdb-lex-database) filenames)
  (set-temporary-lexicon-filenames)
  (when (up-to-date-p filenames
		      (list *psorts-temp-file* 
			    *psorts-temp-index-file*))
    (format t "~%Reading in cached lexicon")
    (clear-lex lexicon t)
    (when (handler-case 
	      (progn
		(setf (psort-db lexicon) 
		  (cdb:open-read *psorts-temp-file*))
		(setf (orth-db lexicon) 
		  (cdb:open-read *psorts-temp-index-file*))
		t)
	    (error (condition)
	      (format t "~%Error: ~A~%" condition)
	      (delete-temporary-lexicon-files lexicon)
	      nil))
      (format t "~%Cached lexicon read")
      (return-from read-cached-lex t)))
  (format t "~%Cached lexicon missing or out-of-date: reading lexicon source files")
  nil)

(defmethod store-cached-lex ((lexicon cdb-lex-database))
  (cdb:close-write (psort-db lexicon))
  (cdb:close-write (orth-db lexicon))
  (setf (psort-db lexicon) nil)
  (setf (orth-db lexicon) nil))

(defmethod set-lexical-entry ((lexicon cdb-lex-database) orth id new-entry)
  (store-psort lexicon id new-entry orth)
  (dolist (orth-el orth)
    (cdb:write-record (orth-db lexicon) (string-upcase orth-el) (string id)))
  orth)

(defmethod store-psort ((lexicon cdb-lex-database) id entry &optional orth)
  (declare (ignore orth))
  (unless (psort-db lexicon)
    (setf (psort-db lexicon) (cdb:open-write *psorts-temp-file*))
    (setf (orth-db lexicon) (cdb:open-write *psorts-temp-index-file*)))
  (cdb:write-record (psort-db lexicon) (string id) 
		    (with-standard-io-syntax (write-to-string entry)))
  id)


(defmethod read-psort ((lexicon cdb-lex-database) id &key (cache t) (recurse t))
  (declare (ignore recurse))
  (unless (psort-db lexicon)
    (setf (psort-db lexicon) 
      (cdb:open-read *psorts-temp-file*)))
  (with-slots (psorts) lexicon
    (cond ((gethash id psorts))
	  (t
	   ;; In case multiple entries are returned, we take the last one
	   (let* ((rec (car (last (cdb:read-record (psort-db lexicon) 
						   (string id)))))
		  (entry (when rec 
                           (with-package (:lkb) (read-from-string rec)))))
	     (when (and entry cache)
	       (setf (gethash id psorts) entry))
	     entry)))))

(defmethod clear-lex ((lexicon cdb-lex-database) &optional no-delete)
  (declare (ignore no-delete))
  ;; Close temporary lexicon files
  (when (orth-db lexicon)
    (cdb:close-read (orth-db lexicon))
    (setf (orth-db lexicon) nil))
  (when (psort-db lexicon)
    (cdb:close-read (psort-db lexicon))
    (setf (psort-db lexicon) nil))
  nil)

(defmethod collect-expanded-lex-ids ((lexicon cdb-lex-database))
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (when (and value
			    (lex-entry-full-fs value))
		   (push id ids)))
	     (slot-value lexicon 'psorts))
    ids))

(defmethod unexpand-psort ((lexicon cdb-lex-database) id)
  (setf (gethash id (slot-value lexicon 'psorts)) nil))

;;; hack
(defmethod delete-temporary-lexicon-files ((lexicon cdb-lex-database))
  (delete-temporary-lexicon-files-aux))
