;;; Copyright (c) 1999--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;; modifications by bmw (aug-03)
;;; - multiple cdb lexicons can run at once
;;; - *psorts-temp-file* etc. moved into slots
;;; - fixed code broken by *lexicon*-related changes

(in-package :lkb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to an off-line hash table lexical cache
;;;

(defun get-new-filename (filename)
  (loop
      until (not (probe-file filename))
      do
	(setf filename (format nil "~aX" filename))
      finally
	(return filename)))

(defclass cdb-lex-database (lex-database)
  ((psort-db :initform nil :accessor psort-db)
   (orth-db :initform nil :accessor orth-db)
   (psorts-temp-file :initform (make-pathname :name "templex" 
					      :directory (pathname-directory (lkb-tmp-dir)))
		     :accessor psorts-temp-file)
   (psorts-temp-index-file :initform (make-pathname :name "templex-index" 
						    :directory (pathname-directory (lkb-tmp-dir)))
		     :accessor psorts-temp-index-file)
   (all-cdb-lex-dbs :allocation :class :initform nil :accessor all-cdb-lex-dbs)))

(defmethod lookup-word ((lexicon cdb-lex-database) orth &key (cache t))
  (declare (ignore cache))
  (with-slots (orth-db) lexicon
  (unless (stringp orth)
    (error "~a is not a string." orth))
  (when (and (null orth-db) (psorts-temp-index-file lexicon))
    (setf orth-db (cdb:open-read (psorts-temp-index-file lexicon))))
  (when orth-db
    (loop
     for record in (cdb:read-record orth-db orth)
     collect (intern record :lkb)))))

(defmethod lexicon-loaded-p ((lexicon cdb-lex-database))
  (not (null (psort-db lexicon))))

(defmethod lex-words ((lexicon cdb-lex-database))
  (unless (orth-db lexicon)
    (setf (orth-db lexicon) 
      (cdb:open-read (psorts-temp-index-file lexicon))))
  (cdb:all-keys (orth-db lexicon)))

(defmethod collect-psort-ids ((lexicon cdb-lex-database) &key (recurse t))
  (declare (ignore recurse))
  (unless (psort-db lexicon)
    (setf (psort-db lexicon) 
      (cdb:open-read (psorts-temp-file lexicon))))
;bmw
  (let ((res (cdb:all-keys (psort-db lexicon))))
    (unless (equal res '("NIL"))
      res)))

(defmethod read-cached-lex ((lexicon cdb-lex-database) filenames)
  (with-slots (psorts-temp-file) lexicon
    (set-temporary-lexicon-filenames)
  (when (up-to-date-p filenames
		      (list psorts-temp-file 
			    (psorts-temp-index-file lexicon)))
    (format t "~%Reading in cached lexicon")
    (clear-lex lexicon :no-delete t)
    (when (handler-case 
	      (progn
		(setf (psort-db lexicon) 
		  (cdb:open-read psorts-temp-file))
		(setf (orth-db lexicon) 
		  (cdb:open-read (psorts-temp-index-file lexicon)))
		(unless (> (cdb:num-entries (psort-db lexicon)) 1) (error "cached lexicon empty"))
		t)
	    (error (condition)
	      (format t "~%Error: ~A~%" condition)
	      (delete-temporary-lexicon-files lexicon)
	      nil))
      (format t "~%Cached lexicon read")
      (return-from read-cached-lex t)))
  (format t "~%Cached lexicon missing or out-of-date: reading lexicon source files")
  nil))
  
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
    (setf (psort-db lexicon) (cdb:open-write (psorts-temp-file lexicon)))
    (setf (orth-db lexicon) (cdb:open-write (psorts-temp-index-file lexicon))))
  (cdb:write-record (psort-db lexicon) (string id) 
		    (with-standard-io-syntax (write-to-string entry)))
  id)

(defmethod read-psort ((lexicon cdb-lex-database) id &key (cache t) (recurse t))
  (declare (ignore recurse))
  (unless (psort-db lexicon)
    (setf (psort-db lexicon) 
      (cdb:open-read (psorts-temp-file lexicon))))
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

(defmethod clear-lex ((lexicon cdb-lex-database) &rest rest)
  (let* ((no-delete (get-keyword-val :no-delete rest))
	 (psorts-temp-files (get-keyword-val :psorts-temp-files rest))
	 (psorts-temp-file (car psorts-temp-files))
	 (psorts-temp-index-file (cdr psorts-temp-files))
	 (lexicon-size 0))

    ;; extract psorts-temp-file and psorts-temp-index-file
    (if (and psorts-temp-file
	     (not psorts-temp-index-file))
	(error "both psorts-temp-file and psorts-temp-index-file must be specified"))
    (unless psorts-temp-files 
      (setf psorts-temp-file (psorts-temp-file lexicon))
      (setf psorts-temp-index-file (psorts-temp-index-file lexicon)))
      
    ;; close (old) temporary lexicon files
    (setf (psort-db lexicon) 
      (and
       (probe-file (psorts-temp-file lexicon))
       (cdb:open-read (psorts-temp-file lexicon))))
    
    (when (orth-db lexicon)
      (cdb:close-read (orth-db lexicon))
      (setf (orth-db lexicon) nil))
    (when (psort-db lexicon)
      (setf lexicon-size 
	(cdb:num-entries (psort-db lexicon)))
      (cdb:close-read (psort-db lexicon))
      (setf (psort-db lexicon) nil))
    (unless 
	(and 
	     no-delete 
	     (> lexicon-size 1))
      (delete-temporary-lexicon-files lexicon)
      ;;(setf (psorts-temp-file lexicon) nil)
      ;;(setf (psorts-temp-index-file lexicon) nil)
      )
    
    ;; prepare (new) temporary lexicon filenames    
    (setf (psorts-temp-file lexicon) psorts-temp-file)
    (setf (psorts-temp-index-file lexicon) psorts-temp-index-file)
    
    ;; if files do not both exist create new ones
    (unless
	(and 
	 (probe-file (psorts-temp-file lexicon))
	 (probe-file (psorts-temp-index-file lexicon)))
      (create-empty-cdb-lex-aux lexicon))
    lexicon))

(defmethod initialize-lex ((lexicon cdb-lex-database) &key no-delete psorts-temp-files)
  (clear-lex 
   lexicon 
   :no-delete no-delete 
   :psorts-temp-files psorts-temp-files))

(defmethod delete-temporary-lexicon-files ((lexicon cdb-lex-database))
;;  (setf (all-cdb-lex-dbs lexicon)
;;    (remove (psorts-temp-file lexicon) (all-cdb-lex-dbs lexicon) :key #'psorts-temp-file :test 'equal))
  (with-slots (psorts-temp-file psorts-temp-index-file) lexicon
    (when (and psorts-temp-file
	       (probe-file psorts-temp-file))
      (delete-file psorts-temp-file))
    (when (and (psorts-temp-index-file lexicon)
	       (probe-file (psorts-temp-index-file lexicon)))
      (delete-file (psorts-temp-index-file lexicon)))
    ;;(setf (psorts-temp-file lexicon) nil)
    ;;(setf (psorts-temp-index-file lexicon) nil)
    (cons psorts-temp-file psorts-temp-index-file)))

(defun create-empty-cdb-lex nil
  (create-empty-cdb-lex-aux (make-instance 'cdb-lex-database)))

(defun create-empty-cdb-lex-aux (lexicon)
  (setf *ordered-lex-list* nil)
  (store-psort lexicon nil nil nil)
  (store-cached-lex lexicon)
  (push lexicon (all-cdb-lex-dbs lexicon))
  lexicon)

(defmethod load-lex ((lexicon cdb-lex-database) &rest rest)
  (let ((filename (get-keyword-val :filename rest)))
    (if (null filename)
	(error "no :filename supplied"))
    (clear-lex lexicon :in-isolation t)
    (if (check-load-names (list filename) 'lexical)
	(let ((*lexicon-in* lexicon)) ;;ugly 
	  (read-tdl-lex-file-aux-internal filename))
      (error "Lexicon file not found"))
    (store-cached-lex lexicon)
    lexicon))

(setf *lexicon* (make-instance 'cdb-lex-database))
