;;; Copyright (c) 1999--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron;
;;;   see `licence.txt' for conditions.


;;; modifications by bmw (dec-03)
;;; - internal reworking of cdb-lex-database + cdb-leaf-database classes 
;;;   and associated script functions

;;; modifications by bmw (aug-03)
;;; - cdb open/close reimplemented cleanly

;;; modifications by bmw (aug-03)
;;; - multiple cdb lexicons can run in parallel
;;; - fixed code broken by *lexicon*-related changes

(in-package :lkb)

;(defvar *lex-file-list*)

(defparameter *syntax-error* nil
  "boolean that is set to t if a syntax error is detected")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to an off-line hash table lexical cache
;;;

(defclass cdb-lex-database (lex-database)
  ((psort-db :initform nil :accessor psort-db)
   (orth-db :initform nil :accessor orth-db)
   (temp-file :initform nil
	      :accessor temp-file
	      :initarg :temp-file)
   (temp-index-file :initform nil
		    :accessor temp-index-file
		    :initarg :temp-index-file)
   (source-files :initform nil :accessor source-files)))

(setf *lexicon* (make-instance 'cdb-lex-database))

;; lexicon is open...
;; opens cached lexicon for reading
(defmethod read-cached-lex ((lex cdb-lex-database) filenames)
  (unless (open-p lex)
    (error "lexicon (~a) is not open" (name lex)))
  (with-slots (temp-file temp-index-file) lex
    (cond
     ((up-to-date-p filenames (list temp-file temp-index-file))
      (format t "~%Reading in cached lexicon (~a)" (name lex))
      (when (open-read lex)
        (setf (source-files lex) filenames)
	(format t "~%Cached lexicon read")
	t))
     (t
      (format t "~%Cached lexicon (~a) missing or out-of-date: reading lexicon source files" (name lex))
      nil))))
  
;; lexicon is open...
;; build cache and open lexicon for reading
(defmethod load-lex-from-files ((lexicon cdb-lex-database) filenames syntax)
  (unless (open-p lexicon)
    (error "lexicon is not open"))
  (build-cache lexicon filenames syntax)
  (unless (open-read lexicon)
    (error "cannot open cache file for lexicon ~a" lexicon)))

;; lexicon is open...
;; open-write lexicon, read filenames whilst building cache, close-write lexicon
(defmethod build-cache ((lexicon cdb-lex-database) filenames syntax)
  (unless (open-p lexicon)
    (error "lexicon is not open"))
  (with-slots (invalid-p source-files) lexicon
;    (setf invalid-p nil)
    (open-write lexicon)
    (setf source-files filenames) 
;    (setf *lex-file-list* filenames) ;;fix_me
    (setf *ordered-lex-list* nil) ;;fix_me
    (cond
     ((check-load-names filenames 'lexical)
      (let ((syntax-error *syntax-error*))
	(let* ((*lexicon-in* lexicon);; *lexicon-in* is needed deep inside read-...-file-aux
	       (*syntax-error* nil)) 
	  (dolist (file-name filenames)
	    (ecase syntax
	      (:tdl (read-tdl-lex-file-aux-internal file-name))
	      (:path (read-lex-file-aux-internal file-name))))
	  (setf invalid-p *syntax-error*)
	  (setf syntax-error (or syntax-error *syntax-error*)))
	(setf *syntax-error* syntax-error))
      (close-read-write lexicon)
      (when invalid-p
	(format t "~%(discarding invalid lexicon)")
	(close-lex lexicon :delete t)
	(unless
            (open-lex lexicon
                      :parameters (list (make-nice-temp-file-pathname ".empty")
                                        (make-nice-temp-file-pathname ".empty-index")))
          (return-from build-cache))
	(write-empty-lex lexicon))
      t)
     (t
      (cerror "Continue" "Lexicon file not found")
      (close-read-write lexicon)
      nil))))

;; sets temp-file/temp-index-file
(defmethod open-lex ((lexicon cdb-lex-database) &key name parameters)
  (let ((temp-file (first parameters))
	(temp-index-file (second parameters)))
    (if (open-p lexicon)
	(close-lex lexicon :in-isolation t))
    (setf (name lexicon) name)
    (setf (temp-file lexicon) temp-file)
    (setf (temp-index-file lexicon) temp-index-file))
  t)

(defmethod open-p ((lexicon cdb-lex-database))
  (with-slots (temp-file temp-index-file) lexicon
      (cond
       ((and temp-file temp-index-file)
	t)
       ((not (or temp-file temp-index-file))
	nil)
       (t
	(error "internal")))))

;; lexicon is open...
(defmethod open-read ((lexicon cdb-lex-database))
  (unless (open-p lexicon)
    (return-from open-read nil))
  (if (open-read-write-p lexicon)
      (close-read-write lexicon))
  (handler-case 
      (with-slots (orth-db psort-db temp-file temp-index-file) lexicon
	(progn
	  (setf psort-db (cdb:open-read temp-file))
	  (setf orth-db (cdb:open-read temp-index-file))
	  t))
    (error (condition)
      (format t "~%Error: ~A~%" condition)
      (delete-temporary-lexicon-files lexicon)
      nil)))

;; lexicon is open...
(defmethod open-write ((lexicon cdb-lex-database))
  (unless (open-p lexicon)
    (return-from open-write nil))
  (if (open-read-write-p lexicon)
      (close-read-write lexicon))
  (handler-case 
      (with-slots (orth-db psort-db) lexicon
 	(progn
	  (setf psort-db (cdb:open-write (temp-file lexicon)))
	  (setf orth-db (cdb:open-write (temp-index-file lexicon)))
	  t))
    (error (condition)
      (format t "~%Error: ~A~%" condition)
      (delete-temporary-lexicon-files lexicon)
      nil)))

(defmethod open-read-write-p ((lexicon cdb-lex-database))
  (or (open-read-p lexicon)
      (open-write-p lexicon)))
  
(defmethod open-read-p ((lexicon cdb-lex-database))
  (with-slots (orth-db psort-db) lexicon
    (when (and orth-db psort-db)
      (cond
       ((and (eq (cdb::cdb-mode orth-db) :input)
	     (eq (cdb::cdb-mode psort-db) :input))
	t)
       ((and (eq (cdb::cdb-mode orth-db) :output)
	     (eq (cdb::cdb-mode psort-db) :output))
	nil)
       ((and (eq (cdb::cdb-mode orth-db) nil)
	     (eq (cdb::cdb-mode psort-db) nil))
	nil)
       (t
	(error "internal"))))))

(defmethod open-write-p ((lexicon cdb-lex-database))
  (with-slots (orth-db psort-db) lexicon
    (when (and orth-db psort-db)
      (cond
       ((and (eq (cdb::cdb-mode orth-db) :output)
	     (eq (cdb::cdb-mode psort-db) :output))
	t)
       ((and (eq (cdb::cdb-mode orth-db) :input)
	     (eq (cdb::cdb-mode psort-db) :input))
	nil)
       ((and (eq (cdb::cdb-mode orth-db) nil)
	     (eq (cdb::cdb-mode psort-db) nil))
	nil)
       (t
	(error "internal"))))))

(defmethod close-lex ((lexicon cdb-lex-database) &key in-isolation delete)
  (declare (ignore in-isolation))
  (with-slots (source-files temp-file temp-index-file) lexicon
    (if (open-p lexicon)
        (close-read-write lexicon))
    (if delete
	(delete-temporary-lexicon-files lexicon))
    (setf source-files nil)
    (setf temp-file nil)
    (setf temp-index-file nil)))

(defmethod empty-cache ((lexicon cdb-lex-database) &key recurse)
  (declare (ignore recurse))
  )

;; lexicon is open...
(defmethod close-read-write ((lexicon cdb-lex-database))
  (unless (open-p lexicon)
    (error "lexicon is not open"))
  (handler-case 
      (with-slots (orth-db psort-db) lexicon
	(when psort-db 
	  (cdb:close-cdb psort-db)
	  (setf psort-db nil))
	(when orth-db
	  (cdb:close-cdb orth-db)
	  (setf orth-db nil)))
    (error (condition)
      (format t "~%Error: ~A~%" condition)
      (delete-temporary-lexicon-files lexicon)
      nil)))

(defmethod delete-temporary-lexicon-files ((lexicon cdb-lex-database))
  (with-slots (temp-file temp-index-file) lexicon
    (delete-temp-file temp-file)
    (delete-temp-file temp-index-file)
    t))

(defun delete-temp-file (filename)
  (and filename
       (probe-file filename)
       (delete-file filename)))

;;
;; reading
;;

(defmethod lookup-word ((lexicon cdb-lex-database) orth &key (cache t))
  (unless (open-read-p lexicon)
    (error "lexicon is not open for reading"))
  (let ((hashed (gethash orth 
			 (slot-value lexicon 'lexical-entries))))
  (cond 
   (hashed
    (if (eq hashed :empty)
	nil
      hashed))
   (t 
    (let ((value (lookup-word-cdb-lex-database lexicon orth)))
      ;:if caching, add entry to cache...
      (when cache
	(setf (gethash orth 
		       (slot-value lexicon 'lexical-entries)) 
	  (if value 
	      value 
	    :empty)))
      value)))))

;; lexicon is open for reading...
(defun lookup-word-cdb-lex-database (lexicon orth)
  (with-slots (orth-db) lexicon
    (unless (stringp orth)
      (error "string expected (~a)" orth))
    (when orth-db
      (loop
	  for record in (cdb:read-record orth-db orth)
	  collect (intern record :lkb)))))

(defmethod lex-words ((lexicon cdb-lex-database))
  (unless (open-read-p lexicon)
    (error "lexicon is not open for reading"))
  (with-slots (orth-db) lexicon
    (when orth-db
      (cdb:all-keys orth-db))))

(defmethod collect-psort-ids ((lexicon cdb-lex-database) &key (cache t) (recurse t))
  (declare (ignore recurse))
  (unless (open-read-p lexicon)
    (error "lexicon is not open for reading"))
  (with-slots (cache-lex-list) lexicon
    (let ((lex-list cache-lex-list))
      (when (null cache-lex-list)
	(setf lex-list (collect-psort-ids-aux lexicon))
	(if (null lex-list)
	    (setf lex-list :empty))
	(if cache (setf cache-lex-list lex-list)))
      (case lex-list
	(:empty nil)
	(otherwise lex-list)))))

(defmethod collect-psort-ids-aux ((lexicon cdb-lex-database))
  (unless (open-read-p lexicon) ;;fix_me sometime
    (return-from collect-psort-ids-aux nil)) 
  (with-slots (psort-db) lexicon
    (when psort-db
      (let ((res (cdb:all-keys psort-db)))
	(unless (equal res '("NIL")) ;; this is cdb output for empty lex
	  (mapcar #'2-symb res))))))

(defmethod read-psort ((lexicon cdb-lex-database) id &key (cache t) (recurse t) (new-instance nil))
  (declare (ignore recurse))
  (unless (open-read-p lexicon)
    (error "lexicon is not open for reading"))
  (with-slots (psorts psort-db) lexicon
    (let ((hashed (and (not new-instance)
		       (gethash id psorts))))
      (cond (hashed
	     (unless (eq hashed :empty)
	       hashed))
	    (t
	     ;; In case multiple entries are returned, we take the last one
	     (let* ((rec (car (last (cdb:read-record psort-db 
						     (string id)))))
		    (entry (when rec 
			     (with-package (:lkb) (read-from-string rec)))))
	       (when cache
		 (setf (gethash id psorts) 
		   (or entry :empty)))
	       entry))))))

;;
;; writing
;;

;; lexicon is open...
(defun write-empty-lex (lexicon)
  (unless (open-p lexicon)
    (error "lexicon is not open"))
  (open-write lexicon)
;  (setf *ordered-lex-list* nil)
  (store-psort lexicon nil nil nil)
  (close-read-write lexicon)
  lexicon)

(defmethod set-lexical-entry ((lexicon cdb-lex-database) orth id new-entry)
  (unless (open-write-p lexicon)
    (error "lexicon is not open for writing"))
  (with-slots (orth-db ) lexicon
    (store-psort lexicon id new-entry orth)
    (dolist (orth-el orth)
      (cdb:write-record orth-db (string-upcase orth-el) (string id)))
    orth))

;; lexicon is open for writing...
(defmethod store-psort ((lexicon cdb-lex-database) id entry &optional orth)
  (declare (ignore orth))
  (unless (open-write-p lexicon)
    (error "lexicon is not open for writing"))
  (with-slots (orth-db psort-db) lexicon
    (cdb:write-record psort-db (string id) 
		      (with-standard-io-syntax (write-to-string entry)))
    id))
