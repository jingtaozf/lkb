(in-package :lkb)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to a simple flat-file lexical cache
;;;

(defclass simple-lex-database (lex-database)
  ((psorts-stream :initform nil)))

#+:null
(setf *lexicon* (make-instance 'simple-lex-database))

(defmethod lexicon-loaded-p ((lexicon simple-lex-database))
  (and (streamp (slot-value lexicon 'psorts-stream))
       (open-stream-p (slot-value lexicon 'psorts-stream))))

(defmethod lex-words ((lexicon simple-lex-database))
  (let ((words nil))
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k words))
	     (slot-value lexicon 'lexical-entries))
    words))

(defmethod read-cached-lex ((lexicon simple-lex-database) filenames)
  (set-temporary-lexicon-filenames)
  (let* ((ok nil)
	 (cache-date
	  (if (and *psorts-temp-file* 
		   (probe-file *psorts-temp-file*))
	      (file-write-date *psorts-temp-file*)))
	 (cache-index-date 
	  (if 
	      (and
	       *psorts-temp-index-file*
	       (probe-file *psorts-temp-index-file*))
	      (file-write-date *psorts-temp-index-file*)))
	 (last-file-date
	  (apply #'max (loop for file in filenames
                           nconc
                             (let ((date (file-write-date file)))
                               (if date (list date)))))))
    (when (and cache-date last-file-date cache-index-date
	       (> cache-date last-file-date) 
	       (> cache-index-date last-file-date))
      (setf ok t)
      (format t "~%Reading in cached lexicon")
      (clear-lex lexicon :no-delete t)
      (handler-case 
	  (read-psort-index-file)
	(error (condition)
	  (format t "~%Error: ~A" condition)
	  (delete-temporary-lexicon-files lexicon)
	  (setf ok nil)))
      (cond (ok (format t "~%Cached lexicon read")
		t)
	    (t (format t "~%Cached lexicon missing or out-of-date: reading lexicon source files")
	       nil)))))


(defmethod store-cached-lex ((lexicon simple-lex-database))
  ;; assume that this is only going to be called after a user has
  ;; successfully done a psorts-temp-file and by an advanced user, so
  ;; don't bother with fancy error checking
  (let ((ok nil))
    (with-slots (psorts psorts-stream) lexicon
      (when *psorts-temp-index-file* 
	(unwind-protect
	    (progn
	      (with-open-file (ostream *psorts-temp-index-file* 
			       :direction :output
			       :if-exists :supersede)
		(maphash #'(lambda (id value)
			     (prin1 id ostream)
			     (write-string " " ostream)
			     (prin1 (car value) ostream)
			     (write-string " " ostream)
			     (prin1 (cadr value) ostream)
			     (terpri ostream)
			     )
			 psorts))
	      (setf ok t))
	  ;; if there's an error during the writing of the index file,
          ;; delete it
          #|
	  (when (and (streamp psorts-stream)
		     (open-stream-p psorts-stream))
	    (finish-output psorts-stream)
            (close psorts-stream))
            |#
	  (unless ok
	    (when (probe-file *psorts-temp-index-file*)
	      (delete-file *psorts-temp-index-file*))))))))

(defmethod clear-lex ((lexicon simple-lex-database) &rest rest)
  (declare (ignore rest))
  ;; Close temporary lexicon file
  (with-slots (psorts-stream) lexicon
      (when (and (streamp psorts-stream)
		 (open-stream-p psorts-stream))
	(close psorts-stream))))

(defmethod collect-expanded-lex-ids ((lexicon simple-lex-database))
  ;; useful for creating a subset of a lexicon which corresponds to a
  ;; particular test suite
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (if (and (cddr value)
                          (lex-entry-full-fs (cddr value)))
                     (push id ids)))
	     (slot-value lexicon 'psorts))
    ids))

(defmethod store-psort ((lexicon simple-lex-database) id entry &optional orth)
  ;; write new entry to the end of the file
  ;; update the hash table entry with the new file pointer
  (with-slots (psorts-stream psorts) lexicon
    (unless (and (streamp psorts-stream)
		 (open-stream-p psorts-stream))
      (open-psorts-stream lexicon))
    (let ((current-file-end (file-length psorts-stream))
	  (specified-entry (cons id entry))
	  (*print-pretty* nil))
      (file-position psorts-stream current-file-end)
      (write specified-entry :stream psorts-stream :level nil :length nil)
      (terpri psorts-stream)
      (when (gethash id psorts)
	(format t "~%Redefining ~A" id))
      (setf (gethash id psorts)
	(list orth current-file-end))))
  id)

;;; todo: cache
(defmethod read-psort ((lexicon simple-lex-database) id &key (cache t)  (recurse t))
  (declare (ignore cache recurse))
  (with-slots (psorts) lexicon
    (let ((hash-table-entry (gethash id psorts)))
      (when hash-table-entry
	(or (cddr hash-table-entry)	; cached
	    (let* ((file-pointer (cadr hash-table-entry))
		   (file-entry (cdr (read-psort-entry-from-file 
				     (slot-value lexicon 'psorts-stream)
				     file-pointer id))))
	      (setf (cddr (gethash id psorts)) file-entry)
	      file-entry))))))

(defmethod unexpand-psort ((lexicon simple-lex-database) id)
  (setf (cddr (gethash id (slot-value lexicon 'psorts))) nil))

(defmethod collect-psort-ids ((lexicon simple-lex-database) &key (recurse t))
  (declare (ignore recurse))
  (let ((ids nil))
    (maphash 
     #'(lambda (name val)
	 (declare (ignore val))
	 (push name ids))
     (slot-value lexicon 'psorts))
    ids))

;; Utilities for flat-file lexical database

(defun open-psorts-stream (lexicon)
  (with-slots (psorts-stream) lexicon
    (unless (and (streamp psorts-stream)
		 (open-stream-p psorts-stream))
      (set-temporary-lexicon-filenames)
      (handler-case
	  (setf psorts-stream
	    (open *psorts-temp-file* 
		  :direction :io
		  :if-exists :append
		  :if-does-not-exist :create))
	(error (condition)
	  (format t "~%Error ~A in opening temporary lexicon file" condition)))
      (unless (and psorts-stream
		   (input-stream-p psorts-stream)
		   (output-stream-p psorts-stream))
	(error "~%Failure to open temporary lexicon file ~A correctly.
               Please create the appropriate directory 
               if it does not currently exist or redefine
               the user functions lkb-tmp-dir and/or
               set-temporary-lexicon-filenames.  Then reload grammar."
	       *psorts-temp-file*)))))

(defun flush-psorts-stream (lexicon)
  (with-slots (psorts-stream) lexicon
    (unless (and (streamp psorts-stream)
		 (output-stream-p psorts-stream))
      (error "Temporary file ~A unexpectedly closed" 
	     *psorts-temp-file*))
    (finish-output psorts-stream)))

(defun read-psort-entry-from-file (psorts-stream file-pointer id)
   #+(and mcl powerpc)(decf ee (CCL::TOTAL-BYTES-ALLOCATED))
   (finish-output psorts-stream)
   (prog1
      (let ((successful-positioning 
            (file-position psorts-stream file-pointer)))
         (unless successful-positioning 
            (error "~%Can't retrieve entry for ~A" id))
         (with-package (:lkb) (read psorts-stream t)))
     #+(and mcl powerpc)(incf ee (CCL::TOTAL-BYTES-ALLOCATED))))

;; Never called?
;;(defun uncache-psort-entry (id)
;;   (let ((hash-table-entry 
;;            (gethash id *psorts*)))
;;      (when hash-table-entry
;;         (setf (cddr hash-table-entry) nil))))

(defun read-psort-index-file nil
  (with-slots (psorts psorts-stream lexical-entries) *lexicon*
    (when (and (streamp psorts-stream)
	       (open-stream-p psorts-stream))
      (finish-output psorts-stream)
      (close psorts-stream))
    (when (and *psorts-temp-file* 
	       *psorts-temp-index-file*
	       (probe-file *psorts-temp-file*)
	       (probe-file *psorts-temp-index-file*))
      (with-open-file (istream *psorts-temp-index-file* :direction :input)
	(unless (input-stream-p istream)
	  (error "~%Failed to open temporary file correctly" 
		 *psorts-temp-index-file*))
	(clrhash lexical-entries)
        (with-package (:lkb)
          (loop
            for id = (read istream nil nil)
            for orth = (and id (read istream nil nil))
            for file-pos = (and orth (read istream nil nil))
            while id
            do
              (setf (gethash id psorts) (list orth file-pos))
              (dolist (orth-el orth)
                (pushnew id (gethash (string-upcase orth-el) 
                                     lexical-entries))))))
      (open-psorts-stream *lexicon*))))

