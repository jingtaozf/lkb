;;; Copyright Ann Copestake 1991-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; April 1997 - modified for YADU
;;;            - output-lexicon etc removed
;;;            - get-psort-type removed (not called)

(in-package :lkb)

;;; Lexical entries and psort storage etc

;;; Lexical entries are indexed by orthography (string)
;;; They are also all potential psorts indexed
;;; by a combination of orthography
;;; plus sense identifier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lex-database () 
  ((lexical-entries :initform (make-hash-table :test #'equal))
   (psorts :initform (make-hash-table :test #'eq))
   (temp-psorts :initform (make-hash-table :test #'eq))
   (extra-lexicons :initform nil :accessor extra-lexicons)))

(defgeneric lookup-word (lexicon orth &key (cache)))

(defgeneric lex-words (lexicon))

(defgeneric lexicon-loaded-p (lexicon))

(defgeneric read-cached-lex (lexicon filenames))

(defgeneric store-cached-lex (lexicon))

(defgeneric set-lexical-entry (lexicon orth id new-entry))

(defgeneric clear-lex (lexicon &optional no-delete))

(defgeneric collect-expanded-lex-ids (lexicon))

(defgeneric store-psort (lexicon id entry &optional orth))

(defgeneric store-temporary-psort (lexicon id entry))

(defgeneric read-psort (lexicon id &key (cache)))

(defgeneric unexpand-psort (lexicon id))

(defgeneric collect-psort-ids (lexicon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This isn't getting used correctly, since it's not preserved in the
;; lexical cache.  But it is only used for outputting source
;; files in other syntaxes and batch checking, where 
;; it is acceptable to have to read in a fresh lexicon

(defvar *ordered-lex-list* nil)

(defparameter *batch-mode* nil
   "set when indexing to prevent errors in expanding a lexical entry being
   signalled as continuable errors, rather than written
   to a file.")

(defstruct (lex-or-psort) 
   orth
   infl-pos ; for a multi-word entry only - a number
            ; indicating the element that can be inflected
            ; or NIL - no inflection
   sense-id
   id ; for a lexical entry this is an
      ; atom formed by combining the strings
   language
   unifs
   def-unifs
   full-fs)

;; interim-fs is removed from the structure since it's redundant -
;; there are calls to it in non-core, but these are just for display
;; so if it's wanted it can be recalculated; also local-fs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Database-independent lexical access functions
;;;

(defun get-psort-entry (id)
  (let ((entry (get-unexpanded-psort-entry id)))
    ;; if we haven't previously expanded then
    ;; destructively modify entry
    (when entry
      (cond ((eql (lex-or-psort-full-fs entry) :fail) nil)
	    ((lex-or-psort-full-fs entry) entry)
	    ((expand-psort-entry entry) entry)
	    (t (setf (lex-or-psort-full-fs entry) :fail)
	       nil)))))
                   
(defun get-unexpanded-psort-entry (id)
  ;; for multi words, where we don't want the full-fs to be created
  ;; until we're sure we've got all the bits
  (let ((entry (read-psort *lexicon* id)))
    entry))

(defun clear-expanded-lex nil
  (dolist (id (collect-expanded-lex-ids *lexicon*))
    (unexpand-psort *lexicon* id)))

(defun clear-non-parents nil
  (format t "~%Removing cached lexical entries")
  (clear-expanded-lex))

(defun get-psort-value (psort) 
  (let ((psort-entry (get-psort-entry psort)))
    (if psort-entry 
	(lex-or-psort-full-fs psort-entry)
      (let ((gr-entry (get-grammar-rule-entry psort)))
	(if gr-entry
	    (rule-full-fs gr-entry)
	  (let ((lr-entry (get-lex-rule-entry psort)))
	    (if lr-entry 
		(rule-full-fs lr-entry)
	      (lex-expansion-error 
	       "Return nil" 
	       (format nil "~A is not a valid psort" psort)))))))))


(defun get-lex-entry (orth)
  (for psort in (remove-duplicates (lookup-word *lexicon* orth))
       filter (get-psort-entry psort)))

(defun get-unexpanded-lex-entry (orth)
  (for psort in (remove-duplicates (lookup-word *lexicon* orth))
       filter (get-unexpanded-psort-entry psort)))

(defun add-lex-from-file (orth sense-id fs-or-type defs)
  (let* ((lex-id (if orth (make-lex-id orth sense-id) sense-id))
         (orth-string (if (and orth *sense-unif-fn*)
                          (format nil "~A" orth) 
			(extract-orth-from-unifs fs-or-type)))
         (infl-pos (if (and (listp orth-string) (cdr orth-string))
		       ;; infl-pos is only relevant for multi-word entries
                       (find-infl-pos fs-or-type orth-string sense-id))))
    ;; adapted for the case where the orthography is only specified
    ;; in the FS; extract-orth-from-unifs must be defined on a
    ;; per-grammar basis
    (set-lexical-entry *lexicon* orth-string lex-id 
		       (make-lex-or-psort
			:orth orth-string
			:infl-pos infl-pos                  
			:sense-id sense-id 
			:id lex-id
			:unifs fs-or-type 
			:def-unifs defs))))

(defun extract-orth-from-unifs (unifs)
  ;; returns a list of strings
  ;; doesn't do much error checking
  (let ((matching-unifs 
         (for unif in unifs
              filter
              (if (unification-p unif)
		  (let ((unif-lhs (unification-lhs unif)))
		    (if (general-path-match unif-lhs *orth-path*)
			unif))))))
    (when matching-unifs
      (let ((n (length *orth-path*))
            (orth-strings nil))
        (cond ((null matching-unifs) nil)
              ((and (not (cdr matching-unifs))
                    (general-path-match 
                     (unification-lhs (car matching-unifs))
                     *orth-path* t))
               (u-value-types (unification-rhs (car matching-unifs))))
              (t 
               (loop
                 (let ((exact-match
                        (for unif in matching-unifs
                             keep-first
                             (let* ((path (unification-lhs unif))
                                    (feats (path-typed-feature-list path)))
                               (if
                                   (or (and (path-p path)
                                            (eq
                                             (nth n feats)
                                             (car *list-head*)))
                                       (and (typed-path-p path)
                                            (let 
                                                ((nth-el (nth n feats)))
                                              (and nth-el
                                                   (eq (type-feature-pair-feature 
                                                        nth-el)
                                                       (car *list-head*))))))
                                   (u-value-p (unification-rhs unif)))))))
                   (unless exact-match (return))
                   (push (car (u-value-types (unification-rhs exact-match)))
                         orth-strings)
                   (incf n)))
               (nreverse orth-strings)))))))
      
(defun general-path-match (unif-path path &optional exactp)      
  (or
   (and (typed-path-p unif-path)
        (typed-path-matches 
         (path-typed-feature-list unif-path) path exactp))
   (and (path-p unif-path)
        (path-matches 
         (path-typed-feature-list unif-path) path exactp))))

(defun path-matches (tfplist flist exactp)
  (cond ((and (null flist) (null tfplist)) t)
        ((null flist) (not exactp))
        ((null tfplist) nil)
        ((eq (car tfplist) (car flist))
         (path-matches (cdr tfplist) (cdr flist) exactp))
        (t nil)))

(defun typed-path-matches (tfplist flist exactp)
  (cond ((and (null flist) (null tfplist)) t)
        ((null flist) (not exactp))
        ((null tfplist) nil)
        ((eq (type-feature-pair-feature (car tfplist)) (car flist))
         (typed-path-matches (cdr tfplist) (cdr flist) exactp))
        (t nil)))  


(defun extract-orth-from-fs (tdfs)
  ;;; returns a single string, possibly concatenating the words
  (let ((fs (tdfs-indef tdfs))
        (current-orth-path *orth-path*)
        (orth-strings nil))
    (let ((simple-value (get-value-at-end-of fs current-orth-path)))
      (if (and simple-value 
               (listp simple-value)
               (stringp (car simple-value)))
          (car simple-value)
        (progn
          (loop 
            (let ((current-orth 
                   (get-value-at-end-of fs 
                                (append current-orth-path *list-head*))))
              (when (or (null current-orth) (eql current-orth 'no-way-through)
                        (not (stringp (car current-orth))))
                (return))
              (setf current-orth-path (append current-orth-path *list-tail*))
              (push (car current-orth) orth-strings)
              (push " " orth-strings)))
          (apply #'concatenate 'string (nreverse (cdr orth-strings))))))))



(defun make-lex-id (orth sense-id)
   (intern (format nil "~A_~A" orth sense-id)))
 
(defun add-psort-from-file (id fs-or-type defs)
  (store-temporary-psort *lexicon*
			 id 
			 (make-lex-or-psort :id id
					    :unifs fs-or-type
					    :def-unifs defs)))

;;; When expanding a lexical entry we want to eventually produce a
;;; tdfs which then has the non-persistent defaults incorporated.  As
;;; an interim stage, we want a fs with all defaults which has not yet
;;; been linked.  We also have the unifications formed from the
;;; orthography etc to unify in to the non-default fs.

(defun lex-expansion-error (string1 string2)
  (if *batch-mode* 
      (format t "~%~A" string2)
    (cerror string1 string2)))

(defun expand-psort-entry (entry &optional local-p interim-p)
  (let* ((*safe-not-to-copy-p* nil)
	 (orth (lex-or-psort-orth entry))
         (lex-id (lex-or-psort-id entry))
         (language (lex-or-psort-language entry))
         (fs (append (lex-or-psort-unifs entry)
		     (when (and orth *sense-unif-fn*)
		       (apply *sense-unif-fn* 
			      (list orth 
				    (format nil "~A" lex-id) language))))))
    (process-unif-list lex-id fs (lex-or-psort-def-unifs entry) entry
		       *description-persistence* nil 
		       local-p interim-p)))
         
(defun process-unif-list (lex-id indef-list default-specs entry persistence
			  &optional linking-p local-p interim-p)
  ;; linking-p is only true with the bc96 version of lexical rules
  ;; local-p and interim-p are called for display, to avoid storing
  ;; unneeded structure
  (let* ((fs (process-unifications indef-list))
	 (indef (if fs (create-wffs fs))))
    (if indef
        (let* ((default-fss
		   (for default-spec in default-specs
			collect
			(make-equivalent-persistence-defaults 
			 indef (car default-spec) (cdr default-spec) lex-id)))
               (local-tdfs (construct-tdfs indef default-fss t)))
          (if local-p 
              local-tdfs
            (let ((interim-fs local-tdfs))
              (setf (tdfs-tail interim-fs)
                (yadu-general-merge-tails
                 (tdfs-tail interim-fs)
                 (tdfs-tail (type-tdfs (get-type-entry (type-of-fs indef))))
                 indef))
              (if interim-p 
                  interim-fs
                (let ((incorp-fs 
                       (if persistence
                           (make-indefeasible interim-fs (list persistence))
                         interim-fs)))
		  ;; Cut off useless pointers, to help garbage collection
		  (compress-dag (tdfs-indef incorp-fs))
                  (setf (lex-or-psort-full-fs entry)
                    (if (and linking-p (fboundp 'link-lex-entry))
                        (funcall 'link-lex-entry incorp-fs)
                      incorp-fs)))))))
      (progn
	(if fs
	    (format t "~%Structure for ~A could not be made well formed" lex-id)
	  (format t "~%Structure for ~A could not be created" lex-id))
	nil))))

;; Never called
;;(defun lex-or-psort-interim-fs (entry)
;;  (expand-psort-entry entry nil t))

(defun lex-or-psort-local-fs (entry)
  (expand-psort-entry entry t nil))

;; Check to see if compiled files match originals

(defun up-to-date-p (in-files out-files)
  (when (every #'probe-file out-files)
    (let ((in-date (apply #'max (mapcar #'file-write-date in-files)))
	  (out-date (apply #'min (mapcar #'file-write-date out-files))))
      (> out-date in-date))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  General lexicon methods
;;;

(defmethod lookup-word :around ((lexicon lex-database) orth &key (cache t))
  (cond ((gethash orth (slot-value lexicon 'lexical-entries)))
	(t 
	 (let ((value (if (next-method-p) (call-next-method))))
	   ;; If main lexicon doesn't have an entry, try the other lexicons
	   (when (null value)
	     (setf value
	       (some #'(lambda (lex) (lookup-word lex orth :cache nil))
		     (extra-lexicons lexicon))))
	   (when (and cache value)
	     (setf (gethash orth (slot-value lexicon 'lexical-entries)) 
	       value))
	   value))))

(defmethod set-lexical-entry ((lexicon lex-database) orth id new-entry)
  (store-psort lexicon id new-entry orth)
  (with-slots (lexical-entries) lexicon
    (dolist (orth-el orth)
      (pushnew id (gethash (string-upcase orth-el) lexical-entries)))))

(defmethod store-temporary-psort ((lexicon lex-database) id 
				  (entry lex-or-psort))
  ;; add to hash but don't store in database
  (with-slots (temp-psorts psorts) lexicon
    (when (or (gethash id psorts)
	      (gethash id temp-psorts))
      (format t "~%Redefining ~A" id))
    (setf (gethash id temp-psorts) entry)))

(defmethod store-temporary-psort ((lexicon lex-database) id 
				  (entry t))
  (store-temporary-psort lexicon id 
			 (make-lex-or-psort 
			  :id id
			  :full-fs entry)))

(defmethod read-psort :around ((lexicon lex-database) id &key (cache t))
  (declare (ignore cache))
  (cond ((gethash id (slot-value lexicon 'temp-psorts)))
	((and (next-method-p) (call-next-method)))
	(t (some #'(lambda (lex) (read-psort lex id :cache nil))
		 (extra-lexicons lexicon)))))

(defmethod collect-expanded-lex-ids :around ((lexicon lex-database))
  (let ((ids nil))
    (maphash #'(lambda (id value)
                 (when (and value (lex-or-psort-full-fs value)
			    (lex-or-psort-orth value))
		   (push id ids)))
	     (slot-value lexicon 'temp-psorts))
    (nconc ids (call-next-method))))

(defmethod unexpand-psort :around ((lexicon lex-database) id)
  (with-slots (temp-psorts) lexicon
    (let ((entry (gethash id temp-psorts)))
      (if entry
	  (setf (lex-or-psort-full-fs entry) nil)
	(call-next-method)))))

(defmethod clear-lex :around ((lexicon lex-database) &optional no-delete)
  (when (fboundp 'clear-generator-lexicon)
    (funcall 'clear-generator-lexicon))
  (clrhash (slot-value lexicon 'lexical-entries))
  (clrhash (slot-value lexicon 'psorts))
  (clrhash (slot-value lexicon 'temp-psorts))
  (when (fboundp 'clear-lexicon-indices)
    (funcall 'clear-lexicon-indices))
  (call-next-method)
  (mapcar #'(lambda (lex) (clear-lex lex no-delete)) (extra-lexicons lexicon))
  (setf (extra-lexicons lexicon) nil) ; AAC - seems to be needed
  (unless no-delete
    (delete-temporary-lexicon-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface to a simple flat-file lexical cache
;;;

(defclass simple-lex-database (lex-database)
  ((psorts-stream :initform nil)))

#+:ignore
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
	  (apply #'max (for file in filenames
			    filter
			    (file-write-date file)))))
    (when (and cache-date last-file-date cache-index-date
	       (> cache-date last-file-date) 
	       (> cache-index-date last-file-date))
      (setf ok t)
      (format t "~%Reading in cached lexicon")
      (clear-lex lexicon t)
      (handler-case 
	  (read-psort-index-file)
	(error (condition)
	  (format t "~%Error: ~A" condition)
	  (delete-temporary-lexicon-files)
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

(defmethod clear-lex ((lexicon simple-lex-database) &optional no-delete)
  (declare (ignore no-delete))
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
                          (lex-or-psort-full-fs (cddr value)))
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

(defmethod read-psort ((lexicon simple-lex-database) id &key (cache t))
  (declare (ignore cache))
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

(defmethod collect-psort-ids ((lexicon simple-lex-database))
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
       (with-package (:lkb)
         (let ((successful-positioning 
                (file-position psorts-stream file-pointer)))
           (unless successful-positioning 
             (error "~%Can't retrieve entry for ~A" id))
           (read psorts-stream t)))
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

(defun delete-temporary-lexicon-files nil
  (when (and *psorts-temp-file*
	     (probe-file *psorts-temp-file*))
    (delete-file *psorts-temp-file*))
  (when (and *psorts-temp-index-file*
	     (probe-file *psorts-temp-index-file*))
    (delete-file *psorts-temp-index-file*)))


