;;; Copyright Ann Copestake 1991-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; April 1997 - modified for YADU
;;;            - output-lexicon etc removed
;;;            - get-psort-type removed (not called)

;;; Lexical entries and psort storage etc

;;; Lexical entries are indexed by orthography (string)
;;; They are also all potential psorts indexed
;;; by a combination of orthography
;;; plus sense identifier

(defvar *lexical-entries* (make-hash-table :test #'equal))

(defvar *psorts-stream* nil)

(defvar *psorts* (make-hash-table))

(defvar *language-lists* nil)

(defstruct (lex-or-psort) 
   orth
   infl-pos ; for a multi-word entry only - a number
            ; indicating the element that can be inflected
            ; or NIL - no inflection
   sense-id
   id ; for a lexical entry this is an
      ; atom formed by combining the strings
   (language *current-language*)
   unifs
   def-unifs
   local-fs
   mother-p ; set if the psort is specified as a parent
            ; cacheing may be sensitive to this value
   interim-fs ; all defaults still specified as defaults
              ; no linking 
   full-fs)

(defun check-for-open-psorts-stream nil
   (unless (and (streamp *psorts-stream*)
                (open-stream-p *psorts-stream*))
      (setf *psorts-stream*
         (open *psorts-temp-file* 
            :direction :io
            :if-exists :supersede
            :if-does-not-exist :create))
      (unless (and (input-stream-p *psorts-stream*)
            (output-stream-p *psorts-stream*))
         (error "~%Failed to open temporary file correctly" 
            *psorts-temp-file*))))

(defun close-temporary-lexicon-file nil
   (when (and (streamp *psorts-stream*)
              (open-stream-p *psorts-stream*))
      (close *psorts-stream*))
   (when (probe-file *psorts-temp-file*)
      (delete-file *psorts-temp-file*)))

(defun store-temporary-psort (id fs)
   (unless (gethash id *psorts*)
      (setf (gethash id *psorts*)
         (cons 'no-pointer 
            (make-lex-or-psort 
               :id id
               :full-fs fs)))))
   

(defun get-psort-entry (id &optional parent-p)
  (let ((entry (get-unexpanded-psort-entry id parent-p)))
               ;; if we haven't previously expanded then
               ;; destructively modify entry
    (if entry
      (cond ((eql (lex-or-psort-full-fs entry) :fail) nil)
            ((lex-or-psort-full-fs entry) entry)
            ((expand-psort-entry entry) entry)
            (t (setf (lex-or-psort-full-fs entry) :fail)
               nil)))))
                   
(defun get-unexpanded-psort-entry (id &optional parent-p)
  ;;; for multi words, where we don't want the full-fs
  ;;; to be created until we're sure we've got all the bits
     (let ((hash-table-entry 
            (gethash id *psorts*)))
      (if hash-table-entry
          (or (cdr hash-table-entry) ; cached
              (let* 
                  ((file-pointer (car hash-table-entry))
                   (file-entry   
                    (cdr (read-psort-entry-from-file 
                          file-pointer id))))
                (setf (gethash id *psorts*) 
                      (cons file-pointer file-entry))
                (when parent-p 
                  (setf (lex-or-psort-mother-p file-entry) t))
                file-entry)))))

(defun read-psort-entry-from-file (file-pointer id)
   #+:mcl(decf ee (CCL::TOTAL-BYTES-ALLOCATED))
   (prog1
    (let ((successful-positioning
            (file-position *psorts-stream* file-pointer)))
      (unless successful-positioning 
         (error "Can't retrieve entry for ~A~%" id))
      (read *psorts-stream* t))
    #+:mcl(incf ee (CCL::TOTAL-BYTES-ALLOCATED))))


(defun store-psort (id entry)
   ;; write new entry to the end of the file
   ;; update the hash table entry with the new file pointer
   (let ((current-file-end (file-length *psorts-stream*))
         (specified-entry (cons id entry)))
      (file-position *psorts-stream* current-file-end)
      (write specified-entry :stream *psorts-stream*)
      (setf (gethash id *psorts*)
         (cons current-file-end nil))))

(defun uncache-psort-entry (id)
   (let ((hash-table-entry 
            (gethash id *psorts*)))
      (when hash-table-entry
         (setf (cdr hash-table-entry) nil))))


(defun lexicon-exists nil
   (and (streamp *psorts-stream*)
        (open-stream-p *psorts-stream*)))

(defun clear-lex nil
  (when (fboundp 'reset-cached-lex-entries)
   (reset-cached-lex-entries)) ; in constraints.lsp
  (clrhash *lexical-entries*)
  (clrhash *psorts*)
  (when (fboundp 'clear-lexicon-indices)
   (clear-lexicon-indices))
  (setf *language-lists* nil)
  (close-temporary-lexicon-file))

(defun clear-expanded-lex nil
  (when (fboundp 'reset-cached-lex-entries)
   (reset-cached-lex-entries)) ; in constraints.lsp
  (maphash #'(lambda (id value)
               (declare (ignore id))
               (setf (cdr value) nil))
           *psorts*))
   
(defun clear-non-parents nil
   (maphash 
      #'(lambda (id value)
         (declare (ignore id))
         (when (integerp (car value))
            (unless 
               (and (lex-or-psort-p (cdr value))
                  (lex-or-psort-mother-p (cdr value)))
               (setf (cdr value) nil))))
      *psorts*))
         

(defun get-psort-value (psort) 
   (let ((psort-entry (get-psort-entry psort)))
      (if psort-entry 
         (lex-or-psort-full-fs psort-entry)
         (let ((gr-entry (get-grammar-rule-entry psort)))
            (if gr-entry
               (rule-full-fs gr-entry)
               (let ((lr-entry (get-lex-rule-entry psort)))
                  (if lr-entry (rule-full-fs lr-entry)
                     (lex-expansion-error "Return nil" 
                        (format nil
                           "~A is not a valid psort" psort)))))))))


(defun get-interim-lex-value (psort) 
   (let ((psort-entry (get-psort-entry psort)))
      (if psort-entry 
         (lex-or-psort-interim-fs psort-entry)
         (lex-expansion-error "Return nil" 
            (format nil
               "~A is not a valid psort" psort)))))

(defun collect-defined-word-strings nil
   (let ((words nil))
      (maphash #'(lambda (k v)
                   (declare (ignore v))
                   (push k words))
         *lexical-entries*)
      words))

(defun get-lex-entry (orth)
   (for psort in (gethash orth *lexical-entries*)
      filter
      (get-psort-entry psort)))

(defun get-unexpanded-lex-entry (orth)
   (for psort in (gethash orth *lexical-entries*)
      filter
      (get-unexpanded-psort-entry psort)))


(defun set-lexical-entry (orth id new-entry)
   (store-psort id new-entry)
   (let ((language-list (assoc *current-language* *language-lists*)))
      (if language-list (pushnew id (cdr language-list))
         (push (list *current-language*) *language-lists*)))
   (for orth-el in orth
        do
        (pushnew id (gethash (string-upcase orth-el) *lexical-entries*))))

(defun add-lex-from-file (orth sense-id fs-or-type defs)
   (let ((lex-id (if orth (make-lex-id orth sense-id) sense-id))
         (orth-string (if (and orth *sense-unif-fn*)
                          (format nil "~A" orth) 
                          (extract-orth-from-unifs fs-or-type))))
     ; adapted for the case where the orthography is only specified in the FS
     ; extract-orth-from-unifs must be defined on a per-grammar basis
      (set-lexical-entry orth-string lex-id 
         (make-lex-or-psort :orth orth-string
            :sense-id sense-id :id lex-id
            :unifs fs-or-type :def-unifs defs))))

(defun extract-orth-from-unifs (unifs)
  ;;; returns a list of strings
  (let ((orth-strings nil)
        (current-orth-path *orth-path*)
        (matching-unifs 
         (for unif in unifs
              filter
              (if (unification-p unif)
                (let ((unif-lhs (unification-lhs unif)))
                  (if (and (path-p unif-lhs)
                           (path-matches (path-typed-feature-list unif-lhs) *orth-path*))
                    unif))))))
    (loop
      (let ((exact-match
             (for unif in matching-unifs
                  keep-first
                  (and
                   (path-matches-exactly (path-typed-feature-list (unification-lhs unif))
                                         (append current-orth-path *list-head*))
                   (u-value-p (unification-rhs unif))))))
        (unless exact-match (return))
        (push (car (u-value-types (unification-rhs exact-match)))
                orth-strings)
        (setf current-orth-path (append current-orth-path *list-tail*))))
    (nreverse orth-strings)))

(defun path-matches (tfplist flist)
  (cond ((null flist) t)
        ((null tfplist) nil)
        ((eql (type-feature-pair-feature (car tfplist)) (car flist))
         (path-matches (cdr tfplist) (cdr flist)))
        (t nil)))

(defun path-matches-exactly (tfplist flist)
  (cond ((and (null flist) (null tfplist)) t)
        ((or (null flist) (null tfplist)) nil)
        ((eql (type-feature-pair-feature (car tfplist)) (car flist))
         (path-matches-exactly (cdr tfplist) (cdr flist)))
        (t nil)))
  

(defun extract-orth-from-fs (tdfs)
  ;;; returns a single string, possibly concatenating the words
  (let ((fs (tdfs-indef tdfs))
        (current-orth-path *orth-path*)
        (orth-strings nil))
    (loop 
      (let ((current-orth (get-value-at-end-of fs (append current-orth-path *list-head*))))
        (when (or (null current-orth) (eql current-orth 'no-way-through))
          (return))
        (setf current-orth-path (append current-orth-path *list-tail*))
        (push (car current-orth) orth-strings)
        (push " " orth-strings)))
    (apply #'concatenate 'string (nreverse (cdr orth-strings)))))



(defun make-lex-id (orth sense-id)
   (intern (format nil "~A_~A" orth sense-id)))
 
(defun add-psort-from-file (id fs-or-type defs)
   (store-psort id 
      (make-lex-or-psort 
         :id id
         :unifs fs-or-type
         :def-unifs defs)))

;;; When expanding a lexical entry we want to eventually produce a tdfs
;;; which then has the non-persistent defaults incorporated
;;; As an interim stage, we want a fs with all defaults 
;;; which has not yet been linked
;;; We also have the unifications formed from the orthography etc
;;; to unify in to the non-default fs


(defun lex-expansion-error (string1 string2)
   (if *batch-mode* 
      (format t "~%~A" string2)
      (cerror string1 string2)))

(defun expand-psort-entry (entry)
   (let* ((orth (lex-or-psort-orth entry))
         (lex-id (lex-or-psort-id entry))
         (language (lex-or-psort-language entry))
         (fs (append (lex-or-psort-unifs entry)
               (if (and orth *sense-unif-fn*)
                  (apply *sense-unif-fn* 
                         (list orth 
                              (format nil "~A" lex-id) language))))))
      (process-unif-list lex-id fs (lex-or-psort-def-unifs entry) entry
         *lexical-persistence* *bc96lrules*)))

;;; *bclrules* is t if the style of lexical rules
;;; and linking adopted in Briscoe and Copestake 1996 
;;; is being used
         

(defun process-unif-list (lex-id indef-list default-specs entry persistence
      &optional linking-p)
  ; linking-p is only true with the bc96 version of lexical rules
   (let* ((fs (process-unifications indef-list))
          (indef (if fs (create-wffs fs))))
      (if indef
        (let* ((default-fss
                 (for default-spec in default-specs
                      collect
                      (make-equivalent-persistence-defaults 
                       indef (car default-spec) (cdr default-spec) lex-id)))
               (local-tdfs (construct-tdfs indef default-fss t))
               (interim-fs
                (if default-fss
                  (yadu
                   local-tdfs
                   (type-tdfs (get-type-entry (type-of-fs indef))))
                  local-tdfs))
               (incorp-fs 
                (if (and default-fss persistence) 
                  (make-indefeasible interim-fs (list persistence))
                  interim-fs)))
          (setf (lex-or-psort-local-fs entry) fs)
          (setf (lex-or-psort-interim-fs entry) interim-fs)
          (setf (lex-or-psort-full-fs entry)
                (if linking-p
                  (link-lex-entry incorp-fs)
                  incorp-fs)))
         (progn
            (if fs
               (format t "~%Structure for ~A could not be made well formed" lex-id)
               (format t "~%Structure for ~A could not be created" lex-id))
            nil))))