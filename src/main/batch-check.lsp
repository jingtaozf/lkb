;;; Somewhat heterogeneous file containing functions
;;; for checking lexicon and indexing semantics

(defun batch-check-lexicon nil
  ; name is a misnomer - actually check all instances
   (setf *batch-mode* t)
   (for id in (collect-psort-ids)
      do
      (let* ((hash-table-entry (gethash id *psorts*))
            (file-pointer (car hash-table-entry)))
         (when (integerp file-pointer)
           (format t "~%Checking ~A" id)
            (let* 
               ((entry 
                     (cdr (read-psort-entry-from-file 
                           file-pointer id)))
                  (orth (lex-or-psort-orth entry))
                  (lex-id (lex-or-psort-id entry)))
               (expand-psort-entry entry)
               (let ((new-fs (lex-or-psort-full-fs entry)))
                  (unless new-fs
                     (format t "~%No feature structure for ~A" lex-id))))
;                  (if new-fs
;                     (check-fs-for-cycles (tdfs-indef new-fs) lex-id)
;                     (format t "~%No feature structure for ~A" lex-id))))
            (setf (cdr (gethash id *psorts*)) nil)))) ; clear structure
   (setf *batch-mode* nil))

(defun index-lexicon nil
  (mrs::clear-semantic-indices)
   (setf *batch-mode* t)
   (let ((ids nil))
     (maphash 
      #'(lambda (name val)
          (declare (ignore name))
          (for id in val
               do
               (pushnew id ids)))
      ; because of multiple lexical entries,
      ; an id may be indexed by multiple orthographies
      *lexical-entries*)
     (for id in ids
          do
          (let* ((hash-table-entry (gethash id *psorts*))
                 (file-pointer (car hash-table-entry)))
            (when (integerp file-pointer)
              (let* 
                  ((entry 
                    (cdr (read-psort-entry-from-file 
                          file-pointer id)))
                   (lex-id (lex-or-psort-id entry)))
                (expand-psort-entry entry)
                (let ((new-fs (lex-or-psort-full-fs entry)))
                  (if new-fs
                      (mrs::extract-lexical-relations entry)
                    (format t "~%No feature structure for ~A" lex-id))))
              (setf (cdr (gethash id *psorts*)) nil)))) ; clear structure
     (setf *batch-mode* nil)))



    