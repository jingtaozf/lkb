;;; Functions for checking instances are well formed

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
;                  (orth (lex-or-psort-orth entry))
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



    