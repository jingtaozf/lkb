;;; Copyright Ann Copestake and Bernard Jones 1992-1997. 
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; April 1997 
;;; new file split off from old rules.lsp, in order to 
;;; keep input functions separate

(defparameter *ordered-rule-list* nil)

(defun read-grammar-file nil  
   (let ((ovwr
            (and (not (zerop (hash-table-count *rules*)))
               (lkb-y-or-n-p "Overwrite existing grammar?"))))
      (let ((file-name 
               (ask-user-for-existing-pathname "Grammar file?")))
         (when file-name (read-grammar-file-aux file-name ovwr)))))

(defun read-grammar-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
;  (clear-cached-edges)
   (when ovwr
      (clear-grammar))
   (read-lex-or-grammar-rule-file file-name nil)
   (lkb-beep))

(defparameter *ordered-lrule-list* nil)

(defun read-lex-rule-file nil  
   (let ((ovwr
      (and (not (zerop (hash-table-count *lexical-rules*)))
         (lkb-y-or-n-p "Overwrite existing rules?"))))
   (let ((file-name 
            (ask-user-for-existing-pathname "Lex rules file?")))
      (when file-name 
         (read-lex-rule-file-aux file-name ovwr)))))

(defun read-lex-rule-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
  (when (fboundp 'reset-cached-lexical-entries)
   (reset-cached-lexical-entries)) ; in constraints.lsp  
  (when ovwr (clear-lex-rules) )    
  (read-lex-or-grammar-rule-file file-name t)
  (lkb-beep))   
      
(defun read-lex-or-grammar-rule-file (file-name lexical)
   (let ((*readtable*
            (define-break-characters 
               '(#\% #\; #\< #\> #\= #\: #\.))))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in ~Arules" (if lexical "lexical " ""))
         (loop
            (let ((next-char (peek-char t istream nil 'eof)))
               (when (eql next-char 'eof) (return))
               (if (or (eql next-char #\;) (eql next-char #\%)) 
                  (read-line istream)
                  (read-rule-entry istream lexical)))))))

(defun read-rule-entry (istream lexical)
   (let* ((id (lkb-read istream nil))
         (entry (make-rule :id id)))
         (multiple-value-bind
            (non-def def)
            (read-psort-unifications id istream)
      (process-unif-list id non-def def entry *rule-persistence*)
      (when (rule-full-fs entry)
         (if lexical
           (progn (push id *ordered-lrule-list*)
            (add-lexical-rule id entry))
           (progn (push id *ordered-rule-list*)
            (add-grammar-rule id entry)))))))


