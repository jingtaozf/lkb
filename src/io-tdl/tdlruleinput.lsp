;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

;;; Input from lexical rule and construction files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in

; *ordered-rule-list* is in io-paths/ruleinput

(defun read-tdl-grammar-file nil  
   (let ((ovwr
            (and (not (zerop (hash-table-count *rules*)))
               (lkb-y-or-n-p "Overwrite existing grammar?"))))
      (let ((file-name 
               (ask-user-for-existing-pathname "Grammar file?")))
         (when file-name (read-tdl-grammar-file-aux file-name ovwr)))))

(defun read-tdl-grammar-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
   (when ovwr
      (clear-grammar))
   (read-tdl-lex-or-grammar-rule-file file-name nil)
   (lkb-beep))

; *ordered-lrule-list* is in io-paths

(defun read-tdl-lex-rule-file nil  
   (let ((ovwr
      (and (not (zerop (hash-table-count *lexical-rules*)))
         (lkb-y-or-n-p "Overwrite existing rules?"))))
   (let ((file-name 
            (ask-user-for-existing-pathname "Lex rules file?")))
      (when file-name 
         (read-tdl-lex-rule-file-aux file-name ovwr)))))

(defun read-tdl-lex-rule-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
  (when (fboundp 'reset-cached-lexical-entries)
   (reset-cached-lexical-entries)) ; in constraints.lsp  
  (when ovwr (clear-lex-rules) )    
  (read-tdl-lex-or-grammar-rule-file file-name t)
  (lkb-beep))   
      
(defun read-tdl-lex-or-grammar-rule-file (file-name lexical)
   (let ((*readtable*
            (make-tdl-break-table)))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in ~Arules" (if lexical "lexical " ""))
         (read-tdl-rule-stream istream lexical))))


(defun read-tdl-rule-stream (istream lexical) 
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\:) 
                 (read-tdl-declaration istream))
               ; declarations like :begin :type
               ((eql next-char #\#) (read-tdl-comment istream))
               (t (read-tdl-rule-entry istream lexical))))))


(defun read-tdl-rule-entry (istream lexical)
   (let* ((id (lkb-read istream nil))
          (entry (make-rule :id id))
          (non-def nil)
          (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following rule name ~A" id))
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following rule name ~A" id))   
       (read-char istream)
       (setf non-def
             (read-tdl-lex-avm-def istream id))
       (check-for #\. istream id)
       (process-unif-list id non-def nil entry *rule-persistence*)
       (when (rule-full-fs entry)
         (if lexical
             (progn (push id *ordered-lrule-list*)
                    (add-lexical-rule id entry))
           (progn (push id *ordered-rule-list*)
                  (add-grammar-rule id entry)))))))
