;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :cl-user)

;;; Input from lexical rule and construction files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in

; *ordered-rule-list* is in io-paths/ruleinput

(defun read-tdl-grammar-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
   (when ovwr
      (clear-grammar))
   (read-tdl-lex-or-grammar-rule-file file-name nil)
   (format t "~%Grammar rule file read"))

; *ordered-lrule-list* is in io-paths

(defun read-tdl-lex-rule-file-aux (file-name ovwr)
  (setf *ordered-rule-list* nil)
  (when (fboundp 'reset-cached-lex-entries)
   (funcall 'reset-cached-lex-entries)) ; in constraints.lsp  
  (when ovwr (clear-lex-rules) )    
  (read-tdl-lex-or-grammar-rule-file file-name t)
  (format t "~%Lexical rule file read"))   
      
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
               ((eql next-char #\%) 
                 (read-line istream))
               ; Bernie morphology
               ((eql next-char #\:) 
                 (read-tdl-declaration istream))
               ; declarations like :begin :type
               ((eql next-char #\#) (read-tdl-comment istream))
               (t (read-tdl-rule-entry istream lexical))))))


(defun read-tdl-rule-entry (istream lexical)
  (let* ((position (1+ (file-position istream)))
	 (id (lkb-read istream nil))
	 (entry (make-rule :id id))
	 (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following rule name ~A" id))
     #+allegro (record-source id istream position)
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following rule name ~A" id))   
       (read-char istream)
       (let ((next-char3 (peek-char t istream nil 'eof)))
         (when (eql next-char3 #\%)
           (read-line istream))
         ; ignore Bernie morphology
         (multiple-value-bind (non-def def)
             (read-tdl-lex-avm-def istream id)
           (check-for #\. istream id)
           (process-unif-list id non-def def entry *rule-persistence*)
           (when (rule-full-fs entry)
             (if lexical
                 (progn (pushnew id *ordered-lrule-list*)
                        (add-lexical-rule id entry))
               (progn (pushnew id *ordered-rule-list*)
                      (add-grammar-rule id entry)))))))))
