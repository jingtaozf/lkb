;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :lkb)

;;; Input from lexical rule and construction files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in

; *ordered-rule-list* is in main/rules.lsp

(defun read-tdl-grammar-file-aux (file-name &optional ovwr)
  (if ovwr 
    (setf *grammar-rule-file-list* (list file-name))
    (pushnew file-name *grammar-rule-file-list* :test #'equal))
  (when ovwr 
    (setf *ordered-rule-list* nil)
    (clear-grammar))
   (read-tdl-lex-or-grammar-rule-file file-name nil))

; *ordered-lrule-list* is in main/rules.lsp

(defun read-tdl-lex-rule-file-aux (file-name &optional ovwr)
  (unless (member file-name *morphology-rule-file-list* :test #'equal)
    (if ovwr 
        (setf *lexical-rule-file-list* (list file-name))
      (pushnew file-name *lexical-rule-file-list* :test #'equal)))
  (when ovwr 
    (setf *ordered-lrule-list* nil))
  (when ovwr (clear-lex-rules) )    
  (read-tdl-lex-or-grammar-rule-file file-name t))   
      
(defun read-tdl-lex-or-grammar-rule-file (file-name lexical)
   (let ((*readtable*
            (make-tdl-break-table)))
      (with-open-file 
         (istream file-name :direction :input)
        (format t "~%Reading in ~Arules file ~A" 
                (if lexical "lexical " "")
                (pathname-name file-name))
        (read-tdl-rule-stream istream lexical)))
   (build-rule-filter))


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
               (t 
                (catch 'syntax-error
                  (read-tdl-rule-entry istream lexical)))))))


(defun read-tdl-rule-entry (istream lexical)
  (let* ((position (1+ (file-position istream)))
	 (id (lkb-read istream nil))
	 (next-char (peek-char t istream nil 'eof)))
    (unless (eql next-char #\:)
      (lkb-read-cerror 
       istream 
       "~%Incorrect syntax following rule name ~A" id)
      (ignore-rest-of-entry istream id))
     #+allegro (record-source id istream position)
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
         (lkb-read-cerror 
          istream 
          "~%Incorrect syntax following rule name ~A" id)
         (ignore-rest-of-entry istream id))
       (read-char istream)
       (let ((next-char3 (peek-char t istream nil 'eof)))
         (when (eql next-char3 #\%)
           (read-line istream))
         ; ignore Bernie morphology
         (multiple-value-bind (non-def def)
             (read-tdl-lex-avm-def istream id)
           (check-for #\. istream id)
           (add-grammar-rule id non-def def *description-persistence* lexical))))))
