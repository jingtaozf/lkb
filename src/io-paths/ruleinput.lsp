;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; April 1997 
;;; new file split off from old rules.lsp, in order to 
;;; keep input functions separate

(defvar *lexical-rule-file-list* nil)
(defvar *morphology-rule-file-list* nil)
(defvar *grammar-rule-file-list* nil)

(defun clear-rule-load-files nil
  (setf *lexical-rule-file-list* nil)
  (setf *morphology-rule-file-list* nil)
  (setf *grammar-rule-file-list* nil))

(defun read-grammar-file nil  
   (let ((ovwr
            (and (not (zerop (hash-table-count *rules*)))
               (lkb-y-or-n-p "Overwrite existing grammar?"))))
      (let ((file-name 
               (ask-user-for-existing-pathname "Grammar file?")))
         (when file-name 
           (if (eql *lkb-system-version* :page)
               (read-tdl-grammar-file-aux file-name ovwr)
             (read-grammar-file-aux file-name ovwr))))))

(defun read-grammar-file-aux (file-name &optional ovwr)
  (if ovwr 
    (setf *grammar-rule-file-list* (list file-name))
    (pushnew file-name *grammar-rule-file-list* :test #'equal))
  (when ovwr 
    (setf *ordered-rule-list* nil)
    (clear-grammar))
   (read-lex-or-grammar-rule-file file-name nil)
   (format t "~%Grammar rule file read"))

(defun reload-grammar-rules nil
  (setf *syntax-error* nil)
  (when (check-load-names *grammar-rule-file-list* "grammar rules")
    (let ((ovwr t))
      (loop for grule-file in (reverse *grammar-rule-file-list*)
           do
           (if (eql *lkb-system-version* :page)
               (read-tdl-grammar-file-aux grule-file ovwr)
             (read-grammar-file-aux grule-file ovwr))
           (setf ovwr nil)))
    (format t "~%Reload complete")))
         
(defun read-lex-rule-file nil  
   (let ((ovwr
      (and (not (zerop (hash-table-count *lexical-rules*)))
         (lkb-y-or-n-p "Overwrite existing rules?"))))
   (let ((file-name 
            (ask-user-for-existing-pathname "Lex rules file?")))
      (when file-name
        (if (eql *lkb-system-version* :page)
            (read-tdl-lex-rule-file-aux file-name ovwr)
          (read-lex-rule-file-aux file-name ovwr))))))

(defun reload-lexical-rules nil
  ;;; reload both lexical and morphological files
  ;;; at once, since can't clear them independently
  (setf *syntax-error* nil)
  (if (or *lexical-rule-file-list* *morphology-rule-file-list*)
    (when (and
             (or (null *lexical-rule-file-list*)
                 (check-load-names *lexical-rule-file-list* "lexical rules"))
             (or (null *morphology-rule-file-list*)
                 (check-load-names *morphology-rule-file-list* "morphology rules")))
      (let ((ovwr t) (movwr t))
        (loop for lrule-file in *lexical-rule-file-list*
             do
             (if (eql *lkb-system-version* :page)
               (read-tdl-lex-rule-file-aux lrule-file ovwr)
               (read-lex-rule-file-aux lrule-file ovwr))
             (setf ovwr nil))
        (loop for mrule-file in *morphology-rule-file-list*
             do
             (read-morph-file-aux mrule-file ovwr movwr)
             (setf movwr nil)))
      (format t "~%Reload complete"))
    (format t "~%No lexical or morphological files loaded")))
         

(defun read-lex-rule-file-aux (file-name &optional ovwr)
  (unless (member file-name *morphology-rule-file-list* :test #'equal)
    (if ovwr 
        (setf *lexical-rule-file-list* (list file-name))
      (pushnew file-name *lexical-rule-file-list* :test #'equal)))
  (when ovwr (setf *ordered-lrule-list* nil)
	(setf *ordered-sprule-list* nil))
  (when ovwr (clear-lex-rules) )    
  (read-lex-or-grammar-rule-file file-name t)
  (format t "~%Lexical rule file read"))   

(defun read-morph-file nil 
    (let* ((ovwr (lkb-y-or-n-p "Overwrite any existing lexical rules?"))
          (filename (ask-user-for-existing-pathname "Morphological rules")))
      (when filename
        (read-morph-file-aux filename ovwr))))

(defun read-morph-file-aux (filename &optional ovwr movwr)
  (if ovwr 
    (setf *morphology-rule-file-list* (list filename))
    (pushnew filename *morphology-rule-file-list* :test #'equal))
  (when movwr (reset-morph-var))
  (if (eql *lkb-system-version* :page)
    (read-tdl-lex-rule-file-aux filename ovwr)
    (read-lex-rule-file-aux filename ovwr)))
      
(defun read-lex-or-grammar-rule-file (file-name lexical)
   (let ((*readtable*
            (define-break-characters 
               '(#\% #\; #\< #\> #\= #\: #\.))))
      (with-open-file 
         (istream file-name :direction :input)
        (format t "~%Reading in ~Arules file ~A" 
                (if lexical "lexical " "")
                (pathname-name file-name))
         (loop
            (let ((next-char (peek-char t istream nil 'eof)))
               (when (eql next-char 'eof) (return))
               (if (or (eql next-char #\;) (eql next-char #\%)) 
                  (read-line istream)
                  (read-rule-entry istream lexical)))))))

(defun read-rule-entry (istream lexical)
   (let* ((id (lkb-read istream nil)))
         (multiple-value-bind
            (non-def def)
             (read-psort-unifications id istream)
           ;; changed so all the work is done by functions in the
           ;; rules file
           (add-grammar-rule id non-def def
                             *description-persistence* lexical))))


