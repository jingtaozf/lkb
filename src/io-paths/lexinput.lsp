;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;;
;;; Drastically simplified for YADU
;;; old laurel input stuff is now in laurel/lrlinput
;;; facilities for templates for trees and quick-check need 
;;; to be added back in


;;; Input from lexical entry and psort files

;;; This uses many of the functions from typeinput.lsp

;;; Non-lexentry psorts are essentially identical except
;;; that there is a simple id rather than a string plus 
;;; sense id

;;; Lexical entries have the following syntax (in BNF):
;;; 
;;; Lexentry -> LexID Path_spec+ Default*
;;;             Comment
;;; Comment -> ; anything eol
;;; LexID -> orth sense-id
;;; Path_spec and Default are defined in the type file


(defparameter *category-display-templates* nil
  "used in parseout.lsp")

(defparameter *ordered-lex-list* nil)

(defun read-lex-file nil
  (setf *ordered-lex-list* nil)
   (let* ((file-name 
            (ask-user-for-existing-pathname "Entry file?"))
         (*current-language*
            (apply #'ask-user-for-multiple-choice "Language" 
               (cons *current-language*
                  (remove *current-language*
                     *possible-languages*)))))
      (when file-name
         (read-lex-file-aux file-name 
            (if (lexicon-exists) 
               (y-or-n-p "Overwrite existing lexicon?"))))))

(defun read-lex-file-aux (file-name overwrite-p)
 ;  (reset-cached-lex-entries) ; in constraints.lsp  
   (when overwrite-p (clear-lex))
   (check-for-open-psorts-stream)
   (let ((*readtable*
            (define-break-characters 
               '(#\; #\< #\> #\= #\: #\. #\/))))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in lexical entry file")
         (loop
            (let ((next-char (peek-char t istream nil 'eof)))
               (when (eql next-char 'eof) (return))
               (if (eql next-char #\;) 
                  (read-line istream)
                  (read-lex-entry istream)))))
      (lkb-beep)
      (format t "~%Lexical entry file read")))

(defun read-lex-entry (istream)
   (let* ((orth (read istream))
         (id (read istream)))
      (push (make-lex-id orth id) *ordered-lex-list*)
      (multiple-value-bind 
         (non-def defs)
         (read-psort-unifications id istream)
         (add-lex-from-file orth id non-def defs))))

(defun read-psort-file nil  
   (check-for-open-psorts-stream)
   (let ((file-name 
            (ask-user-for-existing-pathname "Psorts file?")))
      (when file-name
         (read-psort-file-aux file-name))))

#|
(defun read-qc-file nil  
   (let ((file-name 
            (ask-user-for-existing-pathname "Quick check file?")))
      (when file-name
         (read-psort-file-aux file-name nil t))))
|#

(defun read-psort-file-aux (file-name &optional templates-p qc-p)
  (when templates-p (setf *category-display-templates* nil))
;  (when qc-p (clear-quick-check-table))
   (let ((*readtable*
            (define-break-characters 
               '(#\; #\< #\> #\= #\: #\. #\/))))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in ~A file"
                (cond (templates-p "templates")
;                      (qc-p "quick check")
                      (t "psort")))
         (loop
            (let ((next-char (peek-char t istream nil 'eof)))
               (when (eql next-char 'eof) (return))
               (if (eql next-char #\;) 
                  (read-line istream)
                  (read-psort-entry istream templates-p qc-p)))))
      (lkb-beep)
      (format t "~%~A entry file read" (cond (templates-p "Template")
;                                             (qc-p "Quick check")
                                             (t "Psort")))))

(defun read-psort-entry (istream &optional templates-p qc-p)
   (let ((id (read istream)))
      (multiple-value-bind 
         (non-def defs)
         (read-psort-unifications id istream)
;        (if qc-p 
;          (add-qc-from-file id non-def) ; in check-unif.lsp
;                                        ; defaults would be pointless here
          (progn 
            (add-psort-from-file id non-def defs)            
            (if templates-p (pushnew id *category-display-templates*))))))
 

(defun read-psort-unifications (orth istream &optional type)
   (let ((next-char (peek-char t istream nil 'eof))
         (type-unifs (if type (list 
                        (make-unification :lhs 
                           (make-path :typed-feature-list nil)
                           :rhs (make-u-value :types (list type)))))))
      (cond 
         ((eql next-char 'eof) (error "~%Unexpected end of file"))
         ((char= next-char #\.)
          (read-char istream)
          (if type
             (values type-unifs nil)
            (progn (cerror "~%Ignore entry"
               "~%No lex specification for ~A" 
               orth) nil))) 
          ((or (char= next-char #\;) (char= next-char #\%))           
           (loop (read-line istream)
                 (let ((next-char2 (peek-char t istream nil 'eof)))
                   (unless 
                    (or (char= next-char2 #\;) (char= next-char2 #\%))
                    (return nil))))
           (read-psort-unifications orth istream type))
         ((char= next-char #\/)
            (let ((def-fss
                     (read-defaults istream orth nil)))
                 (check-for #\. istream orth)
                 (values type-unifs def-fss)))
         ((char= next-char #\<)
          (let* ((fs (read-path-spec-list istream orth))
                 (next-char (peek-char t istream nil 'eof)))
            (if (char= next-char #\/)
                (let ((def-fss
                        (read-defaults istream orth nil)))
                  (check-for #\. istream orth)
                  (values (append type-unifs fs) def-fss))
              (progn
                (check-for #\. istream orth)
                (values (append type-unifs fs) nil)))))
         ((not type) ; may have a type by itself 
          (let ((itype (read istream)))
            (if (and itype (is-valid-type itype))
                (read-psort-unifications orth istream itype))))
         (t
            (error "~%Badly formed lexical specification for ~A" orth)))))





(defun output-unif (unif ostream active-p)
   (cond 
      ((unification-p unif)
         (output-unif-lhs ostream unif active-p)
         (output-path ostream (basic-unification-rhs unif) active-p))
      ((path-p unif)
        (output-path ostream unif active-p)) 
      (t (format ostream "~A" unif))))

(defun output-unif-lhs (ostream unif &optional active-p)
  (declare (ignore active-p))
   (format ostream "~%")
   (cond 
      ((unification-p unif) 
         (output-path ostream (basic-unification-lhs unif))
         (format ostream "    =     "))
      (t (error "Unrecognised item in lexical specification ~A" 
            unif))))
         

(defun display-fs-spec (structure ostream &optional active-p)
   (cond 
      ((fs-and-path-p structure) 
         (display-fs-spec (fs-and-path-fs structure) 
            ostream active-p)
         (output-path ostream (fs-and-path-path structure) active-p))
      (t (error "~%Unknown thing in unification specification ~A"
            structure))))  

            
(defun output-path (ostream path &optional active-p)
   (if (path-p path)
      (let ((ordered-list (path-typed-feature-list path)))
         (format ostream "<")
         (when ordered-list
            (output-type-feature-pair ostream (car ordered-list) active-p)
            (for tfp in (cdr ordered-list)
               do
               (format ostream ":" )
               (output-type-feature-pair ostream tfp active-p)))
         (format ostream ">"))
      (format ostream (if active-p "~/FB/~(~A~)~/FP/   " "~S") 
         (if (> (length (u-value-types path)) 1)
            (u-value-types path)
            (car (u-value-types path))))))

      
(defun output-type-feature-pair (ostream tfp &optional active-p)
   (if active-p
      (format ostream " ~A " 
         (type-feature-pair-feature tfp))
      (format ostream "~( ~A ~A ~)" 
         (type-feature-pair-type tfp)
         (type-feature-pair-feature tfp))))



