;;; Copyright Ann Copestake 1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :cl-user)

;;; Input from lexical entry and psort/template files
;;; in TDL format

;;; This uses many of the functions from tdltypeinput.lsp
;;; Syntax for TDL lexical entries, rules etc is effectively 
;;; the same as for type entries, so the functions in this file
;;; are basically concerned with handling the structures appropriately
;;; rather than reading them in


; *category-display-templates* is in io-paths/lexinput

(defun read-tdl-lex-file-aux (file-name overwrite-p)
 ;  (reset-cached-lex-entries) ; in constraints.lsp  
   (when overwrite-p (clear-lex))
   (check-for-open-psorts-stream)
   (let ((*readtable*
            (make-tdl-break-table)))
      (with-open-file 
         (istream file-name :direction :input)
         (format t "~%Reading in lexical entry file")
         (read-tdl-lex-stream istream))
      (format t "~%Lexical entry file read"))
   (flush-psorts-stream-output))


(defun read-tdl-lex-stream (istream) 
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
               (t (read-tdl-lex-entry istream))))))


(defun read-tdl-lex-entry (istream)
;;; Lex-def -> Lexid Avm-def . 
;;; Lexid  -> identifier
;;; Avm-def -> := Conjunction (as in tdltypeinput.lsp)
  (let* ((position  (1+ (file-position istream)))
	 (name (lkb-read istream nil))
	 (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following lexicon name ~A" name))
     #+allegro (record-source name istream position)
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following lexicon name ~A" name))   
       (read-char istream)
       (multiple-value-bind
           (constraint default)
           (read-tdl-lex-avm-def istream name)
         (check-for #\. istream name)
         (push name *ordered-lex-list*)
         (add-lex-from-file nil name constraint default)))))



(defun read-tdl-lex-avm-def (istream name)
  ;;; Avm-def -> := Conjunction
  (clrhash *tdl-coreference-table*)     ; parameter defined in tdltypeinput
  (clrhash *tdl-default-coreference-table*)
  (let ((constraint nil)
        (def-alist nil))
      ;;; read-tdl-conjunction in tdltypeinput
    (for unif in (read-tdl-conjunction istream name nil nil)
         do
         (cond ((unification-p unif) (push unif constraint))
               ((consp unif)
                (let ((entry (assoc (car unif) def-alist)))
                  (if entry
                      (push (cadr unif) (cdr entry))
                    (push unif def-alist))))
               (t (error "~%Unexpected unif in ~A" name))))
    (dolist (coref (make-tdl-coreference-conditions 
                    *tdl-coreference-table* nil))
      (push coref constraint))
    (dolist (coref (make-tdl-coreference-conditions 
                    *tdl-default-coreference-table* t))
      (let ((entry (assoc (car coref) def-alist)))
        (if entry
            (push (cadr coref) (cdr entry))
          (push coref def-alist))))
    (values constraint def-alist)))


;;; Other varieties of files

(defun read-tdl-psort-file-aux (file-name &optional templates-p qc-p)
  (check-for-open-psorts-stream)  
  (when templates-p (setf *category-display-templates* nil))
;  (when qc-p (clear-quick-check-table))
   (let ((*readtable*
          (make-tdl-break-table)))
     (with-open-file 
         (istream file-name :direction :input)
       (format t "~%Reading in ~A file"
               (cond (templates-p "templates")
;;                      (qc-p "quick check")
                     (t "psort")))
         (read-tdl-psort-stream istream templates-p qc-p))
      (format t "~%~A entry file read" (cond (templates-p "Template")
;                                             (qc-p "Quick check")
                                             (t "Psort"))))
   (flush-psorts-stream-output)  
   (if templates-p (split-up-templates)))

(defun read-tdl-psort-stream (istream &optional templates-p qc-p) 
   (loop
      (let ((next-char (peek-char t istream nil 'eof)))
         (when (eql next-char 'eof) (return))
         (cond ((eql next-char #\;) 
                 (read-line istream))
               ; one line comments
               ((eql next-char #\:) 
                 (read-tdl-declaration istream))
               ; declarations like :begin :type
               ((eql next-char #\#) 
                (read-tdl-comment istream))
               (t (read-tdl-psort-entry istream templates-p qc-p))))))

(defun read-tdl-psort-entry (istream &optional templates-p qc-p)
  (declare (ignore qc-p))
   (let* ((name (lkb-read istream nil))
          (next-char (peek-char t istream nil 'eof)))
     (unless (eql next-char #\:)
       (error "~%Incorrect syntax following template name ~A" name))
     (read-char istream)
     (let ((next-char2 (peek-char t istream nil 'eof)))
       (unless (eql next-char2 #\=)
            (error "~%Incorrect syntax following template name ~A" name))   
       (read-char istream)
       (multiple-value-bind
           (constraint default)
           (read-tdl-lex-avm-def istream name)
         (check-for #\. istream name)
;;       (if qc-p 
;;           (add-qc-from-file name constraint) ; in check-unif.lsp      
         (progn 
           (add-psort-from-file name constraint default)
           (if templates-p (pushnew name *category-display-templates*)))))))

 



