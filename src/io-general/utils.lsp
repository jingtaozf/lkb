;;; Copyright Ann Copestake 1991-1998. All Rights Reserved.
;;; No use or redistribution without permission.

;;; Functions moved from io-paths/typeinput.lsp

(in-package :cl-user)

(defun check-for (character istream name)
   (let ((next-char (peek-char t istream nil 'eof)))
     (if (char= next-char character)
         (read-char istream)
       (progn 
         (setf *syntax-error* t)
         (format t 
                 "~%Syntax error: ~A expected and not found in ~A at position ~A" 
                 character name (file-position istream))
         (format t 
                 "~%Inserting ~A" character)
         character))))


(defun lkb-read-cerror (istream string &rest rest)
  (setf *syntax-error* t)
  (format t 
         (concatenate 'string "~%Syntax error at position ~A: " string)  
         (file-position istream) 
         rest))

(defun ignore-rest-of-entry (istream name)
  ;;; called after a continuable error
  ;;; looks for . followed by a newline etc, 
  ;;; possibly with intervening blank space between the . and newline
  (format t "~%Ignoring (part of) entry for ~A" name)
  (loop 
    (let ((next-char
           (peek-char #\. istream nil 'eof)))
      (when (eql next-char 'eof) (return))
      (read-char istream)
      (when
          (loop
            (let ((possible-newline
                   (read-char istream nil 'eof)))
              (when (member possible-newline 
                            '(eof #'\page #\newline #\linefeed #\return))
                (return t))
              (when (not (whitespacep possible-newline))
                (return nil))))
        (return))))
  (throw 'syntax-error nil))
  

(defun define-break-characters (char-list)
   (let ((temporary-readtable (copy-readtable *readtable*)))
      (dolist (break-char char-list)
         (set-macro-character break-char
            #'(lambda (stream x) (declare (ignore stream)) x)
            nil
            temporary-readtable))
      temporary-readtable))

(defun lkb-read (istream &optional strings-allowed)
  (let ((item (read istream)))
    (if (stringp item)
      (if strings-allowed 
        item 
        (error "~%~S should not be a string" item))
      (if (symbolp item)
        item
        (convert-to-lkb-symbol item)))))

(defun convert-to-lkb-symbol (item)
  (intern (format nil "~S" item)))

(defun set-up-type-interactions nil
   (enable-type-interactions)
;   (initialise-type-menus)
   (close-existing-type-hierarchy-trees)
   (when *display-type-hierarchy-on-load*
     (create-type-hierarchy-tree)))

(defun write-time-readably (&optional stream)
  (multiple-value-bind
      (sec min hour date month year)
      (get-decoded-time)
    (format (or stream t) "~%~A:~A:~A ~A ~A ~A~%" hour min sec date 
            (ecase month
                (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") 
                (6 "Jun") (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct")
                (11 "Nov") (12 "Dec"))
            year)))

;;; moved from toplevel.lsp because they are needed for the tty version

(defun get-fs-given-id (fs-id)
  ;;; this accepts type names and rule names as well as lexical ids
  (let ((result (get-tdfs-given-id fs-id)))
    (if result
      (if (tdfs-p result)
        (tdfs-indef result)
        result))))


(defun get-tdfs-given-id (fs-id)
  ;;; this accepts type names and rule names as well as lexical ids
  (let ((lex-entry (get-psort-entry fs-id)))
    (if lex-entry (lex-or-psort-full-fs lex-entry)
      (let ((rule-entry (get-grammar-rule-entry fs-id)))
        (if rule-entry (rule-full-fs rule-entry)
          (let ((lex-rule-entry (get-lex-rule-entry fs-id)))
            (if lex-rule-entry (rule-full-fs lex-rule-entry)
              (progn (eval-possible-leaf-type *leaf-types* fs-id)
                     (let ((type (get-type-entry fs-id)))
                       (if type (tdfs-of fs-id)))))))))))

(defun split-into-words (sentence-string)
  ; split-into-words is used in various places 
  ; so shouldn't be redefined for more complete scanner
   (let ((current-word nil)
         (current-sentence nil))
      (for character in (coerce sentence-string 'list)
         do
         (cond ((char= character #\Space) 
                (when current-word 
                  (push (coerce (nreverse current-word) 'string)
                        current-sentence)
                  (setf current-word nil)))
               (t (push character current-word))))
      (push (coerce (nreverse current-word) 'string) current-sentence)
      (nreverse current-sentence)))

(defun do-parse-tty (sentence)
  (when sentence
    (close-existing-chart-windows)
    ;; this is sometimes called when not in tty mode
    ;; so make sure we remove unwanted charts
    (parse (split-into-words 
            (preprocess-sentence-string 
             (string-trim '(#\space #\tab #\newline) sentence))))))


;;; following is defined in MCL
#-:mcl
(defun whitespacep (char) 
  (member char '(#\space #\tab #\newline #\page #\return #\linefeed)))

;;; Function below is to replace the use of alphanumericp
;;; in the preprocess-sentence-string fn
;;; alphanumericp is defined to return nil on extended characters
;;; in ACL (at least).  This is a pain, because it means there's
;;; no reliable way of stripping punctuation.  For now, specify
;;; explicitly what is to be stripped.

(defun alphanumeric-or-extended-p (char)
  (and (graphic-char-p char)
       (not (member char '(#\space #\! #\" #\# #\$ #\% #\& #\' #\(
                           #\) #\* #\+ #\, #\- #\. #\/ #\: #\;
                           #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
                           #\_ #\` #\{ #\| #\} #\~)))))

;;;  Loading and reloading - called by the tty version as well
;;; as the menu driven version

;;; (defparameter *current-grammar-load-file* nil)
;;; defined in globals

(defun read-script-file-aux (file-name)
  (when file-name
    (let ( 
          #+allegro (excl:*redefinition-warnings* nil)
          #+mcl (*warn-if-redefine* nil)
          #+mcl (ccl::*suppress-compiler-warnings* t)
          (*syntax-error* nil))
    (setf *current-grammar-load-file* file-name)
    (clear-almost-everything)
       (let ((*package* (find-package "CL-USER")))
          (load file-name))
    (lkb-beep)
    (if *syntax-error*
        (format t "~%WARNING: syntax error(s) - check messages")
      (format t "~%Grammar input complete~%")))))



(defun clear-almost-everything nil
    (setf *syntax-error* nil)
    (clear-type-load-files)
    (clear-lex-load-files)
    (clear-rule-load-files)
    (clear-lex *lexicon* t)                ;; doesn't delete temporary files
    (reset-morph-var)
    (clear-grammar)              ;; should clear everything that might not be
    (clear-lex-rules)            ;; overridden, this should do for now    
    (setf  *check-paths* nil))

(defun reload-script-file nil
  (if (and *current-grammar-load-file* 
           (probe-file *current-grammar-load-file*))
      (read-script-file-aux *current-grammar-load-file*)
    (progn
      (if *current-grammar-load-file*
        (format t  "~%Error - existing script file ~A cannot be found"
                *current-grammar-load-file*)
        (format t  "~%Error - no existing script file")) 
     #-:tty(format t "~%Use Load Complete Grammar instead")
     #+:tty(format t "~%Use (read-script-file-aux file-name) instead"))))


;;; Utilities to simplify script file

(defun this-directory nil
  (make-pathname 
   :device (pathname-device *load-truename*)
   :directory (pathname-directory *load-truename*)))

(defun parent-directory nil
  (make-pathname 
   :device (pathname-device *load-truename*)
   :directory (butlast (pathname-directory *load-truename*))))

(defun lkb-pathname (directory name)
  (merge-pathnames
   (make-pathname :name name)
   directory))

(defun lkb-load-lisp (directory name &optional optional)
  (let ((file (merge-pathnames (make-pathname :name name) 
                               directory)))
    (if (probe-file file)
        (progn 
          #-allegro (load file)
  ;;; above also for ACL if no compiler, but don't know how to
  ;;; specify this
          #+allegro (load
		     (handler-bind ((excl:compiler-no-in-package-warning
				     #'(lambda (c)
					 (declare (ignore c))
					 (muffle-warning))))
		       (if (member :compiler *features*)
			   (compile-file file :verbose nil :print nil)
			 file))))
      (unless optional
        (cerror "Continue loading script" "~%File ~A not found" file)))))

(defun load-lkb-preferences (directory name)
  (let ((file (merge-pathnames (make-pathname :name name) 
                               directory)))
    (when (probe-file file)
      (load file))
    (setf *user-params-file* file)))


(defun load-irregular-spellings (pathname)
  (read-irreg-form-string 
   (with-open-file (istream pathname
                    :direction :input)
     (read istream))))

;;; utility for reloading

(defun check-load-names (file-list &optional filetype)
  (let ((ok t))
    (if file-list
      (for file in file-list
           do
           (unless
            (probe-file file)
             (format t "~%Error - file ~A does not exist" file)
             (setf ok nil)))
      (progn
        (format t "~%Error - no ~A files found" 
                (if filetype (string filetype) ""))
        (setf ok nil)))
      ok))

  
