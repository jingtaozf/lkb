;;; Copyright Ann Copestake 1991-1998. All Rights Reserved.
;;; No use or redistribution without permission.

;;; Functions moved from io-paths/typeinput.lsp

(in-package :cl-user)

(defun check-for (character istream name)
   (let ((next-char (peek-char t istream nil 'eof)))
      (unless (char= next-char character)
         (error " ~A expected and not found in ~A at position ~A" character name (file-position istream)))
      (read-char istream)))


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
              (progn (eval-possible-leaf-type fs-id)
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
    (parse (split-into-words 
            (preprocess-sentence-string 
             (string-trim '(#\space #\tab #\newline) sentence))))))


(defun whitespacep (char) 
  (member char '(#\space #\tab #\newline #\page #\return)))
