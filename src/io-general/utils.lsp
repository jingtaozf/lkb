;;; Copyright (c) 1991-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see licence.txt for conditions

;;; Functions moved from io-paths/typeinput.lsp

(in-package :lkb)

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

(defun check-for-string (str istream)
  (loop for character in (coerce str 'list)
      do
	(let ((next-char (peek-char t istream nil 'eof)))
	  (if (char= next-char character)
	      (read-char istream)
	      (error 
                 "~%Syntax error: ~A expected and not found at position ~A" 
                 character (file-position istream))))))


(defun lkb-read-cerror (istream string &rest rest)
  (setf *syntax-error* t)
  (apply #'format 
         (append (list t 
                       (concatenate 'string "~%Syntax error at position ~A: " string)  
                       (file-position istream))
                 rest)))

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
  (with-package (:lkb)
    (let ((item (read istream)))
      (if (stringp item)
        (if strings-allowed 
          item 
          (error "~%~S should not be a string" item))
        (if (symbolp item)
          item
          (convert-to-lkb-symbol item))))))

(defun convert-to-lkb-symbol (item)
  (intern (format nil "~S" item) :lkb))

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
  (let ((lex-entry (get-lex-entry-from-id fs-id)))
    (if lex-entry (lex-entry-full-fs lex-entry)
      (let ((other-entry (get-other-entry fs-id)))
	(if other-entry (psort-full-fs other-entry)
	  (let ((rule-entry (get-grammar-rule-entry fs-id)))
	    (if rule-entry (rule-full-fs rule-entry)
	      (let ((lex-rule-entry (get-lex-rule-entry fs-id)))
		(if lex-rule-entry (rule-full-fs lex-rule-entry)
		  (progn (eval-possible-leaf-type *leaf-types* fs-id)
			 (let ((type (get-type-entry fs-id)))
			   (if type (tdfs-of fs-id)))))))))))))

	  
(defun split-into-words (sentence-string)
  (if (listp sentence-string)
    sentence-string
    ; split-into-words is used in various places 
    ; so shouldn't be redefined for more complete scanner
    (let ((current-word nil)
          (current-sentence nil))
      (loop for character in (coerce sentence-string 'list)
          do
            (cond ((char= character #\Space) 
                   (when current-word 
                     (push (coerce (nreverse current-word) 'string)
                           current-sentence)
                     (setf current-word nil)))
                  (t (push character current-word))))
      (push (coerce (nreverse current-word) 'string) current-sentence)
      (nreverse current-sentence))))

(defun do-parse-tty (sentence)
  (when sentence
    (setf *sentence* sentence)
    (close-existing-chart-windows)
    ;; this is sometimes called when not in tty mode
    ;; so make sure we remove unwanted charts
    (parse (split-into-words 
            (preprocess-sentence-string 
             (string-trim '(#\space #\tab #\newline) sentence))))))

;;; look up words directly (taken from ../tty/tty.lsp)
(defun show-word-aux-tty (word-string exp-p)
  (let* ((orth-list (if word-string 
                        (split-into-words (string-upcase word-string))))
         (lex-entries (if orth-list (get-lex-entry (car orth-list)))))
                                        ; entries indexed by all elements
    (loop for word-entry in lex-entries
        do
          (when (equal (mapcar #'string-upcase (lex-entry-orth word-entry))
                       orth-list)
            (if exp-p
                (display-fs (lex-entry-full-fs word-entry) 
                            (format nil "~(~A~) - ~A - expanded" word-string
                                    (lex-entry-id word-entry))
                            (lex-entry-id word-entry))
              (display-unexpanded-lex-entry word-string word-entry
                                            (lex-entry-id word-entry)))))))

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

;;;
;;; characters dropped in preprocessing (wide-character ones only with ICS)
;;;
(defparameter *punctuation-characters*
  (append
   '(#\! #\" #\& #\' #\(
     #\) #\* #\+ #\, #\- #\. #\/ #\: #\;
     #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
     #\_ #\` #\{ #\| #\} #\~)
   #+:ics
   '(#\ideographic_full_stop #\fullwidth_question_mark 
     #\horizontal_ellipsis #\fullwidth_full_stop
     #\fullwidth_exclamation_mark
     #\fullwidth_comma #\ideographic_space)))

(defun alphanumeric-or-extended-p (c)
  (and (graphic-char-p c) 
       (not (member c *punctuation-characters* :test #'eql))))

(defun punctuationp (thing)
  (let ((string (string thing)))
    (loop
        for c across string
        always (member c *punctuation-characters*))))

;;;  Loading and reloading - called by the tty version as well
;;; as the menu driven version

;;; (defparameter *current-grammar-load-file* nil)
;;; defined in globals

(defun read-script-file-aux (file-name)
  (with-package (:lkb)
    (when file-name
      (let (#+allegro (excl:*redefinition-warnings* nil)
            #+mcl (*warn-if-redefine* nil)
            #+mcl (ccl::*suppress-compiler-warnings* t)
            (*syntax-error* nil))
        (setf *current-grammar-load-file* file-name)
        #-:tty
        (enable-grammar-reload-interactions)
      (clear-almost-everything)
      (load file-name)
      (lkb-beep)
      (if *syntax-error*
        (format t "~%WARNING: syntax error(s) - check messages")
        (format t "~%Grammar input complete~%"))))))



(defun clear-almost-everything nil
    (setf *syntax-error* nil)
    (clear-type-load-files)
    (clear-lex-load-files)
    (clear-rule-load-files)
    (clear-leaf-types *leaf-types*)
    (close-lex *lexicon*) ;; this will close *lexicon* and _all_ sublexicons
    (unless 
        (typep *lexicon* 'cdb-lex-database)
      (setf *lexicon* (make-instance 'cdb-lex-database)))
    (reset-morph-var)
    (clear-grammar)              ;; should clear everything that might not be
    (clear-lex-rules)            ;; overridden, this should do for now
    (clear-idioms)
    (setf  *check-paths* nil)
    #+:preprocessor
    (clear-preprocessor))

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
   :host (pathname-host *load-truename*)
   :device (pathname-device *load-truename*)
   :directory (pathname-directory *load-truename*)))

(defun parent-directory nil
  (make-pathname 
   :host (pathname-host *load-truename*)
   :device (pathname-device *load-truename*)
   :directory (butlast (pathname-directory *load-truename*))))

(defun lkb-pathname (directory name)
  (merge-pathnames
   (make-pathname :name name)
   directory))

;;;
;;; add optional .compile. argument to supress compilation when needed; this
;;; is essential when running multiple LKB instances simultaneously on the
;;; same file system (e.g. as back-end processors for [incr tsdb()]); the 
;;; problem is that multiple processes end up recompiling files in parallel.
;;;                                                       (1-apr-99  -  oe)
;;;
(defun lkb-load-lisp (directory name &optional optional (compile t))
  #-(or :allegro :lispworks)
  (declare (ignore compile))
  
  (let ((file (merge-pathnames (make-pathname :name name) 
                               directory)))
    (if (probe-file file)
      #+:allegro 
      (load
       (handler-bind ((excl:compiler-no-in-package-warning
                       #'(lambda (c)
                           (declare (ignore c))
                           (muffle-warning))))
                     (if (and compile (member :compiler *features*))
                       (compile-file file :verbose nil :print nil)
                       file)))
      #+:lispworks
      (load 
       (if compile (compile-file file :load t :print nil :verbose nil) file))
      #-(or :allegro :lispworks)
      (load file)
      (unless optional
        (cerror "Continue loading script" "~%File ~A not found" file)))))

(defun load-lkb-preferences (directory name)
  (let ((file (merge-pathnames (make-pathname :name name) 
                               directory)))
    (when (probe-file file)
      (load file))
    (setf *user-params-file* file)))


(defun load-irregular-spellings (pathnames)
  (when pathnames
    (unless (listp pathnames)
      (setf pathnames (list pathnames)))
    (with-package (:lkb)
      (read-irreg-form-strings 
       (loop for pathname in pathnames
            nconc
            (let ((res
                   (with-open-file (istream pathname
                                    :direction :input)
                     (read istream))))
              (when (and res (stringp res))
                (list res))))))))
              
  
;;; utility for reloading

(defun check-load-names (file-list &optional filetype)
  (let ((ok t))
    (if file-list
      (loop for file in file-list
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

(defun check-load-name (file)
  (or (probe-file file)
      (progn 
	(format t "~%Error - file ~A does not exist" file)
        nil)))

(defun display-lex-words nil
  (let ((stream lkb::*lkb-background-stream*))
    (format stream "~%")
    (loop for word in (list-lex-words)
        do (format stream "~A, " (string-downcase word)))
    (format stream "~%")))

(defun list-lex-words nil
  (sort (lex-words *lexicon*) #'string-lessp))

(defun list-lex-ids nil
  (sort (collect-psort-ids *lexicon*) #'string-lessp))

(defun list-grammar-rules nil
  (let ((rule-names nil))
    (maphash #'(lambda (name value)
                 (declare (ignore value))
                 (push name rule-names))
             *rules*)
    (sort rule-names #'string-lessp)))

(defun list-lex-rules nil
  (let ((rule-names nil))
    (maphash #'(lambda (name value)
                 (declare (ignore value))
                 (push name rule-names))
             *lexical-rules*)
    (sort rule-names #'string-lessp)))
    

(defun print-chart-toplevel nil
  (let ((stream lkb::*lkb-background-stream*))
    (print-chart :stream stream)))

(defun print-gen-chart-toplevel nil
  (let ((stream lkb::*lkb-background-stream*))
    (print-gen-chart :stream stream)))

;;; Output of derivation trees

(defun construct-derivation-trees nil
  (loop for parse in *parse-record*
      collect
        (deriv-tree-compute-derivation-tree parse)))

#|
;;; for batch parsing
;;; (setf *do-something-with-parse* 'print-derivation-trees)

(defun print-derivation-trees nil
  (format *ostream* "~%~S" (construct-derivation-trees)))

;;; *ostream* is set to the output stream for the batch parse
;;; in parse.lsp

|#

;;; the following are borrowed from the tsdb code 
;;; with some modifications

(defun deriv-tree-edge-label (edge)
  (intern 
   (typecase (edge-rule edge)
     (string (string-upcase (edge-rule edge)))
     (symbol (string (edge-rule edge)))
     (rule (string (rule-id (edge-rule edge))))
     (t :unknown))
   :lkb))

(defun deriv-tree-compute-derivation-tree (edge)
  (let ((edge-children 
         (or (edge-children edge) 
             (if (edge-morph-history edge)
                 (list (edge-morph-history edge))))))
    (if edge-children
      (let* ((start *chart-limit*)
             (end 0)
             (children
              (loop
                  for child in edge-children
                    collect
                    (let ((derivation 
                           (deriv-tree-compute-derivation-tree 
                            child)))
                      (setf start (min start (second derivation)))
                      (setf end (max end (third derivation)))
                      derivation))))
        (nconc (list (deriv-tree-edge-label edge) start end)
               children))
      (list (first (edge-lex-ids edge))
            (edge-from edge) (edge-to edge)
            (list (car (edge-leaves edge)) 
                  (edge-from edge) (edge-to edge))))))


#|
(extract-fine-system-sentence "1@CSLI@formal@none@1@S@Abrams works.@1@2@@oe@8-sep-1999")
|#

(defun extract-fine-system-sentence (str)
  ;;; "1@CSLI@formal@none@1@S@Abrams works.@1@2@@oe@8-sep-1999"
  ;;; returns
  ;;; "Abrams works."
  (if (find #\@ str)
      (let ((ampcount 0)
	    (sstart nil) 
	    (send nil))
	(dotimes (n (length str)) 
	  (let ((char (elt str n))) 
	    (when (eql char #\@) 
	      (setf ampcount (+ 1 ampcount))
	      (when (eql ampcount 6)
		(setf sstart (+ 1 n)))))
	  (when (eql ampcount 7)
	    (setf send n)
	    (return)))
	(if (and sstart send)
	    (subseq str sstart send)
	  str))
    str))

#|
(compare-batch-parse-files "test1" "test2")
|#

(defun compare-batch-parse-files (file1 file2)
  ;;; ignores edges
  (with-open-file (istream1 file1 :direction :input)
    (with-open-file (istream2 file2 :direction :input)
      (loop 
	(let ((str1 (read-line istream1))
	      (str2 (read-line istream2)))
	  (unless (and str1 str2) (return))
	  (unless (equal (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5
			    #\6 #\7 #\8 #\9)
			  str1)
			 (string-right-trim 
			  '(#\0 #\1 #\2 #\3 #\4 #\5
			    #\6 #\7 #\8 #\9)
			  str2))
	    (format t "~%~S~%~S" str1 str2)))))))


