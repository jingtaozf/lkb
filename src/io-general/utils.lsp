;;; Copyright (c) 1991-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Benjamin Waldron
;;; see LICENSE for conditions

;;; Functions moved from io-paths/typeinput.lsp

(in-package :lkb)

(defun check-for (character istream name)
   (let ((next-char (peek-with-comments istream)))
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

(defun morph-read-cerror (string)
  ;;; variant of above for morphology rules
  (setf *syntax-error* t)
  (format t "~A" string)
  nil)
  

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

;; strings of WORDS separated by 1+ spc chars -> list of WORDS
;; list-in -> list-out
(defun split-into-words (sentence-string)
  (if (smaf::saf-p sentence-string)
      ;;return as is
      (return-from split-into-words sentence-string))
  (if (xml-p sentence-string)
      ;;assume MAF input and return as is
      (return-from split-into-words sentence-string))
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

#+:null
(defun display-unexpanded-lex-entry (word-string word-entry id)
  (declare (ignore word-entry))
  (format t "~%~A ~A" word-string id)) 
	  ;;; (lex-entry-unifs word-entry)))


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

(defun punctuationp (thing)
  (let ((string (string thing)))
    (loop
        for c across string
        always (member c *punctuation-characters*))))

(defun alphanumeric-or-extended-p (c)
  (and (graphic-char-p c) 
       (not (member c *punctuation-characters* :test #'eql))))

(defun punctuation-normalize-string (string)
  (loop
      with padding = 128
      with length = (+ (length string) padding)
      with result = (make-array length
                                :element-type 'character
                                :adjustable nil :fill-pointer 0)
      with space = t
      for c across string
      when (or (member c '(#\Space #\Newline #\Tab))
               (not (alphanumeric-or-extended-p c))) do
        (when space (incf padding))
        (unless space
          (vector-push #\Space result)
          (setf space :space))
      else do
        (vector-push c result)
        (setf space nil)
      finally
        (when (and (eq space :space) (not (zerop (fill-pointer result))))
          (decf (fill-pointer result)))
        (return result)))

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
      (build-rule-filter)
      (build-lrfsm)
      #+:lui
      (when (lui-status-p) (lui-parameters))
      (mt:activate-transfer)
      (lkb-beep)
      (if *syntax-error*
        (format t "~%WARNING: error(s) - check messages")
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
    (setf *check-paths* nil)
    (clear-tdl-contexts)
    #+:preprocessor
    (preprocessor:clear-preprocessor)
    #+:preprocessor
    (clear-repp)
    (smaf:reset-conf)
    (mt:initialize-transfer))

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

(defun parent-directory (&optional path)
  (make-pathname 
   :host (pathname-host *load-truename*)
   :device (pathname-device *load-truename*)
   :directory (append
               (butlast (pathname-directory *load-truename*))
               (rest (pathname-directory (make-pathname :directory path))))))

(defun grandparent-directory (&optional path)
  (make-pathname 
   :host (pathname-host *load-truename*)
   :device (pathname-device *load-truename*)
   :directory (append
               (butlast (pathname-directory *load-truename*) 2)
               (rest (pathname-directory (make-pathname :directory path))))))

(defun great-grandparent-directory (&optional path)
  (make-pathname 
   :host (pathname-host *load-truename*)
   :device (pathname-device *load-truename*)
   :directory (append
               (butlast (pathname-directory *load-truename*) 3)
               (rest (pathname-directory (make-pathname :directory path))))))

(defun lkb-pathname (directory name)
  (merge-pathnames (pathname name) directory))

#+:logon
(defun make:logon-directory (path &optional format)
  (let* ((root (getenv "LOGONROOT"))
         (root (namestring (parse-namestring root)))
         (root (make-pathname :directory root)))
    (unless root
      (error "logon-directory(): unable to determine global LOGONROOT."))
    (let ((directory
           (make-pathname 
            :host (pathname-host root)
            :device (pathname-device root)
            :directory 
            (append
             (pathname-directory root)
             (rest (pathname-directory (make-pathname :directory path)))))))
      (if (eq format :string)
        (namestring directory)
        directory))))

#+:logon
(defun make:logon-file (path name &optional format)
  (let ((file (lkb-pathname (logon-directory path) name)))
    (if (eq format :string)
      (namestring file)
      file)))

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
      ;;
      ;; in order to protect concurrent processes compiling a file in parallel,
      ;; attempt puttting a mandatory lock on the source file while we compile
      ;; and load it.  however, only do this when compilation is requested, to
      ;; avoid unnecessary attempts to open the file for writing (which could
      ;; prevent loading a read-only copy of a grammar, for example).
      ;;
      #+:allegro
      (if compile
        (with-open-file (foo file
                         :direction :output :if-exists :append)
          (excl.osi:with-stream-lock (foo)
            (load
             (if (and compile (member :compiler *features*))
               (handler-bind ((excl:compiler-no-in-package-warning
                               #'(lambda (c)
                                   (declare (ignore c))
                                   (muffle-warning))))
                   (compile-file file :verbose nil :print nil))
                 file))))
        (load file))
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

(defun print-token-chart-toplevel nil
  (let ((stream lkb::*lkb-background-stream*))
    (print-tchart :stream stream)))

(defun print-gen-chart-toplevel nil
  (let ((stream lkb::*lkb-background-stream*))
    (print-gen-chart :stream stream)))

(defun print-lrfsm-toplevel nil
  (let ((stream lkb::*lkb-background-stream*))
    (print-nospfsm :stream stream)))

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
     (t "unknown"))
   :lkb))

(defun deriv-tree-compute-derivation-tree (edge)
  (let ((edge-children 
         (edge-children edge))) 
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

;;; interpreting *cvs-version* and testing its value (probably mostly 
;;; in scripts)
;;;
;;; the idea is to call these functions with times written using the same
;;; format as the *cvs-version* because I imagine this will
;;; usually be used by people who have checked the version.lisp file
;;; e.g., (lkb-version-equal-p "2005/08/28 19:31:05")
;;;
;;; These functions should work on versions of the lkb after sometime in 2000
;;; 
;;; I considered adding a facility to just specify a date without a time, 
;;; but this might lead to confusing results when there's multiple versions 
;;; on one date so it seems better to try and enforce precision.

(defun lkb-version-after-p (time-str)
  (let ((cvs-time (decode-cvs-version-time))
	(supplied-time (decode-time-string time-str)))
    (if supplied-time
	(if cvs-time
	    (> cvs-time supplied-time))
      (format t "~%Cannot interpret time ~A" time-str))))

(defun lkb-version-before-p (time-str)
  (let ((cvs-time (decode-cvs-version-time))
	(supplied-time (decode-time-string time-str)))
    (if supplied-time
	(if cvs-time
	    (< cvs-time supplied-time))
      (format t "~%Cannot interpret time ~A" time-str))))

(defun lkb-version-equal-p (time-str)
  (let ((cvs-time (decode-cvs-version-time))
	(supplied-time (decode-time-string time-str)))
    (if supplied-time
	(if cvs-time
	    (= cvs-time supplied-time))
      (format t "~%Cannot interpret time ~A" time-str))))

(defun decode-cvs-version-time nil
  (let ((cvs-str *cvs-version*))
    (if (and cvs-str (stringp cvs-str))
	(let ((time-str (subseq cvs-str 7 (- (length cvs-str) 2))))
	  (decode-time-string time-str)))))

(defun decode-time-string (time-str)
  ;;
  ;; _fix_me_
  ;; it turns out that CVS and SVN differ slightly in how they replace the
  ;; $Date$ keyword: specifically, SVN appears to use the current locale, i.e.
  ;; when building in oslo, norway, we end up with a format different from the
  ;; one we get in stanford, usa.  maybe we should just rewrite this function
  ;; as a cascade of regular expressions (one per format we expect), but for
  ;; now i believe i manage to generalize over the formats we see so far ...
  ;;                                                           (18-nov-08; oe)
  (let* ((slash1 (or (position #\/ time-str) (position #\- time-str)))
         (slash2 (when slash1 
                   (or (position #\/ time-str :start (+ 1 slash1))
                       (position #\- time-str :start (+ 1 slash1)))))
	 (space (when slash2 (position #\space time-str)))
	 (colon1 (when space (position #\: time-str)))
	 (colon2 (when colon1 (position #\: time-str :start (+ 1 colon1))))
         (end (when colon2 (position #\space time-str :start (+ colon2 1)))))
    (if colon1
	(let ((second (parse-integer (subseq time-str (+ 1 colon2) end)))
	      (minute (parse-integer (subseq time-str (+ 1 colon1) colon2)))
	      (hour (parse-integer (subseq time-str (+ 1 space) colon1)))
	      (date (parse-integer (subseq time-str (+ 1 slash2) space)))
	      (month (parse-integer (subseq time-str (+ 1 slash1) slash2)))
	      (year (parse-integer (subseq time-str 0 slash1))))
	  (apply #'encode-universal-time 
		 (list second minute hour date month year))))))

;;;
;;; add LKB-side support for coding systems, intended for use at the very top
;;; of the `script' file in each grammar; use a macro at the top-level, so as
;;; to normalize, say, 'utf-8, utf-8, and :utf-8.
;;;

;; mapping from recognised names to canonical names (first element in each list)
;; TO DO: add Emacs coding names
(define-constant %coding-system-names%
    '(
      (:iso8859-1 :latin1 :ascii :8-bit :|1250| :iso88591 :latin-1 :iso-8859-1)
      (:|1251| :CP1251 :|cp1251| :WINDOWS-1251 :|windows-1251|)
      (:|1252| :CP1252 :|cp1252| :WINDOWS-1252 :|windows-1252|)
      (:|1253| :CP1253 :|cp1253| :WINDOWS-1253 :|windows-1253|) ;; For MS Windows
      (:|1254| :CP1254 :|cp1254| :WINDOWS-1254 :|windows-1254|) ;; For MS Windows
      (:|1255| :CP1255 :|cp1255| :WINDOWS-1255 :|windows-1255|) ;; For MS Windows
      (:|1256| :CP1256 :|cp1256| :WINDOWS-1256 :|windows-1256|) ;; For MS Windows
      (:|1257| :CP1257 :|cp1257| :WINDOWS-1257 :|windows-1257|) ;; For MS Windows
      (:|1258| :CP1258 :|cp1258| :WINDOWS-1258 :|windows-1258|) ;; For MS Windows
      (:iso8859-2 :latin-2 :latin2 :ISO-8859-2 :|iso-8859-2| :|latin-2|)
      (:iso8859-3 :latin-3 :latin3 :ISO-8859-3 :|iso-8859-3| :|latin-3|)
      (:iso8859-4 :latin-4 :latin4 :ISO-8859-4 :|iso-8859-4| :|latin-4|)
      (:iso8859-5 :ISO-8859-5 :|iso-8859-5|)
      (:iso8859-6 :ISO-8859-6 :|iso-8859-6|)
      (:iso8859-7 :ISO-8859-7 :|iso-8859-7|)
      (:iso8859-8 :ISO-8859-8 :|iso-8859-8|)
      (:iso8859-9 :latin-5 :latin5 :ISO-8859-9 :|iso-8859-9| :|latin-5|)
      (:iso8859-14 :latin-8 :latin8 :ISO-8859-14 :|iso-8859-14| :|latin-8|)
      (:iso8859-15 :latin-9 :latin9 :LATIN9 :ISO-8859-15 :ISO8859-15)
      (:koi8-r :|koi8-r|)
      (:emacs-mule)
      (:utf8 :utf-8)
      (:big5)
      (:gb2312)
      (:euc :ujis :euc-jp :eucjp :|eucJP| :JAPAN.EUC :EUC_JP)
      (:|874|) ;; For MS Windows
      (:|932|) ;; For MS Windows
      (:|936|) ;; For MS Windows
      (:|949|) ;; For MS Windows
      (:|950|) ;; For MS Windows
      (:jis)
      (:shiftjis)))

;; mapping from canonical names
;; specified as:
;;  - :XXX when canonical name same as internal name
;;  - (:CANONICAL . :INTERNAL) otherwise
(define-constant %canonical-to-internal-coding-name-mapping%
    #+:allegro
    '(
      :iso8859-1
      :|1251|
      :|1252|
      :|1253|
      :|1254|
      :|1255|
      :|1256|
      :|1257|
      :|1258|
      :iso8859-2
      :iso8859-3
      :iso8859-4
      :iso8859-5
      :iso8859-6
      :iso8859-7
      :iso8859-8
      :iso8859-9
      :iso8859-14
      :iso8859-15
      :koi8-r
      :emacs-mule
      :utf8
      :big5
      :gb2312
      :euc
      :|874|
      :|932|
      :|936|
      :|949|
      :|950|
      :jis
      :shiftjis)
    #+:sbcl
    '(
      :iso8859-1
      (:|1251| . :cp1251)
      (:|1252| . :cp1252)
      (:|1253| . :cp1253)
      (:|1254| . :cp1254)
      (:|1255| . :cp1255)
      (:|1256| . :cp1256)
      (:|1257| . :cp1257)
      (:|1258| . :cp1258)
      :iso8859-2
      :iso8859-3
      :iso8859-4
      :iso8859-5
      :iso8859-6
      :iso8859-7
      :iso8859-8
      :iso8859-9
      :iso8859-14
      :iso8859-15
      :koi8-r
      ;:emacs-mule
      :utf8
      ;:big5
      ;:gb2312
      (:euc . :euc-jp)
      (:|874| . :cp874)
      ;:|932|
      ;:|936|
      ;:|949|
      ;:|950|
      ;:jis
      ;:shiftjis
      )
    #-(or :allegro :sbcl)
    NIL
    )

(defun internal-coding-system-name (name)
  (or
   (find name %canonical-to-internal-coding-name-mapping%)
   (cdr
    (find name %canonical-to-internal-coding-name-mapping%
	  :test #'(lambda (x y)
		    (and (listp y)
			 (eq x (car y))))))))

(defun canonical-coding-system-name (name)
  (car (find name %coding-system-names% 
	     :test #'(lambda (x y)
		       (member x y)))))

(defmacro set-coding-system (coding)
  `(do-set-coding-system
       ,(typecase coding
          (symbol (intern (string coding) :keyword))
          (cons (when (eq (first coding) 'quote)
                  (intern (string (second coding)) :keyword)))
          (string (intern (string-upcase coding) :keyword)))
     ',coding))

(defun do-set-coding-system (coding &optional raw)
  (let (internal-coding-name)
    (unless
	(setf coding
	  (canonical-coding-system-name coding))
      (error "set-coding-system(): invalid coding system `~a'.~%~%~
valid coding systems are~%~a" raw %coding-system-names%))
    (setf internal-coding-name (internal-coding-system-name coding))
    (if (not internal-coding-name)
	(format t
		"~&set-coding-system(): ignoring request for ~a on this Lisp implementation." coding) 
      #+:allegro
      (if (setf excl:*locale* 
	    (excl::find-locale 
	     (format nil ".~a" internal-coding-name)))
	  (format t "~&set-coding-system(): activated ~a." coding)
	(format t "~&set-coding-system(): mysterious problem activating ~a." coding))
      #+:sbcl
      (if (and
	   (sb-impl::get-external-format internal-coding-name)
	   (setf sb-impl::*default-external-format* 
	    internal-coding-name))
	  (format t "~&set-coding-system(): activated ~a." coding)
	(format t "~&set-coding-system(): mysterious problem activating ~a." coding))
      #-(or :allegro :sbcl)
      (format
       t
       "~&set-coding-system(): ignoring request for ~a on this Lisp implementation."
       coding))))

(defun grammar-encoding (coding)
  (do-set-coding-system (intern (string coding) :keyword)))

;;
;; [miscellaneous XML-related functions]
;;

(defun pprint-xml (x)
  (pretty-print-xml x))

;; input xml must have NO superfluous space
(defun pretty-print-xml (xml)
  (coerce
   (cdr ;; remove initial newline introduced by code below
    (loop
       with i = -1
	with last
	with last2
	for c across xml
	if (and (char= #\< last) (char= #\/ c)) do (decf i)
	if (and (char= #\< last) (or (null last2) (char= #\> last2)))
	append (list #\Newline) into x and
	append (loop for n from 1 to i
		   collect #\Space) into x
	if (and (char= #\< last) (char= #\/ c)) do (decf i)
	if (and (char= #\< last) (char= #\! c)) do (decf i)
	if (and (char= #\/ last) (char= #\> c)) do (decf i)
	if (char= #\< c) do (incf i)
	if last append (list last) into x
	do (setf last2 last) (setf last c)
	finally (return (append x (list c)))))
    'string))


;; escape string for use as XML text
(defun xml-escape (str)
  (coerce 
   (loop
       for c across str
       if (char= #\" c) append '(#\& #\q #\u #\o #\t #\;)
       else if (char= #\' c) append '(#\& #\a #\p #\o #\s #\;)
       else if (char= #\& c) append '(#\& #\a #\m #\p #\;)
       else if (char= #\< c) append '(#\& #\l #\t #\;)
       else if (char= #\> c) append '(#\& #\g #\t #\;)
       else append (list c))
   'string))
