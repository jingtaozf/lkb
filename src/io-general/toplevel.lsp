;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

(in-package :lkb)

;;; not loaded in tty mode

(defvar *type-fs-display* nil)

;;; Top level functions
;;;
;;; "Load"
;;; all in various input files
;;; apart from

(defun read-script-file nil
  (with-package (:lkb)
    (let* (#+:allegro 
           (excl:*libfasl* nil)
           (file-name (ask-user-for-existing-pathname "Script file?")))
      (with-output-to-top ()
        (read-script-file-aux file-name)))))

;;; "View"
;;;
;;; "Type hierarchy" show-type-tree

(defparameter *last-type-name* '*top*)

(defun show-type-tree nil
  (multiple-value-bind (type show-all-p ignore-limit-p)
      (ask-user-for-type nil '("Show all types?" . :check-box)
	 '("Ignore 300 descendant limit?" . :check-box))
    (when type
      (if
          (or ignore-limit-p
	      (< (length (ltype-descendants (get-type-entry type))) 300))
	  (create-type-hierarchy-tree type nil show-all-p)
	(show-message-window 
	 (format nil "Type has ~A descendants, display stopped~%Use check box to override" 
		 (length (ltype-descendants (get-type-entry type)))))))))



;;; "Type spec" show-type-spec
(defun show-type-spec nil
   (let ((type (ask-user-for-type)))
      (when type
         (display-type-in-tree type t)
         (show-type-spec-aux type (get-type-entry type)))))

(defun show-type-spec-aux (type type-entry)
   (let ((*type-fs-display* t))
     (display-fs-and-parents 
      (ltype-local-constraint type-entry) 
      (format nil "~(~A~) - definition" type)
      (loop for parent in (ltype-parents type-entry)
           append
           (let ((parent-entry (get-type-entry parent)))
             (if (ltype-glbp parent-entry)
                 (list parent (remove-duplicates (get-real-types parent)))
               (list parent))))
      type)))
           
;;; "Expanded type" show-type                  
(defun show-type nil
  (let* ((type (ask-user-for-type))
        (type-entry (if type (get-type-entry type))))
      (when type-entry 
         (display-type-in-tree type t)
         (show-type-aux type type-entry))))

(defun show-type-aux (type type-entry)
   (if (ltype-tdfs type-entry)
      (let ((*type-fs-display* t))
         (display-fs (ltype-tdfs type-entry) 
                     (format nil "~(~A~) - expanded" type)
                     type))
      (format t "~%No tdfs for type ~A" type)))


;;; "Type hierarchy" show-type-hierarchy
;;; "Word entries" show-words

(defun show-words nil
  (show-word-aux t))

(defun show-word-defs nil
  (show-word-aux nil))

(defun show-word-aux (exp-p)
  (loop 
      with word-string 
      and orth-list 
      and lex-entries
      and prompt = ""
      for word-entry in 
	(loop
	    do
	     (setf word-string (ask-user-for-word prompt))
	     (if word-string 
		 (setf orth-list 
		   (split-into-words (string-upcase word-string)))
	       (return nil))
	     (if orth-list
		 (setf lex-entries
		   (get-lex-entry (car orth-list))))
	     (if lex-entries
		 (return lex-entries)
	       (setf prompt 
		 (format nil "(~a is not defined)" (string-upcase word-string)))))
      do
	(when (equal (mapcar #'string-upcase (lex-entry-orth word-entry))
		     orth-list)
	  (if exp-p
	      (display-fs (lex-entry-full-fs word-entry) 
			  (format nil "~(~A~) - ~A - expanded" word-string
				  (lex-entry-id word-entry))
			  (lex-entry-id word-entry))
	    (display-unexpanded-lex-entry word-string word-entry
					  (lex-entry-id word-entry))))))   

;;; "Lex entry" show-lex
(defun show-lex nil
  (let* ((lex (ask-user-for-lex))
        (lex-entry (if lex (get-lex-entry-from-id lex))))
      (when lex-entry 
            (display-fs (lex-entry-full-fs lex-entry) 
                        (format nil "~(~A~) - expanded" lex)
                        lex))))

(defun show-other nil
  (multiple-value-bind
      (other-id other-entry)
      (ask-user-for-other-id)
      (when other-entry 
            (display-fs (psort-full-fs other-entry) 
                        (format nil "~(~A~) - expanded" other-id)
                        other-id))))

(defun display-unexpanded-lex-entry (lex lex-entry &optional id)
  (display-fs 
   (lex-entry-local-fs lex-entry) 
   (if id
       (format nil "~(~A~) - ~A - definition (indef)" lex id)
     (format nil "~(~A~) - definition (indef)" lex))))


(defun show-grammar-rule nil
  (let* ((rule-entry (ask-user-for-rule)))
      (when rule-entry 
            (display-fs (rule-full-fs rule-entry) 
                        (format nil "~(~A~)" (rule-id rule-entry))
                        (rule-id rule-entry)
                        ))))

(defun show-lex-rule nil
  (let* ((rule-entry (ask-user-for-lexical-rule)))
      (when rule-entry 
            (display-fs (rule-full-fs rule-entry) 
                        (format nil "~(~A~)" (rule-id rule-entry))
                        (rule-id rule-entry))))) 


(defparameter *last-lex-id* 'kim_1)
        

        
;;; 
;;; View utilities


(defun ask-user-for-type (&optional qstring check-box-spec show-all-types-spec 
				    (prompt ""))
  (let ((res
         (with-package (:lkb)
           (ask-for-lisp-movable "Current Interaction" 
             (append
	      (list (cons (format nil "~a~%~a" prompt (or qstring "Type?"))
				  *last-type-name*))
                (if check-box-spec (list check-box-spec) nil)
                (if show-all-types-spec (list show-all-types-spec) nil))
             150 *type-names*))))
      (when res
        (let ((type (car res))
              (check-1-p (cadr res))
	      (check-2-p (caddr res)))
	  (setf *last-type-name* type)
	  (eval-possible-leaf-type *leaf-types* type)
          (let ((type-entry (get-type-entry type)))
            (unless type-entry
               (setf type 
		 (ask-user-for-type qstring 
				    check-box-spec 
				    nil 
				    (format nil "(Type ~A is not defined)" type))))
            (values type check-1-p check-2-p))))))


;;; display-fs is in outputfs.lsp

(defun ask-user-for-lex (&optional (prompt ""))
  (let* ((possible-name
	  (with-package (:lkb)
	    (ask-for-lisp-movable "Current Interaction" 
				  `((,(format nil "~a~%Lex-id?" prompt) . ,*last-lex-id*))
				  150))))
    (when possible-name
      (let* ((lex (car possible-name))
             (lex-entry (get-lex-entry-from-id lex)))
       (setf *last-lex-id* lex)
       (unless lex-entry
          (setf lex (ask-user-for-lex
		     (format nil "(Lexical entry ~A is not defined)" lex))))
        lex))))

(defparameter *last-other-id* 'root)

(defun ask-user-for-other-id (&optional (prompt ""))
  (let ((possible-name
         (with-package (:lkb)
 	  (ask-for-lisp-movable "Current Interaction" 
		 	        `((,(format nil "~a~%Entry id?" prompt) . ,*last-other-id*))
			        150))))
      (when possible-name
         (let* ((id (car possible-name))
		(id-entry (get-other-entry id)))
	   (setf *last-other-id* id)
	   (unless id-entry
	     (setf id (ask-user-for-other-id (format nil "(~A is not defined)" id))))
	   (values id id-entry)))))

(defparameter *last-rule-id* 'head-specifier-rule)

(defun ask-user-for-rule nil
  (let ((rule-names nil))
    (declare (dynamic-extent rule-names))
    (maphash #'(lambda (name value)
		 (declare (ignore value))
		 (push name rule-names))
	     *rules*)
    (setf rule-names (sort rule-names #'string-lessp))
    (let ((possible-name
           (with-package (:lkb)
             (ask-for-lisp-movable "Current Interaction" 
                                   `(("Rule?" . ,*last-rule-id*)) 
                                   150 rule-names))))
      (when possible-name
	(let* ((name (car possible-name))
               (rule-entry (get-grammar-rule-entry name)))
	  (unless rule-entry
	    (format t "~%~A is not defined" name)
	    (setf rule-entry (ask-user-for-rule)))
	  (when name (setf *last-rule-id* name))
	  rule-entry)))))

(defparameter *last-lex-rule-id* nil)

(defun ask-user-for-lexical-rule nil
  (if *lexical-rules*
      (let ((rule-names nil))
        (declare (dynamic-extent rule-names))
        (maphash #'(lambda (name value)
                     (declare (ignore value))
                     (push name rule-names))
                 *lexical-rules*)
        (setf rule-names (sort rule-names #'string-lessp))
        (let ((possible-rule-name
               (if *last-lex-rule-id*
                   (with-package (:lkb)
                     (ask-for-lisp-movable "Current Interaction" 
                                           `(("Lexical Rule?" . ,*last-lex-rule-id*))
                                           150 rule-names))
                 (with-package (:lkb)
                   (ask-for-lisp-movable "Current Interaction" 
                                         `(("Lexical Rule?" . ,(car rule-names)))
                                         150 rule-names))))) 
          (when possible-rule-name
            (let* ((name (car possible-rule-name))
                   (rule-entry (get-lex-rule-entry name)))
              (unless rule-entry 
                (format t "~%Lexical rule ~A is not defined" name)
                (setf rule-entry (ask-user-for-lexical-rule)))
              (when name (setf *last-lex-rule-id* name))
              rule-entry))))))

(defparameter *last-word* "the")

(defun ask-user-for-word (prompt)
   (let ((possible-name
            (ask-for-strings-movable "Current interaction" 
               `((,(format nil "~a~%Word?" prompt) . 
                     ,(or *last-word* 
                        (car (lex-words *lexicon*))))) 150)))
      (when possible-name
         (let* ((lex (car possible-name))
               (lex-string
                  (if (stringp lex)
                     lex
                     (format nil "~S" lex))))
            (when lex-string (setf *last-word* lex-string))
            lex-string))))


;;; Lexical rule application


(defun apply-lex (&optional id)
   (let* ((lex (or id (ask-user-for-lex)))
         (lex-entry (if lex (get-lex-entry-from-id lex)))
         (lex-entry-fs
            (if lex-entry (lex-entry-full-fs lex-entry))))
      (when lex-entry-fs 
         (let 
            ((lex-rule (ask-user-for-lexical-rule)))
           (when lex-rule
               (let 
                   ((result
                     (apply-lex-interactive lex lex-entry-fs lex-rule)))
                  (cond (result
                        (display-fs result
                           (format nil "~(~A~) + ~A" 
                                   lex (rule-id lex-rule))
                           (rule-id lex-rule)))
                     (t (format t 
                           "~%Lexical rule application failed")))))))))

(defun apply-lex-rules (&optional id)
   (let* ((lex (or id (ask-user-for-lex)))
         (lex-entry (if lex (get-lex-entry-from-id lex)))
         (lex-entry-fs
            (if lex-entry (lex-entry-full-fs lex-entry))))
      (when lex-entry-fs 
         (setf *number-of-applications* 0)
         (let ((result-list
                  (try-all-lexical-rules 
                     (list (cons nil lex-entry-fs)))))
            (cond (result-list
                   (draw-active-list
                    (mapcar #'(lambda (result-pair)
                                (let ((string 
                                       (format nil "~(~A~) ~{+ ~A~}" 
                                               lex 
                                               (reverse 
                                                (car result-pair)))))
                                  (cons string (cons string
                                                     (cdr result-pair)))))
                            result-list)
                    "Lexical rule results"
                    (list
                         (cons 
                          "Feature structure"
                          #'(lambda (display-pair)
                              (display-fs (cdr display-pair)
                                          (car display-pair)))))))
               (t (format t 
                     "~%No applicable lexical rules")))))))




;;; "Parse"
;;;
;;; "Parse Input" do-parse


(defparameter *last-parses* '("the dog barks"))


(defun do-parse nil
  (let* ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("Sentence" . ,(cons :typein-menu *last-parses*))) 400)))
    (when sentence
      (setf *sentence* (car sentence))
      (close-existing-chart-windows)
      (let ((str (string-trim '(#\space #\tab #\newline) (car sentence))))
        (setq *last-parses* 
          (butlast
           (cons str (remove str *last-parses* :test #'equal))
           (max 0 (- (length *last-parses*) 12))))
                                        ; limit number of sentences retained

        (with-output-to-top ()
	  (parse
	   (split-into-words 
	    (preprocess-sentence-string str))))))))

;;; "Generate" generate-from-edge
;;; this is in mrstoplevel.lsp


;;; Unification checking

(defun interactive-unification-check nil
  ;;; I've made this just work on FSs since the default
  ;;; stuff won't fail anyway
  (let* ((check-details
          (with-package (:lkb)
            (ask-for-lisp-movable 
             "Check unification" '(("fs1" . head-specifier-rule) 
                                   ("path1 in ()s (optional)" . (args first))
                                   ("fs2" . sleeps_1)
                                   ("path2 in ()s (optional)" . nil)
                                   ("name for result (optional)" . nil)))))
         (fs1-id (car check-details))
         (path1 (cadr check-details))
         (fs2-id (caddr check-details))
         (path2 (cadddr check-details))
         (resname (cadddr (cdr check-details))))
    (when check-details
      (if (and fs1-id fs2-id) 
          (let ((fs1 (get-fs-given-id fs1-id))
                (fs2 (get-fs-given-id fs2-id)))
            (if (and fs1 fs2 (listp path1) (listp path2))
                (let ((resdag fs1))
                  (when 
                      (setq resdag
                        (unify-paths-with-fail-messages 
                         (create-path-from-feature-list path1) 
                         resdag
                    (create-path-from-feature-list path2) 
                    fs2 fs1-id path1 fs2-id path2))
                    (format t "~%Unification successful")
                    (if resname (store-temporary-psort-entry resname resdag))))
              (cond ((null fs1) 
                     (progn (cerror  "~%Try again" "~%~A is not a valid FS identifier" fs1-id)
                            (interactive-unification-check)))
                    ((null fs2) 
                     (progn (cerror  "~%Try again" "~%~A is not a valid FS identifier" fs2-id)
                            (interactive-unification-check)))
                    (t (progn (cerror  "~%Try again" "~%Paths are not lists")
                              (interactive-unification-check))))))
        (progn (cerror  "~%Try again" "~%Need to specify both feature structures")
               (interactive-unification-check))))))


#-(or :clim :www)
(defun compare-parses (&optional edges)
  (declare (ignore edges))
  nil)

;;
;; Interactively set parameters
;;

(defun get-parameters ()
  (setq *lkb-user-params* (sort *lkb-user-params* #'string<))
  (let* ((*print-readably* t)
	 (params (mapcan #'(lambda (p)
			     ;; Skip things we won't be able to read back in
			     (handler-case
				 (list 
				  (cons (format nil "~S" p) 
					(write-to-string (symbol-value p))))
			       (print-not-readable () nil)))
			 *lkb-user-params*))
	 (result (ask-for-strings-movable "Set options" params)))
    (when result
      (loop for p in params
	  for r in result
	  do (setf (symbol-value (read-from-string (car p)))
	       (read-from-string r))))
    (unless *user-params-file*
      (setf *user-params-file* 
        (ask-user-for-new-pathname "File to save parameters?")))
    (when *user-params-file*
      (handler-case 
          (with-open-file
              (ostream *user-params-file* :direction :output
               :if-exists :supersede)
            (format ostream ";;; Automatically generated file - do not edit!")
            (loop for p in params
                for r in result
                do
                  (format ostream "~%(defparameter ~S '~S)"
                          (read-from-string (car p))
                          (read-from-string r))))
        (file-error (condition)
          (format t "~%Parameters not saved to file ~A
                      ~A" *user-params-file* condition))))))

;;
;; Save and load shrunk paths in display settings file
;;

(defvar *display-settings-file* nil)

(defun output-display-settings nil
  (let ((filename
         (or *display-settings-file*
             (ask-user-for-new-pathname 
              "Save type display settings to?")))
	(*print-pretty* nil)
	(*print-readably* t))
    (when filename
      (with-open-file (stream filename 
		       :direction :output 
                       :if-exists :supersede)
	(print *shrunk-types* stream)))))

(defun load-display-settings nil
  (set-up-display-settings
   (ask-user-for-existing-pathname "Load type display settings from?")))

;;; debugging - finding maximal type

(defun find-type-from-features nil
  (let ((feature-list
         (with-package (:lkb)
           (ask-for-lisp-movable "Current Interaction" 
                                 `(("Feature(s)" . (,*diff-list-list*)))
                                 150))))
    (when feature-list 
      (when (listp (car feature-list)) 
        (setf feature-list (car feature-list)))
      (let ((type (maximal-type-of-list feature-list)))
        (if type
            (format t "~%Maximal type for ~A is ~A" feature-list type)
          (let ((bogus-features (loop for f in feature-list
                                     nconc
                                      (if (not (maximal-type-of f)) 
                                          (list f)))))
            (if bogus-features
                (format t "~%Features ~A not found in this grammar"
                        bogus-features)
              (format t "~%Features ~A not mutually compatible" feature-list))))))))

;;; various fns moved to utils.lsp, because they are relevant in tty mode
