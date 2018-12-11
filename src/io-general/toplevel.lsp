;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package :lkb)

;;; not loaded in tty mode

(defvar *type-fs-display* nil)

;;; Top level functions
;;;
;;; "Load"

(defun read-script-file nil
  (let* (#+:allegro (excl:*libfasl* nil) ; relevant only for versions < 7.0
         (file-name (ask-user-for-existing-pathname "Script file?")))
    (read-script-file-aux file-name)))

;;; "View"
;;;
;;; "Type hierarchy" show-type-tree

(defparameter *type-hierarchy-limit* #+:mcclim 15000 #-:mcclim 300)

(defun show-type-tree nil
  (multiple-value-bind (type show-all-p display-definitions-p)
      (ask-user-for-type nil
         #-:clim '("Show all types?" . :check-box)
         #-:clim '("Display type definitions?" . :check-box)) ; both moved to hierarchy window
    (when type
      (if
        (or (null *type-hierarchy-limit*)
            (< (length (ltype-descendants (get-type-entry type))) *type-hierarchy-limit*)
            (y-or-n-p-general 
	      (format nil "Hierarchy contains ~A types so displaying it may take some time.
Do you really want to view it?"
	        (1+ (length (ltype-descendants (get-type-entry type)))))))
        (create-type-hierarchy-tree type nil show-all-p display-definitions-p)))))


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
		 (format nil "~a is not defined." (string-upcase word-string)))))
      do
	(when (equal (mapcar #'string-upcase (lex-entry-orth word-entry))
		     orth-list)
	  (if exp-p
	      (display-fs (lex-entry-full-fs word-entry) 
			  (format nil "~(~A~) - ~A - expanded" word-string
				  (lex-entry-id word-entry))
			  (lex-entry-id word-entry))
	    (display-unexpanded-lex-entry word-string word-entry
					  (lex-entry-id word-entry)))
	  ;; try to ensure that windows appear in chronological order
	  (sleep 0.2))))  

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


;;; 
;;; View utilities

(defparameter *last-type-name* '*top*)

(defun ask-user-for-type (&optional qstring show-all-types-spec check-box-spec)
  (let ((possible-name *last-type-name*)
        (prompt "")
        (res nil))
    (loop
      (setq res
	(ask-for-lisp-movable "Current Interaction" 
           (append
	       (list (cons (format nil "~a~%~a" prompt (or qstring "Type?"))
			   possible-name))
             (if show-all-types-spec (list show-all-types-spec) nil)
             (if check-box-spec (list check-box-spec) nil))
           nil
           (if (and possible-name
                 (member possible-name *type-names* :test #'eq))
               ;; default selection if types presented in a drop-down list
               (cons possible-name (remove possible-name *type-names* :test #'eq))
               nil)))
      (when (or (null res) (null (car res))) (return nil))
      (let ((type (car res))
            (check-1-p (cadr res))
	    (check-2-p (caddr res)))
	(eval-possible-leaf-type *leaf-types* type)
	(let ((type-entry (get-type-entry type)))
          (cond
            (type-entry
              (setq *last-type-name* type)
              (return (values type check-1-p check-2-p)))
            (t
              (setq prompt (format nil "~A is not defined." type)
                    possible-name type))))))))


(defparameter *last-lex-id* 'KIM)

(defun ask-user-for-lex ()
  (let ((prompt "")
        (possible-name
          (if (and *last-lex-id* (get-lex-entry-from-id *last-lex-id*))
            *last-lex-id*
            "")))
    (loop
      (setq possible-name
	(ask-for-lisp-movable "Current Interaction" 
			      `((,(format nil "~a~%Lex id?" prompt) . ,possible-name))
			      nil))
      (let* ((lex (car possible-name))
             (lex-entry (get-lex-entry-from-id lex)))
        (cond
          ((null lex) (return nil))
          (lex-entry
            (setq *last-lex-id* lex)
            (return (values lex lex-entry)))
          (t 
            (setq prompt (format nil "~A is not defined." lex)
                  possible-name lex)))))))


(defparameter *last-other-id* 'ROOT_STRICT)

(defun ask-user-for-other-id ()
  (let ((prompt "")
        (possible-name
          (if (and *last-other-id* (get-other-entry *last-other-id*))
            *last-other-id*
            "")))
    (loop
      (setq possible-name
	(ask-for-lisp-movable "Current Interaction" 
			      `((,(format nil "~a~%Entry id?" prompt) . ,possible-name))
			      nil))
      (let* ((id (car possible-name))
             (id-entry (get-other-entry id)))
        (cond
          ((null id) (return nil))
          (id-entry
            (setq *last-other-id* id)
            (return (values id id-entry)))
          (t 
            (setq prompt (format nil "~A is not defined." id)
                  possible-name id)))))))


(defparameter *last-rule-id* nil)

(defun ask-user-for-rule ()
  (let*
    ((prompt "")
     (rule-names (rules-sorted-on-name *rules*))
     (possible-name
       (cond
         ((and *last-rule-id* (get-grammar-rule-entry *last-rule-id*))
           *last-rule-id*)
         (rule-names ; first alphabetically that starts with character "S"
           (or (find #\S rule-names :key #'(lambda (name) (schar (string name) 0)))
               (car rule-names)))
         (t ""))))
    (loop
      (setq possible-name
	(ask-for-lisp-movable "Current Interaction" 
			      `((,(format nil "~a~%Rule?" prompt) . ,possible-name))
			      nil rule-names))
      (let* ((name (car possible-name))
             (rule-entry (get-grammar-rule-entry name)))
        (cond
          ((null name) (return nil))
          (rule-entry
            (setq *last-rule-id* name)
            (return rule-entry))
          (t 
            (setq prompt (format nil "~A is not defined." name)
                  possible-name name)))))))

(defun rules-sorted-on-name (ht)
  ;; return names sorted alphabetically
  (let ((names nil))
    (maphash #'(lambda (name value)
                 (declare (ignore value))
                 (push name names))
             ht)
    (sort names #'string-lessp)))


(defparameter *last-lex-rule-id* nil)

(defun ask-user-for-lexical-rule ()
  (let*
    ((prompt "")
     (rule-names (rules-sorted-on-name *lexical-rules*))
     (possible-name
       (cond
         ((and *last-lex-rule-id* (get-lex-rule-entry *last-lex-rule-id*))
            *last-lex-rule-id*)
         (rule-names ; first alphabetically that starts with character "V"
           (or (find #\V rule-names :key #'(lambda (name) (schar (string name) 0)))
               (car rule-names)))
         (t ""))))
    (loop
      (setq possible-name
	(ask-for-lisp-movable "Current Interaction" 
			      `((,(format nil "~a~%Lexical rule?" prompt) . ,possible-name))
			      nil rule-names))
      (let* ((name (car possible-name))
             (rule-entry (get-lex-rule-entry name)))
        (cond
          ((null name) (return nil))
          (rule-entry
            (setq *last-lex-rule-id* name)
            (return rule-entry))
          (t 
            (setq prompt (format nil "~A is not defined." name)
                  possible-name name)))))))


(defparameter *last-word* nil)

(defun ask-user-for-word (prompt)
   (let ((possible-word
           (or *last-word* 
               (let ((words (lex-words *lexicon*)))
                 (or (car (member "the" words :test #'equalp)) (car words))))))
      (setq possible-word
        (ask-for-strings-movable "Current interaction" 
                 `((,(format nil "~a~%Word?" prompt) . ,possible-word))
                 nil))
      (when possible-word
         (let* ((lex (car possible-word))
                (lex-string
                  (if (stringp lex)
                     lex
                     (format nil "~S" lex))))
            (when lex-string (setf *last-word* lex-string))
            lex-string))))


;;; Lexical rule application

(defun apply-lex (&optional id fs)
  (let* ((lex (or id (ask-user-for-lex)))
          (lex-entry-fs
            (cond (fs)
                  ((and lex (get-lex-entry-from-id lex))
                    (lex-entry-full-fs (get-lex-entry-from-id lex))))))
    (when lex-entry-fs 
      (let ((lex-rule (ask-user-for-lexical-rule)))
        (when lex-rule
          (let* (#+:clim (*standard-output* clim-user::*lkb-top-stream*)
                 (*unify-debug* :window) ; JAC - added at request of Berthold
                 (result
                   (apply-lex-interactive lex lex-entry-fs lex-rule)))
            (cond (result
                    (let ((id (format nil "~(~A~) + ~A" lex (rule-id lex-rule))))
                       (display-fs
                         result
                         (format nil "~A = ~(~A~)" id (extract-orth-from-fs result))
                         id)))
                  (t (lkb-beep)
                     (format #+:clim clim-user::*lkb-top-stream*
		             #-:clim t 
                             "~%Lexical rule application failed")))))))))

(defun apply-lex-rules (&optional id fs)
  (let* ((lex (or id (ask-user-for-lex)))
          (lex-entry-fs
            (cond (fs)
                  ((and lex (get-lex-entry-from-id lex))
                    (lex-entry-full-fs (get-lex-entry-from-id lex))))))
    (when lex-entry-fs 
      (apply-or-reapply-lex-rules lex lex-entry-fs))))
      
(defun apply-or-reapply-lex-rules (lex fs)
  (setf *number-of-applications* 0)
  (let* ((*maximal-lex-rule-applications* 
	   (if *lex-rule-show-one-step* 1
	       *maximal-lex-rule-applications*))
	 (result-list
	   (try-all-lexical-rules (list (cons nil fs)))))
    (if result-list
	(draw-active-list
	    (mapcar #'(lambda (rules-and-fs)
			(let* ((done
			         (format nil "~(~A~) ~{+ ~A~}" 
				         lex 
				         (reverse (car rules-and-fs))))
			       (done-and-str
			         (format nil "~A = ~(~A~)" 
				         done
				         (extract-orth-from-fs (cdr rules-and-fs)))))
			  (list* done-and-str done-and-str done (cdr rules-and-fs))))
		    result-list)
	    "Lexical rule results"
	    (list
	      (cons 
	        "Feature structure"
	        #'(lambda (display-res)
		    (display-fs (cddr display-res) (car display-res) (cadr display-res))))
              (cons 
	        "Apply lex rule..."
	        #'(lambda (display-res)
		    (apply-lex (cadr display-res) (cddr display-res))))
              (cons 
	        "Apply all lex rules"
	        #'(lambda (display-res)
		    (apply-or-reapply-lex-rules (cadr display-res) (cddr display-res))))))
	(progn
	   (lkb-beep)
	   (format #+:clim clim-user::*lkb-top-stream*
		   #-:clim t
		   "~%No applicable lexical rules")))))


;;; "Parse"
;;;
;;; "Parse Input" do-parse

(defparameter *last-parses* '("the dog barks"))

(defun do-parse nil
  (declare (special *last-generate-from-edge*))
  (let* ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("Sentence" . ,(cons :typein-menu *last-parses*))) 400)))
    (when sentence
      (setf *sentence* (car sentence))
      (close-existing-chart-windows)
      (setq *last-generate-from-edge* nil)
      (let ((str (string-trim '(#\space #\tab #\newline) (car sentence))))
        (setq *last-parses* (cons str (remove str *last-parses* :test #'equal)))
        (setq *last-parses*
          (subseq *last-parses* 0 (min (length *last-parses*) 12)))
        (parse
	  (split-into-words (preprocess-sentence-string str)))))))


;;; "Generate" generate-from-edge
;;; this is in mrstoplevel.lsp


;;; Unification checking

(defun interactive-unification-check nil
  ;;; I've made this just work on FSs since the default
  ;;; stuff won't fail anyway
  (let* ((check-details
           (ask-for-lisp-movable 
             "Check Unification" '(("fs1" . head-specifier-rule) 
                                 ("path1 in ()s (optional)" . (args first))
                                 ("fs2" . sleeps_1)
                                 ("path2 in ()s (optional)" . nil)
                                 ("name for result (optional)" . nil))))
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


;;
;; Interactively set parameters
;;

(defun get-parameters ()
  (setq *lkb-user-params* (sort *lkb-user-params* #'string<))
  (let* ((unreadable-params nil)
	 (p-v-pairs (mapcan #'(lambda (p)
			       ;; Skip things we won't be able to read back in (bmw)
			       ;; these skipped params are _lost_ when saving to file
			       ;; JAC: these skipped params are also not displayed
                               (handler-case
				 (list 
				   (cons (string-downcase (symbol-name p))
			             (let ((*print-readably* t))
				       (prin1-to-string (symbol-value p)) ; is it printable?
				       (symbol-value p))))
				 (print-not-readable ()
			           (push p unreadable-params)
			           nil)))
			 *lkb-user-params*))
	 (result (ask-for-lisp-movable "Set Options" p-v-pairs 780)))
    (when result
      (loop with res = result
            for p in *lkb-user-params*
            unless (member p unreadable-params)
	    do (setf (symbol-value p) (pop res)))
      (unless *user-params-file*
	(setf *user-params-file* 
	  (ask-user-for-new-pathname "File to save parameters?")))
      (when *user-params-file*
	(handler-case 
	  (with-open-file
		(ostream *user-params-file* :direction :output :if-exists :supersede)
	      (format ostream ";;; Automatically generated file - do not edit!~%")
              (loop with res = result
                    for p in *lkb-user-params*
                    unless (member p unreadable-params)
		    do
		    (let ((*package* (find-package "KEYWORD"))) ; so symbols get package prefix
		      (format ostream "(~S ~S '~S)~%" 'defparameter p (pop res)))))
	  (file-error (condition)
	    (format t "~%Parameters not saved to file ~A~%~A"
	    	      *user-params-file* condition)))))))
  
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
   (ask-user-for-existing-pathname "Type display settings file?")))


;;; debugging - finding maximal type

(defun find-type-from-features nil
  (let ((feature-list
         (ask-for-lisp-movable "Current Interaction" 
                               `(("Feature(s)" . (,*diff-list-list*)))
                               nil)))
    (when feature-list 
      (when (listp (car feature-list)) 
        (setf feature-list (car feature-list)))
      (let ((type (maximal-type-of-list feature-list)))
        (if type
            (format t "~&Maximal type for ~A is ~A~%" feature-list type)
            (let ((bogus-features (loop for f in feature-list
                                      unless (maximal-type-of f)
                                      collect f)))
              (if bogus-features
                  (format t "~&Features ~A not found in this grammar~%"
                          bogus-features)
                  (format t "~&Features ~A not mutually compatible~%" feature-list))))))))

;;; various fns moved to utils.lsp, because they are relevant in tty mode
