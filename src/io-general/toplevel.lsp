;;; Copyright Ann Copestake 1992-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :cl-user)

;;; Modified for YADU
;;;
;;; MCL port
;;; split old toplevel.lsp into this file which should be generic CL
;;; and topmenu.lsp which has the commands to create the actual menu

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: set-up-lkb-interaction  (extra menu items added)
;;;          apply-lex

(defvar *type-fs-display* nil)


;;; Top level functions
;;;
;;; "Load"
;;; all in various input files
;;; apart from

(defun read-script-file nil
  (let ((file-name 
         (ask-user-for-existing-pathname "Script file?")))
    (read-script-file-aux file-name)))

;;; "View"
;;;
;;; "Type hierarchy" show-type-tree
(defun show-type-tree nil
   (let ((*last-type-name* *toptype*))
      (declare (special *last-type-name*))
      (multiple-value-bind (type show-all-p)
             (ask-user-for-type nil '("Show all types?" . :check-box))
         (when type
            (let ((type-entry (get-type-entry type)))
               (when type-entry 
                  (create-type-hierarchy-tree type nil show-all-p)))))))


;;; "Type spec" show-type-spec
(defun show-type-spec nil
   (let* ((type (ask-user-for-type))
         (type-entry (if type (get-type-entry type))))
      (when type-entry 
         (display-type-in-tree type)
         (show-type-spec-aux type type-entry))))

(defun show-type-spec-aux (type type-entry)
   (let ((*type-fs-display* t))
     (display-fs-and-parents 
      (type-local-constraint type-entry) 
      (format nil "~(~A~) - definition" type)
      (for parent in (type-parents type-entry)
           append
           (let ((parent-entry (get-type-entry parent)))
             (if (type-glbp parent-entry)
                 (list parent (remove-duplicates (get-real-types parent)))
               (list parent)))))))
           
;;; "Expanded type" show-type                  
(defun show-type nil
  (let* ((type (ask-user-for-type))
        (type-entry (if type (get-type-entry type))))
      (when type-entry 
         (display-type-in-tree type)
         (show-type-aux type type-entry))))

(defun show-type-aux (type type-entry)
   (if (type-tdfs type-entry)
      (let ((*type-fs-display* t))
         (display-fs (type-tdfs type-entry) 
            (format nil "~(~A~) - expanded" type)))
      (format t "~%No tdfs for type ~A" type)))


;;; "Type hierarchy" show-type-hierarchy
;;; "Word entries" show-words

(defun show-words nil
  (show-word-aux t))

(defun show-word-defs nil
  (show-word-aux nil))

(defun show-word-aux (exp-p)
  (let* ((word-string (ask-user-for-word))
         (orth-list (if word-string 
                      (split-into-words (string-upcase word-string))))
         (lex-entries (if orth-list (get-lex-entry (car orth-list)))))
      ; entries indexed by all elements
    (for word-entry in lex-entries
         do
         (when (equal (mapcar #'string-upcase (lex-or-psort-orth word-entry))
                    orth-list)
           (if exp-p
             (display-fs (lex-or-psort-full-fs word-entry) 
                         (format nil "~(~A~) - expanded" word-string))
             (display-unexpanded-lex-entry word-string word-entry))))))

  
;;; "Lex or psort entry" show-lex
(defun show-lex nil
  (let* ((lex (ask-user-for-lex))
        (lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
            (display-fs (lex-or-psort-full-fs lex-entry) 
                     (format nil "~(~A~) - expanded" lex)))))
         

(defun show-lex-def nil
   (let* ((lex (ask-user-for-lex))
         (lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
         (display-unexpanded-lex-entry lex lex-entry))))

(defun display-unexpanded-lex-entry (lex lex-entry)
  (if (eql *lkb-system-version* :laurel)
   (display-fs-and-paths 
      (lex-or-psort-local-fs lex-entry) 
      (format nil "~(~A~) - definition" lex)
      (remove-if-not 
         #'(lambda (unif) 
            (or (c-identity-p unif)
               (equality-p unif)
               (inheritance-p unif)
               (default-inheritance-p unif)))
         (lex-or-psort-unifs lex-entry)))
   (display-fs (lex-or-psort-local-fs lex-entry) 
                     (format nil "~(~A~) - definition (indef)" lex))))

         
(defun show-grammar-rule nil
  (let* ((rule-entry (ask-user-for-rule)))
      (when rule-entry 
            (display-fs (rule-full-fs rule-entry) 
                     (format nil "~(~A~)" (rule-id rule-entry))))))

(defun show-lex-rule nil
  (let* ((rule-entry (ask-user-for-lexical-rule)))
      (when rule-entry 
            (display-fs (rule-full-fs rule-entry) 
                     (format nil "~(~A~)" (rule-id rule-entry)))))) 


(defparameter *last-lex-id* nil)
        

        
;;; 
;;; View utilities
(defparameter *last-type-name* 'synsem-struc)

(defun ask-user-for-type (&optional qstring check-box-spec)
   (let ((res
            (ask-for-lisp-movable "Current Interaction" 
               `((,(or qstring "Type?") . ,*last-type-name*)
                 ,@(if check-box-spec `(,check-box-spec)))
               150)))
      (when res
        (let ((type (car res))
              (show-all-p (cadr res)))
          (eval-possible-leaf-type type)
          (let ((type-entry (get-type-entry type)))
            (unless type-entry
               (format t "~%Type ~A is not defined" type)
               (setf type (ask-user-for-type)))
            (setf *last-type-name* type)
            (values type show-all-p))))))


;;; display-fs is in outputfs.lsp

(defun ask-user-for-lex nil
   (let ((possible-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Lex-id?" . ,(or *last-lex-id*
                        (car (collect-psort-ids *lexicon*)))))
                         150)))
      (when possible-name
         (let* ((lex (car possible-name))
               (lex-entry (get-psort-entry lex)))
            (unless lex-entry
               (format t "~%~A is not defined" lex)
               (setf lex (ask-user-for-lex)))
            (setf *last-lex-id* lex)
            lex))))

(defun ask-user-for-psort nil
   (let ((possible-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Psort-id?" . ,(or *last-lex-id*
                        (car (collect-psort-ids *lexicon*)))))
                         150)))
      (when possible-name
         (let* ((lex (car possible-name))
               (lex-entry (get-psort-entry lex)))
            (unless lex-entry
               (format t "~%~A is not defined" lex)
               (setf lex (ask-user-for-lex)))
            (setf *last-lex-id* lex)
            lex))))

(defparameter *last-rule-id* 'head-specifier-rule)

(defun ask-user-for-rule nil
   (let ((possible-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Rule name" . ,*last-rule-id*)) 150)))
      (when possible-name
         (let* ((name (car possible-name))
               (rule-entry (get-grammar-rule-entry name)))
            (unless rule-entry
               (format t "~%~A is not defined" name)
               (setf rule-entry (ask-user-for-rule)))
            (setf *last-rule-id* name)
            rule-entry))))

(defparameter *last-lex-rule-id* '3rd-sing-verb_infl_rule)

(defun ask-user-for-lexical-rule nil
   (let ((possible-rule-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Lexical Rule?" . ,*last-lex-rule-id*)) 150)))
      (when possible-rule-name
         (let* ((name (car possible-rule-name))
               (rule-entry (get-lex-rule-entry name)))
            (unless rule-entry 
               (format t "~%Lexical rule ~A is not defined" name)
               (setf rule-entry (ask-user-for-lexical-rule)))
            (setf *last-lex-rule-id* name)
            rule-entry))))

(defparameter *last-word* nil)

(defun ask-user-for-word nil
   (let ((possible-name
            (ask-for-strings-movable "Current Interaction" 
               `(("word" . 
                     ,(or *last-word* 
                        (car (lex-words *lexicon*))))) 150)))
      (when possible-name
         (let* ((lex (car possible-name))
               (lex-string
                  (if (stringp lex)
                     lex
                     (format nil "~S" lex))))
            (setf *last-word* lex-string)
            lex-string))))


;;; Lexical rule application


(defun apply-lex nil
   (let* ((lex (ask-user-for-lex))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-full-fs lex-entry))))
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
                              lex (rule-id lex-rule))))
                     (t (format t 
                           "~%Lexical rule application failed")))))))))

(defun apply-lex-rules nil
   (let* ((lex (ask-user-for-lex))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-full-fs lex-entry))))
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


(defparameter *last-parses* '("Kim sleeps"))


(defun do-parse nil
  (let* ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("Sentence" . ,(cons :typein-menu *last-parses*))) 400)))
    (when sentence
      (close-existing-chart-windows)
      (let ((str (string-trim '(#\space #\tab #\newline) (car sentence))))
        (setq *last-parses* 
          (butlast
           (cons str (remove str *last-parses* :test #'equal))
           (max 0 (- (length *last-parses*) 12))))
                                        ; limit number of sentences retained
        (parse
         (split-into-words 
          (preprocess-sentence-string str)))))))


;;; "Generate"
;;;
;;; "Generate" generate-from-edge


(defparameter *last-generate-from-edge* nil)


(defun generate-from-edge nil
   (let ((possible-edge-name 
            (ask-for-lisp-movable "Current Interaction" 
               `(("Parser edge number for input MRS?" .
                    ,(or *last-generate-from-edge* *edge-id*))) 60)))
      (when possible-edge-name
         (setf *last-generate-from-edge* (car possible-edge-name))
         (let ((parser-edge (find-edge-given-id (car possible-edge-name))))
            (if parser-edge
               (let* ((input-sem
                       (car (mrs::extract-mrs (list parser-edge) t))))
   ;; t indicates that this is being run from the generator and that
   ;; the appropriate globals should be set
                  (if (mrs::psoa-liszt input-sem)
                     (progn
                        (close-existing-chart-windows)
                        (generate-from-mrs input-sem)
                        (show-gen-result))
                     (format t "~%Could not extract any MRS relations from edge ~A"
                        (car possible-edge-name))))
               (format t "~%No parser edge ~A"
                        (car possible-edge-name)))))))




;;; Unification checking

(defun interactive-unification-check nil
  ;;; I've made this just work on FSs since the default
  ;;; stuff won't fail anyway
  (let* ((check-details
          (ask-for-lisp-movable 
           "Check unification" '(("fs1" . head-specifier-rule) 
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
             (if resname (store-temporary-psort resname resdag))))
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


#-allegro
(defun compare-parses ()
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
				  (cons (string p) 
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
      (with-open-file
          (ostream *user-params-file* :direction :output
           :if-exists :supersede)
        (format ostream ";;; Automatically generated file - do not edit!")
        (loop for p in params
            for r in result
            do
              (format ostream "~%(defparameter ~A ~A)"
                      (read-from-string (car p))
                      (read-from-string r)))))))

;;
;; Save and load shrunk paths in display settings file
;;

(defvar *display-settings-file* nil)

(defun output-display-settings nil
  (let ((filename
         (or *display-settings-file*
             (ask-user-for-new-pathname 
              "Save type display settings to?"))))
    (when filename
      (unmark-type-table)
      (with-open-file (stream filename :direction :output 
                       :if-exists :supersede)
        (output-type-display *toptype* stream))
      (unmark-type-table))))

(defun output-type-display (type ostream)
   (let ((type-record (get-type-entry type)))
      (unless (seen-node-p type-record) 
         (mark-node-seen type-record)
         (let ((shrunk-paths 
                  (display-dag1 (type-constraint type-record) 'shrunk t)))
            ;; doesn't print anything!
            (when shrunk-paths
               (format ostream 
                  "~%(~A ~%~{~A~%~})" type shrunk-paths)))
         (unless (type-enumerated-p type-record)
            (for daughter in (type-daughters type-record)
               do
               (output-type-display daughter ostream))))))

(defun load-display-settings nil
   (let ((filename (ask-user-for-existing-pathname 
                    "Load type display settings from?")))
     (when filename
       (setf *display-settings-file* filename)
       (set-up-display-settings filename))))
