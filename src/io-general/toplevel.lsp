;;; Copyright Ann Copestake 1992-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Modified for YADU
;;;
;;; MCL port
;;; split old toplevel.lsp into this file which should be generic CL
;;; and topmenu.lsp which has the commands to create the actual menu

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: set-up-lkb-interaction  (extra menu items added)
;;;          apply-lex

;;; Top level functions
;;;
;;; "Load"
;;; all in various input files

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
         (display-fs-and-parents (type-local-constraint type-entry) 
            (format nil 
               "~(~A~)  - definition" 
               type)
            (type-parents type-entry)))))

;;; "Expanded type" show-type                  
(defun show-type nil
  (let* ((type (ask-user-for-type))
        (type-entry (if type (get-type-entry type))))
      (when type-entry 
         (display-type-in-tree type)
         (if (type-tdfs type-entry)
            (display-fs (type-tdfs type-entry) 
            (format nil 
               "~(~A~) - TDFS" 
               type))
            (format t "~%No tdfs for type ~A" type)))))

;;; "Type hierarchy" show-type-hierarchy
;;; "Word entries" show-words

(defun show-words nil
    (let* ((word-string (ask-user-for-word))
        (lex-entry (if word-string (get-lex-entry word-string))))
     (unless lex-entry
         (setf lex-entry (get-lex-entry (string-upcase word-string))))
      (for word-entry in lex-entry 
         do
         (display-fs (lex-or-psort-full-fs word-entry) 
               (format nil "~(~A~) - expanded" word-string))
            ))) 

(defun show-word-defs nil
   (let* ((word-string (ask-user-for-word))
         (lex-entry (if word-string (get-lex-entry word-string))))
      (unless lex-entry
         (setf lex-entry (get-lex-entry (string-upcase word-string))))
      (for word-entry in lex-entry 
         do
         (display-unexpanded-lex-entry word-string word-entry))))
  
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
(defparameter *last-type-name* 'cat)

(defun ask-user-for-type (&optional qstring check-box-spec)
   (let ((res
            (ask-for-lisp-movable "Current Interaction" 
               `((,(or qstring "Type?") . ,*last-type-name*)
                 ,@(if check-box-spec `(,check-box-spec)))
               150)))
      (when res
         (let* ((type (car res))
                (show-all-p (cadr res))
                (type-entry (get-type-entry type)))
            (unless type-entry
               (format t "~%Type ~A is not defined" type)
               (setf type (ask-user-for-type)))
            (setf *last-type-name* type)
            (values type show-all-p)))))


;;; display-fs is in outputfs.lsp

;;; included from lexbatch.lsp

(defun collect-psort-ids nil
   ;; work around Procyon maphash bug - as in checktypes.lsp
   (let ((ids nil))
      (maphash 
         #'(lambda (name val)
             (declare (ignore val))
            (push name ids))
         *psorts*)
      ids))


(defun ask-user-for-lex nil
   (let ((possible-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Lex-id?" . ,(or *last-lex-id*
                        (car (collect-psort-ids)))))
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
                        (car (collect-psort-ids)))))
                         150)))
      (when possible-name
         (let* ((lex (car possible-name))
               (lex-entry (get-psort-entry lex)))
            (unless lex-entry
               (format t "~%~A is not defined" lex)
               (setf lex (ask-user-for-lex)))
            (setf *last-lex-id* lex)
            lex))))

(defparameter *last-rule-id* 'r1)

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

(defparameter *last-lex-rule-id* 'plur-noun)

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
                        (car (collect-defined-word-strings))))) 150)))
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
                        (if 
                           ;; modification to check whether a particular 
                           ;; lexical rule is morphological - if so, then the 
                           ;; unification function is called with an extra 
                           ;; option value which describes the new 
                           ;; orthography of the result.
                           (spelling-change-rule-p lex-rule)
; need to reimplement with-fail-messages
;                           (evaluate-unifications-with-fail-messages lex-rule 
;                              (list (copy-tdfs-completely lex-entry-fs)) 
;                              (list lex)
;                              (car (mapcar #'car
;                                 (morph-generate 
;                                    (extract-orth-from-fs lex-entry-fs)
;                                    (rule-id lex-rule)))))
                           (evaluate-unifications lex-rule 
                              (list (copy-tdfs-completely lex-entry-fs)) 
                              (car (mapcar #'car 
                                      (morph-generate 
                                       (extract-orth-from-fs lex-entry-fs)
                                       (rule-id lex-rule)))))
;                           (evaluate-unifications-with-fail-messages lex-rule
;                              (list (copy-dag-completely lex-entry-fs)) 
;                              (list lex)))))
                           (evaluate-unifications lex-rule
                              (list (copy-tdfs-completely lex-entry-fs))))))
                  (cond (result
                        (display-fs result
                           (format nil "~(~A~) + ~A" 
                              lex (rule-id lex-rule))))
                     (t (format t 
                           "~%Lexical rule application failed")))))))))


(defparameter *number-of-applications* 0)

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
                  (for result-pair in result-list
                     do
                     (display-fs (cdr result-pair)
                        (format nil "~(~A~) ~{+ ~A~}" 
                           lex (reverse (car result-pair))))))
               (t (format t 
                     "~%No applicable lexical rules")))))))




;;; "Parse"
;;;
;;; "Parse Input" do-parse


(defparameter *last-parse* "it annoys Kim that she loves the books")


(defun do-parse nil
   (let ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("Sentence" . ,*last-parse*)) 400)))
      (when sentence
         (setf *last-parse* (car sentence))
         (parse (split-into-words 
                 (preprocess-sentence-string (car sentence)))))))     

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


;;; "Generate"
;;;
;;; "Generate" do-parse


(defparameter *last-generate-from-edge* 1)


(defun generate-from-edge nil
   (let ((possible-edge-name 
            (ask-for-lisp-movable "Current Interaction" 
               `(("Parser edge number for input MRS?" . ,*last-generate-from-edge*)) 60)))
      (when possible-edge-name
         (setf *last-generate-from-edge* (car possible-edge-name))
         (let ((parser-edge (find-edge-given-id (car possible-edge-name))))
            (when parser-edge
               (let ((input-sem
                        (car (mrs::extract-mrs (list parser-edge)))))
                  (if (mrs::psoa-liszt input-sem)
                     (progn
                        (generate-from-mrs input-sem)
                        (show-gen-result))
                     (format t "~%Could not extract any MRS relations from edge ~A"
                        (car possible-edge-name)))))))))


#|
;;; "Type file" output-type-file


(defun output-type-file nil
  (let ((output-file 
         (ask-user-for-new-pathname "Output types to?")))
    (with-open-file (ostream output-file :direction :output)
      (output-types *toptype* ostream)
      (unmark-type-table))))
         
(defun output-types (type ostream)
  (let ((type-record (get-type-entry type)))
    (unless (seen-node-p type-record) 
      (mark-node-seen type-record)
      (format ostream 
              "~%~(~A~) ~(~A~) " type (type-parents type-record))
      (when (type-comment type-record)
        (format ostream "~%~S" (type-comment type-record)))
      (if (type-enumerated-p type-record)
        (format ostream "~%(OR~{~( ~A~)~}).~%" (type-daughters type-record))
        (let ((local-constraint (type-local-constraint type-record)))
          (if (null local-constraint)
            (format ostream ".~%")
            (display-dag1 local-constraint
                          'path2 ostream))
          (for daughter in (type-daughters type-record)
               do
               (output-types daughter ostream)))))))
|#



;;; Unification checking

(defun interactive-unification-check nil
  ;;; I've made this just work on FSs since the default
  ;;; stuff won't fail anyway
  (let* ((check-details
          (ask-for-lisp-movable 
           "Check unification" '(("fs1" . plur-noun) 
                                 ("path1 in ()s (optional)" . (1))
                                 ("fs2" . book_1)
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
                    (copy-dag-completely fs2) fs1-id path1 fs2-id path2))
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


(defun less-interactive-unification-check (check-details)
  ;;; temporary, since dialog doesn't work in ACL
  (let* ((fs1-id (car check-details))
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
                    (copy-dag-completely fs2) fs1-id path1 fs2-id path2))
             (format t "~%Unification successful")
             (if resname (store-temporary-psort resname resdag))))
          (cond ((null fs1) 
                 (error  "~%~A is not a valid FS identifier" fs1-id))
                ((null fs2) 
                 (error  "~%~A is not a valid FS identifier" fs2-id))
                (t (error  "~%Paths are not lists")))))
      (error  "~%Need to specify both feature structures")))))


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
              (let ((type (get-type-entry fs-id)))
                (if type (tdfs-of fs-id))))))))))
