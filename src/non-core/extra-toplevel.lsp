;;; Copyright Ann Copestake 1992-1998 All Rights Reserved.
;;; No use or redistribution without permission.

;;; toplevel functions that used to be in toplevel.lsp but are not
;;; available in the core lkb

;;; "Unlinked entry" 'show-unlinked-lex
;;; only relevant for BC96 style lrules and linking
         
(defun show-unlinked-lex nil
  (let* ((lex (ask-user-for-lex))
        (lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
            (display-fs (lex-or-psort-interim-fs lex-entry) 
                     (format nil "~(~A~) - unlinked" lex)))))

;;; "Expanded type" show-type                  
(defun show-type-indef nil
  (let* ((type (ask-user-for-type))
        (type-entry (if type (get-type-entry type))))
      (when type-entry 
         (display-type-in-tree type)
         (if (type-constraint type-entry)
            (display-fs (type-constraint type-entry) 
            (format nil 
               "~(~A~) - TDFS" 
               type))
            (format t "~%No constraint for type ~A" type)))))

(defun show-bc96lex-rule nil
  (let* ((rule-entry (ask-user-for-lexical-rule)))
      (when rule-entry 
            (display-lrule-window 
               (lrule-input-tdfs rule-entry) 
               (lrule-output-tdfs rule-entry)
               (format nil "~(~A~)" (lrule-name rule-entry))))))

(defun show-tlinks nil 
   (let* ((*last-lex-id* (or *last-lex-id* 'love_1))
         (lex (ask-user-for-lex)))
      (when lex
         (let
         ((target-language 
            (if (lkb-y-or-n-p "Specify target language")
            (apply #'ask-user-for-multiple-choice "Translate to"
               *possible-languages*))))
         (let ((tlinks
            (if target-language
               (find-tlinks-by-language lex target-language)
               (find-tlinks lex))))
            (cond 
               (tlinks
                  (for tlink in tlinks
                     do
                  (display-fs (tlink-full-fs tlink)
                     (format nil "~A ~A ~A ~A" 
                        (tlink-psort1 tlink)
                        (tlink-lang1 tlink)
                        (tlink-psort2 tlink)
                        (tlink-lang2 tlink)))))
               (t (format t 
                     "~%No appropriate tlink found"))))))))

(defun show-tlink-rules nil 
   (for tlink-rule in (get-tlink-rules)
      do
      (display-fs (tlink-rule-full-fs tlink-rule)
         (format nil "~A ~A" 
            (tlink-rule-rule1 tlink-rule)
            (tlink-rule-rule2 tlink-rule)))))
 
;;; "Inherit display settings" inherit-display-settings

(defun inherit-display-settings nil
   (let ((type (ask-user-for-type)))
      (when type
         (inherit-display type nil))))
         
(defun inherit-display (type current-paths)
   (let* ((type-record (get-type-entry type))
         (shrunk-paths 
            (union
               (display-dag1 (type-constraint type-record) 'shrunk t)
               current-paths :test #'equal)))
      (when shrunk-paths
         (for path in shrunk-paths
            do
            (set-dag-display-value 
               (type-constraint type-record)
               path :shrink)))
      (unless (type-enumerated-p type-record)
         (for daughter in (type-daughters type-record)
            do
            (inherit-display daughter shrunk-paths)))))

(defun apply-bc96-lex nil
   (let* ((lex (ask-user-for-lex))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-interim-fs lex-entry))))
      (when lex-entry-fs 
         (let 
            ((lex-rule (ask-user-for-lexical-rule)))
            (when lex-rule
               (let 
                  ((result
                     (link-and-incorp
                        (apply-lrule lex-rule lex-entry-fs)
                        (lrule-name lex-rule))))
                  (cond (result
                        (display-fs result
                           (format nil "~(~A~) + ~A" 
                              lex (lrule-name lex-rule))))
                     (t (format t 
                           "~%Lexical rule application failed")))))))))

(defun apply-bc96-lex-rules nil
   (let* ((lex (ask-user-for-lex))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-interim-fs lex-entry))))
      (when lex-entry-fs 
         (setf *number-of-applications* 0)
         (let ((result-list
                  (apply-all-lexical-rules 
                     (list lex-entry-fs))))
            (cond (result-list
                  (for result in result-list
                     do
                     (display-fs result
                        (format nil "~(~A~)" 
                           lex))))
               (t (format t 
                     "~%No applicable lexical rules")))))))

;;; tlink application

(defun apply-tlink nil
   (let* ((lex (ask-user-for-lex))
         (target-language 
            (if lex
            (apply #'ask-user-for-multiple-choice "Translate to"
               *possible-languages*)))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-full-fs lex-entry))))
      (when lex-entry-fs 
         (let ((result
            (translate-feature-structure-temp
                     lex-entry-fs lex target-language)))
            (cond 
               (result
                  (for fs in result
                     do
                  (display-fs fs
                     (format nil "~(~A~) in ~A" 
                        lex target-language))))
               (t (format t 
                     "~%No appropriate tlink found")))))))

;;; "Map"
;;;
;;; "Define languages"       map-define-language     mapping.lsp
;;; "Select source entries"  select-psorts-to-link   mapping.lsp
;;; "Select Bilingual"       map-select-bilingual    mapping.lsp
;;; "Perform mapping"        map-do-map              mapping.lsp


;;; "Index"
;;;
;;; "Index & check"    index-do-index     lexbatch.lsp
;;; "Load indices"     read-indices       lexbatch.lsp
;;; "Save indices"     save-indices       lexbatch.lsp


;;; "Clear"            clear-non-parents  lex.lsp



;;; "Output"
;;;

;;; "Compiled type file" output-compiled-type-file  typeinput.lsp
;;; "Display settings"

#|

(defun output-display-settings nil
   (let ((output-file 
            (ask-user-for-new-pathname "Output type display settings to?")))
      (with-open-file (ostream output-file :direction :output)
         (output-type-display *toptype* ostream)
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
|#


;;; "Lexicon file" output-lexicon-file 
(defun output-lexicon-file nil
   (let ((output-file 
            (ask-user-for-new-pathname 
               "Output machine readable lexicon file to?")))
      (with-open-file (ostream output-file :direction :output)
            (output-quick-lexicon ostream))))
   
;;; Constraints
;;;             (make-menu-item :name "Constraints"
;;;                :value (open-top-level-menu
;;;                   (list
;;;                      (make-menu-item :name "Resolve psort"
;;;                         :value #'do-resolve-psort)
;;;                      (make-menu-item :name "Parse"
;;;                         :value #'do-constraint-parse)
;;;                      (make-menu-item :name "Translate"
;;;                         :value #'do-translation)))
;;;                :available-p nil)

(defun do-resolve-psort nil
  (let* ((lex (ask-user-for-psort))
        (lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
            (resolve-psort lex))))
   
(defun do-constraint-parse nil
   (let* ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("Sentence" . ,*last-parse*)) 400))
         (language
                  (apply #'ask-user-for-multiple-choice 
                     "Language" 
                     (cons *current-language*
                        (remove *current-language*
                           *possible-languages*)))))
      (when sentence
         (setf *last-parse* (car sentence))                        
         (constraint-parse (split-into-words (car sentence)) language))))

(defun do-translation nil
   (let* ((sentence 
            (ask-for-strings-movable "Current Interaction" 
               `(("SL string" . ,*last-parse*)) 400))
         (source-language
                  (apply #'ask-user-for-multiple-choice 
                     "Source language" 
                     (cons *current-language*
                        (remove *current-language*
                           *possible-languages*)))))
      (when sentence
         (setf *last-parse* (car sentence))
         (let ((target-language
                  (apply #'ask-user-for-multiple-choice 
                     "Target language"
                        (remove source-language
                           *possible-languages*)))) 
            (set-type-of-signs)                
            (translate (split-into-words (car sentence)) source-language
               target-language)))))

(defun do-translate-psort nil   
   (let* ((psort (ask-user-for-psort))
         (psort-entry (if psort (get-psort-value psort))))
      (when psort-entry 
         (let* ((source-language
                  (apply #'ask-user-for-multiple-choice 
                     "Source language" 
                     (cons *current-language*
                        (remove *current-language*
                           *possible-languages*))))
               (target-language
                  (apply #'ask-user-for-multiple-choice 
                     "Target language"
                     (remove source-language
                        *possible-languages*))))
            (set-type-of-osign)
            (translate-psort psort-entry source-language
               target-language)))))
   

(defun set-type-of-signs nil
   (when
      (lkb-y-or-n-p "Set input and output types?")
      (let ((itype (ask-user-for-type "Source sign type?")))
         (if itype
            (setf *type-of-isign* itype)))
      (let ((otype (ask-user-for-type "Target sign type?")))
         (if otype
            (setf *type-of-osign* otype)))))


(defun set-type-of-osign nil
   (when
      (lkb-y-or-n-p "Set output type?")
      (let ((otype (ask-user-for-type "Target sign type?")))
         (if otype
            (setf *type-of-osign* otype)))))

