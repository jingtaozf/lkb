;;;
;;; Skeleton tty interface
;;; 
;;; Loading functions - all available as ...-aux (see script)
;;;
;;; View
;;;
;;;                     (make-menu-item :name "Type definition"
;;;                        :value #'show-type-spec)

(defun show-type-spec-tty (type)
  (let* ((type-entry (if type (get-type-entry type))))
    (when type-entry
      (display-fs-and-parents-tty (type-local-constraint type-entry)
                                  (type-parents type-entry)))))


;;;                     (make-menu-item :name "Expanded type"
;;;                        :value #'show-type)

(defun show-type-tty (type)
  (let* ((type-entry (if type (get-type-entry type))))
    (when type-entry
      (display-fs-and-parents-tty (type-constraint type-entry)
                                  (type-parents type-entry)))))

;;;                     (make-menu-item :name "Lex or psort definition"
;;;                        :value #'show-lex-def)

(defun show-lex-def-tty (lex)
   (let ((lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
         (display-unexpanded-lex-entry-tty lex lex-entry))))




(defun display-unexpanded-lex-entry-tty (lex lex-entry)
  (declare (ignore lex))
   (display-fs-and-paths-tty 
      (lex-or-psort-local-fs lex-entry) 
      (remove-if-not 
         #'(lambda (unif) 
            (or (c-identity-p unif)
               (equality-p unif)
               (inheritance-p unif)
               (default-inheritance-p unif)))
         (lex-or-psort-unifs lex-entry))))

;;;                     (make-menu-item :name "Lex or psort entry"
;;;                        :value #'show-lex)

(defun show-lex-tty (lex)
  (let ((lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
            (display-fs-tty (lex-or-psort-full-fs lex-entry)))))

;;;                     (make-menu-item :name "Word definitions"
;;;                        :value #'show-word-defs)

(defun show-word-defs-tty (word-string)
   (let ((lex-entry (if word-string (get-lex-entry word-string))))
      (unless lex-entry
         (setf lex-entry (get-lex-entry (string-upcase word-string))))
      (for word-entry in lex-entry 
         do
         (display-unexpanded-lex-entry-tty word-string word-entry))))

;;;                     (make-menu-item :name "Word entries"
;;;                        :value #'show-words)

(defun show-words-tty (word-string)
    (let ((lex-entry (if word-string (get-lex-entry word-string))))
     (unless lex-entry
         (setf lex-entry (get-lex-entry (string-upcase word-string))))
      (for word-entry in lex-entry 
         do
         (display-fs-tty (lex-or-psort-full-fs word-entry)))))

;;;                     (make-menu-item :name "Grammar rule"
;;;                        :value #'show-grammar-rule)

(defun show-grammar-rule-tty (name)
  (let* ((rule-entry  (get-grammar-rule-entry name)))
      (when rule-entry 
            (display-fs-tty (rule-full-fs rule-entry)))))

;;;                     (make-menu-item :name "Lexical rule"
;;;                       :value #'show-lex-rule)

(defun show-lex-rule-tty (name)
  (let* ((rule-entry (get-lex-rule-entry name)))
      (when rule-entry 
            (display-fs-tty (rule-full-fs rule-entry)))))


;;;                     (make-menu-item :name "Tlinks"
;;;                        :value #'show-tlinks)

(defun show-tlinks-tty (lex target-language) 
  (when lex
    (let ((tlinks
           (if target-language
             (find-tlinks-by-language lex target-language)
             (find-tlinks lex))))
      (cond 
       (tlinks
        (for tlink in tlinks
             do
             (display-fs-tty (tlink-full-fs tlink))))
       (t (format t 
                  "~%No appropriate tlink found"))))))

;;;                     (make-menu-item :name "Tlink rules"
;;;                        :value #'show-tlink-rules)))

(defun show-tlink-rules-tty nil 
   (for tlink-rule in (get-tlink-rules)
      do
      (display-fs-tty (tlink-rule-full-fs tlink-rule))))

;;; (make-menu-item :name "Parse input"
;;;                        :value #'do-parse)

(defun do-parse-tty (sentence)
  (when sentence
    (parse (split-into-words (preprocess-sentence-string sentence)))))

;;; (make-menu-item :name "Show chart"
;;;                        :value #'show-chart)))
;;; show-chart should be OK

;;;                     (make-menu-item :name "Apply lexical rule"
;;;                        :value #'apply-lex)

(defun apply-lex-tty (lex lex-rule-name &optional check)
   (let* ((lex-rule (get-lex-rule-entry lex-rule-name))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-full-fs lex-entry))))
      (when lex-entry-fs 
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
                           (evaluate-unifications lex-rule 
                              (list (copy-tdfs-completely lex-entry-fs)) 
                              (mapcar #'car 
                                      (full-morph-generate 
                                       (extract-orth-from-fs lex-entry-fs)
                                       (rule-id lex-rule))))
                           (evaluate-unifications lex-rule
                              (list (copy-tdfs-completely lex-entry-fs))))))
                  (cond (result
                        (display-fs-tty result)
                        (when check
                           (let* ((target-lex-entry 
                                    (if check 
                                       (get-psort-entry check)))
                                 (target-lex-entry-fs
                                    (if target-lex-entry 
                                       (copy-tdfs-completely 
                                          (lex-or-psort-full-fs 
                                             target-lex-entry)))))
                              (when target-lex-entry-fs
                                 (if (yadu target-lex-entry-fs 
                                       (copy-tdfs-completely result))
                                    (display-fs-tty target-lex-entry-fs)
                                    (format t 
                                       "~%Doesn't unify"))))))
                     (t (format t 
                           "~%Lexical rule application failed"))))))))


;;;                    (make-menu-item :name "Apply all lex rules"
;;;                       :value #'apply-lex-rules)

(defparameter *number-of-applications* 0)

(defun apply-lex-rules-tty (lex)
   (let* ((lex-entry (if lex (get-psort-entry lex)))
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
                     (display-fs-tty (cdr result-pair))))
               (t (format t 
                     "~%No applicable lexical rules")))))))



;;;                     (make-menu-item :name "Apply ordinary tlink"
;;;                       :value #'apply-tlink)

(defun apply-tlink-tty (lex target-language)
   (let* ((lex-entry (if lex (get-psort-entry lex)))
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
                  (display-fs-tty fs)))
               (t (format t 
                     "~%No appropriate tlink found")))))))


;;;                     (make-menu-item :name "Expand tlinks"
;;;                        :value #'create-new-tlinks)
;;; create-new-tlinks should work
 

;;;                     (make-menu-item :name "Resolve psort"
;;;                        :value #'do-resolve-psort)
;;;                     (make-menu-item :name "Parse"
;;;                        :value #'do-constraint-parse)
;;;                     (make-menu-item :name "Translate"
;;;                        :value #'do-translation)
;;;                     (make-menu-item :name "Translate psort"
;;;                        :value #'do-translate-psort)))


;;;            (make-menu-item :name "Index"
;;;               :value (open-top-level-menu
;;;                  (list
;;;                     (make-menu-item :name "Index & check"
;;;                        :value #'index-do-index)
;;;                     (make-menu-item :name "Load indices"
;;;                        :value #'read-indices)
;;;                     (make-menu-item :name "Save indices"
;;;                        :value #'save-indices)))

;;;            (make-menu-item :name "Tidy up"
;;;               :value #'clear-non-parents
;;;               :available-p nil) 



;;;                     (make-menu-item :name "Inherit display settings"
;;;                        :value #'inherit-display-settings)

                
;;;                     (make-menu-item :name "Dump system"
;;;                        :value #'dump-lkb)

;;;                     (make-menu-item :name "Display settings"
;;;                        :value #'output-display-settings)


;;;                     (make-menu-item :name "Lexicon file"
;;;                        :value #'output-lexicon-file 




(defun display-fs-tty (fs)
   (display-dag fs 'simple))


(defun display-fs-and-paths-tty (fs paths)
  (display-fs-tty fs)
  (for path in paths
       do
       (format t "~%~S" path)))
           

(defun display-fs-and-parents-tty (fs parents)
   (display-fs-tty fs)
   (format t "~S" parents))


;;; Functions below here are to replace functions in graphics files
;;; called from functions in core files


(defun draw-new-type-tree (type title something)
  (declare (ignore type title something))
  nil)

(defun create-type-hierarchy-tree nil
  (declare (ignore type))
  nil)

(defun close-existing-type-hierarchy-trees nil
  nil)

(defun show-parse nil
   (if *parse-record*
      (for edge in *parse-record*
         do
         (pprint (parse-tree-structure edge)))
      (format t "~%No parses")))

(defun display-type-in-tree (type)
  (declare (ignore type))
  nil)

(defun enable-type-interactions nil
  nil)

(defun disable-type-interactions nil
  nil)

(defun y-or-n-p-general (str)
  (y-or-n-p str))

(defun lkb-beep nil
;;; so that misc.lsp doesn't need to be loaded in the tty version
  nil)

;;; might be useful

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

