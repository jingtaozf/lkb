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
  (eval-possible-leaf-type type)
  (let* ((type-entry (if type (get-type-entry type))))
    (when type-entry
      (display-fs-and-parents-tty (type-local-constraint type-entry)
                                  (type-parents type-entry)))))


;;;                     (make-menu-item :name "Expanded type"
;;;                        :value #'show-type)

(defun show-type-tty (type)
  (eval-possible-leaf-type type)
  (let* ((type-entry (if type (get-type-entry type))))
    (when type-entry
      (display-fs-and-parents-tty (type-tdfs type-entry)
                                  (type-parents type-entry)))))

;;;                     (make-menu-item :name "Lex or psort definition"
;;;                        :value #'show-lex-def)

(defun show-lex-def-tty (lex)
   (let ((lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
         (display-unexpanded-lex-entry-tty lex lex-entry))))


(defun display-unexpanded-lex-entry-tty (lex lex-entry)
  (declare (ignore lex))
  (if (eql *lkb-system-version* :laurel)
      (display-fs-and-paths-tty
       (lex-or-psort-local-fs lex-entry) 
       (remove-if-not 
        #'(lambda (unif) 
            (or (c-identity-p unif)
                (equality-p unif)
                (inheritance-p unif)
                (default-inheritance-p unif)))
        (lex-or-psort-unifs lex-entry)))
    (display-fs-tty (lex-or-psort-local-fs lex-entry))))
                   
;;;                     (make-menu-item :name "Lex or psort entry"
;;;                        :value #'show-lex)

(defun show-lex-tty (lex)
  (let ((lex-entry (if lex (get-psort-entry lex))))
      (when lex-entry 
            (display-fs-tty (lex-or-psort-full-fs lex-entry)))))

;;;                     (make-menu-item :name "Word definitions"
;;;                        :value #'show-word-defs)

(defun show-word-defs-tty (word-string)
  (show-word-aux-tty word-string nil))

;;;                     (make-menu-item :name "Word entries"
;;;                        :value #'show-words)

(defun show-words-tty (word-string)
  (show-word-aux-tty word-string t))

(defun show-word-aux-tty (word-string exp-p)
  (let* ((orth-list (if word-string 
                      (split-into-words (string-upcase word-string))))
         (lex-entries (if orth-list (get-lex-entry (car orth-list)))))
      ; entries indexed by all elements
    (for word-entry in lex-entries
         do
         (when (equal (mapcar #'string-upcase (lex-or-psort-orth word-entry))
                    orth-list)
           (if exp-p
             (display-fs-tty (lex-or-psort-full-fs word-entry))
             (display-unexpanded-lex-entry-tty word-string word-entry))))))

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


;;; (make-menu-item :name "Parse input"
;;;                        :value #'do-parse)

(defun do-parse-tty (sentence)
  (when sentence
    (parse (split-into-words 
            (preprocess-sentence-string 
             (string-trim '(#\space #\tab #\newline) sentence))))))

;;; (make-menu-item :name "Print chart"
;;;                        :value #'print-chart)))
;;; print-chart should be OK

;;;                     (make-menu-item :name "Apply lexical rule"
;;;                        :value #'apply-lex)


;;; (make-menu-item :name "Generate..."
;;;                        :value 'generate-from-edge)

(defun do-generate-tty (&optional edge-name)
   (let ((possible-edge-name 
            (or edge-name *last-generate-from-edge* *edge-id*)))
      (when possible-edge-name
         (setq *last-generate-from-edge* edge-name)
         (let ((parser-edge (find-edge-given-id possible-edge-name)))
            (if parser-edge
               (let* ((input-sem
                       (car (mrs::extract-mrs (list parser-edge) t))))
   ;; t indicates that this is being run from the generator and that
   ;; the appropriate globals should be set
                  (if (mrs::psoa-liszt input-sem)
                     (progn
                        (format t "~&Generating from parser edge ~A" possible-edge-name)
                        (generate-from-mrs input-sem)
                        (show-gen-result))
                     (format t "~&Could not extract any MRS relations from edge ~A"
                        possible-edge-name)))
               (format t "~&No parser edge ~A" possible-edge-name))))))


(defun apply-lex-tty (lex lex-rule-name)
   (let* ((lex-rule (get-lex-rule-entry lex-rule-name))
         (lex-entry (if lex (get-psort-entry lex)))
         (lex-entry-fs
            (if lex-entry (lex-or-psort-full-fs lex-entry))))
      (when lex-entry-fs 
            (when lex-rule
               (let 
                  ((result (apply-lex-interactive lex lex-entry-fs lex-rule)))
                 (cond (result
                        (display-fs-tty result))
                       (t (format t 
                                  "~&Lexical rule application failed"))))))))


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
                     "~&No applicable lexical rules")))))))


;;;                     (make-menu-item :name "Parse"
;;;                        :value #'do-constraint-parse)

;;;            (make-menu-item :name "Tidy up"
;;;               :value #'clear-non-parents
;;;               :available-p nil) 


                
;;;                     (make-menu-item :name "Dump system"
;;;                        :value #'dump-lkb)


(defun display-fs-tty (fs)
   (display-dag fs 'simple))


(defun display-fs-and-paths-tty (fs paths)
  (display-fs-tty fs)
  (for path in paths
       do
       (format t "~&~S" path)))
           

(defun display-fs-and-parents-tty (fs parents)
   (display-fs-tty fs)
   (format t "~S" parents))


;;; Functions below here are to replace functions in graphics files
;;; called from functions in core files


(defun draw-new-type-tree (node title horizontalp existing show-all-p)
  (declare (ignore node title horizontalp existing show-all-p))
  nil)

(defun create-type-hierarchy-tree (&optional (type *toptype*) old-window show-all-p)
  (declare (ignore type old-window show-all-p))
  nil)

(defun close-existing-type-hierarchy-trees nil
  nil)


(defun show-parse nil
   (if *parse-record*
      (for edge in *parse-record*
         do
         (format t "~&Edge ~A P:" (edge-id edge))
         (pprint (parse-tree-structure edge)))
      (format t "~&No parses")))

(defun show-gen-result nil
   (if *gen-record*
      (for edge in *gen-record*
         do
         (format t "~&Edge ~A G:" (edge-id edge))
         (pprint (parse-tree-structure edge)))
      (format t "~&No strings generated")))


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
                    fs2 
                    fs1-id path1 fs2-id path2))
             (format t "~&Unification successful")
             (if resname (store-temporary-psort resname resdag))))
          (cond ((null fs1) 
                 (error  "~&~A is not a valid FS identifier" fs1-id))
                ((null fs2) 
                 (error  "~&~A is not a valid FS identifier" fs2-id))
                (t (error  "~%Paths are not lists")))))
      (error  "~&Need to specify both feature structures")))))

