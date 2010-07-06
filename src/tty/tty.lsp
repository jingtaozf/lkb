;;; Copyright (c) 1991--2003
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;;
;;; Skeleton tty interface
;;; 
;;; Loading functions - all available as ...-aux (see script)
;;; now including read-script-file-aux
;;;
;;; View
;;;
;;;                     (make-menu-item :name "Type definition"
;;;                        :value #'show-type-spec)

(defun show-type-spec-tty (type)
  (eval-possible-leaf-type *leaf-types* type)
  (let* ((type-entry (if type (get-type-entry type))))
    (if type-entry
      (display-fs-and-parents-tty (ltype-local-constraint type-entry)
                                  (ltype-parents type-entry))
      (format t "~%Type ~A not found" type))))


;;;                     (make-menu-item :name "Expanded type"
;;;                        :value #'show-type)

(defun show-type-tty (type)
  (eval-possible-leaf-type *leaf-types* type)
  (let* ((type-entry (if type (get-type-entry type))))
    (if type-entry
      (display-fs-and-parents-tty (ltype-tdfs type-entry)
                                  (ltype-parents type-entry))
      (format t "~%Type ~A not found" type))))

;;;                     (make-menu-item :name "Lex entry"
;;;                        :value #'show-lex)

(defun show-lex-tty (lex)
  (let ((lex-entry (if lex (get-lex-entry-from-id lex))))
      (if lex-entry 
        (display-fs-tty (lex-entry-full-fs lex-entry))
        (format t "~%Entry ~A not found" lex))))

(defun show-other-tty (other-id)
  (let ((other-entry (if other-id (get-other-entry other-id))))
      (if other-entry 
        (display-fs-tty (psort-full-fs other-entry))
        (format t "~%Entry ~A not found" other-id))))

;;;                     (make-menu-item :name "Word definitions"
;;;                        :value #'show-word-defs)

(defun show-word-defs-tty (word-string)
  (show-word-aux-tty word-string nil))

;;;                     (make-menu-item :name "Word entries"
;;;                        :value #'show-words)

(defun show-words-tty (word-string)
  (show-word-aux-tty word-string t))

;;; show-word-aux-tty moved to ../io-general/utils.lsp 
;;; for encoding challenged users (FCB)

;;;                     (make-menu-item :name "Grammar rule"
;;;                        :value #'show-grammar-rule)

(defun show-grammar-rule-tty (name)
  (let* ((rule-entry  (get-grammar-rule-entry name)))
      (if rule-entry 
          (display-fs-tty (rule-full-fs rule-entry))
          (format t "~%No rule ~A" name))))

;;;                     (make-menu-item :name "Lexical rule"
;;;                       :value #'show-lex-rule)

(defun show-lex-rule-tty (name)
  (let* ((rule-entry (get-lex-rule-entry name)))
      (if rule-entry 
          (display-fs-tty (rule-full-fs rule-entry))
        (format t "~%No rule ~A" name))))


;;; (make-menu-item :name "Parse input"
;;;                        :value #'do-parse)

;;; do-parse-tty is in utils.lsp

;;; (make-menu-item :name "Print chart"
;;;                        :value #'print-chart)))
;;; print-chart should be OK

;;;                     (make-menu-item :name "Apply lexical rule"
;;;                        :value #'apply-lex)


(defun apply-lex-tty (lex lex-rule-name)
   (let* ((lex-rule (get-lex-rule-entry lex-rule-name))
         (lex-entry (if lex (get-lex-entry-from-id lex)))
         (lex-entry-fs
            (if lex-entry (lex-entry-full-fs lex-entry))))
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


(defun apply-lex-rules-tty (lex)
   (let* ((lex-entry (if lex (get-lex-entry-from-id lex)))
         (lex-entry-fs
            (if lex-entry (lex-entry-full-fs lex-entry))))
      (when lex-entry-fs 
         (setf *number-of-applications* 0)
         (let ((result-list
                  (try-all-lexical-rules 
                     (list (cons nil lex-entry-fs)))))
            (cond (result-list
                  (loop for result-pair in result-list
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


;;;
;;; _fix_me_
;;; no good need to have `-tty' version of this (and others); unify things one
;;; day, so that these functions dispatch appropriately.       (8-jun-03; oe)
;;;
(defun display-fs-tty (fs)
  (if #+:lui (lui-status-p :avm) #-:lui nil
    #+:lui (lui-display-fs fs nil nil) 
    (display-dag fs 'simple)))

#+:lui
(defun display-fs (fs title &optional id)
  (if #+:lui (lui-status-p :avm) #-:lui nil
    #+:lui (lui-display-fs fs title id) #-:lui nil
    (display-fs-tty fs)))

#+:lui
(defun show-chart ()
  (when (lui-status-p :chart)
    (lui-show-chart)))
  
(defun display-fs-and-paths-tty (fs paths)
  (display-fs-tty fs)
  (loop for path in paths
       do
       (format t "~&~S" path)))
           

(defun display-fs-and-parents-tty (fs parents)
  (when fs
   (display-fs-tty fs))
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

(defun close-existing-chart-windows nil
  nil)


(defun show-parse (&optional edges title)
  (declare (ignore title))
  (let ((edges (or edges *parse-record*)))
    (if edges
      (if #+:lui (lui-status-p :tree) #-:lui nil
        #+:lui (lui-show-parses edges *sentence*) #-:lui nil
        (loop 
            for edge in edges
            do
              (format t "~&Edge ~A P:" (edge-id edge))
              (pprint (parse-tree-structure edge))
            finally
              (let* ((symbol (when (find-package :mrs)
                               (find-symbol "OUTPUT-MRS-AFTER-PARSE" :mrs)))
                     (hook (when (and symbol (fboundp symbol))
                             (symbol-function symbol))))
                (when hook (funcall hook edges)))))
      (format t "~&No parses"))))


(defun display-type-in-tree (type &optional scroll-onlyp)
  (declare (ignore type scroll-onlyp))
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
             (if resname (store-temporary-psort-entry resname resdag))))
          (cond ((null fs1) 
                 (error  "~&~A is not a valid FS identifier" fs1-id))
                ((null fs2) 
                 (error  "~&~A is not a valid FS identifier" fs2-id))
                (t (error  "~%Paths are not lists")))))
      (error  "~&Need to specify both feature structures")))))

;; This macro takes care of synchonization problems in the CLIM version

(defmacro with-output-to-top ((&optional foo) &body body)
  (declare (ignore foo))
  `(progn
     ,@body
     (terpri)))

;;; should not be called in tty version anyway
;;; but added to avoid undefined function warnings

(defun show-message-window (message)
  (declare (ignore message))
  (error "Should not be called in tty version"))


