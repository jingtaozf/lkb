;;; Copyright Ann Copestake 1992-1998
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake

;;; generator output functions which are not dialect specific

(defun show-gen-result nil
   (if *gen-record*
      (for edge in *gen-record*
         do
         (display-parse-tree edge nil))
      (let ((possible-edge-name
               (ask-for-lisp-movable "Current Interaction" 
                  `(("No generation results - specify an edge number" . ,*edge-id*)) 60)))
         (when possible-edge-name
            (let* ((edge-id (car possible-edge-name))
                   (edge-record (find-gen-edge-given-id edge-id)))
               (when edge-record 
                  (display-parse-tree edge-record nil)))))))
            
(defun show-gen-edge nil
   (let ((possible-edge-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Specify an edge number" . ,*edge-id*)) 60)))
      (when possible-edge-name
         (let* ((edge-id (car possible-edge-name))
                (edge-record (find-gen-edge-given-id edge-id)))
            (when edge-record 
               (display-parse-tree edge-record t))))))


(defun find-gen-edge-given-id (edge-id)
   (dolist (entry *gen-chart*)
      (dolist (edge (cdr entry))
         (when (eql edge-id (edge-id edge))
            (return-from find-gen-edge-given-id edge)))))


;;; parse output functions which are not dialect specific

(defun show-parse nil
   (if *parse-record*
      (for edge in *parse-record*
         do
         (display-parse-tree edge nil))
      (let ((possible-edge-name
               (ask-for-lisp-movable "Current Interaction" 
                  `(("No parses - specify an edge number" . ,*edge-id*)) 60)))
         (when possible-edge-name
            (let* ((edge-id (car possible-edge-name))
                  (edge-record (find-edge-given-id edge-id)))
               (when edge-record 
                  (display-parse-tree edge-record nil)))))))
            
(defun show-parse-edge nil
   (let ((possible-edge-name
            (ask-for-lisp-movable "Current Interaction" 
               `(("Specify an edge number" . ,*edge-id*)) 60)))
      (when possible-edge-name
         (let* ((edge-id (car possible-edge-name))
               (edge-record (find-edge-given-id edge-id)))
            (when edge-record 
               (display-parse-tree edge-record t))))))

;;; display-parse-tree is dialect specific

(defun find-edge-given-id (edge-id)
  (or
   (dotimes (i *chart-limit*)
      (let ((chart-entry (aref *chart* i)))
         (if (chart-entry-p chart-entry) 
            (let ((edge 
                     (for config in (chart-entry-configurations chart-entry) 
                        car-filter
                        (let ((edge (chart-configuration-edge config)))
                           (if (eql edge-id (edge-id edge))
                              edge)))))
               (when edge (return edge))))))
   (find edge-id *morph-records* :key #'edge-id)))


;;; called from display-parse-tree

(defun make-edge-symbol (edge-id &optional morph-p)
   ;; create it uninterned so data put on to its property list doesn't hang
   ;; around after all other references to it have gone
   (make-symbol
      (if (stringp edge-id)
         edge-id
         (format nil "~A~A" (if morph-p 'medge 'edge) edge-id))))



(defun find-category-abb (fs)
  ;;; Two versions of this - one as in the original LKB and another 
  ;;; which is for page emulation.
  ;;; The LKB version is simple - it
  ;;; checks to see whether fs is subsumed by any of the
  ;;; special templates which are listed in *category-display-templates*
  ;;; and uses the template name as a symbol if it does
  ;;;
  ;;; The PAGE emulation version relies on unification
  ;;; a - the template's type is ignored 
  ;;; b - the unification is checked on a portion of the FS
  ;;;     reached by the *label-fs-path*
  ;;; c - there are two types of templates - label and meta
  ;;;     The label templates provide the first half of the node label
  ;;;     then the meta template is checked - if this is satisfied,
  ;;;     the path *recursive-path* is followed into the fs
  ;;;     and this is checked against the *local-path*
  ;;;     of the label nodes, and so on recursively
  ;;;     This gives nodes like S/NP

  ;;; Longer term, rules should be indexed by these categories.
  (if (eql *lkb-system-version* :page)
    (calculate-tdl-label fs)
    (dolist (tmpl *category-display-templates*)
      (let* ((tmpl-entry (get-psort-entry tmpl))
              (tmpl-fs (if tmpl-entry (tdfs-indef (lex-or-psort-full-fs tmpl-entry)))))
          (when (and tmpl-fs (dag-subsumes-p tmpl-fs (tdfs-indef fs)))
            (return tmpl))))))
        

;;; code after this point is for the PAGE version

; structures and globals

(defvar *label-display-templates* nil)
(defvar *meta-display-templates* nil)

(defstruct (label-template)
  label
  fs)

(defstruct (meta-template)
  prefix
  suffix
  fs)

;;; Initialisation stuff

(defun split-up-templates nil
  ; called when *category-display-templates*
  ; is set up by the code in tdllexinput.lsp
  (setf *label-display-templates* nil)
  (setf *meta-display-templates* nil)
  (for tmpl in *category-display-templates*
       do
       (let* ((tmpl-entry (get-psort-entry tmpl))
              (tmpl-fs (if tmpl-entry 
                           (tdfs-indef 
                                  (lex-or-psort-full-fs tmpl-entry)))))
         (if tmpl-fs 
           (if (label-template-fs-p tmpl-fs)
               (push (make-label-template
                      :fs tmpl-fs 
                      :label (get-string-path-value tmpl-fs *label-path*
                                                   tmpl))
                     *label-display-templates*)
             (push (make-meta-template
                    :fs tmpl-fs 
                    :prefix (get-string-path-value tmpl-fs *prefix-path*
                                                  tmpl)
                    :suffix (get-string-path-value tmpl-fs *suffix-path*
                                                  tmpl))
                   *meta-display-templates*))
           (format t "~%Warning: no valid fs for ~A" tmpl)))))


(defun label-template-fs-p (fs)
  (let ((type (type-of-fs fs)))
    (eql (if (listp type) (car type) type) 
         *label-template-type*)))

; extracting label string

(defun get-string-path-value (tmpl-fs path tmpl)
  ;;; it is an error for the structure not to have the 
  ;;; feature which has been declared to provide the label name
  ;;; and for this not to be a string
  ;;; If this occurs, a warning message is printed
  ;;; and the template name is used instead
  (if path
      (let* ((dag-found (existing-dag-at-end-of tmpl-fs path))
             (dag-value (if dag-found (type-of-fs dag-found)))
             (label (if (stringp (car dag-value)) 
                        (car dag-value))))
        (or label
            (progn 
              (format t "~%Warning: no ~A in ~A, template name used instead"
                      path tmpl)
              (string tmpl))))
    ""))


;;; Calculating a tree node label for a fs

(defun calculate-tdl-label (fs)
  (let ((fs-node (existing-dag-at-end-of (tdfs-indef fs) 
                                         *label-fs-path*)))
    (if fs-node
        (string-upcase
         (concatenate 'string 
                      (match-label fs-node)
                      (check-meta fs-node)))
      "UNK")))

; matching the label part

(defun match-label (fs)
   (if fs
      (dolist (tmpl *label-display-templates*)
         (when (template-match-p (label-template-fs tmpl) fs t)
            (return (label-template-label tmpl))))
      "?"))

; checking for slash etc

(defun check-meta (fs)
  (let ((meta-fs (existing-dag-at-end-of fs *recursive-path*)))
    (if (null meta-fs)
        ""
      (dolist (meta-tmpl *meta-display-templates*)
        (when (template-match-p (meta-template-fs meta-tmpl) fs nil)
          (return (concatenate 'string 
                               (meta-template-prefix meta-tmpl) 
                               (match-label (existing-dag-at-end-of 
                                             meta-fs
                                             *local-path*))                
                               (meta-template-suffix meta-tmpl))))))))


(defun template-match-p (tmpl-fs fs label-p)
  ;;; the test is whether all the `real' parts of the
  ;;; template fs (i.e. the bits apart from e.g. LABEL-NAME)
  ;;; unify with the node
  ;;; actually there's some spurious copying going on,
  ;;; but check this works first
  (for feat in (get-real-templ-feats tmpl-fs label-p)
       all-satisfy
       (let ((real-templ-fs (get-dag-value tmpl-fs feat))
             (sub-fs (get-dag-value fs feat)))
         (and sub-fs
              (unifiable-dags-p real-templ-fs sub-fs)))))          
  

(defun get-real-templ-feats (tmpl-fs label-p)
  (let ((feats (top-level-features-of tmpl-fs)))
    (set-difference feats (if label-p '(LABEL-NAME) 
                            '(META-PREFIX META-SUFFIX)))))

                              
;;; JAC added the following to parseout.lsp, but better here


(defun tree-node-text-string (x)
   (let ((full-string
           (typecase x
              (symbol (symbol-name x))
              (string x)
              (t (princ-to-string x)))))
      (if (> (length full-string) 30)
         (subseq full-string 0 30)
         full-string)))

