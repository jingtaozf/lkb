;;; General purpose routines for outputting
;;; types, lexicon etc, in various formats.
;;; Takes functions from the old tdloutput and lilout files

(in-package :lkb)

#|
;;; all take optional second argument for file name
(output-types :lilfes "Macintosh HD:foo")
;;;
;;; both do all output types including glbtypes
;;; all the options sort the types so that no type
;;; is used before it is defined since PAGE and LILFES
;;; require this
;;; 


;;; by default, the expanded information is output
;;; optional argument allows for local constraints only
(output-lex :lilfes "Macintosh HD:foo1")
(output-grules :lilfes "Macintosh HD:foo2")
(output-lrules :tdl)

(output-lex-and-derived :lilfes "Macintosh HD:foo3")
(output-lex-and-derived :tdl "Macintosh HD:foo4")

(output-lex-and-derived :ebl "~aac/ebl.lex")
;;; options for syntax are :lilfes :tdl
;;; and :path
|#



(defun output-types (syntax &optional file-name)
  (unless (member syntax '(:tdl :path :lilfes))
    (error "Unsupported syntax specifier ~A" 
           syntax))
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (loop for type-name in (sort-by-appearance-order
                         (copy-list
                          ; remove unaccessed leaf types
                          (remove-if-not 
                           #'(lambda (x) (get-type-entry x))
                           (append *ordered-type-list*
                                   *ordered-glbtype-list*))))
           do
           (let ((entry (get-type-entry type-name)))                  
             (ecase syntax
               (:tdl (output-type-as-tdl type-name entry
                                         ostream))
               (:path (output-type-as-paths type-name entry
                                         ostream))
               (:lilfes (output-type-as-lilfes type-name entry
                                               ostream))))))))


;;; Neither of these lexical output functions
;;; will work from a cached lexicon

(defun output-lex (syntax &optional file-name local-p)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (let ((count 0))
        (loop for lex-name in (reverse *ordered-lex-list*)
             do
             (if (> count 100)
               (progn (clear-expanded-lex)
                      (setf count 0))
               (incf count))
             (let ((entry (get-psort-entry lex-name)))
               (if entry
                   (case syntax
                     (:tdl (output-instance-as-tdl lex-name entry
                                                   ostream local-p))
                     (:lilfes 
                      (when local-p
                        (error "Local only output not supported with LiLFeS"))
                      (output-instance-as-lilfes 
                       lex-name entry
                       ostream))
                     (t (error "Unsupported syntax specifier ~A"
                               syntax)))
                 (format t "~%Warning ~A not found" lex-name))))))))

#|
(defun output-lex-and-derived (syntax &optional file-name ids-used)
  ;;; lexicon and everything that can be derived from it
  ;;; via lexical rule.  Ordered by base form.
  (unless file-name 
    (setf file-name
      (ask-user-for-new-pathname "Output file?")))
  (if (eq syntax :pet)
    (if ids-used
      (output-lexicon-for-pet file-name ids-used)
      (output-lexicon-for-pet file-name))
    (when file-name 
      (with-open-file 
          (ostream file-name :direction :output :if-exists :supersede)
        (let ((count 0)
	      (eblstream (when (eq syntax :ebl)
                         (open (concatenate 'string file-name ".lextypes")
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create))))
          (unless (or ids-used *ordered-lex-list*)
            (cerror "Continue without lexicon" 
                    "No lexicon list - lexicon must be read in from scratch"))
          (loop for lex-name in (or ids-used (reverse *ordered-lex-list*))
               do            
               (if (> count 100)
                 (progn (clear-expanded-lex)
                        (setf count 0))
                 (incf count))
               (setf *number-of-applications* 0)
               (let* ((lex-entry (get-psort-entry lex-name))
                      (lex-entry-fs 
                       (if lex-entry
                         (lex-or-psort-full-fs lex-entry)
                         (error "Entry for ~A not found" lex-name)))
                      (stem (lex-or-psort-orth lex-entry))
                      (result-list
                       (cons (cons nil lex-entry-fs)
                             (try-all-lexical-rules 
                              (list (cons nil lex-entry-fs)) 
                              nil)))
                      (idno 0))
                 (loop for result-pair in result-list
                      do
                      (let* ((derivation 
                              (append (first result-pair) (list lex-name)))
                             (id (format nil "~(~a~)_~d" lex-name idno))
                             (fs (cdr result-pair))
                             (orth (extract-orth-from-fs fs)))
                        (case syntax
                          (:tdl 
                           (output-derived-instance-as-tdl orth fs ostream 
                                                           lex-name idno))
                          (:lilfes 
                           (output-derived-instance-as-lilfes 
                            orth fs ostream id stem derivation))
                          (:ebl
                           (output-for-ebl orth fs ostream (car result-pair)
					   lex-name lex-entry-fs eblstream))
                          (:chic
                           (output-for-chic orth fs ostream (car result-pair) 
                                            lex-name lex-entry-fs 
                                            (lex-or-psort-infl-pos lex-entry)
                                            stem))
                          (:uc
                           (output-for-uc orth fs ostream (car result-pair) 
                                           lex-name lex-entry-fs 
                                           (lex-or-psort-infl-pos lex-entry)))
                          (t (error "Unsupported syntax specifier ~A"
                                    syntax))))
                      (incf idno))))
	  (when (eq syntax :ebl)
	    (output-rules-for-ebl eblstream)))))))
|#

(defun output-lex-and-derived (syntax &optional file-name ids-used)
  ;;; lexicon and everything that can be derived from it
  ;;; via lexical rule.  Ordered by base form.
  (unless file-name 
    (setf file-name
      (ask-user-for-new-pathname "Output file?")))
  (if (eq syntax :pet)
    (if ids-used
      (output-lexicon-for-pet file-name ids-used)
      (output-lexicon-for-pet file-name))
    (when file-name 
      (with-open-file 
          (ostream file-name :direction :output :if-exists :supersede)
        (let ((count 0)
	      (eblstream (when (eq syntax :ebl)
                         (open (concatenate 'string file-name ".lextypes")
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create))))
          (unless (or ids-used *ordered-lex-list*)
            (cerror "Continue without lexicon" 
                    "No lexicon list - lexicon must be read in from scratch"))
          (for lex-name in (or ids-used (reverse *ordered-lex-list*))
               do            
               (if (> count 100)
                 (progn (clear-expanded-lex)
                        (setf count 0))
                 (incf count))
               (setf *number-of-applications* 0)
               (let* ((lex-entry (get-psort-entry lex-name))
                      (lex-entry-fs 
                       (if lex-entry
                         (lex-or-psort-full-fs lex-entry)
                         (error "Entry for ~A not found" lex-name)))
                      (stem (lex-or-psort-orth lex-entry))
                      (result-list
                       (cons (cons nil lex-entry-fs)
                             (try-all-lexical-rules 
                              (list (cons nil lex-entry-fs)) 
                              (if (eq syntax :ebl)
				    (loop
				      for rule in (get-indexed-lrules nil nil)
				      when (not (inflectional-rule-p rule))
				      collect (rule-id rule))))))
                      (idno 0))
                 (for result-pair in result-list
                      do
                      (let* ((derivation 
                              (append (first result-pair) (list lex-name)))
                             (id (format nil "~(~a~)_~d" lex-name idno))
                             (fs (cdr result-pair))
                             (orth (extract-orth-from-fs fs)))
                        (case syntax
                          (:tdl 
                           (output-derived-instance-as-tdl orth fs ostream 
                                                           lex-name idno))
                          (:lilfes 
                           (output-derived-instance-as-lilfes 
                            orth fs ostream id stem derivation))
                          (:ebl
                           (output-for-ebl orth fs ostream (car result-pair)
					   lex-name lex-entry-fs eblstream))
                          (:chic
                           (output-for-chic orth fs ostream (car result-pair) 
                                            lex-name lex-entry-fs 
                                            (lex-or-psort-infl-pos lex-entry)
                                            stem))
                          (:uc
                           (output-for-uc orth fs ostream (car result-pair) 
                                           lex-name lex-entry-fs 
                                           (lex-or-psort-infl-pos lex-entry)))
                          (t (error "Unsupported syntax specifier ~A"
                                    syntax))))
                      (incf idno))))
	  (when (eq syntax :ebl)
	    (output-rules-for-ebl eblstream)))))))

(defun output-rules-for-ebl (stream)
  (labels ((output-rules (stream rules)
             (loop
                 for rule being each hash-value in rules
                 and id being each hash-key in rules
                 for type = (type-of-fs (tdfs-indef (rule-full-fs rule)))
                 for head = (if (or (subtype-p type 'head_final)
                                    (subtype-p type 'coord_phr))
                                1 0)
                 for adjunctionp = (subtype-p type 'head_mod_phrase_simple) do
                   (format 
                    stream
                    "(~s ~s ~d ~:[nil~;t~])~%" 
                    id type head adjunctionp))))
    (output-rules stream *rules*)
    (output-rules stream *lexical-rules*))
  (when (and (streamp stream) (open-stream-p stream))
    (close stream)))

(defun dag-inflected-p (dag)
  (declare (ignore dag))
  t)

(defun inflectional-rule-p (object)
  (let* ((name (cond
                ((stringp object) object)
                ((symbolp object) (symbol-name object))
                ((rule-p object) (symbol-name (rule-id object)))
                (t 
                 (error 
                  "inflectional-rule-p(): invalid call `~s'." 
                  object))))
         (break (max 0 (- (length name) (length *lex-rule-suffix*)))))
    (when (string-equal name *lex-rule-suffix* :start1 break)
      (subseq name 0 break))))

;;;
;;; generate full-form table for PET.  ann, i hope you will forgive me: this
;;; hard-wires the current LinGO set-up, where no non-inflectional rules apply
;;; before inflection.  thus, we save a large number of rule applications and
;;; an even larger number of copies (of intermediate structures).  i believe
;;; once we return to a sane morphology, it should be feasible to compile out
;;; the rule feeding first and then use this compiled-out set rather than the
;;; recursive calls on each new structure.  the copies really make this much
;;; more expensive than it should be; the difference for this is 2:47 to 27 s,
;;; i.e. a good factor of six speed-up.                     (25-mar-00  -  oe)
;;;

(defun output-entry-for-pet (stream instance form irule ipos length)
  ;;
  ;; filter out `unk-' VerbMobil default lexical entries (25-mar-00  -  oe)
  ;;
  (unless (or (null form) (string= form ""))
    (format
     stream
     "  {\"~(~a~)\", ~(~s~), NULL, ~:[NULL~*~;\"~(~a~)\"~], ~d, ~d},~%"
     instance form irule irule ipos length)))

(defun output-lexicon-for-pet (file 
                               &optional (ids (or *ordered-lex-list*
                                                  (collect-expanded-lex-ids 
                                                   *lexicon*))))
  (if (null ids)
    (format
     t
     "~&output-lexicon-for-pet(): ~
      must (re-)read lexicon or parse test suite first.~%")
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (loop
          with rules = (loop
                           for rule in (get-indexed-lrules nil nil)
                           when (inflectional-rule-p rule) collect rule)
          with caches = (make-array (length rules))
          with successes = 0
          with failures = 0
          finally (return (cons successes failures))
          for id in (remove-duplicates ids)
          for i from 1
          for entry = (get-psort-entry id)
          for tdfs = (and entry (lex-or-psort-full-fs entry))
          for inflectedp = (and tdfs (dag-inflected-p (tdfs-indef tdfs)))
          for ipos = (and entry (lex-or-psort-infl-pos entry))
          when (zerop (mod i 100)) do (clear-expanded-lex)
          when inflectedp do
            (let* ((orth (lex-or-psort-orth entry))
                   (form (nth (if ipos (- ipos 1) 0) orth)))
              (output-entry-for-pet
               stream
               id form nil (or ipos 0) (length orth)))
          else when entry do
            (loop
                with dag = (tdfs-indef tdfs)
                with type = (type-of-fs dag)
                for rule in rules
                for j from 0
                for rid = (rule-id rule)
                for spellingp = (spelling-change-rule-p rule)
                for rtdfs = (rule-full-fs rule)
                for orth = (lex-or-psort-orth entry)
                for stem = (let* ((orth (loop
                                            for foo on orth
                                            collect (first foo)
                                            when (rest foo) collect " ")))
                             (apply #'concatenate 'string orth))
                for form = (if spellingp
                             (first (first (full-morph-generate stem rid)))
                             stem)
                for daughter = (first (rule-rhs rule))
                for path = (nth (+ daughter 1) (rule-order rule))
                for cache = (rest (sassoc type (aref caches j)))
                for result = (unless (eq cache :fail)
                               (with-unification-context (ignore)
                                 (yadu! rtdfs tdfs path)))
                when result do
                  (incf successes)
                  (let* ((form (split-into-words form))
                         (form (nth (if ipos (- ipos 1) 0) form)))
                    (output-entry-for-pet
                     stream
                     id form rid (or ipos 0) (length orth)))
                else unless cache do
                  (incf failures)
                  (let* ((parent (get-type-entry type))
                         (tdfs (type-tdfs parent))
                         (dag (tdfs-indef tdfs))
                         (vector (nth daughter 
                                      (rule-daughters-restricted rule)))
                         (result (when (x-restrict-and-compatible-p dag vector)
                                   (with-unification-context (ignore)
                                     (yadu! rtdfs tdfs path))))
                         (entry (cons type (if result t :fail))))
                    (push entry (aref caches j))))))))


(defun output-for-ebl (orth fs ostream rule-list base-id base-fs ostream2)
  (declare (ignore fs))
  (let* ((type (type-of-fs (tdfs-indef base-fs)))
         (category (find-possibly-cached-cat type fs))
         (infl-rules nil)
         (other-rules nil))
    (when (and category 
	       (not (equal category "?"))
	       (not (equal category "STEM")))
      (loop for rule in rule-list 
           do
           (if (inflectional-rule-p rule)
               (push rule infl-rules)
             (push rule other-rules)))
      (format ostream 
              "~%(~S ~S ~S ~S ~A)" 
              (split-into-words orth) 
              type
              (cons base-id infl-rules)
              other-rules
              category)
      ;; build lexical-types
      (when ostream2
        (format
         ostream2
         "(~s . ~s)~%"
         base-id type)))))

(defun output-for-chic (orth fs ostream rule-list base-id base-fs 
                             infl-pos stem)
  (let* ((type (type-of-fs (tdfs-indef base-fs)))
         (infl-rules nil)
         (infl (dag-inflected-p (tdfs-indef fs)))
         (other-rules nil))
    (declare (ignore type))
    (loop for rule in rule-list 
         do
         (if (inflectional-rule-p rule)
             (push rule infl-rules)
           (push rule other-rules)))
    (unless other-rules
      (if infl
          (format ostream 
                  "\"~(~S~)\" ~(~S~) ~(~S~) ~(~S~)~%" 
                  base-id
                  (if infl-pos (nth (- infl-pos 1) 
                                    (split-into-words orth)) orth)
                  (first stem)
                  (inflectional-rule-p (first infl-rules)))))))

(defun output-for-uc (orth fs ostream rule-list base-id base-fs infl-pos)
  (let* ((type (type-of-fs (tdfs-indef base-fs)))
         (infl-rules nil)
         (infl (dag-inflected-p (tdfs-indef fs)))
         (other-rules nil))
    (declare (ignore type))
    (loop for rule in rule-list 
         do
         (if (inflectional-rule-p rule)
             (push rule infl-rules)
           (push rule other-rules)))
    (unless other-rules
      (if infl
         (format ostream 
                  "  {\"~(~S~)\", ~(~S~), NULL, ~:[NULL~*~;\"~(~S~)\"~], ~S, ~S},~%" 
                  base-id
                  (if infl-pos (nth (- infl-pos 1) 
                                    (split-into-words orth)) orth)
                  infl-rules
                  (if infl-rules (first infl-rules))
                  (if infl-pos infl-pos 0)
                  (length (split-into-words orth)))))))

(defvar *cat-type-cache* (make-hash-table))

(defun find-possibly-cached-cat (type fs)
  (let ((cached-cat nil)
        ;(cached-cat (gethash type *cat-type-cache*))
        )
    (or cached-cat
        (let ((cat (find-category-abb fs)))
          (setf (gethash type *cat-type-cache*) cat)
          cat))))

(defun output-grules (syntax &optional file-name local-p)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
        (loop for rule-name in (reverse *ordered-rule-list*)
             do
             (let ((entry (get-grammar-rule-entry rule-name)))                  
               (case syntax
                 (:tdl (output-instance-as-tdl rule-name entry
                                               ostream local-p))
                 (:lilfes 
                  (when local-p
                    (error "Local only output not supported with LiLFeS"))
                  (output-instance-as-lilfes 
                           rule-name entry
                           ostream))
                 (t (error "Unsupported syntax specifier ~A"
                           syntax))))))))

(defun output-lrules (syntax &optional file-name local-p)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
        (loop for rule-name in (reverse *ordered-lrule-list*)
             do
             (let ((entry (get-lex-rule-entry rule-name)))                  
               (case syntax
                 (:tdl (output-instance-as-tdl rule-name entry
                                               ostream local-p))
                 (t (error "Unsupported syntax specifier ~A"
                           syntax))))))))


(defun output-root (syntax &optional file-name)
  (unless file-name 
    (setf file-name
         (ask-user-for-new-pathname "Output file?")))
  (when file-name 
    (with-open-file 
        (ostream file-name :direction :output :if-exists :supersede)
      (loop for root-symbol in (if (listp *start-symbol*) *start-symbol*
                            (list *start-symbol*))
             do
             (let ((entry (get-psort-entry root-symbol)))   
               (if entry 
                   (case syntax
                     (:lilfes 
                      (output-instance-as-lilfes root-symbol entry
                                                 ostream :root))
                     (t (error "Unsupported syntax specifier ~A"
                               syntax)))
                 (format t "~%Warning ~A not found" root-symbol)))))))

;;; Support functions

;;; Sorting types by appearance order

(defvar *complete-order-alist* nil)

(defun sort-by-appearance-order (types)
  (let ((type-order-alist nil)
        (ok t))
    (setf *complete-order-alist* nil)
    (loop for type in types
         do
        (let ((type-entry (get-type-entry type)))
          (when type-entry
            ;; ignore unused leaf types
            (let ((types-used (extract-used-types type)))
              (push (cons type types-used) type-order-alist)))))
    (loop for type in types
         do
         (construct-all-ref-types type type-order-alist nil))
    (if ok
        (stable-sort 
         types
         #'(lambda (x y) 
             (< (length (assoc x *complete-order-alist*))
                (length (assoc y *complete-order-alist*))))))))

(defun construct-all-ref-types (type type-order-alist types-so-far)
  (when (member type types-so-far)
    (error "~%Mutual recursion involving ~A" type))
  (let ((done (assoc type *complete-order-alist*)))
    (if done (cdr done)
      (let* ((immediate-ref (cdr (assoc type type-order-alist)))
             (all-ref
              (remove-duplicates
               (append immediate-ref
                       (loop for ref-type in immediate-ref
                            append
                            (construct-all-ref-types ref-type type-order-alist
                                                     (cons type types-so-far)))))))
        (push (cons type all-ref) *complete-order-alist*)
        all-ref))))

(defun extract-used-types (type)
  (declare (special *res*))
  (setf *res* nil)
  (let ((type-entry (get-type-entry type)))
    (when type-entry 
      (let ((type-local-fs (type-local-constraint type-entry)))
        (when type-local-fs
          (loop for feat in (top-level-features-of type-local-fs)
               do
               (let ((internal-fs (get-dag-value type-local-fs feat)))
                 (collect-types-from-fs internal-fs))))
        (loop for parent in (type-parents type-entry)
             do 
             (pushnew parent *res*))
        *res*))))
         

(defun collect-types-from-fs (dag-instance)
  (declare (special *res*))
  (let ((real-dag (follow-pointers dag-instance)))
      (pushnew 
       (type-of-fs real-dag)
       *res* :test #'eq)
      (dolist (arc (dag-arcs real-dag))
        (collect-types-from-fs 
         (get-dag-value real-dag (dag-arc-attribute arc))))))

;;; expand-local-only-constraints is
;;; called from checktypes if hierarchy-only-p is set
;;; It is useful when reading in a set of types
;;; defined for PAGE which do not expand correctly in the
;;; LKB.  Note that it avoids setting the type of the
;;; local-constraint feature structure.

(defun expand-local-only-constraints nil
   (let ((ok t))
     (unmark-type-table)
     (determine-atomic-types)
     (loop for node in *type-names*
          do
          (let ((type-entry (get-type-entry node)))
            (unless 
                (expand-local-only-constraint node type-entry)
              (setf ok nil))))
     ok))
         

(defun expand-local-only-constraint (node type-entry)
  (let* ((*unify-debug-cycles* t)       ; turn on cyclic dag warning messages
         (constraint-spec (type-constraint-spec type-entry))
         (local-constraint 
          (if constraint-spec (process-unifications constraint-spec))))
    (if (and constraint-spec (null local-constraint))
        (progn
          (format t "~%Type ~A has an invalid constraint specification" node)
          nil)
     (progn
       (setf (type-local-constraint type-entry) local-constraint)
       t))))






