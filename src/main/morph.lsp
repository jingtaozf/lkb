;;; Copyright Bernard Jones 1993. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Bernard Jones
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK

;;; modified aac Dec 1994 to allow script to read in morphological data
;;;              Dec 1995 changed 'morph-rule to *morph-rule-type*
;;;              Feb 1996 changed test to allow for subtypes of *morph-rule-type*

;;; note - file format DEMANDS that the morphological rule
;;; should have the type *morph-rule-type* indicated by the bare atom
;;; immediately following the rule name


;;; Tree & Rule structure defs

(defstruct l-tree-node branches rules parent)
(defstruct tree-set 
   (suff-comp (make-l-tree-node)) 
   (suff-root (make-l-tree-node))
   (pre-comp (make-l-tree-node)) 
   (pre-root (make-l-tree-node)) 
   (in-comp (make-l-tree-node)) 
   (in-root (make-l-tree-node)))
(defstruct suffix-rule affix-name replacement prefix infix)
(defstruct infix-rule affix-name replacement prefix suffix)
(defstruct prefix-rule affix-name replacement suffix infix)
(defstruct morph-rule rule-type rule-name subrules)

;;; Global variable initialisation
(defvar *morph-trees* (make-tree-set))
(defmacro suffix-tree-comp nil `(tree-set-suff-comp *morph-trees*)) 
(defmacro prefix-tree-comp nil `(tree-set-pre-comp *morph-trees*))
(defmacro infix-tree-comp nil `(tree-set-in-comp *morph-trees*))
(defmacro suffix-tree-root nil `(tree-set-suff-root *morph-trees*)) 
(defmacro prefix-tree-root nil `(tree-set-pre-root *morph-trees*))
(defmacro infix-tree-root nil `(tree-set-in-root *morph-trees*))
(defvar *letter-set-list* nil)

;;; Reset function 
(defun reset-morph-var () 
  (setf *morph-trees* (make-tree-set))
  (setf *letter-set-list* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialisation and macro definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro a-list-search-update (key-list node)
   `(if (car ,key-list) 
      (or (cdr (assoc (car ,key-list) (l-tree-node-branches ,node))) 
         (cdr 
            (assoc (car ,key-list) 
               (push 
                  (cons (car ,key-list) 
                     (make-l-tree-node :parent 
                        (cons (car ,key-list) ,node))) 
                  (l-tree-node-branches ,node)))))
      ,node))

(defmacro ass-look-up (where)
  `(cdr 
     (assoc (car residue) (l-tree-node-branches ,where) :test #'eq)))

(defmacro my-coerce (symbol-in)
  `(cond ((null ,symbol-in) nil)
	 (t (remove '#\* (coerce (string-upcase ,symbol-in) 'list)))))

(defmacro suffix-args nil `(subseq instance 4 6))
           
(defmacro infix-args nil `(subseq instance 2 4))

(defmacro prefix-args nil `(subseq instance 0 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to do macro character unbinding
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chrexp (list)
   (mapcar 
      #'(lambda (y) 
         (remove '* 
            (mapcar #'(lambda (x) (intern (string x))) 
               (coerce (string y) 'list))))
      list))

(defun scanmac (list)
   (let (ret-list)
      (dolist (element list (remove-duplicates ret-list :key #'car))
         (loop
            (when (equal '! (pop element)) 
               (push (list (pop element) (gensym)) ret-list))
            (unless element (return))))))

(defun macro-unbind (rule l-set)
   (let*
      ((nurule (chrexp rule))
         (a-list (scanmac nurule))
         (m-rule 
            (list 
               (mapcar 
                  #'(lambda (y)
                     (remove '! 
                        (mapl 
                           #'(lambda (x) 
                              (when (equal '! (car x)) 
                                 (setf (cadr x) 
                                    (cadr (assoc (cadr x) a-list))))) 
                           y))) 
                  nurule))))
      (dolist (macro a-list m-rule)
         (setf m-rule
            (apply #'append 
               (mapcar 
                  #'(lambda (w) 
                     (mapcar
                        #'(lambda (z)
                           (mapcar 
                              #'(lambda (v)
                                 (substitute w (cadr macro) v)) 
                              z))
                        m-rule))
                  (cadr (assoc (car macro) l-set))))))))

;;; Utility function connected with morph-rule compilation

(defun lenfail (lis num)
   (dolist (elem lis) (or (eq (length elem) num) (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Morphological rule I/O functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; minor mods for consistency/user friendliness!  AAC
 
(defun morph-file-compile ()
   (let* ((ovwr (lkb-y-or-n-p "Overwrite any existing morphological structures?"))
          (filename (ask-user-for-existing-pathname "Morphological rules")))
      (when filename
         (morph-file-read-aux filename ovwr)
         (lkb-beep))))

(defun morph-file-read-aux (filename ovwr)
   (when ovwr
      (reset-morph-var))
   (with-open-file 
      (istream filename
         :direction :input)
      (format t "~%Loading morphological data...")
      (block outer
         (loop
            ;; Go through the file - ignore comments.
            ;; If a percent sign occurs outside a rule,
            ;; it must be a new letter set definition - 
            ;; process it - failing that, process the 
            ;; next rule.
            (let ((next-char (peek-char t istream nil 'eof)))
               (when (eql next-char 'eof) (return-from outer))
               (if (eql next-char #\;
                     )
                  (read-line istream)
                  (if (eql next-char #\%)
                     (let* ((string-thing (read-line istream))
                           (form (read-from-string string-thing 
                                 nil 'eof :start 1)))
                        (cond
                           ((eql form 'eof) (error "~%Bad file"))
                           ((not (listp form)) (error "~Bad line"))
                           ((eql (car form) 'letter-set)
                              (setf *letter-set-list*
                                 (letter-set-add form 
                                    *letter-set-list*)))
                           (t (error "~%Wrong type of command"))))                    
                     (morph-item-process istream))))))))


(defun morph-item-process (istream)
   (let ((id (read istream))
         (type (read istream))
         (current-set *letter-set-list*)
         method-list)
      (when (or (eql type *morph-rule-type*)
                (subtype-p type *morph-rule-type*))
        ;;; added the possibility of the rule type being a subtype
        ;;; of *morph-rule-type* 
        ;;; AAC Feb 1996
         (block outer
            (loop
               ;; Within each rule, if there is a set of percent marked
               ;; headers, add them as morphological rules.
               (let ((next-char (peek-char t istream nil 'eof)))
                  (when (eql next-char 'eof) 
                     (error "~%Incomplete rule definition for %A" id))
                  (if (eql next-char #\;
                        )
                     (read-line istream)
                     (if (eql next-char #\%)
                        (let ((string-thing (read-line istream))
                              (start-pos 1))
                           (loop
                              (multiple-value-bind (form end-value)
                                 (read-from-string string-thing nil 
                                    'eof :start start-pos)
                                 (when (eql form 'eof) (return))
                                 (if 
                                    (and 
                                       (listp form) 
                                       (eql (car form) 'letter-set))
                                    (setf current-set 
                                       (letter-set-add form 
                                          current-set))
                                    (setf method-list 
                                       (append method-list 
                                          (list form))))
                                 (setf start-pos end-value))))
                        (return-from outer)))))))
      ;; Tidy up by scanning to the end of the rule
      (let ((*readtable*
               (define-break-characters 
                  '(#\% #\;
                     #\< #\> #\= #\: #\.))))         
         (loop
            (when (eql (read istream nil #\.) #\.) (return))))
      ;; Then compile the morphological rules
      (when method-list
         (morph-input 
            (structify 
               (list 
                  (car method-list) 
                  (cons id (cdr method-list))))
            current-set))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Morphological rule compilation functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Turn rules into a rule-structure
(defun structify (rule)
   (make-morph-rule
      :rule-type (car rule)
      :rule-name (caadr rule)
      :subrules (hexa-expand (cdadr rule) (car rule))))

;;; Make two element subrules into 6 element ones
(defun hexa-expand (subrules name)
   (cond 
      ((eq name 'letter-set) (car subrules))
      ((eq name 'suffix) (hexify subrules 4))
      ((eq name 'infix) (hexify subrules 2))
      ((eq name 'prefix) (hexify subrules 0))
      (t subrules)))

(defun hexify (elements position)
   (for member in elements
      when (listp member)
      collect
      (if 
         (eq (length member) 2) 
         (replace 
            (copy-list '(* * * * * *)) 
            member 
            :start1 position)
         member)))

(defun morph-input (rule *l-set*)
   (declare (special *l-set*))
   (cond 
      ((null (morph-rule-subrules rule)) (error "~%No subrules"))
      ((lenfail (morph-rule-subrules rule) 6) 
         (error "~%Subrules incorrectly formatted"))
      ((eq (morph-rule-rule-type rule) 'suffix) 
         (setf (morph-rule-subrules rule) 
            (apply #'append 
               (mapcar #'(lambda (x) (macro-unbind x *l-set*)) 
                  (morph-rule-subrules rule))))
         (add-suffix rule))
      ((eq (morph-rule-rule-type rule) 'prefix)
         (setf (morph-rule-subrules rule) 
            (apply #'append 
               (mapcar #'(lambda (x) (macro-unbind x *l-set*)) 
                  (morph-rule-subrules rule))))
         (add-prefix rule))
      ((eq (morph-rule-rule-type rule) 'infix) 
         (setf (morph-rule-subrules rule) 
            (apply #'append 
               (mapcar #'(lambda (x) (macro-unbind x *l-set*)) 
                  (morph-rule-subrules rule))))
         (add-infix rule))
      (t (error "~%Unspecified rule type"))))

(defun letter-set-add (new-rule letter-set)
   (push
      (append 
         (last 
            (mapcar #'(lambda (x) (intern (string x)))
               (coerce 
                  (string (caadr new-rule)) 
                  'list)))
         (list
            (mapcar #'(lambda (x) (intern (string x)))
               (coerce 
                  (string (cadadr new-rule)) 
                  'list))))
      letter-set))

(defun add-infix (new-rule) 
   (for subrule in (morph-rule-subrules new-rule)
      do
      (add-infix-instance 
         (morph-rule-rule-name new-rule) 
         subrule)))

(defun add-suffix (new-rule) 
   (for subrule in (morph-rule-subrules new-rule)
      do
      (add-suffix-instance
         (morph-rule-rule-name new-rule) 
         subrule)))

(defun add-prefix (new-rule) 
   (for subrule in (morph-rule-subrules new-rule)
      do
      (add-prefix-instance 
         (morph-rule-rule-name new-rule) 
         subrule)))

(defun add-suffix-instance (rule-name instance)
   (let (comp-node root-rule)
      (do 
         ((letters (cdr (reverse (sixth instance))) (cdr letters)) 
            (tree 
               (a-list-search-update (reverse (sixth instance)) 
                  (suffix-tree-comp)) 
               (a-list-search-update letters tree)))
         ((null letters) 
            (setf comp-node tree)
            (setf (l-tree-node-rules tree) 
               (cons 
                  (make-suffix-rule :affix-name rule-name :replacement 
                     (do 
                        ((lletters 
                              (cdr (reverse (fifth instance))) 
                              (cdr lletters)) 
                           (ttree 
                              (a-list-search-update 
                                 (reverse (fifth instance)) 
                                 (suffix-tree-root)) 
                              (a-list-search-update lletters ttree)))
                        ((null lletters) 
                           (setf root-rule 
                              (make-suffix-rule 
                                 :affix-name rule-name 
                                 :replacement nil 
                                 :prefix (reverse (prefix-args)) 
                                 :infix (reverse (infix-args))))
                           (setf (l-tree-node-rules ttree) 
                              (cons root-rule 
                                 (l-tree-node-rules ttree)))
                           ttree)) 
                     :prefix (prefix-args) :infix (infix-args)) 
                  (l-tree-node-rules tree)))))
      (setf (suffix-rule-replacement root-rule) comp-node)))

(defun add-infix-instance (rule-name instance)
   (let (comp-node root-rule)
      (do 
         ((letters (cdr (fourth instance)) (cdr letters)) 
            (tree 
               (a-list-search-update (fourth instance) 
                  (infix-tree-comp)) 
               (a-list-search-update letters tree)))
         ((null letters) 
            (setf comp-node tree)
            (setf (l-tree-node-rules tree) 
               (cons 
                  (make-infix-rule :affix-name rule-name :replacement 
                     (do 
                        ((lletters (cdr (third instance)) 
                              (cdr lletters)) 
                           (ttree 
                              (a-list-search-update (third instance) 
                                 (infix-tree-root)) 
                              (a-list-search-update lletters ttree)))
                        ((null lletters) 
                           (setf root-rule 
                              (make-infix-rule 
                                 :affix-name rule-name 
                                 :replacement nil 
                                 :prefix (reverse (prefix-args)) 
                                 :suffix (reverse (suffix-args))))
                           (setf (l-tree-node-rules ttree) 
                              (cons root-rule 
                                 (l-tree-node-rules ttree)))
                           ttree)) 
                     :prefix (prefix-args) :suffix (suffix-args)) 
                  (l-tree-node-rules tree)))))
      (setf (infix-rule-replacement root-rule) comp-node)))

(defun add-prefix-instance (rule-name instance)
   (let (comp-node root-rule)
      (do 
         ((letters (cdr (second instance)) (cdr letters)) 
            (tree 
               (a-list-search-update (second instance) 
                  (prefix-tree-comp)) 
               (a-list-search-update letters tree)))
         ((null letters) 
            (setf comp-node tree)
            (setf (l-tree-node-rules tree) 
               (cons 
                  (make-prefix-rule :affix-name rule-name :replacement 
                     (do 
                        ((lletters (cdr (first instance)) 
                              (cdr lletters)) 
                           (ttree 
                              (a-list-search-update (first instance) 
                                 (prefix-tree-root)) 
                              (a-list-search-update lletters ttree)))
                        ((null lletters) 
                           (setf root-rule 
                              (make-prefix-rule 
                                 :affix-name rule-name 
                                 :replacement nil 
                                 :infix (reverse (infix-args)) 
                                 :suffix (reverse (suffix-args))))
                           (setf (l-tree-node-rules ttree) 
                              (cons root-rule 
                                 (l-tree-node-rules ttree)))
                           ttree)) 
                     :infix (infix-args) :suffix (suffix-args)) 
                  (l-tree-node-rules tree)))))
      (setf (prefix-rule-replacement root-rule) comp-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for morphological analysis
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun morph-analyse (string)
   (remove-morphemes 
      (mapcar #'(lambda (x) (intern (string x))) 
         (coerce (string-upcase string) 'list))))

(defun remove-morphemes (word)
   (let ((definites-list (copy-list '(nil))))
      (do ((current-combinations
               (list (list word))
               (append
                  (remove-prefix current-combinations)
                  (remove-infix current-combinations)
                  (remove-suffix current-combinations))))
         ((null current-combinations) (cdr definites-list))
         (nconc definites-list
            (for morphological-possibility in current-combinations
               when 
               (or
                  (equal morphological-possibility 
                     (remove-duplicates morphological-possibility 
                        :test #'equal))
                  (or
                     (setf current-combinations 
                        (remove morphological-possibility 
                           current-combinations :test #'equal)) 
                     nil))
               filter
               (let* 
                  ((root 
                        (string-downcase
                           (coerce 
                              (mapcar #'character 
                                 (car morphological-possibility)) 
                              'string)))
                     (lexical 
                        (gethash (string-upcase root) 
                           *lexical-entries*)))
                  (and lexical
                     (cons root 
                        (cdr morphological-possibility)))))))))

(defun remove-suffix (input-words)
   (for entry in input-words
      append
      (let ((node (suffix-tree-comp))
            (oldword 
               (string-downcase
                  (coerce 
                     (mapcar #'character 
                        (car entry)) 
                     'string))))
         (for residue on (reverse (car entry))
            while node
            append
            (setf node (ass-look-up node))
            (and node
               (let ((nodal-rules (l-tree-node-rules node)))
                  (and nodal-rules
                     (process-suffix 
                        oldword
                        (cdr residue) 
                        nodal-rules 
                        (cdr entry)))))))))

(defun remove-prefix (input-words)
   (for entry in input-words
      append
      (let ((node (prefix-tree-comp))
            (oldword 
               (string-downcase
                  (coerce 
                     (mapcar #'character 
                        (car entry)) 
                     'string))))
         (for residue on (car entry)
            while node
            append
            (setf node (ass-look-up node))
            (and node 
               (let ((nodal-rules (l-tree-node-rules node)))
                  (and nodal-rules
                     (process-prefix 
                        oldword
                        (cdr residue) 
                        nodal-rules 
                        (cdr entry)))))))))

(defun remove-infix (input-words)
   (let (prefixer)
      (for entry in input-words
         append
         (prog1
            (for segment on (car entry)
               append
               (prog1
                  (let ((node (infix-tree-comp))
                        (oldword 
                           (string-downcase
                              (coerce 
                                 (mapcar #'character 
                                    (car entry)) 
                                 'string))))
                     (for residue on segment
                        while node
                        append
                        (setf node (ass-look-up node))
                        (and node 
                           (let ((nodal-rules (l-tree-node-rules node)))
                              (and nodal-rules
                                 (process-infix 
                                    oldword
                                    (cdr residue) 
                                    prefixer 
                                    nodal-rules 
                                    (cdr entry)))))))
                  (push (car segment) prefixer)))
            (setf prefixer nil)))))

(defun process-suffix (oldword word new-rules old-rules)
   (for rule in new-rules
      filter
      (let ((new-word
               (infix-remove 
                  (prefix-remove 
                     (append 
                        (reverse word) 
                        (do 
                           ((repl nil 
                                 (cons (car (l-tree-node-parent eert)) 
                                    repl))
                              (eert (suffix-rule-replacement rule) 
                                 (cdr (l-tree-node-parent eert))))
                           ((null (l-tree-node-parent eert)) 
                              (reverse repl))))
                     (suffix-rule-prefix rule))
                  (suffix-rule-infix rule))))
         (and 
            (or word (suffix-rule-replacement rule))
            new-word
            (cons 
               new-word
               (cons 
                  (list (suffix-rule-affix-name rule) oldword)
                  old-rules))))))

(defun process-prefix (oldword word new-rules old-rules)
   (for rule in new-rules
      filter
      (let ((new-word
               (infix-remove 
                  (suffix-remove 
                     (append 
                        (do 
                           ((repl nil 
                                 (cons (car (l-tree-node-parent eert)) 
                                    repl))
                              (eert (prefix-rule-replacement rule) 
                                 (cdr (l-tree-node-parent eert))))
                           ((null (l-tree-node-parent eert)) repl)) 
                        word)
                     (prefix-rule-suffix rule))
                  (prefix-rule-infix rule))))
         (and 
            (or word (prefix-rule-replacement rule))
            new-word
            (cons 
               new-word
               (cons 
                  (list (prefix-rule-affix-name rule) oldword)
                  old-rules))))))

(defun process-infix (oldword postamble preamble new-rules old-rules)
   (for rule in new-rules
      filter
      (let ((new-word
               (suffix-remove 
                  (prefix-remove 
                     (append 
                        (reverse preamble) 
                        (do 
                           ((repl nil 
                                 (cons (car (l-tree-node-parent eert)) 
                                    repl))
                              (eert (infix-rule-replacement rule) 
                                 (cdr (l-tree-node-parent eert))))
                           ((null (l-tree-node-parent eert)) repl))
                        postamble)
                     (infix-rule-prefix rule))
                  (infix-rule-suffix rule))))
         (and 
            (or postamble preamble (infix-rule-replacement rule))
            new-word
            (cons 
               new-word
               (cons 
                  (list (infix-rule-affix-name rule) oldword)
                  old-rules))))))

(defun suffix-remove (word suffix)
   (let* ((rem (second suffix))
         (pos (search rem word)))
      (if rem
         (and (eq pos (- (length word) (length rem)))
            (append
               (buttail word pos)
               (car suffix)))
         word)))

(defun buttail (word pos)
  ;;; Bernie left this undefined - it is apparently never called
  ;;; when running the morph system with the LKB, so this function
  ;;; is just to avoid annoying warning messages 
  (format t "~%Warning: function BUTTAIL finally got called with args ~A ~A"
          word pos)
  nil)

(defun prefix-remove (word prefix)
   (let ((rem (second prefix)))
      (if rem
         (and (eq 0 (search rem word))
            (append 
               (car prefix) 
               (nthcdr (length rem) word)))
         word)))

(defun infix-remove (word infix)
   (let* ((rem (second infix))
         (pos (search rem word)))
      (if rem
         (and pos
            (append
               (buttail word pos)
               (car infix)
               (nthcdr (+ pos (length rem)) word)))
         word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for morphological generation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun morph-generate (string rule)
   (add-morphemes 
      (mapcar #'(lambda (x) (intern (string x))) 
         (coerce (string-upcase string) 'list))
      rule))

(defun add-morphemes (word rule)
   (for morphological-possibility in 
      (or
         (make-prefix word rule)
         (make-infix word rule)
         (make-suffix word rule))
      filter
      (cons 
         (string-downcase
            (coerce 
               (mapcar #'character 
                  (car morphological-possibility)) 
               'string)) 
         (cdr morphological-possibility))))
   
(defun make-suffix (input-word rule)
   (let* ((node (suffix-tree-root))
         (oldword 
            (string-downcase
               (coerce 
                  (mapcar #'character input-word) 
                  'string)))
         (output 
            (process-suffix 
               oldword
               (reverse input-word) 
               (for poss in (l-tree-node-rules node)
                  when (eql rule (suffix-rule-affix-name poss))
                  collect
                  poss)
               nil)))
      (for residue on (reverse input-word)
         while node
         do
         (setf node (ass-look-up node))
         (and node
            (setf output 
               (process-suffix 
                  oldword
                  (cdr residue) 
                  (for poss in (l-tree-node-rules node)
                     when (eql rule (suffix-rule-affix-name poss))
                     collect
                     poss)
                  nil))))
      output))

(defun make-prefix (input-word rule)
   (let* ((node (prefix-tree-root))
         (oldword 
            (string-downcase
               (coerce 
                  (mapcar #'character input-word) 
                  'string)))
         (output 
            (process-prefix 
               oldword
               input-word 
               (for poss in (l-tree-node-rules node)
                  when (eql rule (prefix-rule-affix-name poss))
                  collect
                  poss)
               nil)))
      (for residue on input-word
         while node
         do
         (setf node (ass-look-up node))
         (and node
            (setf output 
               (process-prefix 
                  oldword
                  (cdr residue) 
                  (for poss in (l-tree-node-rules node)
                     when (eql rule (prefix-rule-affix-name poss))
                     collect
                     poss)
                  nil))))
      output))

;;; Currently dubious, hence not instnatiated...

(defun make-infix (input-word rule) 
  (declare (ignore rule input-word))
  nil) 