#+:allegro(in-package :clim-user)

#+:allegro(eval-when 
 (compile load eval)
(shadowing-import '(cl-user::read-type-file cl-user::read-type-files 
cl-user::read-lex-file
cl-user::read-q-lex-file cl-user::read-psort-file 
cl-user::read-grammar-file cl-user::read-lex-rule-file
cl-user::morph-file-compile 
cl-user::read-tlink-file cl-user::read-tlink-rule-file
cl-user::read-templates-file cl-user::read-qc-file 
cl-user::set-up-type-cache cl-user::show-type-spec 
cl-user::show-type cl-user::show-lex-def cl-user::show-lex
cl-user::show-word-defs cl-user::show-words 
cl-user::show-grammar-rule cl-user::show-lex-rule 
cl-user::show-tlinks cl-user::show-tlink-rules
cl-user::do-parse cl-user::show-parse cl-user::show-parse-edge 
cl-user::show-chart cl-user::parse-sentences 
cl-user::apply-lex cl-user::apply-lex-rules cl-user::apply-tlink
cl-user::create-new-tlinks cl-user::interactive-unification-check
cl-user::do-resolve-psort cl-user::do-constraint-parse cl-user::do-translation 
cl-user::do-translate-psort
cl-user::index-do-index cl-user::read-indices 
cl-user::save-indices cl-user::clear-non-parents
cl-user::inherit-display-settings 
cl-user::dump-lkb cl-user::output-display-settings
cl-user::output-type-file cl-user::output-lexicon-file
; yadu specific
cl-user::show-type-tdfs cl-user::show-unlinked-lex
cl-user::*lkb-system-version*)))

(defun create-lkb-system-menu nil
;  (unless (eql *lkb-system-version* :lkb4)
;    (error "LKB4 functions are probably not loaded"))
   (setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
         (list
              (make-lkb-submenu-item :menu-title "Load"
                 :menu-items
                  (list 
                     (make-menu-item :name "Type file"
                        :value 'read-type-file :available-p t)
                     (make-menu-item :name "Type files"
                        :value 'read-type-files :available-p t)
                     (make-menu-item :name "Lexicon file"
                        :value 'read-lex-file :available-p nil)
                     (make-menu-item :name "Compiled lexicon file"
                        :value 'read-q-lex-file :available-p nil)
                     (make-menu-item :name "Psort file"
                        :value 'read-psort-file :available-p nil)
                     (make-menu-item :name "Grammar rule file"
                        :value 'read-grammar-file :available-p nil)
                     (make-menu-item :name "Lexical rule file"
                        :value 'read-lex-rule-file :available-p nil)
                     (make-menu-item :name "Morphological data file"
                        :value 'morph-file-compile :available-p nil)
                     (make-menu-item :name "Tlink file"
                        :value 'read-tlink-file :available-p nil)
                     (make-menu-item :name "Tlink rule file"
                        :value 'read-tlink-rule-file :available-p nil)
                     (make-menu-item :name "Templates file"
                        :value 'read-templates-file :available-p nil)
                     (make-menu-item :name "Quick check file"
                        :value 'read-qc-file :available-p nil)  
                     )
                  :available-p t)
         (make-lkb-submenu-item :menu-title "View"
                 :menu-items
                  (list 
                     (make-menu-item :name "Type definition"
                        :value 'show-type-spec)
                     (make-menu-item :name "Expanded type"
                        :value 'show-type)
                     (make-menu-item :name "Lex or psort definition"
                        :value 'show-lex-def)
                     (make-menu-item :name "Lex or psort entry"
                        :value 'show-lex)
                     (make-menu-item :name "Word definitions"
                        :value 'show-word-defs)
                     (make-menu-item :name "Word entries"
                        :value 'show-words)
                     (make-menu-item :name "Grammar rule"
                        :value 'show-grammar-rule)
                     (make-menu-item :name "Lexical rule"
                        :value 'show-lex-rule)
                     (make-menu-item :name "Tlinks"
                        :value 'show-tlinks)
                     (make-menu-item :name "Tlink rules"
                        :value 'show-tlink-rules))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Parse"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Parse input"
                        :value 'do-parse-batch)
                     (make-menu-item :name "Show parse"
                        :value 'show-parse)
                     (make-menu-item :name "Show edge"
                        :value 'show-parse-edge)
                     (make-menu-item :name "Show chart"
                        :value 'show-chart)
                     (make-menu-item :name "Batch parse"
                        :value 'parse-sentences-batch))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Links"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Apply lexical rule"
                        :value 'apply-lex)
                     (make-menu-item :name "Apply all lex rules"
                        :value 'apply-lex-rules)
                     (make-menu-item :name "Apply ordinary tlink"
                        :value 'apply-tlink)
                     (make-menu-item :name "Expand tlinks"
                        :value 'create-new-tlinks)
                     (make-menu-item :name "Unification check"
                        :value 'interactive-unification-check)
                     )
               :available-p nil)
            (make-lkb-submenu-item :menu-title "Constraints"
                 :menu-items                     
                  (list
                     (make-menu-item :name "Resolve psort"
                        :value 'do-resolve-psort)
                     (make-menu-item :name "Parse"
                        :value 'do-constraint-parse)
                     (make-menu-item :name "Translate"
                        :value 'do-translation)
                     (make-menu-item :name "Translate psort"
                        :value 'do-translate-psort))
               :available-p nil)
            (make-lkb-submenu-item :menu-title "Index"
                 :menu-items 
                  (list
                     (make-menu-item :name "Index & check"
                        :value 'index-do-index)
                     (make-menu-item :name "Load indices"
                        :value 'read-indices)
                     (make-menu-item :name "Save indices"
                        :value 'save-indices))
               :available-p nil)
            (make-menu-item :name "Tidy up"
               :value 'clear-non-parents
               :available-p nil) 
            (make-lkb-submenu-item :menu-title "Edit"
                 :menu-items 
                  (list 
;;;                      (make-menu-item :name "Type"
;;;                         :value nil ; 'edit-type
;;;                         :available-p nil)
                     (make-menu-item :name "Inherit display settings"
                        :value 'inherit-display-settings)
;;;                      (make-menu-item :name "Lex or psort entry"
;;;                         :value nil ; 'edit-lex
;;;                         :available-p nil)
                     )               
               :available-p nil)
            (make-lkb-submenu-item :menu-title "Output"
                 :menu-items 
                  (list                     
                     (make-menu-item :name "Dump system"
                        :value 'dump-lkb)
                     (make-menu-item :name "Display settings"
                        :value 'output-display-settings)
                     (make-menu-item :name "Types with glbs"
                        :value 'output-type-file 
                        :available-p nil)
                     (make-menu-item :name "Lexicon file"
                        :value 'output-lexicon-file 
                        :available-p nil))
               :available-p nil)))))


(defun create-yadu-system-menu nil
;;  (unless (eql *lkb-system-version* :yadu)
;;    (error "YADU functions are probably not loaded"))
   (setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
         (list
              (make-lkb-submenu-item :menu-title "Load"
                 :menu-items
                  (list 
                     (make-menu-item :name "Type file"
                        :value 'read-type-file :available-p t)
                     (make-menu-item :name "Type files"
                        :value 'read-type-files :available-p t)
                     (make-menu-item :name "Lexicon file"
                        :value 'read-lex-file :available-p nil)
                     (make-menu-item :name "Grammar rule file"
                        :value 'read-grammar-file :available-p nil)
                     (make-menu-item :name "Lexical rule file"
                        :value 'read-lex-rule-file :available-p nil)
                     )
                  :available-p t)
         (make-lkb-submenu-item :menu-title "View"
                 :menu-items
                  (list 
                     (make-menu-item :name "Type definition"
                        :value 'show-type-spec)
                     (make-menu-item :name "Expanded type"
                        :value 'show-type-tdfs)
                     (make-menu-item :name "Lex entry"
                        :value 'show-lex)
                     (make-menu-item :name "Unlinked entry"
                        :value 'show-unlinked-lex)
                     (make-menu-item :name "Word entries"
                        :value 'show-words)
                     (make-menu-item :name "Grammar rule"
                        :value 'show-grammar-rule)
                     (make-menu-item :name "Lexical rule"
                        :value 'show-lex-rule)
                     )
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Parse"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Parse input"
                        :value 'do-parse)
                     (make-menu-item :name "Show parse"
                        :value 'show-parse)
                     (make-menu-item :name "Show edge"
                        :value 'show-parse-edge)
                     (make-menu-item :name "Show chart"
                        :value 'show-chart))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Links"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Apply lexical rule"
                        :value 'apply-lex)
                     (make-menu-item :name "Apply all lex rules"
                        :value 'apply-lex-rules)
; add the following sometime as in lkb4+
;                     (make-menu-item :name "Unification check"
;                        :value 'interactive-unification-check)
                     )
               :available-p nil)
            (make-menu-item :name "Tidy up"
               :value 'clear-non-parents
               :available-p nil) 
            (make-lkb-submenu-item :menu-title "Edit"
                 :menu-items 
                  (list 
                     (make-menu-item :name "Inherit display settings"
                        :value 'inherit-display-settings)
                     )               
               :available-p nil)
            (make-lkb-submenu-item :menu-title "Output"
                 :menu-items 
                  (list                     
                     (make-menu-item :name "Dump system"
                        :value 'dump-lkb)
                     (make-menu-item :name "Display settings"
                        :value 'output-display-settings)
                     )
               :available-p nil)))))
