#+:allegro(in-package :clim-user)
#-:allegro(in-package :cl-user)
#+:allegro
(eval-when 
    (compile load eval)
  (shadowing-import '(cl-user::read-script-file
                      cl-user::read-type-patch-files
                      cl-user::read-type-file 
		      cl-user::read-type-files 
		      cl-user::read-lex-file
		      cl-user::read-grammar-file 
		      cl-user::read-lex-rule-file
		      cl-user::read-morph-file
                      cl-user::read-psort-file 
		      cl-user::read-parse-nodes-file
		      ;; view
		      cl-user::show-type-tree
		      cl-user::show-type-spec 
		      cl-user::show-type 
		      cl-user::show-lex-def 
		      cl-user::show-lex
		      cl-user::show-word-defs 
		      cl-user::show-words 
		      cl-user::show-grammar-rule 
		      cl-user::show-lex-rule 
		      ;; parse
		      cl-user::do-parse 
		      cl-user::show-parse 
		      cl-user::show-parse-edge 
		      cl-user::show-chart 
		      cl-user::print-chart
		      cl-user::parse-sentences 
		      cl-user::compare-parses
		      ;; generate
		      cl-user::generate-from-edge
                      cl-user::show-gen-result
                      cl-user::show-gen-edge
		      cl-user::show-gen-chart
		      cl-user::print-gen-chart
		      ;; link
		      cl-user::apply-lex 
		      cl-user::apply-lex-rules 
		      cl-user::interactive-unification-check
		      ;; tidy
		      cl-user::clear-non-parents
		      ;; output
		      cl-user::output-type-file 
                      cl-user::output-display-settings
                      cl-user::load-display-settings
		      cl-user::*lkb-system-version*
		      ;; options
		      cl-user::get-parameters)))

;;; Menus moved to here from topmenu.lsp, since they can be
;;; treated as independent between ACL and MCL

(defun create-mini-lkb-system-menu nil
  ;;; cut down system for teaching purposes
;;  (unless (eql *lkb-system-version* :lkb4)
;;    (error "LKB4 functions are probably not loaded"))
   (setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
                  (list
                   (make-menu-item :name "Load grammar..."
                                   :value 'read-script-file :available-p t)
                   (make-lkb-submenu-item :menu-title "View"
                                          :menu-items
                                          (list 
                                           (make-menu-item :name "Type hierarchy..."
                                                           :value 'show-type-tree)
                                           (make-menu-item :name "Type definition..."
                                                           :value 'show-type-spec)
                                           (make-menu-item :name "Expanded type..."
                                                           :value 'show-type)
;                                  (make-menu-item :name "Lex definition..."
;                                                   :value 'show-lex-def)
                                           (make-menu-item :name "Lex entry..."
                                                           :value 'show-lex)
;                                  (make-menu-item :name "Word definitions..."
;                                                  :value 'show-word-defs)
                                           (make-menu-item :name "Word entries..."
                                                           :value 'show-words)
                                           (make-menu-item :name "Grammar rule..."
                                                           :value 'show-grammar-rule)
                                           (make-menu-item :name "Lexical rule..."
                                                           :value 'show-lex-rule)
                                           )
                                          :available-p t)
                   (make-lkb-submenu-item :menu-title "Parse"
                                          :menu-items                       
                                          (list 
                                           (make-menu-item :name "Parse input..."
                                                           :value 'do-parse)
                                           (make-menu-item :name "Show parse"
                                                           :value 'show-parse)
                                           (make-menu-item :name "Show edge..."
                                                           :value 'show-parse-edge)
                                           (make-menu-item :name "Show chart"
                                                           :value 'show-chart)
                                           (make-menu-item :name "Print chart"
                                                           :value 'print-chart)
                                           (make-menu-item :name "Batch parse..."
                                                           :value 'parse-sentences))
                                          :available-p nil)
                   (make-lkb-submenu-item :menu-title "Tests"
                                          :menu-items                       
                                          (list 
                                           (make-menu-item :name "Apply lexical rule..."
                                                           :value 'apply-lex)
                                           (make-menu-item :name "Apply all lex rules..."
                                                           :value 'apply-lex-rules)
                                           (make-menu-item :name "Unification check..."
                                                           :value 'interactive-unification-check)
                                           )
                                          :available-p nil) 
                   (make-lkb-submenu-item :menu-title "Options"
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Set options..."
                                                           :value 'get-parameters :available-p t)
                                           (make-menu-item :name "Save display settings..."
                                                           :value 'output-display-settings)
                                           (make-menu-item :name "Load display options..."
                                                           :value 'load-display-settings)))))))


(defun create-big-lkb-system-menu nil
  ;;; for system with MRS etc
   (setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
         (list
              (make-lkb-submenu-item :menu-title "Load"
                 :menu-items
                  (list 
                     (make-menu-item :name "Complete grammar..."
                                     :value 'read-script-file :available-p t)
                     (make-menu-item :name "Patch type files"
                                     :value 'read-type-patch-files 
                                     :available-p nil)
                     (make-menu-item :name "Type file..."
                        :value 'read-type-file :available-p t)
                     (make-menu-item :name "Type files..."
                        :value 'read-type-files :available-p t)
                     (make-menu-item :name "Lexicon file..."
                        :value 'read-lex-file :available-p nil)
                     (make-menu-item :name "Grammar rule file..."
                        :value 'read-grammar-file :available-p nil)
                     (make-menu-item :name "Lexical rule file..."
                        :value 'read-lex-rule-file :available-p nil)
                     (make-menu-item :name "Morphology file..."
                        :value 'read-morph-file :available-p nil)
                     (make-menu-item :name "Root definition file..."
                        :value 'read-psorts-file :available-p nil)
                     (make-menu-item :name "Node name file..."
                        :value 'read-parse-nodes-file :available-p nil)
                     )
                  :available-p t)
         (make-lkb-submenu-item :menu-title "View"
                 :menu-items
                  (list 
                     (make-menu-item :name "Type hierarchy..."
                        :value 'show-type-tree)
                     (make-menu-item :name "Type definition..."
                        :value 'show-type-spec)
                     (make-menu-item :name "Expanded type..."
                        :value 'show-type)
                     (make-menu-item :name "Lex definition..."
                        :value 'show-lex-def)
                     (make-menu-item :name "Lex entry..."
                        :value 'show-lex)
                     (make-menu-item :name "Word definitions..."
                        :value 'show-word-defs)
                     (make-menu-item :name "Word entries..."
                        :value 'show-words)
                     (make-menu-item :name "Grammar rule..."
                        :value 'show-grammar-rule)
                     (make-menu-item :name "Lexical rule..."
                        :value 'show-lex-rule)
                     )
               :available-p t)
         (make-lkb-submenu-item :menu-title "Parse"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Parse input..."
                        :value 'do-parse)
                     (make-menu-item :name "Show parse"
                        :value 'show-parse)
                     (make-menu-item :name "Show edge..."
                        :value 'show-parse-edge)
                     (make-menu-item :name "Show chart"
                        :value 'show-chart)
                     (make-menu-item :name "Print chart"
                        :value 'print-chart)
                     (make-menu-item :name "Batch parse..."
  		        :value 'parse-sentences)
		     (make-menu-item :name "Compare..."
                        :value 'compare-parses))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Generate"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Generate..."
                        :value 'generate-from-edge)
                     (make-menu-item :name "Show result"
                        :value 'show-gen-result)
                     (make-menu-item :name "Show edge..."
                        :value 'show-gen-edge)
                     (make-menu-item :name "Show chart"
                        :value 'show-gen-chart)
                     (make-menu-item :name "Print chart"
                        :value 'print-gen-chart))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Links"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Apply lexical rule..."
                        :value 'apply-lex)
                     (make-menu-item :name "Apply all lex rules..."
                        :value 'apply-lex-rules)
                     (make-menu-item :name "Unification check..."
                        :value 'interactive-unification-check)
                     )
               :available-p nil)           
            (make-menu-item :name "Tidy up"
               :value 'clear-non-parents
               :available-p nil) 
            (make-lkb-submenu-item :menu-title "Output"
                 :menu-items 
                  (list                     
                     (make-menu-item :name "Dump system..."
                        :value 'dump-lkb)
;                     (make-menu-item :name "Types with glbs..."
;                        :value 'output-type-file 
;                        :available-p nil)
                     (make-menu-item :name "Display settings"
                                     :value 'output-display-settings)
                     )
		  :available-p nil)
	    (make-lkb-submenu-item 
	     :menu-title "Options"
	     :menu-items                       
	     (list 
	      (make-menu-item :name "Set options"
			      :available-p t
			      :value 'get-parameters))
	     :available-p t)))))


