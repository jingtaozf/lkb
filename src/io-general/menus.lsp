#+(and :allegro :clim)(in-package :clim-user)
#-(and :allegro :clim)(in-package :cl-user)
#+:allegro
(eval-when 
    (compile load eval)
  (shadowing-import '(cl-user::read-script-file
                      cl-user::reload-script-file
                      cl-user::read-type-patch-files
                      cl-user::reload-leaf-files
                      cl-user::reload-lex-files
                      cl-user::reload-grammar-rules
                      cl-user::reload-lexical-rules
                      cl-user::reload-template-files
                      cl-user::reload-psort-files
		      ;; view
		      cl-user::show-type-tree
		      cl-user::show-type-spec 
		      cl-user::show-type 
		      cl-user::show-lex
		      cl-user::show-words 
		      cl-user::show-grammar-rule 
		      cl-user::show-lex-rule 
		      ;; parse
		      cl-user::do-parse 
		      cl-user::show-parse 
		      cl-user::show-parse-edge 
		      cl-user::show-chart 
		      cl-user::parse-sentences 
		      cl-user::compare-parses
                      ;; generate
		      cl-user::generate-from-edge
                      cl-user::show-gen-result
                      cl-user::show-gen-edge
		      cl-user::show-gen-chart
		      cl-user::print-gen-chart
                      cl-user::index-for-generator
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
                      ;; debug
                      cl-user::print-chart
                      cl-user::batch-check-lexicon
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
                   (make-lkb-submenu-item :menu-title "Load"
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Complete grammar..."
                                                           :value 'read-script-file 
                                                           :available-p t)
                                           (make-menu-item :name "Reload grammar"
                                                           :value 'reload-script-file 
                                                           :available-p nil))
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
                                           (make-menu-item :name "Lex entry..."
                                                           :value 'show-lex)
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
                                           (make-menu-item :name "Redisplay parse"
                                                           :value 'show-parse)
                                           (make-menu-item :name "Show chart"
                                                           :value 'show-chart)
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
                                           )
                                          :available-p nil) 
                   (make-lkb-submenu-item :menu-title "Debug"
                                          :available-p nil
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Check lexicon"
                                                           :value 'batch-check-lexicon :available-p nil)
                                           (make-menu-item :name "Print chart"
                                                           :value 'print-chart :available-p nil)))
                   (make-lkb-submenu-item :menu-title "Options"
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Expand menu"
                                                           :value 'expand-lkb-menu :available-p t)
                                           (make-menu-item :name "Set options..."
                                                           :value 'get-parameters :available-p t)
                                           (make-menu-item :name "Save display settings..."
                                                           :value 'output-display-settings)
                                           (make-menu-item :name "Load display options..."
                                                           :value 'load-display-settings))
                                          :available-p t)))))


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
                     (make-menu-item :name "Reload grammar"
                                     :value 'reload-script-file 
                                     :available-p nil)
                     (make-menu-item :name "Reload constraints"
                                     :value 'read-type-patch-files
                                     :available-p nil)
                     (make-menu-item :name "Reload leaf types"
                                     :value 'reload-leaf-files
                                     :available-p nil)
                     (make-menu-item :name "Reload lexicon"
                        :value 'reload-lex-files :available-p nil)
                     (make-menu-item :name "Reload grammar rules"
                        :value 'reload-grammar-rules :available-p nil)
                     (make-menu-item :name "Reload lexical rules"
                        :value 'reload-lexical-rules :available-p nil)
                     (make-menu-item :name "Reload tree nodes"
                        :value 'reload-template-files :available-p nil)
                     (make-menu-item :name "Reload other instances"
                        :value 'reload-psort-files :available-p nil)
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
                     (make-menu-item :name "Lex entry..."
                        :value 'show-lex)
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
                     (make-menu-item :name "Redisplay parse"
                        :value 'show-parse)
                     (make-menu-item :name "Show chart"
                        :value 'show-chart)
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
                     (make-menu-item :name "Redisplay result"
                        :value 'show-gen-result)
                     (make-menu-item :name "Show chart"
                                     :value 'show-gen-chart)
                     (make-menu-item :name "Index"
                        :value 'index-for-generator))
               :available-p nil)
         (make-lkb-submenu-item :menu-title "Tests"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Apply lexical rule..."
                        :value 'apply-lex)
                     (make-menu-item :name "Apply all lex rules..."
                        :value 'apply-lex-rules)
                     )
                  :available-p nil)           
         (make-lkb-submenu-item :menu-title "Debug"
                                :available-p nil
                                :menu-items
                                (list
                                 (make-menu-item :name "Check lexicon"
                                                 :value 'batch-check-lexicon :available-p nil)
                                 (make-menu-item :name "Print chart"
                                                 :value 'print-chart :available-p nil)))
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
                     )
		  :available-p nil)
	    (make-lkb-submenu-item 
	     :menu-title "Options"
	     :menu-items                       
	     (list 
              (make-menu-item :name "Shrink menu"
                                    :value 'shrink-lkb-menu :available-p t)
	      (make-menu-item :name "Set options"
			      :available-p t
			      :value 'get-parameters)
              (make-menu-item :name "Save display settings..."
                                                           :value 'output-display-settings)
              (make-menu-item :name "Load display options..."
                                                           :value 'load-display-settings))
	     :available-p t)))))


