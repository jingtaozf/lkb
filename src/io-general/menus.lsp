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
                      #+:oe
                      cl-user::parse-with-preprocessor
		      cl-user::show-parse 
		      cl-user::show-chart 
		      cl-user::compare-parses
                      ;; mrs
                      cl-user::read-mrs-rule-file
                      cl-user::clear-mrs-rules
                      cl-user::choose-mrs-output-level
                      ;; generate
                      cl-user::show-gen-result
		      cl-user::show-gen-chart
                      cl-user::index-for-generator
                      cl-user::read-gen-rule-file
                      cl-user::clear-gen-rules
                      cl-user::index-for-generator
		      ;; link
		      cl-user::apply-lex 
		      cl-user::apply-lex-rules 
		      ;; tidy
		      cl-user::clear-non-parents
                      cl-user::interactive-create-check-paths
		      ;; output
		      cl-user::output-type-file 
                      cl-user::output-display-settings
                      cl-user::load-display-settings
		      cl-user::*lkb-system-version*
                      ;; debug
                      cl-user::print-chart
                      cl-user::print-gen-chart
                      cl-user::batch-check-lexicon
		      ;; options
		      cl-user::get-parameters
                      cl-user::find-type-from-features)))

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
                                                           :available-p :always)
                                           (make-menu-item :name "Reload grammar"
                                                           :value 'reload-script-file 
                                                           :available-p :grammar))
                                          :available-p :always)
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
                                          :available-p :always)
                   (make-lkb-submenu-item :menu-title "Parse"
                                          :menu-items                       
                                          (list 
                                           (make-menu-item :name "Parse input..."
                                                           :value 'do-parse-batch)
                                           (make-menu-item :name "Redisplay parse"
                                                           :value 'show-parse)
                                           (make-menu-item :name "Show parse chart"
                                                           :value 'show-chart)
                                           (make-menu-item :name "Batch parse..."
                                                           :value 'parse-sentences-batch))
                                          :available-p :grammar)
                   (make-lkb-submenu-item :menu-title "Debug"
                                          :available-p :grammar
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Check lexicon"
                                                           :value 'batch-check-lexicon 
                                                           :available-p :grammar)
                                           (make-menu-item :name "Print chart"
                                                           :value 'print-chart 
                                                           :available-p :grammar))
                                           )
                   (make-lkb-submenu-item :menu-title "Options"
                                          :menu-items
                                          (list
                                           (make-menu-item :name "Expand menu"
                                                           :value 'expand-lkb-menu 
                                                           :available-p :always)
                                           (make-menu-item :name "Set options..."
                                                           :value 'get-parameters 
                                                           :available-p :always)
                                           (make-menu-item :name "Save display settings..."
                                                           :value 'output-display-settings)
                                           (make-menu-item :name "Load display options..."
                                                           :value 'load-display-settings))
                                          :available-p :always)))))


(defun create-big-lkb-system-menu nil
  ;;; for system with MRS etc
   (setf *lkb-menu*
   (make-instance 'menu :menu-title "Lkb" :menu-items
         (list
              (make-lkb-submenu-item :menu-title "Load"
                 :menu-items
                  (list 
                     (make-menu-item :name "Complete grammar..."
                                     :value 'read-script-file :available-p :always)
                     (make-menu-item :name "Reload grammar"
                                     :value 'reload-script-file 
                                     :available-p :grammar)
                     (make-menu-item :name "Reload constraints"
                                     :value 'read-type-patch-files
                                     :available-p :grammar)
                     (make-menu-item :name "Reload leaf types"
                                     :value 'reload-leaf-files
                                     :available-p :grammar)
                     (make-menu-item :name "Reload lexicon"
                        :value 'reload-lex-files :available-p :grammar)
                     (make-menu-item :name "Reload grammar rules"
                        :value 'reload-grammar-rules :available-p :grammar)
                     (make-menu-item :name "Reload lexical rules"
                        :value 'reload-lexical-rules :available-p :grammar)
                     (make-menu-item :name "Reload tree nodes"
                        :value 'reload-template-files :available-p :grammar)
                     (make-menu-item :name "Reload other entries"
                        :value 'reload-psort-files :available-p :grammar)
                     )
                  :available-p :always)
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
               :available-p :always)
         (make-lkb-submenu-item :menu-title "Parse"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Parse input..."
                        :value 'do-parse-batch)
                     #+:oe
                     (make-menu-item :name "Parse file..."
                        :value 'parse-with-preprocessor)
                     (make-menu-item :name "Redisplay parse"
                        :value 'show-parse)
                     (make-menu-item :name "Show parse chart"
                        :value 'show-chart)
                     (make-menu-item :name "Batch parse..."
  		        :value 'parse-sentences-batch)
		     (make-menu-item :name "Compare..."
                        :value 'compare-parses))
                  :available-p :grammar)
         (make-lkb-submenu-item :menu-title "MRS"
                 :menu-items                       
                 (list 
                     (make-menu-item :name "Load munger..."
                                     :value 'read-mrs-rule-file
                                     :available-p :mrs)
                     (make-menu-item :name "Clear munger"
                                     :value 'clear-mrs-rules
                                     :available-p :mrs)
                     (make-menu-item :name "Output level"
                                     :value 'choose-mrs-output-level
                                     :available-p :mrs))
               :available-p :mrs)
         (make-lkb-submenu-item :menu-title "Generate"
                 :menu-items                       
                 (list 
                     (make-menu-item :name "Redisplay realisation"
                                     :value 'show-gen-result
                                     :available-p :mrs)
                     (make-menu-item :name "Show gen chart"
                                     :value 'show-gen-chart
                                     :available-p :mrs)
                     (make-menu-item :name "Load heuristics..."
                                     :value 'read-gen-rule-file
                                     :available-p :mrs)
                     (make-menu-item :name "Clear heuristics"
                                     :value 'clear-gen-rules
                                     :available-p :mrs)
                     (make-menu-item :name "Index"
                                     :value 'index-for-generator
                                     :available-p :mrs))
               :available-p :mrs)
         (make-lkb-submenu-item :menu-title "Debug"
                                :available-p :grammar
                                :menu-items
                                (list
                                 (make-menu-item :name "Check lexicon"
                                                 :value 'batch-check-lexicon :available-p :grammar)
                                 (make-menu-item :name "Find features' type..."
                                                 :value 'find-type-from-features :available-p :grammar)
                                 (make-menu-item :name "Print parser chart"
                                                 :value 'print-chart :available-p :grammar)
                                 (make-menu-item :name "Print generator chart"
                                                 :value 'print-gen-chart :available-p :mrs)))
         (make-lkb-submenu-item :menu-title "Advanced"
                 :menu-items 
                   (list                     
;;;                      (make-menu-item :name "Dump system..."
;;;                                      :value 'dump-lkb :available-p :always)
                      (make-menu-item :name "Tidy up"
                                      :value 'clear-non-parents
                                      :available-p :grammar)
                      (make-menu-item :name "Create quick check file..."
                                      :value 'interactive-create-check-paths
                                      :available-p :grammar)
;                     (make-menu-item :name "Types with glbs..."
;                        :value 'output-type-file 
;                        :available-p :grammar)
                     )
		  :available-p :always)
	    (make-lkb-submenu-item 
	     :menu-title "Options"
	     :menu-items                       
	     (list 
              (make-menu-item :name "Shrink menu"
                                    :value 'shrink-lkb-menu :available-p :always)
	      (make-menu-item :name "Set options"
			      :available-p :always
			      :value 'get-parameters)
              (make-menu-item :name "Save display settings..."
                                                           :value 'output-display-settings)
              (make-menu-item :name "Load display options..."
                                                           :value 'load-display-settings))
	     :available-p :always)))))


