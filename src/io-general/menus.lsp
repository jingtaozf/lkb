#+:clim (in-package :clim-user)
#-:clim (in-package :lkb)

#+:clim
(eval-when 
    (compile load eval)
  (shadowing-import '(lkb::read-script-file
                      lkb::reload-script-file
                      lkb::read-type-patch-files
                      lkb::reload-leaf-files
                      lkb::reload-lex-files
                      lkb::reload-grammar-rules
                      lkb::reload-lexical-rules
                      lkb::reload-template-files
                      lkb::reload-psort-files
		      ;; view
		      lkb::show-type-tree
		      lkb::show-type-spec 
		      lkb::show-type 
		      lkb::show-lex
		      lkb::show-words 
		      lkb::show-grammar-rule 
		      lkb::show-lex-rule
                      lkb::display-lex-words
                      ;; parse
                      #+:ltemplates
                      lkb::parse-with-preprocessor
		      lkb::show-parse 
		      lkb::show-chart 
		      lkb::compare-parses
                      ;; mrs
                      lkb::read-mrs-rule-file
                      lkb::clear-mrs-rules
                      lkb::choose-mrs-output-level
                      ;; generate
                      lkb::show-gen-result
		      lkb::show-gen-chart
                      lkb::index-for-generator
                      lkb::read-gen-rule-file
                      lkb::clear-gen-rules
                      lkb::index-for-generator
		      ;; link
		      lkb::apply-lex 
		      lkb::apply-lex-rules 
		      ;; tidy
		      lkb::clear-non-parents
                      lkb::interactive-create-check-paths
		      ;; output
		      lkb::output-type-file 
                      lkb::output-display-settings
                      lkb::load-display-settings
		      lkb::*lkb-system-version*
                      ;; debug
                      lkb::print-chart-toplevel
                      lkb::print-gen-chart-toplevel
                      lkb::batch-check-lexicon
		      ;; options
		      lkb::get-parameters
                      lkb::find-type-from-features)))

;;; Menus moved to here from topmenu.lsp, since they can be
;;; treated as independent between ACL and MCL

(defun create-mini-lkb-system-menu nil
  ;;; cut down system for teaching purposes
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
                                                           :available-p :grammar-file))
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
                                                           :value 'print-chart-toplevel 
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
                                     :available-p :grammar-file)
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
                     (make-menu-item :name "All words"
                        :value 'display-lex-words)
                     )
               :available-p :always)
         (make-lkb-submenu-item :menu-title "Parse"
                 :menu-items                       
                  (list 
                     (make-menu-item :name "Parse input..."
                        :value 'do-parse-batch)
                     #+:ltemplates
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
                                                 :value 'print-chart-toplevel :available-p :grammar)
                                 (make-menu-item :name "Print generator chart"
                                                 :value 'print-gen-chart-toplevel :available-p :mrs)))
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


