;;; Copyright Ann Copestake 1992-1997
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
        
;;; ACL version

(defparameter *parse-window-width* 400
  "Initial width of tree window")

(defparameter *parse-window-height* 400
  "Initial height of tree window")

(defparameter *ptree-text-style* 
  (clim:parse-text-style (list :sans-serif :roman (or *parse-tree-font-size* 9)))
  "Text style for node labels.")

(defparameter *ptree-node-sep* 6
  "Spacing between nodes in a single generation.")

(defparameter *ptree-level-sep* 12
  "Spacing between levels in the tree.")                    
    
(defstruct ptree-node
  "Data structure for parse-tree nodes."
  (name "")				; Node label
  (pstruct nil)			; record for node
  (children nil))
               
      
(defun display-parse-tree (edge &optional jac-param)
  ;;; JAC added an extra arg to this - need to find out why
  ;;; but meanwhile, just allow it so that tree-nodes.lsp stays
  ;;; graphics independent
   ;;; takes an edge and builds the tree below it for input
   ;;; to ACL graph package - then displays it
   ;;; with active nodes
  (declare (ignore jac-param))
   (let* ((edge-id (edge-id edge))
         (edge-symbol 
            (make-edge-symbol
               edge-id))
         (top-ptree-node 
            (make-new-parse-tree edge-symbol edge)))
       (draw-new-parse-tree top-ptree-node 
                            (format nil "Edge ~A" edge-id))))

;;; make-edge-symbol is in tree-nodes.lsp   

(defun make-new-parse-tree (edge-symbol edge-record)
  (make-ptree-node 
   :name edge-symbol
   :pstruct edge-record
   :children 
   (if edge-record
       (let ((daughters (edge-children edge-record)))
         (if daughters
             (for daughter in daughters
                  collect
                  (let ((daughter-edge-symbol 
                         (make-edge-symbol 
                          (edge-id daughter))))
                    (make-new-parse-tree 
                     daughter-edge-symbol daughter)))
           (let ((leaf-node (make-edge-symbol 
                             (car (edge-leaves edge-record)))))
             (list (make-ptree-node :name leaf-node
                              :pstruct nil 
                              :children
                              (if *dont-show-morphology* nil
                                (let ((mdaughter 
                                       (edge-morph-history edge-record)))
                                  (if mdaughter
                                      (let ((daughter-edge-symbol 
                                             (make-edge-symbol 
                                              (edge-id mdaughter) t)))         
                                        (make-morph-tree daughter-edge-symbol
                                                         mdaughter)))))))))))))

(defun make-morph-tree (edge-symbol edge-record)
  (list
  (make-ptree-node :name edge-symbol
                   :pstruct edge-record
                   :children
                   (if edge-record
                       (let ((mdaughter (edge-morph-history edge-record)))
                         (if mdaughter
                             (let ((daughter-edge-symbol 
                                    (make-edge-symbol (edge-id mdaughter) t)))
                               (make-morph-tree daughter-edge-symbol
                                                mdaughter))))))))

    


;;
;; Define a frame class for our parse tree window
;;

(clim:define-application-frame parse-tree ()
  ((nodes :initform nil
   :accessor parse-tree-nodes))
  (:panes
   (display :application
	    :display-function 'draw-parse-tree
	    :text-cursor nil
	    :width *parse-window-width* :height *parse-window-height*
	    :text-style *ptree-text-style*
	    :borders nil
            :display-after-commands nil))
  (:layouts
    (:default display)))

(defun draw-new-parse-tree (topnode title)
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (mp:process-run-function title 
                             #'clim:run-frame-top-level pframe)))



(defun draw-parse-tree (ptree-frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (parse-tree-nodes ptree-frame)))
    (clim:format-graph-from-root
     node-tree
     #'(lambda (node s)
	 (if (ptree-node-pstruct node)
	     (clim:with-output-as-presentation (stream node 'ptree-node)
                   (write-string
                      (princ-to-string (or 
                        (find-category-abb 
                         (edge-dag (ptree-node-pstruct node)))
                        (edge-category 
                         (ptree-node-pstruct node)))) s))
	   (write-string (princ-to-string (ptree-node-name node)) s)))
     #'ptree-node-children
     :stream stream 
     :merge-duplicates nil
     :orientation :vertical
     :generation-separation *ptree-level-sep*
     :within-generation-separation *ptree-node-sep*
     :center-nodes nil)))


;;; menus



;; 
;; Add [EXIT] button
;;


(define-parse-tree-command (com-exit-parse-tree :menu "Close")
    ()
  (clim:frame-exit clim:*application-frame*))


;;
;; Make nodes active
;;

(define-parse-tree-command (com-parse-tree-menu)
    ((node 'ptree-node :gesture :select))
  (let* ((edge-record (ptree-node-pstruct node))
         (command (clim:menu-choose
                   '(("Feature Structure" :value fs)
                     ("Rule" :value rule)))))
    (when command
          (handler-case
            (ecase command
                   (fs (display-fs (edge-dag edge-record)
                          (format nil "Edge ~A" (edge-id edge-record))))
                   (rule 
                    (let* ((rule-name (edge-rule-number edge-record))
                           (rule (or (get-grammar-rule-entry rule-name)
                                     (get-lex-rule-entry rule-name))))
                      (if rule
                            (display-fs (rule-full-fs rule)
                                        (format nil "~A" rule-name))
                        (let ((alternative (get-tdfs-given-id rule-name)))
                          (when alternative
                            (display-fs alternative
                                        (format nil "~A" rule-name))))))))
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))


