;;; Copyright Ann Copestake 1992-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; 1995 modified for MCL port

;;; 1996 modified to allow trees to be displayed without automatically
;;; added glb types


;;; 1996 rewritten for ACL port
;;; This version uses built in Allegro graph drawer rather than JAC's
;;; and is based on Rob's parse tree drawing code

(defconstant *window-width* 400
  "Initial width of tree window")

(defconstant *window-height* 400
  "Initial height of tree window")

(defconstant *tree-text-style* (clim:parse-text-style '(:sans-serif :roman 9))
  "Text style for node labels.")

(defconstant *tree-node-sep* 6
  "Spacing between nodes in a single generation.")

(defconstant *tree-level-sep* 12
  "Spacing between levels in the tree.")


(defstruct hier-node
  "Data structure for type-hierarchy nodes."
  (name "")				; Node label
  (type-entry nil)			; type record for node
  (children nil))

(defparameter *type-hier-frame* nil)

(defparameter *type-hier-pane* nil)


;; Close function - should be as in MCL version

(defun close-existing-type-hierarchy-trees nil
  nil)

;;
;; Open a type-hierarchy-window
;;

(defun create-type-hierarchy-tree (&optional hide-glb-types-p)
  (declare (ignore hide-glb-types-p))
  ; to do - hide-glb-types option
  (for type in *type-names*
       do
       (let ((type-entry (get-type-entry type)))
         (when type-entry
               (setf (mark-field-seen (type-marks type-entry)) nil))))
  (let ((top-hierarchy-node 
         (construct-type-hier-node *toptype*)))
; side effect is to construct children node
    (display-type-hierarchy top-hierarchy-node)
    nil))



(defun construct-type-hier-node (name)
  (let ((type-entry (get-type-entry name)))
    (or (mark-field-seen (type-marks type-entry))
        (let ((node
               (make-hier-node :name (type-hier-print-name name)
                               :type-entry type-entry
                               :children 
                               (if type-entry
                                   (for child in 
                                        (type-daughters type-entry)
                                        collect
                                        (construct-type-hier-node child))))))
          (setf (mark-field-seen (type-marks type-entry))
                node)
          node))))



(defun type-hier-print-name (name)
  (let ((strung (princ-to-string name)))
    (when (> (length strung) 20)
           (setf strung (concatenate 'string (subseq strung 0 19) "...")))
   (string-downcase strung)))


;;
;; Define a frame class for our tree window
;;

(clim:define-application-frame type-hierarchy ()
  ((nodes :initform nil
   :accessor type-hierarchy-nodes))
  (:panes
   (display :application
	    :display-function 'draw-type-hierarchy
	    :text-cursor nil
	    :width *window-width* :height *window-height*
	    :text-style *tree-text-style*
	    :borders nil
            :display-after-commands nil))
  (:layouts
    (:default display)))

(defparameter *node-positions* (make-hash-table :test #'equal))

(defun display-type-hierarchy (node)
  (when (and *type-hier-frame* 
             (eql (clim:frame-state *type-hier-frame*) :shrunk))
        (clim:note-frame-deiconified 
         (clim:frame-manager *type-hier-frame*)
         *type-hier-frame*))
  (if (and *type-hier-frame* 
           (eql (clim:frame-state *type-hier-frame*) :enabled))
      (progn
        (clim:window-clear *type-hier-pane*)
        (setf (type-hierarchy-nodes *type-hier-frame*) node)
        (clim:redisplay-frame-panes *type-hier-frame* :force-p t))
    (progn
      (setf *type-hier-frame* nil)
      (setf *type-hier-pane* nil)
      (clrhash *node-positions*)
      (let ((thframe (clim:make-application-frame 'type-hierarchy)))
        (setf (type-hierarchy-nodes thframe) node)
        (mp:process-run-function "Type Hierarchy" 
                                 #'clim:run-frame-top-level thframe)
        (setf *type-hier-frame* thframe)
        (setf *type-hier-pane* (clim:get-frame-pane thframe 'display))))))
    


(defun draw-type-hierarchy (type-hierarchy
                                stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (type-hierarchy-nodes type-hierarchy)))
    (clim:format-graph-from-root
     node-tree
     #'(lambda (node s)
	 (if (hier-node-type-entry node)
	     (clim:with-output-as-presentation (stream node 'hier-node)
	       (write-string (hier-node-name node) s))
	   (write-string (hier-node-name node) s)))
     #'hier-node-children
     :stream stream 
     :merge-duplicates t
;;; CLIM bug -  duplicate-key duplicate-test are missing
     :arc-drawer #'store-and-draw
     :orientation :horizontal 
     :generation-separation *tree-level-sep*
     :within-generation-separation *tree-node-sep*
     :center-nodes nil)))



(defun store-and-draw (stream from-node to-node x1 y1 x2 y2
                              &rest drawing-options)
  (declare (ignore from-node))
; store positions so we can scroll
; to a given type
  (when (hier-node-p to-node)
   (setf (gethash (hier-node-name to-node) *node-positions*)
         (list x2 y2)))
  (apply #'clim:draw-line* stream x1 y1 x2 y2 drawing-options))


;; 
;; Add [EXIT] button
;;


(define-type-hierarchy-command (com-exit-tree :menu "Close")
    ()
  (clim:frame-exit clim:*application-frame*))


;;
;; Make nodes active
;;

(define-type-hierarchy-command (com-type-hier-menu)
    ((node 'hier-node :gesture :select))
  (let* ((type-entry (hier-node-type-entry node))
         (type (type-name type-entry))
         (command (clim:menu-choose
                   '(("Help" :value help)
                     ("Shrink/expand" :value shrink)
                     ("Type definition" :value def)
                     ("Expanded type" :value exp)))))
    (when command
          (handler-case
            (ecase command
                   (help (display-type-comment type 
                                  (type-comment type-entry)))
                   (shrink (lkb-beep))
                   (def                      
                     (if (type-constraint type-entry)
                         (display-fs-and-parents 
                          (type-local-constraint type-entry) 
                          (format nil 
                                  "~(~A~)  - definition" 
                                  type)
                          (type-parents type-entry))
                       (format clim-user:*lkb-top-stream* 
                               "~%No constraint for type ~A" type)))
                   (exp (if (type-constraint type-entry)
                            (display-fs-and-parents 
                             (type-constraint type-entry) 
                                   (format nil 
                                           "~(~A~) - expanded" 
                                           type)
                                   (type-parents type-entry))
                          (format clim-user:*lkb-top-stream* 
                                  "~%No constraint for type ~A" type))))
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))
 
;;; called from top level menu commands etc

(defun display-type-in-tree (type)
  (let ((type-entry (get-type-entry type)))
    (when type-entry
; eventually need to treat shrunk types
          (let ((type-name (type-hier-print-name type)))
            (when (and *type-hier-frame*
                   (eql (clim:frame-state *type-hier-frame*) :shrunk))
                   (clim:note-frame-deiconified 
                    (clim:frame-manager *type-hier-frame*)
                    *type-hier-frame*))
                    ; fn documented in release notes for 4.3
            (if (and *type-hier-frame*
                  (eql (clim:frame-state *type-hier-frame*) :enabled))
                (progn
                   (scroll-to-type type-name *type-hier-pane*)
                   (clim:window-expose *type-hier-pane*))
              (create-type-hierarchy-tree))))))
; we're not scrolling because it's likely that the scrolling won't happen
; at the right time because of multiprocessing.  But this is
; only a problem if the damnfool user has closed the window


(defun scroll-to-type (type-name pane)
  (let* ((bounding-rect (clim:pane-viewport-region pane))
         (width (clim:bounding-rectangle-width bounding-rect))
         (height (clim:bounding-rectangle-height bounding-rect))
         (position (gethash type-name *node-positions*))
         (half-text-height (floor (text-font-height pane) 2)))
    (when position
          (clim:scroll-extent pane
                  (max 0 (- (car position) (floor width 2)))
                  (max 0 (- (cadr position) (floor height 2))))
; next stuff highlights temporarily - too difficult to
; get a more permanent reverse and remove it at the right time
          (clim:draw-rectangle* pane
                   (car position) 
                   (max 0 (- (cadr position) half-text-height))
                   (+ (clim:stream-string-width pane type-name) (car position)) 
                   (+ (cadr position) half-text-height)
                   :filled t :ink clim:+flipping-ink+)
          (sleep 1)
          (clim:draw-rectangle* pane
                   (car position) 
                   (max 0 (- (cadr position) half-text-height))
                   (+ (clim:stream-string-width pane type-name) (car position)) 
                   (+ (cadr position) half-text-height)
                   :filled t :ink clim:+flipping-ink+))))
      
