;;; Copyright Ann Copestake 1992-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

(in-package :user)

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

(defun create-type-hierarchy-tree (&optional (type *toptype*) old-window 
					     show-all-p)
   ;; if show-all-p is true then we never hide any nodes. If it's false then
   ;; we call hide-in-type-hierarchy-p on each type to see whether it should
   ;; be hidden
   (for name in *type-names*
      do 
      (unless (symbolp name)
         (let ((real-thing name))
            (setq name (intern (princ-to-string name)))
            (setf (get name 'real-thing) real-thing))) 
      (setf (get name 'daughters) nil))
   (clear-type-visibility)
   (propagate-visibility-in-type-tree type)
   (let ((node (car (make-new-type-tree type show-all-p))))
      (display-type-hierarchy node
         (format nil "Type hierarchy below ~(~A~)" type) t old-window)))

;;; initially all nodes are marked not visible. If we're not a shrunk node, go
;;; on to attempt to mark each daughter as visible If we're marked as visible
;;; then daughters must have been done already

(defun propagate-visibility-in-type-tree (type)
   (let ((type-record (get-type-entry type)))
      (when (and (not (type-shrunk-p type-record))
                 (not (type-visible-p type-record)))
         (for daughter in (type-daughters type-record)
            do
            (propagate-visibility-in-type-tree daughter)))
      (setf (type-visible-p type-record) t)))

(defun make-new-type-tree (type show-all-p)
   (let ((type-record (get-type-entry type)))
      (cond
         ((not (type-visible-p type-record))
            nil)
         ((and (not show-all-p)
             (fboundp 'hide-in-type-hierarchy-p)
             (funcall (symbol-function 'hide-in-type-hierarchy-p) type))
            (mapcan
               #'(lambda (d) (make-new-type-tree d show-all-p))
               (type-daughters type-record)))
         (t
            (let ((node
                   (if (symbolp type) type
                      (let ((new (intern (princ-to-string type))))
                         (setf (get new 'real-thing) type)
                         new))))
               (unless (get node 'daughters)
                  (setf (get node 'daughters)
                     (delete-duplicates
                        (mapcan
                           #'(lambda (d) (make-new-type-tree d show-all-p))
                           (type-daughters type-record)) :test #'eq)))
               (list node))))))


;;
;; Define a frame class for our tree window
;;

(clim:define-application-frame type-hierarchy ()
  ((nodes :initform nil
	  :accessor type-hierarchy-nodes))
  (:panes
   (display 
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-type-hierarchy
			  :text-cursor nil
			  :width *window-width* 
			  :height *window-height*
			  :text-style *tree-text-style*
			  :borders nil
			  :background clim:+white+
			  :foreground clim:+black+
			  :display-time nil))))))
  (:layouts
   (:default display)))

(defun display-type-hierarchy (node title horizontalp existing)
  (declare (ignore horizontalp))
  (if existing
      (progn
	(clim:enable-frame existing)
	(clim:raise-frame existing)
        (setf (type-hierarchy-nodes existing) node)
	(setf (clim:frame-pretty-name existing) title)
        (clim:redisplay-frame-panes existing :force-p t))
    (progn
      (setf *type-hier-frame* nil)
      (setf *type-hier-pane* nil)
      (let ((thframe (clim:make-application-frame 'type-hierarchy)))
        (setf (type-hierarchy-nodes thframe) node)
        (mp:process-run-function "Type Hierarchy" 
                                 #'clim:run-frame-top-level thframe)
	(setf (clim:frame-pretty-name thframe) title)
	(setf *type-hier-frame* thframe)
        (setf *type-hier-pane* (clim:get-frame-pane thframe 'display))))))


(defun draw-type-hierarchy (type-hierarchy stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (type-hierarchy-nodes type-hierarchy))
	(x (clim:bounding-rectangle-min-x (clim:pane-viewport stream)))
	(y (clim:bounding-rectangle-min-x (clim:pane-viewport stream))))
    (silica:inhibit-updating-scroll-bars (stream)
      (clim:format-graph-from-root
       node-tree
       #'(lambda (node s)
	   (clim:with-output-as-presentation (stream node 'hier-node)
	     (clim:with-drawing-options (stream :ink clim:+red+)
	       (write-string (type-node-text-string node) s))))
       #'(lambda (node) (get node 'daughters))
       :stream stream 
       :merge-duplicates t
       ;; CLIM bug -  duplicate-key duplicate-test are missing
       ;; :arc-drawer #'store-and-draw
       :orientation :horizontal 
       :generation-separation *tree-level-sep*
       :within-generation-separation *tree-node-sep*
       :center-nodes nil))
    (clim:scroll-extent stream x y)))

;;; Fix this?

(defparameter *node-text-scratch-string*
    (make-array 30 :element-type 'character :fill-pointer 0))

(defun type-node-text-string (node)
   (without-interrupts ; the code in here isn't re-entrant
      (let* ((str *node-text-scratch-string*)
             (full-string (symbol-name node))
             (full-length (length full-string))
             (len (min full-length 30)))
         (setf (fill-pointer str) len)
         (dotimes (n len)
            (setf (char str n) (char-downcase (char full-string n))))
         (when (> full-length 30) (setf (char str 29) (code-char 201))) ; '...'
         str)))

;; 
;; Add [EXIT] button
;;

(define-type-hierarchy-command (com-exit-tree :menu "Close")
    ()
  (setq *type-hier-frame* nil)
  (unhighlight-type *standard-output*)
  (clim:frame-exit clim:*application-frame*))

;;
;; Make nodes active
;;

(define-type-hierarchy-command (com-type-hier-menu)
    ((node 'hier-node :gesture :select))
  (unhighlight-type *standard-output*)
  (let* ((type-entry (get-type-entry node))
         (type (type-name type-entry))
	 (command (clim:menu-choose
                   (append '(("Help" :value help))
			   (when (type-daughters type-entry)
			       '(("Shrink/expand" :value shrink)))
			   '(("Type definition" :value def))
			   '(("Expanded type" :value exp))
			   '(("New hierarchy" :value new))))))
    (when command
          (handler-case
            (ecase command
	      (help 
	       (display-type-comment node (type-comment type-entry)))
	      (shrink   
	       (setf (type-shrunk-p type-entry) 
		 (not (type-shrunk-p type-entry)))
	       (create-type-hierarchy-tree 
		(type-hierarchy-nodes clim:*application-frame*)
		clim:*application-frame*))
	      (def                      
		  (if (type-constraint type-entry)
		      (display-fs-and-parents 
		       (type-local-constraint type-entry) 
		       (format nil 
			       "~(~A~)  - definition" 
			       node)
		       (type-parents type-entry))
		    (format t "~%No constraint for type ~A" node)))
	      (exp (if (type-constraint type-entry)
		       (display-fs-and-parents 
			(type-constraint type-entry) 
			(format nil 
				"~(~A~) - expanded" 
				type)
			(type-parents type-entry))
		     (format clim-user:*lkb-top-stream* 
			     "~%No constraint for type ~A" type)))
	      (new
	       (create-type-hierarchy-tree (type-name type-entry) nil)))
	    (error (condition)
	      (format clim-user:*lkb-top-stream*  
		      "~%Error: ~A~%" condition))))))
 
;;; NB Problems caused by having only 1 field per type for shrunk and visible
;;; flags and allowing multiple type windows on screen at once:
;;; shrinking/expanding a type in one window will give inconsistent
;;; expand/shrink behavour of that type if it appears in another window.  A
;;; type may be expanded automatically in the process of highlighting one of
;;; its descendents, which could also cause confusion wrt another window

;;; called from top level menu commands etc. Try to make type visible by
;;; unshrinking any ancestors if necessary - up to top type for this window if
;;; we currently have one on screen, and ask for type hierarchy window to be
;;; scrolled so given type is visible in centre, and the type highlighted. If
;;; we're looking in an existing window and the type isn't a descendent of the
;;; window's top type then we give up immediately. If there's not a hierarchy
;;; onscreen give up. User can always open one up from toplevel view menu

(defvar *needs-redisplay* nil)

(defvar *type-selected* nil)

(defun display-type-in-tree (node)
  (let* ((type-entry
	  (or (get-type-entry node)
	      (get-type-entry (get node 'real-thing))))
	 (type (type-name type-entry))
	 (top-type (if *type-hier-frame* 
		       (type-hierarchy-nodes *type-hier-frame*) 
		     *toptype*))
	 (*needs-redisplay* nil))
    (when (and type-entry
	       *type-hier-frame*
	       (or (eq type top-type)
		   (member type (retrieve-descendants top-type))))
      ;; ensure the type will be visible, whether or not it is now
      (unshrink-ancestors type-entry top-type)
      (when *needs-redisplay*
	(create-type-hierarchy-tree (type-hierarchy-nodes *type-hier-frame*)
				    *type-hier-frame*))
      (let* ((stream (clim:frame-standard-output *type-hier-frame*))
	     (record (find-type stream type)))
	(when record
	  (scroll-to record stream)
	  (highlight-type record stream))))))

(defun unshrink-ancestors (type-entry top-type)
  ;; can't just use type-ancestors list since we have to stop at top-type arg
  (unless (eql (type-name type-entry) top-type)
    (for parent in (type-parents type-entry)
         do
         (let ((parent-entry (get-type-entry parent)))
	   (when (type-shrunk-p parent-entry)
	     (setq *needs-redisplay* t))
	   (setf (type-shrunk-p parent-entry) nil)
	   (unshrink-ancestors parent-entry top-type)))))

;;; Search the display list for a type

(defun find-type (stream type)
  (catch 'found-type
    (find-type-1 (slot-value stream 'clim:output-record) stream type)))

(defun find-type-1 (rec stream type)
  (clim:map-over-output-records 
   #'(lambda (rec)
       (when (and (clim:presentationp rec) 
		  (eq type (clim:presentation-object rec)))
	 (throw 'found-type rec))
       (dolist (q (clim:output-record-children rec)) 
	 (find-type-1 q stream type)))
   rec))

;;; Show a highlighted type.

(defun highlight-type (type stream)
  (unhighlight-type stream)
  (setq *type-selected* 
    (clim:with-new-output-record (stream)
      (clim:with-output-recording-options (stream :record t)
	(multiple-value-bind (x1 y1 x2 y2)
	    (clim:bounding-rectangle* 
	     (clim:output-record-parent type))
	  (clim:draw-rectangle* stream x1 y1 x2 y2 :
				ink clim:+flipping-ink+ :filled t))))))

(defun unhighlight-type (stream)
  (when *type-selected*
    (clim:erase-output-record *type-selected* stream )))
