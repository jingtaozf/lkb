;;; Copyright (c) 1991--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

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

(defun make-tree-text-style nil
  (clim:parse-text-style 
   (list 
    :sans-serif :roman *type-tree-font-size*)))

(defconstant *tree-node-sep* 6
  "Spacing between nodes in a single generation.")

(defconstant *tree-level-sep* 12
  "Spacing between levels in the tree.")


(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (defstruct hier-node
    "Data structure for type-hierarchy nodes."
    (name "")				; Node label
    (type-entry nil)			; type record for node
    (children nil)))

;; Close function

(defvar *type-hierarchy-frames* nil)

;;; frames are pushed on to this list when created
;;; by display-type-hierarchy.  Would be tidier if they were
;;; removed by the quit function, but doesn't seem to be necessary

(defun close-existing-type-hierarchy-trees nil
  (loop for frame in *type-hierarchy-frames*
       do
       (clim:execute-frame-command frame '(clim-user::com-close-to-replace)))
  (setf *type-hierarchy-frames* nil))

;;
;; Open a type-hierarchy-window
;;

(defun create-type-hierarchy-tree (&optional (type *toptype*) old-window 
					     show-all-p)
  ;; if show-all-p is true then we never hide any nodes. If it's false
  ;; then we call hide-in-type-hierarchy-p on each type to see whether
  ;; it should be hidden
  (loop for name in *type-names*
      do 
	(unless (symbolp name)
	  (let ((real-thing name))
	    (setq name (intern (princ-to-string name)))
	    (setf (get name 'real-thing) real-thing))) 
	(setf (get name 'daughters) nil))
  (clear-type-visibility)
  (propagate-visibility-in-type-tree type)
  (let ((node (car (make-new-type-tree type show-all-p t))))
    (display-type-hierarchy 
     node (format nil "Type hierarchy below ~(~A~)" type) t old-window
     show-all-p)))

;;; initially all nodes are marked not visible. If we're not a shrunk node,
;;; go on to attempt to mark each daughter as visible
;;; If we're marked as visible then daughters must have been done already
;;; If we start below a shrunk node then nodes are visible despite this

(defun propagate-visibility-in-type-tree (type)
   (let ((type-record (get-type-entry type)))
      (when (and (not (type-shrunk-p type-record))
                 (not (type-visible-p type-record)))
         (loop for daughter in (type-daughters type-record)
            do
            (propagate-visibility-in-type-tree daughter)))
      (setf (type-visible-p type-record) t)))


(defun make-new-type-tree (type show-all-p toplevel-p)
   ;; make sure that top type is not hidden, no matter what
   ;; hide-in-type-hierarchy-p function says - otherwise we may end up
   ;; displaying no hierarchy at all (if all descendents are hidden), or just
   ;; one branch rather than all
   (let ((type-record (get-type-entry type)))
     (when (type-visible-p type-record)
       (if (and (not toplevel-p) (not show-all-p)
		(fboundp 'hide-in-type-hierarchy-p)
		(funcall (symbol-function 'hide-in-type-hierarchy-p) type))
	   (mapcan #'(lambda (d) 
		       (make-new-type-tree d show-all-p nil))
		   (type-daughters type-record))
	 (let ((node
		(if (symbolp type) type
		  (let ((new (intern (princ-to-string type))))
		    (setf (get new 'real-thing) type)
		    new))))
	   (unless (get node 'daughters)
	     (setf (get node 'daughters)
	       (delete-duplicates
		(mapcan
		 #'(lambda (d) (make-new-type-tree d show-all-p nil))
		 (type-daughters type-record)) :test #'eq)))
	   (list node))))))

;;
;; Define a frame class for our tree window
;;

(define-lkb-frame type-hierarchy
  ((nodes :initform nil
	  :accessor type-hierarchy-nodes)
   (show-all-p :initform nil
	       :accessor type-hierarchy-show-all-p))
  :display-function 'draw-type-hierarchy
  :width *window-width* 
  :height *window-height*
  :text-style (make-tree-text-style))

(defun display-type-hierarchy (node title horizontalp existing show-all-p)
  (declare (ignore horizontalp))
  (if existing
      (progn
        (setf (type-hierarchy-nodes existing) node)
	(setf (clim:frame-pretty-name existing) title)
        (clim:redisplay-frame-panes existing :force-p t))
      (mp:run-function "Type Hierarchy" 
                               #'display-type-hierarchy-really
                               node title show-all-p)))

(defun display-type-hierarchy-really (node title show-all-p)
  (let ((thframe (clim:make-application-frame 'type-hierarchy)))
    (push thframe *type-hierarchy-frames*)
    (setf (type-hierarchy-nodes thframe) node)
    (setf (type-hierarchy-show-all-p thframe) show-all-p)
    (setf (clim:frame-pretty-name thframe) title)
    (clim:run-frame-top-level thframe)))


(defun draw-type-hierarchy (type-hierarchy stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let* ((node-tree (type-hierarchy-nodes type-hierarchy))
         (viewport (clim:pane-viewport stream))
         (x (if viewport (clim:bounding-rectangle-min-x viewport)))
         (y (if viewport (clim:bounding-rectangle-min-y viewport))))
    (silica:inhibit-updating-scroll-bars #+:allegro (stream)
      (clim:format-graph-from-root
       node-tree
       #'(lambda (node s)
	   (let ((pos (current-position s)))
	     (clim:with-output-as-presentation (stream node 'hier-node)
	       (clim:with-drawing-options (stream :ink clim:+red+)
		 (write-string (type-node-text-string node) s)))
	     (when (type-shrunk-p
		    (or (get-type-entry node) 
			(get-type-entry (get node 'real-thing))))
	       (frame-text-box s pos (current-position s)))))
       #'(lambda (node) (get node 'daughters))
       :stream stream 
       :merge-duplicates t
       ;; CLIM bug -  duplicate-key duplicate-test are missing
       ;; :arc-drawer #'store-and-draw
       :orientation :horizontal 
       :generation-separation *tree-level-sep*
       :within-generation-separation *tree-node-sep*
       :center-nodes nil))
    (when (and x y)
      (clim:scroll-extent stream x y))))

(defparameter *node-text-scratch-string*
    (make-array 32 :element-type 'character :fill-pointer 0))

(defun type-node-text-string (node)
   (#+:allegro excl:without-interrupts   ; the code in here isn't re-entrant
    #+:lispworks mp:without-interrupts
    #+:mcl without-interrupts
    #-(or :allegro :lispworks mcl) 
    (error "no known without-interrupts(); see `tree.lsp'")
      (let* ((str *node-text-scratch-string*)
             (full-string (symbol-name node))
             (full-length (length full-string))
             (len (min full-length 32)))
         (setf (fill-pointer str) len)
         (dotimes (n len)
            (setf (char str n) (char-downcase (char full-string n))))
         (when (> full-length 30) (setf (subseq str 29) "...")) ; '...'
         str)))

;;
;; Make nodes active
;;

(define-type-hierarchy-command (com-type-hier-menu) 
    ((node 'hier-node :gesture :select))
  (clim:with-application-frame (frame)
    (unhighlight-objects frame)
    (let ((type-entry (get-type-entry node)))
      (pop-up-menu 
       `(;; ("Help" :value help 
	 ;;  :active ,(type-comment type-entry))
	 ("Shrink/expand" :value shrink
			  :active ,(type-daughters type-entry))
	 ("Type definition" :value def)
	 ("Expanded type" :value exp)
	 ("New hierarchy" :value new))
       (help (display-type-comment node (type-comment type-entry)))
       (shrink (setf (type-shrunk-p type-entry) 
		 (not (type-shrunk-p type-entry)))
	       (create-type-hierarchy-tree (type-hierarchy-nodes frame) frame
					   (type-hierarchy-show-all-p frame)))
       (def (show-type-spec-aux node type-entry))
       (exp (show-type-aux node type-entry))
       (new
	(let ((*last-type-name* (type-name type-entry)))
	  (declare (special *last-type-name*))
	  (multiple-value-bind (type show-all-p)
	      (ask-user-for-type nil 
				 '("Show all types?" . :check-box)
				 '("Ignore 300 descendant limit" . :check-box))
	    (when type
	      (let ((type-entry (get-type-entry type)))
		(when type-entry 
		  (create-type-hierarchy-tree type nil show-all-p)))))))))))
 
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
;;; window's top type then we show a new hierarchy

(defvar *needs-redisplay* nil)

(defun display-type-in-tree (node &optional scroll-onlyp)
  (let* ((frame (reuse-frame 'type-hierarchy))
	 (type-entry
	  (or (get-type-entry node)
	      (get-type-entry (get node 'real-thing))))
	 (type (type-name type-entry))
	 (top-type (if frame
		       (type-hierarchy-nodes frame) 
		     *toptype*))
	 (*needs-redisplay* nil))
    (when type-entry
      (if (and frame
	       (or (eq type top-type)
		   (member type-entry (retrieve-descendants top-type))))
          ;; ensure the type will be visible, whether or not it is now
          (progn
            (unshrink-ancestors type-entry top-type)
            (when *needs-redisplay*
              (create-type-hierarchy-tree (type-hierarchy-nodes frame) frame
                                          (type-hierarchy-show-all-p frame)))
            (let* ((stream (clim:frame-standard-output frame))
                   (record (find-object stream #'(lambda (t1) (eq t1 type)))))
              (when record
                (scroll-to record stream)
                (highlight-objects (list record) frame))))
        (unless scroll-onlyp
          (create-type-hierarchy-tree type nil t))))))
        

(defun unshrink-ancestors (type-entry top-type)
  ;; can't just use type-ancestors list since we have to stop at top-type arg
  (unless (eql (type-name type-entry) top-type)
    (loop for parent in (type-parents type-entry)
         do
         (let ((parent-entry (get-type-entry parent)))
	   (when (type-shrunk-p parent-entry)
	     (setq *needs-redisplay* t))
	   (setf (type-shrunk-p parent-entry) nil)
	   (unshrink-ancestors parent-entry top-type)))))


;; ----------------------------------------------------------------------
;; Draw type hierarchy using daVinci

(defvar *davinci-nodes* nil)

(defun davinci (&optional (type *toptype*) old-window show-all-p)
  (declare (ignore old-window))
  ;; if show-all-p is true then we never hide any nodes. If it's false
  ;; then we call hide-in-type-hierarchy-p on each type to see whether
  ;; it should be hidden
  (dolist (name *type-names*)
    (unless (symbolp name)
      (let ((real-thing name))
	(setq name (intern (princ-to-string name)))
	(setf (get name 'real-thing) real-thing))) 
    (setf (get name 'daughters) nil))
  (clear-type-visibility)
  (propagate-visibility-in-type-tree type)
  (let ((node (car (make-new-type-tree type show-all-p t)))
	(*davinci-nodes* (make-hash-table :test #'equal)))
    (with-open-file (stream "~/test.daVinci" :direction :output 
		     :if-exists :supersede)
      (write-char #\[ stream)
      (davinci-node stream node)
      (write-char #\] stream)))
  t)

(defun davinci-node (stream node)
  (let ((name (symbol-name node)))
    (unless (gethash name *davinci-nodes*)
      (format stream "l(\"~a\",n(\"\"," name)
      (format stream "[a(\"OBJECT\",\"~a\"),a(\"_GO\",\"text\")],[" name)
      (dolist (node2 (get node 'daughters))
	(let ((name2 (symbol-name node2)))
	  (format stream 
		  "l(\"~a->~a\",e(\"\",[a(\"_DIR\",\"none\")],r(\"~a\")))," 
		  name name2 name2)))
      (write-string "]))," stream)
      (terpri stream)
      (setf (gethash name *davinci-nodes*) t)
      (dolist (node2 (get node 'daughters))
	(davinci-node stream node2)))))

#|
;; ----------------------------------------------------------------------
;; Draw type hierarchy using dot

;; Remove edges which are implied by transitivity
(defun detrans (node)
  (let ((type-record 


(defun dot (&optional (type *toptype*) show-all-p)
  ;; if show-all-p is true then we never hide any nodes. If it's false
  ;; then we call hide-in-type-hierarchy-p on each type to see whether
  ;; it should be hidden
  (dolist (name *type-names*)
    (unless (symbolp name)
      (print "?")
      (let ((real-thing name))
	(setq name (intern (princ-to-string name)))
	(setf (get name 'real-thing) real-thing))) 
    (setf (get name 'daughters) nil))
  (clear-type-visibility)
  (propagate-visibility-in-type-tree type)
  (let ((node (detrans (car (make-new-type-tree type show-all-p t))))
	(node-hash (make-hash-table :test #'equal)))
    (with-open-file (stream "~/test.dot" :direction :output 
		     :if-exists :supersede)
      (write-line "digraph G {" stream)
      (dot-node stream node node-hash)
      (write-line "}" stream)))
  t)

(defun dot-node (stream node node-hash)
  (let ((name (symbol-name node)))
    (unless (gethash name node-hash)
      (dolist (node2 (get node 'daughters))
	(let ((name2 (symbol-name node2)))
	  (format stream "~a -> ~a;~%" (string-downcase name)
		  (string-downcase name2))))
      (setf (gethash name node-hash) t)
      (dolist (node2 (get node 'daughters))
	(dot-node stream node2 node-hash)))))
|#