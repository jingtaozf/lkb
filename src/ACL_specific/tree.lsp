;;; Copyright (c) 1991--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

;;; 1995 modified for MCL port

;;; 1996 modified to allow trees to be displayed without automatically
;;; added glb types


;;; 1996 rewritten for ACL port
;;; This version uses built in Allegro graph drawer rather than JAC's
;;; and is based on Rob's parse tree drawing code

;;; *** temporary test, to allow this file to be loaded into an old LOGON LKB session

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*window-width*)
(defparameter *window-width* 500
  "Initial width of tree window")

(defparameter *window-height* 500
  "Initial height of tree window")

(defparameter *tree-node-sep* 6
  "Spacing between nodes in a single generation.")

(defparameter *tree-level-sep* #+:mcclim 20 #-:mcclim 12
  "Spacing between levels in the tree.")
  ))

(defun make-tree-text-style ()
  (clim:make-text-style :sans-serif :roman (or *type-tree-font-size* 12)))

(declaim (notinline make-tree-text-style))


(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (defstruct hier-node
    "Data structure for type-hierarchy nodes."
    (name "")				; Node label
    (type-entry nil)			; type record for node
    (children nil)))

(clim:define-presentation-type hier-node ())

;; Close function

(defvar *type-hierarchy-frames* nil)

;;; frames are pushed on to this list when created
;;; by display-type-hierarchy.  Would be tidier if they were
;;; removed when that window closes, but doesn't seem to be necessary

(defun close-existing-type-hierarchy-trees nil
  (loop for frame in *type-hierarchy-frames*
       do
       (clim:execute-frame-command frame '(clim-user::com-close-to-replace)))
  (setf *type-hierarchy-frames* nil))

;;
;; Open a type-hierarchy-window
;;

(defun create-type-hierarchy-tree (&optional (type *toptype*)
                                   existing
				   (show-all-p (if existing (type-hierarchy-show-all-p existing)))
                                   (display-definitions-p
                                     (if existing (type-hierarchy-display-definitions-p existing))))
  (clear-type-visibility)
  (propagate-visibility-in-type-tree type)
  (let ((node
          (car (make-new-type-tree type show-all-p))))
    (display-type-hierarchy
      node existing show-all-p display-definitions-p type)))

(defun propagate-visibility-in-type-tree (type)
  ;; initially all nodes are marked not visible. If we're not a shrunk node,
  ;; go on to attempt to mark each daughter as visible
  ;; If we're marked as visible then daughters must have been done already
  ;; If we start below a shrunk node then nodes are visible despite this
   (let ((type-record (get-type-entry type)))
      (when (and (not (ltype-shrunk-p type-record))
                 (not (ltype-visible-p type-record)))
         (loop for daughter in (ltype-daughters type-record)
            do
            (propagate-visibility-in-type-tree daughter)))
      (setf (ltype-visible-p type-record) t)))


;;; Compute the daughters and parents to show in the type hierarchy display. Goes both
;;; upwards and downwards in the hierarchy, and takes account of both 'hidden' types and
;;; shrunk types.

(defun make-new-type-tree (type show-all-p)
  ;; if show-all-p is true then we never hide any nodes. If it's false
  ;; then we call hide-in-type-hierarchy-p on each type to see whether
  ;; it should be hidden
  (let* ((hide-fn
           (and (not show-all-p) (fboundp 'hide-in-type-hierarchy-p)
              (symbol-function 'hide-in-type-hierarchy-p)))
         (ntypes (length *type-names*))
         (type-table (make-array ntypes :element-type 'bit))
         (index-to-type-name (make-array ntypes :element-type t))
         (hide-table (make-hash-table :test #'eq)))
    (flet
      ((caching-hide-fn (type)
         (multiple-value-bind (hidep foundp)
              (gethash type hide-table)
            (if foundp
                hidep
                (setf (gethash type hide-table) (funcall hide-fn type))))))
      (loop for name in *type-names*
            for ind from 0
            do 
	    (unless (symbolp name)
	      (let ((real-thing name))
	        (setq name (intern (princ-to-string name)))
	        (setf (get name 'real-thing) real-thing))) 
	    (setf (get name 'daughters) nil)
            (setf (get name 'type-index) ind)
            (setf (svref index-to-type-name ind) name))
       (make-new-type-tree-upwards
         type (if hide-fn #'caching-hide-fn) type-table index-to-type-name)
       (make-new-type-tree-downwards
         type (if hide-fn #'caching-hide-fn) type-table index-to-type-name)
       (list *toptype*))))

(defun make-new-type-tree-upwards (type hide-fn type-table index-to-type-name)
   (let ((ptypes nil))
      (labels
         ((non-hidden-parents (type)
            (loop for p in (ltype-parents (get-type-entry type))
               nconc
               (if (and (not (eq p *toptype*)) hide-fn (funcall hide-fn p))
                  (non-hidden-parents p)
                  (list p))))
          (make-new-type-tree-upwards-1 (type)
            ;; ignore any shrunk status otherwise might not reach *toptype*
            (let ((node
                     (if (symbolp type) type (intern (princ-to-string type)))))
               (dolist (parent (non-hidden-parents type))
                  (let ((p
                          (if (symbolp parent) parent (intern (princ-to-string parent)))))
                     (unless (get p 'daughters) ; processed already?
                        (push p ptypes)
                        (make-new-type-tree-upwards-1 p))
                     (push node (get p 'daughters)))))))
         (make-new-type-tree-upwards-1 type)
         (loop for p in ptypes
            do (setf (get p 'daughters)
                  (filter-descendant-daughters
                     (get p 'daughters) type-table index-to-type-name))))))

(defun make-new-type-tree-downwards (type hide-fn type-table index-to-type-name)
   ;; make sure that start type is not hidden, no matter what the
   ;; hide-in-type-hierarchy-p function says - otherwise we may end up
   ;; displaying nothing at all (if all its descendents are also hidden)
   (let ((done (make-hash-table :test #'eq)))
      (labels
         ((make-new-type-tree-downwards-1 (type toplevel-p)
            (let ((type-record (get-type-entry type)))
               (when (ltype-visible-p type-record) ; i.e. not shrunk
                  (let ((node
                           (if (symbolp type) type (intern (princ-to-string type)))))
                     (unless (gethash node done)
                        (let ((dlist
                                (loop for d in (ltype-daughters type-record)
                                   nconc
                                   (make-new-type-tree-downwards-1 d nil))))
                           (setf (get node 'daughters)
                              (if hide-fn
                                 (filter-descendant-daughters dlist
                                    type-table index-to-type-name)
                                 dlist))
                        (setf (gethash node done) t)))
                     (if (and (not toplevel-p) hide-fn (funcall hide-fn type))
                        (copy-list (get node 'daughters))
                        (list node)))))))
         (make-new-type-tree-downwards-1 type t))))

(defun filter-descendant-daughters (dlist type-table index-to-type-name)
   ;; dlist is a list of potential daughters. Remove duplicates, and also filter out
   ;; any potential daughter that is a descendant of another daughter in this list -
   ;; preventing display of spurious links caused by splicing out GLBs
   (declare (bit-vector type-table))
   (cond
      ((null (cdr dlist)) dlist)
      (t
         (fill type-table 0)
         (loop for d in dlist
            do
            (setf (sbit type-table (get d 'type-index)) 1))
         (loop
            with start = 0
            for n = (position 1 type-table :start start) ; iterate across 1's
            while n
            do
            (loop
               for desc in (ltype-descendants (get-type-entry (svref index-to-type-name n)))
               do
               (setf (sbit type-table (get (ltype-name desc) 'type-index)) 0))
            (setq start (1+ n)))
         ;; return types corresponding to 1's
         (loop
            with res = nil
            with start = 0
            for n = (position 1 type-table :start start)
            while n
            do
            (push (svref index-to-type-name n) res)
            (setq start (1+ n))
            finally (return res)))))

;;
;; Define a frame class for type hierarchy windows
;;

(define-lkb-frame type-hierarchy
  ((nodes :initform nil
	  :accessor type-hierarchy-nodes)
   (show-all-p :initform nil
	       :accessor type-hierarchy-show-all-p)
   (display-definitions-p :initform nil
	                  :accessor type-hierarchy-display-definitions-p)
   (focus-type :initform nil
	       :accessor type-hierarchy-focus-type)
   (scaling :initform 1.0
	    :accessor type-hierarchy-scaling))
  :display-function 'draw-type-hierarchy
  :text-style (make-tree-text-style)
  :width *window-width* 
  :height *window-height*)

(defun display-type-hierarchy (node existing show-all-p
                               display-definitions-p focus-type)
  (if existing
    (clim:redisplay-frame-panes existing :force-p t)
    (mp:run-function "Type Hierarchy" 
      #'display-type-hierarchy-really
      node
      (format nil "Type Hierarchy ~A ~(~A~)"
              (cond
                ((eq focus-type *toptype*) "below")
                ((null (ltype-daughters (get-type-entry focus-type))) "above")
                (t "around"))
              focus-type)
      show-all-p display-definitions-p focus-type)))

(defun display-type-hierarchy-really (node title show-all-p display-definitions-p focus-type)
  (let ((frame (clim:make-application-frame 'type-hierarchy)))
    (push frame *type-hierarchy-frames*)
    (setf (type-hierarchy-nodes frame) node)
    (setf (type-hierarchy-show-all-p frame) show-all-p)
    (setf (type-hierarchy-display-definitions-p frame) display-definitions-p)
    (setf (type-hierarchy-focus-type frame) focus-type) ; window will scroll to show this type
    (setf (clim:frame-pretty-name frame) title)
    (clim:run-frame-top-level frame)))


(defparameter *type-hierarchy-zoom-factor* 1.6)
(defconstant +zoom-factor-epsilon+ 0.00001)

(defun draw-type-hierarchy (type-hierarchy stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let* ((node-tree (type-hierarchy-nodes type-hierarchy))
         (focus-type (type-hierarchy-focus-type type-hierarchy))
         (scaling (type-hierarchy-scaling type-hierarchy))
         (telescopedp (< scaling (- 1.0 +zoom-factor-epsilon+)))
         (display-definitions-p (type-hierarchy-display-definitions-p type-hierarchy))
         (text-size (* *type-tree-font-size* scaling)))
    (silica:inhibit-updating-scroll-bars #+:allegro (stream)
      ;; would ideally do all of the scaling just with clim:with-scaling, but CLIM does not
      ;; scale text, and scaling text in conjunction with overall scaling causes inconsistency
      (clim:format-graph-from-root
        node-tree
        #'(lambda (node s)
            (clim:with-output-as-presentation (stream node 'hier-node :single-box t)
              (with-text-style-new-size (stream text-size)
                (if (ltype-shrunk-p
                      (or (get-type-entry node) 
                          (get-type-entry (get node 'real-thing))))
                  (clim:surrounding-output-with-border (s :line-dashes t :line-thickness 2)
                    (draw-type-entry node s telescopedp display-definitions-p))
                  (draw-type-entry node s telescopedp display-definitions-p)))))
          #'(lambda (node) (get node 'daughters))
          :stream stream 
          :merge-duplicates t
          :orientation :horizontal 
          :maximize-generations #+:mcclim t #-:mcclim nil ; definitely t for McCLIM
          :generation-separation
          (ceiling (* *tree-level-sep* (expt scaling 0.5))) ; slower increase/decrease to spread
          :within-generation-separation
          (max (floor (* *tree-node-sep* scaling)) (if display-definitions-p 1 0))
          :arc-drawing-options
          (list :ink ; zoomed out links gray since black gives a too dense effect
            (if telescopedp (clim:make-gray-color 0.6) clim:+black+))
          :graph-type :dag
          :center-nodes nil)
      #+:mcclim (clim:change-space-requirements stream) ; recompute stream's bounding box
      (scroll-to-type-in-hierarchy stream focus-type))))

(defvar *type-print-fn* nil)
(defparameter +pale-steel-blue+ (clim:make-rgb-color 0.90 0.92 0.95)) ; lighter than LightSteelBlue

(defun draw-type-entry (type-name stream telescopedp display-definitions-p)
  (let
    ((*type-print-fn*
       #'(lambda (name depth stream)
           (if (= depth 0)
             (clim:with-drawing-options (stream :ink clim:+red+) (write-string name stream))
             (write-string name stream))))
     dag)
    (if (and display-definitions-p
             (setq dag (ltype-local-constraint (get-type-entry type-name))))
      (clim:surrounding-output-with-border (stream :filled t :line-thickness 0
          :ink #+:mcclim +pale-steel-blue+ #-:mcclim clim:+blue+) ; !!! Allegro CLIM doesn't fill
        (display-dag1 dag 'display stream))
      (clim:with-drawing-options (stream :ink clim:+red+)
        (write-string
          (type-node-text-string type-name (if telescopedp 20 32)) ; more abbreviated if small
          stream)))))

(defun type-node-text-string (node max-length)
  (let* ((full-string (symbol-name node))
         (full-length (length full-string))
         (len (min full-length max-length))
         (str (nstring-downcase (subseq full-string 0 len)))) ; subseq always copies
    (when (> full-length len) (replace str "..." :start1 (- len 3)))
    str))

(defun scroll-to-type-in-hierarchy (stream type-name)
  (let ((record
          (find-object stream #'(lambda (x) (eq x type-name)))))
    (when record (scroll-to record stream))))

;;
;; Make nodes active
;;

(define-type-hierarchy-command (com-type-hier-menu) 
    ((node 'hier-node :gesture :select))
  (clim:with-application-frame (frame)
    (unhighlight-objects frame)
    (let ((type-entry (get-type-entry node)))
      (pop-up-menu 
       `(("Help" :value help 
	         :active ,(ltype-comment type-entry))
	 ("Shrink/expand" :value shrink
			  :active ,(ltype-daughters type-entry))
	 ("Type definition" :value def)
	 ("Expanded type" :value exp)
	 ("New hierarchy" :value new))
       (help (display-type-comment node (ltype-comment type-entry)))
       (shrink
         (setf (ltype-shrunk-p type-entry) (not (ltype-shrunk-p type-entry)))
	 (create-type-hierarchy-tree
           (type-hierarchy-focus-type frame) frame))
       (def (show-type-spec-aux node type-entry))
       (exp (show-type-aux node type-entry))
       (new
        ;; used to put up a dialog, but check box options are now in hierarchy window (below)
        ;; (let ((*last-type-name* (ltype-name type-entry)))
	;;    (declare (special *last-type-name*))
	;;    (show-type-tree))
        (create-type-hierarchy-tree (ltype-name type-entry) nil))))))


;;; Add buttons to menu bar: zoom in/out, show/hide types, show/hide defns

(define-type-hierarchy-command (com-zoom-in-from-hierarchy :menu "Zoom In")
  ()
  (clim:with-application-frame (frame)
    (when (< (type-hierarchy-scaling frame)
             (- (expt *type-hierarchy-zoom-factor* 4) +zoom-factor-epsilon+)) ; prevent >4 zoom ins
      (setf (type-hierarchy-scaling frame)
        (* (type-hierarchy-scaling frame) *type-hierarchy-zoom-factor*)))
    (create-type-hierarchy-tree
      (type-hierarchy-focus-type frame) frame)))

(define-type-hierarchy-command (com-zoom-out-from-hierarchy :menu "Zoom Out")
  ()
  (clim:with-application-frame (frame)
    (when (> (type-hierarchy-scaling frame)
             (+ (expt (/ 1 *type-hierarchy-zoom-factor*) 9) +zoom-factor-epsilon+)) ; prevent >9
      (setf (type-hierarchy-scaling frame)
        (/ (type-hierarchy-scaling frame) *type-hierarchy-zoom-factor*)))
    (create-type-hierarchy-tree
      (type-hierarchy-focus-type frame) frame)))

(define-type-hierarchy-command (com-show-types-from-hierarchy :menu "Show/Hide Types")
  ()
  (clim:with-application-frame (frame)
    (setf (type-hierarchy-show-all-p frame) (not (type-hierarchy-show-all-p frame)))
    (create-type-hierarchy-tree
      (type-hierarchy-focus-type frame) frame)))

(define-type-hierarchy-command (com-show-defns-from-hierarchy :menu "Show/Hide Defns")
  ()
  (clim:with-application-frame (frame)
    (setf (type-hierarchy-display-definitions-p frame)
      (not (type-hierarchy-display-definitions-p frame)))
    (create-type-hierarchy-tree
      (type-hierarchy-focus-type frame) frame)))


;;; NB Problems caused by having only 1 field per type for shrunk
;;; flag and allowing multiple type windows on screen at once: if a type
;;; of interest is not visible in a window due to ancestor(s) being shrunk
;;; then we would have to unshrink them to display the type. Finesse
;;; the issue by only highlighting a type if it's displayed in the first
;;; type hierarchy window - otherwise open a new hierarchy window.

(defun display-type-in-tree (node &optional scroll-onlyp)
  (let* ((frame (reuse-frame 'type-hierarchy))
         (type-entry
	   (or (get-type-entry node)
	       (get-type-entry (get node 'real-thing))))
	 (type (ltype-name type-entry))
         stream record)
    (when type-entry
      (cond
        ((and frame
	      (setq stream (clim:frame-standard-output frame))
              (setq record (find-object stream #'(lambda (t1) (eq t1 type)))))
          (scroll-to record stream)
          (highlight-objects (list record) frame))
        ((not scroll-onlyp)
          (create-type-hierarchy-tree type nil nil nil))))))
        
#|
(defun unshrink-ancestors (type-entry top-type)
  ;; can't just use type-ancestors list since we have to stop at top-type arg
  (unless (eql (ltype-name type-entry) top-type)
    (loop for parent in (ltype-parents type-entry)
         do
         (let ((parent-entry (get-type-entry parent)))
	   (when (ltype-shrunk-p parent-entry)
	     (setq *needs-redisplay* t))
	   (setf (ltype-shrunk-p parent-entry) nil)
	   (unshrink-ancestors parent-entry top-type)))))
|#


;; ----------------------------------------------------------------------
;; Draw type hierarchy using daVinci

(defvar *davinci-nodes* nil)

(defun davinci (&optional (type *toptype*) show-all-p)
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
  (let ((node (car (make-new-type-tree type show-all-p)))
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
  (let ((node (car (make-new-type-tree type show-all-p)))
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
