;;;; A COMMON LISP GRAPHICAL GRAPH PACKAGE

;;; Copyright John Carroll 1991. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; John Carroll
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK

;;; There are 2 entry points: graph-display-layout and graph-display-output.
;;; See file ':worksheet.lsp' for some example calls. More detailed 
;;; documentation to follow.

;;; (in-package 'user)

;;; hacked by aac for lkb type hierarchy display - some changes for type hierarchy
;;; chart, and parse tree display (special customisation for former controlled
;;; by *type-display*

;;; calls externally-defined graphics functions draw-line-x-y, move-to-x-y
;;; (and draw-circular-link-x-y but only in case of circularities, which won't
;;; occur in lkb)
;;;
;;; (defmacro draw-line-x-y (stream from-x from-y to-x to-y dashing) ...)
;;;
;;; (defmacro move-to-x-y (stream x y) ...)
;;;
;;; (defmacro draw-circular-link-x-y (stream centre-x centre-y semi-major-axis 
;;;      horizontalp) ...)

(defstruct
   (graph-description
      (:constructor make-graph-description
         (user-root max-x max-y node-width-function node-height horizontalp nodes))
      (:print-function
         (lambda (x stream level)
            (declare (ignore level))
            (print-unreadable-object (x stream :type t :identity t)
               (format stream "~A ~A" (graph-description-user-root x)
                  (graph-description-horizontalp x))))))
   (user-root nil :read-only t)
   (max-x nil :read-only t)
   (max-y nil :read-only t)
   (node-width-function nil :read-only t)
   (node-height nil :read-only t)
   (horizontalp nil :read-only t)
   (nodes nil :read-only t))

(defconstant +maximum-pixel-dimension+ #x7fff)

(defstruct graph-node
   depth parents daughters reverse-links cached-width relative-offset x y)

(defstruct
   (dummy-graph-node (:include graph-node)
      (:constructor make-dummy-graph-node
         (depth parents daughters reverse-links cached-width relative-offset x y))))

(defstruct
   (proper-graph-node (:include graph-node)
      (:constructor make-proper-graph-node
         (depth parents daughters reverse-links cached-width relative-offset x y
            contents)))
   (contents nil :read-only t))


(defvar *graph-node-table* nil)
#+procyon (proclaim '(pro:always-bound *graph-node-table*))

(defconstant max-node-height 256)
(defconstant max-node-width 1000)


(defmacro get-node-width (node node-width-function)
   `(or (graph-node-cached-width ,node)
      (get-checked-node-width ,node-width-function ,node)))

(defun get-checked-node-width (node-width-function node)
   (let ((w (funcall node-width-function 
               (proper-graph-node-contents node))))
      (if (and (integerp w) (<= 0 w max-node-width))
         (setf (graph-node-cached-width node) w)
         (error "Width of node ~S is not an integer in the range 0-~D."
            (proper-graph-node-contents node) max-node-width))))


(defun graph-display-function-error (arg-name function-name)
   (error "The ~A argument to ~S is not a function." arg-name function-name))


;;; Entry point 1

(defun graph-display-layout (user-root node-daughter-function node-width-function
      node-height horizontalp)
   (unless (functionp node-daughter-function)
      (graph-display-function-error "NODE-DAUGHTER-FUNCTION" 'graph-display-layout))
   (unless (functionp node-width-function)
      (graph-display-function-error "NODE-WIDTH-FUNCTION" 'graph-display-layout))
   (unless (and (integerp node-height) (< 0 node-height max-node-height))
      (error "The NODE-HEIGHT argument to ~S is not an integer in the range 1-~D."
         'graph-display-layout max-node-height))
   (let ((root
            (make-proper-graph-node 0 nil nil nil nil nil nil nil user-root))
         (*graph-node-table*
            (make-hash-table :test #'eq)))
      (setf (gethash user-root *graph-node-table*) root)
      (graph-assign-depths user-root root (list root) node-daughter-function)
      (setq *graph-node-table* nil)
      (let
         ((node-depths-table 
               (graph-add-dummy-nodes root
                  (make-array 1 :initial-element (list root)))))
         ;; lkb hack - only allow reordering with type hierarchy. Need to do it
         ;; otherwise full grammar initially too wide
         (when *type-display* (graph-assign-x-ordering node-depths-table))
         (let*
            ((max-width
                  (graph-assign-x-positions
                     node-depths-table node-width-function 
                     (+ node-height 
                        8) ; was 5 - increased for lkb
                     horizontalp))
               (max-depth
                  (graph-assign-y-positions
                     node-depths-table node-width-function (+ node-height 17) 
                     horizontalp)))
            (make-graph-description user-root
               (if horizontalp max-depth max-width)
               (if horizontalp max-width max-depth)
               node-width-function node-height horizontalp node-depths-table)))))


(defun graph-assign-depths (user-node node parents-visited node-daughter-function)
   (check-node-depth node)
   (for user-daughter in (funcall node-daughter-function user-node)
      do
      (let ((daughter (gethash user-daughter *graph-node-table*)))
         (cond
            ((null daughter)
               (setq daughter
                  (make-proper-graph-node (1+ (graph-node-depth node))
                     (list node) nil nil nil nil nil nil user-daughter))
               (setf (gethash user-daughter *graph-node-table*) daughter)
               (push daughter (graph-node-daughters node))
               (graph-assign-depths
                  user-daughter daughter (cons daughter parents-visited)
                  node-daughter-function))
            ((member daughter parents-visited :test #'eq)
               ;; circular link - reverse its direction, don't set up 
               ;; link if it goes from daughter directly back to itself
               (cond
                  ((eq node daughter)
                     (push daughter (graph-node-reverse-links daughter)))
                  ((and (eq daughter (cadr parents-visited))
                        (< (graph-node-depth node)
                           (+ (graph-node-depth daughter) 2)))
                     ;; already a direct forward connection from parent at 
                     ;; level just above this one - move node down one level 
                     ;; and insert a dummy node so that forward and backward 
                     ;; links can follow different paths 
                     (setf (graph-node-depth node) 
                        (+ (graph-node-depth daughter) 2))
                     (graph-update-depths node)
                     (let ((new
                              (make-dummy-graph-node (1+ (graph-node-depth daughter))
                                 (list daughter) (list node) (list node) nil nil nil
                                 nil)))
                        (push new (graph-node-parents node))
                        (push new (graph-node-daughters daughter))
                        (push new (graph-node-reverse-links daughter))))
                  (t
                     (push daughter (graph-node-parents node))
                     (push node (graph-node-daughters daughter))
                     (push node (graph-node-reverse-links daughter)))))
            (t
               ;; daughter has been created before - its daughters must 
               ;; also have been done then
               (when (<= (graph-node-depth daughter) (graph-node-depth node))
                  ;; daughter is currently above level it should be - set its
                  ;; depth to just below here and update depths of all its 
                  ;; direct and indirect descendents
                  (setf (graph-node-depth daughter) (1+ (graph-node-depth node)))
                  (graph-update-depths daughter))
               (push node (graph-node-parents daughter))
               (push daughter (graph-node-daughters node)))))))


(defun graph-update-depths (node)
   (check-node-depth node)
   (for daughter in (the list (graph-node-daughters node))
      when (<= (graph-node-depth daughter) (graph-node-depth node))
      do
      (setf (graph-node-depth daughter) (1+ (graph-node-depth node)))
      (graph-update-depths daughter)))


(defun check-node-depth (node)
   (when (> (graph-node-depth node) (- +maximum-pixel-dimension+ 10))
      (error "Graph has too many levels for ~S to handle." 'graph-display-layout)))


;;;

(defun graph-add-dummy-nodes (node table)
   (for daughter in (the list (graph-node-daughters node))
      do
      (let ((depth (graph-node-depth daughter)))
         (when (>= depth (length (the simple-vector table)))
            (let ((new-table
                     (make-array (1+ depth) :initial-element nil)))
               (replace new-table table)
               (setq table new-table)))
         (when (> depth (1+ (graph-node-depth node)))
            ;; daughter currently lower than just below here - connect
            ;; to here with dummy nodes
            (setf (graph-node-daughters node)
               (delete daughter (the list (graph-node-daughters node)) :test #'eq))
            (setf (graph-node-parents daughter)
               (delete node (the list (graph-node-parents daughter)) :test #'eq))
            (let ((reversep
                     (member daughter (graph-node-reverse-links node) :test #'eq)))
               (when reversep
                  (setf (graph-node-reverse-links node)
                     (delete daughter (the list (graph-node-reverse-links node))
                        :test #'eq)))
               (graph-connect-dummy-nodes node daughter table reversep)))
         (unless (member daughter (svref table depth) :test #'eq)
            (push daughter (svref table depth))
            (setq table
               (graph-add-dummy-nodes daughter table)))))
   table)


(defun graph-connect-dummy-nodes (higher lower table reversep)
   (for depth fixnum (1+ (graph-node-depth higher))
      to (1- (graph-node-depth lower))
      do
      (let ((new
               (make-dummy-graph-node depth (list higher) nil nil nil nil nil 
                  nil)))
         (push new (svref table depth))
         (push new (graph-node-daughters higher))
         (when reversep
            (push new (graph-node-reverse-links higher)))
         (setq higher new)))
   (push higher (graph-node-parents lower))
   (setf (graph-node-daughters higher) (list lower))
   (when reversep
      (setf (graph-node-reverse-links higher) (list lower))))


;;;

(defun graph-assign-x-ordering (node-depths-table)
   (for node in (the list (svref node-depths-table 0)) 
      do
      (setf (graph-node-relative-offset node) 0))
   (let ((max-depth
            (1- (length (the simple-vector node-depths-table)))))
      (for iteration fixnum 1 to 2
         do
         (for depth fixnum 1 to max-depth
            do
            (graph-assign-x-ordering-up/down depth node-depths-table 1))
         (unless (eql iteration 2)
            (for depth fixnum (1- max-depth) downto 0
               do
               (graph-assign-x-ordering-up/down depth node-depths-table -1))))))


(defun graph-assign-x-ordering-up/down (depth node-depths-table direction)
   (for node in (the list (svref node-depths-table depth))
      do 
      (let ((parents/children 
               (if (minusp direction) (graph-node-daughters node) 
                  (graph-node-parents node))))
         (setf (graph-node-relative-offset node) 
            (if parents/children 
               (truncate
                  (let ((sum 0))
                     (for parent/child in (the list parents/children)
                        do 
                        (incf sum (graph-node-relative-offset parent/child)))
                     sum)
                  (length (the list parents/children)))
               (graph-node-relative-offset node)))))
   (setf (svref node-depths-table depth) 
      (stable-sort (svref node-depths-table depth) 
         #'(lambda (n1 n2)
             (< (graph-node-relative-offset n1) (graph-node-relative-offset n2)))))
   (let ((n 0))
      (for node in (the list (svref node-depths-table depth))
         do 
         (when (eql n +maximum-pixel-dimension+)
            (error "Graph has too many nodes in a single level for ~S to handle."
               'graph-display-layout))
         (setf (graph-node-relative-offset node) n)
         (incf n))))


;;;

(defun graph-assign-x-positions (node-depths-table node-width-function 
      separation horizontalp)
   (for node in (the list (svref node-depths-table 0))
      do
      (setf (graph-node-x node) 0))
   (let ((total-depth
            (1- (length (the simple-vector node-depths-table))))
         (max-width 0))
      (for iteration fixnum 1 to 2
         do
         (for depth fixnum 1 to total-depth ; child <- mean of parents
            do
            (setq max-width
               (max max-width
                  (graph-assign-x-positions-up/down depth node-depths-table 
                     node-width-function 
                     (if (and *type-display* (<= depth 2)) ; hack for type hierarchy to
                        (* separation 2)                   ; increase vertical separation
                        separation)                        ; at first 2 levels
                     horizontalp 1))))
         (for depth fixnum (1- total-depth) downto 0 ; parent <- mean of children
            do
            (when (or (not *type-display*)
                     ;; in type display - mid-point of children unless we've done 1
                     ;; iteration and it's got too dense at this depth 
                     (eql iteration 1)
                     (< (length (svref node-depths-table depth)) 20))
               (setq max-width
                  (max max-width
                     (graph-assign-x-positions-up/down depth node-depths-table 
                        node-width-function 
                        (if (and *type-display* (<= depth 2))
                           (* separation 2)
                           separation) 
                        horizontalp -1))))))
      ;; shift x coords back down to zero
      (let ((min-x most-positive-fixnum)
            (max-first-width 0))
         (for depth fixnum 0 to total-depth
            do
            (unless horizontalp
               (let ((first-node (car (svref node-depths-table depth))))
                  (when (and first-node (not (dummy-graph-node-p first-node)))
                     (setq max-first-width
                        (max (get-node-width first-node node-width-function)
                           max-first-width)))))
            (for node in (the list (svref node-depths-table depth))
               do
               (setq min-x (min min-x (graph-node-x node)))))
         (decf min-x (truncate (+ max-first-width separation) 2))
         (for depth fixnum 0 to total-depth
            do
            (for node in (the list (svref node-depths-table depth))
               do
               (decf (graph-node-x node) min-x)))
         (decf max-width min-x))
      (+ max-width separation)))


(defun graph-assign-x-positions-up/down (depth node-depths-table
      node-width-function separation horizontalp direction &aux (last-x 0))
   (for node in (the list (svref node-depths-table depth))
      do
      (let* ((parents/children
               (if (minusp direction) (graph-node-daughters node)
                  (graph-node-parents node)))
             (desired-x
               (if parents/children
                  (truncate
                     (let ((sum 0))
                        (for parent/child in (the list parents/children)
                           do
                           (incf sum (graph-node-x parent/child)))
                        sum)
                     (length (the list parents/children)))
                  (graph-node-x node))))
         (setf (graph-node-x node) desired-x)))
   (when (or *type-display* (and *chart-display* (> depth 2)))
      (setf (svref node-depths-table depth)
         (sort (svref node-depths-table depth)
            #'(lambda (n1 n2) (< (graph-node-x n1) (graph-node-x n2))))))
   (let ((n 0) (prev-dummy-node-p nil)
         (dummy-separation (truncate (* separation 2) 3)))
      (for node in (the list (svref node-depths-table depth))
         do
         (when
            (>= last-x
               (- +maximum-pixel-dimension+ max-node-width (* (+ max-node-height 5) 2)))
            (error "Graph is too wide for ~S to handle. There is room for only ~A out ~
                    of ~A nodes/links at depth ~A."
               'graph-display-layout (1- n) (length (svref node-depths-table depth))
               (1+ depth)))
         (let* ((dummy-node-p (dummy-graph-node-p node))
                (half-width
                  (if (or horizontalp dummy-node-p) 0
                     (truncate (get-node-width node node-width-function) 2))))
            (when (eql last-x 0) (setq last-x half-width))
            (setf (graph-node-x node)
               (max (graph-node-x node)
                  (+ last-x ; no room so have to go further out
                     (if (or dummy-node-p prev-dummy-node-p) dummy-separation separation)
                     half-width)))
            (setq last-x (+ (graph-node-x node) half-width))
            (setq prev-dummy-node-p dummy-node-p))
         (incf n)))
   last-x)


;;;

(defun graph-assign-y-positions (node-depths-table node-width-function separation
      horizontalp)
   (let ((current 1))
      (for depth fixnum 0 to
         (1- (length (the simple-vector node-depths-table)))
         do
         (when
            (>= current
               (- +maximum-pixel-dimension+ max-node-width max-node-height 17))
            (error "Graph is too deep for ~S to handle." 'graph-display-layout))
         (for node in (the list (svref node-depths-table depth))
            do
            (setf (graph-node-y node) current))
         (setq current
            (+ current separation
               (if horizontalp
                  (let ((max-width 0))
                     (for node in (the list (svref node-depths-table depth))
                        do
                        (setq max-width
                           (max max-width
                              (if (dummy-graph-node-p node) 0
                                 (get-node-width node node-width-function)))))
                     max-width)
                  0))))
      current))


;;; Entry point 2
;;;
;;; Note that the x and y fields in nodes run vertically and horizontally
;;; respectively when the output format is horizontal. Max-x and max-y in
;;; the description structure run in the conventional directions, though.

(defun graph-display-output (stream description node-print-function)
   (unless (and (streamp stream) (output-stream-p stream))
      (error "The STREAM argument to ~S is not a stream open for output."
         'graph-display-output))
   (unless (graph-description-p description)
      (error "The DESCRIPTION argument to ~S is not a graph-description."
         'graph-display-output))
   (unless (functionp node-print-function)
      (graph-display-function-error "NODE-PRINT-FUNCTION" 'graph-display-output))
   (funcall
      (if (graph-description-horizontalp description)
         'graph-display-horizontally 'graph-display-vertically)
      stream node-print-function
      (graph-description-node-width-function description)
      (graph-description-node-height description)
      (graph-description-nodes description)))


;;; display type hierarchy / chart horizontally

(defun graph-display-horizontally (stream node-print-function node-width-function
      node-height node-depths-table)
   (let ((half-node-height (1- (truncate node-height 2))))
      (for depth fixnum (if *type-display* 0 1)
                 to (1- (length (the simple-vector node-depths-table)))
         do
         (for node in (the list (svref node-depths-table depth))
            do
            (when (or *type-display* (and *chart-display* (> depth 1)))
               (for parent in (the list (graph-node-parents node))
                  do
                  ;; x and y positions for nodes are top left of label
                  (draw-line-x-y stream
                     (if (dummy-graph-node-p parent) (graph-node-y parent)
                        (+ (graph-node-y parent) 1
                           (get-node-width parent node-width-function)))
                    (+ (graph-node-x parent) half-node-height)
                     (if (dummy-graph-node-p node) (graph-node-y node)
                        (- (graph-node-y node) 2))
                     (+ (graph-node-x node) half-node-height)
                     (member node (graph-node-reverse-links parent) :test #'eq))))
            (unless (dummy-graph-node-p node)
               (move-to-x-y stream (graph-node-y node) (graph-node-x node))
               (funcall node-print-function stream 
                  (proper-graph-node-contents node))
               (when (member node (graph-node-reverse-links node) :test #'eq)
                  (draw-circular-link-x-y stream
                     (+ (graph-node-y node)
                        (get-node-width node node-width-function))
                     (+ (graph-node-x node) half-node-height)
                     node-height t)))))))


;;; display parse tree vertically

(defun graph-display-vertically (stream node-print-function node-width-function
      node-height node-depths-table)
   (for depth fixnum 0 to (1- (length (the simple-vector node-depths-table)))
      do
      (for node in (the list (svref node-depths-table depth))
         do
         (for parent in (the list (graph-node-parents node))
            do
            ;; x and y positions for nodes are in middle of top of label
            (draw-line-x-y stream
               (graph-node-x parent)
               (if (dummy-graph-node-p parent) (graph-node-y parent)
                  (1- (+ (graph-node-y parent) node-height)))
               (graph-node-x node)
               (1- (graph-node-y node))
               (member node (graph-node-reverse-links parent) :test #'eq)))
         (unless (dummy-graph-node-p node)
            (move-to-x-y stream
               (- (graph-node-x node)
                  (truncate (get-node-width node node-width-function) 2))
               (graph-node-y node))
            (funcall node-print-function stream (proper-graph-node-contents node))
            (when (member node (graph-node-reverse-links node) :test #'eq)
               (draw-circular-link-x-y stream
                  (+ (graph-node-x node)
                     (truncate (get-node-width node node-width-function) 2))
                  (+ (graph-node-y node) (1- (truncate node-height 2)))
                  node-height nil))))))


;;; End of file
