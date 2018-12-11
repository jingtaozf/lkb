;;; Copyright (c) 1991--2018
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Frederik Fouvry;
;;;   see `LICENSE' for conditions.

;;; ACL version

(in-package :lkb)

;;; Appearance parameters

(defparameter *parse-window-width* 400
  "Initial width of tree window")

(defparameter *parse-window-height* 400
  "Initial height of tree window")

(defun lkb-parse-tree-font ()
  (clim:make-text-style :sans-serif :roman (or *parse-tree-font-size* 12)))

(defun lkb-summary-tree-font ()
  (clim:make-text-style :sans-serif :roman (or *summary-tree-font-size* 8)))

(declaim (notinline lkb-parse-tree-font lkb-summary-tree-font))


(def-lkb-parameter *ptree-node-sep* #+:mcclim 14 #-:mcclim 6
  "Spacing between nodes in a single generation.")

(def-lkb-parameter *ptree-level-sep* #+:mcclim 14 #-:mcclim 12
  "Spacing between levels in the tree.")                    

(defparameter *parsum-node-sep* 5)

(defparameter *parsum-level-sep* 5)                    


;;; Globals used to keep track of parse chart - set in chartout which is therefore read in
;;; after this file

(defvar *main-chart-frame* nil)
(defvar *sub-chart-window-frames* nil)


;;; Window showing a single parse tree

(clim:define-presentation-type node ())

(define-lkb-frame parse-tree
  ((nodes :initform nil
          :accessor parse-tree-nodes)
   (current-chart :initform nil
                  :accessor parse-tree-current-chart))
  :info-bar t
  :display-function 'draw-parse-tree
  :text-style (lkb-parse-tree-font)
  :width :compute
  :height :compute)

(define-info-bar node (node stream)
  (let ((edge (get node 'edge-record)))
    (when (edge-p edge)
      (if (g-edge-p edge)
          (write-string (string-downcase (string (g-edge-index edge))) stream)
          (format stream "~A-~A" (edge-from edge) (edge-to edge)))
      (format stream " [~A] ~A " (edge-id edge)
        (if (rule-p (edge-rule edge)) (rule-id (edge-rule edge)) (edge-rule edge)))
      (format stream "~{~(~A~)~^ ~}" (edge-lex-ids edge))))) ; JAC 8-Dec-2018: was edge-leaves


(defun draw-new-parse-tree (topnode title horizontalp &optional counter)
  (declare (ignore horizontalp))
  (mp:run-function
    title #'draw-new-parse-tree-really topnode counter title))

(defun draw-new-parse-tree-really (topnode counter &optional title)
  (let ((pframe (clim:make-application-frame 'parse-tree)))
    (setf (parse-tree-nodes pframe) topnode)
    (setf (parse-tree-current-chart pframe) 
      (or counter
          *chart-generation-counter*))
    (setf (clim:frame-pretty-name pframe) (or title "Parse Tree"))
    (clim:run-frame-top-level pframe)))


(defun draw-parse-tree (ptree-frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((node-tree (parse-tree-nodes ptree-frame)))
    (clim:stream-set-cursor-position stream 3 1)
    (clim:format-graph-from-root
      node-tree
      #'(lambda (node stream)
          (multiple-value-bind (s lex-p) 
              (get-string-for-edge node)
            (if lex-p
              (with-text-style-bold-face (stream) (write-string s stream))
              (clim:with-output-as-presentation (stream node 'node) (write-string s stream)))))
      #'find-children
      :graph-type #-:mcclim :parse-tree #+:mcclim :tree ; see graph.lsp
      :stream stream 
      :merge-duplicates nil
      :orientation :vertical
      :generation-separation *ptree-level-sep*
      :within-generation-separation *ptree-node-sep*
      :center-nodes nil)
    (clim:scroll-extent stream 0 0)))


;;; Menus on active parse tree nodes

(define-parse-tree-command (com-parse-tree-menu)
    ((edge-symbol 'node :gesture :select))
  (let* ((edge-record (get edge-symbol 'edge-record))
         (edge-fs (get edge-symbol 'edge-fs))
         (item (edge-rule edge-record))
         (rule-name (if (rule-p item) (rule-id item) item)))
    (pop-up-menu
     `(
       ("Feature structure"
        :value fs)
       ("Unfilled feature structure"
        :value ufs)
       (,(format nil "Feature structure - Chart edge ~A" (edge-id edge-record))
        :value edge-fs)
       (,(format nil "Unfilled feature structure - Chart edge ~A" (edge-id edge-record))
        :value edge-ufs)
       ("Show edge in chart" 
        :value edge
        :active ,(and (not (g-edge-p edge-record))
                      (or
                       (not (parse-tree-current-chart clim:*application-frame*))
                       (eql (parse-tree-current-chart clim:*application-frame*)
                            *chart-generation-counter*))))
       (,(if (rule-p item)
             (format nil "Rule ~A" (rule-id item))
             "Entry") ; omit the item since it might not be displayable in the menu font
        :value rule
        :active ,(or (rule-p item) (get-tdfs-given-id item)))
       ("Sement"
        :value sement
        :active ,(mrs::algebra-available-p))
       ("Rule sement"
        :value rule-sement
        :active ,(mrs::algebra-available-p))
       ("Check algebra"
        :value check-algebra
        :active ,(mrs::algebra-available-p)) 
       ("Generate from edge" :value generate
                             :active ,(and *mrs-loaded*
                                       (not (g-edge-p edge-record))))
       #+:ignore ; lex ids information moved to info bar
       (,(format nil "Lex ids ~A" (edge-lex-ids edge-record))
        :value nil
        :active nil))
     (fs (display-fs edge-fs
                       (format nil "Edge ~A ~A - Tree FS" 
                               (edge-id edge-record)
                               (if (g-edge-p edge-record) "G" "P"))))
     (ufs (display-fs (unfilled-tdfs (copy-tdfs-completely edge-fs))
                       (format nil "Edge ~A ~A - Tree Unfilled FS" 
                               (edge-id edge-record)
                               (if (g-edge-p edge-record) "G" "P"))))
     (edge-fs
      (let ((tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p tdfs)
          (display-fs tdfs
                      (format nil "Edge ~A ~A - Edge FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P"))))))
     (edge-ufs
      (let ((tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p tdfs)
          (display-fs (unfilled-tdfs (copy-tdfs-completely tdfs))
                      (format nil "Edge ~A ~A - Edge Unfilled FS" 
                              (edge-id edge-record)
                              (if (g-edge-p edge-record) "G" "P"))))))
     (edge
          (progn
            (cond ((and *main-chart-frame* 
                        (eql (clim:frame-state *main-chart-frame*) 
                             :enabled))
                   nil)
                  ((and *main-chart-frame* 
                        (eql (clim:frame-state *main-chart-frame*) 
                             :shrunk))
                   (clim:raise-frame *main-chart-frame*))
                  (t (show-chart)
                     (wait-until-chart-ready)))
            (highlight-edge edge-record *main-chart-frame* :scroll-to t)))
     (rule 
      (let* ((item (edge-rule edge-record))
             (rule (if (rule-p item) item nil)))
        (if rule
            (display-fs (rule-full-fs rule)
                        (format nil "~A" (rule-id rule))
                        (rule-id rule))
          (let ((alternative (get-tdfs-given-id item)))
            (when alternative
              (display-fs alternative 
                          (format nil "~A" item)
                          item))))))
     (sement       
      ;;; we have to use the tdfs as used by the parser here, because
      ;;; otherwise we have things on the rels list that shouldn't be 
      ;;; there because of the diff list
      (let ((parse-tdfs (and (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p parse-tdfs)
          (show-mrs-sement-window parse-tdfs edge-fs edge-record
                                  (format nil "Edge ~A ~A - Sement" 
                                          (edge-id edge-record)
                                          (if (g-edge-p edge-record) "G" "P"))))))
     (rule-sement       
      ;;; this is to get the contribution of the rule - for cases where
      ;;; it has a c-cont
      (let ((rule-tdfs (and (edge-p edge-record) 
                            (rule-p (edge-rule edge-record))
                            (rule-full-fs (edge-rule edge-record)))))
        (when (tdfs-p rule-tdfs)
          (show-mrs-rule-sement-window rule-tdfs
                                  (format nil "Edge ~A ~A - Rule sement" 
                                          (edge-id edge-record)
                                          (if (g-edge-p edge-record) "G" "P"))))))
     (check-algebra       
      (let ((parse-tdfs (if (edge-p edge-record) (edge-dag edge-record))))
        (when (tdfs-p parse-tdfs)
          (show-mrs-sement-check-window parse-tdfs edge-fs edge-record
                                  (format nil "Edge ~A ~A - algebra check" 
                                          (edge-id edge-record)
                                          (if (g-edge-p edge-record) "G" "P")))))) 
     (generate (funcall 'really-generate-from-edge edge-record)))))


;;; ***** Display window showing multiple parses ********
;;; because it's very annoying to get zillions of windows when there's
;;; a highly ambiguous sentence, the following code displays a
;;; single window with lots of little parse trees.  It is
;;; based on Rob's compare code (see compare.lsp)

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
           #-:ansi-eval-when (load eval compile)
  (defstruct parsum 
    top ; top node of a single parse tree
    output-record ; its output record
    edge))

(clim:define-presentation-type parsum ())

(define-lkb-frame parse-summary
  ((trees :initform nil
          :accessor parse-summary-trees)
   (current-chart :initform nil
                  :accessor parse-summary-current-chart)
   (last-margin :initform nil
                :accessor parse-summary-last-margin)
   (last-ncolumns :initform 0
                  :accessor parse-summary-last-ncolumns))
  :info-bar t
  :display-function 'draw-parse-summary-window
  :text-style (lkb-summary-tree-font)
  ;; JAC 5-Dec-2018: width and height were previously :compute, but now there is no
  ;; well-defined default width/height since trees are formatted to fit window width
  :width *parse-window-width*
  :height *parse-window-height*)

(define-info-bar parsum (parsum stream)
  (let ((edge (when parsum (parsum-edge parsum))))
    (when edge
      (format stream "~A-~A [~A] ~A "
        (edge-from edge) (edge-to edge) (edge-id edge) (rule-id (edge-rule edge))))))

(defmethod clim:run-frame-top-level :before ((frame parse-summary) &key &allow-other-keys)
  ;; Set up top edge display - this is based on the style in the summary tree pane but
  ;; with the size from the full-size parse tree font - so resize pane and reset its style
  (let ((lkb-pane
          (clim:find-pane-named frame 'lkb-pane))
        (doc-pane 
          (clim:find-pane-named frame 'doc-pane)))
    (when (and lkb-pane doc-pane)
      (let* ((new-style
               (text-style-new-size
                 (clim:pane-text-style lkb-pane)
                 (clim:text-style-size (lkb-parse-tree-font))))
             (text-height
               (clim:text-style-height new-style doc-pane)))
        (setf (clim:medium-text-style doc-pane) new-style)
        (clim:change-space-requirements 
          doc-pane
          :resize-frame t
          :min-height text-height
          :max-height text-height)))))


;;; Detect resizing of the window by an :after method on allocate-space. If there is a
;;; significant change in the window width then redisplay, which might reflow the columns

(define-parse-summary-command com-resize-window ((frame 'parse-summary))
  (draw-parse-summary-window
    frame (clim:find-pane-named frame 'lkb-pane)))

(defmethod clim:allocate-space :after ((pane lkb-pane) width height)
  (declare (ignore width height))
  (clim:with-application-frame (frame)
    (when (slot-exists-p frame 'last-margin) ; does frame reformat lkb-pane on resize?
      (let ((margin (clim:stream-text-margin pane)))
        (with-slots (last-margin) frame
          (cond
            ((null last-margin) ; a brand new frame, not a resize?
              (setf last-margin margin))
            ((> (abs (- margin last-margin)) 10) ; significant change since last redisplay attempt?
              (setf last-margin margin)
              (clim:execute-frame-command frame `(com-resize-window ,frame)))))))))


(defun invalidate-chart-commands nil
  ;;; this function should be redundant now because of *chart-generation-counter*
  nil)
#|
  (clim:map-over-frames 
   #'(lambda (frame) 
       (when (and (eql (clim:frame-name frame) 'parse-summary)
                  (member (clim:frame-state frame) '(:enabled :shrunk)))
         (setf (clim:command-enabled 'com-multiple-tree-menu frame) nil) 
         (setf (clim:command-enabled 'com-show-chart-from-tree frame) nil)))))
|#

(defun show-parse-summary (parses &optional title)
  (if #+:lui (lui-status-p :tree) #-:lui nil
    (let ((input (format nil "~{~a~^ ~}" (edge-leaves (first parses)))))
      #+:lui (lui-show-parses parses input) #-:lui nil)
    (mp:run-function title #'show-parse-summary-really parses title)))

(defun show-parse-summary-really (parses &optional title)
  (let ((frame (clim:make-application-frame 'parse-summary)))
    (set-up-parse-summary parses frame)
    (setf (clim:frame-pretty-name frame) 
      (or title
          (format nil "Parse Trees (~A) for \"~A\""
            (length parses)
            (shortened-sentence-string (edge-leaves (car parses))))))
    (clim:run-frame-top-level frame)))


(defun set-up-parse-summary (parses frame)
  (setf (parse-summary-current-chart frame) *chart-generation-counter*)
  (setf (parse-summary-trees frame)
    (mapcar #'(lambda (p) 
                (make-parsum :top (make-new-parse-tree p 1)
                             :edge p))
            parses)))

(defun draw-parse-summary-window (frame stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let* ((trees (parse-summary-trees frame))
         (node-style (clim:pane-text-style stream))
         (max-tree-width-approx
           (loop
              for tree in trees
              repeat 10 ; a sample, otherwise a trivial window resize could be expensive
              maximize (compute-parse-tree-width (parsum-top tree) stream node-style)))
         (window-width (clim:stream-text-margin stream))
         (ncolumns (max 1 (floor window-width (+ max-tree-width-approx 15)))))
    (unless (eql ncolumns (parse-summary-last-ncolumns frame))
      (setf (parse-summary-last-ncolumns frame) ncolumns)
      (clim:window-clear stream)
      (clim:stream-set-cursor-position stream 3 1)
      (clim:formatting-table
        (stream :x-spacing 15 :y-spacing 5 :equalize-column-widths t)
        (loop
          while trees
          do
          (clim:formatting-row (stream) ; display in row-wise order
            (loop
              repeat ncolumns
              while trees
              for tree = (pop trees)
              do
              (clim:formatting-cell (stream) (draw-single-res-tree tree stream))))))
      (clim:scroll-extent stream 0 0)
      ;; update pane contents size to ensure scroll bar extents are correct
      #+:mcclim
      (multiple-value-bind (w h)
          (clim:bounding-rectangle-size (clim:stream-output-history stream))
        (clim:change-space-requirements stream :width w :height h)))))

(defun compute-parse-tree-width (node stream node-style)
  (let ((lex-style (text-style-bold-face node-style)))
    (labels
      ((node/daughters-max-width (node)
        (multiple-value-bind (s lex-p) 
            (get-string-for-edge node)
          (max 
            (+ (clim:stream-string-width stream s
                 :text-style (if lex-p lex-style node-style))
               *parsum-node-sep*)
            (loop
              for d in (find-children node)
              sum (node/daughters-max-width d))))))
      (node/daughters-max-width node))))

(defun draw-single-res-tree (tree stream)
  (setf (parsum-output-record tree)
    (clim:with-new-output-record (stream)
      (clim:with-output-recording-options (stream :record t)
        (clim:with-output-as-presentation 
          (stream tree 'parsum :single-box t)
          (clim:format-graph-from-root
            (parsum-top tree)
            #'(lambda (node stream)
                (multiple-value-bind (s lex-p) 
                    (get-string-for-edge node)
                  (if lex-p
                    (with-text-style-bold-face (stream) (write-string s stream))
                    (write-string s stream))))
            #'find-children
            :graph-type #-:mcclim :parse-tree #+:mcclim :tree ; see graph.lsp
            :stream stream 
            :merge-duplicates nil
            :orientation :vertical
            :generation-separation *parsum-level-sep*
            :within-generation-separation *parsum-node-sep*
            :move-cursor t
            :center-nodes nil))))))


(define-parse-summary-command (com-multiple-tree-menu)
    ((tree 'parsum :gesture :select))
    (pop-up-menu
        `(("Show enlarged tree" :value show)
          ("Highlight chart nodes" :value chart) 
          ("Partial chart" :value partial-chart)
          ("Generate" :value generate :active ,*mrs-loaded*)
          ("MRS" :value mrs :active ,*mrs-loaded*)
          ("Prolog MRS" :value prolog :active ,*mrs-loaded*)
          ("RMRS" :value rmrs :active ,*mrs-loaded*)
          ("DMRS" :value dmrs :active ,*mrs-loaded*)
          ("Indexed MRS" :value indexed :active ,*mrs-loaded*)
          ("Scoped MRS" :value scoped :active ,*mrs-loaded*)
          #+:logon
          ("UTool MRS" :value utool :active ,*mrs-loaded*)
          ("Dependencies" :value dependencies :active ,*mrs-loaded*)
          ("FOL approximation" :value fol :active ,*mrs-loaded*)
          ("Rephrase" :value rephrase :active ,*mrs-loaded*))
        (show (draw-new-parse-tree
                (parsum-top tree)
                (format nil "Parse Tree for \"~A\""
                  (shortened-sentence-string (edge-leaves (parsum-edge tree))))
                nil 
                (parse-summary-current-chart clim:*application-frame*)))
        (chart
             (if (or (not (parse-summary-current-chart clim:*application-frame*))
                     (eql (parse-summary-current-chart clim:*application-frame*)
                          *chart-generation-counter*))
                 (progn
                   (cond ((and *main-chart-frame* 
                               (eq (clim:frame-state *main-chart-frame*) 
                                   :enabled)))
                         ((and *main-chart-frame* 
                               (eq (clim:frame-state *main-chart-frame*) 
                                   :shrunk))
                           (clim:raise-frame *main-chart-frame*))
                         (t
                           (show-chart) 
                           (wait-until-chart-ready)))
                   (display-edge-in-chart (parsum-edge tree)))
                 (lkb-beep)))
        (partial-chart
             (if (or (not (parse-summary-current-chart clim:*application-frame*))
                     (eql (parse-summary-current-chart clim:*application-frame*)
                          *chart-generation-counter*))
                 (multiple-value-bind (root subframe-p)
                   (cond ((and *main-chart-frame* 
                               (eq (clim:frame-state *main-chart-frame*) 
                                   :enabled))
                           (values (chart-window-root *main-chart-frame*) t))
                         ((and *main-chart-frame* 
                               (eq (clim:frame-state *main-chart-frame*) 
                                   :shrunk))
                           (values (chart-window-root *main-chart-frame*) t))
                         (t
                           (values (construct-chart-no-display) nil)))
                   (display-partial-chart root (parsum-edge tree) subframe-p))
                 (lkb-beep)))
        ;; funcall avoids undefined function warnings
        (generate (funcall 'really-generate-from-edge (parsum-edge tree)))
        (mrs (funcall 'show-mrs-window (parsum-edge tree)))
        (indexed (funcall 'show-mrs-indexed-window (parsum-edge tree)))
        (prolog (funcall 'show-mrs-prolog-window (parsum-edge tree)))
        (scoped (funcall 'show-mrs-scoped-window (parsum-edge tree)))
        #+:logon
        (utool (funcall 'show-mrs-utool-window (parsum-edge tree)))
        (rmrs (funcall 'show-mrs-rmrs-window (parsum-edge tree)))
        (dmrs (funcall 'show-mrs-dmrs-window (parsum-edge tree)))
        (dependencies 
             (funcall 'show-mrs-dependencies-window (parsum-edge tree)))
        (fol 
             (funcall 'show-mrs-fol-window (parsum-edge tree)))
        (rephrase
             (let ((symbol (when (find-package :mt)
                             (find-symbol "REPHRASE" :mt))))
               (when (and symbol (fboundp symbol))
                 (funcall symbol (parsum-edge tree)))))))


(defun wait-until-chart-ready nil
  (mp:process-wait-with-timeout "Waiting" 5
    #'(lambda ()
        (and *main-chart-frame*
             (eql (clim:frame-state *main-chart-frame*) :enabled)
             #+:mcclim
             (clim:frame-properties *main-chart-frame* 'finished)
             #-:mcclim
             (getf (clim:frame-properties *main-chart-frame*) 'finished)))))


(define-parse-summary-command (com-show-chart-from-tree :menu "Show chart")
    ()   
  (if (or (not (parse-summary-current-chart clim:*application-frame*))
          (eql (parse-summary-current-chart clim:*application-frame*)
               *chart-generation-counter*))
      (show-chart)
    (lkb-beep)))

(define-parse-summary-command 
    (com-compare-from-tree :name "Compare" :menu t)
    ()
  (clim:with-application-frame (frame)
    (let ((edges (loop
                     for tree in (parse-summary-trees frame)
                     collect (parsum-edge tree))))
      (compare-parses edges))))
