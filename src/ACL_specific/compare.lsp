;;; Copyright (c) 1998--2002
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

;;;
;;; Tools to help build a treebank
;;;

;;;
;;; ToDO
;;;
;;; - at least one-level of `Undo';
;;; - Shift and Ctrl accelerators on `Next' et al.;
;;; - define equivalence classes of discriminants (possibly based on equality
;;;   of in and out sets), so that `yes' selections can propagate;
;;; - group equivalent discriminators somehow;
;;; - add info bar: show numbers of in and out trees per discriminant
;;; - bug fix: identify multiple applications of unary rule at same position;
;;; - investigate lack of edge for leafs in tree in interactive mode;
;;;

(in-package :lkb)

(def-lkb-parameter *tree-comparison-threshold* 10000)

(def-lkb-parameter *tree-display-threshold* 20)

(def-lkb-parameter *tree-display-view* :classic)

(def-lkb-parameter *tree-display-trees-width* 530)

(def-lkb-parameter *tree-display-discriminants-width* 430)

(def-lkb-parameter *tree-display-height* 630)

;;;
;;; a switch to control auto-advance in update mode: set to non-nil to enable
;;; auto-save in updates; set to a (positive) number to cause that many seconds
;;; delay before the auto-save and only auto-save (and -advance) on unambiguous
;;; items (i.e. where the recorded decisions completely disambiguate the new
;;; parses).
;;;
(def-lkb-parameter *tree-automatic-update-p* nil)

(def-lkb-parameter *tree-skeptical-update-p* nil)

(def-lkb-parameter *tree-update-match-hook* nil)

(def-lkb-parameter *tree-save-on-reject-p* t)

(def-lkb-parameter *tree-display-semantics-p* t)

(defparameter *tree-initialization-hook* nil)

(defparameter *tree-completion-hook* nil)

(defstruct ctree 
  edge
  id
  score
  flags
  symbol
  record
  ink)

(defun compare-parses (&optional (edges *parse-record*)
                       &key input mode view)

  (when (eq edges *parse-record*)
    ;;
    ;; fix up edges in chart: one fine day, we should really simplify the chart
    ;; set-up: there are way too many structures, for no apparent reason ...
    ;;
    ;;
    ;; _fix_me_
    ;; as of 5-aug-03, presumably, this should no longer be necessary: the edge
    ;; constructor should now always maintain .from. and .to. correctly.
    ;;                                                          (6-aug-03; oe)
    ;;
    (loop
        for i from 1 to (- *chart-limit* 1)
        for configurations = (aref *chart* i 0)
        do
          (loop
              for configuration in configurations
              for begin = (chart-configuration-begin configuration)
              for end = (chart-configuration-end configuration)
              for edge = (chart-configuration-edge configuration)
              do
                (setf (edge-from edge) begin)
                (setf (edge-to edge) end))))
  
  (when edges 
    (mp:run-function "Tree Comparison"
     #'(lambda ()
         (let ((*tree-discriminants-mode* 
                (or mode *tree-discriminants-mode*))
               (*tree-display-view*
                (or view *tree-display-view*)))
           (compare edges :input input))))))

(defun compare (edges &key (runp t) input)
                           
  (let ((frame (if runp
                 (clim:make-application-frame 'compare-frame)
                 (clim:make-application-frame 
                  'compare-frame :frame-manager nil))))
    #+:debug
    (setf %frame% frame)
    (setf (compare-frame-chart frame) *chart-generation-counter*)
    (setf (compare-frame-runp frame) runp)
    (set-up-compare-frame frame edges :input input)
    (setf (clim:frame-pretty-name frame) 
      (or input (format nil "~a" (edge-leaves (first edges)))))
    (when runp
      (funcall #'clim:run-frame-top-level frame))
    frame))

(defun run-compare-frame (frame)
  (clim:run-frame-top-level frame))

(defun set-up-compare-frame (frame edges 
                             &key (runp (compare-frame-runp frame)) 
				  input
				  (display (let ((foo (getenv "DISPLAY")))
					     (and (stringp foo) 
						  (not (string= foo "")) 
						  foo))))

  (setf *cached-category-abbs* nil)

  (setf (compare-frame-runp frame) runp)
  ;;
  ;; when called in error analysis mode, i.e. against the scoring results of
  ;; some stochastic parse selection model, throw out all edges that are not 
  ;; part of the set of trees for closer inspection.
  ;;
  (when (compare-frame-inspect frame)
    (setf (compare-frame-display frame) :inspect)
    (loop
        with result = nil
        with inspect = (compare-frame-inspect frame)
        with all = (append (first inspect) 
                           (list (second inspect)) 
                           (third inspect))
        for edge in edges
        for id = (edge-foo edge)
        for score = (find id all :key #'(lambda (foo)
                                          (rest (assoc :result-id foo))))
        when score do 
          (setf (edge-score edge) (rest (assoc :score score)))
          (push edge result)
        finally
          (setf edges (nreverse result))))

  ;;
  ;; large sets of edges can take some time (and memory :-{) to display; query
  ;; for user confirmation before moving on.
  ;;
  (when (and runp display
             (integerp *tree-comparison-threshold*)
             (> (length edges) *tree-comparison-threshold*)
             (or *tree-automatic-update-p*
                 (null (clim:notify-user 
                        frame
                        (format 
                         nil 
                         "Excessive Set of Trees (~a).  Continue?"
                         (length edges))
                        :style :question 
                        :title "Tree Comparison Safeguard"))))
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :skip) frame)
    (return-from set-up-compare-frame :skip))

  (when (and runp display) (frame-cursor frame :vertical-scroll))
  ;;
  ;; _fix_me_
  ;; probably, we should move this further down, until after extraction of
  ;; discriminants, so that start-up time is not reflected.
  ;;                                                           (6-jun-04; oe)
  ;;
  (setf (compare-frame-decisions frame) (list (make-decision :type :start)))
  (when (null (compare-frame-input frame))
    (setf (compare-frame-input frame) 
      (or input (format nil "~{~a~^ ~}" (edge-leaves (first edges))))))

  ;;
  ;; wrap each edge into a `ctree' structure, so that we can record additional
  ;; information (associated parse tree symbol, CLIM output record, et al.).
  ;;
  (setf (compare-frame-trees frame)
    (loop
        with inspect = (compare-frame-inspect frame)
        with gold = (rest (assoc :result-id (second inspect)))
        for i from 0
        for edge in edges
        for id = (if (numberp (edge-foo edge))
                   (edge-foo edge)
                   i)
        for score = (edge-score edge)
        for symbol = (when (and *tree-use-node-labels-p*
                                (eq (compare-frame-view frame) :classic))
                       (make-new-parse-tree edge 1 t))
        for flags = (when (and gold (= id gold)) (list :gold))
        for tree = (make-ctree :edge edge :id id 
                               :score score :flags flags :symbol symbol)
        collect tree
        when (and runp *tree-use-node-labels-p* (zerop (mod i 50))) do
          #+:allegro
          (format
           excl:*initial-terminal-io*
           "~&[~a] set-up-compare-frame(): expanding tree # ~a~%"
           (current-time :long :short) i)
          #-:allegro
          nil
        finally
          #+:allegro
          (when (and runp *tree-use-node-labels-p*)
            (format
             excl:*initial-terminal-io*
             "~&[~a] set-up-compare-frame(): rebuilt ~a tree~p.~%"
             (current-time :long :short) (length edges) (length edges)))
          #-:allegro
          nil))
  ;;
  ;; keep copy of original set of trees; needed for full resets
  ;;
  (setf (compare-frame-otrees frame) (copy-list (compare-frame-trees frame)))

  ;;
  ;; extract (minimal) set of elementary properties to discriminate analyses
  ;;
  (unless (compare-frame-exact frame)
    ;;
    ;; _fix_me_
    ;; there are different usages of the 'exact' mode, including picking out
    ;; derivations among realization results that can be aligned (in various
    ;; interpretations of that term) with the 'gold' parse that provided the
    ;; input semantics for generation.  i dimly recall that discriminants in
    ;; that setup ended up ill-defined (presumably because one and the same
    ;; edge can occur in different surface positions), hence we disabled the
    ;; extraction of discriminants below.  in another 'exact' use case, viz.
    ;; finding matching lexical sub-trees in a lexical-only profile, it would
    ;; seem possible to extract discriminants, which would then have their
    ;; values determined by an update-discriminants() call below.  however, we
    ;; would need another flag or something, to communicate whether or not the
    ;; compare frame would actually extract discriminants.  so far, at least,
    ;; there is no practical benefit to having available discriminants for the
    ;; above use cases, however.                              (12-aug-11; oe)
    ;;
    (setf (compare-frame-discriminants frame)
      (find-discriminants
       edges
       :mode (compare-frame-mode frame)
       :tags (compare-frame-tags frame)))
    #+:allegro
    (when runp
      (format
       excl:*initial-terminal-io*
       "~&[~a] set-up-compare-frame(): found ~a discriminant~p.~%"
       (current-time :long :short)
       (length (compare-frame-discriminants frame)) 
       (length (compare-frame-discriminants frame)))))

  ;;
  ;; preset discriminants from recorded decisions or gold decisions (during an
  ;; update); record `gold' discriminants that are no longer pertinent.
  ;;
  (unless (compare-frame-exact frame)
    (setf (compare-frame-lead frame)
      (preset-discriminants 
       (compare-frame-discriminants frame) 
       (compare-frame-preset frame)
       (compare-frame-gold frame)
       (compute-discriminant-skew edges))))

  #+:allegro
  (when runp
    (loop
        initially
          (format
           excl:*initial-terminal-io*
           "~&~%[~@[~a~]] `~a'~%~%"
           (compare-frame-item frame) (compare-frame-input frame))
        for foo in (compare-frame-lead frame)
        do
          (format
           excl:*initial-terminal-io*
           "  [~2,'0d ~2,'0d] ~a ~a | ~a~@[ `~a'~]~%"
           (discriminant-start foo) (discriminant-end foo)
           (discriminant-state-as-string foo)
           (discriminant-toggle-as-string foo)
           (discriminant-key foo) (discriminant-value foo))
        finally (when (compare-frame-lead frame)
                  (format excl:*initial-terminal-io* "~%"))))

  (setf (compare-frame-edges frame) edges)
  (unless (compare-frame-exact frame)
    (recompute-in-and-out frame t))

  ;;
  ;; when running in `exact match' update mode, adjust discriminants and most
  ;; everything else to zoom in on the matching derivation, if any
  ;;
  (when (compare-frame-exact frame)
    (setf (compare-frame-lead frame) nil)
    (when (compare-frame-discriminants frame)
      (update-discriminants 
       (compare-frame-discriminants frame)
       (compare-frame-exact frame) t)
      #+:null
      (recompute-in-and-out frame))
    (setf (compare-frame-in frame) nil)
    (setf (compare-frame-out frame) nil)

    (loop
        for edge in (compare-frame-edges frame)
        when (member edge (compare-frame-exact frame))
        do (push edge (compare-frame-in frame))
        else do (push edge (compare-frame-out frame))))

  (when *tree-initialization-hook*
    (let* ((hook *tree-initialization-hook*)
           (extras (and (consp hook) (rest hook)))
           (hook (if (consp hook) (first hook) hook))
           (hook (typecase hook
                   (function hook)
                   (symbol (and (fboundp hook) (symbol-function hook)))
                   (string (ignore-errors 
                            (symbol-function (read-from-string hook)))))))
      (multiple-value-bind (result condition) 
          (when (functionp hook)
            (ignore-errors (apply hook (cons frame extras))))
        (cond
         ((= (length result) (length (compare-frame-discriminants frame)))
          (loop
              for discriminant in (compare-frame-discriminants frame)
              for state in result
              unless (eq state :unknown) do
                (setf (discriminant-toggle discriminant) state)
                (setf (discriminant-state discriminant) state)
              finally (recompute-in-and-out frame)))
         ((null result))
         (t
          (format
           #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
           "tree-initialization-hook(): ~
            discriminant count mismatch (~a vs. ~a).~%"
           (length result) (length (compare-frame-discriminants frame)))))
        (when condition
          (clim:beep)
          (format
           #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
           "tree-initialization-hook(): error `~a'.~%"
           (normalize-string (format nil "~a" condition)))))))

  ;;
  ;; extract some quantitative summary measures on update procedure; entirely
  ;; for record keeping purposes.
  ;;
  (when (and runp (compare-frame-update frame))
    (push (cons :u-matches
                (loop
                    for foo in (compare-frame-discriminants frame)
                    count (discriminant-gold foo)))
          (compare-frame-update frame))
    (push (cons :u-mismatches (length (compare-frame-lead frame)))
          (compare-frame-update frame))
    (push (cons :u-pin (length (compare-frame-in frame)))
          (compare-frame-update frame))
    (push (cons :u-pout (length (compare-frame-out frame))) 
          (compare-frame-update frame)))

  ;;
  ;; in case the preferred tree was selected directly from the tree display,
  ;; we may not have recorded active decisions, i.e. the choice was reflected
  ;; in entailed discriminant states only.
  ;;
  (when (find :select (compare-frame-preset frame) :key #'discriminant-type)
    (loop
        for discriminant in (compare-frame-discriminants frame)
        for preset = (discriminant-preset discriminant)
        when preset do
          (setf (discriminant-state discriminant) 
            (discriminant-state preset)))
    (recompute-in-and-out frame))
  (when (find :reject (compare-frame-preset frame) :key #'discriminant-type)
    (setf (compare-frame-in frame) nil)
    (setf (compare-frame-out frame) edges))

  (when (and runp *tree-skeptical-update-p*)
    (let ((gactive (compare-frame-gactive frame)))
      (when (and (numberp gactive) (zerop gactive))
        (setf (compare-frame-in frame) nil)
        (setf (compare-frame-out frame) (compare-frame-edges frame)))))

  ;;
  ;; always update tree and discriminant state here: this will cause the frame
  ;; to redraw both lower panes.
  ;; 
  (when (and runp display)
    (clim::redisplay-frame-pane frame 'top :force-p t)
    (clim::redisplay-frame-pane frame 'comment :force-p t)
    (clim::redisplay-frame-pane frame 'status :force-p t))
  (when display (update-trees frame))

  ;;
  ;; _fix_me_
  ;; need to disable input from frame for this to work reliably. 
  ;;                                                         (14-oct-02 ; oe)
  (when (and runp *tree-automatic-update-p*)
    (when (numberp *tree-automatic-update-p*)
      (sleep *tree-automatic-update-p*))
    (record-decision 
     (make-decision :type (if (update-match-p frame) :save :flag))
     frame)
    (return-from set-up-compare-frame :skip))

  (setf (compare-frame-gactive frame) nil)
  (when (and runp display) (frame-cursor frame :default)))

(defun reset-or-reconsider-compare-frame (frame &optional mode)

  #+:debug
  (setf %frame frame)
  (setf (compare-frame-trees frame) (compare-frame-otrees frame))
  (setf (compare-frame-in frame) (compare-frame-edges frame))
  (setf (compare-frame-out frame) nil)
  (loop
      for foo in (compare-frame-discriminants frame)
      do
        (setf (discriminant-time foo) (get-universal-time))
        (setf (discriminant-gold foo) nil)
        (setf (discriminant-preset foo) nil)
        (setf (discriminant-state foo) :unknown)
        (setf (discriminant-toggle foo) :unknown))

  ;;
  ;; preset discriminants from recorded decisions or gold decisions (during an
  ;; update); record `gold' discriminants that are no longer pertinent.
  ;;
  (unless (compare-frame-exact frame)
    (setf (compare-frame-lead frame)
      (preset-discriminants 
       (compare-frame-discriminants frame) 
       (compare-frame-preset frame)
       (compare-frame-gold frame)
       (compute-discriminant-skew (compare-frame-edges frame))
       nil)))

  (unless (compare-frame-exact frame)
    (recompute-in-and-out frame t))

  (unless (eq mode :reconsider)  
    (when (find :reject (compare-frame-preset frame) :key #'discriminant-type)
      (setf (compare-frame-in frame) nil)
      (setf (compare-frame-out frame) (compare-frame-edges frame))))

  ;;
  ;; always update tree and discriminant state here: this will cause the frame
  ;; to redraw both lower panes.
  ;; 
  (clim::redisplay-frame-pane frame 'top :force-p t)
  (clim::redisplay-frame-pane frame 'comment :force-p t)
  (clim::redisplay-frame-pane frame 'status :force-p t)
  (update-trees frame))

(defstruct decision
  type
  value
  (time (get-universal-time)))

(defun record-decision (decision &optional frame)
  (if frame
    (let ((value (decision-value decision)))
      (when (discriminant-p value) 
        (setf (discriminant-time value) (decision-time decision)))
      (push decision (compare-frame-decisions frame)))
    (clim:with-application-frame (frame)
      (record-decision decision frame))))
  
(defun comparison-top-font ()
  (list :sans-serif :roman 12))

(defun comparison-tree-font ()
  (list :sans-serif :roman (or *comparison-tree-font-size* 7)))

(defun comparison-dependencies-font ()
  (list :sans-serif :roman (or *comparison-dependencies-font-size* 10)))

(defun comparison-comment-font ()
  (list :sans-serif :roman (or *comparison-discriminant-font-size* 10)))

(defun comparison-status-font ()
  (list :sans-serif :roman (or *comparison-discriminant-font-size* 10)))

(defun comparison-discriminant-font ()
  (list :sans-serif :roman (or *comparison-discriminant-font-size* 10)))

(clim:define-application-frame compare-frame ()
  ((runp :initform 0 :accessor compare-frame-runp)
   (item :initform nil :accessor compare-frame-item)
   (input :initform nil :accessor compare-frame-input)
   (tags :initform nil :accessor compare-frame-tags)
   (start :initform nil :accessor compare-frame-start)
   (end :initform nil :accessor compare-frame-end)
   (edges :initform nil :accessor compare-frame-edges)
   (derivations :initform nil :accessor compare-frame-derivations)
   (trees :initform nil :accessor compare-frame-trees)
   (otrees :initform nil :accessor compare-frame-otrees)
   (preset :initform nil :accessor compare-frame-preset)
   (exact :initform nil :accessor compare-frame-exact)
   (gold :initform nil :accessor compare-frame-gold)
   (lead :initform nil :accessor compare-frame-lead)
   (mode :initform *tree-discriminants-mode* :accessor compare-frame-mode)
   (view :initform *tree-display-view* :accessor compare-frame-view)
   (show :initform *tree-results-show* :accessor compare-frame-show)
   (discriminants :initform nil :accessor compare-frame-discriminants)
   (decisions :initform nil :accessor compare-frame-decisions)
   (confidence :initform nil :accessor compare-frame-confidence)
   (in :initform nil :accessor compare-frame-in)
   (out :initform nil :accessor compare-frame-out)
   (display :initform :concise :accessor compare-frame-display)
   (threshold 
    :initform *tree-display-threshold* :accessor compare-frame-threshold)
   (tstream :initform nil :accessor compare-frame-tstream)
   (chart :initform nil :accessor  compare-frame-chart)
   (comment :initform " " :accessor compare-frame-comment)
   (version :initform nil :accessor compare-frame-version)
   (gversion :initform nil :accessor compare-frame-gversion)
   (gactive :initform nil :accessor compare-frame-gactive)
   (gderivation :initform nil :accessor compare-frame-gderivation)
   (inspect :initform nil :accessor compare-frame-inspect)
   (update :initform nil :accessor compare-frame-update)
   (ids :initform nil :accessor compare-frame-ids)
   (controller :initform nil :accessor compare-frame-controller))
  (:panes
   (top
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 2)  
        (clim:make-pane 'clim:application-pane
                        :display-function 'draw-top-window
                        :text-cursor nil
                        :text-style (comparison-top-font)
                        :height :compute
                        :end-of-line-action :allow
                        :end-of-page-action :allow
                        :borders nil
                        :incremental-redisplay nil
                        :display-time nil
                        :background clim:+white+
                        :foreground clim:+black+))))
   
   (trees  
    (clim:spacing (:thickness 0)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-trees-window
			  :text-cursor nil
			  :width *tree-display-trees-width*
			  :text-style (comparison-tree-font)
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  :incremental-redisplay nil
			  :display-time nil
			  :background clim:+white+
			  :foreground clim:+black+)))))
   (comment 
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
        (clim:make-pane 'clim:application-pane
                        :display-function 'draw-comment-window
                        :text-cursor nil
                        :text-style (comparison-comment-font)
                        :height :compute
                        :end-of-line-action :allow
                        :end-of-page-action :allow
                        :borders nil
                        :incremental-redisplay nil
                        :display-time nil
                        :background clim:+white+
                        :foreground clim:+black+))))
   (status 
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
        (clim:make-pane 'clim:application-pane
                        :display-function 'draw-status-window
                        :text-cursor nil
                        :text-style (comparison-status-font)
                        :height :compute
                        :end-of-line-action :allow
                        :end-of-page-action :allow
                        :borders nil
                        :incremental-redisplay nil
                        :display-time nil
                        :background clim:+white+
                        :foreground clim:+black+))))
   (discriminants  
    (clim:spacing (:thickness 0)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-discriminants-window
			  :text-cursor nil
			  :width *tree-display-discriminants-width*
			  :height *tree-display-height*
			  :text-style (comparison-discriminant-font)
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  :incremental-redisplay t
			  :background clim:+white+
			  :foreground clim:+black+))))))
  (:layouts
   (:default (clim:vertically ()
               top
               (clim:horizontally () 
                 trees 
                 (clim:vertically () comment status discriminants))))))

(define-compare-frame-command (com-exit-compare-frame :menu "Close")
    ()
 (clim:with-application-frame (frame)
   (record-decision (make-decision :type :close) frame)
   (setf *tree-display-threshold* (compare-frame-threshold frame))
   (if (compare-frame-controller frame)
     (mp:process-revoke-arrest-reason (compare-frame-controller frame) :wait)
     (clim:frame-exit frame))))

#+:null
(define-compare-frame-command (com-first-compare-frame :menu "First")
    ()
  (clim:with-application-frame (frame)
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :first) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-previous-compare-frame :menu "Previous")
    ()
  (clim:with-application-frame (frame)
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :previous) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-next-compare-frame :menu "Next")
    ()
  (clim:with-application-frame (frame)
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :next) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

#+:null
(define-compare-frame-command (com-last-compare-frame :menu "Last")
    ()
  (clim:with-application-frame (frame)
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :last) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-reject-compare-frame :menu "Reject")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :reject) frame)
    (setf (compare-frame-in frame) nil)
    (setf (compare-frame-out frame) (compare-frame-edges frame))
    (update-trees frame t :discriminant)
    (when *tree-save-on-reject-p*
      (record-decision (make-decision :type :save))
      (when (compare-frame-controller frame) 
        (mp:process-revoke-arrest-reason 
         (compare-frame-controller frame) :wait)))))

(define-compare-frame-command (com-clear-compare-frame :menu "Clear")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :clear) frame)
    (reset-discriminants frame)))

(define-compare-frame-command (com-reset-compare-frame :menu "Reset")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :reset) frame)
    (reset-or-reconsider-compare-frame frame)))

(define-compare-frame-command (com-reconsider-compare-frame :menu "Reconsider")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :reset) frame)
    (reset-or-reconsider-compare-frame frame :reconsider)))

(define-compare-frame-command (com-ordered-compare-frame :menu "Ordered")
    ()
  (clim:with-application-frame (frame)
    (setf (compare-frame-display frame) :ordered)
    (update-trees frame)))


(define-compare-frame-command (com-concise-compare-frame :menu "Concise")
    ()
  (clim:with-application-frame (frame)
    (setf (compare-frame-display frame) :concise)
    (update-trees frame)))


(define-compare-frame-command (com-full-compare-frame :menu "Full")
    ()
  (clim:with-application-frame (frame)
    (setf (compare-frame-display frame) nil)
    (update-trees frame)))

(define-compare-frame-command (com-save-compare-frame :menu "Save")
    ()
  (clim:with-application-frame (frame)
    (frame-cursor frame :horizontal-scroll)
    (record-decision (make-decision :type :save))
    (when (compare-frame-controller frame) 
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-confidence-compare-frame :menu "Confidence")
    ()
  (clim:with-application-frame (frame)
    (let ((command (clim:menu-choose
                    '(("High (3)" :value 3 :active t)
                      ("Fair (2)" :value 2 :active t)
                      ("Low (1)" :value 1 :active t)
                      ("Zero (0)" :value 0 :active t)))))
      (when command
        (setf (compare-frame-confidence frame) command)))
    (clim::redisplay-frame-pane frame 'top :force-p t)))

(define-compare-frame-command (com-toggle-compare-frame :menu "Toggle")
    ()
  (clim:with-application-frame (frame)
    (if (and (integerp *tree-display-threshold*)
             (= *tree-display-threshold* (compare-frame-threshold frame)))
      (setf *tree-display-threshold* nil)
      (setf *tree-display-threshold* (compare-frame-threshold frame)))
    (update-trees frame)))

(defun draw-top-window (frame stream &rest rest)

  (declare (ignore rest))
  ;;
  ;; in case we were displaying the window with an uninitialized frame
  ;;
  (when (null (compare-frame-edges frame))
    (clim:formatting-table (stream)
      (clim:with-text-style (stream (comparison-top-font))
        (clim:formatting-row (stream)
          (let ((record 
                 (clim:formatting-cell (stream :align-x :center :min-width 950)
                   (format
                    stream
                    "- analyzing the parse forest; please wait -"))))
            (recolor-record record clim:+red+)
            (clim:replay record stream)))))
    (return-from draw-top-window))

  (clim:formatting-table (stream)
    (clim:with-text-style (stream (comparison-top-font))
      (clim:formatting-row (stream)
        (let ((record 
               (clim:formatting-cell (stream :align-x :center :min-width 950)
                 (format
                  stream
                  "~:[~*~;(~a) ~][~a : ~a~@[ @ ~a~]]  ~a"
                  (compare-frame-item frame) (compare-frame-item frame)
                  (length (compare-frame-in frame)) 
                  (length (compare-frame-out frame))
                  (let ((foo (compare-frame-confidence frame)))
                    (when (and (integerp foo) (>= foo 0) (<= foo 3))
                      (aref #("zero" "low" "fair" "high") foo)))
                  (compare-frame-input frame)))))
          (when (= (length (compare-frame-in frame)) 1)
            (recolor-record record clim:+blue+)
            (clim:replay record stream))
          (when (update-match-p frame)
            (recolor-record record clim:+magenta+)
            (clim:replay record stream)))))))

(defun draw-trees-window (frame stream &rest rest)

  (declare (ignore rest))
  ;;
  ;; in case we were displaying the window with an uninitialized frame
  ;;
  (when (null (compare-frame-edges frame))
    (return-from draw-trees-window))

  (setf (compare-frame-tstream frame) stream)
  (unless (and (integerp *tree-display-threshold*)
               (> (length (compare-frame-trees frame)) 
                  *tree-display-threshold*))
    (clim:formatting-table (stream :x-spacing "X")
      (loop
          for tree in (compare-frame-trees frame)
          do
            (setf (ctree-ink tree) clim:+foreground-ink+)
            (setf (ctree-record tree)
              (clim:with-new-output-record (stream)
                (clim:with-text-style (stream (comparison-tree-font))
                  (clim:with-output-recording-options (stream :record t)
                    (clim:formatting-row (stream)
                      (clim:formatting-cell 
                          (stream :align-x :center :align-y :top)
                        (clim:with-text-style 
                            (stream 
                             (clim:parse-text-style '(:sans-serif :bold 12)))
                          (format stream "~%[~a]" (ctree-id tree))))
                      (clim:formatting-cell 
                          (stream :align-x :left :align-y :center)
                        (clim:formatting-row (stream)
                          (clim:formatting-cell 
                              (stream :align-x :left :align-y :top)
                            (format stream "~@[(~a)~]~%" (ctree-score tree)))
                          (clim:formatting-cell 
                              (stream :align-x :center :align-y :top)
                            (clim:with-output-as-presentation 
                                (stream tree 'ctree :single-box t)
                              (if (eq (compare-frame-view frame) :classic)
                                (clim:format-graph-from-root
                                 (or (ctree-symbol tree)
                                     (setf (ctree-symbol tree)
                                       (make-new-parse-tree 
                                        (ctree-edge tree) 1)))
                                 #'(lambda (node stream)
                                     (multiple-value-bind (s bold-p) 
                                         (get-string-for-edge node)
                                       (clim:with-text-face
                                           (stream (if bold-p :bold :roman))
                                         (write-string s stream))))
                                 #'(lambda (node) (get node 'daughters))
                                 :graph-type :parse-tree
                                 :stream stream 
                                 :merge-duplicates nil
                                 :orientation :vertical
                                 :generation-separation 7
                                 :move-cursor t
                                 :within-generation-separation 7
                                 :center-nodes nil)
                                (let* ((edge (ctree-edge tree))
                                       (mrs (or (edge-mrs edge)
                                                (setf (edge-mrs edge)
                                                  (ignore-errors
                                                   (mrs::extract-mrs edge))))))
                                  (when mrs
                                    (mrs:eds-output-psoa
                                     mrs :stream stream))))))))
                      (terpri stream)))))))
      (when (and (compare-frame-trees frame)
                 (null (rest (compare-frame-trees frame))))
        (draw-trees-window-completion frame stream)))
    (update-tree-colours frame)))

(define-compare-frame-command (com-tree-popup)
    ((tree 'ctree :gesture :select))

  (let* ((mrsp *mrs-loaded*)
         (command (clim:menu-choose
                   (list
                    '("Yes" :value yes :active t)
                    #+:null
                    '("No" :value no :active t)
                    '("Enlarged Tree" :value show)
                    (list "MRS" :value 'mrs :active mrsp)
		    (list "RMRS" :value 'rmrs :active mrsp)
                    (list "Indexed MRS" :value 'indexed :active mrsp)
                    (list "Scoped MRS" :value 'scoped :active mrsp)
                    #+:logon
                    (list "UTool MRS" :value 'utool :active mrsp)
                    (list "Dependencies" :value 'dependencies :active mrsp)
                    (list "Rephrase" :value 'rephrase :active mrsp))))
         (edge (ctree-edge tree)))
    (when command
      (handler-case
	  (ecase command
	    (yes 
             (record-decision 
              (make-decision :type :select :value edge))
	     (clim:with-application-frame (frame)
               (update-discriminants 
                (compare-frame-discriminants frame) edge t)
               (recompute-in-and-out frame)
               (if (smember (compare-frame-display frame) 
                            '(:concise :ordered :inspect))
                 (update-trees frame)
                 (update-tree-colours frame))))
	    (no
             ;;
             ;; _fix_me_
             ;; not sure what to do here: there may be no discriminant(s) to
             ;; exclusively rule out this single tree; we would presumably have
             ;; to create one and add it to the global list of discriminants;
             ;; not clear this is so desirable.                (12-oct-02; oe)
             ;;
             (record-decision 
              (make-decision :type :drop :value edge))
	     (clim:with-application-frame (frame)
               (update-discriminants 
                (compare-frame-discriminants frame) edge nil)
               (if (smember (compare-frame-display frame) 
                            '(:concise :ordered :inspect))
                 (update-trees frame)
                 (update-tree-colours frame))))
	    (show 
             (clim:with-application-frame (frame)
               (display-parse-tree
                nil nil
                :symbol (or (ctree-symbol tree)
                            (setf (ctree-symbol tree)
                              (make-new-parse-tree (ctree-edge tree) 1)))
                :title (format nil "Parse Tree #~a" (ctree-id tree))
                :counter (compare-frame-chart frame))))
            (mrs
             (when edge
               (ignore-errors (funcall 'show-mrs-window edge))))
	    (rmrs 
	     (when edge
	       (ignore-errors (funcall 'show-mrs-rmrs-window edge))))
            (indexed
             (when edge
               (ignore-errors (funcall 'show-mrs-indexed-window edge))))
            (scoped
             (when edge
               (ignore-errors (funcall 'show-mrs-scoped-window edge))))
            #+:logon
            (utool
             (when edge
               (ignore-errors (funcall 'show-mrs-utool-window edge))))
            (dependencies
             (when edge
               (ignore-errors (funcall 'show-mrs-dependencies-window edge))))
            (rephrase
             (let ((symbol (when (find-package :mt)
                             (find-symbol "REPHRASE" :mt))))
               (when (and symbol (fboundp symbol))
                 (funcall symbol edge)))))
            
        (storage-condition (condition)
          (with-output-to-top ()
            (format t "~%Memory allocation problem: ~A~%" condition)))
	(error (condition)
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))
        (serious-condition (condition)
          (with-output-to-top ()
            (format t "~%Something nasty: ~A~%" condition)))))))

(defun draw-trees-window-completion (frame stream)
  (let* ((hook *tree-completion-hook*)
         (hook (typecase hook
                 (null nil)
                 (function hook)
                 (symbol (and (fboundp hook) (symbol-function hook)))
                 (string (ignore-errors 
                          (symbol-function (read-from-string hook))))))
         (tree (first (compare-frame-trees frame)))
         (edge (ctree-edge tree))
         (mrs (or (edge-mrs edge)
                  (ignore-errors (mrs::extract-mrs edge))))
         (eds (when mrs (ignore-errors (mrs:eds-convert-psoa mrs)))))
    
    (multiple-value-bind (result condition) 
        (when (functionp hook) (ignore-errors (funcall hook edge mrs)))
      (when condition
        (clim:beep)
        (format
         #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
         "tree-completion-hook(): error `~a'.~%"
         (normalize-string (format nil "~a" condition))))
      (let* ((comment (if (stringp result)
                        result
                        (rest (assoc :comment result))))
             (result (unless (stringp result) result))
             (font (rest (assoc :font result)))
             (face (rest (assoc :face result)))
             (size (rest (assoc :size result)))
             (style (when (or font face size)
                      (clim:merge-text-styles
                       (list font face size) '(:sans-serif :bold 12))))
             (color (rest (assoc :color result)))
             (color (ignore-errors (apply #'clim:make-rgb-color color)))
             (bottomp (rest (assoc :bottom result)))
             (align (or (rest (assoc :align result)) :center)))
        (when (and comment (null bottomp))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :center :align-y :top)
              (format stream " ")))
          (let ((record
                 (clim:with-new-output-record (stream)
                   (clim:formatting-row (stream)
                     (clim:formatting-cell 
                         (stream :align-x :center :align-y :top)
                       (format stream "" (ctree-id tree)))
                     (clim:formatting-cell (stream :align-x align)
                       (clim:with-text-style (stream style)
                         (format stream "~%~a" comment)))))))
            (when color (recolor-record record color))
            (clim:replay record stream)))
        (when (and eds (not (eq (compare-frame-view frame) :modern)))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :center :align-y :top)
              (format stream " ")))
          (let ((record 
                 (clim:with-new-output-record (stream)
                   (clim:formatting-row (stream)
                     (clim:formatting-cell 
                         (stream :align-x :center :align-y :top)
                       (clim:with-text-style 
                           (stream 
                            (clim:parse-text-style '(:sans-serif :bold 12)))
                         (format stream "[~a]" (ctree-id tree))))
                     (clim:formatting-cell (stream :align-x :left)
                       (clim:formatting-column (stream)
                         (clim:formatting-cell (stream :align-x :left)
                           (format stream "~@[(~a)~]~%" (ctree-score tree)))
                         (clim:formatting-cell (stream :align-x :center)
                           (clim:with-text-style 
                               (stream (comparison-dependencies-font))
                             (format stream "~a" eds)))))))))
            (recolor-record 
             record
             (let ((status (mrs:eds-suspicious-p eds))
                   (orange (or (clim:find-named-color
                                "orange" (clim:frame-palette frame) 
                                :errorp nil)
                               clim:+yellow+)))
               (cond
                ((member :cyclic status) clim:+red+)
                ((member :fragmented status) orange)
                (t (if (update-match-p frame) clim:+magenta+ clim:+blue+)))))
            (clim:replay record stream)))
        (when (and comment bottomp)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :center :align-y :top)
              (format stream " ")))
          (let ((record
                 (clim:with-new-output-record (stream)
                   (clim:formatting-row (stream)
                     (clim:formatting-cell 
                         (stream :align-x :center :align-y :top)
                       (format stream "" (ctree-id tree)))
                     (clim:formatting-cell (stream :align-x align)
                       (clim:with-text-style (stream style)
                         (format stream "~%~a" comment)))))))
            (when color (recolor-record record color))
            (clim:replay record stream)))))))

(defun draw-comment-window (frame stream &rest rest)
  
  (declare (ignore rest))

  (clim:with-text-style (stream (comparison-comment-font))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :min-width 410)
          (let* ((comment (compare-frame-comment frame))
                 (comment (if (or (null comment) (equal comment ""))
                            " "
                            comment)))
            (format stream "~a" comment)))))))

(defun draw-status-window (frame stream &rest rest)
  
  (declare (ignore rest))

  (clim:with-text-style (stream (comparison-status-font))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :min-width 410)
          (let ((version (compare-frame-version frame))
                (gold (compare-frame-gversion frame)))
            (if (and version (not (equal version "")))
              (format 
               stream 
               "~a~:[    ~;~]" 
               version (or (null gold) (equal gold "")))
              (format stream " "))
            (when (and gold (not (equal gold "")))
              (let ((record (clim:with-new-output-record (stream)
                              (format stream "~a" gold))))
                (recolor-record 
                 record 
                 (if (update-match-p frame) clim:+magenta+ clim:+blue+))
                (clim:replay record stream)))))))))

(defun draw-discriminants-window (frame stream &rest rest)

  (declare (ignore rest))

  ;;
  ;; in case we are displaying the window with an uninitialized frame
  ;;
  (when (or (null (compare-frame-edges frame))
            (null (compare-frame-discriminants frame)))
    (return-from draw-discriminants-window))

  (let ((discriminants (compare-frame-discriminants frame)))
    (clim:with-text-style (stream (comparison-discriminant-font))
      (clim:formatting-table (stream :x-spacing "X")
        (loop
            for item in discriminants
            for record = (discriminant-record item)
            when record do (clim:clear-output-record record)
            unless (discriminant-hidep item) do
              (clim:formatting-row (stream)
                (setf (discriminant-record item)
                  (clim:with-new-output-record (stream)
                    (clim:with-output-recording-options (stream :record t)
                      (clim:with-output-as-presentation
                          (stream item 'discriminant)
                        (clim:updating-output 
                            (stream :cache-value (discriminant-state item))
                          (clim:formatting-cell 
                              (stream :align-x :center :min-width "+")
                            (write-string
                             (discriminant-state-as-string item) stream)))
                        (clim:updating-output 
                            (stream :cache-value (discriminant-toggle item))
                          (clim:formatting-cell 
                              (stream :align-x :center :min-width "+")
                            (write-string 
                             (discriminant-toggle-as-string item) stream)))
                        (clim:formatting-cell (stream :align-x :left)
                          (format stream "~a" (discriminant-key item)))
                        (clim:formatting-cell (stream :align-x :left)
                          (format 
                           stream 
                           "~@[~a~]" (discriminant-value item))))))))
              ;;
              ;; _fix_me_
              ;; there ought to be a way of drawing things in the intended 
              ;; colour right from the start; apparently, neither rob nor i 
              ;; could work that out quickly; CLIM isa bitch to work with :-(.
              ;;                                               (9-oct-02; oe)
              (let ((record (discriminant-record item)))
                (when (discriminant-gold item)
                  (recolor-record record (if (update-match-p frame) 
                                           clim:+magenta+
                                           clim:+blue+))
                  (clim:replay record stream))))))))

(define-compare-frame-command (com-discriminant-popup)
    ((discriminant 'discriminant :gesture :select))
  (let ((command (clim:menu-choose
		  `(("Yes" :value yes)
		    ("No" :value no)
		    ("Unknown" :value unknown)
		    (,(format 
                       nil 
                       "In Parses (~a)" 
                       (length (discriminant-in discriminant)))
                     :value in)
		    (,(format
                       nil
                       "Out Parses (~a)"
                       (length (discriminant-out discriminant)))
                       :value out)))))
    (when command
      (handler-case
	  (ecase command
	    (yes 
             (record-decision (make-decision :type :yes :value discriminant))
             (setf (discriminant-state discriminant) t)
             (setf (discriminant-toggle discriminant) t))
	    (no 
             (record-decision (make-decision :type :no :value discriminant))
             (setf (discriminant-state discriminant) nil)
             (setf (discriminant-toggle discriminant) nil))
	    (unknown 
             (record-decision 
              (make-decision :type :unknown :value discriminant))
             (setf (discriminant-state discriminant) :unknown)
             (setf (discriminant-toggle discriminant) :unknown))
            ;;
            ;; _fix_me_
            ;; in concise mode, at least, make these only show trees that are
            ;; still available globally, i.e. ignore all trees that have become
            ;; out already.                                    (20-jan-03; oe)
            ;;
            (in (show-parse-tree-frame (discriminant-in discriminant)))
            (out (show-parse-tree-frame (discriminant-out discriminant))))
	(error (condition) 
	  (declare (ignore condition))
          nil))
      (clim:with-application-frame (frame)
        (recompute-in-and-out frame)
        (update-trees frame t :discriminant)))))

(defun update-trees (frame 
                     &optional (redrawp t) (context nil)
                     &key (runp (compare-frame-runp frame)))

  (when runp (frame-cursor frame :vertical-scroll))
  (if (null redrawp)
    (when runp (update-tree-colours frame))
    (let ((in (compare-frame-in frame)))
      (when runp
        (loop
            for tree in (compare-frame-trees frame)
            when (ctree-record tree) do
              (clim:clear-output-record (ctree-record tree))
              (setf (ctree-record tree) nil))
        (loop
            for discriminant in (compare-frame-discriminants frame)
            for record = (discriminant-record discriminant)
            when record do
              (clim:clear-output-record record)
              (setf (discriminant-record discriminant) nil)))
      (case (compare-frame-display frame)
        (:ordered
         (setf (compare-frame-trees frame)
           (stable-sort 
            (copy-list (compare-frame-otrees frame))
            #'(lambda (foo bar)
                (let ((foop (member (ctree-edge foo) in :test #'eq))
                      (barp (member (ctree-edge bar) in :test #'eq)))
                  (if (and foop (null barp))
                    t
                    (let ((foo (ctree-id foo))
                          (bar (ctree-id bar)))
                      (and (numberp foo) (numberp bar) (<= foo bar))))))))
         (loop
             for discriminant in (compare-frame-discriminants frame)
             do
               (setf (discriminant-hidep discriminant) nil)))
        (:inspect
         (setf (compare-frame-trees frame)
           (stable-sort 
            (copy-list (compare-frame-otrees frame))
            #'(lambda (foo bar)
                (let ((flags (ctree-flags foo))
                      (foo (ctree-score foo))
                      (bar (ctree-score bar)))
                  (when (and (numberp foo) (numberp bar))
                  (or (> foo bar)
                      (and (= foo bar) (smember :gold flags))))))))

         (loop
             for discriminant in (compare-frame-discriminants frame)
             do
               (setf (discriminant-hidep discriminant) nil)))
        (:concise
         (setf (compare-frame-trees frame)
           (loop
               for tree in (compare-frame-otrees frame)
               when (member (ctree-edge tree) in :test #'eq)
               collect tree))
         (loop
             with trees = (compare-frame-trees frame)
             with n = (length trees)
             for discriminant in (compare-frame-discriminants frame)
             for in = (discriminant-in discriminant)
             do
               (setf (discriminant-hidep discriminant)
                 (or (not (eq (discriminant-state discriminant) :unknown))
                     (and (<= n (length in))
                          (not (loop
                                   for tree in trees
                                   for edge = (ctree-edge tree)
                                   for match = (find edge in :test #'eq)
                                   thereis (null match))))))))
        (t
         (setf (compare-frame-trees frame) 
           (copy-list (compare-frame-otrees frame)))
         (loop
             for discriminant in (compare-frame-discriminants frame)
             do
               (setf (discriminant-hidep discriminant) nil))))

      (unless (or (null runp) (eq context :tree))
        (clim::redisplay-frame-pane frame 'trees :force-p t))
      ;;
      ;; _fix_me_
      ;; always force complete redraw: omitting this causes rather disturbing
      ;; glitches in the state and toggle displays in non-concise display mode
      ;; and the overlay of old ink with new ink in concise display; maybe our
      ;; drawing function needs improvement (maybe CLIM :-{).  (15-oct-02; oe)
      ;;
      (when runp
        (clim::redisplay-frame-pane frame 'discriminants :force-p t)
        (clim::redisplay-frame-pane frame 'top :force-p t))))
  
  (when runp (frame-cursor frame :default)))

(defun update-tree-colours (frame)

  (let ((stream (compare-frame-tstream frame)))
    (loop
        with in = (compare-frame-in frame)
        for tree in (compare-frame-trees frame)
        for ink = (cond ((not (member (ctree-edge tree) in :test #'eq))
                         clim:+red+)
                        ((and (not (rest in)) 
                              (eq (ctree-edge tree) (first in)))
                         (if (update-match-p frame) 
                           clim:+magenta+
                           clim:+blue+))
                        (t clim:+foreground-ink+))
        for record = (ctree-record tree)
        unless (or (null record) (eq ink (ctree-ink tree)))
        do
          (setf (ctree-ink tree) ink)
          (recolor-record record ink)
          (clim:replay record stream))))

(defun recolor-record (record ink)
  (labels ((recolor-node (node) 
	     (when (clim:displayed-output-record-p node)
	       (setf (clim:displayed-output-record-ink node) ink))
	     (clim:map-over-output-records #'recolor-node node)))
    (recolor-node record)))

(defun frame-cursor (frame &optional (cursor :default))
  
  (cond
   ((clim:application-frame-p frame)
    (let* ((sheet (clim:frame-top-level-sheet frame))
           (port (clim:find-port))
           (pointer (and port (clim:port-pointer port))))
      #-:null
      (declare (ignore pointer))
      (when (clim:sheetp sheet)
        (frame-cursor sheet cursor))
      #+:null
      (when pointer
        (setf (clim:pointer-cursor pointer) cursor))))
   ((clim:sheetp frame)
    (setf (clim:sheet-pointer-cursor frame) cursor)
    #+(and :allegro (not :mswindows))
    (loop
        for child in (clim:sheet-children frame)
        ;;
        ;; _fix_me_
        ;; although the CLIM 2.2 User Guide claims that all sheets (can) have
        ;; children, sheet-children() actually throws an exeption on some of
        ;; the native Motif widgets; this calls for a bug report to Franz.
        ;;                                                    (16-oct-02; oe)
        unless (or (eq (class-of child) 
                       (find-class 'tk-silica::motif-menu-bar))
                   (eq (class-of child) 
                       (find-class 'tk-silica::motif-scroll-bar)))
        do (frame-cursor child cursor)))))

(defun reset-discriminants (frame)
  (setf (compare-frame-trees frame) (compare-frame-otrees frame))
  (setf (compare-frame-in frame) (compare-frame-edges frame))
  (setf (compare-frame-out frame) nil)
  (loop
      for foo in (compare-frame-discriminants frame)
      do
        (setf (discriminant-time foo) (get-universal-time))
        (setf (discriminant-state foo) :unknown)
        (setf (discriminant-toggle foo) :unknown)
        ;;
        ;; loose all memory of preset values at this point; no way back.
        ;;
        (setf (discriminant-preset foo) nil)
        (setf (discriminant-gold foo) nil))
  (recompute-in-and-out frame t)
  (update-trees frame))

(defun recompute-in-and-out (frame &optional resetp)
  ;;
  ;; apply inference rules from [Carter, 1997] until a fixpoint is reached
  ;; _fix_me_
  ;; now that some of the other parts are reasonably efficient, we can look at
  ;; sets of a thousand or more discriminants; propagation of discriminants 
  ;; becomes noticeably sluggish with more than a few hundred.
  ;;                                                          (5-nov-02; oe)
  (let ((threshold *tree-display-threshold*)
        (initial (length (compare-frame-in frame)))
        (decision (first (compare-frame-decisions frame))))
    (cond
     ((and (decision-p decision) (eq (decision-type decision) :reject))
      (setf (compare-frame-in frame) nil)
      (setf (compare-frame-out frame) (compare-frame-edges frame))
      ;;
      ;; always needs tree redraw, unless active set was empty before.
      ;;
      (not (zerop initial)))
     ((and (decision-p decision) (eq (decision-type decision) :select))
      (let ((active (decision-value decision)))
        (setf (compare-frame-in frame) (list active))
        (setf (compare-frame-out frame)
          (remove active (compare-frame-edges frame))))
      t)
     (t
      (let ((donep nil))
        (setf (compare-frame-in frame) (compare-frame-edges frame))
        (setf (compare-frame-out frame) nil)
        (when resetp
          ;;
          ;; we risk loosing preset (gold) assignments here; maybe check for a
          ;; preset value and restore that here?              (12-oct-02; oe)
          ;;
          ;; _fix_me_
          ;; give more thought to propagation of decisions through updates: a
          ;; hard decision (i.e. toggle) may be lost during an upddate and only
          ;; survive as a soft decision (i.e. entailed state).  over time, this
          ;; could lead to primarily soft annotations, and the distinction from
          ;; the original annotation cycle will be completely washed out.  one
          ;; option could be to promote entailments from `gold' to hard when
          ;; performing the update, but that appears to blur the distinction
          ;; between actual decisions and entailment just as much.  maybe have
          ;; additional values for toggle (and presumably state): inherited on
          ;; or off, or even hard inherited vs. soft inherited?
          ;;                                                  (14-oct-02; oe)
          ;;
          (loop
              for foo in (compare-frame-discriminants frame)
              for preset = (discriminant-preset foo)
              for gold = (discriminant-gold foo)
              do
                (setf (discriminant-state foo) (discriminant-toggle foo))
              when gold do
                (setf (discriminant-state foo) (discriminant-state gold))
              when preset do
                (setf (discriminant-state foo) (discriminant-state preset))))
        (loop 
            until donep
            do
              (setf donep t)
              (loop
                  for foo in (compare-frame-discriminants frame)
                  when (null (discriminant-state foo)) do
                    (mark-out (discriminant-in foo) frame)
                  when (eq (discriminant-state foo) t) do
                    (mark-out (discriminant-out foo) frame))
              (setf (compare-frame-in frame) 
                (set-difference 
                 (compare-frame-edges frame) (compare-frame-out frame)
                 :test #'eq))
              (loop
                  for foo in (compare-frame-discriminants frame)
                  when (null (intersection 
                              (discriminant-in foo) (compare-frame-in frame)
                              :test #'eq)) do
                    (when (discriminant-state foo)
                      (setf (discriminant-state foo) nil)
                      (setf donep nil))
                  else when (subsetp 
                             (compare-frame-in frame) (discriminant-in foo)
                             :test #'eq) do
                    (when (not (discriminant-state foo))
                      (setf (discriminant-state foo) t)
                      (setf donep nil)))))
      
      (and (integerp threshold) 
           (or (and (> initial threshold) 
                    (<= (length (compare-frame-in frame)) threshold))
               (and (<= initial threshold)
                    (> (length (compare-frame-in frame)) threshold))))))))
	
(defun mark-out (edges frame)
  (loop
      for edge in edges
      do 
        (pushnew edge (compare-frame-out frame) :test #'eq)
        (setf (compare-frame-in frame) 
          (remove edge (compare-frame-in frame)))))

(defun update-match-p (frame)
  (when (and *tree-automatic-update-p* *tree-update-match-hook*)
    (let ((hook (typecase *tree-update-match-hook*
                  (null nil)
                  (function *tree-update-match-hook*)
                  (symbol (and (fboundp *tree-update-match-hook*)
                               (symbol-function *tree-update-match-hook*)))
                  (string (ignore-errors 
                           (symbol-function 
                            (read-from-string *tree-update-match-hook*)))))))
      (when hook (ignore-errors (funcall hook frame))))))

;;;
;;; from here on, HTML output routines; this should probably be reorganized, so
;;; that the comparison class is generic, and only the CLIM-specific parts are
;;; in the segregated directory.                                (6-aug-03; oe)
;;;
(defun html-compare (frame &key (stream t) (indentation 0))

  #+:debug
  (setf %frame% frame)

  (let ((treep (or (not (integerp *tree-display-threshold*))
                   (<= (length (compare-frame-trees frame)) 
                       *tree-display-threshold*))))

    (format
     stream
     "~v,0t<table class=compare>~%~
      ~v,0t  <tr>~%~v,0t    <td class=compareTop colspan=2 align=center>~%~
      ~v,0t      ~:[~*~;(~a)  ~]~a [~a : ~a~@[ @ ~a~]]</td></tr>~%~
      ~v,0t  <tr>~%"
     indentation
     indentation indentation
     indentation
     (compare-frame-item frame) (compare-frame-item frame)
     (compare-frame-input frame)
     (length (compare-frame-in frame)) 
     (length (compare-frame-out frame))
     (let ((foo (compare-frame-confidence frame)))
       (when (and (integerp foo) (>= foo 0) (<= foo 3))
         (aref #("zero" "low" "fair" "high") foo)))
     indentation)
    (when treep
      (format
       stream
       "~v,0t    <td class=compareTrees align=left valign=top>~%"
       indentation)
      (html-trees frame :stream stream :indentation (+ indentation 6))
      (format stream "~%~v,0t    </td>~%" indentation))
    (let ((mrsp (when (and *tree-display-semantics-p*
                           (compare-frame-trees frame)
                           (null (rest (compare-frame-trees frame))))
                  (let ((edge (ctree-edge 
                               (first (compare-frame-trees frame)))))
                    (or (edge-mrs edge)
                        (when (edge-dag edge) (mrs::extract-mrs edge)))))))
      (format
       stream
       "~v,0t    <td class=compare~:[Discriminants~;Mrs~] ~
                     align=right valign=top>~%"
       indentation mrsp)
      (if (not mrsp)
        (html-discriminants 
         frame :stream stream :onlyp (not treep) 
         :indentation (+ indentation 6))
        #+:mrs
        (mrs::output-mrs1 mrsp 'mrs::html stream))))
  (format
   stream
   "~v,0t    </td>~%~v,0t  </tr>~%~v,0t</table>"
   indentation indentation indentation))

(defun html-trees (frame &key (stream t) (indentation 0))

  (unless (and (integerp *tree-display-threshold*)
               (> (length (compare-frame-trees frame)) 
                  *tree-display-threshold*))
    (loop
        initially
          (format 
           stream
           "~v,0t<table class=compareTrees>~%" indentation)
        finally
          (format stream "~v,0t</table>" indentation)
        with in = (compare-frame-in frame)
        for tree in (compare-frame-trees frame)
        for color = (unless (member (ctree-edge tree) in :test #'eq) "red")
        do
          (format 
           stream 
           "~v,0t  <tr>~%~v,0t    ~
            <td class=compareTreeLabel valign=top>~%~v,0t      ~
            <table class=compareTreeLabel>~%~v,0t        ~
            <tr><td class=compareTreeId>[~a]</td></tr>~%~v,0t        ~
            <tr><td class=compareTreeScore>~:[~*&nbsp;~;~a~]</td>~
            </tr>~%~v,0t      </table></td>~%~v,0t    ~
            <td class=compareTree>~%"
           indentation indentation
           indentation
           indentation
           (ctree-id tree) indentation
           (ctree-score tree) (ctree-score tree)
           indentation indentation)
          (html-tree 
           (ctree-edge tree) :tree (ctree-symbol tree)
           :indentation (+ indentation 6) :color color :stream stream)
          (format
           stream
           "~%~v,0t    </td></tr>~%" indentation))))

(defun html-discriminants (frame &key (stream t) onlyp (indentation 0))

  (let ((discriminants (compare-frame-discriminants frame)))
    (format 
     stream 
     "~v,0t<table class=compareDiscriminants~@[Only~] cellpadding=2>~%" 
     indentation onlyp)
    (loop
        for i from 0
        for item in discriminants
        for state = (discriminant-state-as-string item)
        for toggle = (discriminant-toggle item)
        unless (discriminant-hidep item) do
          (format
           stream
           "~v,0t  <tr>~%~v,0t    ~
            ~:[<td class=compareDiscriminantState>~a</td>~%~v,0t      ~;~2*~]~
            <td class=compareDiscriminantToggle>~%~v,0t      ~
            <select size=1 name=\"~a\"~%~v,0t              ~
                    onChange=\"this.form.submit()\">~%~v,0t        ~
            <option value=\"?\"~@[ selected~*~]>?</option>~%~v,0t        ~
            <option value=\"+\"~@[ selected~*~]>+</option>~%~v,0t        ~
            <option value=\"-\"~@[ selected~*~]>-</option>~%~v,0t        ~
            </select></td>~%~v,0t      ~
            <td class=compareDiscriminantKey>~a</td>~%~v,0t    ~
            ~@[<td class=compareDiscriminantValue>~a</td>~]</tr>~%"
           indentation indentation
           (eq (compare-frame-display frame) :concise) state indentation
           indentation
           i indentation
           indentation
           (eq toggle :unknown) indentation
           (eq toggle t) indentation
           (null toggle) indentation
           indentation
           (discriminant-key item) indentation
           (discriminant-value item) indentation))
    (format stream "~v,0t</table>" indentation)))

(defun ptb-compare-hook (frame &key size)
  (declare (ignore extras))
  (let* ((input (compare-frame-input frame))
         (input (if (stringp input)
                  (let ((symbol (find-symbol "READ-PTB-FROM-STRING" :tsdb)))
                    (when symbol (funcall (symbol-function symbol) input))
                    input)))
         (comment (compare-frame-comment frame)))
    (when (and (consp input) (stringp comment))
      (setf (compare-frame-input frame) comment)
      (lui-display-tree input comment :size size))
    nil))

#+:null
(setf lkb::*tree-initialization-hook* '("lkb::ptb-compare-hook" :size 8))
