;;;
;;; Tools to help build a tree bank
;;;

(in-package :lkb)

(def-lkb-parameter *preference-file* "~/grammar/parses.txt")


;;; **********************************************************************
;;; Main entry points

(defun batch-compare (filename)
  (mp:run-function "Batch" #'batch-compare-really filename))

(defun batch-compare-really (filename)
  (let ((frame (clim:make-application-frame 'compare-frame)))
    (setf (compare-frame-stream frame) (open filename :direction :input))
    (setf (clim:frame-pretty-name frame) "Batch compare")
    (when (next-sentence frame)
      (clim:run-frame-top-level frame))))

(defun next-sentence (frame)
  (let ((*parse-record*)
	(success))
    (loop 
	do (setq success (parse-next-sentence frame))
	until (and success
		   (car *parse-record*)))
    (when success
      (set-up-compare-frame *parse-record* frame)
      t)))

(defun parse-next-sentence (frame)
  (let ((line (when (compare-frame-stream frame)
		(read-line (compare-frame-stream frame) nil nil))))
    (when line
      (incf (compare-frame-item frame))
      (parse
       (split-into-words 
	(preprocess-sentence-string 
	 (string-trim '(#\space #\tab #\newline) line)))
       nil))))

(defun compare-parses nil
  (when *parse-record*
    (compare *parse-record*)))

(defun compare (parses)
  (let ((frame (clim:make-application-frame 'compare-frame)))
    (setf (compare-frame-current-chart frame) 
          *chart-generation-counter*)
    (set-up-compare-frame parses frame)
    (setf (clim:frame-pretty-name frame) 
      (format nil "~a" (edge-leaves (car parses))))
    (mp:run-function "Compare" #'clim:run-frame-top-level frame)))

(defun set-up-compare-frame (parses frame)
  (setf (compare-frame-decisions frame) (list (make-decision :type :start)))
  (setf (compare-frame-trees frame)
    (mapcar #'(lambda (p) 
		(make-ptree :top (make-new-parse-tree p 1)))
	    parses))
  (setf (compare-frame-discrs frame) (find-discriminants frame))
  (recompute-in-and-out frame t))

;;; **********************************************************************
;;; Collect differences among a set of parses

(defstruct discr 
  key value start end in out toggle state type time record)

(defvar *discrs*)

(defun find-discriminants (frame)
  (let ((*discrs* nil)
	(parses (compare-frame-trees frame)))
    ;; Collect all discriminants
    (dolist (parse parses)
      (find-discriminants-in-parse frame (ptree-top parse) (ptree-top parse)))
    ;; Filter out discriminants that are implied by other longer discriminants
    (setq *discrs*
      (delete-if #'(lambda (x) 
		     (some #'(lambda (y)
			       (and (equal (discr-in x)
					   (discr-in y))
				    (eq (discr-type x) :constituent)
				    (eq (discr-type y) :constituent)
				    (> (length (discr-value y))
				       (length (discr-value x)))))
			   *discrs*))
		 *discrs*))
    ;; Filter out discriminants that don't rule out any parses
    (setq *discrs*
      (delete-if #'(lambda (x) (= (length (discr-in x))
				  (length parses)))
		 *discrs*))
    ;; Compute out from in
    (dolist (x *discrs*)
      (setf (discr-out x) (set-difference (mapcar #'ptree-top parses)
					  (discr-in x))))
    ;; Sort in order of yield, with semantic discriminants at end
    (setf *discrs*
      (sort *discrs* 
            #'(lambda (a b)
                (let* ((astart (discr-start a))
                       (aend (discr-end a))
                       (aspan (when (and (numberp astart) (numberp aend))
                                (- aend astart)))
                       (bstart (discr-start b))
                       (bend (discr-end b))
                       (bspan (when (and (numberp bstart) (numberp bend))
                                (- bend bstart))))
                  (if (and aspan bspan)
                    (if (= aspan bspan)
                      (if (= astart bstart)
                        (or (and (eq (discr-type a) :constituent)
                                 (or (eq (discr-type b) :type)
                                     (eq (discr-type b) :rel)))
                            (and (eq (discr-type a) :type)
                                 (eq (discr-type b) :rel)))
                        (< astart bstart))
                      (> aspan bspan))
                    (> (if (stringp (discr-value a))
                         (length (discr-value a))
                         0)
                       (if (stringp (discr-value b))
                         (length (discr-value b))
                         0)))))))))

(defun find-discriminants-in-parse (frame parse top)
  (let ((fs (get parse 'edge-fs))
	(record (get parse 'edge-record))
	(daughters (get parse 'daughters)))
    (cond (record
	   (let* ((leaves (edge-leaves record))
		  (label (find-category-abb fs))
		  (item (edge-rule record))
		  (rule (if (rule-p item) (rule-id item) item))
		  (yield (apply #'concatenate 
				`(string
				  ,@(mapcan #'(lambda (x) 
						(list x " ")) 
					    leaves))))
                  (start (edge-from record))
                  (end (edge-to record)))
	     (add-discriminant frame label yield :constituent top start end)
	     (if (stringp rule)
               (progn
                 (add-discriminant frame
                                   (format 
                                    nil "~(~a~)"
                                    (type-of-fs (tdfs-indef fs))) 
                                   rule :type top start end)
                 (add-discriminant frame
                                   (format 
                                    nil "~(~a~)"
                                    (type-of-fs (existing-dag-at-end-of 
                                                 (tdfs-indef fs)
                                                *discriminant-path*)))
                                   rule :rel top start end))
	       (add-discriminant frame
                                 (symbol-name rule) yield 
                                 :constituent top start end))))
	  (fs
           (break)
	   (add-discriminant frame (symbol-name parse) 
			     (type-of-fs (tdfs-indef fs))
			     :type
			     top 0 0)
	   (add-discriminant frame (symbol-name parse) 
			     (type-of-fs (existing-dag-at-end-of 
					  (tdfs-indef fs)
                                          *discriminant-path*))
			     :rel top 0 0)))
    (dolist (child daughters)
      (find-discriminants-in-parse frame child top))))

(defun add-discriminant (frame key value type top start end)
  (let ((old
         (loop
             for discriminant in *discrs* 
             thereis (and (equal (discr-key discriminant) key)
                          (equal (discr-value discriminant) value)
                          (equal (discr-type discriminant) type)
                          (equal (discr-start discriminant) start)
                          (equal (discr-end discriminant) end)
                          discriminant))))
    (if old
      (pushnew top (discr-in old))
      (let ((preset (loop
                        for discriminant in (compare-frame-preset frame)
                        thereis (and (eql (discr-start discriminant) start)
                                     (eql (discr-end discriminant) end)
                                     (eq (discr-type discriminant) type)
                                     (string-equal 
                                      (discr-key discriminant) key)
                                     discriminant))))
        (push (make-discr :key key :value value
                          :in (list top) :out nil
                          :type type
                          :start start :end end
                          :toggle (if preset (discr-toggle preset) :unknown)
                          :state (if preset (discr-state preset) :unknown))
              *discrs*)))))

(defstruct decision
  type
  value
  (time (get-universal-time)))

(defun record-decision (decision &optional frame)
  (if frame
    (let ((value (decision-value decision)))
      (when (discr-p value) (setf (discr-time value) (decision-time decision)))
      (push decision (compare-frame-decisions frame)))
    (clim:with-application-frame (frame)
      (record-decision decision frame))))
  
;;; **********************************************************************
;;; Application frame for comparison window

(clim:define-application-frame compare-frame ()
  ((discrs :initform nil
	   :accessor compare-frame-discrs)
   (preset :initform nil :accessor compare-frame-preset)
   (decisions :initform nil :accessor compare-frame-decisions)
   (trees :initform nil
	  :accessor compare-frame-trees)
   (in :initform nil
       :accessor compare-frame-in-parses)
   (out :initform nil
	:accessor compare-frame-out-parses)
   (trees-stream :initform nil
		 :accessor compare-frame-trees-stream)
   (item :initform 0
	 :accessor compare-frame-item)
   (stream :initform nil
	   :accessor compare-frame-stream)
   (current-chart :initform nil
                  :accessor  compare-frame-current-chart)
   (version :initform nil :accessor compare-frame-version)
   (controller :initform nil :accessor compare-frame-controller))
  (:panes
   (trees  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-trees-window
			  :text-cursor nil
			  :width 450
			  ;; :height 100
			  :text-style (clim:parse-text-style 
				       (list :sans-serif :roman 7))
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  ;; :incremental-redisplay '(t :check-overlapping nil)
			  :display-time nil
			  :background clim:+white+
			  :foreground clim:+black+)))))
   (display  
    (clim:outlining (:thickness 1)
      (clim:spacing (:thickness 1)  
	(clim:scrolling (:scroll-bars :both)
	  (clim:make-pane 'clim:application-pane
			  :display-function 'draw-compare-window
			  :text-cursor nil
			  :width 450
			  :height 650
			  :text-style (clim:parse-text-style 
				       (list :sans-serif :roman 10))
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :borders nil
			  :incremental-redisplay t
			  :background clim:+white+
			  :foreground clim:+black+))))))
  (:layouts
   (:default (clim:horizontally () trees display))))


(define-compare-frame-command (com-exit-compare-frame :menu "Close")
    ()
 (clim:with-application-frame (frame)
   (record-decision (make-decision :type :close) frame)
   (when (compare-frame-stream frame)
     (close (compare-frame-stream frame)))
   (if (compare-frame-controller frame)
     (mp:process-revoke-arrest-reason 
      (compare-frame-controller frame) :wait)
     (clim:frame-exit frame))))

(define-compare-frame-command (com-save-compare-frame :menu "Save")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :save))
    (if (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait)
      (loop
          for tree in (compare-frame-in-parses frame)
          do
            (write-record 
             (edge-leaves 
              (get (ptree-top (car (compare-frame-trees frame))) 'edge-record))
             (type-tree tree))))))


(define-compare-frame-command (com-first-compare-frame :menu "First")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :first) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))
      
(define-compare-frame-command (com-pervious-compare-frame :menu "Previous")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :previous) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-next-compare-frame :menu "Next")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :next) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-last-compare-frame :menu "Last")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :last) frame)
    (when (compare-frame-controller frame)
      (mp:process-revoke-arrest-reason 
       (compare-frame-controller frame) :wait))))

(define-compare-frame-command (com-clear-compare-frame :menu "Clear")
    ()
  (clim:with-application-frame (frame)
    (record-decision (make-decision :type :clear) frame)
    (setf (compare-frame-in-parses frame) (compare-frame-trees frame))
    (setf (compare-frame-out-parses frame) nil)
    (dolist (d (compare-frame-discrs frame))
      (setf (discr-time d) (get-universal-time))
      (setf (discr-state d) :unknown)
      (setf (discr-toggle d) :unknown))
    (recompute-in-and-out frame)
    (update-trees frame))) 

(defun type-tree (tree)
  (let ((edge-record (get tree 'edge-record)))
    (cons (if edge-record
	      (if (rule-p (edge-rule edge-record))
		  (rule-id (edge-rule edge-record))
		(edge-category edge-record))
	    (symbol-name tree))
	  (mapcar #'type-tree (get tree 'daughters)))))

(defun write-record (sentence result)
  (let ((timestamp 
	 (multiple-value-bind (sec min hour date month year)
	     (get-decoded-time)
	   (format nil "~a-~a-~a ~a:~a:~a" year month date hour min sec)))
	(*print-pretty* nil))
    (with-open-file (stream *preference-file* 
		     :direction :output :if-exists :append
		     :if-does-not-exist :create)
      (write (list timestamp 
                   #+(and :allegro-version>= (version>= 5 0))
                   (sys:user-name)
                   #-(and :allegro-version>= (version>= 5 0))
                   (system:getenv "USER")
                   sentence result)
	     :stream stream :level nil :length nil)
      (write-char #\lf stream))))


;;; **********************************************************************
;;; Stuff for mini tree pane

(defstruct ptree 
  ;; Top node of parse tree
  top					
  ;; Output record of tree
  output-record				
  ;; Current color of tree
  ink)
  
(defun draw-trees-window (window stream)
  (setf (compare-frame-trees-stream window) stream)
  (dolist (tree (compare-frame-trees window))
    (setf (ptree-ink tree) clim:+foreground-ink+)
    (setf (ptree-output-record tree)
      (clim:with-new-output-record (stream)
	(clim:with-output-recording-options (stream :record t)
	  (clim:with-output-as-presentation 
	      (stream tree 'ptree :single-box t)
	    (clim:format-graph-from-root
	     (ptree-top tree)
	     #'(lambda (node stream)
		 (multiple-value-bind (s bold-p) 
		     (get-string-for-edge node)
		   (clim:with-text-face (stream (if bold-p :bold :roman))
		     (write-string s stream))))
	     #'(lambda (node) (get node 'daughters))
	     :graph-type :parse-tree
	     :stream stream 
	     :merge-duplicates nil
	     :orientation :vertical
	     :generation-separation 7
	     :move-cursor t
	     :within-generation-separation 7
	     :center-nodes nil)))
	(terpri stream))))
  (update-trees window))

(defun propagate-discriminants (frame top)
  (loop
      for discriminant in (compare-frame-discrs frame)
      when (member top (discr-in discriminant) :test #'eq)
      do
        (when (null (discr-toggle discriminant))
          (setf (discr-toggle discriminant) :unknown)
          (setf (discr-time discriminant) (get-universal-time)))
        (setf (discr-state discriminant) t)
      when (member top (discr-out discriminant) :test #'eq)
      do
        (when (eq (discr-toggle discriminant) t)
          (setf (discr-toggle discriminant) :unknown)
          (setf (discr-time discriminant) (get-universal-time)))
        (setf (discr-state discriminant) nil))
  (recompute-in-and-out frame))

(define-compare-frame-command (com-tree-menu)
    ((tree 'ptree :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Yes" :value yes :active t)
                    ("No" :value no :active t)
		    ("Show tree" :value show)))))
    (when command
      (handler-case
	  (ecase command
	    (yes 
             (record-decision 
              (make-decision :type :select :value (ptree-top tree)))
	     (clim:with-application-frame (frame)
               ;;
               ;; _fix_me_
               ;; determine proper way of propagating discriminants: toggle
               ;; all discriminants for this tree on, overwrite negative toggle
               ;; where necessary, then set status of all others to negative.
               ;;
               (propagate-discriminants frame (ptree-top tree))
	       (update-trees frame)))
	    (show 
             (clim:with-application-frame (frame)
               (draw-new-parse-tree (ptree-top tree)
                                    "Parse tree" nil
                                    (compare-frame-current-chart frame)))))
        (storage-condition (condition)
          (with-output-to-top ()
            (format t "~%Memory allocation problem: ~A~%" condition)))
	(error (condition)
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))
        (serious-condition (condition)
          (with-output-to-top ()
            (format t "~%Something nasty: ~A~%" condition)))))))


(defun update-trees (window)
  (let (; (done-p nil)
	(stream (compare-frame-trees-stream window)))
    (dolist (tree (compare-frame-trees window))
      (let ((ink (cond ((member (ptree-top tree) 
				(compare-frame-out-parses window)
				:test #'eq)
			clim:+red+)
		       ((and (not (cdr (compare-frame-in-parses window)))
			     (eq (car (compare-frame-in-parses window))
				 (ptree-top tree)))
			clim:+green+)
		       (t clim:+foreground-ink+))))
;	(when (eq ink clim:+green+)
;	  (setq done-p t))
	(unless (eq ink (ptree-ink tree))
	  (setf (ptree-ink tree) ink)
	  (recolor-tree (ptree-output-record tree) ink)
	  (clim:replay (ptree-output-record tree) stream))))))
;;    (setf (clim:command-enabled 'com-done-compare-frame window) done-p)))

(defun recolor-tree (record ink)
  (labels ((recolor-node (node) 
	     (when (clim:displayed-output-record-p node)
	       (setf (clim:displayed-output-record-ink node) ink))
	     (clim:map-over-output-records #'recolor-node node)))
    (declare (dynamic-extent recolor-node))
    (recolor-node record)))
  
  
;;; **********************************************************************
;;; Stuff for constituent pane

(defun draw-compare-window (window stream)
  (let ((discrs (compare-frame-discrs window)))
    (clim:updating-output (stream :cache-value t)
      (format stream "[item # ~a] `~{~a~^ ~}'~%~%" 
	      (compare-frame-item window)
	      (edge-leaves (get (ptree-top (car (compare-frame-trees window)))
				'edge-record))))
    (clim:updating-output (stream) 
      (format stream "~a parse~:p in, ~a parse~:p out~@[  ~a~]~%~%" 
	      (length (compare-frame-in-parses window))
	      (length (compare-frame-out-parses window))
              (compare-frame-version window)))
    (clim:formatting-table (stream :x-spacing "X")
      (dolist (d discrs)
	(clim:formatting-row (stream)
          (setf (discr-record d)
            (clim:with-new-output-record (stream)
              (clim:with-output-recording-options (stream :record t)
                (clim:with-output-as-presentation
                    (stream d 'discr)
                  #+:null
                  (clim:formatting-cell (stream :align-x :center)
                    (format stream "~a" (discr-start d)))
                  #+:null
                  (clim:formatting-cell (stream :align-x :center)
                    (format stream "~a" (discr-end d)))
                  (clim:updating-output (stream :cache-value (discr-state d))
                    (clim:formatting-cell (stream :align-x :center)
                      (write-string (cond ((eq (discr-state d) t) "+")
                                          ((null (discr-state d)) "-")
                                          (t "?"))
                                    stream)))
                  (clim:updating-output (stream :cache-value (discr-toggle d))
                    (clim:formatting-cell (stream :align-x :center)
                      (write-string (cond ((eq (discr-toggle d) t) "+")
                                      ((null (discr-toggle d)) "-")
                                      (t "?"))
                                    stream)))
                  (clim:formatting-cell (stream :align-x :left)
                    (format stream "~A" (discr-key d)))
                  (clim:formatting-cell (stream :align-x :left)
                    (format stream "~A" (discr-value d))))))))))))


(define-compare-frame-command (com-discr-menu)
    ((discr 'discr :gesture :select))
  (let ((command (clim:menu-choose
		  '(("Yes" :value yes)
		    ("No" :value no)
		    ("Unknown" :value unknown)
		    ("Show in parses" :value in)
		    ("Show out parses" :value out)))))
    (when command
      (handler-case
	  (ecase command
	    (yes 
             (record-decision (make-decision :type :yes :value discr))
             (setf (discr-state discr) t)
             (setf (discr-toggle discr) t))
	    (no 
             (record-decision (make-decision :type :no :value discr))
             (setf (discr-state discr) nil)
             (setf (discr-toggle discr) nil))
	    (unknown 
             (record-decision 
              (make-decision :type :unknown :value discr))
             (setf (discr-state discr) :unknown)
             (setf (discr-toggle discr) :unknown))
	    (in (dolist (p (discr-in discr))
		  (draw-new-parse-tree p (format nil "In parse (~A)" 
						 (discr-key discr))
				       nil)))
	    (out (dolist (p (discr-out discr))
		   (draw-new-parse-tree p (format nil "Out parse (~A)" 
						  (discr-key discr))
					nil))))
	(error (condition) 
	  (declare (ignore condition) )
		   nil))
      (recompute-in-and-out clim:*application-frame* t)
      (update-trees clim:*application-frame*))))

;; Apply inference rules from Carter (1997) until nothing changes

(defun recompute-in-and-out (frame &optional resetp)
  (setf (compare-frame-in-parses frame) (mapcar #'ptree-top 
						(compare-frame-trees frame)))
  (setf (compare-frame-out-parses frame) nil)
  (when resetp
    (loop
        for d in (compare-frame-discrs frame)
        do
          (setf (discr-state d) (discr-toggle d))))
  (let ((done-p nil))
    (loop until done-p
	do
	  (setq done-p t)
	  (dolist (d (compare-frame-discrs frame))
	    (cond (;; R1
		   (null (discr-state d))
		   (mark-out (discr-in d) frame))
		  (;; R2
		   (eq (discr-state d) t)
		   (mark-out (discr-out d) frame))))
	  (setf (compare-frame-in-parses frame) 
	    (set-difference (mapcar #'ptree-top (compare-frame-trees frame))
			    (compare-frame-out-parses frame)
			    :test #'eq))
	  (dolist (d (compare-frame-discrs frame))
	    (cond (;; R3
		   (null (intersection (discr-in d) 
				       (compare-frame-in-parses frame)
				       :test #'eq))
		   (when (discr-state d)
		     (setf (discr-state d) nil)
		     (setq done-p nil)))
		  (;; R4
		   (subsetp (compare-frame-in-parses frame) 
			    (discr-in d)
			    :test #'eq)
		   (when (not (discr-state d))
		     (setf (discr-state d) t)
		     (setq done-p nil))))))))
	
(defun mark-out (parses frame)
  (dolist (p parses)
    (pushnew p (compare-frame-out-parses frame) :test #'eq)
    (setf (compare-frame-in-parses frame)
      (remove p (compare-frame-in-parses frame)))))

