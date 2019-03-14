(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2018 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

(defvar %edges%)

(defvar %mrs%)

(defun mrs-transfer-font ()
  (clim:make-text-style :sans-serif :roman 10))

(declaim (notinline mrs-transfer-font))


(clim:define-application-frame mrs-transfer ()
  ((frames :initform nil :accessor mrs-transfer-frames :allocation :class)
   (edges :initform nil :accessor mrs-transfer-edges)
   (i :initform 0 :accessor mrs-transfer-i)
   (stack :initform nil :accessor mrs-transfer-stack)
   (master :initform nil :accessor mrs-transfer-master)
   (title :initform "" :accessor mrs-transfer-title)
   (mode :initform nil :accessor mrs-transfer-mode)
   (target :initform nil :accessor mrs-transfer-target :allocation :class)
   (stream :initform nil :accessor mrs-transfer-stream))
  (:panes
    (mrs-transfer-pane
      (clim:make-pane 'lkb::lkb-pane ; JAC 10-Dec-2018 - was clim:application-pane
        :text-cursor nil
        :end-of-line-action :allow
        :end-of-page-action :allow
        ;; *** :borders nil
        :text-style (mrs-transfer-font)
        :background clim:+white+
        :foreground clim:+black+
        :incremental-redisplay t
        :display-function 'show-mrs-transfer)))
  (:layouts
    (default
      (clim:scrolling (:scroll-bars :both :width 500 :height 600)
        mrs-transfer-pane))))

  
;;; Bookkeeping for creating and closing frames

(defmethod clim:run-frame-top-level :before ((frame mrs-transfer) &key)
  (mp:with-process-lock (lkb::*lkb-frame-lock*)
    (push frame (mrs-transfer-frames frame))))

(defmethod clim:frame-exit :before ((frame mrs-transfer)
                                    #+:allegro &rest #+:allegro keys)
  ;; !!! the &rest argument in Allegro CLIM is undocumented and conflicts with the CLIM 2 spec
  #+:allegro (declare (ignore keys))
  (mp:with-process-lock (lkb::*lkb-frame-lock*)
    ;; if this frame was the last to be created then deregister it
    (when (eq frame lkb::*last-frame*)
      (setq lkb::*last-frame* nil))
    (setf (mrs-transfer-frames frame) 
      (delete frame (mrs-transfer-frames frame)))))

(defmethod initialize-instance :around ((frame mrs-transfer) &rest initargs)
  (if lkb::*manage-window-placement*
    (multiple-value-bind (left top width height)
        (lkb::compute-frame-position-and-size frame)
      (apply #'call-next-method
        frame :left left :top top :width width :height height initargs))
    (call-next-method)))


;;; Frame commands

(define-mrs-transfer-command (com-close-mrs-transfer :menu "Close") 
    ()
  (mp:with-process-lock (lkb::*lkb-frame-lock*)
    (clim:with-application-frame (frame)
     (clim:frame-exit frame))))

(define-mrs-transfer-command (com-close-all-mrs-transfer :menu "Close All") 
    ()
  (mp:with-process-lock (lkb::*lkb-frame-lock*)
    (clim:with-application-frame (frame)
     (let ((frames (mrs-transfer-frames frame)))
       (loop
         for brother in (mrs-transfer-frames frame)
         unless (eq brother frame) do
           (clim:execute-frame-command brother '(com-close-mrs-transfer)))
       ;; short delay so front window close does not overtake ones beneath (if any)
       (when (cdr frames) (sleep 0.2))
       (clim:execute-frame-command frame '(com-close-mrs-transfer))))))


(define-mrs-transfer-command (com-previous-mrs-transfer :menu "Previous")
    ()
  (clim:with-application-frame (frame)
    (when (> (mrs-transfer-i frame) 0)
      (decf (mrs-transfer-i frame))
      (setf (clim:frame-pretty-name frame) (transfer-title frame))
      (clim::redisplay-frame-panes frame :force-p t))))
      

(define-mrs-transfer-command (com-next-mrs-transfer :menu "Next")
    ()
  (clim:with-application-frame (frame)
    (when (< (mrs-transfer-i frame) 
             (- (length (or (mrs-transfer-stack frame) 
                            (mrs-transfer-edges frame))) 1))
      (incf (mrs-transfer-i frame))
      (setf (clim:frame-pretty-name frame) (transfer-title frame))
      (clim::frame-replay frame (mrs-transfer-stream frame))
      (clim::redisplay-frame-panes frame :force-p t))))


(define-mrs-transfer-command (com-transfer-mrs-compare :menu "Compare")
    ()
  (clim:with-application-frame (frame)
    (when (mrs-transfer-edges frame)
      (loop
          for i from 0
          for edge in (mrs-transfer-edges frame)
          for mrs = (edge-mrs edge)
          collect (lkb::make-edge :id i :mrs mrs :foo i) 
          into edges
          finally
            (lkb::compare-parses
             edges :input (mrs-transfer-title frame)
             :mode :modern :view :modern)))))


#+:tsdb
(define-mrs-transfer-command (com-transfer-mrs-parse :menu "Parse")
    ()
  (parse-interactively))


(define-mrs-transfer-command (com-transfer-mrs-transfer :menu "Transfer")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame))))
           (mrs (edge-mrs edge))
           (edges (transfer-mrs mrs :filter nil :block nil)))
      (when edges (browse-mrss edges "Transfer Output")))))


(define-mrs-transfer-command (com-generate-mrs-transfer :menu "Generate")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame))))
           (mrs (edge-mrs edge))
           (*package* (find-package :lkb)))
      (loop
          for target in (rest lkb::*translate-grid*)
          for file = (merge-pathnames
                      (lkb::lkb-tmp-dir)
                      (format
                       nil
                       ".transfer.~a.~(~a~)"
                       (lkb::current-user) target))
          do
            (with-open-file (stream file :direction :output
                             :if-exists :supersede)
              (mrs::output-mrs1 mrs 'mrs::simple stream))))))


(define-mrs-transfer-command (com-transfer-mrs-debug :name "Debug" :menu t)
    ()
  (clim:with-application-frame (frame)
    (lkb::pop-up-menu
      '(("Test" :value :test)
        ("Scope" :value :scope)
        ("UTool" :value :utool)
        ("Simple" :value :mrs)
        ("Indexed" :value :indexed)
        ("Dependencies" :value :dependencies)
        ("Fragment" :value :fragment)
        ("Sort" :value :sort)
        ("Step" :value :step)
        #+:allegro
        ("Clone" :value :clone)
        ("Save" :value :save)
        #+:allegro
        ("Edit" :value :edit)
        ("Read" :value :read)
        ("Print" :value :print)
        ("LaTeX" :value :latex)
        ("Rule" :value :rule)
        ("Select" :value :select)
        ("Contrast" :value :contrast)
        #+:null
        ("Apply" :value :apply)
        #+:null
        ("Trace" :value :trace)
        #+:null
        ("Untrace" :value :untrace))
      (:test
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (mrs (mrs::produce-one-scope mrs))
                (title (format nil "~a - Cheap Scope" (transfer-title frame))))
           (browse-mrss (list mrs) title)))

      (:scope
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title (format nil "~a - Scopes" (transfer-title frame))))
           (lkb::show-mrs-scoped-window nil mrs title)))
        
      (:utool
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title (format nil "~a - Scopes" (transfer-title frame))))
           (lkb::show-mrs-utool-window nil mrs title)))

      (:mrs
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title (format nil "~a - MRS" (transfer-title frame))))
           (lkb::show-mrs-window nil mrs title)))

      (:indexed
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title (format nil "~a - Indexed" (transfer-title frame))))
           (lkb::show-mrs-indexed-window nil mrs title)))

      (:dependencies
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title 
                 (format nil "~a - Dependencies" (transfer-title frame))))
           (lkb::show-mrs-dependencies-window nil mrs title)))
        
      (:fragment
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (fragments (discriminate-fragments mrs))
                (title 
                 (format nil "~a [Fragments]" (transfer-title frame))))
           (when fragments
             (browse-mrss fragments title))))

      (:sort
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title 
                 (format nil "~a [sorted]" (transfer-title frame))))
           (when mrs
             (browse-mrss (list (mrs::sort-mrs mrs)) title))))
        
      (:step
         (let* ((edge (nth (mrs-transfer-i frame) 
                           (or (mrs-transfer-stack frame)
                               (mrs-transfer-edges frame)))))
           (browse-mrss edge "Transfer Debug")))

      #+:allegro
      (:clone
         (let ((meta (class-of frame))
               (new (clim:make-application-frame 'mrs-transfer)))
           (loop
               for slot in '(edges i stack title mode)
               do
                 (setf (clos:slot-value-using-class meta new slot)
                   (clos:slot-value-using-class meta frame slot)))
           (setf (mrs-transfer-master new) frame)
           (mp:run-function 
            (format nil "~a [clone]" (mrs-transfer-title new))
            #'(lambda ()
                (setf (clim:frame-pretty-name new) 
                  (or (transfer-title new) "Transfer Input"))
                (clim:run-frame-top-level new))))
         ;;
         ;; _fix_me_
         ;; use class copier instead and invoke run-function() directly on it.
         ;;                                                     (8-jan-04; oe)
         #+:null
         (if (mrs-transfer-stack frame)
           (browse-mrss
            (first (mrs-transfer-edges frame)) (mrs-transfer-title frame)
            :stack (mrs-transfer-stack frame) :i (mrs-transfer-i frame))
           (browse-mrss 
            (mrs-transfer-edges frame) (mrs-transfer-title frame))))

      (:save
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (file 
                 (format nil "/tmp/transfer.debug.~a" (lkb::current-user))))
           (ignore-errors
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (format
               stream
               ";;;~%;;; ~a --- ~a @ ~a~%;;;~%"
               (mrs-transfer-title frame) 
               (lkb::current-user) (lkb::current-time :long :pretty))
              (mrs::output-mrs1 mrs 'mrs::simple stream)
              (format 
               #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
               "~&browse-mrss(): saved current view to `~a'.~%"
               file)))))

      (:edit
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (file 
                 (format nil "/tmp/transfer.debug.~a" (lkb::current-user))))
           (ignore-errors
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (format
               stream
               ";;;~%;;; ~a --- ~a @ ~a~%;;;~%"
               (mrs-transfer-title frame) 
               (lkb::current-user) (lkb::current-time :long :pretty))
              (mrs::output-mrs1 mrs 'mrs::simple stream)
              (format 
               #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
               "~&browse-mrss(): saved current view to `~a'.~%"
               file))
            (when (lep:lep-is-running)
              (lep::eval-in-emacs 
               (format nil "(rogue-find-file \"~a\")" file))))))

      (:read
         (let ((file 
                (format nil "/tmp/transfer.debug.~a" (lkb::current-user))))
           (if (probe-file file)
             (ignore-errors
              (let ((mrs (mrs::read-mrs-from-file file)))
                (when (mrs::psoa-p mrs)
                  #-:debug
                  (setf %mrs% mrs)
                  (browse-mrss (list mrs)))))
             (format
              #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
              "~&browse-mrss(): unable to open `~a'.~%"
              file))))

      (:print
         (print-pane-to-postscript
           frame (clim:find-pane-named frame 'mrs-transfer-pane)))

      (:latex
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (file 
                 (format nil "/tmp/mrs.~a.tex" (lkb::current-user))))
           (ignore-errors
            (with-open-file (stream file
                             :direction :output :if-exists :supersede)
              (format
               stream
               "%~%% ~a --- ~a @ ~a~%%~%"
               (mrs-transfer-title frame) 
               (lkb::current-user) (lkb::current-time :long :pretty))
              (mrs::output-mrs1 mrs 'mrs::latex stream)
              (format 
               #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
               "~&browse-mrss(): (LaTeX) saved current view to `~a'.~%"
               file)))))

      (:rule (interactively-browse-mtr frame))

      (:select
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs)))
           (when (mrs::psoa-p mrs)
             (setf (mrs-transfer-target frame) mrs)
             (setf %mrs% mrs)
             (format
              #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
              "~&browse-mrss(): current view now active as selection.~%"))))

      (:contrast
         (cond
          ((mrs-transfer-target frame)
           (let* ((mrs (nth (mrs-transfer-i frame) 
                            (or (mrs-transfer-stack frame) 
                                (mrs-transfer-edges frame))))
                  (mrs (if (edge-p mrs) (edge-mrs mrs) mrs)))
             (when (mrs::psoa-p mrs)
               (let* ((stream (make-string-output-stream)))
                 (multiple-value-bind (result condition)
                     (ignore-errors
                      (let ((*standard-output* stream))
                        (mrs::mrs-equalp 
                         mrs (mrs-transfer-target frame) nil t nil)))
                   (cond
                    (condition
                      (clim:beep)
                      (format
                       #+:allegro excl:*initial-terminal-io*
                       #-:allegro *terminal-io*
                       "~&browse-mrss(): mrs-equalp() error `~a'.~%"
                       (lkb::normalize-string (format nil "~a" condition))))
                    (t
                     (format
                      #+:allegro excl:*initial-terminal-io*
                      #-:allegro *terminal-io*
                      "~&browse-mrss(): the equivalence comparison was ~
                       ~:[negative~;positive~].~%"
                      result)
                     (unless result
                       (format
                        #+:allegro excl:*initial-terminal-io*
                        #-:allegro *terminal-io*
                        "~%~%~a~%~%"
                        (get-output-stream-string stream))))))))))
          (t
           (clim:beep)
           (format
            #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
            "~&browse-mrss(): no active MRS selection.~%")))))))



(defun show-mrs-transfer (frame stream &rest rest)
  (declare (ignore rest))

  (setf (mrs-transfer-stream frame) stream)
  (let* ((edge (nth (mrs-transfer-i frame) 
                    (or (mrs-transfer-stack frame) 
                        (mrs-transfer-edges frame))))
         (mrs (edge-mrs edge))
         (*package* (find-package :lkb))
         (*print-right-margin* 80))
    (clim:formatting-table (stream)
      (when (edge-rule edge)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left)
            (let ((mrs::*mrs-raw-output-p* nil)
                    (%transfer-raw-output-p% nil))
              (format stream "~a~%" edge))))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left)
            (format stream "~%~%"))))
      (clim:formatting-row (stream)
        (let ((record 
                (clim:formatting-cell (stream :align-x :left)
                  (if mrs
                    (mrs::output-mrs1 mrs 'mrs::simple stream)
                    (format 
                      stream 
                      "~%Invalid MRS Object~%")))))
          (when (and (fragmentp mrs) (not (edge-source edge)))
            (lkb::recolor-record record clim::+blue+))
          (when (and (edge-semi edge) (null (edge-source edge)))
            (lkb::recolor-record record clim:+magenta+))
          (when (edge-source edge)
            (lkb::recolor-record record clim:+red+)))))))

(defun transfer-title (frame)
  (let* ((edge (nth (mrs-transfer-i frame) (mrs-transfer-stack frame)))
         (string
          (case (mrs-transfer-mode frame)
            (:mtr
             (format
              nil 
              "~a @ ~a"
              (mrs-transfer-title frame)
              (aref 
               #("FILTER" "CONTEXT" "INPUT" "OUTPUT" "DEFAULT" "OUTPUT (RAW)")
               (mrs-transfer-i frame))))
            (t
             (format 
              nil 
              "~a (# ~a of ~:[~a~@[+~a~]~;~a~*~])~@[ [~(~a~)]~]" 
              (mrs-transfer-title frame) (mrs-transfer-i frame)
              edge
              (if edge 
                (length (mrs-transfer-stack frame))
                (loop
                    for edge in (mrs-transfer-edges frame)
                    unless (edge-source edge) count 1))
              (unless edge
                (let ((n (loop
                             for edge in (mrs-transfer-edges frame)
                             when (edge-source edge) count 1)))
                  (unless (zerop n) n)))
              (when (and (edge-p edge) (mtr-p (edge-rule edge)))
                (mtr-id (edge-rule edge))))))))
    (if (mrs-transfer-master frame)
      (format nil "~a [clone]" string)
      string)))

(defun browse-mrss (edges 
                    &optional (title "Transfer Input") 
                    &key stack i)

  (unless edges (return-from browse-mrss))
  #-:debug
  (setf %edges% edges)
  (mp:run-function 
   title 
   #'(lambda ()
       (let ((frame (clim:make-application-frame 'mrs-transfer))
             (%transfer-edge-id% 0))
         (setf (mrs-transfer-title frame) title)
         (typecase edges
           (edge
            (setf (mrs-transfer-edges frame) (list edges))
            (if stack
              (setf (mrs-transfer-stack frame) stack)
              (loop
                  for edge = edges then (edge-daughter edge)
                  while edge do (push edge (mrs-transfer-stack frame))
                  finally (setf (mrs-transfer-stack frame)
                            (nreverse (mrs-transfer-stack frame)))))
            (when i (setf (mrs-transfer-i frame) i))
            (setf (mrs-transfer-mode frame) :debug))
           (mtr
            ;;
            ;; _fix_me_
            ;; the OUTPUT vs. DEFAULTS presentation is, hmm, misleading, since
            ;; variables in defaults carry no meaning; ideally, we would merge
            ;; the two components for visualization, or at least `shrink' the
            ;; DEFAULTS part, so that variables without properties disappear.
            ;;                                                 (8-jan-04; oe)
            ;; although we now do the overlay part, shrinking may still be a
            ;; good idea.                                      (9-jan-04; oe)
            ;;
            (setf (mrs-transfer-edges frame) 
              (list
               (make-edge :mrs (mtr-filter edges))
               (make-edge :mrs (mtr-context edges))
               (make-edge :mrs (mtr-input edges))
               #+:null
               (make-edge :mrs (merge-and-copy-mrss 
                                (mtr-output edges) (mtr-defaults edges)))
               #-:null
               (make-edge :mrs (mtr-output edges))
               (make-edge :mrs (mtr-defaults edges))))
            (loop
                for edge in (mrs-transfer-edges frame)
                while (null (edge-mrs edge)) do (incf (mrs-transfer-i frame)))
            (setf (mrs-transfer-title frame)
              (format nil "`~(~a~)' MTR" (mtr-id edges)))
            (setf (mrs-transfer-mode frame) :mtr))
           (list
            (setf (mrs-transfer-edges frame) 
              (loop
                  with *transfer-edge-limit* = nil
                  for edge in edges
                  when (edge-p edge) collect edge
                  else when (mrs::eds-p edge)
                  collect (make-edge :mrs (mrs::eds-to-mrs edge :errorp nil))
                  else collect (make-edge :mrs edge)))))
         (setf (clim:frame-pretty-name frame) 
           (or (transfer-title frame) "Transfer Input"))
         (clim:run-frame-top-level frame)))))

(let (previous)
  (defun interactively-browse-mtr (frame &optional (prompt ""))
    (let* ((rules
            (loop
                for mtrs in *transfer-rule-sets*
                append (loop for mtr in (mtrs-mtrs mtrs) collect mtr)))
           (names
            (sort (loop for rule in rules collect (mtr-id rule)) #'string-lessp)))
      (let ((selection
              (lkb::ask-for-lisp-movable
                "Current Interaction"
                `((,(format nil "~A~%MTR Identifier?" prompt) . ,(or previous (first names))))
                nil names)))
        (when selection
          (let* ((id (first selection))
                 (mtr (find id rules :key #'mtr-id)))
            (unless mtr
              (format
               #+:allegro excl:*initial-terminal-io* #-:allegro *terminal-io*
               "~&interactively-browse-mtr(): `~a' undefined.~%"
               id)
              (return-from interactively-browse-mtr
                (interactively-browse-mtr frame "Not defined - try again.")))
            (browse-mrss mtr)
            (setf previous id)))))))

