(in-package :mt)

(defvar %edges%)

(defvar %mrs%)

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
   (top
    (clim:outlining (:thickness 0)
      (clim:spacing (:thickness 2)  
        (clim:scrolling (:scroll-bars :both)
          (clim:make-pane 
           'clim:application-pane
           :text-cursor nil
           :end-of-line-action :allow
           :end-of-page-action :allow
           :borders nil
           :incremental-redisplay t
           :display-function 'show-mrs-transfer
           :width 450 
           :height 600)))))))

(let ((lock (mp:make-process-lock)))
  
  (define-mrs-transfer-command (com-close-mrs-transfer :menu "Close") 
      ()
    (mp:with-process-lock (lock)
      (clim:with-application-frame (frame)
        (setf (mrs-transfer-frames frame) 
          (delete frame (mrs-transfer-frames frame)))
        (clim:frame-exit frame))))

  (define-mrs-transfer-command (com-close-all-mrs-transfer :menu "Close All") 
      ()
    (mp:with-process-lock (lock)
      (clim:with-application-frame (frame)
        (loop
            for brother in (mrs-transfer-frames frame)
            unless (eq brother frame) do
              (clim:execute-frame-command brother '(com-close-mrs-transfer)))
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


(define-mrs-transfer-command (com-transfer-mrs-transfer :menu "Transfer")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame))))
           (mrs (edge-mrs edge))
           (edges (transfer-mrs mrs :filter nil)))
      (when edges (browse-mrss edges "Transfer Output")))))


(define-mrs-transfer-command (com-generate-mrs-transfer :menu "Generate")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame))))
           (mrs (edge-mrs edge))
           (file (format nil "/tmp/.transfer.~a" (lkb::current-user)))
           (*package* (find-package :lkb)))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (mrs::output-mrs1 mrs 'mrs::simple stream)))))


(define-mrs-transfer-command (com-transfer-mrs-debug :name "Debug" :menu t)
    ()
  (clim:with-application-frame (frame)
    (let ((command (clim:menu-choose
                    '(("Scope" :value :scope :active t)
                      ("Indexed" :value :indexed :active t)
                      ("Dependencies" :value :dependencies :active t)
                      ("Sort" :value :sort :active t)
                      ("Step" :value :step :active t)
                      ("Clone" :value :clone :active t)
                      ("Save" :value :save :active t)
                      ("Read" :value :read :active t)
                      ("Print" :value :print :active t)
                      ("Rule" :value :rule :active t)
                      ("Select" :value :select :active t)
                      ("Compare" :value :compare :active t)
                      #+:null
                      ("Apply" :value :apply :active t)
                      #+:null
                      ("Trace" :value :trace :active t)
                      #+:null
                      ("Untrace" :value :untrace :active t)))))
      (case command

        (:scope
         (let* ((mrs (nth (mrs-transfer-i frame) 
                          (or (mrs-transfer-stack frame) 
                              (mrs-transfer-edges frame))))
                (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
                (title (format nil "~a - Scopes" (transfer-title frame))))
           (lkb::show-mrs-scoped-window nil mrs title)))

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
                (push frame (mrs-transfer-frames new))
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
               excl:*initial-terminal-io*
               "~&browse-mrss(): saved current view to `~a'.~%"
               file)))))

        (:read
         (let ((file 
                (format nil "/tmp/transfer.debug.~a" (lkb::current-user))))
           (if (probe-file file)
             (ignore-errors
              (with-open-file (stream file :direction :input)
                (let ((mrs (mrs::read-mrs-from-file file)))
                  (when (mrs::psoa-p mrs)
                    #-:debug
                    (setf %mrs% mrs)
                    (browse-mrss (list mrs))))))
             (format
              excl:*initial-terminal-io*
              "~&browse-mrss(): unable to open `~a'.~%"
              file))))

        (:print
         (multiple-value-bind (destination orientation scale name)
             (lkb::get-print-options)
           (case destination
             (:printer (format t "~%Direct Printing Not Yet Supported"))
             (:file	
              (when (or (not (probe-file name))
                        (clim:notify-user 
                         frame
                         (format 
                          nil 
                          "File ~a exists.~%Overwrite?" 
                          name)
                         :style :question))
                (handler-case
                    (with-open-file (output name 
                                     :direction :output 
                                     :if-exists :supersede)
                      (clim:with-output-to-postscript-stream 
                          (stream output 
                                  :scale-to-fit (not scale) 
                                  :multi-page scale
                                  :orientation orientation)
                        (funcall (clim-internals::pane-display-function 
                                  (clim-internals::find-frame-pane-of-type 
                                   frame 'clim:application-pane))
                                 frame stream)))
                  (storage-condition (condition)
                    (format t "~%Memory allocation problem: ~A~%" condition))
                  (error (condition)
                    (format t "~%Error: ~A~%" condition))
                  (serious-condition (condition)
                    (format t "~%Something nasty: ~A~%" condition))))))))
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
              excl:*initial-terminal-io*
              "~&browse-mrss(): current view now active as selection.~%"))))

        (:compare
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
                       excl:*initial-terminal-io*
                       "~&browse-mrss(): mrs-equalp() error `~a'.~%"
                       (lkb::normalize-string (format nil "~a" condition))))
                    (t
                     (format
                      excl:*initial-terminal-io*
                      "~&browse-mrss(): the equivalence comparison was ~
                       ~:[negative~;positive~].~%"
                      result)
                     (unless result
                       (format
                        excl:*initial-terminal-io*
                        "~%~%~a~%~%"
                        (get-output-stream-string stream))))))))))
          (t
           (clim:beep)
           (format
            excl:*initial-terminal-io*
            "~&browse-mrss(): no active MRS selection.~%"))))))))



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
      (clim:with-text-style (stream (mrs-transfer-font))
        (clim:formatting-row (stream)
          (let ((record 
                 (clim:formatting-cell (stream :align-x :left)
                   (if mrs
                     (mrs::output-mrs1 mrs 'mrs::simple stream)
                     (format 
                      stream 
                      "~%Invalid MRS Object~%")))))
            (when (and (mrs::fragmentp mrs) (not (edge-source edge)))
              (lkb::recolor-record record clim::+blue+))
            (when (edge-source edge)
              (lkb::recolor-record record clim:+red+))))
        (when (edge-rule edge)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~%~%")))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a~%" edge))))))))

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
              (mrs-transfer-title frame)
              (mrs-transfer-i frame)
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
                  for edge in edges
                  when (edge-p edge) collect edge
                  else collect (make-edge :mrs edge)))))
         (setf (clim:frame-pretty-name frame) 
           (or (transfer-title frame) "Transfer Input"))
         (push frame (mrs-transfer-frames frame))
         (clim:run-frame-top-level frame)))))

(let (previous)
  (defun interactively-browse-mtr (frame)
    (let* ((rules
            (loop
                for mtrs in *transfer-rule-sets*
                append (loop for mtr in (mtrs-mtrs mtrs) collect mtr)))
           (names (loop for rule in rules collect (mtr-id rule)))
           (names (sort names #'string-lessp)))
      (declare (dynamic-extent rules names))
      (let ((selection
             (lkb::with-package (:lkb)
               (lkb::ask-for-lisp-movable 
                "Transfer Rule Selection" 
                `(("MTR Identifier" . ,(or previous (first names))))
                150 names))))
        (when selection
          (let* ((id (first selection))
                 (mtr (loop 
                          for mtr in rules 
                          when (eq (mtr-id mtr) id) return mtr)))
            (unless mtr
              (clim:beep)
              (format
               excl:*initial-terminal-io*
               "~&interactively-browse-mtr(): `~a' undefined.~%"
               id)
              (return-from interactively-browse-mtr
                (interactively-browse-mtr frame)))
            (browse-mrss mtr)
            (setf previous id)))))))

#+:tsdb
(eval-when (:load-toplevel :execute)
  (setf (gethash :mrs tsdb::*statistics-browsers*) "mt::browse-mrss"))

(defun mrs-transfer-font ()
  '(:sans-serif :roman 10))
