(in-package :mt)

(defvar %edges%)

(clim:define-application-frame mrs-transfer ()
  ((frames :initform nil :accessor mrs-transfer-frames :allocation :class)
   (edges :initform nil :accessor mrs-transfer-edges)
   (i :initform 0 :accessor mrs-transfer-i)
   (stack :initform nil :accessor mrs-transfer-stack)
   (title :initform "" :accessor mrs-transfer-title)
   (mode :initform nil :accessor mrs-transfer-mode)
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
           :width 550 
           :height 500)))))))

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


(define-mrs-transfer-command (com-scope-mrs-transfer :menu "Scope")
    ()
  (clim:with-application-frame (frame)
    (let* ((mrs (nth (mrs-transfer-i frame) 
                     (or (mrs-transfer-stack frame) 
                         (mrs-transfer-edges frame))))
           (mrs (if (edge-p mrs) (edge-mrs mrs) mrs))
           (title (format nil "~a - Scopes" (transfer-title frame))))
      (lkb::show-mrs-scoped-window nil mrs title))))


(define-mrs-transfer-command (com-transfer-mrs-transfer :menu "Transfer")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame))))
           (mrs (edge-mrs edge))
           (edges (transfer-mrs mrs :filterp nil)))
      (when edges (browse-mrss edges "Transfer Output")))))


(define-mrs-transfer-command (com-transfer-mrs-clone :menu "Clone")
    ()
  (clim:with-application-frame (frame)
    ;;
    ;; _fix_me_
    ;; use class copier instead and invoke run-function() directly on it.
    ;;                                                         (8-jan-04; oe)
    (if (mrs-transfer-stack frame)
      (browse-mrss
       (first (mrs-transfer-edges frame)) (mrs-transfer-title frame)
       :stack (mrs-transfer-stack frame) :i (mrs-transfer-i frame))
      (browse-mrss (mrs-transfer-edges frame) (mrs-transfer-title frame)))))


(define-mrs-transfer-command (com-transfer-mrs-debug :menu "Debug")
    ()
  (clim:with-application-frame (frame)
    (let* ((edge (nth (mrs-transfer-i frame) 
                      (or (mrs-transfer-stack frame)
                          (mrs-transfer-edges frame)))))
      (browse-mrss edge "Transfer Debug"))))


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


(define-mrs-transfer-command (com-print-mrs-transfer :menu "Print") 
    ()
  (clim:with-application-frame (frame)
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
               (format t "~%Something nasty: ~A~%" condition)))))))))

(defun show-mrs-transfer (frame stream &rest rest)
  (declare (ignore rest))

  (setf (mrs-transfer-stream frame) stream)
  (let* ((edge (nth (mrs-transfer-i frame) 
                    (or (mrs-transfer-stack frame) 
                        (mrs-transfer-edges frame))))
         (mrs (edge-mrs edge))
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
            (unless (and (numberp (edge-source edge))
                         (zerop (edge-source edge)))
              (lkb::recolor-record record clim:+red+))))
        (when (edge-rule edge)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~%~%")))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a~%" edge))))))))

(defun transfer-title (frame)
  (let ((edge (nth (mrs-transfer-i frame) (mrs-transfer-stack frame))))
    (case (mrs-transfer-mode frame)
      (:mtr
       (format
        nil 
        "~a @ ~a"
        (mrs-transfer-title frame)
        (aref 
         #("FILTER" "CONTEXT" "INPUT" "OUTPUT" "DEFAULT")
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
              when (zerop (edge-source edge)) count 1))
        (unless edge
          (let ((n (loop
                       for edge in (mrs-transfer-edges frame)
                       unless (zerop (edge-source edge)) count 1)))
            (unless (zerop n) n)))
        (when (and (edge-p edge) (mtr-p (edge-rule edge)))
          (mtr-id (edge-rule edge))))))))

(defun browse-mrss (edges 
                    &optional (title "Transfer Input") 
                    &key stack i)

  (unless edges (return-from browse-mrss))
  #-:debug
  (setf %edges% edges)
  (mp:run-function 
   title 
   #'(lambda ()
       (let ((frame (clim:make-application-frame 'mrs-transfer)))
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
            (setf (mrs-transfer-edges frame) 
              (list
               (make-edge :mrs (mtr-filter edges))
               (make-edge :mrs (mtr-context edges))
               (make-edge :mrs (mtr-input edges))
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

#+:tsdb
(eval-when (:load-toplevel :execute)
  (setf (gethash :mrs tsdb::*statistics-browsers*) "mt::browse-mrss"))

(defun mrs-transfer-font ()
  '(:sans-serif :roman 12))
