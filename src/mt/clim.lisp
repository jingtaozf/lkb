(in-package :mt)

(clim:define-application-frame mrs-transfer ()
  ((frames :initform nil :accessor mrs-transfer-frames :allocation :class)
   (mrss :initform nil :accessor mrs-transfer-mrss)
   (i :initform 0 :accessor mrs-transfer-i)
   (title :initform "" :accessor mrs-transfer-title)
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
           :width 500 
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
      (let ((title (format 
                    nil 
                    "~a (# ~a of ~a)" 
                    (mrs-transfer-title frame)
                    (mrs-transfer-i frame)
                    (length (mrs-transfer-mrss frame)))))
        (setf (clim:frame-pretty-name frame) title))
      (clim::redisplay-frame-panes frame :force-p t))))

(define-mrs-transfer-command (com-next-mrs-transfer :menu "Next")
    ()
  (clim:with-application-frame (frame)
    (when (< (mrs-transfer-i frame) (- (length (mrs-transfer-mrss frame)) 1))
      (incf (mrs-transfer-i frame))
      (let ((title (format 
                    nil 
                    "`~a' (# ~a of ~a)" 
                    (mrs-transfer-title frame)
                    (mrs-transfer-i frame)
                    (length (mrs-transfer-mrss frame)))))
        (setf (clim:frame-pretty-name frame) title))
      (clim::frame-replay frame (mrs-transfer-stream frame))
      (clim::redisplay-frame-panes frame :force-p t))))

(define-mrs-transfer-command (com-scope-mrs-transfer :menu "Scope")
    ()
  (clim:with-application-frame (frame)
    (let ((mrs (nth (mrs-transfer-i frame) (mrs-transfer-mrss frame)))
          (title (format 
                  nil 
                  "`~a' (# ~a of ~a) - Scopes" 
                  (mrs-transfer-title frame)
                  (mrs-transfer-i frame)
                  (length (mrs-transfer-mrss frame)))))
      (lkb::show-mrs-scoped-window nil mrs title))))

(define-mrs-transfer-command (com-transfer-mrs-transfer :menu "Transfer")
    ()
  (clim:with-application-frame (frame)
    (let* ((mrs (nth (mrs-transfer-i frame) (mrs-transfer-mrss frame)))
           (edges (mt::transfer-mrs mrs)))
      (browse-mrss
       (loop for edge in edges collect (mt::edge-mrs edge))
       "Transfer Output"))))

(define-mrs-transfer-command (com-generate-mrs-transfer :menu "Generate")
    ()
  (clim:with-application-frame (frame)
    (let ((mrs (nth (mrs-transfer-i frame) (mrs-transfer-mrss frame)))
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
  (let ((mrs (nth (mrs-transfer-i frame) (mrs-transfer-mrss frame))))
    (clim:formatting-table (stream)
      (clim:with-text-style (stream (mrs-transfer-font))
        (clim:formatting-row (stream)
          (let ((record 
                 (clim:formatting-cell (stream :align-x :left)
                   (if mrs
                     (mrs::output-mrs1 mrs 'mrs::active-t stream)
                     (format 
                      stream 
                      "~%Invalid MRS Object~%")))))
            (declare (ignore record))))))))

(defun browse-mrss (mrss &optional (title "Transfer Input"))
  (mp:run-function 
   title 
   #'(lambda ()
       (let ((full (format nil "`~a' (# ~a of ~a)" title 0 (length mrss)))
             (frame (clim:make-application-frame 'mrs-transfer)))
         (setf (mrs-transfer-mrss frame) mrss)
         (setf (mrs-transfer-title frame) title)
         (setf (clim:frame-pretty-name frame) (or full "Transfer Input"))
         (push frame (mrs-transfer-frames frame))
         (clim:run-frame-top-level frame)))))

#+:tsdb
(eval-when (:load-toplevel :execute)
  (setf (gethash :mrs tsdb::*statistics-browsers*) "mt::browse-mrss"))

(defun mrs-transfer-font ()
  '(:sans-serif :roman 12))