;;; Copyright Ann Copestake 1991 - 1998
;;; 
;;; This version for Allegro 5.0 on Windows NT/95/98 is
;;; based on the version for Allegro 3.0.1 by
;;; Anthony Hull

(in-package :cl-user)

(defun add-type-and-active-fs-region (stream start-pos 
      type-label-list val shrunk-p atomic-p &optional top-box full-tdfs)
   (declare (ignore full-tdfs))
   (with-bold-output stream (format stream "~(~A~)" val))
   (add-active-fs-region stream start-pos type-label-list val shrunk-p atomic-p 
     top-box))



(defstruct edit-fs-record
   box path value type-p shrunk-p top-p)

;; type-p can have values :parent :atomic :fs :atomic or nil

(defstruct fs-display-record 
   fs title paths parents)

(defun set-associated-fs (stream fs title &optional paths parents)
   (setf (get-stream-prop stream 'feature-structure)
      (make-fs-display-record :fs fs :title title :paths paths 
         :parents parents)))


(defun add-active-fs-region (stream start-pos type-label-list val shrunk-p 
      atomic-p &optional top-box)
   (let ((box (make-box-relative-from-corner start-pos
               (- (current-position-x stream) (position-x start-pos))
               (font-size (font stream)))))
      (push
         (make-edit-fs-record :box
            box
            :path type-label-list :value val :type-p 
            (if atomic-p :atomic :fs) :shrunk-p 
            (if shrunk-p 
               :shrunk)
            :top-p top-box)
         (get-stream-prop stream 'active-menu))
      (if shrunk-p 
         (draw-box stream box))))


;;; active fs windows

;;; A node has been clicked. Pop up appropriate menu and perform
;;; appropriate action if an item was selected.
;;; (perform-edit-fs-action t nil 'top nil nil)


;  APH - code to replace defdevice

(defclass active-fs-window (bitmap-window) ())
(defmethod default-pane-class ((w active-fs-window)) 'active-fs-pane)

(defclass active-fs-pane (bitmap-pane) ())

(defmethod event
    ((stream active-fs-pane)
     (event-number (eql common-graphics:mouse-down))
     button-state position)
   ; AAC - time no longer passed as an argument
   (let
        ((fs-record (get-stream-prop (window-parent stream)
                       'feature-structure))
         (found
            (for record in
               (get-stream-prop (window-parent stream) 'active-menu)
               keep-first
               (inside-box-p position 
                  (edit-fs-record-box record)))))
      (when found
         (perform-view-fs-action stream
            fs-record
            (edit-fs-record-path found) 
            (edit-fs-record-value found)
            (edit-fs-record-type-p found)
            (edit-fs-record-shrunk-p found)
            (edit-fs-record-top-p found)))))


(defun perform-view-fs-action (stream fs-record path value type-p 
      shrunk-p top-p)
   (let ((action 
            (cond 
               (top-p (top-fs-action stream fs-record))
               (type-p
               (view-type-action stream path value shrunk-p type-p))
               (t (view-psort-action stream value)))))
      (when (or (eql action :shrink)
            (eql action :expand))
         (let ((fs (fs-display-record-fs fs-record))
               (title (fs-display-record-title fs-record))
               (parents (fs-display-record-parents fs-record))
               (paths (fs-display-record-paths fs-record)))
            (set-dag-display-value fs 
               (reverse path) action)
            (clear-page stream)
            (cond 
               (parents
                  (redisplay-fs-and-parents stream fs title parents))
               (paths (redisplay-fs-and-paths stream fs title paths))
               (t (redisplay-fs stream fs title)))))))
      


(defun view-type-action (stream path type shrunk-p type-p)
   (let ((type-entry (get-type-entry type)))
      (if type-entry 
            (pop-up-fs-menu stream path type type-entry shrunk-p type-p)
         (lkb-beep))))


(defun pop-up-fs-menu (stream path type type-entry shrunk-p type-p)
   (let ((menu 
            (open-menu
               (list
                  (make-menu-item :name "Hierarchy"
                     :value 
                     #'(lambda ()
                          (display-type-in-tree type)))
                  (make-menu-item :name "Help"
                     :value 
                     #'(lambda ()
                          (display-type-comment type
                             (nsubstitute #\Space #\Newline
                                (type-comment type-entry))
                             stream))
                     :available-p (type-comment type-entry))
                  (make-menu-item :name "Shrink/expand"
                     :value #'(lambda ()
                                 (if shrunk-p :expand :shrink))
                     :available-p (eql type-p :fs))
                  (make-menu-item :name "Type definition"
                     :value 
                     #'(lambda ()
                          (if (type-constraint type-entry)
                             (display-fs-and-parents (type-local-constraint type-entry) 
                                (format nil 
                                   "~(~A~)  - definition" 
                                   type)
                                (type-parents type-entry))
                             (format t "~%No constraint for type ~A" type))))
                  (make-menu-item :name "Expanded type"
                     :value #'(lambda ()
                                 (if (type-constraint type-entry)
                                    (display-fs-and-parents (type-constraint type-entry) 
                                       (format nil 
                                          "~(~A~) - expanded" 
                                          type)
                                       (type-parents type-entry))
                                    (format t "~%No constraint for type ~A" type)))))     
               'pop-up-menu aclwin:*lisp-main-window*
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (pop-up-menu menu)))
         (close menu)
         result)))


(defun display-type-comment (type comment-string parent-stream)
   (let ((existing (find-window "Explanation of types")))
      (if existing
         (select-window existing)
         (setf existing
               (common-graphics:open-stream 'text-edit-window
                  aclwin:*lisp-main-window* :output :title "Explanation of types")))
      (file-position existing :end)
      (do* ((str (concatenate 'string (string type) " " comment-string))
            (len (length str))
            (prev 0 (1+ next))
            (next (or (position #\space str :start (1+ prev)) len)
             (or (position #\space str :start (1+ prev)) len))
            (buf nil)
            (n 0)
            (width (- (truncate (window-interior-width existing)
                         (stream-string-width existing "M"))
                      3)))
           ((> prev next)
            (when buf
               (format existing "~%~{~A~^ ~}" (nreverse buf))))
         (incf n (1+ (- next prev)))
         (when (> n width)
            (when buf
               (format existing "~%~{~A~^ ~}" (nreverse buf))
               (setq buf nil))
            (setq n (1+ (- next prev))))
         (push (subseq str prev next) buf))))


(defun view-psort-action (stream psort)
   (let ((lex-entry (if psort (get-psort-entry psort))))
      (if lex-entry 
         (pop-up-psort-menu stream psort lex-entry)
         (let ((lex-rule-entry 
                  (get-lex-rule-entry psort)))
            (if lex-rule-entry
               (pop-up-lex-rule-menu stream psort lex-rule-entry)
               (lkb-beep))))))


(defun pop-up-psort-menu (stream psort lex-entry)
   (let ((menu 
            (open-menu
               (list
                  (make-menu-item :name "Psort definition"
                     :value 
                     #'(lambda ()
                        (display-unexpanded-lex-entry psort lex-entry)))
                  (make-menu-item :name "Expanded psort"
                     :value #'(lambda ()
                        (display-fs (lex-or-psort-full-fs lex-entry) 
                           (format nil "~(~A~) - expanded" psort)))))     
               'pop-up-menu aclwin:*lisp-main-window*
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (pop-up-menu menu)))
         (close menu)
         result)))

(defun pop-up-lex-rule-menu (stream psort rule-entry)
   (let ((menu 
            (open-menu
               (list
                  (make-menu-item :name "Show rule"
                     :value 
                     #'(lambda ()
                        (display-fs (rule-full-fs rule-entry) 
                           (format nil "~(~A~)" (rule-id rule-entry))))))     
               'pop-up-menu aclwin:*lisp-main-window*
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (pop-up-menu menu)))
         (close menu)
         result)))


;  APH - code to replace defdevice.

(defclass correct-width-window (active-fs-window) ())
(defmethod resize-window
    ((window correct-width-window) position)
   (call-next-method
      window
      (make-position
         (page-width window t)
         (min (page-height window t) (position-y position)))))


(defparameter *active-fs-page-width* 700)


;;;  APH - set-font changed to remove reference to mac-graphics, orig:
;;;    (set-font fs-window (make-font 'mac::mac-graphics 'helvetica 9 nil))
;;;  fs-window selected on creation.
;;;  Page height increased from 1800 to 5000 - 18/04/97

(defun display-basic-fs (fs title &optional parents paths)
   (let ((fs-window (common-graphics:open-stream 'correct-width-window
                      aclwin:*lisp-main-window* :io
                       :title title :window-state :shrunk
                       :page-height 5000
                       :page-width *active-fs-page-width*
                      )))
      (set-font fs-window (make-font :swiss :arial 15 nil))
      (set-associated-fs fs-window fs title paths parents)
      (display-fs-main fs-window fs title parents paths nil)
      (select-window fs-window)))


;;;  APH - set-font changed to remove reference to mac-graphics, orig:
;;;        (MAKE-FONT 'MACINTOSH::MAC-GRAPHICS :CHICAGO 12 NIL))))
;;;  Window now resized using set-page-size

(defun display-fs-main (fs-window fs title parents paths redisplay-p)
   (draw-active-title fs-window title)
   (let* ((parents-width (if parents 
               (display-active-parents parents fs-window) 0))
         (dag-width (or (display-dag1 fs 'edit fs-window) 0))
         (path-width (if paths (display-active-dpaths paths fs-window) 0))
         (max-width (max dag-width path-width parents-width 100)))
       (terpri fs-window)
       (set-page-size fs-window
         (max 
            (+ 10 max-width)
            (if redisplay-p (page-width fs-window t)
               (+ 200 (string-width title 
                        (make-font :roman :arial 14 '(:bold))))))
         (max 
            (position-y (current-position fs-window))
            (window-interior-height fs-window)))
      (resize-window fs-window
         (make-position
            (min
               (page-width fs-window t)
               (- (page-width aclwin:*lisp-main-window* t)
                  (position-x
                     (window-interior-top-left fs-window))
                  20))
            (min
               (position-y (current-position fs-window))
               (- (page-length aclwin:*lisp-main-window* t)
                  (position-y
                     (window-interior-top-left fs-window))
                  20))))
      (unless redisplay-p (expand-window fs-window))))

(defun display-fs (fs title)
   (display-basic-fs fs title))

(defun display-fs-and-paths (fs title paths)
      (display-basic-fs fs title nil paths))

         
(defun display-active-dpaths (dpath-list ostream)
   (let ((max-width 0))
      (for unif in dpath-list
         do
         (output-unif unif ostream t)
         (setf max-width (max max-width (current-position-x ostream))))
      max-width))

;;; display-fs-spec etc are in lexinput.lsp

(defun display-active-psort (psort ostream)
   (let ((start-pos (current-position ostream)))
      (with-bold-output ostream
        (format ostream "~A  " psort))
      (push
         (make-edit-fs-record :box
            (make-box-relative-from-corner start-pos
               (- (current-position-x ostream) 
                  (position-x start-pos))
               (font-size (font ostream)))
            :path nil :value psort :type-p nil)
         (get-stream-prop ostream 'active-menu))))
            

(defun display-fs-and-parents (fs title parents)
   (display-basic-fs fs title parents))

(defun display-active-parents (parents ostream)
   ;;; this function is dedicated to my Mother and Father
   (let ((max-width 0))
   (format ostream "~%Parents = ")
   (for parent in parents
      do
     (let ((start-pos (current-position ostream)))
            (with-bold-output ostream
             (format ostream "~(~A~)   " parent))
            (push
               (make-edit-fs-record :box
                  (make-box-relative-from-corner start-pos
                     (- (current-position-x ostream) 
                        (position-x start-pos))
                     (font-size (font ostream)))
                  :path nil :value parent :type-p :parent)
               (get-stream-prop ostream 'active-menu))
            (setf max-width 
               (max max-width (current-position-x ostream)))))
   (format ostream "~%")
   max-width))


(defun redisplay-fs (fs-window fs title)
   (display-fs-main fs-window fs title nil nil t))


(defun redisplay-fs-and-parents (fs-window fs title parents)
   (display-fs-main fs-window fs title parents nil t))

(defun redisplay-fs-and-paths (fs-window fs title paths)
    (display-fs-main fs-window fs title nil paths t))



(defun draw-active-title (stream title)
   (let ((start-pos (current-position stream)))
      (display-title stream title)
      (push
         (make-edit-fs-record :box
            (make-box-relative-from-corner 
               start-pos
               (- (position-x (current-position stream))
                  (position-x start-pos))
               (font-size (font stream)))
            :top-p t)
         (get-stream-prop stream 'active-menu))
      (terpri stream)))

(defun display-title (stream title)
   (with-underlined-output stream
      (format stream "~%~A~%" 
        (subseq title 0 (position #\Space title)))))


(defun top-fs-action (stream fs-record)
   (let ((menu 
            (open-menu
               (list
                  (make-menu-item :name "Print shrunk"
                     :value 
                     #'(lambda ()
                          (print-fs-plus fs-record t)))
                  (make-menu-item :name "Print expanded"
                     :value 
                     #'(lambda ()
                          (print-fs-plus fs-record)))
                  (make-menu-item :name "Output TeX"
                     :value 
                     #'(lambda ()
                          (output-fs-in-tex fs-record)))
                  (make-menu-item :name "Store fs"
                     :value 
                     #'(lambda ()
                          (store-as-psort fs-record)))
                  (make-menu-item :name "LDB entry"
                     :value 
                     #'(lambda ()
                          (show-ldb-entry fs-record))
                     :available-p (boundp '*dictionaries-available*)))               
               ;;;                   (make-menu-item :name "Write paths"
               ;;;                      :value 
               ;;;                      #'(lambda ()
               ;;;                         (write-fs fs-record)))
               ;;;                   (make-menu-item :name "Write (path notation)"
               ;;;                      :value 
               ;;;                      #'(lambda ()
               ;;;                         (write-fs fs-record t))))    
               'pop-up-menu aclwin:*lisp-main-window*
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (pop-up-menu menu)))
         (close menu)
         result)))


;;;  APH - set-font changed to remove reference to mac-graphics, orig:
;;; (set-font print-stream (make-font 'mac::mac-graphics 'helvetica 9 nil))
;;; printer-job-dialog changed to choose-default-printer
;;;  'printer-port changed to 'printer
;;;  (common-graphics:open-stream 'text 'printer :output)) changed (see function definition)

(defun print-fs-plus (fs-record &optional shrunk)
   (when fs-record
      (with-open-stream
         (print-stream
            (common-graphics:open-stream 'printer 'null-location :output))
         (let ((prtfont (ask-user-for-font :initial-font
                           (make-font :swiss :arial 18 nil))))
            (setq prtfont
               (vary-font prtfont :size
                  (floor (* (font-size prtfont)
                            (stream-units-per-inch print-stream))
                     (stream-units-per-inch aclwin:*screen*))))
            (set-font print-stream prtfont))
         (let ((fs (fs-display-record-fs fs-record))
               (title (fs-display-record-title fs-record))
               (paths (fs-display-record-paths fs-record))
               (parents (fs-display-record-parents fs-record)))
            (cond 
               (parents
                  (print-fs-and-parents print-stream fs title parents shrunk))
               (paths (print-fs-and-paths print-stream fs title paths shrunk))
               (t (display-title print-stream title)
                  (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
                  (terpri print-stream)))))))

(defun print-fs-and-parents (print-stream fs title parents &optional shrunk)
   (display-title print-stream title)
   (format print-stream "~%Parents = ")
   (for parent in parents
      do (format print-stream "~/FB/~(~A~)~/FP/   " parent))
   (format print-stream "~%")
   (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
   (terpri print-stream))

(defun print-fs-and-paths (print-stream fs title dpath-list &optional shrunk)
   (display-title print-stream title)
   (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
   (for unif in dpath-list
      do
      (output-unif unif print-stream nil))
   (terpri print-stream))

;;; TeX macros

(defun output-fs-in-tex (fs-record)
   (let ((fs (fs-display-record-fs fs-record)))
      (when fs
         (let ((file-name 
                  (ask-user-for-new-pathname
                   "File for LaTeX macros?"
                   :allowed-types '(("All files" . "*.*"))
                   :change-current-directory-p t
                   :host(namestring(current-directory)))))
            (when file-name
               (display-dag fs 'tex file-name))))))

(defun store-as-psort (fs-record)
   (let ((psort-name 'no-name)
         (fs (fs-display-record-fs fs-record)))
      (when fs
         (loop
            (setf psort-name 
               (car
               (ask-for-lisp-movable "Current Interaction" 
                  `(("Lex-id?" . ,psort-name))
                  150)))                         
            (unless psort-name (return))
            (if
               (store-temporary-psort psort-name fs)
               (return)
               (cerror "Try Again" "Name already used"))))))
               
            

;;; only accessible from menu when in the LDB
;;; actual calls to ldb functions are in ldbint.lsp

(defun show-ldb-entry (fs-record)
   (let ((fs (fs-display-record-fs fs-record)))
      (when fs
         (let ((dict
                  (get-lkb-constant-value
                     '(sense-id dictionary)
                     fs nil))
               (ldb-entry-no 
                  (get-lkb-constant-value 
                     '(sense-id ldb-entry-no)
                     fs t)))
            (display-ldb-entry-from-lkb dict
               ldb-entry-no)))))
  

(defun get-lkb-constant-value  (f-list fs number)
   (let ((dag (dag-at-end-of 
               (create-path-from-feature-list 
                  f-list)
               fs)))      
      (when dag 
         (let ((strings (type-of-fs dag)))
            (when (and strings 
                  (listp strings) 
                  (eql (length strings) 1)
                  (stringp (car strings)))
              (if number (parse-integer (car strings)) 
               (intern (car strings))))))))
                  

