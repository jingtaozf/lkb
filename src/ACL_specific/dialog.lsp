;;; Copyright (c) 1991-2018 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)

;;; Dialogs for selecting files, getting user input or choices, and getting postscript print
;;; options.

(defparameter +dark-colour+ #+:mcclim climi::*3d-dark-color* #-:mcclim (clim:make-gray-color 0.59))
(defparameter +normal-colour+ #+:mcclim climi::*3d-normal-color* #-:mcclim (clim:make-gray-color 0.84))
(defparameter +light-colour+ (clim:make-gray-color 0.92))


;;; Dialogs
;;; Some general purpose functions

(defun ask-user-for-existing-pathnames (prompt)
  ; scruffy
   (let ((pathnames nil))
      (loop 
         (let 
            ((pathname (ask-user-for-existing-pathname prompt)))
            (unless pathname (return))
            (push pathname pathnames)))
      (nreverse pathnames)))

;; clim-user:*lkb-top-frame* is set up in the ACL specific file topmenu.lsp

(defun ask-user-for-existing-pathname (prompt)
  (loop for filename = (select-file clim-user:*lkb-top-frame* 
				    :title prompt
				    :directory clim-user:*last-directory*)
      do (when filename
	   (setq clim-user:*last-directory* 
             (make-pathname :name nil :type nil :defaults (pathname filename))))
      until (or (null filename)
		(and (probe-file filename)
                     ;; make sure file isn't really a directory
                     (or (pathname-name filename) (pathname-type filename))))
      finally (return filename)))

(defun ask-user-for-new-pathname (prompt &optional protected)
  (loop for filename = (select-file clim-user:*lkb-top-frame*
				    :dialog-type :save
				    :title prompt
				    :directory clim-user:*last-directory*)
      do (when filename
	   (setq clim-user:*last-directory* 
             (make-pathname :name nil :type nil :defaults (pathname filename)))
           (if (equal filename protected)
               (progn
                 (show-message-window
                  (format nil "Not permitted to overwrite the input file `~a'" protected))
                 (setf filename nil))))
      until (or (null filename)
		(not (probe-file filename))
                (when (y-or-n-p-general
                        (format nil 
                          "File `~a' exists.~%Overwrite it?" 
                          filename))
                  (delete-file filename)))
      finally (return filename)))


(defun select-file (frame &key title directory (dialog-type :open))
  ;; Select-file is present in some CLIM 2.x implementations, but with various functionality
  ;; and argument lists. It's not mentioned in the CLIM 2 specification and it's not present
  ;; in all implementations.
  ;; !!! Therefore, LKB code should not attempt to call clim:select-file directly - it should
  ;; call this function instead
  #+(and (not :select-file-lkb) (not :mswindows)) (declare (ignore dialog-type))
  #+:select-file-lkb (declare (ignore frame))
  #+:select-file-lkb
  (with-dialog-positioning (left top) 600
    (sf:select-file
      :frame-name 'lkb-file-selector
      :title title
      :directory (or directory (user-homedir-pathname))
      :dialog-type dialog-type
      :prompt (if (eq dialog-type :save) "Save as:" "Name:")
      :ok-label (if (eq dialog-type :save) "Save" "OK")
      :left left :top (+ top 22) :width 600 :height 400))
  #-:select-file-lkb
  (clim:select-file frame ;; assume the Allegro CL/CLIM 2.1 version of this function
    :title title
    :directory directory
    ;; in Allegro CLIM on Windows (only), to allow the user to give a new file name,
    ;; need to specify :dialog-type :save
    #+:mswindows :dialog-type #+:mswindows dialog-type))

#+:select-file-lkb
(progn
(defclass lkb-file-selector (sf:file-selector) ())

(defmethod sf:list-places ((frame lkb-file-selector))
  ;; add places that the user has visited last in their LKB interactions
  (labels
    ((stable-remove-duplicates-equal (lst) ; guaranteed to keep 1st occurrence of a duplicate
       (if lst
         (cons (car lst)
           (stable-remove-duplicates-equal (remove (car lst) (cdr lst) :test #'equal))))))
    (stable-remove-duplicates-equal
      (append (call-next-method)
        (let* ((home (make:getenv "DELPHINHOME"))
               (home-path
                 (when home
                   (parse-namestring home nil *default-pathname-defaults* :junk-allowed t))))
          (when home-path
            (list (cl-fad:pathname-as-directory home-path))))
        (loop
          for x in (list (locally (declare (special *print-filename*)) *print-filename*)
                         *current-grammar-load-file*
                         *grammar-directory*
                         clim-user:*last-directory*)
          when (or (stringp x) (pathnamep x))
          collect (make-pathname :name nil :type nil :defaults (pathname x)))))))
)


;;; Ask user for a y/n response. In Allegro CLIM could be done with notify-user :style :question,
;;; but :style is implementation-specific and anyway the dialog doesn't match the other LKB dialogs

(defun y-or-n-p-general (query-string &optional title)
  (show-message-window query-string '("Yes" "No") (or title "Question")))


;;; Query the user for lisp data: coerce the cdrs of the prompt-init pairs to strings,
;;; and on return coerce the strings from user input back to s-expressions. Treat an init of
;;; the empty string as specifying an empty field - and if the field comes back empty then
;;; this does not signify any lisp s-expression, so return it as nil

(defun ask-for-lisp-movable (title prompt-init-pairs 
			     &optional expected-width choices)
  (with-package (:lkb)
    (let ((new-prompt-init-pairs 
           (mapcar #'(lambda (p-i-p)
                       (cons (car p-i-p)
                         (cond
                           ((eq (cdr p-i-p) :check-box) ":CHECK-BOX")
                           ((equal (cdr p-i-p) "") "")
                           (t (let ((*print-readably* t)) (prin1-to-string (cdr p-i-p)))))))
                   prompt-init-pairs)))
      (mapcar #'(lambda (x)
                  (cond
                    ((equal x "") nil)
                    ((stringp x)
                      (handler-case (read-from-string x)
                        (error (c)
                          (error "Faulty expression `~A' causing ~A" x c))))
                    (t x)))
              (ask-for-strings-movable title new-prompt-init-pairs 
                                       expected-width choices)))))

;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked, nil is returned
;;; The dialog box built is sized appropriately

(defun ask-for-strings-movable (title prompt-init-pairs 
				&optional width choices)
  ;; TODO: if we have more choices than *maximum-list-pane-items* then we could use
  ;; them as a source of possible completions in an editable text-field gadget
  (let ((width ; minimum 600 wide for consistency and to avoid left column overwriting right
          (max (or width 0) 600)))
    (with-dialog-positioning (left top) width
      (let ((frame
   	     (clim:make-application-frame 'strings-dialog
    	       :pretty-name title
    	       :left left :top top :width width
               :prompt-init-pairs
    	       (if (and choices (<= (length choices) *maximum-list-pane-items*))
    	         (loop
    	           for p-i-p in prompt-init-pairs
    	           collect
    	           (if (and (atom (cdr p-i-p)) (not (equal (cdr p-i-p) ":CHECK-BOX")))
    	             (list* (car p-i-p) :list choices)
    	             p-i-p))
    	         prompt-init-pairs)
    	       :pane-names
               (loop repeat (length prompt-init-pairs) collect (gensym "STRINGS-GADGET")))))
        (clim:run-frame-top-level frame)
        (strings-dialog-result frame)))))


(defmacro text-field-spacing (pane)
  ;; in McCLIM only, wrap whitespace and outline around text-field gadget, otherwise
  ;; it looks too tight and flat
  #-:mcclim pane
  #+:mcclim
  `(clim:outlining (:thickness 1 :background +dark-colour+)
     (clim:outlining (:thickness 3 :background clim:+white+) ,pane)))

(defmacro horizontally-equal-widths (p1 p2)
  ;; lay out horizontally 2 panes with equal widths, and then pad on right
  `(clim:horizontally ()
     #+:mcclim
     (clim:make-pane 'clim:grid-pane
       :contents
       (list (list (clim:horizontally () ,p1 5) (clim:horizontally () 5 ,p2))))
     #-:mcclim
     (clim:horizontally (:x-spacing 10) (1/2 ,p1) (1/2 ,p2))
     :fill))

(clim:define-application-frame strings-dialog ()
  ;; each item in the prompt-init-pairs list has one of the following forms:
  ;; (label . ":CHECK-BOX") or :check-box
  ;; (label :list . non-empty-list-of-atoms)
  ;; (label :typein-menu . list-of-strings/nil)
  ;; (label . default-string)
  ((prompt-init-pairs :initarg :prompt-init-pairs
                      :reader strings-dialog-prompt-init-pairs)
   (pane-names :initarg :pane-names
               :reader strings-dialog-pane-names)
   (result :initform nil
           :accessor strings-dialog-result))
  (:menu-bar nil)
  (:pane
    (clim:vertically ()
      (clim:spacing (:thickness 15)
        (clim:make-pane 'clim:table-pane :x-spacing 10 :y-spacing 4 :align-y :center
          :contents
          (loop for p-i-p in (strings-dialog-prompt-init-pairs clim:*application-frame*)
            for name in (strings-dialog-pane-names clim:*application-frame*)
            collect
            (list
              ;; on the left: a label, possibly multi-line
              (if (and (> (length (car p-i-p)) 0) (find #\Newline (car p-i-p) :start 1))
                (clim:make-pane 'clim:vbox-pane
                  #+:mcclim :max-width #+:mcclim '(:relative 0) ; prevent any stretch
                  :contents
                  (loop for str in (split-at-linefeeds-and-squeeze (car p-i-p))
                    collect
                    (clim:make-pane 'clim:label-pane :label str)))
                (clim:make-pane 'clim:label-pane
                  #+:mcclim :max-width #+:mcclim '(:relative 0)
                  :label (string-trim '(#\Newline) (car p-i-p))))
	      ;; in the middle: extra space since Allegro CLIM table-pane ignores x-spacing
              #-:mcclim (clim:make-pane 'clim:label-pane :label "  ")
              ;; on the right: a check box, option list, or editable text field
              (cond
                ((member (cdr p-i-p) '(":CHECK-BOX" :check-box) :test #'equal)
                  (clim:make-pane 'clim:hbox-pane
                    :min-width 300
                    :contents
                    (list
                      (clim:make-pane 'clim:toggle-button :name name :value nil)
                      :fill))) ; gadget gets its min width, but allow dialog to grow rightwards
                ((and (consp (cdr p-i-p)) (eq (second p-i-p) :list))
                  (clim:make-pane 'clim:hbox-pane
                    :min-width 300
                    :contents
                    (list
                      (clim:make-pane 'clim:option-pane
                        :name name
                        :value (car (cddr p-i-p))
                        :test #'equal
                        :items (cddr p-i-p)
                        :name-key #'(lambda (name) (format nil " ~A " name)))
                      :fill)))
                (t
                  (clim:make-pane 'clim:hbox-pane
                    :x-spacing 0
                    :equalize-height t
                    #+:mcclim :min-width #+:mcclim 300 ; Allegro CLIM would take as default width
                    :contents
                    (cons
                      (text-field-spacing
                        (clim:make-pane 'clim:text-field ; in McCLIM doesn't work if subclassed
                          :name name
                          :value
                          (or (if (and (consp (cdr p-i-p))
                                       (eq (second p-i-p) :typein-menu))
                                 (car (cddr p-i-p))
                                 (cdr p-i-p))
                              "")
                          :editable-p t
                          :end-of-line-action :scroll
                          :max-width clim:+fill+
                          :background clim:+white+
                          :text-style (lkb-dialog-font)
                          :scroll-bars nil
                          :borders nil))
                      ;; optionally a pop up menu of alternatives for its sister text field
                      (if (and (consp (cdr p-i-p)) (eq (second p-i-p) :typein-menu) (cddr p-i-p))
                        (list
                          (clim:make-pane 'clim:clim-stream-pane
                            :min-width 23 :width 23 :max-width 23 ; fix its width
                            :height 23 ; prevent it stretching its sister vertically
                            :scroll-bars nil
                            :borders nil
                            :display-function
                            (let ((name name) (p-i-p p-i-p)) ; capture vars for closure
                              #'(lambda (frame pane)
                                  (declare (ignore frame))
                                  (display-prev-alternatives
                                    pane (cons name (cddr p-i-p))))))))))))))))
      #-:mcclim :fill ; in Allegro CLIM, can't prevent vertical stretch so make it here
      (clim:spacing (:thickness 15)
        (horizontally-equal-widths
          (clim:make-pane 'clim:push-button
            :label "  OK  "
            :align-x :center
            :y-spacing 5
            #-:mcclim :show-as-default #+:mcclim :show-as-default-p t ; keyword discrepancy
            :activate-callback #'strings-dialog-ok-callback)
          (clim:make-pane 'clim:push-button
            :label "  Cancel  "
            :align-x :center
            :y-spacing 5
            :activate-callback #'dialog-close-callback))))))

#+:mcclim
(defmethod clim-extensions:find-frame-type ((frame strings-dialog))
  ;; make dialogs have more dialog-like window controls (e.g. no maximize button)
  :dialog)

(defun strings-dialog-ok-callback (button)
  (declare (ignore button))
  (clim:with-application-frame (frame)
    (setf (strings-dialog-result frame)
      (loop for name in (strings-dialog-pane-names frame)
        collect
        (clim:gadget-value (get-pane-by-name frame name))))
    (clim:frame-exit frame)))

(defun get-pane-by-name (frame name)
  ;; get a pane via its :name slot (not through a (name . body) entry in the :panes
  ;; option to define-application-frame). This is straightforward in McCLIM, but in
  ;; Allegro CLIM we need to look for it in the frame's sheets
  #+:mcclim
  (clim:find-pane-named frame name)
  #-:mcclim
  (clim:map-over-sheets
    #'(lambda (p)
        (when (eql (clim:pane-name p) name) (return-from get-pane-by-name p)))
    (clim:frame-panes frame)))


;;; Strings-dialog-alts acts like a push button that that pops up a menu of alternatives, and
;;; if one is chosen, inserts it as the value of the gadget registered as its 'sister'

(clim:define-presentation-type strings-dialog-alts ())

(clim:define-presentation-method clim:highlight-presentation ((type strings-dialog-alts)
							      record stream state)
  (declare (ignore record))
  (draw-prev-alternatives stream (eq state :highlight)))

(define-strings-dialog-command (com-strings-dialog-menu) 
    ((sister-and-alts 'strings-dialog-alts :gesture :select))
  (clim:with-application-frame (frame)
    (let ((val
  	    (choose-from-strings-dialog-alts (cdr sister-and-alts))))
      (when val
        (setf (clim:gadget-value (get-pane-by-name frame (car sister-and-alts)))
	  val)))))

(defmethod choose-from-strings-dialog-alts (items &rest keys &key y-spacing text-style)
   (apply #'clim:menu-choose
          ;; not enough horizontal padding around menu items in McCLIM
  	  #+:mcclim
  	  (mapcar
  	     #'(lambda (item) (cons (format nil " ~A " item) item))
  	     items)
          #-:mcclim items
          :scroll-bars nil
          :y-spacing (or y-spacing '(4 :point))
          :text-style (or text-style (lkb-dialog-font))
          keys))

(defun display-prev-alternatives (pane sister-and-alts)
  (clim:with-output-as-presentation (pane sister-and-alts 'strings-dialog-alts :single-box t)
    (draw-prev-alternatives pane nil)))

(defun draw-prev-alternatives (pane highlightp)
  (flet
    ((draw-left-arrow (stream x0 y0) ; coordinates of apex (on left)
       (let ((dx 6) (dy 6))
	 (clim:draw-polygon* stream
	   (list x0 y0 (+ x0 dx) (- y0 dy) (+ x0 dx) (+ y0 dy))
	   :ink clim:+black+))))
    (let ((max-x (clim:bounding-rectangle-width pane))
          (max-y (clim:bounding-rectangle-height pane)))
      (clim:draw-rectangle* pane
        0 0 (1- max-x) (1- max-y) :filled nil
        :ink #+:mcclim clim:+white+ #-:mcclim +light-colour+) ; fit in with native appearance
      (clim:draw-rectangle* pane
        1 1 (- max-x 2) (- max-y 2) :filled nil
        :ink #+:mcclim clim:+white+ #-:mcclim +light-colour+)
      #+:mcclim
      (clim:draw-rectangle* pane
        2 2 (- max-x 2) (- max-y 2) :ink (if highlightp +light-colour+ +normal-colour+))
      (clim:draw-line* pane
        (1- max-x) 1 (1- max-x) (1- max-y) :ink +dark-colour+)
      (clim:draw-line* pane
        (- max-x 2) 2 (- max-x 2) (1- max-y) :ink +dark-colour+)
      (clim:draw-line* pane
        1 (1- max-y) (1- max-x) (1- max-y) :ink +dark-colour+)
      (clim:draw-line* pane
        2 (- max-y 2) (1- max-x) (- max-y 2) :ink +dark-colour+)
      (draw-left-arrow pane
        (- (round max-x 2) 4) (round max-y 2)))))


;;; temporary for ACL - doesn't work in Windows XP

(defun ask-user-for-multiple-choice (question-string &rest args)
    (loop
     (let ((result 
            (clim:menu-choose args
                       :label question-string
                       :associated-window clim-user:*lkb-top-stream*
                       :scroll-bars nil :y-spacing '(4 :point))))
       (when result 
	 (return result)))))
							

;;; Print options dialog
;;;
;;; (get-print-options)

(defvar *print-destination* :file)
(defvar *print-paper-size* :a4)
(defvar *print-orientation* :portrait)
(defvar *print-multi* t)
(defvar *print-filename* nil)

#+:mcclim
(defparameter +print-paper-sizes+
  '((:a4 . "A4") (:a3 . "A3") (:letter . "US Letter") (:11x17 . "Tabloid")))

(defun get-print-options ()
  (unless *print-filename* (setq *print-filename* (user-homedir-pathname)))
  (with-dialog-positioning (left top) 450
    (let ((frame
   	   (clim:make-application-frame 'print-dialog
    	     :pretty-name "Set Print Options"
    	     :left left :top top :width 450)))
      (clim:run-frame-top-level frame)
      (if (print-dialog-result frame)
        (values *print-destination* *print-paper-size* *print-orientation* *print-multi*
                *print-filename*)
        nil))))

(clim:define-application-frame print-dialog ()
  ((result :initform nil :accessor print-dialog-result))
  (:menu-bar nil)
  (:panes
    (destination-choices
      (clim:make-pane 'clim:option-pane
        :value (if (equal *print-destination* :printer) "Printer" "File")
        :value-changed-callback
        #'(lambda (gadget new-value)
           (clim:with-application-frame (frame)
             (print-dialog-update-df-but frame new-value)))
        :test #'string=
        :items '("Printer" "File")))
    (destination-file
      (clim:make-pane 'clim:push-button
	 :label "  Filename...  "
	 :align-x :center
         :y-spacing 5
         :activate-callback #'print-dialog-filename-callback))
    #+:mcclim
    (paper-size-choices
      (clim:make-pane 'clim:option-pane
        :value (cdr (assoc *print-paper-size* +print-paper-sizes+))
        :name-key #'(lambda (name) (format nil " ~A " name))
        :test #'string=
        :items (mapcar #'cdr +print-paper-sizes+)))
    #-:mcclim
    (orientation-choices
      (clim:make-pane 'clim:option-pane
        :value (if (equal *print-orientation* :portrait) "Portrait" "Landscape")
        :test #'string=
        :items '("Portrait" "Landscape")))
    #-:mcclim
    (pages-choices
      (clim:make-pane 'clim:toggle-button
        :value *print-multi*))
    (ok-button
      (clim:make-pane 'clim:push-button
	:label "  OK  "
	:align-x :center
        :y-spacing 5
	#-:mcclim :show-as-default #+:mcclim :show-as-default-p t ; keyword discrepancy
	:activate-callback #'print-dialog-ok-callback))
    (cancel-button
      (clim:make-pane 'clim:push-button
	:label "  Cancel  "
	:align-x :center
        :y-spacing 5
	:activate-callback #'dialog-close-callback)))
  (:layouts
    (default
      (clim:vertically (#+:mcclim :max-width #+:mcclim '(:relative 0)) ; prevent any stretch
        (clim:spacing (:thickness 15)
          (clim:make-pane 'clim:table-pane :x-spacing 30 :align-y :center
            :contents
	    (list
	      ;; spacing-pane around each gadget since Allegro CLIM table-pane ignores y-spacing
	      (list (clim:make-pane 'clim:label-pane :label "Destination:  ")
                    (clim:horizontally (:x-spacing 50)
                      (clim:spacing (:thickness 3) destination-choices)
                      destination-file
                      (clim:make-pane 'clim:label-pane :label "")))
	      #+:mcclim
              (list (clim:make-pane 'clim:label-pane :label "Paper size:  ")
	            (clim:horizontally () (clim:spacing (:thickness 3) paper-size-choices) :fill))
	      #-:mcclim
              (list (clim:make-pane 'clim:label-pane :label "Orientation:  ")
	            (clim:horizontally () (clim:spacing (:thickness 3) orientation-choices) :fill))
	      #-:mcclim
	      (list (clim:make-pane 'clim:label-pane :label "Use multiple pages?  ")
	            (clim:horizontally () (clim:spacing (:thickness 3) pages-choices) :fill)))))
        #-:mcclim :fill ; in Allegro CLIM, can't prevent vertical stretch so make it here
        (clim:spacing (:thickness 15)
          (horizontally-equal-widths ok-button cancel-button))))))

#+:mcclim
(defmethod clim-extensions:find-frame-type ((frame print-dialog))
  ;; make dialogs have more dialog-like window controls (e.g. no maximize button)
  :dialog)

(defmethod clim:run-frame-top-level :before ((frame print-dialog) &key &allow-other-keys)
  (print-dialog-update-df-but frame *print-destination*))

(defun print-dialog-update-df-but (frame value)
  (let ((df-but (clim:find-pane-named frame 'destination-file)))
    (if (member value '("Printer" :printer) :test #'equal)
      (clim:deactivate-gadget df-but)
      (clim:activate-gadget df-but))))
               
(defmethod print-dialog-filename-callback (button)
  (declare (ignore button))
  (let ((filename
  	  (select-file clim:*application-frame*
	    :directory
	    (make-pathname :name nil :type nil :defaults (pathname *print-filename*))
	    :dialog-type
	    :save)))
    (when filename (setq *print-filename* filename))))

(defun print-dialog-ok-callback (button)
  (clim:with-application-frame (frame)
    (setq *print-destination*
      (if (equal (clim:gadget-value (clim:find-pane-named frame 'destination-choices)) "Printer")
          :printer :file))
    #+:mcclim
    (setq *print-paper-size*
      (car (rassoc (clim:gadget-value (clim:find-pane-named frame 'paper-size-choices))
             +print-paper-sizes+ :test #'equal)))
    #-:mcclim
    (setq *print-orientation*
      (if (equal (clim:gadget-value (clim:find-pane-named frame 'orientation-choices)) "Portrait")
          :portrait :landscape))
    #-:mcclim
    (setq *print-multi*
      (clim:gadget-value (clim:find-pane-named frame 'pages-choices)))
    (setf (print-dialog-result frame) t)
    (clim:frame-exit frame)))
