;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; Most of the functions in here are implementation specific
;;; This has been extensively modified for MCL

(in-package :lkb)


;;; Dialogs
;;; Some general purpose functions

;;; This is a function so users can change font sizes after code has loaded

(defun lkb-dialog-font nil
   (if (ccl:osx-p) (ccl::sys-font-spec)
      (list "Chicago" *dialog-font-size* :srccopy :plain)))


(defun ask-user-for-existing-pathnames (prompt)
  ; scruffy
   (let ((pathnames nil))
      (loop 
         (let 
            ((pathname (ask-user-for-existing-pathname prompt)))
            (unless pathname (return))
            (push pathname pathnames)))
      (nreverse pathnames)))

(defun ask-user-for-existing-pathname (prompt)
  ; to match Procyon def
  (declare (ignore prompt))
  (let ((filename 
         (catch-cancel
           (choose-file-dialog))))
    (if (eql filename :cancel) nil
        filename)))

(defun ask-user-for-new-pathname (prompt &optional protected)
  (declare (ignore protected))
  ; to match Procyon def
  (let ((filename 
         (catch-cancel
           (choose-new-file-dialog :prompt prompt))))
    (if (eql filename :cancel) nil
        filename)))

(defun y-or-n-p-general (query-string)
  ;;; to avoid dialect specific stuff going in main files
  (let ((response 
         (catch-cancel
           (y-or-n-dialog query-string))))
    (not (or (null response) (eql response :cancel)))))




;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked nil is returned
;;; The dialog box built is sized appropriately

(defun ask-for-strings-movable (title prompt-init-pairs 
                                &optional (expected-width 100))
  (with-package (:lkb)
    (let* ((font (lkb-dialog-font))
           (ascent (font-info font))
           (spacing (+ ascent 4))
           (button-height 20)
           (button-width 74) 
           (prompt-width
              (+ 20
                 (find-maximum-string-width font 
                    (mapcar #'car prompt-init-pairs))))
           (value-width 
              (+ 20
                 (max
                    (find-maximum-string-width font 
                       (mapcar #'cdr prompt-init-pairs))
                 (or expected-width 0))))
           (count 0)
           (prompt-init-items 
              (loop for prompt-init-pair in prompt-init-pairs
                 append
                 (make-prompt-init-dialog-items 
                    0 button-height prompt-width 
                    spacing
                    value-width
                    (incf count)
                    (car prompt-init-pair)
                    (cdr prompt-init-pair)
                    font))))
       (ask-for-strings-dialog title prompt-init-items prompt-width 
          value-width button-width button-height spacing font))))

(defun find-maximum-string-width (font items)
   (apply #'max 
      (mapcar #'(lambda (val) (if (stringp val) (string-width val font) 0))
         items)))

(defun make-prompt-init-dialog-items (offset button-height prompt-width spacing
                                      value-width count prompt init font)
   (declare (special *choice-default*))
   (list
       (make-dialog-item 'static-text-dialog-item
          (make-point spacing
             (+ spacing (* (1- count) (+ spacing button-height))))
          (make-point prompt-width button-height)
          prompt
          nil
          :view-font font)
       (let ((top-left
                (make-point (+ offset spacing prompt-width spacing) 
                   (+ spacing (* (1- count) (+ spacing button-height))))))
          (cond
             ((eq init :check-box)
                (make-dialog-item 'check-box-dialog-item
                   top-left
                   (make-point value-width button-height)
                   nil))
             ((and (consp init) (eq (car init) :typein-menu))
              (if (and (find-class 'typein-menu nil)
                       (< (length (cdr init)) 100))
                (make-dialog-item 'typein-menu
                   top-left
                   (make-point value-width (+ button-height 3))
                   (if (and (boundp '*choice-default*) *choice-default*)
                      *choice-default* (cadr init))
                   nil
                   :view-font font
                   :menu-position :right
                   :menu-items
                   (mapcan
                      #'(lambda (s)
                          (when (< (length s) 256)
                             (list
                                (make-instance 'typein-menu-item :menu-item-title s))))
                      (cdr init)))
                ; MCL 2.0.1 doesn't have typein-menu
                (make-dialog-item 'editable-text-dialog-item
                                  top-left
                                  (make-point value-width button-height)
                                  (if (and (boundp '*choice-default*) *choice-default*)
                                     *choice-default* (cadr init))
                                  nil
                                  :view-font font)))
             (t
                (make-dialog-item 'editable-text-dialog-item
                   top-left
                   (make-point value-width button-height)
                   init
                   nil
                   :view-font font))))))


(defun ask-for-strings-dialog (title prompt-init-items prompt-width
                               value-width button-width 
                               button-height spacing font)
  (let* ((request-dialog nil) (return-values nil)
         (title-width
            (+ 40 (string-width title font)))
         (ideal-width
            (max title-width
               (+ spacing spacing spacing
                  (max (+ prompt-width value-width) 
                       (+ button-width button-width)))))
         (ideal-height 
            (+ spacing
               (* (1+ (truncate (length prompt-init-items) 2))
                  (+ spacing button-height))))                 
         (window-width
            (min ideal-width (- *screen-width* 100)))
         (window-height 
            (min ideal-height (- *screen-height* 100))))                  
    (setf request-dialog   
          (make-instance 'dialog
            :window-type :document
            :window-title ""
            :view-position (make-point 
                            (truncate (- *screen-width* window-width) 2) 60)
            :view-size (make-point window-width window-height)
            :view-font font
            :back-color (if (ccl:osx-p) *tool-back-color* *white-color*)
            (if (ccl:osx-p) :theme-background :ignore) t
            :close-box-p nil
            :view-subviews
            (list*
             (make-dialog-item 'button-dialog-item
                               (make-point
                                  (- window-width (* 2 (+ spacing button-width)))
                                  (- window-height (+ spacing button-height)))
                               (make-point button-width button-height)
                                "Cancel"
                                #'(lambda (item) 
                                    (setf return-values :cancel)
                                    (window-close (view-container item))))
             (make-dialog-item 'default-button-dialog-item
                (make-point
                   (- window-width (+ spacing button-width))
                   (- window-height (+ spacing button-height)))
                (make-point button-width button-height)
                "OK"
                #'(lambda (item) 
                    (dotimes (n (length (view-subviews request-dialog)))
                       (when (> n 1) ; skip the two buttons
                          (let ((d-item (aref (view-subviews request-dialog) n)))
                             (setq return-values 
                                (nconc return-values
                                   (cond
                                      ((typep d-item 'editable-text-dialog-item)
                                         (list (dialog-item-text d-item)))
                                      ((and (find-class 'typein-menu nil)
                                            (typep d-item 'typein-menu))
                                         ; old MCL won't have this type 
                                         (list (dialog-item-text
                                                (ccl::typein-editable-text d-item))))
                                      ((typep d-item 'check-box-dialog-item)
                                         (list (check-box-checked-p d-item)))))))))
                    (window-close (view-container item))))
             prompt-init-items)))
    (let ((loop-return 
           (loop (event-dispatch)          
                 (when (null (wptr request-dialog)) 
                   (if return-values 
                     (return return-values)
                     (return :cancel))))))
      (if (eql loop-return :cancel) nil loop-return))))


(defun ask-for-lisp-movable (title prompt-init-pairs &optional expected-width choices)
   ;; Procyon version called a special dialog item - no known equivalnet in MCL
   ;; so coerce the cdrs of the prompt-init pairs to strings and coerce the
   ;; results back to s-expressions
   (let*
      ((*choice-default* nil)
       (new-prompt-init-pairs 
          (mapcar #'(lambda (p-i-p)
                      (cons (car p-i-p)
                            (cond
                               ((eq (cdr p-i-p) :check-box)
                                 :check-box)
                               ;; ugly way of passing in multiple choices - convert
                               ;; to a typein-menu
                               ((and choices (< (length choices) 100))
                                 (setq *choice-default* (format nil "~A" (cdr p-i-p)))
                                 (cons :typein-menu
                                    (mapcar #'(lambda (x) (format nil "~A" x))
                                       choices)))
                               (t (format nil "~A" (cdr p-i-p))))))
             prompt-init-pairs))) 
     (declare (special *choice-default*))
     (mapcar
        #'(lambda (x)
            (cond
               ((symbolp x) x)
               ((equal x "") nil)
               (t (read-from-string x))))
        (ask-for-strings-movable title new-prompt-init-pairs expected-width))))


;;; Bernie kludgy code rewritten for MCL
;;;
;;; ask-user-for-multiple-choice uses buttons - previous incarnations
;;; had an other button if there were > nine choices but it would
;;; be better to use a sequence dialog in that case
;;; - this never arises in the LKB currently anyway

(defun ask-user-for-multiple-choice (question-string &rest args)
  (with-package (:lkb)
    (let* ((font (lkb-dialog-font))
           (remainder (mapcar #'(lambda (arg) (format nil "~S" arg)) args))
           (button-width 
            (max 60 
                 (+ (if (ccl:osx-p) 40 10) ; take account of rounded ends
                    (find-maximum-string-width font remainder))))
           (spacing 10)
           (button-height 20)
           (request-dialog nil) (return-value nil)
            (value nil) (count 0) (max-width 0)
           (buttons nil) 
           (current-width 0) 
           (current-height 0))
      ;;; First item is the default
      ;;; Arrange subsequent items in a table
      ;;; up to 4 items - all across
      ;;; > 4 - in rows of 4 (could make this neater)
      (loop 
        (when (null remainder) (return))
        (setf value (car remainder))
        (setf remainder (cdr remainder))
        (when value
          (incf count)
          (setf current-width 
                (if (eql (mod count 4) 1)
                  spacing
                  (+ current-width spacing button-width)))
          (setf max-width (max current-width max-width))
          (when (and (not (eql count 1))
                     (eql (mod count 4) 1))
                (setf current-height (+ button-height current-height spacing)))
          (push (make-dialog-item 'default-button-dialog-item
                                  (make-point (+ spacing current-width) 
                                              (+ spacing current-height))
                                  (make-point button-width button-height)
                                  value
                                  #'(lambda (item)
                                      (setf return-value 
                                        (dialog-item-text item))
                                      (window-close (view-container item)))
                                  :default-button (eql count 1))
                buttons)))
      (setf request-dialog   
            (make-instance 'dialog
              :window-type :document
              :window-title question-string
              :view-position '(:top 60)
              :view-size (make-point 
                          (max (+ max-width button-width spacing spacing)
                               (+ (string-width question-string font) 20))
                          (+ current-height button-height spacing spacing))
              :view-font font
              :back-color (if (ccl:osx-p) *tool-back-color* *white-color*)
              (if (ccl:osx-p) :theme-background :ignore) t
              :close-box-p nil
              :view-subviews
              (nreverse buttons)))
      (loop (event-dispatch)         ; process other guff 
            (when (null (wptr request-dialog)) 
              (if return-value 
                (return (read-from-string return-value))
                (return nil)))))))

#|
(ask-user-for-multiple-choice "Well?" "foobar" "wombat" "aardvark"
"arrdwolf" "weeble")

(ask-user-for-multiple-choice "Well?" 'foobar 'weeble)

(ask-user-for-multiple-choice "Well?" 1 2 3 4 5 6 7 8)

returns the lisp object corresponding to the button
|#
