;;; Copyright Ann Copestake 1991-7 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Most of the functions in here are implementation specific
;;; This has been extensively modified for MCL

;;; Dialogs
;;; Some general purpose functions

(defparameter *dialog-font* (list "Chicago" *dialog-font-size* :srccopy :plain))


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

(defun ask-user-for-new-pathname (prompt)
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

;;; Becuase it's necessary to be able to move windows around
;;; to examine entries etc before making decisions the ordinary
;;; pop-up dialogs are unsuitable


   ;; like y-or-n-p but allows windows to be moved around etc
   ;; before making the choice
   ;; If reverse-default is missing or nil then yes is the
   ;; default option - otherwise no is
   ;; request dialog is a continuous dialogue - so it appears
   ;; as soon as open-dialog is called
   ;; There are two buttons yes and no
   ;; Clicking on either of these buttons or hitting return causes the
   ;; loop to terminate and the appropriate value to be returned

(defun y-or-n-p-movable (query-string &optional reverse-default)
  (let* ((return-value nil) 
         (title "Yes or No?")
         (dialog-box nil)
         (font *dialog-font*)
         (done-flag nil)
         (string-height (font-height font))
         (query-width (string-width query-string font))
         (width (max 150 (string-width title) (+ 20 query-width)))
         (box-top (+ 20 string-height))
         (box-space (/ (- width 120) 3)))
    (setf dialog-box
          (make-instance 'dialog
            :window-type :document
            :window-title title
            :view-position '(:top 60)
            :view-size (make-point width (+ box-top 30))
            :view-font font
            :close-box-p nil
            :view-subviews
            (list
             (make-dialog-item 'static-text-dialog-item
                               #@(10 10)
                               (make-point string-height query-width)
                               query-string
                               nil)
             (make-dialog-item 'default-button-dialog-item
                               (make-point box-space box-top)
                               #@(60 20)
                               (if reverse-default "no" "yes")
                               ; this is called when the button is clicked
                               #'(lambda (item)
                                   (setf return-value (not reverse-default))
                                   (setf done-flag t)
                                   (window-close (view-container item))))
             (make-dialog-item 'default-button-dialog-item
                               (make-point (+ box-space box-space 60) box-top)
                               #@(60 20)
                               (if reverse-default "yes" "no")
                               ; this is called when the button is clicked
                               #'(lambda (item)
                                   (setf return-value reverse-default)
                                   (setf done-flag t)
                                   (window-close (view-container item)))
                               :default-button nil))))
    (loop (event-dispatch)          
          (when (null (wptr dialog-box)) 
           (if done-flag 
             (return return-value)
             (error "Yes or no dialog box closed without selection"))))))


;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked nil is returned
;;; The dialog box built is sized appropriately


(defun ask-for-strings-movable (title prompt-init-pairs &optional expected-width)
   (let* ((spacing 10) (button-height 20) 
         (font *dialog-font*)
         (title-width (+ 40 (string-width title font)))
         (prompt-width (max (floor (/ title-width 2))
            (+ 20
            (find-maximum-string-width font 
              (cons "OK"
               (mapcar #'car 
                  prompt-init-pairs))))))
         (value-width 
            (max (floor (/ title-width 2))
            (+ 20
            (max (find-maximum-string-width font 
                   (cons "CANCEL"
                     (mapcar #'cdr 
                        prompt-init-pairs)))
               (or expected-width 0)))))
         (count 0)
         (prompt-init-items 
            (for prompt-init-pair in prompt-init-pairs
               append
               (incf count)
               (make-prompt-init-dialog-items 'editable-text-dialog-item 
                                              button-height prompt-width 
                  spacing
                  value-width
                  count
                  (car prompt-init-pair)
                  (cdr prompt-init-pair)))))
      (ask-for-strings-dialog title prompt-init-items prompt-width 
         value-width button-height spacing font)))

(defun find-maximum-string-width (font string-list)
      (apply #'max 
         (mapcar #'(lambda (val) (string-width val font))
           string-list)))

(defun make-prompt-init-dialog-items (widget-type button-height prompt-width spacing
                                      value-width count prompt init)
   (list
       (make-dialog-item 'static-text-dialog-item
                         (make-point spacing (* (+ count 1) 
                                             (+ spacing 
                                             button-height)))
                         (make-point prompt-width button-height)
                         prompt
                         nil)
       (make-dialog-item widget-type
                         (make-point (+ spacing spacing prompt-width) 
                                     (* (+ count 1) 
                                             (+ spacing 
                                             button-height)))
                         (make-point value-width button-height)
                         init
                         nil)))

(defun ask-for-strings-dialog (title prompt-init-items prompt-width
                                     value-width button-height spacing font)
  (let ((request-dialog nil) (return-values nil)
        (window-width (+ (* 3 spacing) prompt-width value-width))
        (window-height 
         (+ spacing (* (+ 1 (length prompt-init-items))
                       (+ spacing button-height)))))                  
    (setf request-dialog   
          (make-instance 'dialog
            :window-type :document
            :window-title title
            :view-position '(:top 60)
            :view-size (make-point window-width window-height)
            :view-font font
            :close-box-p nil
            :view-subviews
            (cons
             (make-dialog-item 'default-button-dialog-item
                               (make-point spacing spacing)
                               (make-point prompt-width button-height)
                               "OK"
                               #'(lambda (item) 
                                   (let ((i 0))
                                   (setf return-values 
                                         (for d-item in (cddr (dialog-items request-dialog))
                                              filter 
                                              (incf i)
                                              (if (evenp i) 
                                                       (dialog-item-text d-item))))
                                   (window-close (view-container item)))))
             (cons
              (make-dialog-item 'button-dialog-item
                               (make-point (+ spacing spacing prompt-width) spacing)
                               (make-point value-width button-height)
                                "Cancel"
                                #'(lambda (item) 
                                    (setf return-values 
                                          :cancel)
                                    (window-close (view-container item))))
              prompt-init-items))))
    (let ((loop-return 
           (loop (event-dispatch)          
                 (when (null (wptr request-dialog)) 
                   (if return-values 
                     (return return-values)
                     (return :cancel))))))
      (if (eql loop-return :cancel) nil loop-return))))

(defun ask-for-lisp-movable (title prompt-init-pairs &optional expected-width)
;;; Procyon version called a special dialog item - no known equivalnet in MCL
;;; so coerce the cdrs of the prompt-init pairs to strings and coerce the
;;; results back to s-expressions
  (let ((new-prompt-init-pairs 
         (mapcar #'(lambda (p-i-p)
                     (cons (car p-i-p) (format nil "~S" (cdr p-i-p))))
             prompt-init-pairs))) 
    (mapcar #'(lambda (x) (unless (equal x "") (read-from-string x)))
            (ask-for-strings-movable title new-prompt-init-pairs expected-width))))


;;; Bernie kludgy code rewritten for MCL
;;;
;;; ask-user-for-multiple-choice uses buttons - previous incarnations
;;; had an other button if there were > nine choices but it would
;;; be better to use a sequence dialog in that case
;;; - this never arises in the LKB currently anyway



(defun ask-user-for-multiple-choice (question-string &rest args)
  (let* ((font *dialog-font*)
         (remainder (mapcar #'(lambda (arg) (format nil "~S" arg)) args))
         (button-width 
          (max 60 
               (+ 10
                  (find-maximum-string-width font remainder))))
         (spacing 10)
         (button-height (font-height font))
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
                                    (setf return-value (dialog-item-text item))
                                    (window-close (view-container item)))
                                :default-button (eql count 1))
              buttons)))
    (setf request-dialog   
          (make-instance 'dialog
            :window-type :document
            :window-title question-string
            :view-position '(:top 60)
            :view-size (make-point (max (+ max-width button-width spacing spacing)
                                        (+ (string-width question-string font) 20))
                                   (+ current-height button-height spacing spacing))
            :view-font font
            :close-box-p nil
            :view-subviews
            (nreverse buttons)))
    (loop (event-dispatch)         ; process other guff 
          (when (null (wptr request-dialog)) 
            (if return-value 
              (return (read-from-string return-value))
              (return nil))))))

#|
(ask-user-for-multiple-choice "Well?" "foobar" "wombat" "aardvark"
"arrdwolf" "weeble")

(ask-user-for-multiple-choice "Well?" 'foobar 'weeble)

(ask-user-for-multiple-choice "Well?" 1 2 3 4 5 6 7 8)

returns the lisp object corresponding to the button
|#