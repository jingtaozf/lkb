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

;;; like y-or-n-p but allows windows to be moved around etc
;;; before making the choice
;;; If reverse-default is missing or nil then yes is the
;;; default option - otherwise no is
;;; request dialog is a continuous dialogue - so it appears
;;; as soon as open-dialog is called
;;; There are two buttons yes and no
;;; Clicking on either of these buttons or hitting return causes the
;;; loop to terminate and the appropriate value to be returned

(defun y-or-n-p-movable (query-string &optional reverse-default)
  (let* ((spacing 10) (button-height 18) (button-width 74)
         (return-value nil) 
         (title "")
         (dialog-box nil)
         (font *dialog-font*)
         (done-flag nil)
         (query-width (max (string-width query-string font) 100))
         (width
            (max 150 (string-width title)
                 (+ spacing query-width spacing button-width spacing))))
    (setq dialog-box
          (make-instance 'dialog
            :window-type :document
            :window-title title
            :view-position '(:top 60)
            :view-size
            (make-point width
               (+ spacing button-height spacing button-height spacing))
            :view-font font
            :close-box-p nil
            :view-subviews
            (list
             (make-dialog-item 'static-text-dialog-item
                               (make-point spacing spacing)
                               (make-point query-width button-height)
                               query-string
                               nil)
             (make-dialog-item 'default-button-dialog-item
                               (make-point (+ spacing query-width spacing) spacing)
                               (make-point button-width button-height)
                               (if reverse-default "Yes" "No")
                               ; this is called when the button is clicked
                               #'(lambda (item)
                                   (setf return-value reverse-default)
                                   (setf done-flag t)
                                   (window-close (view-container item)))
                               :default-button nil)
             (make-dialog-item 'default-button-dialog-item
                               (make-point (+ spacing query-width spacing)
                                  (+ spacing button-height spacing))
                               (make-point button-width button-height)
                               (if reverse-default "No" "Yes")
                               ; this is called when the button is clicked
                               #'(lambda (item)
                                   (setf return-value (not reverse-default))
                                   (setf done-flag t)
                                   (window-close (view-container item)))))))
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

(defun ask-for-strings-movable (title prompt-init-pairs &optional (expected-width 100))
   (let* ((spacing 16) (button-height 18) (button-width 74) 
          (font *dialog-font*)
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
             (for prompt-init-pair in prompt-init-pairs
                append
                (make-prompt-init-dialog-items 
                   button-height prompt-width 
                   spacing
                   value-width
                   (incf count)
                   (car prompt-init-pair)
                   (cdr prompt-init-pair)))))
      (ask-for-strings-dialog title prompt-init-items prompt-width 
         value-width button-width button-height spacing font)))

(defun find-maximum-string-width (font items)
   (apply #'max 
      (mapcar #'(lambda (val) (if (stringp val) (string-width val font) 0))
         items)))

(defun make-prompt-init-dialog-items (button-height prompt-width spacing
                                      value-width count prompt init)
   (list
       (make-dialog-item 'static-text-dialog-item
          (make-point spacing
             (+ spacing (* (1- count) (+ spacing button-height))))
          (make-point prompt-width button-height)
          prompt
          nil)
       (make-dialog-item
          (if (eq init :check-box) 'check-box-dialog-item 'editable-text-dialog-item)
          (make-point (+ spacing prompt-width spacing) 
             (+ spacing (* (1- count) (+ spacing button-height))))
          (make-point value-width button-height)
          (if (eq init :check-box) nil init)
          nil)))

(defun ask-for-strings-dialog (title prompt-init-items prompt-width
                                     value-width button-width button-height spacing font)
  (let* ((request-dialog nil) (return-values nil)
         (title-width
            (+ 40 (string-width title font)))
         (window-width
            (max title-width
               (+ spacing spacing spacing
                  (max (+ prompt-width value-width) (+ button-width button-width)))))
         (window-height 
            (+ spacing
               (* (1+ (truncate (length prompt-init-items) 2))
                  (+ spacing button-height)))))                  
    (setf request-dialog   
          (make-instance 'dialog
            :window-type :document
            :window-title ""
            :view-position '(:top 60)
            :view-size (make-point window-width window-height)
            :view-font font
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
                                   (setf return-values 
                                         (for d-item in (cddr (dialog-items request-dialog))
                                              nconc 
                                              (cond
                                                 ((typep d-item 'editable-text-dialog-item)
                                                    (list (dialog-item-text d-item)))
                                                 ((typep d-item 'check-box-dialog-item)
                                                    (list (check-box-checked-p d-item))))))
                                   (window-close (view-container item))))
             prompt-init-items)))
    (let ((loop-return 
           (loop (event-dispatch)          
                 (when (null (wptr request-dialog)) 
                   (if return-values 
                     (return return-values)
                     (return :cancel))))))
      (if (eql loop-return :cancel) nil loop-return))))


(defun ask-for-lisp-movable (title prompt-init-pairs &optional expected-width)
   ;; Procyon version called a special dialog item - no known equivalnet in MCL
   ;; so coerce the cdrs of the prompt-init pairs to strings and coerce the
   ;; results back to s-expressions
   (let ((new-prompt-init-pairs 
          (mapcar #'(lambda (p-i-p)
                      (cons (car p-i-p)
                            (if (eq (cdr p-i-p) :check-box) :check-box
                                (format nil "~A" (cdr p-i-p)))))
             prompt-init-pairs))) 
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
  (let* ((font *dialog-font*)
         (remainder (mapcar #'(lambda (arg) (format nil "~S" arg)) args))
         (button-width 
          (max 60 
               (+ 10
                  (find-maximum-string-width font remainder))))
         (spacing 10)
         (button-height 18)
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