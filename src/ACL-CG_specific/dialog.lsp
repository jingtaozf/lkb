;;; Copyright Ann Copestake 1991-1998 All Rights Reserved.
;;; No use or redistribution without permission.
;;; ACL 5.0 Common Graphics port - based on version by Anthony Hull
;;; 
;;; Dialogs
;;; Some general purpose functions

(in-package :cl-user)

(defun lkb-dialog-font nil
   (cg:make-font :roman ::arial 14 '(:bold)))

(defun ask-user-for-existing-pathname (prompt)
   (cg:ask-user-for-existing-pathname prompt))

(defun ask-user-for-existing-pathnames (prompt)
   (let ((pathnames nil))
      (loop 
         (let 
            ((pathname (cg:ask-user-for-existing-pathname prompt
                        :allowed-types '(("All files" . "*.*"))
                        :change-current-directory-p t
                        :host(namestring(current-directory)))))
            (unless pathname (return))
            (push pathname pathnames)))
      (nreverse pathnames)))

(defun y-or-n-p-general (query-string)
  ;;; to avoid dialect specific stuff going in main files
   (cg:y-or-n-dialog query-string))



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

   
(defun y-or-n-p-movable (title &optional reverse-default)
   (let* ((request-dialog nil) (closed-flag nil) (return-value nil)         
         (font (lkb-dialog-font))
         (width (max 180 (+ 20 (string-width title font)))))
      (setf request-dialog   
         (open-dialog
            (list
              (make-dialog-item :widget 'button
                  :title (if reverse-default :no :yes)
                  :set-value-fn 
                  ; this is called when the button is clicked
                  #'(lambda (&rest args) 
                     (setf closed-flag t) 
                     (setf return-value (not reverse-default))
                     (values t t))
                  :box (make-box-relative 20 40 60 20))
               (make-dialog-item :widget 'button
                  :title (if reverse-default :yes :no)
                  :box (make-box-relative 
                    (+ (floor (/ width 2)) 20) 40 60 20)
                  :set-value-fn 
                  #'(lambda (&rest args) 
                     (setf closed-flag t) 
                     (setf return-value reverse-default)
                     (values t t))))
            'dialog
            aclwin:*lisp-main-window*
            :pop-up-p nil
            :user-shrinkable nil
            ; no close box on the window
            :title title
            :font font
            :window-interior (make-box-relative 60 90 width 110)))
      (loop (process-pending-events)
         ; allows other mouse events etc to be processed
         ; closed-flag is set by the set-value-fn of the buttons
         (when closed-flag (close request-dialog) (return nil)))
      return-value))

(defun y-or-n-or-but-p-movable (title &optional reverse-default)
   (let* ((request-dialog nil) (closed-flag nil) 
         (return-value nil)   (recurse-p nil)      
         (font (lkb-dialog-font))
         (width (max 180 (+ 20 (string-width title font)))))
      (setf request-dialog   
         (open-dialog
            (list
              (make-dialog-item :widget 'button
                  :title (if reverse-default :no :yes)
                  :set-value-fn 
                  ; this is called when the button is clicked
                  #'(lambda (&rest args) 
                     (setf closed-flag t) 
                     (setf return-value (not reverse-default))
                     (values t t))
                  :box (make-box-relative 20 40 60 20))
               (make-dialog-item :widget 'button
                  :title (if reverse-default :yes :no)
                  :box (make-box-relative 
                    (+ (floor (/ width 2)) 20) 40 60 20)
                  :set-value-fn 
                  #'(lambda (&rest args) 
                     (setf closed-flag t) 
                     (setf return-value reverse-default)
                     (values t t)))
               (make-dialog-item :widget 'check-box
                  :title :no-recurse
                  :box (make-box-relative 20 75 100 20)
                  :value nil))
            'dialog
            aclwin:*lisp-main-window*
            :pop-up-p nil
            :user-shrinkable nil
            ; no close box on the window
            :title title
            :font font
            :window-interior (make-box-relative 60 90 width 110)))
      (loop (process-pending-events)
         ; allows other mouse events etc to be processed
         ; closed-flag is set by the set-value-fn of the buttons
         (setf recurse-p (not (dialog-item-value 
            (third (dialog-items request-dialog)))))
         (when closed-flag (close request-dialog) (return nil)))
      (values return-value 
         recurse-p)))





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
         (font (lkb-dialog-font))
         (title-width (+ 40 (string-width title font)))
         (prompt-width (max (floor (/ title-width 2))
            (+ 20
            (find-maximum-string-width font "OK"
               (mapcar #'car 
                  prompt-init-pairs)))))
         (value-width 
            (max (floor (/ title-width 2))
            (+ 20
            (max (find-maximum-string-width font "CANCEL"
                  (mapcar #'cdr 
                     prompt-init-pairs))
               expected-width))))
         (prompt-init-items 
           (let ((count 0))
            (for prompt-init-pair in prompt-init-pairs
               append
              (incf count)
               (make-prompt-init-dialog-items 'editable-text button-height prompt-width 
                  spacing
                  value-width
                  count
                  (car prompt-init-pair)
                  (cdr prompt-init-pair))))))
      (ask-for-strings-dialog title prompt-init-items prompt-width 
         value-width button-height spacing font)))

(defun find-maximum-string-width (font button-string string-list)
   (apply #'max 
      (mapcar #'(lambda (val) (if (stringp val) (string-width val font) 0))
         (cons button-string string-list))))

(defun make-prompt-init-dialog-items (widget-type button-height prompt-width spacing
                                      value-width count prompt init)
   (list
       (make-dialog-item :widget 'static-text
                  :value prompt
                  :box (make-box-relative 
                                    spacing 
                                    (* (+ count 1) 
                                             (+ spacing 
                                             button-height))
                                    prompt-width
                                    button-height))
               (make-dialog-item :widget widget-type
                  :title nil
                  :value init
                  :set-value-fn #'(lambda (&rest args) (values t nil))
                  :box (make-box-relative 
                                    (+ spacing spacing prompt-width) 
                                    (* (+ count 1) 
                                             (+ spacing 
                                             button-height))
                                    value-width
                                    button-height))))

(defun ask-for-strings-dialog (title prompt-init-items prompt-width
      value-width button-height spacing font)
   (let ((request-dialog nil) (return-values nil)
         (window-width (+ (* 3 spacing) prompt-width value-width))
         (window-height 
            (+ spacing (* (+ 1 (length prompt-init-items))
                          (+ spacing button-height)))))                  
      (setf request-dialog   
         (open-dialog
            (cons
               (make-dialog-item :widget 'button
                  :title :ok
                  :box (make-box-relative spacing spacing
                                             prompt-width button-height)
                  :set-value-fn 
                  #'(lambda (&rest args) 
                      (let ((i 0))
                     (setf return-values 
                        (for d-item in (cddr (dialog-items request-dialog))
                           filter 
                          (incf i)
                          (if (evenp i) 
                              (dialog-item-value d-item))))) 
                     (values t t)))
               (cons
               (make-dialog-item :widget 'button
                  :title :cancel
                  :box (make-box-relative (+ spacing spacing prompt-width)
                                             spacing
                                             value-width button-height)
                  :set-value-fn 
                  #'(lambda (&rest args) 
                     (setf return-values 
                        :cancel)
                     (values t t)))
               prompt-init-items))
            'dialog
            aclwin:*lisp-main-window*
            :pop-up-p nil
            :title title
            :font font
            :user-shrinkable nil
            :window-interior 
            (make-box-relative 60 90 window-width window-height)))
      (loop (process-pending-events)
         (when return-values (close request-dialog) (return nil)))
      (if (eql return-values :cancel) nil return-values)))


(defun ask-for-lisp-movable (title prompt-init-pairs &optional expected-width)
   (let* ((spacing 10) (button-height 20) 
         (font (lkb-dialog-font))
         (prompt-width 
            (+ 20
            (find-maximum-string-width font "OK"
               (mapcar #'(lambda (x) (format nil "~S" (car x)))
                  prompt-init-pairs))))
         (value-width 
            (+ 20
            (max (find-maximum-string-width font "CANCEL"
                  (mapcar #'(lambda (x) (format nil "~S" (cdr x))) 
                     prompt-init-pairs))
               expected-width)))
         (prompt-init-items 
           (let ((count 0))
            (for prompt-init-pair in prompt-init-pairs
               append
               (incf count)
               (make-prompt-init-dialog-items 
                  'lisp-text
                  button-height prompt-width 
                  spacing
                  value-width
                  count
                  (car prompt-init-pair)
                  (cdr prompt-init-pair))))))
      (ask-for-strings-dialog title prompt-init-items prompt-width 
         value-width button-height spacing font)))


;;; Bernie's version of ask-user-for-choice
;;; Slightly cleaned up and actually close the dialog!

(defvar *choice-dialog* nil)
(defvar *choice-dialog-args* nil)

(defmacro mdib (x y title)
   `(and ,title
      (make-dialog-item
         :widget 'button
         :title ,title
         :set-value-fn
         #'(lambda (&rest val) (values t t))
         :box (make-box-relative ,x ,y butt-len 20))))


;;; APH Heading added to title.

(defun ask-user-for-multiple-choice (choice &rest args)
   ;;; This copes with an indefinite number of arguments.
   ;;; it sets the buttons up in a 3x3 array, with an 
   ;;; other button if there are more than nine arguments
   (let* ((font (lkb-dialog-font))
         (butt-len 
            (max 60 
               (+ 10
                  (apply #'max
                    (for arg in args 
                      collect 
                      (string-width arg font))))))
         (col2 (+ 20 butt-len))
         (col3 (+ col2 10 butt-len))
         (dibox (+ col3 10 butt-len))
         (len (length args)) 
         (nine-flag (eq 9 len))
         (scrns (> len 8)))
      (when args
         (setf *choice-dialog-args* args)
         (setf *choice-dialog*
            (open-dialog
               (remove nil
                  (list
                     (mdib 10 35 (first *choice-dialog-args*))
                     (mdib col2 35 (second *choice-dialog-args*))
                     (mdib col3 35 (third *choice-dialog-args*))
                     (mdib 10 65 (fourth *choice-dialog-args*))
                     (mdib col2 65 (fifth *choice-dialog-args*))
                     (mdib col3 65 (sixth *choice-dialog-args*))
                     (mdib 10 95 (seventh *choice-dialog-args*))
                     (mdib col2 95 (eighth *choice-dialog-args*))
                     (if nine-flag
                        (mdib col3 95 (ninth *choice-dialog-args*))
                        (if scrns
                           (make-dialog-item
                              :widget 'button
                              :title :other
                              :set-value-fn 
                              #'(lambda (&rest x) 
                                 (other-but col2 col3 butt-len 
                                    dibox choice))
                              :box (make-box-relative col3 95 butt-len 20))))
                     (make-dialog-item
                        :widget 'static-text
                        :value choice
                        :box (make-box-relative 10 5 (- dibox 20) 15))))
               'dialog
               aclwin:*lisp-main-window*
               :title choice
               :pop-up-p t
               :font font
               :window-interior 
               (if scrns
                  (make-box-relative 200 200 dibox 120)
                  (make-box-relative 200 200 dibox 
                     (+ 30 (* 30 (ceiling len 3)))))))
         (let ((result 
                  (dialog-item-title 
                     (pop-up-dialog
                        *choice-dialog*))))
            (close *choice-dialog*)
            result))))



 
(defun other-but (col2 col3 butt-len dibox choice)
   (setf *choice-dialog-args* 
      (append
         (subseq *choice-dialog-args* 8)
         (subseq *choice-dialog-args* 0 8)))
   ;;; cycle the arguments
   (update-dialog
      *choice-dialog*
      (remove nil
         (list
            (mdib 10 35 (first   *choice-dialog-args*))
            (mdib col2 35 (second  *choice-dialog-args*))
            (mdib col3 35 (third   *choice-dialog-args*))
            (mdib 10 65 (fourth  *choice-dialog-args*))
            (mdib col2 65 (fifth   *choice-dialog-args*))
            (mdib col3 65 (sixth   *choice-dialog-args*))
            (mdib 10 95 (seventh *choice-dialog-args*))
            (mdib col2 95 (eighth  *choice-dialog-args*))
            (make-dialog-item
               :widget 'button
               :title :other
               :set-value-fn 
               #'(lambda (&rest x) 
                  (other-but col2 col3 butt-len dibox choice))
               :box (make-box-relative col3 95 butt-len 20))
            (make-dialog-item
                        :widget 'static-text
                        :value choice
                        :box (make-box-relative 10 5 (- dibox 20) 15))))))
                 
