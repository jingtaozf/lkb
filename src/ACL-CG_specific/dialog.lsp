;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


;;; ACL 5.0 Common Graphics port - based on version by Anthony Hull
;;; 
;;; Dialogs
;;; Some general purpose functions

(in-package :lkb)

(defun lkb-dialog-font nil
   (cg:make-font :roman ::arial 14 '(:bold)))

(defun ask-user-for-existing-pathname (prompt)
   (cg:ask-user-for-existing-pathname prompt))

(defun ask-user-for-new-pathname (prompt)
   (cg:ask-user-for-new-pathname prompt))

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

;;; ask-for-strings-movable takes a title and a list of 
;;; prompt . initial-value pairs
;;; A dialog is built which contains two buttons
;;; :ok and :cancel and a series of non-editable editable text
;;; pairs corresponding to the argument list
;;; When the ok box is clicked the amended vales are returned
;;; when the cancel box is clicked nil is returned
;;; The dialog box built is sized appropriately

(defun ask-for-strings-movable (title prompt-init-pairs 
                                &optional expected-width)
  (with-package (:lkb)
    (let* ((spacing 10) (button-height 20) 
          (font (lkb-dialog-font))
          (title-width (+ 40 (cg:font-string-width 
                               font title)))
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
                (or expected-width 0)))))
          (prompt-init-items 
            (let ((count 0))
             (loop for prompt-init-pair in prompt-init-pairs
                 append
                 (progn
                   (incf count)
                   (make-prompt-init-dialog-items 
                    'cg:editable-text button-height prompt-width 
                    spacing
                    value-width
                    count
                    (car prompt-init-pair)
                    (cdr prompt-init-pair)))))))
       (ask-for-strings-dialog title prompt-init-items prompt-width 
          value-width button-height spacing font))))

(defun find-maximum-string-width (font button-string string-list)
   (apply #'max 
      (mapcar #'(lambda (val) (if (stringp val) 
                                 (cg:font-string-width 
                                   font val) 0))
         (cons button-string string-list))))

(defun make-prompt-init-dialog-items (widget-type button-height 
                                      prompt-width spacing
                                      value-width count prompt init)
   (list
       (aclwin:make-dialog-item :widget 'cg:static-text
                  :value prompt
                  :box (cg:make-box-relative 
                                    spacing 
                                    (* (+ count 1) 
                                             (+ spacing 
                                             button-height))
                                    prompt-width
                                    button-height))
    (cond
     ((eq init :check-box)
      (aclwin:make-dialog-item :widget 'cg:check-box
        :title "" ; setting to nil cause title to be check-box-pane!
        :box (cg:make-box-relative 
             (+ spacing spacing prompt-width) 
             (* (+ count 1) 
                (+ spacing 
                   button-height))
             value-width
             button-height)))
     ((and (consp init) (eq (car init) :typein-menu))
      ;; we don't deal with this yet
      (aclwin:make-dialog-item :widget widget-type
        :title nil
        :value (cadr init)
        :set-value-fn #'(lambda (&rest args) (values t nil))
        :box (cg:make-box-relative 
               (+ spacing spacing prompt-width) 
               (* (+ count 1) 
                  (+ spacing 
                     button-height))
               value-width
               button-height)))
    (t (aclwin:make-dialog-item :widget widget-type
      :title nil
      :value init
      :set-value-fn #'(lambda (&rest args) (values t nil))
      :box (cg:make-box-relative 
             (+ spacing spacing prompt-width) 
             (* (+ count 1) 
                (+ spacing 
                   button-height))
             value-width
             button-height))))))

(defun ask-for-strings-dialog (title prompt-init-items prompt-width
                               value-width button-height spacing font)
  (with-package (:lkb)
    (let ((request-dialog nil) (return-values nil)
          (window-width (+ (* 3 spacing) prompt-width value-width))
          (window-height 
             (+ spacing (* (+ 1 (length prompt-init-items))
                           (+ spacing button-height)))))                  
       (setf request-dialog   
          (cg:open-dialog
             (cons
                (aclwin:make-dialog-item :widget 'cg:button
                   :title :ok
                   :box (cg:make-box-relative spacing spacing
                                              prompt-width button-height)
                   :set-value-fn 
                   #'(lambda (&rest args) 
                       (let ((i 0))
                      (setf return-values 
                         (loop for d-item in (cddr (cg:dialog-items request-dialog))
                             nconc
                               (progn
                                 (incf i)
                                 (if (evenp i) 
                                     (list (cg:value d-item)))))))
                      (values t t)))
                (cons
                (aclwin:make-dialog-item :widget 'cg:button
                   :title :cancel
                   :box (cg:make-box-relative (+ spacing spacing prompt-width)
                                              spacing
                                              value-width button-height)
                   :set-value-fn 
                   #'(lambda (&rest args) 
                      (setf return-values 
                         :cancel)
                      (values t t)))
                prompt-init-items))
             'cg:dialog
             (lkb-parent-stream)
             :pop-up-p nil
             :title title
             :font font
             :user-shrinkable nil
             :window-interior 
             (cg:make-box-relative 60 90 window-width window-height)))
       (loop (cg:process-pending-events)
          (when return-values (close request-dialog) (return nil)))
       (if (eql return-values :cancel) nil return-values))))


(defun ask-for-lisp-movable (title prompt-init-pairs &optional 
                              expected-width choices)
  (declare (ignore choices))
  (with-package (:lkb)
    ;; CLIM version uses choices when there's a specified list
    ;; of things
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
                (or expected-width 0))))
          (prompt-init-items 
            (let ((count 0))
             (loop for prompt-init-pair in prompt-init-pairs
                 append
                   (progn
                     (incf count)
                     (make-prompt-init-dialog-items 
                      'cg:lisp-text
                      button-height prompt-width 
                      spacing
                      value-width
                      count
                      (car prompt-init-pair)
                      (cdr prompt-init-pair)))))))
       (ask-for-strings-dialog title prompt-init-items prompt-width 
          value-width button-height spacing font))))


(defun ask-user-for-multiple-choice (title &rest args)
  (with-package (:lkb)
    (cg:ask-user-for-choice-from-list title args)))

