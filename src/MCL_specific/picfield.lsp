;;; The class of a pop-up field.  This is implemented as a "pop-up-menu" which has menu items
;;; as given by the passed in list (a menu-items list).  The instance arguments are:-
;;;
;;;   item-display - text to display in the window (defines size of window)
;;;   menu-items - list of things to put on the menu
;;;   view-container - where to put the menu
;;;   view-position - the start of the string
;;;
;;; The pop-up is automatically positioned in the *current-view* at the current point.

(in-package :ccl)

(defclass pop-up-field (pop-up-menu)
  ((shrunk-p :initform :shrunk-p :initarg nil :accessor shrunk-p)
   (other-junk :initform :other-junk :initarg nil :accessor other-junk))
  ;; other-junk is for random junk that doesn't affect the display
  (:default-initargs
    :view-size #@(1 1)    ;;; to allow the pop-up-menu code to evaluate the size
    :view-font '("Helvetica" 9 :bold)))

(defmethod initialize-instance :after ((self pop-up-field)
                                       &rest initargs
                                       &key item-display view-position shrunk-p other-junk)
  (declare (ignore initargs view-position))
  (setf (other-junk self) other-junk)
  (setf (shrunk-p self) shrunk-p)
  (set-view-size self
                 (string-width (or item-display "") (view-font self))
                 (+ (cl-user::font-ascent self) (cl-user::font-descent self))))


;;; Annie wants not a triangle
(defmethod view-draw-contents ((self pop-up-field))
  (declare (ignore self))
  nil)

#|
  (erase-rect self (view-scroll-position self) (view-size self))
  (when (shrunk-p self)
    (frame-rect self (view-scroll-position self) (view-size self)))
  (move-to self 0 (cl-user::font-ascent self))
  (let* ((item-display (pop-up-menu-item-display self))
         (my-string (if (stringp item-display)
                      item-display
                      (format nil "~A" item-display))))
    (princ my-string self)))
|#

#| TEST CASES

(setq *my-pop-up-1*
      (make-instance 'pop-up-field
                     :item-display "Lexical blaney"
                     :view-position #@(20 20)
                     :shrunk-p t
                     :menu-items
                     (list
                      (make-instance 'menu-item
                                     :menu-item-title "item one"
                                     :menu-item-action #'(lambda ()
                                                           (print 1)))
                      (make-instance 'menu-item
                                     :menu-item-title "item two"
                                     :menu-item-action #'(lambda ()
                                                           (print 2)))
                      (make-instance 'menu-item
                                     :menu-item-title "item three"
                                     :menu-item-action #'(lambda ()
                                                           (print 3)))
                      (make-instance 'menu-item
                                     :menu-item-title "item fourteen"
                                     :menu-item-action #'(lambda ()
                                                           (print 14))))))

(setq *my-pop-up-2*
      (make-instance 'pop-up-field
                     :item-display "Gooey"
                     :view-position #@(30 50)
                     :menu-items
                     (list
                      (make-instance 'menu-item
                                     :menu-item-title "item one"
                                     :menu-item-action #'(lambda ()
                                                           (print 1)))
                      (make-instance 'menu-item
                                     :menu-item-title "item ninety nice"
                                     :menu-item-action #'(lambda ()
                                                           (print 99)))
                      (make-instance 'menu-item
                                     :menu-item-title "item three hundred and three"
                                     :menu-item-action #'(lambda ()
                                                           (print 303)))
                      (make-instance 'menu-item
                                     :menu-item-title "item fourteen fourteen (is this French?)"
                                     :menu-item-action #'(lambda ()
                                                           (print 1414))))))

(setq my-dial (make-instance 'dialog
                             :view-size #@(200 200)
                             :window-title "Pop-up Field Test"
                             :view-subviews (list *my-pop-up-1* *my-pop-up-2*)))

|#

"PICFIELD.LSP"



