;;; Copyright (c) 1993-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen,
;;; John Bowler, see licence.txt for conditions


(in-package :ccl)

(Require 'quickdraw)
(ccl::require-interface 'quickdraw)
(Require 'scrolling-windows "CCL:examples;scrolling-windows")

;;; PICTURE-MAKER
;;; Create a picture independent of any window (apparently)
(defclass picture-maker (window) ()
  (:default-initargs
    :window-show nil))

(defmethod initialize-instance ((self picture-maker) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (start-picture self))

;;; Closing the window is the ONLY way of getting the picture. The CALLER is
;;; responsible for making sure that window-close is called and the picture
;;; (eventually) gets kill-pictured.
(defmethod window-close ((self picture-maker))
  (let ((my-pict (get-picture self)))
    (call-next-method)
    my-pict))

;;; SCROLLER
;;; a picture-window-pane is the real thing which is drawn - it holds the
;;; pict file, when it has been made and draws it.
(defclass picture-window-pane (scroller)
  ((pict-data :initform nil :accessor pict-data)
   (zoom-factor :initarg :scale :initform 1 :accessor zoom-factor))
  (:default-initargs)
  ;;; field-size comes from container
  ;;; view-size is container view-size - (15,15)!!!! (not my idea)
  )

;;; The draw method only needs to do anything if there is a picture
(defmethod view-draw-contents ((self picture-window-pane))
   (when (pict-data self)
       (draw-picture self (pict-data self)))
   (call-next-method))

;;; SCROLLING WINDOW
;;; The container class is a "scrolling-window" - this contains the scroller
;;; (above) and the scroll bars.  The complexity here is that the interface
;;; is through THIS window, so we have to indirect all the useful functions.
(defclass picture-window (scrolling-window) ()
  (:default-initargs
    :scroller-class 'picture-window-pane
    :close-box-p nil
    :window-type :document-with-zoom
    :view-font '("Helvetica" 10 :plain)))   ;; Change as required!

;;; This can be created with standard arguments then the PICT written into the
;;; window, or the PICT can be supplied as an argument.
(defmethod initialize-instance ((self picture-window) &rest initargs
                                &key pict)
  (declare (ignore initargs))
  (call-next-method)
  (if pict (setf (pict-data (my-scroller self)) pict))
  self)

;;; Close the window freeing the pict at the same time (if there is one)
(defmethod window-close ((self picture-window))
  (clear-picture self)
  (call-next-method))

;;; Clear out the PICT
(defmethod clear-picture ((self picture-window))
  (let ((my-pict (pict-data (my-scroller self))))
    (when my-pict
          (setf (pict-data (my-scroller self)) nil)
          (kill-picture my-pict)))
  (setf (zoom-factor (my-scroller self)) 1)
  nil)

;;; Start a new picture (clearing an existing one).
(defmethod start-picture ((self picture-window)
                          &optional left top right bottom)
  (declare (ignore left top right bottom))
  (clear-picture self)
  (call-next-method))

;;; End the picture - called get-picture for compatibility
(defmethod get-picture ((self picture-window))
    (setf (pict-data (my-scroller self)) (call-next-method))
    (invalidate-view (my-scroller self))
    (pict-data (my-scroller self)))

#| TEST CASES

(defparameter *win* (make-instance 'picture-window
                                   :window-title "picture"
                                   :field-size #@(500 500)
                                   ;;; view-size limits the pict DRAWING area
                                   :view-size #@(150 150)))
(start-picture *win*)
;; (frame-round-rect *win* 10 10 10 10 100 100)
(paint-oval *win* 75 75 200 200)
(frame-rect *win* 20 20 100 100)
(erase-oval *win* 50 50 135 135)

(get-picture *win*)
(window-close *win*)

;;; Case the next - because the pict is made out-of-line the drawing space
;;; is not limited by the eventual window view-size
(defparameter *win* (make-instance 'picture-maker :view-size #@(1000 1000)))
(paint-oval *win* 75 75 180 200)
(frame-rect *win* 20 20 100 100)
(erase-oval *win* 50 50 135 150)
(defparameter *pict* (window-close *win*))

(defparameter *win* (make-instance 'picture-window
                                   :window-title "picture 2"
                                   :pict *pict*
                                   :field-size #@(300 300)
                                   :view-size #@(50 50)))


(window-close *win*)
|#

"PICWIN.LSP"
