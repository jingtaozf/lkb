

(defun change-screen-res (width height)
 (setf *screen-width* width)
  (setf *screen-height* height))

#|
(change-screen-res 640 480)
(change-screen-res 832 624)
(change-screen-res 1024 768)
(change-screen-res 1152 870)

|#

(defparameter *parse-tree-font-size* 12)

(defparameter *fs-type-font-size* 12)

(defparameter *fs-title-font-size* 12)

(defparameter *type-tree-font-size* 12)

(defparameter *dialog-font-size* 12)


(defparameter *type-font* (list "Helvetica" *fs-type-font-size*))

(defparameter *title-font* (list "Chicago" *fs-title-font-size*))

(defparameter *parse-tree-font* (list "Helvetica" (or *parse-tree-font-size* 9)))

(defparameter *type-tree-font* (list "Helvetica" *type-tree-font-size*))

(defparameter *dialog-font* (list "Chicago" *dialog-font-size* :srccopy :plain))

(setf *fred-default-font-spec* '("monaco" 12 :PLAIN))