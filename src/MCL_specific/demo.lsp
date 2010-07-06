;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


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


(setf *fred-default-font-spec* '("monaco" 12 :PLAIN))
