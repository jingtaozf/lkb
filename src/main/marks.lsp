;;; Copyright Ann Copestake 1991. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK

(in-package :cl-user)

;;; a series of functions to look after marking and unmarking of types


(defstruct (mark-field (:type vector)) seen active) ; tree-depth)
        
(defmacro active-node-p (type-record)
   `(mark-field-active (type-marks ,type-record)))

(defmacro seen-node-p (type-record)
   `(mark-field-seen (type-marks ,type-record)))

(defmacro mark-node-active-and-seen (type-record)
   `(let ((.type-marks. (type-marks ,type-record)))
      (setf (mark-field-active .type-marks.) t)
      (setf (mark-field-seen .type-marks.) t)))

(defmacro mark-node-active (type-record)
   `(setf (mark-field-active (type-marks ,type-record)) t))

(defmacro mark-node-seen (type-record)
   `(setf (mark-field-seen (type-marks ,type-record)) t))

(defmacro mark-node-seen-mark (type-record mark)
   `(push ,mark (mark-field-seen (type-marks ,type-record))))

(defmacro unmark-node-active (type-record)
   `(setf (mark-field-active (type-marks ,type-record)) nil))
   
(defmacro clear-marks (type-record)
   `(let ((.type-marks. (type-marks ,type-record)))
      (when .type-marks.
         (setf (mark-field-active .type-marks.) nil)
         (setf (mark-field-seen .type-marks.) nil))))
   
(defmacro create-mark-field (type-record)
   `(setf (type-marks ,type-record) (make-mark-field)))
