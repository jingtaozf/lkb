;;; Copyright Ann Copestake 1991. All Rights Reserved.
;;; No use or redistribution without permission.
;;; 
;;; Ann Copestake
;;; Computer Laboratory, University of Cambridge
;;; Pembroke Street
;;; Cambridge, UK



;;; a series of functions to look after marking and unmarking of types


(defstruct mark-field seen active tree-depth)
        
(defun active-node-p (type-record)
   (mark-field-active (type-marks type-record)))

(defun seen-node-p (type-record)
   (mark-field-seen (type-marks type-record)))

(defun mark-node-active-and-seen (type-record)
   (setf (mark-field-active (type-marks type-record)) t)
   (setf (mark-field-seen (type-marks type-record)) t))

(defun mark-node-active (type-record)
   (setf (mark-field-active (type-marks type-record)) t))

(defun mark-node-seen (type-record)
   (setf (mark-field-seen (type-marks type-record)) t))

(defun mark-node-seen-mark (type-record mark)
   (push mark (mark-field-seen (type-marks type-record))))

(defun unmark-node-active (type-record)
      (setf (mark-field-active (type-marks type-record)) nil))
   
(defun clear-marks (type-record)
      (setf (mark-field-active (type-marks type-record)) nil)
      (setf (mark-field-seen (type-marks type-record)) nil))
   
(defun create-mark-field (type-record)
   (setf (type-marks type-record) (make-mark-field)))
   