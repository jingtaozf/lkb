;;; Copyright Ann Copestake 1995-1997. All Rights Reserved.
;;; No use or redistribution without permission.
;;;

(in-package :cl-user)

;;; TDFS display

;;; Jan 1997 - moved from yadu.lsp
;;; relies on yadu structs

;;; display-dag2 
;;; called from activefs.lsp


;;; display the indef structure 
;;; put a slash after it about half way down
;;; display the def structure
;;; put another slash after it
;;; display the tail in path notation
;;; return the maximum width this has taken
      

(defun display-dag2 (tdfs device stream) 
  (let* ((*shrunk-local-dags* (tdfs-shrunk tdfs))
         (*not-shrunk-local-dags* (tdfs-not-shrunk tdfs))
         (indef-dag (tdfs-indef tdfs))
         (def-dag (yadu-winner tdfs))
         (tail (tdfs-tail tdfs))
         (indef-width (display-dag1 indef-dag device stream))
         (def-width 0)
         (tail-width 0))
         (when tail
           (format stream "/")
           (setf def-width
            (display-dag1 def-dag device stream 0))
           (format stream "/")
           (setf tail-width 
                 (apply #'max
                       (for tail-element in tail
                            collect
                            (display-tail tail-element stream))))) 
         (apply #'max 
                (cons 0 (for val in (list indef-width def-width
                                          tail-width)
                             filter
                             (if (integerp val) val))))))

;;; tails are displayed as
;;; specificity:  < path > = type
;;; or
;;; specificity:  < path > = < path >
;;; types are bold, lower case, as usual
;;;
;;; * indicates non-lexical persistence
;;;
;;; the new fs display type 'tail is in outputfs.lsp

;;; *lexical-persistence* is in globals.lsp

(defun display-tail (tail-element stream) 
  ; needs fixing
   (format stream "~%")
   (let ((start-pos (current-position stream))
         (pers (tail-element-persistence tail-element))
         (spec (tail-element-spec tail-element)))
;         (dag-instance (tail-element-fs tail-element)))
      (unless (equal pers (list *lexical-persistence*))
         (format stream " *"))
      (add-type-and-active-fs-region stream 
         start-pos nil spec nil t)
      (format stream ": ")
      (format stream (tail-element-path-rep tail-element))))
;      (display-dag1 dag-instance 'tail stream 
;        (current-position-x stream))))
