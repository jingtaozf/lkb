;;; Copyright (c) 1995-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

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
      

(defun display-dag2 (tdfs device stream &optional box) 
  (let* ((*shrunk-local-dags* (tdfs-shrunk tdfs))
         (*not-shrunk-local-dags* (tdfs-not-shrunk tdfs))
         (indef-dag (tdfs-indef tdfs))
         (def-dag (yadu-winner tdfs))
         (tail (tdfs-tail tdfs))
         (indef-width (display-dag1 indef-dag device stream nil nil box))
         (def-width 0)
         (tail-width 0))
         (when tail
           (format stream "/")
           (setf def-width
            (display-dag1 def-dag device stream 0 nil box))
           (format stream "/")
           (setf tail-width 
                 (apply #'max
                       (loop for tail-element in tail
                            collect
                            (display-tail tail-element stream))))) 
         (apply #'max 
                (cons 0 (loop for val in (list indef-width def-width
                                          tail-width)
                            when (integerp val) 
                            collect val)))))

;;; tails are displayed as
;;; specificity:  < path > = type
;;; or
;;; specificity:  < path > = < path >
;;; types are bold, lower case, as usual
;;;
;;; * indicates non-lexical persistence
;;;
;;; the new fs display type 'tail is in outputfs.lsp

;;; *description-persistence* is in globals.lsp

(defun display-tail (tail-element stream) 
   (format stream "~%")
   (let ((start-pos (current-position stream))
         (pers (tail-element-persistence tail-element))
         (spec (tail-element-spec tail-element)))
      (unless (equal pers (list *description-persistence*))
         (format stream " *"))
      (add-type-and-active-fs-region stream 
         start-pos nil spec nil t)
      (format stream ": ")
      (let ((path-rep (tail-element-path-rep tail-element)))
        (if (yadu-pv-p path-rep)
            (let ((path (path-typed-feature-list (yadu-pv-path path-rep)))
                  (value (yadu-pv-value path-rep)))
              (output-tail-features stream path) 
              (let ((new-start-pos (current-position stream)))
                (add-type-and-active-fs-region stream 
                                               new-start-pos nil
                                               value nil t)))
          (loop for path in (yadu-pp-paths path-rep)
               do
               (output-tail-features stream                        
                     (path-typed-feature-list path))))
        (current-position-x stream))))
          
          
(defun output-tail-features (stream label-list)
  (if label-list
      (progn
        (format stream "< ~A " (car label-list))
        (when (cdr label-list)
          (let ((remainder (cdr label-list)))
            (loop 
              (unless remainder (return))
              (format stream " ~A " (car remainder))
              (setf remainder (cdr remainder)))))
        (format stream ">"))
    (format stream "<>"))
  (format stream " "))
