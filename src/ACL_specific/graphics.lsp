;;; Graphical fs drawing 
;;; CL dialect specific
;;; This version for Allegro CL - CLIM2



;;; SIMPLE DRAWING

;;; All these position functions only affect the text-cursor in CLIM
;;; CLIM2 manual 1994 ed 19.3

(defun move-to-x-y (stream x y)
  (clim:stream-set-cursor-position stream x y))

(defun current-position-y (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (declare (ignore x))
     y))


(defun current-position-x (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (declare (ignore y))
     x))
  

(defun current-position (stream)
  (multiple-value-bind (x y)
     (clim:stream-cursor-position stream)
     (clim:make-point x y)))

(defun position-y (point)
  (clim:point-y point))

(defun position-x (point)
  (clim:point-x point))
 

;;; RECTANGLE DRAWING

#|
;;; not used ?
(defun make-box-relative-from-corner (start-position w h)
  (cons start-position (make-point (+ (point-x start-position) w)
                                   (+ (point-y start-position) h))))
|#

(defun frame-text-box (stream start-pos end-pos)
  ;;; given a start position and an end position from
  ;;; some text output, draws a suitable frame round it
  (let* ((start-x (position-x start-pos)) 
         (start-y (position-y start-pos))
         (x1 (- start-x 1)) ; upper-left point's x
         (y1 (- start-y 1)) ; upper-left point's y
         (x2 (+ (position-x end-pos) 1)) ; lower-right point's x
         (y2 (+ start-y 1 (text-font-height stream))))
                                        ; lower-right point's y
    (clim:draw-rectangle* stream x1 y1 x2 y2 :filled nil
                          :line-dashes t :line-thickness 2)))


                              
;;; actually draw-rectangle* is more efficient, but leave for now  
(defun draw-box (stream box)
  (clim:draw-rectangle stream (car box) (cdr box) :filled nil))

#|
;;; not used ?
(defun inside-box-p (position box)
  (let ((xpos (point-x position)) (ypos (point-y position)))
    (and (>= xpos (point-x (car box)))
         (<= xpos (point-x (cdr box)))
         (>= ypos (point-y (car box)))
         (<= ypos (point-y (cdr box))))))
|#

;;; FONT HANDLING

(defun actual-text-style (stream)
  ;;; check that this stream supports text styles
  ;;; otherwise just return NIL
  (if (clim:extended-output-stream-p stream)
      (if (clim:medium-text-style stream)
          (clim:merge-text-styles (clim:medium-text-style stream)
                             (clim:medium-default-text-style stream))
          (clim:medium-default-text-style stream))))
  


#|
;;; not used ?
(defun font (stream)
  (actual-text-style stream))

;;; can't do this one, because in CLIM all text styles
;;; are relative to a medium
(defun font-height (font-spec)
 (error "Untranslatable function"))
|#

(defun text-font-height (stream)
   (clim:text-style-height (actual-text-style stream) stream)) 

(defun font-ascent (stream)
  (clim:text-style-ascent (actual-text-style stream) stream))

(defun font-descent (stream)
  (clim:text-style-descent (actual-text-style stream) stream))

#|
;;; luckily it's not used - can't do it in CLIM apparently
(defun font-leading (stream)
 (error "Untranslatable function"))
|#
  

(defun stream-string-width (stream string)
  (clim:stream-string-width stream string))



;;; The functions called from format statements won't work, because
;;; it appears that Allegro CL hasn't implemented user-defined
;;; format-directives.  
;;; We also have to be careful,
;;; because we apprently can't change text-style
;;; on some windows.  So the stuff that uses
;;; FB etc will all have to be rewritten

(defmacro with-bold-output (ostream body)
  `(clim:with-text-style (,ostream '(nil :bold nil)) ,body))


(defun lkb-y-or-n-p (strg)
  ;;; define so it uses y-or-n-p-dialog 
  (y-or-n-p-general strg))
