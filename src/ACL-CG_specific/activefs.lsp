;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions

;;; This version for Allegro 5.0 on Windows NT/95/98 is
;;; partly based on the version for Allegro 3.0.1 by
;;; Anthony Hull

(in-package :lkb)

;;; the main entry points are a series of functions with names like
;;; display-basic-fs
;;; these functions create a window. They then call
;;; call display-dag1 (and any other ancilliary display functions)
;;;  these draw the FS into the window and
;;;  have the side effects of making the type names active

;;; This is the font for the pop up menu and the display windows

;;; *fs-type-font-size* etc is expressed in points
;;; but make-font wants pixels so approx 
;;; conversion using cg:stream-units-per-inch

(defun make-active-fs-type-font-spec nil
   (let ((pix (cg:stream-units-per-inch (lkb-screen-stream))))
      (cg:make-font :roman :|COURIER NEW| 
        (ceiling (* *fs-type-font-size* pix) 72) nil)))

(defun make-active-fs-title-font-spec nil
   (let ((pix (cg:stream-units-per-inch (lkb-screen-stream))))
   (cg:make-font :roman :|COURIER NEW|
     (ceiling (* *fs-type-font-size* pix) 72)
     nil)))


;;; following is called by outputfs.lsp

(defun add-type-and-active-fs-region (stream start-pos 
      type-label-list val shrunk-p atomic-p &optional top-box full-tdfs)
   (declare (ignore full-tdfs))
   (with-bold-output stream (format stream "~(~A~)" val))
   (add-active-fs-region stream start-pos type-label-list val shrunk-p atomic-p 
     top-box))

;;; classes are defined in fsclasses.lsp

;; type-p can have values :parent :atomic :fs :atomic or nil
;; top-p can be a string

(defstruct fs-display-record 
   fs title paths parents type-fs-display (changed-p t)
  data id)

(defun set-associated-fs (stream fs title &optional paths parents id)
   (setf (cg-user::active-fs-window-feature-structure stream)
      (make-fs-display-record :fs fs :title title :paths paths 
         :parents parents :type-fs-display *type-fs-display*
        :data (make-hash-table :test #'equal) :id id)))

(defun create-box-for-fs-region (start-pos end-pos stream)
   (cg:make-box-relative-from-corner start-pos
    (- (cg:position-x end-pos) (cg:position-x start-pos))
    (cg:font-size (cg:font stream))))

(defun add-active-fs-region (stream start-pos type-label-list val shrunk-p 
      atomic-p &optional top-box)
   (let ((fs-record (cg-user::active-fs-window-feature-structure 
                     stream)))
      (unless fs-record 
         (error "add-active-fs-region called on invalid stream"))
      (let ((changed-p (fs-display-record-changed-p fs-record)))
       (when changed-p
          (cg-user::add-fs-region-record stream 
           (create-box-for-fs-region start-pos 
               (cg:current-position stream)
               stream) 
           type-label-list
           val (if atomic-p :atomic :fs) (if shrunk-p :shrunk) top-box))      
       (if shrunk-p 
          (cg:draw-box stream 
            (create-box-for-fs-region start-pos 
               (cg:current-position stream)
               stream))))))
            
;;; **** display function entry points ****

(defun display-basic-fs (fs title &optional parents paths id)
   (let ((fs-window (cg:open-stream 'cg-user::active-fs-window
                      (lkb-parent-stream) :io
                       :title title
                       :page-height (truncate (cg:height (lkb-screen-stream)) 4)
                       :page-width (truncate (cg:width (lkb-screen-stream)) 4)
                       :window-state :shrunk
                       :scrollbars t
                       :font (make-active-fs-type-font-spec)
                      )))
      (set-associated-fs fs-window fs title paths parents id)
      (cg:expand-window fs-window)
      (cg:select-window fs-window)))


(defun display-fs-main (fs-window box)
   (let ((fs-record (cg-user::active-fs-window-feature-structure 
                      fs-window)))
      (when fs-record
         (let
          ((changed-p (fs-display-record-changed-p fs-record))
           (fs (fs-display-record-fs fs-record))
          (title (fs-display-record-title fs-record))
          (parents (fs-display-record-parents fs-record))
           (paths (fs-display-record-paths fs-record)))
      (cg:move-to-x-y fs-window 10 10)
      (draw-active-title fs-window title)
      (let* ((parents-width 
               (if parents 
                  (display-active-parents parents fs-window) 0))
             (dag-width (or (if (tdfs-p fs) 
                               (display-dag2 fs 'edit fs-window box)
                               (display-dag1 fs 'edit fs-window nil nil box)) 0))
             (path-width (if paths (display-active-dpaths paths fs-window) 0))
             (max-width (max dag-width path-width parents-width 100)))
         (terpri fs-window)
         (when changed-p
            (cg:set-page-size fs-window
              (max 
                (+ 10 max-width)
                (+ 200 (cg:font-string-width  
                         (make-active-fs-title-font-spec) title)))
              (max 
                (cg:position-y (cg:current-position fs-window))
                (cg:interior-height fs-window)))
            (cg:resize-window fs-window
              (cg:make-position
               (min
                (cg:page-width fs-window t)
                (- (cg:page-width (lkb-screen-stream) t)
                   (cg:position-x
                    (cg:interior-top-left fs-window))
                   200))
               (min
                (cg:position-y (current-position fs-window))
                (- (cg:page-length (lkb-screen-stream) t)
                   (cg:position-y
                    (cg:interior-top-left fs-window))
                   200))))
            (setf (fs-display-record-changed-p fs-record)
                  nil)))))))

(defun display-fs (fs title &optional id)
   (display-basic-fs fs title nil nil id))

(defun display-fs-and-paths (fs title paths &optional id)
      (display-basic-fs fs title nil paths id))

(defun display-fs-and-parents (fs title parents &optional id)
   (display-basic-fs fs title parents nil id))

         
(defun display-active-dpaths (dpath-list ostream)
   (let ((max-width 0))
      (loop for unif in dpath-list
         do
         (output-unif unif ostream t)
         (setf max-width (max max-width (current-position-x ostream))))
      max-width))

;;; display-fs-spec etc are in lexinput.lsp

(defun display-active-psort (psort ostream)
   (let ((start-pos (current-position ostream)))
      (with-bold-output ostream
        (format ostream "~A  " psort))
      (cg-user::add-fs-region-record ostream 
       (create-box-for-fs-region start-pos 
               (cg:current-position ostream)
               ostream) 
       nil psort nil nil nil)))
            

(defun display-active-parents (parents ostream)
   ;;; this function is dedicated to my Mother and Father
   (let ((max-width 0))
   (format ostream "~%Parents = ")
   (loop for parent in parents
      do
     (let ((start-pos (current-position ostream)))
        (with-bold-output ostream
          (format ostream "~(~A~)   " parent))           
        (cg-user::add-fs-region-record ostream
         (create-box-for-fs-region start-pos 
          (cg:current-position ostream)
          ostream)
         nil parent :parent nil nil)
        (setf max-width 
              (max max-width (cg:current-position-x ostream)))))
   (format ostream "~%")
   max-width))


(defun draw-active-title (stream title)
   (format stream "~%")
   (let ((start-pos (current-position stream)))
      (display-title stream title)
      (let ((box (create-box-for-fs-region start-pos 
               (cg:current-position stream)
               stream)))
      (cg-user::add-fs-region-record stream box nil
       nil nil nil title)
      (terpri stream))))

(defun display-title (stream title)
   (with-underlined-output stream
      (format stream "~A" 
        (subseq title 0 (position #\Space title)))))

;;; FIX
;;; Support for interactive unification check 
;;; to be added

;;; FIX
;;; Search for coreferences in a feature structure
;;; to be added

(defun add-active-pointer (stream position pointer valuep)
  (declare (ignore position))
   (format stream "<~A>" pointer)
   (when valuep
      (write-string " = " stream)))


;;; ***** Pop up menus *****
;;;
;;; There are several sorts of pop up menu which can occur
;;;
;;; 1. pop up menus for types - normally within FSs but also as parents
;;; 2. pop up menu on the top of a FS window - allows output of the
;;;    FS as a whole in TeX, storage as a psort etc
;;; 3. pop up menus for psorts displayed in paths

;;; A node has been clicked. Pop up appropriate menu and perform
;;; appropriate action if an item was selected.
;;; (perform-edit-fs-action t nil 'top nil nil)

(defun perform-view-fs-action (stream fs-record path value type-p 
      shrunk-p top-p)
   (let ((action 
            (cond 
               (top-p (top-fs-action stream fs-record top-p))
               (type-p
               (view-type-action stream path value shrunk-p type-p))
               (t (view-psort-action stream value)))))
      (when (or (eql action :shrink)
            (eql action :expand))
         (let ((fs (fs-display-record-fs fs-record)))
            (set-dag-display-value fs 
               (reverse path) action 
              (fs-display-record-type-fs-display fs-record))
            (setf (fs-display-record-changed-p fs-record)
                  t)
            (clrhash (fs-display-record-data fs-record))
            (cg:invalidate stream)))))

      
;;; **** pop up menus for types in FSs ****

(defun view-type-action (stream path type shrunk-p type-p)
   (let ((type-entry (get-type-entry type)))
      (if type-entry 
            (pop-up-fs-menu stream path type type-entry shrunk-p type-p)
         (lkb-beep))))


(defun pop-up-fs-menu (stream path type type-entry shrunk-p type-p)
   (declare (ignore path stream))
   (let ((menu 
            (cg:open-menu
               (list
                  (make-instance 'cg:menu-item :name "Hierarchy"
                     :available-p (type-p type-entry)
                     :value 
                     #'(lambda ()
                          (display-type-in-tree type)))
#|
                  (make-instance 'cg:menu-item :name "Help"
                     :value 
                     #'(lambda ()
                          (display-type-comment type
                             (nsubstitute #\Space #\Newline
                                (type-comment type-entry))
                             stream))
                     :available-p (type-comment type-entry))
|# 
                  (make-instance 'cg:menu-item :name "Shrink/expand"
                     :value #'(lambda ()
                                 (if shrunk-p :expand :shrink))
                     :available-p (eql type-p :fs))
                  (make-instance 'cg:menu-item :name "Type definition"
                     :available-p (type-p type-entry)
                     :value 
                     #'(lambda ()
                          (if (type-constraint type-entry)
                              (display-fs-and-parents 
                               (type-local-constraint type-entry) 
                                (format nil 
                                   "~(~A~)  - definition" 
                                   type)
                                (type-parents type-entry)
                                type)
                             (format t "~%No constraint for type ~A" type))))
                  (make-instance 'cg:menu-item :name "Expanded type"
                     :available-p (type-p type-entry)
                     :value #'(lambda ()
                                 (if (type-constraint type-entry)
                                     (display-fs-and-parents 
                                      (type-constraint type-entry) 
                                       (format nil 
                                          "~(~A~) - expanded" 
                                          type)
                                       (type-parents type-entry)
                                       type)
                                    (format t "~%No constraint for type ~A" type)))))     
               'cg:pop-up-menu (lkb-parent-stream)
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))

;;; ***** pop up menu actions for types ******


(defun display-type-comment (type comment-string &optional parent-stream)
   (declare (ignore parent-stream))
   (let ((existing (cg:find-window "Explanation of types")))
      (if existing
         (cg:select-window existing)
         (setf existing
               (cg:open-stream 'text-edit-window
                  (lkb-parent-stream) :output :title "Explanation of types")))
      (file-position existing :end)
      (do* ((str (concatenate 'string (string type) " " comment-string))
            (len (length str))
            (prev 0 (1+ next))
            (next (or (position #\space str :start (1+ prev)) len)
             (or (position #\space str :start (1+ prev)) len))
            (buf nil)
            (n 0)
            (width (- (truncate (cg:interior-width existing)
                         (stream-string-width existing "M"))
                      3)))
           ((> prev next)
            (when buf
               (format existing "~%~{~A~^ ~}" (nreverse buf))))
         (incf n (1+ (- next prev)))
         (when (> n width)
            (when buf
               (format existing "~%~{~A~^ ~}" (nreverse buf))
               (setq buf nil))
            (setq n (1+ (- next prev))))
         (push (subseq str prev next) buf))))

;;; **** pop up menus for psorts (called when paths are displayed) *****

(defun view-psort-action (stream psort)
   (let ((lex-entry (if psort (get-psort-entry psort))))
      (if lex-entry 
         (pop-up-psort-menu stream psort lex-entry)
         (let ((lex-rule-entry 
                  (get-lex-rule-entry psort)))
            (if lex-rule-entry
               (pop-up-lex-rule-menu stream psort lex-rule-entry)
               (lkb-beep))))))


(defun pop-up-psort-menu (stream psort lex-entry)
   (declare (ignore stream))
   (let ((menu 
            (cg:open-menu
               (list
                  (make-instance 'cg:menu-item :name "Entry definition"
                     :value 
                     #'(lambda ()
                        (display-unexpanded-lex-entry psort lex-entry)))
                  (make-instance 'cg:menu-item :name "Expanded entry"
                     :value #'(lambda ()
                        (display-fs (lex-or-psort-full-fs lex-entry) 
                                    (format nil "~(~A~) - expanded" psort)
                                    psort
                                    ))))     
               'cg:pop-up-menu (lkb-parent-stream)
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))

(defun pop-up-lex-rule-menu (stream psort rule-entry)
   (declare (ignore stream psort))
   (let ((menu 
            (cg:open-menu
               (list
                  (make-instance 'cg:menu-item :name "Show rule"
                     :value 
                     #'(lambda ()
                        (display-fs (rule-full-fs rule-entry) 
                                    (format nil "~(~A~)" (rule-id rule-entry))
                                    (rule-id rule-entry)
                                    ))))     
               'cg:pop-up-menu (lkb-parent-stream)
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))



;;; *** the title or top pop up menu ****

(defun top-fs-action (stream fs-record name)
  (declare (ignore stream name)) 
  (let* ((id (fs-display-record-id fs-record))
         (menu 
            (cg:open-menu
               (list
                  (make-instance 'cg:menu-item :name "Output TeX"
                     :available-p t
                     :value 
                     #'(lambda ()
                         (output-fs-in-tex fs-record)))
                  (make-instance 'cg:menu-item :name "Apply lex rule"
                     :available-p (and id (get-psort-entry id))
                     :value 
                     #'(lambda ()
                         (apply-lex id)))
                  (make-instance 'cg:menu-item :name "Apply lex rules"
                     :available-p (and id (get-psort-entry id))
                     :value 
                     #'(lambda ()
                          (apply-lex-rules id)))
                  (make-instance 'cg:menu-item :name "Show source" 
                     :available-p (and id (source-available-p id))
                     :value 
                    #'(lambda ()
                          (edit-source id)))   
                  )                  
               'cg:pop-up-menu (lkb-parent-stream)
               :selection-function #'lkb-funcall-menu-item)))
      (let ((result (cg:pop-up-menu menu)))
         (close menu)
         result)))


#|

(defun print-fs-plus (fs-record &optional shrunk)
   (when fs-record
      (with-open-stream
         (print-stream
            (cg:open-stream 'printer 'null-location :output))
         (let ((prtfont (cg:ask-user-for-font :initial-font
                           (cg:make-font :swiss :arial 18 nil))))
            (setq prtfont
               (cg:vary-font prtfont :size
                  (floor (* (cg:font-size prtfont)
                            (cg:stream-units-per-inch print-stream))
                     (cg:stream-units-per-inch (lkb-screen-stream)))))
            (setf (cg:font print-stream) prtfont))
         (let ((fs (fs-display-record-fs fs-record))
               (title (fs-display-record-title fs-record))
               (paths (fs-display-record-paths fs-record))
               (parents (fs-display-record-parents fs-record)))
            (cond 
               (parents
                  (print-fs-and-parents print-stream fs title parents shrunk))
               (paths (print-fs-and-paths print-stream fs title paths shrunk))
               (t (display-title print-stream title)
                  (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
                  (terpri print-stream)))))))

(defun print-fs-and-parents (print-stream fs title parents &optional shrunk)
   (display-title print-stream title)
   (format print-stream "~%Parents = ")
   (loop for parent in parents
      do (format print-stream "~/FB/~(~A~)~/FP/   " parent))
   (format print-stream "~%")
   (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
   (terpri print-stream))

(defun print-fs-and-paths (print-stream fs title dpath-list &optional shrunk)
   (display-title print-stream title)
   (display-dag1 fs (if shrunk 'swindow 'window) print-stream)
   (loop for unif in dpath-list
      do
      (output-unif unif print-stream nil))
   (terpri print-stream))

|#

;;; TeX macros

(defun output-fs-in-tex (fs-record)
   (let ((fs (fs-display-record-fs fs-record)))
      (when fs
         (let ((file-name 
                  (cg:ask-user-for-new-pathname
                   "File for LaTeX macros?"
                   :change-current-directory-p t
                   :host (namestring (current-directory)))))
            (when file-name
               (display-dag fs 'tex file-name))))))

(defun store-as-psort (fs-record)
   (let ((psort-name 'no-name)
         (fs (fs-display-record-fs fs-record)))
      (when fs
         (loop
            (setf psort-name 
              (car
               (with-package (:lkb)
               (ask-for-lisp-movable "Current Interaction" 
                  `(("Lex-id?" . ,psort-name))
                  150))))                         
            (unless psort-name (return))
            (if
               (store-temporary-psort psort-name fs)
               (return)
               (cerror "Try Again" "Name already used"))))))
               
            
;;;; stuff to atempt to avoid unnecessary redrawing
;;; the fs-record associated with a window has an associated hash table
;;; which is indexed by the path.  The elements in the hash table
;;; are fs-tmp-stores which record
;;; 1) start position where the FS was begun
;;; 2) end y coordinate
;;; 3) a box which encloses the FS plus a buffer zone
;;;    when a redraw command asks for a box that doesn't intersect
;;;    with this, this fs will be ignored when drawing
;;; 4) the flag associated with this node when it's drawn from new
;;; 5) the reentrancy pointer associated with this node 
;;;    when it's drawn from new
;;; 6) the start positions of the features

(defstruct fs-tmp-store 
  start end box flag pointer labels)

(defun store-fs-record-data (stream rpath flag pointer)
   ;; called (indirectly) from print-dag
   ;; stores the data when the structure is first drawn
   ;; or if the user shrinks/expands (or otherwise alters)
   ;; the FS
   (let ((stream-fs-record 
           (cg-user::active-fs-window-feature-structure stream)))
      (when 
          (and stream-fs-record
               (fs-display-record-changed-p stream-fs-record))
         (let
          ((start-pos (cg:current-position stream)))
           (setf
            (gethash rpath
              (fs-display-record-data stream-fs-record))
            (make-fs-tmp-store 
             :start start-pos         
             :flag flag
             :pointer pointer))))))

(defun store-fs-record-data-label (stream rpath)
   ;;; adds a feature start position
   ;;; (assumes no changes in canonical order take place
   ;;; without the FS being marked as changed!)
   (let ((stream-fs-record 
           (cg-user::active-fs-window-feature-structure stream)))
      (when 
          (and stream-fs-record
               (fs-display-record-changed-p stream-fs-record))
         (let
          ((label-xpos (cg:current-position-x stream))
           (label-ypos (cg:current-position-y stream))
           (stored-data
            (gethash rpath
              (fs-display-record-data stream-fs-record))))
          (when stored-data
             (push (cons label-xpos label-ypos)
               (fs-tmp-store-labels stored-data)))))))
   
   
(defun store-fs-record-data-end (stream rpath)
   ;; called at the end of print-dag - stores
   ;; the y position of the end of the dag and calculates
   ;; a box
   (let ((stream-fs-record 
          (cg-user::active-fs-window-feature-structure stream)))
      (when 
          (and stream-fs-record
               (fs-display-record-changed-p stream-fs-record))
         (let
          ((end-pos (cg:current-position-y stream))
           (stored-data
            (gethash rpath
              (fs-display-record-data stream-fs-record))))
          (when stored-data
             (setf (fs-tmp-store-end stored-data)
                   end-pos)
             (setf (fs-tmp-store-labels stored-data)
                   (nreverse (fs-tmp-store-labels stored-data)))
             (let ((start-pos (fs-tmp-store-start stored-data)))
                ;; if the box is simply calculated to exactly
                ;; match the data, scrolling etc can confuse it
                ;; so we allow a buffer - this could be tuned ...
                   (setf (fs-tmp-store-box stored-data)
                         (cg:make-box
                          (max 0 (- (cg:position-x start-pos) 20))
                          (max 0 (- (cg:position-y start-pos)
                                    50))
                          5000 
                          (+ 50 end-pos)))))))))
      

(defun store-fs-redisplay (stream rpath box)
   ;; if we're dealing with an unchanged FS we want to
   ;; ignore this structure if we can, but otherwise to
   ;; redraw it using the stored values (because we may
   ;; not have drawn all the preceeding stuff)
   (let ((stream-fs-record 
          (cg-user::active-fs-window-feature-structure stream)))
      (if (and stream-fs-record
               (not (fs-display-record-changed-p stream-fs-record)))
         (let ((stored-data (gethash rpath 
                              (fs-display-record-data stream-fs-record))))
            (if stored-data
               (if (dag-not-visible box (fs-tmp-store-box stored-data))                  
                  (values t nil nil nil)
                  (let ((stored-flag (fs-tmp-store-flag stored-data))
                        (stored-pointer (fs-tmp-store-pointer stored-data))
                        (stored-label-pos (fs-tmp-store-labels stored-data)))
                     (cg:move-to stream (fs-tmp-store-start stored-data)) 
                     (values nil stored-flag stored-pointer stored-label-pos)))
               (values nil nil nil nil)))
         (values nil nil nil nil))))

(defun dag-not-visible (box record-box)
   (not (cg:box-intersect-p 
          box record-box)))



