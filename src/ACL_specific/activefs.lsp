;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Modified Jan 1995 to avoid unnecessary duplication in code

;;; Port to MCL requires extensive rewrite

;;; rewritten 1996 for Allegro / CLIM

;;; 1997 - merged with YADU version

;;; the main entry points are a series of functions with names like
;;; display-basic-fs
;;; these functions create a window. They then call
;;; call display-dag1 (and any other ancilliary display functions)
;;;  these draw the FS into the window and
;;;  have the side effects of making the type names active

;;; This is the font for the pop up menu and the display windows

(defparameter *type-font* (clim:parse-text-style '(:sans-serif :roman 9)))

(defparameter *title-font* (clim:parse-text-style '(:fix :roman 12)))

;;; ***** active types *****

;;; moved from outputfs.lsp to make that Lisp-independent

;;; for ACL it appears that we need things of a particular
;;; type for presentations, and the easiest way of doing this 
;;; is with a struct

(defstruct type-thing value type-label-list shrunk-p full-tdfs)

;;; YADU - here and below, the slot full-tdfs is needed so that
;;; B+C96 style lexical rules can be displayed in the type -> type format
;;; with the full fs associated with full-tdfs


(defun add-type-and-active-fs-region (stream start-pos 
      type-label-list val shrunk-p atomic-p &optional top-box full-tdfs)
  (declare (ignore start-pos atomic-p top-box))
  (let ((type-rec
         (make-type-thing :value val
                          :type-label-list type-label-list
                          :shrunk-p shrunk-p
                          :full-tdfs full-tdfs)))
    (clim:with-text-style (stream '(nil :bold nil))
         (clim:with-output-as-presentation 
              (stream type-rec 'type-thing)
              (format stream "~(~A~)" val)))))



;;; ***** Records and classes *******

(defstruct fs-display-record 
;;; the record of the FS associated with a window
   fs title paths parents lrout)

;;
;; Define a frame class for our FS windows
;;

(clim:define-application-frame active-fs-window ()
  ((fs :initform nil
   :accessor active-fs-window-fs))
  (:panes
   (display :application
	    :display-function 'draw-active-fs
	    :text-cursor nil
	    :width :compute :height :compute
;	    :text-style *tree-text-style*
	    :borders nil
            :display-after-commands nil))
  (:layouts
    (default display)))

;;; **** display function entry points ****

(defun display-basic-fs (fs title &optional parents paths output-fs)
   (let ((fs-window 
          (clim:make-application-frame 'active-fs-window)))
        (setf (active-fs-window-fs fs-window) 
              (make-fs-display-record :fs fs :title title :paths paths 
                                      :parents parents
                                      :lrout output-fs))
        (mp:process-run-function "FS" 
                                 #'clim:run-frame-top-level
                                 fs-window)))

(defun display-fs (fs title)
   (display-basic-fs fs title))

(defun display-fs-and-parents (fs title parents)
   (display-basic-fs fs title parents))

(defun display-fs-and-paths (fs title paths)
      (display-basic-fs fs title nil paths))


;;; YADU display

;;; display-tdfs-window is unnecessary - use display-fs instead

(defun display-lrule-window (input-tdfs output-tdfs title)
  (display-basic-fs input-tdfs title nil nil output-tdfs))


;;; process lock appears to be necessary (at least in CLIM 2.1.beta)
;;; to avoid the situation where the wrong output goes to a 
;;; window when several FSs are drawn one after another
;;; for instance, when displaying a series of results of LR application

(defvar *fs-output-lock* (mp:make-process-lock))

(defun draw-active-fs (window stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (mp:with-process-lock (*fs-output-lock*)
    (let* ((fs-record (active-fs-window-fs window))
           (fs (fs-display-record-fs fs-record))
           (title (fs-display-record-title fs-record))
           (parents (fs-display-record-parents fs-record))
           (paths (fs-display-record-paths fs-record))
           (lrule-out (fs-display-record-lrout fs-record))
           (fudge 20)
           (max-width 0))
    ;;; the terpris are here because of the CLIM 2.1 
    ;;; bug of apparently not allowing for scroll bars when 
    ;;; sizing a window
      (terpri stream)
      (terpri stream)
      (terpri stream)
      (draw-active-title stream fs title parents paths)
      (when parents 
            (setf max-width (+ fudge (display-active-parents parents stream))))
      (let ((dag-width (or (if (tdfs-p fs) 
                               (display-dag2 fs 'edit stream)
                             (if lrule-out
                                 (display-lrule fs lrule-out stream)
                                 (display-dag1 fs 'edit stream))) 0)))
        (setf max-width (max (+ fudge dag-width max-width)))
        (when paths (setf max-width 
                        (max max-width 
                             (+ fudge (display-active-dpaths paths stream))))))
      (move-to-x-y stream max-width (current-position-y stream))
    ; for CLIM bug as above
      (format stream ".~%")
      stream)))


;;; display-dag2 and display-lrule should be in output(td)fs.lsp
;;; rather than activefs.lsp

;;; **** displaying parents and paths ***

(defun display-active-parents (parents ostream)
   ;;; this function is dedicated to my Mother and Father
  (format ostream "~%Parents = ")
  (for parent in parents
       do
       (let ( ; (start-pos (current-position ostream))
             (val (make-type-thing :value parent)))
         (clim:with-text-style (ostream '(nil :bold nil))
              (clim:with-output-as-presentation 
                   (ostream val 'type-thing)
                   (format ostream "~(~A~) " (type-thing-value val))))))
  (let ((max-width (current-position-x ostream)))
    (format ostream "~%")
    max-width))

(defun display-active-dpaths (dpath-list ostream)
   (let ((max-width 0))
     (for unif in dpath-list
          do
         (output-unif unif ostream t)
         (setf max-width (max max-width (current-position-x ostream))))
      max-width))


(defstruct title-thing title fs)

(defun draw-active-title (stream fs title parents paths)
  (declare (ignore parents paths))
  (let* ((val (make-title-thing :title title :fs fs)))
;         (short-title (subseq title 0 (position #\Space title))))
    (clim:with-text-style (stream '(nil nil :large))
              (clim:with-output-as-presentation 
                   (stream val 'title-thing)
                   (format stream "~%~A~%" title)))))



;; 
;; Add [EXIT] button
;;


(define-active-fs-window-command (com-exit-active-fs :menu "Close")
    ()
  (clim:frame-exit clim:*application-frame*))



;;; ***** Pop up menu creation *****
;;;
;;; There are several sorts of pop up menu which can occur
;;;
;;; 1. pop up menus for types - normally within FSs but also as parents
;;; 2. pop up menu on the top of a FS window - allows output of the
;;;    FS as a whole in TeX, storage as a psort etc
;;; 3. pop up menus for psorts displayed in paths

;;; **** pop up menus for types in FSs ****

(define-active-fs-window-command (com-type-fs-menu)
    ((type-thing 'type-thing :gesture :select))
  (let* ((type (type-thing-value type-thing))
         (type-entry (get-type-entry (if (listp type) (car type) type)))
         (type-label-list (type-thing-type-label-list type-thing))
         (shrunk-p (type-thing-shrunk-p type-thing))
         (full-tdfs (type-thing-full-tdfs type-thing)))
    (if (and (atom type) type-entry)
        (let
         ((command (clim:menu-choose
                   '(("Hierarchy" :value hier)
                     ("Help" :value help)
                     ("Shrink/expand" :value shrink)
                     ("Type definition" :value def)
                     ("Expanded type" :value exp)
                     ("Full structure" :value full)))))
         ; see CLIM 13-2 for making things inactive
    (when command
          (handler-case
            (ecase command
                   (hier (display-type-in-tree type))
                   (help (display-type-comment type 
                                  (type-comment type-entry)))
                   (shrink (shrink-fs-action 
                                   clim:*application-frame*
                                   (if shrunk-p :expand :shrink)
                                                type-label-list))
                   (def                      
                     (if (type-constraint type-entry)
                         (display-fs-and-parents 
                          (type-local-constraint type-entry) 
                          (format nil 
                                  "~(~A~)  - definition" 
                                  type)
                          (type-parents type-entry))
                       (format clim-user:*lkb-top-stream* 
                               "~%No constraint for type ~A" type)))
                   (exp (if (type-constraint type-entry)
                            (display-fs-and-parents 
                             (type-constraint type-entry) 
                                   (format nil 
                                           "~(~A~) - expanded" 
                                           type)
                                   (type-parents type-entry))
                          (format clim-user:*lkb-top-stream* 
                                  "~%No constraint for type ~A" type)))
                   (full (if full-tdfs
                             (display-fs full-tdfs
                                 (format nil 
                                    "LR constraint")))))
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))))



  


;;; ***** pop up menu actions for types ******

; shrink-fs not yet implemented

(defun shrink-fs-action (window action path)
  (let* ((fs-record (active-fs-window-fs window))
         (fs (fs-display-record-fs fs-record)))
      ;   (title (fs-display-record-title fs-record))
      ;   (parents (fs-display-record-parents fs-record))
      ;   (paths (fs-display-record-paths fs-record)))
    (set-dag-display-value fs (reverse path) action)
    (clim:redisplay-frame-panes window :force-p t)))


(defun display-type-comment (type comment-string &optional parent-stream)
  (declare (ignore parent-stream type))
  (if comment-string
      (format clim-user:*lkb-top-stream* 
              "~%~A" comment-string)))




;;; *** the title or top pop up menu ****


(define-active-fs-window-command (com-title-fs-menu)
    ((title-thing 'title-thing :gesture :select))
  (let* ((fs (title-thing-fs title-thing))
         (command (clim:menu-choose
                   '(("Output TeX" :value tex)
                     ("Store fs" :value store)))))
    (when command
          (handler-case
            (ecase command
                   (tex (output-fs-in-tex fs))
                   (store (store-as-psort fs)))
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))



;;; **** pop up menus for psorts (called when paths are displayed) *****

;;; display-fs-spec etc are in lexinput.lsp

(defstruct psort-thing value)

(defun display-active-psort (psort ostream)
  (let ( ; (start-pos (current-position ostream))
        (val (make-psort-thing :value psort)))
    (clim:with-text-style (ostream '(nil :bold nil))
        (clim:with-output-as-presentation 
              (ostream val 'psort-thing)
                   (format ostream "~A  " (psort-thing-value val))))))

(define-active-fs-window-command (com-psort-menu)
    ((psort-thing 'psort-thing :gesture :select))
  (let* ((psort (psort-thing-value psort-thing))
         (lex-entry (if psort (get-psort-entry psort))))
        (if lex-entry 
          (pop-up-psort-menu-items psort lex-entry)
          (let ((lex-rule-entry 
                  (get-lex-rule-entry psort)))
            (if lex-rule-entry
               (pop-up-lex-rule-menu-items psort lex-rule-entry))))))


(defun pop-up-psort-menu-items (psort lex-entry)
  (let ((command (clim:menu-choose
                   '(("Psort definition" :value def)
                     ("Expanded psort" :value exp)))))
    (when command
          (handler-case
            (ecase command
                   (def (display-unexpanded-lex-entry psort lex-entry))  
                   (exp (display-fs (lex-or-psort-full-fs lex-entry) 
                           (format nil "~(~A~) - expanded" psort)))) 
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))


(defun pop-up-lex-rule-menu-items (psort rule-entry)
  (declare (ignore psort))
  (let ((command (clim:menu-choose
                   '(("Show rule" :value rule)))))
    (when command
          (handler-case
            (ecase command
                   (rule (display-fs (rule-full-fs rule-entry) 
                         (format nil "~(~A~)" (rule-id rule-entry)))))
            (error (condition)
                   (format clim-user:*lkb-top-stream*  
                           "~%Error: ~A~%" condition))))))

;;;  ***** TeX macros  ******

(defun output-fs-in-tex (fs)
      (when fs
         (let ((file-name 
                  (ask-user-for-new-pathname "File for LaTeX macros?"))) 
            (when file-name
                  (let ((real-name 
                         (merge-pathnames 
                          (make-pathname :directory *lkb-source-dir*)
                          file-name)))
                    (display-dag fs 'tex real-name))))))


;;; ***** Other title menu functions *****

(defun store-as-psort (fs)
   (let ((psort-name 'no-name))
      (when fs
           (setf psort-name 
               (car
               (ask-for-lisp-movable "Current Interaction" 
                  `(("Lex-id?" . ,psort-name))
                  150)))
            (if psort-name
              (or 
               (store-temporary-psort psort-name fs)
               (progn
                 (clim:notify-user clim:*application-frame* 
                                   "Psort name already used")
                 (store-as-psort fs)))))))

           
                  

