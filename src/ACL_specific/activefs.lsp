;;; Copyright Ann Copestake 1991-1997 All Rights Reserved.
;;; No use or redistribution without permission.
;;; 

;;; Modified Jan 1995 to avoid unnecessary duplication in code

;;; Port to MCL requires extensive rewrite

;;; rewritten 1996 for Allegro / CLIM

;;; 1997 - merged with YADU version

(in-package :user)

;;; the main entry points are a series of functions with names like
;;; display-basic-fs
;;; these functions create a window. They then call
;;; call display-dag1 (and any other ancilliary display functions)
;;;  these draw the FS into the window and
;;;  have the side effects of making the type names active

;;; This is the font for the pop up menu and the display windows

(def-lkb-parameter *type-font-spec* '(:sans-serif :roman 9))

(def-lkb-parameter *title-font-spec* '(:fix :roman 12))

;;; ***** active types *****

;;; moved from outputfs.lsp to make that Lisp-independent

;;; for ACL it appears that we need things of a particular type for
;;; presentations, and the easiest way of doing this is with a struct

(defstruct type-thing value type-label-list shrunk-p full-tdfs)

;;; YADU - here and below, the slot full-tdfs is needed so that B+C96
;;; style lexical rules can be displayed in the type -> type format
;;; with the full fs associated with full-tdfs

(defun add-type-and-active-fs-region (stream start-pos 
				      type-label-list val shrunk-p atomic-p 
				      &optional top-box full-tdfs)
  (declare (ignore start-pos atomic-p top-box))
  (let ((type-rec
         (make-type-thing :value val
                          :type-label-list type-label-list
                          :shrunk-p shrunk-p
                          :full-tdfs full-tdfs)))
    (clim:with-text-style (stream '(nil :bold nil))
      (clim:with-output-as-presentation 
	  (stream type-rec 'type-thing)
	(write-string (string-downcase val) stream)))))


;;; ***** Records and classes *******

(defstruct fs-display-record 
;;; the record of the FS associated with a window
   fs title paths parents type-fs-display)

;;
;; Define a frame class for our FS windows
;;

(define-lkb-frame active-fs-window 
    ((fs  :initform nil
	  :accessor active-fs-window-fs))
  :display-function 'draw-active-fs
  :width :compute 
  :height :compute
  :output-record (make-instance 'clim:standard-tree-output-history))
    

;;; **** display function entry points ****

(defun display-basic-fs (fs title &optional parents paths output-fs)
  (declare (ignore output-fs))
  (let ((fs-window 
          (clim:make-application-frame 'active-fs-window)))
        (setf (active-fs-window-fs fs-window) 
              (make-fs-display-record :fs fs :title title :paths paths 
                                      :parents parents
				      :type-fs-display *type-fs-display*))
	(setf (clim:frame-pretty-name fs-window) title)
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

;;(defun display-lrule-window (input-tdfs output-tdfs title)
;;  (display-basic-fs input-tdfs title nil nil output-tdfs))


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
           (fudge 20)
           (max-width 0))
      (silica:inhibit-updating-scroll-bars (stream)
        (clim:with-text-style (stream *type-font-spec*)
	  (clim:with-output-recording-options (stream :draw nil :record t)
	    (draw-active-title stream fs title parents paths)
	    (when parents 
	      (setf max-width (+ fudge 
				 (display-active-parents parents stream))))
	    (let ((dag-width (or (if (tdfs-p fs) 
				     (display-dag2 fs 'edit stream)
				   (display-dag1 fs 'edit stream)) 0)))
	      (setf max-width (max (+ fudge dag-width max-width)))
	      (when paths (setf max-width 
			    (max max-width 
				 (+ fudge 
				    (display-active-dpaths paths stream))))))
	    (move-to-x-y stream max-width (current-position-y stream)))))
      (clim:replay (clim:stream-output-history stream) stream))
    stream))

;;; display-dag2 and display-lrule should be in output(td)fs.lsp
;;; rather than activefs.lsp

;;; **** displaying parents and paths ***

(defun display-active-parents (parents ostream)
  ;; this function is dedicated to my Mother and Father
  (format ostream "~%Parents = ")
  (for parent in parents
       do
       (let (;; (start-pos (current-position ostream))
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

(defun draw-active-title (stream fs title parents paths)
  (declare (ignore fs parents paths))
  (clim:with-text-style (stream *title-font-spec*)
    (clim:with-output-as-presentation 
	(stream t 'symbol)
      (format stream "~%~A~%" title))))


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
         (full-tdfs (type-thing-full-tdfs type-thing))
	 (frame clim:*application-frame*))
    (when (and (atom type) type-entry)
      (pop-up-menu
       `(("Hierarchy" :value hier
		      :active ,(getf (class-frames frame) 
				     (find-class 'type-hierarchy)))
	 ("Help" :value help
		 :active ,(type-comment type-entry))
	 ("Shrink/expand" :value shrink)
	 ("Show source" :value source
			:active ,(source-available-p type))
	 ("Type definition" :value def
			    :active 
			    ,(type-constraint type-entry))
	 ("Expanded type" :value exp
			  :active ,(type-constraint type-entry))
	 ("Full structure" :value full
			   :active full-tdfs)
	 ("Select" :value select)
	 ("Unify" :value unify
		  :active ,(and *fs1* (highlighted-class frame))))
       (hier (display-type-in-tree type))
       (help (display-type-comment type (type-comment type-entry)))
       (source (edit-source type))
       (shrink (shrink-fs-action frame
				 (if (type-thing-shrunk-p type-thing)
				     :expand 
				   :shrink)
				 type-label-list))
       (def (show-type-spec-aux type type-entry))
       (exp (show-type-aux type type-entry))
       (full (if full-tdfs
		 (display-fs full-tdfs (format nil "LR constraint"))))
       (select (select-fs frame type-thing))
       (unify (try-unify-fs frame type-thing))))))

;;; ***** pop up menu actions for types ******

(defun shrink-fs-action (window action path)
  (set-dag-display-value (fs-display-record-fs (active-fs-window-fs window))
			 (reverse path)
			 action
			 (fs-display-record-type-fs-display window))
  (clim:redisplay-frame-panes window :force-p t))


(defun display-type-comment (type comment-string &optional parent-stream)
  (declare (ignore parent-stream type))
  (when comment-string
    (with-output-to-top ()
      (format t "~%~A" comment-string))))

;;; *** the title or top pop up menu ****

(define-active-fs-window-command (com-title-fs-menu)
    ((name 'symbol :gesture :select))
  (let ((fs (fs-display-record-fs 
	     (active-fs-window-fs clim:*application-frame*))))
    (pop-up-menu
     `(("Output TeX" :value tex)
       ("Show source" :value source 
		      :active ,(source-available-p name))
       ("Store fs" :value store))
     (tex (output-fs-in-tex fs))
     (source (edit-source name))
     (store (store-as-psort fs)))))
  
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
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))))))


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
	  (with-output-to-top ()
	    (format t "~%Error: ~A~%" condition)))))))

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
	(car (ask-for-lisp-movable "Current Interaction" 
				   `(("Lex-id?" . ,psort-name))
				   150)))
      (when psort-name
	(or (store-temporary-psort psort-name fs)
	    (progn
	      (clim:notify-user clim:*application-frame* 
				"Psort name already used")
	      (store-as-psort fs)))))))

;;; Support for interactive unification check

(defvar *fs1* nil)
(defvar *path1* nil)

(defun frame-dag (frame)
  (let ((fs (fs-display-record-fs (active-fs-window-fs frame))))
    (if (tdfs-p fs)
	(tdfs-indef fs)
      fs)))

(defun select-fs (frame type-thing)
  (let ((sel (find-object (clim:frame-standard-output frame) 
			  #'(lambda (e) 
			      (eq e type-thing)))))
    (setq *fs1* (frame-dag frame))
    (setq *path1* (reverse (type-thing-type-label-list type-thing)))
    (unhighlight-class frame)
    (highlight-objects (clim:output-record-children sel) frame)))

(defun try-unify-fs (frame type-thing)
  (let* ((fs2 (frame-dag frame))
	 (path2 (reverse (type-thing-type-label-list type-thing))))
    (with-output-to-top ()
      (unify-paths-with-fail-messages 
       (create-path-from-feature-list *path1*)
       *fs1*
       (create-path-from-feature-list path2)
       fs2
     ;;; was copied, but shouldn't be necessary
       :selected1 *path1* :selected2 path2)
      (terpri))
    (setq *fs1* nil)
    (unhighlight-class frame)))

;;; **********************************************************************

(defstruct pointer
  label valuep)

(defun add-active-pointer (stream position pointer valuep)
  (declare (ignore position))
  (clim:with-output-as-presentation (stream (make-pointer :label pointer
							  :valuep valuep)
					    'pointer)
    (format stream "<~A>" pointer))
  (when valuep
    (write-string " = " stream)))

(define-active-fs-window-command (com-pointer-menu)
    ((pointer 'pointer :gesture :select))
  (clim:with-application-frame (frame)
    (pop-up-menu
     (append (unless (pointer-valuep pointer)
	       '(("Find value" :value value)))
	     '(("Find next" :value next)))
     (value (let ((sel (clim:output-record-children
			(find-object (clim:frame-standard-output frame) 
				     #'(lambda (p) 
					 (and (pointer-p p)
					      (and (eql (pointer-label pointer)
							(pointer-label p))
						   (pointer-valuep p))))))))
	      (scroll-to (car sel) (clim:frame-standard-output frame))
	      (setq *fs1* nil)
	      (highlight-objects sel frame)))
     (next))))
