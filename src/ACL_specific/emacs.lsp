;;; Copyright (c) 1991-2017 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions


(in-package :lkb)

;;; Record source locations for things, and provide an interface to
;;; display sources in emacs. In allegro, use built-in source recording
;;; and lep facility. Otherwise, code it from scratch and assume we have
;;; SWANK available (although possibly not currently running).

(defun record-source (type stream position)
   (declare (ignore position))
   #+:allegro
   (setf (excl:source-file type :lkb) (excl::filename stream))
   #-:allegro
   (when (typep stream 'file-stream)
     (setf (get type 'type-source-file) (pathname stream))))

(defun edit-source (thing)
  (let ((source
          #+:allegro (ignore-errors (excl:source-file thing :lkb))
          #-:allegro (get thing 'type-source-file)))
    (when source
      (edit-file thing source))))

(defun source-available-p (thing)
  #+:allegro
  (and (lep:lep-is-running)
       (ignore-errors (excl:source-file thing :lkb)))
  #-:allegro
  (and (or swank::*emacs-connection* (swank::default-connection))
       (get thing 'type-source-file)))

(defun edit-file (thing file)
  #+:allegro
  (when (lep:lep-is-running)
    (lep::eval-in-emacs (format nil "(find-tdl-definition \"~a\" \"~a\")"
				thing 
				#+:mswindows
				(convert-backslashes-in-filename
				 (format nil "~A" file))
				#-:mswindows file)))
  ;; eval-in-emacs requires (setq slime-enable-evaluate-in-emacs t) on the emacs side
  #-:allegro
  (flet
    ((edit-fn ()
      (swank:eval-in-emacs
        `(find-tdl-definition
           ,(string thing) ,(namestring (get thing 'type-source-file))))))
    (cond
      (swank::*emacs-connection* (funcall #'edit-fn))
      ((swank::default-connection)
        (swank::with-connection ((swank::default-connection))
          (funcall #'edit-fn))))))


(defun convert-backslashes-in-filename (str)
  ;;; apparently needed for Windoze because the emacs interface
  ;;; can't cope with the backslashes in the filename - doesn't
  ;;; recognise them as escaped.  However, emacs accepts the
  ;;; *nix forward slash syntax, so ...
  ;;; e.g.
  ;;; "D:\\lingo\\lkb\\src\\data\\esslli2k\\grammars\\grammar1\\types.tdl"
  ;;; to
  ;;; "D:/lingo/lkb/src/data/esslli2k/grammars/grammar1/types.tdl"
  (let ((char-list (coerce str 'list))
	(res nil))
    (loop (unless char-list (return))
      (let ((first (car char-list)))
	(setf char-list (cdr char-list))
	(if (eql first #\\)
	    (progn
	      (when (eql (car char-list) #\\)
		(setf char-list (cdr char-list)))
	      (push #\/ res))
	  (push first res))))
    (coerce (nreverse res) 'string)))
    

;; Redefine a single type

(defun redefine-type (definition)
  (setq *syntax-error* nil)
  (let ((*readtable* (make-tdl-break-table)))
    (with-input-from-string (istream definition)
      (read-tdl-type-stream istream t)))
  (unless *syntax-error* 
    (if *amend-error*
      (setq *amend-error* nil)
      (when (patch-type-table) 
	(canonicalise-feature-order)           
	(set-up-type-interactions)
	(format t "~%Done")
	t))))

;; Functions to manipulate emacs menu bar

(defun add-lkb-menu (menu)
  #+:allegro
  (lep::eval-in-emacs-query :string (format nil "(fi::install-menubar '~A)"
					    menu))
  #-:allegro
  (error "~S not implemented" 'add-lkb-menu))


#|

;; Use an emacs buffer as top-level window

(defun make-lkb-stream ()
  (let ((s nil))
    (flet ((foo (stream)
	     (declare (ignore stream))
	     (or s (setq s (make-lkb-listener-stream)))))
      (let ((s (make-instance 'excl::lazy-encapsulating-stream
		 :init-in #'foo
		 :init-out #'foo)))
	(setf (excl::interactive-stream-p s) t)
	s))))

(defun make-lkb-listener-stream ()
  (let ((s (lep:make-editor-listener-stream :splitp t :name "lkb")))
    (setf (excl::interactive-stream-p s) t)
    s))

|#

;;; Generation and MRS/RMRS from emacs

(defun generate-rmrs-from-emacs (str)
  (with-package (:mrs)
    (let ((rmrs (mrs::read-single-rmrs-from-string str)))
      (when (and rmrs (mrs::rmrs-p rmrs))
	(mrs::generate-from-rmrs rmrs)))))

