;;; Copyright (c) 1991-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see licence.txt for conditions


(in-package :lkb)

;;; Record source locations for things, and provide an interface to
;;; display sources in emacs

(defun record-source (type stream position)
   (declare (ignore position))
   (setf (excl:source-file type :lkb) (excl::filename stream)))

(defun edit-source (thing)
  (let ((source (ignore-errors (excl:source-file thing :lkb))))
    (when source
      (edit-file thing source))))

(defun source-available-p (thing)
  (and (lep:lep-is-running)
       (ignore-errors (excl:source-file thing :lkb))))

(defun edit-file (thing file)
  (when (lep:lep-is-running)
    (lep::eval-in-emacs (format nil "(find-tdl-definition \"~a\" \"~a\")"
				thing 
				#+:mswindows
				(convert-backslashes-in-filename
				 (format nil "~A" file))
				#-:mswindows file))))

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
  (setf *syntax-error* nil)
  (with-output-to-top ()
    (let ((*readtable* (make-tdl-break-table)))
      (with-input-from-string (istream definition)
	(read-tdl-type-stream istream t)))
    (unless *syntax-error* 
      (if *amend-error*
          (setf *amend-error* nil)
	(when (patch-type-table) 
	  (canonicalise-feature-order)           
	  (set-up-type-interactions)
	  (format t "~%Done.")
	  t)))))

;; Functions to manipulate emacs menu bar

(defun add-lkb-menu (menu)
  (lep::eval-in-emacs-query :string (format nil "(fi::install-menubar '~A)"
					    menu)))


#|

;; Use an emacs buffer as top-level window
b
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

