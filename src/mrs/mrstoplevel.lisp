;;; Copyright (c) 1998-2003 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package :lkb)

;;; MRS related output functions to clean up MRS-less LKB



;;; generator output functions which are not dialect specific

(defun show-gen-result nil
  (if #+:lui (lui-status-p :realization) #-:lui nil
    (lui-show-gen-result)
    #+:tty
    (show-gen-result-tty)
    #-:tty
     (if *gen-record*
        (draw-active-list
           (sort
              (mapcar
                 #'(lambda (edge)
                     (cons (if (stringp (edge-string edge))
                             (edge-string edge)
                             (format
                              nil
                              "~{~A~^ ~}" 
                              (fix-spelling (g-edge-leaves edge))))
                        edge))
                 *gen-record*)
              #'string-lessp :key #'car)
           "Generated Sentences"
           (list
              (cons "Feature structure"
                 #'(lambda (edge)
                     (display-fs (g-edge-dag edge)
                        (format nil "Edge ~A G - Tree FS" (g-edge-id edge)))))
              (cons "Tree"
                 #'(lambda (edge)
                     (display-parse-tree edge nil)))
              (cons "MRS"
                  #'(lambda (edge)
                      (show-mrs-window edge)))))
        (show-message-window "No strings generated"))))


#-:tty
(defun show-generator-input ()
  (when *generator-input*
    #+:mrs
    (mrs::browse-mrs
     *generator-input* "Generator Input MRS" :display :indexed)))

#-:tty
(defun show-generator-internal-mrs ()
  (when *generator-internal-mrs*
    #+:mrs
    (mrs::browse-mrs
     *generator-internal-mrs* "Generator Internal MRS" :display :indexed)))

#-:tty
(defun show-gen-edge (&optional id)
  (let ((possible-edge-name
         (if (numberp id)
           (list id)
           (ask-for-lisp-movable "Current Interaction" 
            `(("Specify an edge number" . ,*edge-id*)) 60))))
      (when possible-edge-name
         (let* ((edge-id (car possible-edge-name))
                (edge-record (find-gen-edge-given-id edge-id)))
            (when edge-record 
               (display-parse-tree edge-record t))))))


(defun find-gen-edge-given-id (edge-id)
   (dolist (entry *gen-chart*)
      (dolist (edge (append (cadr entry) (cddr entry)))
         (when (eql edge-id (edge-id edge))
            (return-from find-gen-edge-given-id edge)))))


;;; Graphical display of generator chart (show-gen-chart) (show-gen-chart t)

#-:tty
(defun show-gen-chart (&optional all-p) 
   (if *gen-chart*
      (let ((root (make-symbol "")))
         (setf (get root 'root) t)
         (create-gen-chart-pointers root all-p)
         (draw-chart-lattice root
            (format nil "Generator Chart (~A edges)" (if all-p "all" "inactive")))
         root)
      (lkb-beep)))
      


(defun create-gen-chart-pointers (root all-p)
   ;; create a global mapping from edge-ids to symbols, not interned - so we don't
   ;; end up hanging on to old edges
   (let ((edge-symbols nil))
      (dolist (entry *gen-chart*)
         (dolist (e (append (cadr entry) (cddr entry)))
            (push
               (list* (dotted-edge-id e)
                  (make-edge-symbol (dotted-edge-id e))
                  (dotted-edge-needed e))
               edge-symbols)))
      (dolist (entry *gen-chart*)
         (let ((chart-index (string-downcase (string (car entry)))))
            (dolist (e (append (cadr entry) (cddr entry)))
               (let ((edge-symbol
                        (cadr (assoc (dotted-edge-id e) edge-symbols))))
                  (setf (get edge-symbol 'chart-edge-span)
                     (if (dotted-edge-needed e)
                        (concatenate 'string chart-index " A") chart-index))
                  (setf (get edge-symbol 'chart-edge-contents) e)
                  (if (dotted-edge-children e)
                     (dolist (c (dotted-edge-children e))
                        (when c
                           (push edge-symbol
                              (get (cadr (assoc (dotted-edge-id c) edge-symbols))
                                 'chart-edge-descendents))))
                     (push edge-symbol (get root 'chart-edge-descendents)))))))
      (unless all-p
         ;; remove intermediate links consisting of active edges
         (dolist (pair edge-symbols)
            (setf (get (cadr pair) 'chart-edge-descendents)
               (create-gen-chart-pointers-collapse
                  (get (cadr pair) 'chart-edge-descendents)
                  edge-symbols))))))


(defun create-gen-chart-pointers-collapse (nodes edge-symbols)
   (mapcan
      #'(lambda (node)
          (if (cddr (find node edge-symbols :key #'cadr))
             (create-gen-chart-pointers-collapse
                (get node 'chart-edge-descendents) edge-symbols)
             (list node)))
      nodes))



;;; from toplevel.lsp

;;; "Generate"
;;;
;;; "Generate" generate-from-edge


(defparameter *last-generate-from-edge* nil)

#-:tty
(defun generate-from-edge nil
  (let ((possible-edge-name 
         (ask-for-lisp-movable 
          "Current Interaction" 
          `(("Parser edge number for input MRS?" 
             . ,(or *last-generate-from-edge* *edge-id*))) 60)))
    (when possible-edge-name
      (setf *last-generate-from-edge* (car possible-edge-name))
      (let ((parser-edge (find-edge-given-id (car possible-edge-name))))
	(if parser-edge
	    (really-generate-from-edge parser-edge)
	  (show-message-window (format nil "No parser edge ~A" (car possible-edge-name))))))))

#-:tty
(defun really-generate-from-edge (parser-edge)    
  (declare (special *dmrs-grammar-p*))
  (let* ((input-sem (if *dmrs-grammar-p*
			(mrs::extract-dmrs parser-edge)
			(mrs::extract-mrs parser-edge))))
    (with-output-to-top ()
      (cond 
       (*dmrs-grammar-p* 
	(show-message-window
	 (format nil "Generation from native DMRS not implemented yet")))
	#|
	     (when (and input-sem 
		     (mrs::dmrs-nodes input-sem))
	       (close-existing-chart-windows)
	     (generate-from-dmrs input-sem)
	     (show-gen-result)))
	     |#
	    ((and input-sem (mrs::psoa-p input-sem)
               (mrs::psoa-liszt input-sem))
	     (close-existing-chart-windows)
	     (generate-from-mrs input-sem)
	     (show-gen-result))
	    (t (show-message-window
		(format nil "Could not extract valid *MRS from edge ~A"
			(edge-id parser-edge))))))))

#-:tty
(defun toggle-mrs-base nil
  (setf mrs::*mrs-base-output-p* (not mrs::*mrs-base-output-p*)))

(defun toggle-mrs-scoping nil
  (setf mrs::*mrs-scoping-output-p* (not mrs::*mrs-scoping-output-p*)))

(defun toggle-rmrs-xml nil
  (setf mrs::*rmrs-xml-output-p* (not mrs::*rmrs-xml-output-p*)))

(defun toggle-rmrs nil
  (setf mrs::*rmrs-compact-output-p* (not mrs::*rmrs-compact-output-p*)))
                                                    
;;; logic of choose-mrs-output-level was getting horrible
;;; and it didn't work on Windows (XP) anyway
;;;
;;; new functionality means you can have as many (or as few)
;;; types of output as you like

;;; tty version

;;; (make-menu-item :name "Generate..."
;;;                        :value 'generate-from-edge)

(defun do-generate-tty (&optional edge-name debug-p)
   (let ((possible-edge-name 
            (or edge-name *last-generate-from-edge* *edge-id*)))
      (when possible-edge-name
         (setq *last-generate-from-edge* edge-name)
         (let ((parser-edge (find-edge-given-id possible-edge-name)))
            (if parser-edge
               (let* ((input-sem
                       (mrs::extract-mrs parser-edge)))
                  (if (mrs::psoa-liszt input-sem)
                     (progn
                        (format t "~&Generating from parser edge ~A" possible-edge-name)
                        (if debug-p (generate-from-mrs-internal input-sem)
			  (generate-from-mrs input-sem))
                        (show-gen-result-tty))
                     (format t "~&Could not extract any MRS relations from edge ~A"
                        possible-edge-name)))
               (format t "~&No parser edge ~A" possible-edge-name))))))

(defun show-gen-result-tty nil
  (if *gen-record*
      (loop for edge in *gen-record*
	  do
	    (format t "~&Edge ~A G:" (edge-id edge))
	    (pprint (edge-string edge));;bmw -- display generated STRING
	    (pprint (parse-tree-structure edge))
	  finally
	    (force-output) ;;bmw -- ensure we see the output
	    (terpri)) ;; bmw -- ensure we end on a new line
    (format t "~&No strings generated")))


