(in-package :lkb)

;;; MRS related output functions to clean up MRS-less LKB



;;; generator output functions which are not dialect specific

#-tty
(defun show-gen-result nil
   (if *gen-record*
      (draw-active-list
         (sort
            (mapcar
               #'(lambda (edge)
                   (cons (format nil "~{~A~^ ~}" 
                                 (fix-spelling (g-edge-leaves edge)))
                      edge))
               *gen-record*)
            #'string-lessp :key #'car)
         "Generated Sentences"
         (list
            (cons "Feature structure"
               #'(lambda (edge)
                   (display-fs (g-edge-dag edge)
                      (format nil "Edge ~A G - Tree FS" (g-edge-id edge)))))
            (cons "Edge"
               #'(lambda (edge)
                   (display-parse-tree edge nil)))))
      (format t "~%No strings generated")))

#|     
      (let ((possible-edge-name
               (ask-for-lisp-movable "Current Interaction" 
                  `(("No generation results - specify an edge number" . ,*edge-id*)) 60)))
         (when possible-edge-name
            (let* ((edge-id (car possible-edge-name))
                   (edge-record (find-gen-edge-given-id edge-id)))
               (when edge-record 
                  (display-parse-tree edge-record nil)))))))
|#

#-tty
(defun show-gen-edge nil
  (let ((possible-edge-name
         (with-package (:lkb)
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

#-tty
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
         (let ((chart-index (string-downcase (symbol-name (car entry)))))
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

#-tty
(defun generate-from-edge nil
  (let ((possible-edge-name 
         (with-package (:lkb)
           (ask-for-lisp-movable 
            "Current Interaction" 
            `(("Parser edge number for input MRS?" 
               . ,(or *last-generate-from-edge* *edge-id*))) 60))))
    (when possible-edge-name
      (setf *last-generate-from-edge* (car possible-edge-name))
      (let ((parser-edge (find-edge-given-id (car possible-edge-name))))
	(if parser-edge
	    (really-generate-from-edge parser-edge)
	  (format t "~%No parser edge ~A" (car possible-edge-name)))))))

#-tty
(defun really-generate-from-edge (parser-edge)    
  (let* ((input-sem (mrs::extract-mrs parser-edge t)))
    ;; t indicates that this is being run from the generator and that
    ;; the appropriate globals should be set
    (with-output-to-top ()
      (if (and input-sem (mrs::psoa-p input-sem)
               (mrs::psoa-liszt input-sem))
	  (progn
	    (close-existing-chart-windows)
	    (generate-from-mrs input-sem)
	    (show-gen-result))
	(format t "~%Could not extract valid MRS from edge ~A"
		(edge-id parser-edge))))))

#-tty
(defun choose-mrs-output-level nil
  (let ((output-level 
         (ask-user-for-multiple-choice 
          "MRS output?" 
          :none
          :base
          :scoped)))
    (case output-level
      (:none (setf mrs::*mrs-output-p* nil)
             (setf mrs::*mrs-scoping* nil))
      (:base (setf mrs::*mrs-output-p* t)
             (setf mrs::*mrs-scoping* nil))
      (:scoped (setf mrs::*mrs-output-p* nil)
             (setf mrs::*mrs-scoping* t)))))

                                                    


;;; tty version

;;; (make-menu-item :name "Generate..."
;;;                        :value 'generate-from-edge)

(defun do-generate-tty (&optional edge-name)
   (let ((possible-edge-name 
            (or edge-name *last-generate-from-edge* *edge-id*)))
      (when possible-edge-name
         (setq *last-generate-from-edge* edge-name)
         (let ((parser-edge (find-edge-given-id possible-edge-name)))
            (if parser-edge
               (let* ((input-sem
                       (mrs::extract-mrs parser-edge t)))
   ;; t indicates that this is being run from the generator and that
   ;; the appropriate globals should be set
                  (if (mrs::psoa-liszt input-sem)
                     (progn
                        (format t "~&Generating from parser edge ~A" possible-edge-name)
                        (generate-from-mrs input-sem)
                        (show-gen-result-tty))
                     (format t "~&Could not extract any MRS relations from edge ~A"
                        possible-edge-name)))
               (format t "~&No parser edge ~A" possible-edge-name))))))

(defun show-gen-result-tty nil
   (if *gen-record*
      (for edge in *gen-record*
         do
         (format t "~&Edge ~A G:" (edge-id edge))
         (pprint (parse-tree-structure edge)))
      (format t "~&No strings generated")))


