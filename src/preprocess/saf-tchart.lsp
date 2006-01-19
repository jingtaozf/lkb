;;; Copyright (c) 2005-2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)

(defvar *saf*)

(defun saf-num-lattice-nodes (saf)
  (length (saf-lattice-nodes (saf-lattice saf))))

(defun saf-setup-morphs (saf)
  (case *morph-option*
    (:with-tokeniser-partial-tree
	(saf-to-tchart saf))
    (t 
     (saf-to-tchart saf
		    :filter #'(lambda (x) (eq (saf-edge-type x) :leaf))))))

(defun xml-maf-to-tchart (xml)
  (saf-to-tchart (xml-to-saf-object xml)))
  
(defun saf-to-tchart (saf &key (filter #'identity)
			       tchart)
  (unless tchart
    (setf *tchart* (make-tchart))
    (setf *tchart-max* 0))
  (setf *saf* saf)
  (unless (member (saf-meta-addressing (saf-meta saf)) '("xpoint" "xchar")
		  :test #'string=)
    (error "Unhandled addressing attribute (~a)" (saf-meta-addressing (saf-meta saf))))
  (saf-lattice-to-edges (saf-lattice saf)
			:filter filter)
  *tchart*)

(defun saf-lattice-to-edges (saf-lattice &key (filter #'identity))
  (loop 
      for e in 
	(loop for f in (saf-lattice-edges saf-lattice)
	    when (funcall filter f)
	    collect f)
      when (eq (saf-edge-type e) :leaf)
      collect e into leafs
      when (eq (saf-edge-type e) :non-leaf)
      collect e into non-leafs
      finally     
	(loop for e in (append leafs non-leafs)
	    do (augment-tchart-from-saf-edge e))))

(defun next-tchart-edge-id (&optional (tchart *tchart*))
  (let ((edges (get-edges tchart)))
    (if edges
	(apply #'max (mapcar #'edge-id edges))
      0)))

;; to do: replace global *tchart* + *tchart-max* + ??? with objects
(defun augment-tchart-from-saf-edge (saf-edge)
  (let* ((edge  (saf-edge-to-edge saf-edge))
	 (from (edge-from edge))
	 (to (edge-to edge))
	 (cc (make-chart-configuration :begin from
				       :end to
				       :edge edge)))
    (setf (aref *tchart* to 0) (push cc (aref *tchart* to 0)))
    (setf (aref *tchart* from 1) (push cc (aref *tchart* from 1)))
    (when (> to *tchart-max*)
      ;(format t "~%WARNING: increasing *tchart-max* to ~a" to)
      (setf *tchart-max* to))
    *tchart*))

(defun saf-edge-to-edge (saf-edge)
  (case (saf-edge-type saf-edge)
    (:leaf (saf-edge-to-tedge saf-edge))
    (:non-leaf (saf-edge-to-medge saf-edge))))

(defun saf-edge-to-tedge (saf-edge)
  (unless (eq :leaf (saf-edge-type saf-edge))
    (error "leaf edge expected"))
  (with-slots (id source target from to content) saf-edge
      (make-token-edge 
       :id (id-to-int id)
       :from (id-to-int source)
       :to (id-to-int target)
       :string content
       :cfrom (point-to-char-point from)
       :cto (point-to-char-point to)
       :word (string-upcase content)
       :leaves (list content))))

(defun saf-edge-to-medge (saf-edge)
  (unless (eq :non-leaf (saf-edge-type saf-edge))
    (error "non-leaf edge expected"))
  ;; assume tedges already in chart
  (with-slots (id source target daughters content form) saf-edge
    (let* ((children 
	    (loop for d in daughters
		collect (id-to-token-edge d (get-tedges *tchart*)))) 
	   (leaf-edges children) ;;fix me
	   )
      (unless (= (id-to-int source) (leaf-edges-from leaf-edges))
	(error "source must match that of leaf edges"))
      (unless (= (id-to-int target) (leaf-edges-to leaf-edges))
	(error "target must match that of leaf edges"))
      (make-morpho-stem-edge 
       :id (id-to-int id)
       :children children
       :leaves (loop for x in leaf-edges collect (edge-string x))
       :from (id-to-int source)
       :to (id-to-int target)
       :string form
       :word (string-upcase form)
       :current (string-upcase form)
       :stem (saf-fs-path-value '("stem") (saf-edge-content saf-edge))
       :partial-tree (partial-tree-from-content (saf-edge-content saf-edge))
       ))))

(defun partial-tree-from-content (content)
  (let ((x (saf-fs-path-value '("partial-tree") content)))
    (if (stringp x)
	(read-from-string x))))

(defun saf-fs-path-value (path fs)
  (cond
   ((null fs)
    nil)
   ((null path)
    fs)
   ((listp fs)
    (saf-fs-path-value 
     (cdr path)
     (saf-fs-feature-value fs (car path))))))

(defun saf-fs-feature-value (fs feature)
  (let ((x (find feature fs 
		 :key #'saf-fv-feature
		 :test #'string=)))
    (if x
	(saf-fv-value x))))

(defun id-to-token-edge (id tedges)
  (find (id-to-int id) tedges :key #'token-edge-id :test #'=))

;; xchar: first char ignored, rest give integer
(defun point-to-char-point (point &optional addressing)
  (unless addressing
    (setf addressing (saf-meta-addressing (saf-meta *saf*))))
  (cond
    ((string= addressing "xchar") (parse-integer (subseq point 1)))
    ((string= addressing "xpoint") -1)))

;; id: first char ignored, rest gives integer
(defun id-to-int (id)
  (let ((i (if id (parse-integer (subseq id 1)))))
    (if i
	(and (setf *edge-id* (max i *edge-id*))
	     i)
      (- (incf *edge-id*)))))
