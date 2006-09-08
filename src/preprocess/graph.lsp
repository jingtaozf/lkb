;; Copyright (c) 2006
;;;   Ben Waldron;
;;; see `licence.txt' for conditions.

(in-package :lkb)



;; INEFFICIENT. never mind.
;; return all edge paths between node-x and node-y
(defun get-edge-paths-x2y (node-x node-y &key back-array)
  (unless back-array
    (setf back-array (get-paths-x2y node-x node-y)))
  (if (equalp node-x node-y)
      (list nil)
    (loop
	with sources = (aref back-array node-y)
	for source in sources
	for edge-paths = (get-edge-paths-x2y node-x source
					     :back-array back-array)
	for edges = (get-tedges-source-target source node-y)
	append
	  (loop
	      for edge-path in edge-paths
	      append
		(loop
		    for edge in edges
		    collect (append edge-path (list edge)))))))

;; return set of edges on paths from node-x to node-y
(defun get-edges-x2y (node-x node-y)
  (loop
      with array = (get-paths-x2y node-x node-y)
      for target from 0 to (1- (array-dimension array 0))
      for sources = (aref array target)
      append
	(loop for source in sources
	    append (get-tedges-source-target source target))))

;; return token edges spanning source to target
(defun get-tedges-source-target (source target)
  ;; FIXME: inefficient
  (intersection
   (get-tedges-source source)
   (get-tedges-target target)))

;; return token edges outgoing from source node
(defun get-tedges-source (source)
  (loop
      for cc in (aref *tchart* source 1)
      for edge = (chart-configuration-edge cc)
      when (token-edge-p edge)
	   collect edge))

;; return token edges ingoing to target node
(defun get-tedges-target (target)
  (loop
      for cc in (aref *tchart* target 0)
      for edge = (chart-configuration-edge cc)
      when (token-edge-p edge)
	   collect edge))

;; true if exists path of morph edges spanning tchart
(defun medge-spanning-path-p nil
  (aref (get-paths-x2y 0 *tchart-max* 
		       :filter #'morpho-stem-edge-p) *tchart-max*))

;; true if exists path of morph edges in tchart from x to y
(defun medge-path-x2y-p (x y)
  (aref (get-paths-x2y x y
		       :filter #'morpho-stem-edge-p)
	y))

(defun medge-spanned-p (source target)
  (or
   (medge-path-x2y-p source target)
   ;; [bmw] fixme: implement graphs functions cleanly
   (loop
       for cc in (aref *tchart* target 1) ;;out 
       for edge = (chart-configuration-edge cc)
       for target2 = (edge-to edge)
       thereis (medge-path-x2y-p source target2)
	       )))

;;; add 1-paths from node-z to agenda
(defun update-paths-x2y-agenda (node-z agenda &key (filter #'identity))
  (loop
      for cc in (aref *tchart* node-z 1) ;;out 
      for edge = (chart-configuration-edge cc)
      for source = (edge-from edge)
      for target = (edge-to edge)
      when (funcall filter edge)
      do (pushnew (cons source target) agenda
		  :test #'equalp))
  agenda)

;; return array defining paths from node-x to node-y
(defun get-paths-x2y (node-x node-y &key (filter #'identity))
  (let* (;; create array to store paths from x
	 (paths-from-x (make-array (list (1+ *tchart-max*)) :initial-element nil))
	 ;; initialise agenda
	 agenda)
    (unless (= node-x node-y)
      (setf agenda (update-paths-x2y-agenda node-x nil :filter filter))
      ;; process agenda items...
      (loop 
	  with processed = nil
	  while agenda
		;; next item
	  for item = (pop agenda)
	  for source = (car item)
	  for target = (cdr item)
	  unless (member item processed :test #'equalp)
	  do
	    ;(format t "~&item  ~a" item)
	    ;; update array
	    (setf (aref paths-from-x target)
	      (cons
	       source
	       ;;(cons (aref paths-from-x source) target)
	       (aref paths-from-x target)))
	    (unless (= target node-y)
	      ;; no loops, so no need to look further
	      (setf agenda
		(update-paths-x2y-agenda target agenda :filter filter)))
	    (push item processed)))
    ;; pick out result
    paths-from-x))
