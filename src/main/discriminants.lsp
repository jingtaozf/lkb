;;; Copyright (c) 1998--2002.
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :lkb)

(def-lkb-parameter *tree-use-node-labels-p* nil)

(def-lkb-parameter *tree-discriminants-mode* :classic)

(def-lkb-parameter *tree-discriminants-separator* " || ")

(def-lkb-parameter *tree-results-show* :tree)

(defvar *tree-discriminants-chart* nil)

(defstruct discriminant
  type key value start end in out top toggle state hidep
  time record preset gold)

(defun find-discriminants (edges &key (mode *tree-discriminants-mode*))
  
  (let* ((n (if (edge-p (first edges)) 
              (+ (loop
                     for edge in edges
                     maximize (edge-to edge))
                 1)
              0))
         (*tree-discriminants-chart* (make-array (list n n)))
         %discriminants%)
    (declare (special %discriminants%))
    ;;
    ;; first, extract all elementary properties that can possibly discriminate
    ;; parses.
    ;;
    (loop
        for edge in edges
        do (extract-discriminants-from-edge edge edge :mode mode))
    ;;
    ;; then, filter out discriminants that either (i) all parses have or (ii) 
    ;; are implied by other discriminants, based on `size' (i.e. span), for
    ;; the time being. 
    ;;
    (setq %discriminants%
      (loop
          with nparses = (length edges)
          for foo in %discriminants%
          for type = (discriminant-type foo)
          for in = (discriminant-in foo)
          for match = (or (= (length in) nparses)
                          (when (eq type :constituent)
                            (loop
                                with span = (- (discriminant-end foo)
                                               (discriminant-start foo))
                                for bar in %discriminants%
                                thereis 
                                  (and (eq (discriminant-type bar) 
                                           :constituent)
                                       (< span 
                                          (- (discriminant-end bar)
                                             (discriminant-start bar)))
                                       (equal (discriminant-in bar) in)))))
          unless match collect foo))
    ;;
    ;; compute out parses from in parses
    ;;
    (loop
        for foo in %discriminants%
        for out = (set-difference edges (discriminant-in foo) :test #'eq)
        do
          (setf (discriminant-out foo) out))
    
    ;;
    ;; sort in order of yield, with semantic discriminants at end
    ;;
    (setf %discriminants%
      (sort %discriminants%
            #'(lambda (a b)
                (if (eq mode :classic)
                  (let* ((astart (discriminant-start a))
                         (aend (discriminant-end a))
                         (aspan (- aend astart))
                         (bstart (discriminant-start b))
                         (bend (discriminant-end b))
                         (bspan (- bend bstart)))
                    (if (= aspan bspan)
                      (if (= astart bstart)
                        (or (and (eq (discriminant-type a) :constituent)
                                 (or (eq (discriminant-type b) :type)
                                     (eq (discriminant-type b) :relation)))
                            (and (eq (discriminant-type a) :type)
                                 (eq (discriminant-type b) :relation)))
                        (< astart bstart))
                      (> aspan bspan)))
                  (let* ((keya (discriminant-key a))
                         (keyb (discriminant-key b)))
                    (if (not (char= (char keya 0) #\_))
                      (or (char= (char keyb 0) #\_) (string< keya keyb))
                      (when (char= (char keyb 0) #\_)
                        (string< keya keyb))))))))))

(defun extract-discriminants-from-edge (edge top &key (mode :classic))
  
  (if (eq mode :classic)
    (let* ((tdfs (edge-dag edge))
           (yield 
            (if (edge-children edge)
              (format 
               nil 
               "~{~a~^~a~}" 
               (loop
                   with children = (edge-children edge)
                   with last = (first (last children))
                   for edge in children
                   nconc 
                     (list 
                      (format nil "~{~a~^ ~}" (edge-leaves edge))
                      (if (eq edge last)
                        ""
                        *tree-discriminants-separator*))))
              (format nil "~{~a~^ ~}" (edge-leaves edge))))
           (label (when *tree-use-node-labels-p* (edge-label edge)))
           (rule (edge-rule edge))
           (start (edge-from edge))
           (end (edge-to edge)))
      ;;
      ;; for all constituents, always record node label (when available).
      ;; _fix_me_
      ;; a separate :label type would be cleaner here.         (13-oct-02; oe)
      ;;
      (when label
        (add-discriminant label yield :constituent top start end))
      (cond
       ((stringp rule)
        ;;
        ;; a leaf of the derivation, i.e. a lexical entry: record preterminal 
        ;; type and, when available, key semantic relation.
        ;; 
        (add-discriminant 
         (format nil "~(~a~)" (type-of-fs (tdfs-indef tdfs))) 
         yield :type top start end)
        (when (and tdfs *discriminant-path*)
          (let* ((dag (existing-dag-at-end-of 
                       (tdfs-indef tdfs) *discriminant-path*))
                 (type (and dag (type-of-fs dag))))
            (when type
              (add-discriminant 
               (format nil "~(~a~)" type)
               rule :relation top start end)))))
       ((rule-p rule)
        ;;
        ;; all other nodes of the derivation: record rule identifier.
        ;;
        (add-discriminant 
         (symbol-name (rule-id rule)) yield :constituent top start end))
       ((and (null rule) (edge-category edge))
        (add-discriminant 
         (string (edge-category edge)) yield :constituent top start end)))
      (loop
          for child in (edge-children edge)
          do (extract-discriminants-from-edge child top :mode mode))
      (when (edge-morph-history edge)
        (extract-discriminants-from-edge 
         (edge-morph-history edge) top :mode mode)))
    #+:mrs
    (let* ((eds (mrs::ed-convert-edge edge))
           (triples (mrs::ed-explode eds)))
      (loop
          for triple in triples
          for key = (format nil "~{~a~^ ~}" triple)
          do
            (add-discriminant key nil :ed top nil nil)))))

(defun add-discriminant (key value type top start end)

  (declare (special %discriminants%))

  (let ((old (find-discriminant :type type :key key :start start :end end)))
    (if old
      ;;
      ;; _fix_me_
      ;; this used to be pushnew() and take a lot of time; for a given .top.,
      ;; it will be hard (though, unfortunately, possible in theory) to find
      ;; multiple equivalent (i.e. in terms of type, key, start, and end)
      ;; discriminants that characterize that edge.  multiple occurances of the
      ;; same unary rule at the same position (discharging several optional
      ;; complements, say) could be one such case, though.  however, for that
      ;; to work, we will need to elaborate the notions of discriminats already
      ;; because the old-style code would only provide a single occurance of
      ;; that discriminant and, thus, be insufficient to fully disambiguate.  a
      ;; quick-and-dirty way of ruling out these cases, at least, would be to
      ;; keep track of which tree gave rise to the .old. discriminant (e.g. by
      ;; virtue of an additional :top slot), and then signal an error or just
      ;; ignore it (in compliance to old-style code :-).        (16-oct-02; oe)
      ;;
      (unless (eq (discriminant-top old) top)
        (push top (discriminant-in old)))
      (let ((discriminant (make-discriminant :key key :value value
                                             :in (list top) :out nil
                                             :type type :top top
                                             :start start :end end
                                             :toggle :unknown 
                                             :state :unknown)))
        (push discriminant %discriminants%)
        (when (and (integerp start) (integerp end))
          (push discriminant (aref *tree-discriminants-chart* start end)))))))

(defun find-discriminant  (&key key value type start end)
  
  (declare (ignore value) (special %discriminants%))
  
  (loop
      with candidates = (if (and (integerp start) (integerp end))
                          (aref *tree-discriminants-chart* start end)
                          %discriminants%)
      for discriminant in candidates
      thereis (and (equal (discriminant-key discriminant) key)
                   (equal (discriminant-type discriminant) type)
                   (equal (discriminant-start discriminant) start)
                   (equal (discriminant-end discriminant) end)
                   discriminant)))

(defun preset-discriminants (discriminants preset &optional gold)
  
  (let (lead)
    (loop
        for old in gold
        for new = (loop 
                     for new in discriminants
                     thereis (when (discriminant-equal old new) new))
        when new do
          (setf (discriminant-toggle new) (discriminant-toggle old))
          (setf (discriminant-state new) (discriminant-state old))
          (setf (discriminant-gold new) old)
        else do (push old lead))
    (loop
        for new in discriminants
        for old = (unless (discriminant-gold new)
                    (loop 
                        for old in preset
                        thereis (and (discriminant-equal old new) old)))
        when old do
          (setf (discriminant-toggle new) (discriminant-toggle old))
          (setf (discriminant-state new) (discriminant-state old))
          (setf (discriminant-preset new) old))
    lead))

(defun update-discriminants (discriminants top toggle)
  (loop
      for foo in discriminants
      when (or (and toggle (member top (discriminant-in foo) :test #'eq))
               (and (null toggle)
                    (eq top (first (discriminant-out foo)))
                    (null (rest (discriminant-out foo)))))
      do
        (when (null (discriminant-toggle foo))
          (setf (discriminant-toggle foo) :unknown)
          (setf (discriminant-time foo) (get-universal-time)))
        (setf (discriminant-state foo) t)
      when (or (and toggle (member top (discriminant-out foo) :test #'eq))
               (and (null toggle)
                    (eq top (first (discriminant-in foo)))
                    (null (rest (discriminant-in foo)))))
      do
        (when (eq (discriminant-toggle foo) t)
          (setf (discriminant-toggle foo) :unknown)
          (setf (discriminant-time foo) (get-universal-time)))
        (setf (discriminant-state foo) nil)))

(defun discriminant-equal (foo bar)
  (and (or (null (discriminant-key foo)) (null (discriminant-key bar))
           (equal (discriminant-key foo) (discriminant-key bar)))
       (or (null (discriminant-type foo)) (null (discriminant-type bar))
           (equal (discriminant-type foo) (discriminant-type bar)))
       (or (null (discriminant-start foo)) (null (discriminant-start bar))
           (equal (discriminant-start foo) (discriminant-start bar)))
       (or (null (discriminant-end foo)) (null (discriminant-end bar))
           (equal (discriminant-end foo) (discriminant-end bar)))))

(defun discriminant-state-as-string (discriminant)
  (let ((state (discriminant-state discriminant)))
    (cond ((eq state t) "+") ((null state) "-") (t "?"))))

(defun discriminant-toggle-as-string (discriminant)
  (let ((toggle (discriminant-toggle discriminant)))
    (cond ((eq toggle t) "+") ((null toggle) "-") (t "?"))))
