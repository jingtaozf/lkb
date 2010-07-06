;;; Copyright (c) 1998--2002.
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `LICENSE' for conditions.

(in-package :lkb)

(def-lkb-parameter *tree-use-node-labels-p* nil)

(def-lkb-parameter *tree-discriminants-mode* :classic)

(def-lkb-parameter *tree-discriminants-separator* " || ")

(def-lkb-parameter *tree-results-show* :tree)

(defparameter *tree-discriminants-skews* nil #+:null '("," "(" ")" "."))

(defparameter *tree-discriminants-blaze* nil)

(defvar *tree-discriminants-chart* nil)

(defstruct discriminant
  type key value start end in out top toggle state hidep
  time record preset gold blaze tag)

(defun find-discriminants (edges 
                           &key (mode *tree-discriminants-mode*)
                                (blaze *tree-discriminants-blaze*)
                                tags)
  (let* ((n (if (edge-p (first edges)) 
              (+ (loop
                     for edge in edges
                     when (numberp (edge-to edge))
                     maximize (edge-to edge))
                 1)
              0))
         (*tree-discriminants-chart* 
          (make-array (list n n) :initial-element nil))
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
    ;; in case we have a `blaze' (mapping to prime strong trees, according to
    ;; what we have observed in earlier corpus exposure) attempt to prime the
    ;; (lexical) discriminants that match the blaze, which can be a positive
    ;; or negative match.
    ;;
    (when blaze
      (loop
          with n = (length tags)
          for discriminant in %discriminants%
          for start = (discriminant-start discriminant)
          for end = (discriminant-end discriminant)
          for tag = (when (and (numberp start) (numberp end)
                               (= (+ start 1) end) (< end n)
			       (eq (discriminant-type discriminant) :type))
                      (nth start tags))
          when tag do (blaze-discriminant discriminant tag blaze)))
    
    ;;
    ;; then, filter out discriminants that either (i) all parses have or (ii) 
    ;; are implied by other discriminants, based on `size' (i.e. span), for
    ;; the time being. 
    ;;
    (setf %discriminants%
      (loop
          with nparses = (length edges)
          for foo in %discriminants%
          for type = (discriminant-type foo)
          for in = (discriminant-in foo)
          for match = (or (= (length in) nparses)
                          (when (and (eq type :constituent)
				     (null (discriminant-toggle foo)))
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

    #+:null
    (format 
     t 
     "find-discriminants(): ~a discriminants of ~a blazed~%"
     (loop
         for discriminant in %discriminants%
         when (discriminant-toggle discriminant) count 1)
     (length %discriminants%))

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
          do (extract-discriminants-from-edge child top :mode mode)))
    #+:mrs
    (let* ((eds (mrs::ed-convert-edge edge))
           (mrs::*eds-include-quantifiers-p* t)
           (mrs::*eds-include-vacuous-relations-p* t)
           (triples (mrs::ed-explode eds)))
      (declare (special mrs::*eds-include-quantifiers-p*
                        mrs::*eds-include-vacuous-relations-p*))
      (loop
          for triple in triples
          for key = (format nil "~{~a~^ ~}" triple)
          do
            (add-discriminant key nil :ed top 0 0)))))

(defun add-discriminant (key value type top start end)

  (declare (special %discriminants%))

  (let ((old (find-discriminant :type type :key key :start start :end end)))
    (if old
      ;;
      ;; _fix_me_
      ;; this used to be pushnew() and take a lot of time; for a given .top.,
      ;; it will be hard (though, unfortunately, possible in theory) to find
      ;; multiple equivalent (i.e. in terms of type, key, start, and end)
      ;; discriminants that characterize that edge; multiple occurrences of the
      ;; same unary rule at the same position (discharging several optional
      ;; complements, say) could be one such case, though.  however, for that
      ;; to work, we will need to elaborate the notion of discriminants already
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

(defun preset-discriminants (discriminants preset
                             &optional gold skew (allp t))

  (let (lead)
    (loop
        for old in gold
        for new
        = (when (or allp (not (eq (discriminant-toggle old) :unknown)))
            (loop 
                for new in discriminants
                thereis (when (discriminant-equal old new skew) new)))
        when new do
          (setf (discriminant-toggle new) (discriminant-toggle old))
          (setf (discriminant-state new) (discriminant-state old))
          (setf (discriminant-gold new) old)
        else do (push old lead))
    (loop
        for new in discriminants
        for old 
        = (unless (discriminant-gold new)
            (loop 
                for old in preset
                thereis (and (not (eq (discriminant-toggle old) :unknown))
                             (discriminant-equal old new) old)))
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

(defmacro skewed-discriminant-start (discriminant skew)
  `(if ,skew
     (let ((start (discriminant-start ,discriminant)))
       (- start (aref ,skew start)))
     (discriminant-start ,discriminant)))

(defmacro skewed-discriminant-end (discriminant skew)
  `(if ,skew
     (let ((end (discriminant-end ,discriminant)))
       (- end (aref ,skew end)))
     (discriminant-end ,discriminant)))

(defun discriminant-equal (foo bar &optional skew)
  (and (or (null (discriminant-key foo)) (null (discriminant-key bar))
           (equal (discriminant-key foo) (discriminant-key bar)))
       (or (null (discriminant-type foo)) (null (discriminant-type bar))
           (equal (discriminant-type foo) (discriminant-type bar)))
       (or (null (discriminant-start foo)) (null (discriminant-start bar))
           (equal (discriminant-start foo)
                  (skewed-discriminant-start bar skew)))
       (or (null (discriminant-end foo)) (null (discriminant-end bar))
           (equal (discriminant-end foo)
                  (skewed-discriminant-end bar skew)))))

(defun discriminant-state-as-string (discriminant)
  (let ((state (discriminant-state discriminant)))
    (cond ((eq state t) "+") ((null state) "-") (t "?"))))

(defun discriminant-toggle-as-string (discriminant)
  (let ((toggle (discriminant-toggle discriminant)))
    (cond ((eq toggle t) "+") ((null toggle) "-") (t "?"))))

(defun edge-yield (edge)
  (unless (null edge)
    (let ((daughters (edge-children edge)))
      (if (null daughters)
        (list edge)
        (loop 
            for daughter in daughters
            nconc (edge-yield daughter))))))

(defun compute-discriminant-skew (edges)
  ;;
  ;; _fix_me_
  ;; there appears to be a bug in PET derivations: `do not throw [...]' comes
  ;; out as 
  ;;
  ;;  (hcomp 0 7
  ;;   (dont_3 1 2 ("Do not" 1 2))
  ;;   [...] (bse_verb_infl_rule 2 3 (throw_away_v1 0 2 3 ("throw" 2 3))) [...]
  ;;
  ;; for now, attempt to work around this and assume that end positions will be
  ;; correct.  maybe this should go into reconstruction, even.  (11-jul-04; oe)
  ;;
  (when *tree-discriminants-skews*
    (loop
        with yield = (edge-yield (first edges))
        with size = (loop
                        for edge in yield maximize (edge-to edge))
        with skew = (make-array (+ size 1) :initial-element 0)
        with offset = 0
        with start = 0
        for edge in yield
        for surface = (first (edge-leaves edge))
        when (member surface *tree-discriminants-skews* :test #'string-equal)
        do (incf offset)
        do 
          (loop
              for i from (+ start 1) to (edge-to edge)
              do (setf (aref skew i) offset))
          (setf start (edge-to edge))
        finally (return skew))))

(defun read-blaze (&optional file)
  (setf *tree-discriminants-blaze* nil)
  (when file
   (if (probe-file file)
     (with-open-file (stream file :direction :input)
       (loop
	   with blaze = (make-hash-table :test #'equal)
	   with *package* = (find-package :lkb)
	   for form = (ignore-errors (read stream nil nil))
	   while form do
	     (let ((word (first form))
		   (tag (second form))
		   (types (rest (rest form))))
	       ;;
	       ;; _fix_me_
	       ;; use .word. information for lexicalisation, at some point.
	       ;;                                                (5-feb-04; oe)
	       (declare (ignore word))
	       (setf (gethash tag blaze) types))
	   finally
	     (format
	      (or #+:allegro excl:*initial-terminal-io* t)
	      "read-blaze(): read ~d blaze~p from `~a'.~%" 
	      (hash-table-count blaze) (hash-table-count blaze) file)
	     (setf *tree-discriminants-blaze* blaze)))
     (format
      (or #+:allegro excl:*initial-terminal-io* t)
      "read-blaze(): unable to open `~a'.~%" 
      file))))

(defun blaze-discriminant (discriminant tag blaze)
  (let* ((key (intern (string-upcase (discriminant-key discriminant)) :lkb))
         (key (when (is-valid-type key) key))
         (bucket (copy-list (gethash tag blaze))))
    (if (null key)
      (format
       (or #+:allegro excl:*initial-terminal-io* t)
       "blaze-discriminant(): ignoring discriminant with invalid key `~a'.~%"
       (discriminant-key discriminant))
      (loop
          for type = (pop bucket)
	  for confidence = (pop bucket)
	  while (and type confidence)
          when (and (is-valid-type type) (numberp confidence)
                    (or (eq key type) (subtype-p key type))) do
            (unless (zerop confidence)
              (setf (discriminant-toggle discriminant) 
                (> confidence 0.0))
              (setf (discriminant-blaze discriminant) tag))
	    (format
	     (or #+:allegro excl:*initial-terminal-io* t)
             "[~a] blaze-discriminant(): ~
              [~2,'0d ~2,'0d] ~a ~a | ~a~@[ `~a'~]~%"
             (current-time :long :short)
	     (discriminant-start discriminant) (discriminant-end discriminant)
             (discriminant-state-as-string discriminant)
             (discriminant-toggle-as-string discriminant)
             (discriminant-key discriminant) 
	     (discriminant-value discriminant))
	    (return)))))
