;;; Copyright (c) 2006--2007 Stephan Oepen; see `licence.txt' for conditions.

(in-package :mrs)

;;;
;;; an attempt at generalizing over various ways of linking to the underlying
;;; input to the parser, be it by character or vertex ranges (as used at times
;;; in HoG et al.) or token identifiers (originally at YY and now in LOGON).
;;; currently, there are four distinct value formats:
;;;
;;;   <0:4>    character range (i.e. a sub-string of an assumed flat input);
;;;   <0#2>    chart vertex range (traditional in PET to some degree);
;;;   <0 1 3>  token identifiers, i.e. links to basic input units;
;;;   <@42>    edge identifier (used internally in generation)
;;;
;;; of these, the first is maybe most widely supported across DELPH-IN tools,
;;; while the second (in my view) should be deprecated.  the third resembles
;;; what was used in VerbMobil, YY, and now LOGON; given that the input to a
;;; `deep' parser can always be viewed as a token lattice, this is probably the
;;; most general mode, and we should aim to establish it over time: first, the
;;; underlying input may not have been string-shaped (but come from the lattice
;;; of a speech recognizer), and second even with one underlying string there
;;; could be token-level ambiguity, so identifying the actual token used in an
;;; analysis preserves more information.  properties like the sub-string range,
;;; prosodic information (VerbMobil), or pointers to KB nodes (YY) can all be
;;; associated with the individual tokens sent into the parser.  finally, the
;;; fourth mode is used in generation, where surface linking actually is a two-
;;; stage process (see comments in `generate.lsp').              (4-dec-06; oe)
;;;
(defun output-lnk (lnk &key (stream t))
  (if stream
    (when *show-lnk-p*
      (case (first lnk)
        (:characters
         (let ((start (second lnk))
               (end (third lnk)))
           (when (and (numberp start) (numberp end)
                      (>= start 0) (>= end 0))
             (format stream "<~a:~a>" (second lnk) (third lnk)))))
        (:vertices
         (format stream "<~a#~a>" (second lnk) (third lnk)))
        (:tokens
         (format stream "<~{~a~^ ~}>" (rest lnk)))
        (:id
         (format stream "<@~a>" (second lnk)))))
    (with-output-to-string (stream)
      (output-lnk lnk :stream stream))))

(defun read-lnk (&optional (stream t))
  (ignore-errors
   (when (and (streamp stream) (open-stream-p stream))
     (let ((c (peek-char t stream nil nil))
           (*readtable* (copy-readtable))
           type)
       (set-syntax-from-char #\: #\" *readtable*)
       (set-syntax-from-char #\# #\" *readtable*)
       (set-syntax-from-char #\, #\" *readtable*)
       (set-syntax-from-char #\> #\" *readtable*)
       (when (and c (char= c #\<))
         (read-char stream nil nil)
         (let ((c (peek-char t stream nil nil)))
           (when (and c (member c '(#\@) :test #'char=))
             (read-char stream nil nil)
             (setf type :id)))
         (let ((start (read stream nil nil)))
           (when (numberp start)
             (let ((c (peek-char t stream nil nil)))
               (cond
                ((null c) (return-from read-lnk))
                ((member c '(#\: #\#) :test #'char=)
                 (read-char stream nil nil)
                 (let ((end (read stream nil nil)))
                   (when (numberp end)
                     (let ((foo (read-char stream nil nil)))
                       (when (and foo (char= foo #\>))
                         (list
                          (if (char= c #\:) :characters :vertices)
                          start end))))))
                (t
                 (loop
                     with ids = (list start)
                     for c = (peek-char t stream nil nil)
                     for id = (unless (and c (char= c #\>))
                                (read stream nil nil))
                     when (and c (char= c #\>))
                     return (cons (or type :tokens) (nreverse ids))
                     unless (numberp id) do (return-from read-lnk)
                     do (push id ids))))))))))))
                     
(defun combine-lnks (lnk1 lnk2)
  (cond
   ((null lnk1) lnk2)
   ((null lnk2) lnk1)
   ((eq (first lnk1) (first lnk2))
    (case (first lnk1)
      ((:characters :vertices)
       (list
        (first lnk1)
        (min (second lnk1) (second lnk2)) (max (third lnk1) (third lnk2))))
      (:tokens
       (list :tokens (union (rest lnk1) (rest lnk2))))))))

(defun compute-lnk-distortion (pairs &key tags)
  (declare (ignore tags))
  ;;
  ;; when pair-wise comparing EPs from the same MRS, we distinguish as follows:
  ;;   0: identical span
  ;;   1 or -1: inclusion or backward inclusion
  ;;   2 or -2: adjacent precedence (or inverse)
  ;;   3 or -3: non-adjacent precedence
  ;;
  (labels ((adjacentp (i j)
             (<= (abs (- i j)) 1))
           (compare-lnks (lnk1 lnk2)
             (when (and (eq :characters (first lnk1))
                        (eq :characters (first lnk2)))
               (let ((start1 (second lnk1))
                     (end1 (third lnk1))
                     (start2 (second lnk2))
                     (end2 (third lnk2)))
                 (cond
                  ((and (adjacentp start1 start2) (adjacentp end1 end2)) 0)
                  ((and (adjacentp start1 start2) (> end1 end2)) 1)
                  ((and (< start1 start2) (adjacentp end1 end2)) 1)
                  ((and (adjacentp start1 start2) (< end1 end2) -1))
                  ((and (> start1 start2) (adjacentp end1 end2)) -1)
                  ((adjacentp end1 start2) 2)
                  ((adjacentp start1 end2) -2)
                  ((< end1 start2) 3)
                  ((> start1 end2) -3)
                  (t 0))))))
    
    (loop
        with n = (length pairs)
        with distortion = 0
        with eps1 = (loop for foo in pairs collect (first foo))
        with eps2 = (loop for foo in pairs collect (rest foo))
        for key in eps1
        for klnk = (rel-lnk key)
        for match in eps2
        for mlnk = (rel-lnk match)
        do
          (loop
              for ep1 in eps1
              for ep2 in eps2
              unless (and (eq key ep1) (eq match ep2))
              do
                (let* ((key (compare-lnks klnk (rel-lnk ep1)))
                       (match (compare-lnks mlnk (rel-lnk ep2))))
                  (when (and key match)
                    (incf distortion (abs (- key match))))))
        finally (return (if (zerop n) 0 (float (/ distortion n)))))))

(in-package :lkb)

#+:lkb
(defun lnk-tdfs (tdfs lnks)
  (unless (consp mrs::*rel-lnk-path*)
    (format
     t
     "lnk-tdfs(): invalid value for *rel-lnk-path* (~a)."
     mrs::*rel-lnk-path*)
    (return-from lnk-tdfs tdfs))
  
  (if *within-unification-context-p*
    (let* ((lnks (if (consp lnks) lnks (list lnks)))
           (unifications
            (loop
                for lnk in lnks
                for value = (if (stringp lnk) lnk (write-to-string lnk))
                for path = *list-head* then (append *list-tail* path)
                collect
                  (make-unification
                   :lhs (create-path-from-feature-list path)
                   :rhs (make-u-value :type value))
                into unifications
                finally
                  (let ((path (nconc (butlast path) *list-tail*)))
                    (return
                      (cons
                       (make-unification
                        :lhs (create-path-from-feature-list path)
                        :rhs (make-u-value :type *empty-list-type*))
                       unifications)))))
           (overlay (process-unifications unifications nil))
           (overlay (and overlay (create-wffs overlay nil)))
           (path
            (append mrs::*initial-semantics-path* mrs::*psoa-liszt-path*))
           (rels (unify-existing-dag-at-end-of (tdfs-indef tdfs) path)))
      (when (and overlay rels)
        (loop
            with visited
            with *unify-wffs* = t
            for path = *list-head* then (append *list-tail* path)
            for lnk = (unify-existing-dag-at-end-of
                       rels (append path mrs::*rel-lnk-path*))
            ;;
            ;; protect ourselves against cyclic RELS feature structures (which
            ;; might have cropped up during unification but have yet to be 
            ;; found by the copier.
            ;;
            while (and lnk (not (member lnk visited :test #'eq)))
            do (push lnk visited)
            when (eq (unify-get-type lnk) *list-type*)
            do (unify-dags lnk overlay)))
      tdfs)
    (with-unification-context (null)
      (let ((result (lnk-tdfs tdfs lnks)))
        (copy-tdfs-elements result)))))
