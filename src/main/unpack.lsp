;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LKB -*-

;;; Copyright (c) 2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: unpack.lsp
;;;      module: selective unpacking from parse or generation forests
;;;     version: 0.0 (29-nov-04)
;;;  written by: oe, university of sussex
;;; last update: 
;;;  updated by: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lkb)

(defvar *debug-stream* t)

(defparameter *unpacking-scoring-hook* nil)


(defun cross-product (lists)
  (if (null (rest lists))
    (loop
        for foo in (first lists) collect (list foo))
    (loop
        with rests = (cross-product (rest lists))
        for foo in (first lists)
        nconc (loop
                  for bar in rests
                  collect (cons foo bar)))))

#+:fdebug
(defparameter *unpacking-failure-paths* (make-hash-table :test #'equal))

(defun explode! (edges adjuncts)
  (append
   edges
   (loop
       for active in adjuncts
       for atdfs = (edge-dag active)
       for path = (first (g-edge-needed active))
       for forwardp = (first (edge-children active))
       for new = 
         (loop
             for passive in edges
             for ptdfs = (edge-dag passive)
             for result = (unless (logtest (g-edge-rels-covered active)
                                           (g-edge-rels-covered passive))
                            (with-unification-context (ignore)
                              (let ((result (yadu! atdfs ptdfs path)))
                                (when result
                                  (restrict-and-copy-tdfs result)))))
             when result collect 
               (make-g-edge 
                :id (next-edge :unpack)
                :rule (edge-rule active) :dag result
                :category (indef-type-of-tdfs result)
                :children (if forwardp
                            (append (loop
                                        for foo in (edge-children active)
                                        when (listp foo) append foo
                                        else collect foo)
                                    (list passive))
                            (cons passive (rest (edge-children active))))
                :leaves (let ((foo (loop
                                       for foo in (edge-leaves active)
                                       append foo)))
                          (if forwardp
                            (append foo (edge-leaves passive))
                            (append (edge-leaves passive) foo)))
                :lex-ids (let ((foo (loop
                                        for foo in (edge-lex-ids active)
                                        append foo)))
                           (if forwardp
                             (append foo (edge-lex-ids passive))
                             (append (edge-lex-ids passive) foo)))
                :lexemes (append (g-edge-lexemes active) 
                                 (g-edge-lexemes passive))
                :mod-index (g-edge-mod-index active)
                :rels-covered (logior (g-edge-rels-covered active)
                                      (g-edge-rels-covered passive))))
       when new append (explode! new adjuncts))))

(defun unpack-edges (edges)
  (loop
      for edge in edges
      append (unpack-edge! edge)))

(defun unpack-edge! (edge)
  (or (edge-foo edge)
      (setf (edge-foo edge) (unpack-edge!! edge))))

(defun unpack-edge!! (edge &optional insidep)
  #+:fdebug
  (clrhash *unpacking-failure-paths*)
  (labels ((instantiate (edge children)
             (let (#+:fdebug
                   (*unify-debug* :return))
               (with-unification-context (ignore)
                 (loop
                     with rule = (edge-rule edge)
                     with paths = (rest (rule-order rule))
                     with result = (rule-full-fs rule)
                     with leaves = nil
                     with lex-ids = nil
                     with rels = #+:mrs
                                 (if (mrs::found-rule-p rule) 
                                   (mrs::found-rule-main-rels rule) 
                                   0)
                                 #-:mrs
                                 0
                     with lexemes = nil
                     for path in paths
                     for child in children 
                     for tdfs = (edge-dag child)
                     when (and (g-edge-p child)
                               (logtest rels (g-edge-rels-covered child)))
                     do (setf result nil)
                     while result do
                       (setf leaves (append leaves (edge-leaves child)))
                       (setf lex-ids (append lex-ids (edge-lex-ids child)))
                       (setf result (yadu! result tdfs path))
                     when (g-edge-p child) do
                       (setf rels (logior rels (g-edge-rels-covered child)))
                       (setf lexemes (append lexemes (g-edge-lexemes child)))
                     finally
                       (when result 
                         (setf result (restrict-and-copy-tdfs result))
                         (return
                           (cond
                            (result
                             (if (g-edge-p edge)
                               (make-g-edge
                                :id (next-edge :unpack)
                                :rule rule :dag result
                                :category (indef-type-of-tdfs result)
                                :children children 
                                :leaves leaves :lex-ids lex-ids
                                :index (g-edge-index edge)
                                :mod-index (g-edge-mod-index edge)
                                :rels-covered rels :lexemes lexemes)
                               (make-edge
                                :id (next-edge :unpack) 
                                :rule rule :dag result
                                :category (indef-type-of-tdfs result)
                                :from (edge-from edge) :to (edge-to edge)
                                :children children 
                                :leaves leaves :lex-ids lex-ids)))
                            (t
                             (incf (packings-failures *packings*))
                             nil)))))))))

    #+:udebug
    (format
     *debug-stream*
     "unpack-edge(): ~a~%" edge)
    
    (let ((children (edge-children edge))
          (morphology (edge-morph-history edge))
          (adjuncts 
           ;;
           ;; adjoined modifiers may themselves be packed; for now, unpack them
           ;; before attempting to insert them into our trees; the caching, we
           ;; believe, we take care of the combinatorics, such that there is no
           ;; expected gain in postponing modifier unpacking into another phase
           ;; --- unless we somehow ended up building large numbers of trees
           ;; that ultimately fail and were otherwise unneeded; after an hour
           ;; or so over coffee (at Frederik), neither john nor i expect that
           ;; to be the case, though.                           (15-dec-03; oe)
           ;;
           ;; _fix_me_
           ;; for now, we assume modifier (active) edges are exactly binary;
           ;; the rest of the generator rather strongly makes that assumption
           ;; already.  in general, unpack-edge!() should be able to unpack
           ;; active edges too, however.                        (15-dec-03; oe)
           ;;
           (loop
               for edge in (when (consp (edge-adjuncts edge))
                             (edge-adjuncts edge))
               for rule = (edge-rule edge)
               for rtdfs = (rule-full-fs rule)
               for path = (first (rule-daughters-apply-order rule))
               for forwardp = (first (edge-children edge))
               for new = (unpack-edge! 
                          (if forwardp
                            (first (edge-children edge))
                            (second (edge-children edge))))
               append
                 (loop
                     with *deleted-daughter-features* = nil
                     for child in new
                     for tdfs = (edge-dag child)
                     for result = (with-unification-context (ignore)
                                    (let ((result (yadu! rtdfs tdfs path)))
                                      (when result
                                        (restrict-and-copy-tdfs result))))
                     when result collect
                       (make-g-edge
                        :id (next-edge :unpack)
                        :rule (edge-rule edge) :dag result
                        :category (indef-type-of-tdfs result)
                        :needed (g-edge-needed edge)
                        :children (if forwardp
                                    (list child nil)
                                    (list nil child))
                        :leaves (if forwardp
                                  (list (edge-leaves child) nil)
                                  (list nil (edge-leaves child)))
                        :lex-ids (if forwardp
                                   (list (edge-lex-ids child) nil)
                                   (list nil (edge-lex-ids child)))
                        :lexemes (g-edge-lexemes child)
                        :mod-index (g-edge-mod-index edge)
                        :rels-covered (logior (g-edge-rels-covered edge)
                                              (g-edge-rels-covered child)))))))
  
      (cond
       ;;
       ;; ignore genuinely frozen edges; now that we are into the unpacking
       ;; phase, frosted edges represent valid alternatives again.
       ;; 
       ((and (edge-frozen edge) (minusp (edge-frozen edge)))
        #+:udebug
        (format
         t
         "~&unpack-edge!(): ignoring <~d> (frozen for <~d>)~%"
         (edge-id edge) (edge-frozen edge))
        nil)
       ;;
       ;; unless we are inside of a recursive call on this edge already, make
       ;; sure we recurse on all packed nodes and accumulate results.
       ;;
       ((and (null insidep) (or (edge-packed edge) (edge-equivalent edge)))
        (nconc (unpack-edge!! edge t)
               (loop
                   for edge in (edge-packed edge)
                   nconc (unpack-edge! edge))
               (loop
                   for edge in (edge-equivalent edge)
                   nconc (unpack-edge! edge))))
       ;;
       ;; given the (idiosyncratic) LKB representation of rule applications
       ;; that affect the surface form, this is just a variant of the general
       ;; case where we have children.
       ;;
       (morphology
        (explode! 
         (loop
             with decompositions = (unpack-edge! morphology)
             for decomposition in decompositions
             for instantiation = (instantiate edge (list decomposition))
             when instantiation collect instantiation)
         adjuncts))
       ;;
       ;; the (default) recursive case: for each daughter, unfold it and build
       ;; list of unfolding results, one per daughter.  then compute all ways
       ;; in which this edge can be unfolded (`decomposed') and instantiate
       ;; each one in turn; feed total number of decompositions and index into
       ;; instantiate() to support cache maintenance.
       ;;
       (children
        (explode!
         (loop
             with daughters = (loop
                                  for edge in children
                                  collect (unpack-edge! edge))
             with decompositions = (cross-product daughters)
             for decomposition in decompositions
             for instantiation = (instantiate edge decomposition)
             when instantiation collect instantiation)
         adjuncts))
       ;;
       ;; at the leafs of the tree, terminate the recursion.
       ;;
       (t 
        (when (edge-odag edge) (setf (edge-dag edge) (edge-odag edge)))
        (explode! (list edge) adjuncts))))))

#+:null
(let ((*active-parsing-p* t)
      (*show-parse-p* nil)
      (*first-only-p* nil)
      (*chart-packing-p* t)
      contemplated filtered executed successful)
  (reset-packings)
  (time (multiple-value-setq (contemplated filtered
                              executed successful)
          (do-parse-tty 
           "so we will have an evening there to go over things or relax.")))
  (format
   t
   "~&~d trees; (=~d, >~d, <~d) packings; ~d readings; ~d [~d] edges~%"
   (length *parse-record*)
   (packings-equivalent *packings*)
   (packings-proactive *packings*)
   (packings-retroactive *packings*)
   (if *chart-packing-p*
     (time
      (loop
          for edge in *parse-record*
          sum (length (unpack-edge! edge))))
     (length *parse-record*))
   (tsdb::get-field :pedges (summarize-chart))
   (loop
       with mark = (gensym)
       for edge in *parse-record*
       sum (count-nodes
            edge :mark mark
            :packingp *chart-packing-p* :chartp t))))

(defstruct agenda
  (size 0)
  data)

(defparameter *agenda-recycling-p* nil)

(defparameter *agenda-pool-size* 0)

(defparameter *agenda-pool* nil)

(defun agenda-insert (agenda score item)
  (if (agenda-data agenda)
    (if (>= score (first (first (agenda-data agenda))))
      (setf (agenda-data agenda)
        (cons (cons score item) (agenda-data agenda)))
      (loop
          for data on (agenda-data agenda)
          for next = (first (rest data))
          when (or (null next) (>= score (first next))) do
            (setf (rest data) (cons (cons score item) (rest data)))
          and return nil))
    (setf (agenda-data agenda) (list (cons score item))))
  (incf (agenda-size agenda)))

(defun agenda-empty-p (agenda)
  (= (agenda-size agenda) 0))

(defun agenda-pop (agenda)
  (when (agenda-data agenda)
    (decf (agenda-size agenda))
    (rest (pop (agenda-data agenda)))))

(defstruct unpacking
  decompositions hypotheses instantiations
  (agenda (make-agenda)))

(defmethod print-object ((object unpacking) stream)
  (format 
   stream 
   "#[U <~{~a~^ ~}>]"
   (unpacking-decompositions object)))

(defstruct decomposition
  lhs rhs done)

(defmethod print-object ((object decomposition) stream)
  (format 
   stream 
   "#[D ~(~a~) < ~(~{~a ~^~}~)>]"
   (decomposition-lhs object) (decomposition-rhs object)))

(defmacro decomposition-record-indices (decomposition indices)
  `(push ,indices (decomposition-done ,decomposition)))

(defmacro decomposition-indices-done-p (decomposition indices)
  `(member ,indices (decomposition-done ,decomposition) :test #'equal))

(defstruct hypothesis
  score decomposition indices daughters edge)

(defmethod print-object ((object hypothesis) stream)
  (format 
   stream 
   "#[H ~a~@[ ~a~]]"
   (hypothesis-indices object) (hypothesis-edge object)))

(defun selectively-unpack-edges (edges &optional n &key test)

  (unless edges (return-from selectively-unpack-edges))
  (setf %edges edges)
  (if (or (null n) (not (numberp n)) (<= n 0) (null *unpacking-scoring-hook*))
    (let ((edges (unpack-edges edges)))
      (if test
        (loop
            for edge in edges
            when (funcall test edge) collect edge)
        edges))
    ;;
    ;; ignore genuinely frozen edges; now that we are into the unpacking
    ;; phase, frosted edges represent valid alternatives again.  since we are
    ;; interested in the probability distribution over all results, use one of
    ;; the packed edges as the `representative' for all of them, i.e. make sure
    ;; that all non-frozen edges are hypothesized against the agenda of that 
    ;; one special edge.
    ;; 
    (let* ((active (loop
                       for edge in edges
                       unless (and (edge-frozen edge)
                                   (minusp (edge-frozen edge)))
                       collect edge))
           (representative (first active)))
      (hypothesize-edge representative 0 :top (or (rest active) t))
      (loop
          for i from 0
          for hypothesis = (hypothesize-edge representative i)
          for new = (when hypothesis 
                      (let ((edge (instantiate-hypothesis hypothesis)))
                        (when (and edge 
                                   (or (null test) (funcall test edge)))
                          edge)))
          while (and hypothesis (>= n 1))
          when new do (decf n) and collect new))))

(defun hypothesize-edge (edge i &key top agenda)
  ;;
  ;; returns expected score for .i.-th instantiation of this .edge., where some
  ;; of these might turn out inconsistent later.  whenever we are called with a
  ;; new (aka previously unseen) value for .i., we assume it is the immediately
  ;; following index from the previous call, i.e. we will search for the next
  ;; best hypothesis.
  ;;
  (when (null (edge-unpacking edge))
    
    (unless (= i 0)
      (error "hypothesize-edge(): first time call with i == ~a" i))
    
    (let* ((unpacking (make-unpacking))
           (agenda (or agenda (unpacking-agenda unpacking))))
      (setf (edge-unpacking edge) unpacking)
      (decompose-edge edge)
      (loop
          for decomposition in (unpacking-decompositions unpacking)
          for n = 0
          for daughters 
          = (loop
                for edge in (decomposition-rhs decomposition)
                do (incf n) collect (hypothesize-edge edge 0))
          for indices = (make-list n :initial-element 0)
          for hypothesis
          = (make-hypothesis
             :decomposition decomposition
             :indices indices
             :daughters daughters)
          for score = (score-hypothesis hypothesis)
          do 
            #+:hdebug
            (format t "~%>> ~a~%~%" hypothesis)
            (decomposition-record-indices decomposition indices)
            (agenda-insert agenda score hypothesis))
      ;;
      ;; for the special case that we are working on `top' edges, i.e. those in
      ;; *parse-record* or *gen-record*, we need to ensure that decompositions
      ;; corresponding to top-level packings are hypothesized into the agenda 
      ;; of the host edge, and that we invoke the same procedure an local 
      ;; alternates i.e. other arguments to selectively-unpack-edges() that
      ;; happen to not be the one representative edge.
      ;;
      (when top
        (loop
            for edge in (edge-packed edge)
            unless (and (edge-frozen edge) 
                        (minusp (edge-frozen edge)))
            do (hypothesize-edge edge 0 :agenda agenda))
        (loop
            for edge in (edge-equivalent edge)
            unless (and (edge-frozen edge) 
                        (minusp (edge-frozen edge)))
            do (hypothesize-edge edge 0 :agenda agenda))
        (when (consp top)
          (loop
              for edge in top
              do (hypothesize-edge edge 0 :agenda agenda :top t))))))
  
  (let* ((unpacking (edge-unpacking edge))
         (agenda (unpacking-agenda unpacking))
         (hypothesis (when unpacking 
                       (nth i (unpacking-hypotheses unpacking)))))
    (if hypothesis
      ;;
      ;; in case we have hypothesized this decomposition before, just reuse it;
      ;;
      hypothesis
      ;;
      ;; otherwise, retrieve the current best candidate, try generating new
      ;; hypotheses from `vertical' search, i.e. advancing either one of the
      ;; daughter indices on the current best, put those on the agenda, and
      ;; return the one just retrieved.
      ;;
      (unless (agenda-empty-p agenda)
        (let* ((hypothesis (agenda-pop agenda))
               (indiceses 
                (loop
                    for foo on (hypothesis-indices hypothesis)
                    collect (append prefix
                                    (cons (+ (first foo) 1) (rest foo)))
                    collect (first foo) into prefix)))
          #+:hdebug
          (format t "~%<< ~a~%~%" hypothesis)
          
          (loop
              with decomposition = (hypothesis-decomposition hypothesis)
              for indices in indiceses
              for daughters 
              = (unless (decomposition-indices-done-p decomposition indices)
                  (loop
                      for edge in (decomposition-rhs decomposition)
                      for i in indices
                      for daughter = (hypothesize-edge edge i)
                      when (null daughter) return nil          
                      collect daughter))
              for new 
              = (when daughters
                  (make-hypothesis
                   :decomposition decomposition
                   :indices indices
                   :daughters daughters))
              when new
              do        
                #+:hdebug
                (format t "~%>> ~a~%~%" new)
                (decomposition-record-indices decomposition indices)
                (agenda-insert 
                  agenda (score-hypothesis new) new))
          (setf (unpacking-hypotheses unpacking)
            (nconc (unpacking-hypotheses unpacking) (list hypothesis)))
          hypothesis)))))

(defun decompose-edge (edge)
  ;;
  ;; entirely called for its side effect: populate `decomposition' set in the 
  ;; `unpacking' record of .edge.
  ;;
  (when (null (edge-unpacking edge))
    (setf (edge-unpacking edge) (make-unpacking)))
  
  (let ((unpacking (edge-unpacking edge))
        (children (or (edge-children edge)
                      (let ((morphology (edge-morph-history edge)))
                        (and morphology (list morphology))))))

    (when (null children)
      (let ((decomposition (make-decomposition :lhs edge)))
        (push decomposition (unpacking-decompositions unpacking))))
    
    ;;
    ;; _fix_me_
    ;; possibly we could save some cons()es here, essentially doing the cross
    ;; product on-the-fly, i.e. as we go along.                  (3-dec-04; oe)
    ;;
    (loop
        for child in children
        for packed = (loop
                         for foo in (edge-packed child)
                         for frozen = (edge-frozen foo)
                         unless (and frozen (minusp frozen))
                         collect foo)
        for equivalent = (loop
                             for foo in (edge-equivalent child)
                             for frozen = (edge-frozen foo)
                             unless (and frozen (minusp frozen))
                             collect foo)
        collect (cons child (nconc packed equivalent)) into foo
        finally 
          (loop
              for rhs in (cross-product foo)
              for decomposition = (make-decomposition :lhs edge :rhs rhs)
              do (push decomposition (unpacking-decompositions unpacking))))))

(defun score-hypothesis (hypothesis)
  (setf (hypothesis-score hypothesis)
    (+
     (loop
         for daughter in (hypothesis-daughters hypothesis)
         for score = (or (hypothesis-score daughter)
                         (score-hypothesis daughter))
         sum score)
     (let ((decomposition (hypothesis-decomposition hypothesis)))
       (if *unpacking-scoring-hook*
         (funcall
          *unpacking-scoring-hook*
          (decomposition-lhs decomposition)
          (decomposition-rhs decomposition))
         0)))))

(defun instantiate-hypothesis (hypothesis)

  (let ((cache (hypothesis-edge hypothesis)))
    (cond
     (cache (unless (eq cache :fail) cache))
     ((null (hypothesis-daughters hypothesis))
      (let* ((decomposition (hypothesis-decomposition hypothesis))
             (edge (decomposition-lhs decomposition)))
        (when (edge-odag edge) (setf (edge-dag edge) (edge-odag edge)))
        (setf (edge-score edge) (hypothesis-score hypothesis))
        (setf (hypothesis-edge hypothesis) edge)))
     (t
      (setf (hypothesis-edge hypothesis)
        (let* ((children (loop
                             for daughter in (hypothesis-daughters hypothesis)
                             for child = (instantiate-hypothesis daughter)
                             when (null child) return nil
                             collect child)))
          (if children
            (with-unification-context (ignore)
              (loop
                  with score = (hypothesis-score hypothesis)
                  with decomposition = (hypothesis-decomposition hypothesis)
                  with edge = (decomposition-lhs decomposition)
                  with rule = (edge-rule edge)
                  with paths = (rest (rule-order rule))
                  with result = (rule-full-fs rule)
                  with leaves = nil
                  with lex-ids = nil
                  with rels = (if (mrs::found-rule-p rule) 
                                (mrs::found-rule-main-rels rule) 
                                0)
                  with lexemes = nil
                  for path in paths
                  for child in children
                  for tdfs = (edge-dag child)
                  when (and (g-edge-p child)
                            (logtest rels (g-edge-rels-covered child)))
                  do (setf result nil)
                  while result do
                    (setf leaves (append leaves (edge-leaves child)))
                    (setf lex-ids (append lex-ids (edge-lex-ids child)))
                    (setf result (yadu! result tdfs path))
                  when (g-edge-p child) do
                    (setf rels (logior rels (g-edge-rels-covered child)))
                    (setf lexemes (append lexemes (g-edge-lexemes child)))
                  finally
                    (when result (setf result (restrict-and-copy-tdfs result)))
                    (return
                      (if result
                        (if (g-edge-p edge)
                          (make-g-edge
                           :id (next-edge :unpack) :score score
                           :rule rule :dag result
                           :category (indef-type-of-tdfs result)
                           :children children 
                           :leaves leaves :lex-ids lex-ids
                           :index (g-edge-index edge)
                           :mod-index (g-edge-mod-index edge)
                           :rels-covered rels :lexemes lexemes :baz edge)
                          (make-edge
                           :id (next-edge :unpack) :score score
                           :rule rule :dag result
                           :category (indef-type-of-tdfs result)
                           :from (edge-from edge) :to (edge-to edge)
                           :children children 
                           :leaves leaves :lex-ids lex-ids :baz edge))
                        :fail))))
            :fail)))
      (let ((result (hypothesis-edge hypothesis)))
        (unless (eq result :fail) result))))))

(defun hypothesis-derivation (hypothesis)
  (let* ((decomposition (hypothesis-decomposition hypothesis))
         (edge (decomposition-lhs decomposition))
         (id (edge-id edge))
         (from (edge-from edge))
         (to (edge-to edge))
         (score (hypothesis-score hypothesis)))
    (if (null (hypothesis-daughters hypothesis))
      (list
       id (first (edge-lex-ids edge)) score from to
       (list (edge-rule edge) from to))
      (nconc
       (list
        id (rule-id (edge-rule edge)) score from to)
       (loop
           for daughter in (hypothesis-daughters hypothesis)
           collect (hypothesis-derivation daughter))))))

#+:null
(progn
  (setf *parse-record* nil)
  (excl:gc) (excl:gc t) (excl:gc)
  (do-parse-tty "kim saw the cat in the hotel near the lake ~
                 when sandy arrived with abrams ")
  (let ((estart *edge-id*)
        (ustart *unifications*)
        (cstart *copies*))
    (setf all (time (unpack-edges *parse-record*)))
    (loop
        for edge in all
        for score = (tsdb::mem-score-edge edge)
        do (setf (edge-score edge) score))
    (setf all (sort all #'> :key #'edge-score))
    (format
     t
     "~%~a result~p: ~a edges; ~a unifications; ~a copies.~%"
     (length all) (length all)
     (- *edge-id* estart) (- *unifications* ustart) (- *copies* cstart))
    (let ((estart *edge-id*)
          (ustart *unifications*)
          (cstart *copies*))
      (setf best (time (selectively-unpack-edges *parse-record* 5)))
      (format
       t
       "~%~a result~p: ~a edges; ~a unifications; ~a copies.~%"
       (length best) (length best)
       (- *edge-id* estart) (- *unifications* ustart) (- *copies* cstart)))
    (loop
        for i from 0
        for edge in best
        for ederivation = (compute-derivation-tree edge)
        for target in all
        for tderivation = (compute-derivation-tree target)
        unless (tsdb::derivation-equal ederivation tderivation)
        do (format
            t
            "[~a] derivation mismatch:~%  ~s~%  ~s~%~%"
            i ederivation tderivation))))
