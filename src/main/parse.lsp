;;; Copyright Ann Copestake 1992-1998
;;; All Rights Reserved.
;;; No use or redistribution without permission.
;;; 


;;; Port to MCL - moved dialect specific display stuff to parseout.lsp

;;; Modified 03/93 to incorporate LKB morphology system - BEMJ
;;;
;;; mods to: add-word
;;;          evaluate-unifications
;;; new fns: process-morph-possibilities
;;;          apply-morph-rule
;;; AAC 1995 - junked most of Bernie's mods - call morph-analyse and
;;; proceed from there ...

;;; This file implements the chart parser.
;;; A rule is applied in a standard fashion (see below). In case a mother node
;;; can be constructed, the dags of the daughter nodes (which are already) 
;;; recorded in the chart) are unified, following the constraints specified 
;;; by the unification patterns on the rule. The process of  unification 
;;; either blocks the actual application of the rule (due to  failure to meet
;;; constraints), or triggers a success (in which case the new dag - the 
;;; result of the unification) is associated with the node constructed for the
;;; mother. This information is recorded in the chart, and the process 
;;; continues.
;;;      The chart itself is kept and can be displayed by the top level
;;; commands.
;;; Control strategy is purely bottom up, left corner and not the world's most
;;; efficient.

;;; The chart structure
;;;
;;; The chart is an array indexed by the vertex number 
;;; This is slightly inelegant in that the size of the array is set.
;;; Vertices are integers, not atoms.

(defvar *parse-unifs* 0)
(defvar *parse-fails* 0)
(defvar *check-paths-successes* 0)
(defvar *check-paths-fails* 0)

(defvar *cached-orth-str-list* nil)

;;; *chart-limit* is defined in globals.lsp

(defvar *chart* (make-array (list *chart-limit*))) 

(defvar *parse-record* nil)

;;; *chart* is a vector of chart-entry structures - one for each end vertex

(defstruct (chart-entry) configurations)

;;; chart-entry-configurations is a list of chart-configurations

(defstruct (chart-configuration) 
    begin edge roots)

;;; begin is a vertex - edge is an edge 

;;; an edge is a structure
;;; it has the following properties
;;; category - eg S VP etc - now a type
;;; rule-number - either the word (storms etc) or the number of the grammar
;;; rule which has been applied to create that edge  
;;; dag - the dag associated with the constituent
;;; leaves - orthography of whatever this edge has been formed from
;;; lex-ids - like leaves, but identifiers of whole structures
;;; children - daughter edges
 
(defstruct
   (edge
      (:constructor make-edge
                    (&key id category rule-number dag 
                          (dag-restricted (restrict-fs (tdfs-indef dag)))
                          leaves lex-ids children morph-history 
                          spelling-change)))
   id category rule-number dag dag-restricted leaves lex-ids
   children morph-history spelling-change)

(defstruct
   (mrecord
      (:constructor make-mrecord
                    (&key fs (fs-restricted (restrict-fs (tdfs-indef fs)))
                          lex-ids rules history)))
   fs fs-restricted lex-ids rules history)

(defstruct 
  (mhistory)
  rule-id
  fs
  new-spelling)

(defvar *edge-id* 0)

(defun next-edge nil
   (incf *edge-id*)
   *edge-id*)

(defvar *morphs* (make-array (list *chart-limit*) :initial-element nil))

;;; *morphs* is added, paralleling *chart* to allow for
;;; multi-word entries (and eventually idioms perhaps).
;;; multi-word entries may have affixation on any member
;;; but they span several vertices.  It's therefore
;;; necessary to treat them as individual words with respect to
;;; the orthographemic component, and to record the results of that
;;; on a structure, so that putative multiword entries can be checked
;;; when we get to the rightmost element.

(defstruct (morph-edge)
   id word morph-results)

;;;

(defvar *morph-records* nil)

;;; *morph-records* is just so that the morphological history
;;; (i.e. inflection, derivation rules and any zero-morpheme rules
;;; interspersed among them) can be displayed

(defun clear-chart nil
   (setf *cached-orth-str-list* nil)
   (setf *parse-record* nil) 
   (fill *chart* nil) 
   (fill *morphs* nil)
   (setf *morph-records* nil)
   (setf *edge-id* 0))

;;; Entry point to this group of functions is parse which is passed the
;;; sentence as a list of strings and is called from the top level

(defun parse (user-input)
   (cond ((> (length user-input) *chart-limit*)
         (format t "Can't parse this length of sentence~%") nil)
      (t
         (let ((*safe-not-to-copy-p* t)
               (*parse-unifs* 0) (*parse-fails* 0) (*check-paths-successes* 0)
               (*check-paths-fails* 0))
            (clear-chart)
            #+powerpc(setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
            (add-morphs-to-morphs user-input)
            (add-words-to-chart)
            (setf *parse-record*
               (find-spanning-edges 0 (length user-input)))
            (show-parse)
            (values *parse-unifs* *parse-fails* *check-paths-successes*
               *check-paths-fails*)))))

(defun add-morphs-to-morphs (user-input)
   (let ((current 0))
      (dolist (word user-input)
         (let* ((new (+ current 1))
                (morph-poss 
                 (progn #+:powerpc(decf gg (CCL::%HEAP-BYTES-ALLOCATED))
                        (prog1
                            (append (morph-analyse word)
                                    (find-irregular-morphs word))
                          #+:powerpc(incf gg (CCL::%HEAP-BYTES-ALLOCATED))))))
           (unless morph-poss (format t "~%Word ~A is not in lexicon" word)
                   (return))
           (setf (aref *morphs* current)
                 (make-morph-edge :id current 
                                  :word word 
                                  :morph-results morph-poss))
           (setf current new)))))

(defun add-words-to-chart nil
   (let ((current 0)
         (to-be-accounted-for (make-array (list *chart-limit*) 
                                          :initial-element nil)))
     ;; to-be-accounted for is needed because we cannot tell
     ;; that a word is impossible until after the 
     ;; whole sentence has been processed
     ;; because it may be part of a multi-word
     (loop
       (let ((morph-poss (aref *morphs* current)))
         (when (null morph-poss)
           (return nil))
         (incf current)
         (multiple-value-bind (ind-results multi-strings)
           (add-word (morph-edge-word morph-poss)
                     (morph-edge-morph-results morph-poss) current)
           (unless (or ind-results multi-strings)
             (setf (aref to-be-accounted-for current)
                   (morph-edge-word morph-poss)))
           ; record the fact we haven't analysed this word
           (for mstr in multi-strings
                ; wipe the record for multi-words which allow for it
                do
                (let ((words (split-into-words mstr)))
                  (dotimes (x (length words))
                       (setf (aref to-be-accounted-for (- current x)) 
                             nil)))))))
     (dotimes (y current)
       (when (aref to-be-accounted-for y)
         (format t "~%No sign can be constructed for ~A" 
                 (aref to-be-accounted-for y))))))


(defun add-word (local-word morph-poss right-vertex)
   ;;; get-senses returns a list of conses of ids and dags 
   ;;; corresponding to the word senses 
   ;;; - the type of the dag is used 
   ;;; to do the indexing
      (let* ((word-senses 
              (for morph-res in morph-poss
                   append
                   (for sense in (get-senses (car morph-res))
                        append
                        (if (cdr morph-res)
                            (apply-all-lexical-and-morph-rules 
                             (list (make-mrecord :lex-ids (list (car sense))
                                                 :fs (cdr sense) 
                                                 :rules (cdr morph-res))))
                          (list (make-mrecord :lex-ids (list (car sense))
                                 :fs (cdr sense) :rules nil)))))))
        (dolist (mrec word-senses)
          (let ((lex-ids (mrecord-lex-ids mrec))
                (sense (mrecord-fs mrec))
                (history (mrecord-history mrec)))
            (activate-context (- right-vertex 1) 
                              (construct-lex-edge sense history local-word
                                                    lex-ids)
                                 right-vertex)))
        (let ((multi-results
               (add-multi-words morph-poss right-vertex)))
          ; add-multi-words is mostly for side effects, but want to
          ; check if we've found something, and produce correct error
          ; messages, so we return the strings found
          (values word-senses multi-results))))

(defun construct-lex-edge (sense history word lex-ids)
  (make-edge :id (next-edge) 
             :category (indef-type-of-tdfs sense) 
             :rule-number 
             (if history (mhistory-rule-id 
                          (car 
                           history)) word)
             :dag sense
             :leaves (list word)
             :lex-ids lex-ids
             :morph-history 
             (construct-morph-history 
              lex-ids history)
             :spelling-change (if history (mhistory-new-spelling
                                           (car 
                                            history)))))

(defun get-senses (stem-string)
  (let* ((*safe-not-to-copy-p* nil)
         (entries (get-unexpanded-lex-entry 
                   (string-upcase stem-string))))
    (for entry in entries
         filter
         (if (not (cdr (lex-or-psort-orth entry)))
           ; exclude multi-words
             (let* ((id (lex-or-psort-id entry))
                    (expanded-entry
                     (get-psort-entry id)))
               (when expanded-entry
                 (cons id
                       (lex-or-psort-full-fs expanded-entry))))))))

;;; get-multi-senses has to return a structure

(defstruct (sense-record)
  word-string
  left-vertex
  lex-ids
  fs
  morph-res
  mrecs)
  
(defun add-multi-words (morph-poss right-vertex)
   (let* ((multi-strings nil)
          (word-senses 
          (for stem in (remove-duplicates 
                        (for analysis in morph-poss
                             collect (car analysis)) :test #'string-equal)
               ; make sure we have all the possible stems
               ; in case inflection is going to be allowed on rightmost element
               ; but otherwise the variable morph-poss is not used
               append
               (for sense-record in (get-multi-senses stem right-vertex)
                    filter
                    (let* ((sense (sense-record-fs sense-record))
                           (lex-ids (sense-record-lex-ids sense-record))
                           (new-morph-res 
                            (sense-record-morph-res sense-record))
                           (mrecs 
                            (if (cdr new-morph-res)
                              (apply-all-lexical-and-morph-rules 
                               (list (make-mrecord :fs sense 
                                                   :lex-ids lex-ids
                                                   :rules 
                                                   (cdr new-morph-res))))
                              (list (make-mrecord :fs sense 
                                                  :lex-ids lex-ids      
                                                  :rules nil)))))
                      (if mrecs
                        (progn
                          (setf (sense-record-mrecs sense-record) mrecs)
                          sense-record)))))))                      
         (dolist (sense-record word-senses)
           (let ((word (sense-record-word-string sense-record))
                 (left-vertex (sense-record-left-vertex sense-record)))
             (push word multi-strings)
             (dolist (mrec (sense-record-mrecs sense-record))
               (let ((sense (mrecord-fs mrec))
                     (lex-ids (mrecord-lex-ids mrec))
                     (history (mrecord-history mrec)))
                 (activate-context left-vertex 
                                   (construct-lex-edge sense history word
                                                       lex-ids)      
                                   right-vertex)))))
         ; return multi-strings, so we know what's been found
         multi-strings))


(defun get-multi-senses (stem-string right-vertex)
  (let ((entries (get-unexpanded-lex-entry (string-upcase stem-string))))
    (for entry in entries
         append
         (if (cdr (lex-or-psort-orth entry))
           (check-multi-word stem-string entry right-vertex
                             (lex-or-psort-id entry))))))


(defun check-multi-word (stem unexpanded-entry right-vertex id)
  (let ((entry-orth (lex-or-psort-orth unexpanded-entry))
        (ok t)
        (new-morph-res nil)
        (amalgamated-stems nil)
        (amalgamated-words nil)
        (inflection-position (lex-or-psort-infl-pos unexpanded-entry))) 
    (when (< right-vertex (length entry-orth))
      (return-from check-multi-word nil)) ; too near start of sentence
    (if (string-equal (car (last entry-orth)) stem)
      ; only check multi-words when we have the rightmost
      (let ((current-vertex (- right-vertex (length entry-orth)))
            (current-position 1))
        (dolist (word-stem entry-orth)
          (let* ((morph-entry (aref *morphs* current-vertex))
                (existing-word (morph-edge-word morph-entry)))
            (if (eql current-position inflection-position)
              ; inflection allowed here
              (let ((current-morph-res (morph-edge-morph-results morph-entry)))
                (setf new-morph-res
                      (for res in current-morph-res
                           filter
                           (if (string-equal word-stem (car res))
                             res)))
                (unless new-morph-res
                  (setf ok nil)
                  (return)))
             ; else cannot be inflected        
              (unless 
                (string-equal word-stem existing-word)
                (setf ok nil)
                (return)))
            (push word-stem amalgamated-stems)
            (push " " amalgamated-stems)
            (push existing-word amalgamated-words)
            (push " " amalgamated-words)
            (incf current-vertex)
            (incf current-position)))
        (if ok
          (let ((expanded-entry (get-psort-entry id)))
            (if expanded-entry
                (let*
                    ((full-stem-string 
                      (apply #'concatenate 'string 
                             (nreverse (cdr amalgamated-stems))))
                     (full-word-string 
                      (apply #'concatenate 'string 
                             (nreverse (cdr amalgamated-words)))))
                  (cons 
                   (make-sense-record :word-string full-word-string
                                      :left-vertex (- right-vertex 
                                                      (length entry-orth))
                                      :morph-res (list full-stem-string)
                                      :lex-ids (list (lex-or-psort-id
                                                expanded-entry))
                                      :fs (lex-or-psort-full-fs 
                                           expanded-entry))
                   (for rule in (for res in new-morph-res
                                     filter
                                     (caadr res))
                        collect
                        (make-sense-record :word-string full-word-string
                                           :left-vertex (- right-vertex 
                                                           (length entry-orth))
                                           :morph-res 
                                           (list full-stem-string 
                                                 (list rule 
                                                       full-word-string))
                                           :lex-ids (list (lex-or-psort-id
                                                expanded-entry))
                                    :fs (lex-or-psort-full-fs 
                                         expanded-entry))))))))))))


(defun construct-morph-history (lex-ids history)
  ;;; the rule on an edge refers `back' i.e. to the way it was
  ;;; constructed, so when this is called, the rule-id and 
  ;;; the new spelling (if any) of the current-record have
  ;;; already been put into an edge
  (if history
      (let* ((current-record (car history))
             (fs (mhistory-fs current-record))
             (new-edge (construct-lex-edge fs (cdr history) nil lex-ids)))
        (push new-edge *morph-records*)
        new-edge)))
    


(defun apply-all-lexical-and-morph-rules (entries)
  ;;; This function applies morphological rules, possibly interleaved with
  ;;; lexical rules, but terminating when the last morphologically significant 
  ;;; rule has been applied, since the parser will take care of the rest
  ;;;
  ;;; entries is a list of mrecords - current-fs, morph-rule-ids, history
  ;;; (the history is nil initially, then a list of mhistory structures)
  ;;;
  ;;; the function returns a list of such mrecords, though the second element
  ;;; will be nil in each case
  ;;;
   (let ((transformed-entries 
            (for entry in entries
               append
               (let ((fs (mrecord-fs entry))
                     (fs-restricted (mrecord-fs-restricted entry))
                     (lex-ids (mrecord-lex-ids entry))
                     (morph-rules (mrecord-rules entry))
                     (history (mrecord-history entry)))
                 (if (>=  (length history) *maximal-lex-rule-applications*)
                   (progn (format t 
                             "~%Warning - probable circular lexical rule") 
                          nil)
                   (append
                      (for rule in (get-matching-lex-rules fs)
                             filter
                             (let ((result (apply-morph-rule 
                                            rule fs fs-restricted nil)))
                               (if result 
                                   (make-mrecord :fs result
                                                 :lex-ids lex-ids
                                           :rules morph-rules 
                                           :history (cons
                                                     (make-mhistory 
                                                      :rule-id (rule-id rule)
                                                      :fs fs
                                                      :new-spelling nil)
                                                     history)))))
                      (if morph-rules
                         (let* ((morph-rule-info (car morph-rules))
                                (new-orth (cadr morph-rule-info))
                                (rule-id (car morph-rule-info))
                                (rule-entry (get-lex-rule-entry rule-id))
                                (result
                                 (if rule-entry
                                     (apply-morph-rule 
                                      rule-entry fs fs-restricted new-orth))))
                           (unless rule-entry
                             (format t 
                                     "~%Warning: rule ~A specified by ~
                                       morphology was not found"
                                     rule-id))
                           (if result 
                             (list (make-mrecord :fs result 
                                                 :lex-ids lex-ids
                                   :rules (cdr morph-rules)
                                   :history (cons (make-mhistory 
                                                      :rule-id rule-id
                                                      :fs fs
                                                      :new-spelling new-orth)
                                                  history))))))))))))
     (if transformed-entries
         (append (remove-if #'mrecord-rules transformed-entries)
            (apply-all-lexical-and-morph-rules 
             (remove-if-not #'mrecord-rules transformed-entries))))))


(defun apply-morph-rule (rule fs fs-restricted new-orth)
   (and
      (restrictors-compatible-p (car (rule-daughters-restricted rule))
         fs-restricted)
      (evaluate-unifications rule (list fs) new-orth)))


(defun activate-context (left-vertex edge right-vertex &optional no-unary)
       (add-to-chart left-vertex edge right-vertex)
       (dolist (rule (get-matching-rules (edge-dag edge) no-unary))
	       	; grammar rule application is attempted when 
                ; we've got all the bits
               (apply-grammar-rule rule
                                   (reverse (rule-daughters-restricted rule))
                                   left-vertex
                                   right-vertex
                                   (list edge))))


(defun add-to-chart (left edge right)
   ;;; Find an existing chart-entry structure if there is one and add a
   ;;; new chart-configuration to it, otherwise build up a new chart-entry
   (let ((item (aref *chart* right)))
      (if item (push (make-chart-configuration :begin left :edge edge)
            (chart-entry-configurations item))
         (setf (aref *chart* right)
            (make-chart-entry 
               :configurations
               (list
                  (make-chart-configuration :begin left :edge edge)))))))
 
(defun apply-grammar-rule (rule rule-restricted-list left-vertex right-vertex
                           child-fs-list)
   ;; Application of a grammar rule 
   ;; Every time an edge is added to the chart, a check is made to see whether
   ;; its addition triggers a rule application. 
   ;; (That is whether it has a category corresponding to the rightmost 
   ;; daughter of a grammar rule - checked before calling apply-grammar-rule
   ;; and whether edges corresponding to the other daughter categories are
   ;; already on the chart.) 
   ;; If yes collect the dags associated with the children, perform
   ;; the unifications specified by the rule, and if the unification(s) 
   ;; succeed, create a new edge (for the mother), record its dag and associated
   ;; information, add this to the chart, and invoke the same process
   ;; recursively.
   (if (restrictors-compatible-p
          (car rule-restricted-list) (edge-dag-restricted (car child-fs-list)))
      (progn
         (incf *check-paths-successes*)
         (if (cdr rule-restricted-list)
            (let ((entry (aref *chart* left-vertex)))
               (when entry
                  (dolist (configuration (chart-entry-configurations entry))
                     (apply-grammar-rule
                        rule
                        (cdr rule-restricted-list)
                        (chart-configuration-begin configuration)
                        right-vertex
                        (cons (chart-configuration-edge configuration) 
                              child-fs-list)))))
            ;; we've got all the bits
            (apply-immediate-grammar-rule rule left-vertex 
                                          right-vertex child-fs-list)))
      (incf *check-paths-fails*)))


(defparameter *debugging* nil)

(defun apply-immediate-grammar-rule (rule left-vertex right-vertex 
                                          child-fs-list)
   ;;; attempt to apply a grammar rule when we have all the parts which
   ;;; match its daughter categories 
   (let ((unification-result
           (evaluate-unifications rule (mapcar #'edge-dag child-fs-list)
               nil child-fs-list)))
      (if unification-result
         (let ((new-edge (make-edge :id (next-edge)
                                   :category (indef-type-of-tdfs 
                                              unification-result)
                                   :rule-number (rule-id rule)
                                   :children child-fs-list
                                   :dag unification-result
                                   :lex-ids 
                                   (mapcan
                                    #'(lambda (child)
                                        (copy-list (edge-lex-ids child)))
                                    child-fs-list)
                                   :leaves
                                   (mapcan
                                    #'(lambda (child)
                                        (copy-list (edge-leaves child)))
                                    child-fs-list))))
              (activate-context left-vertex new-edge right-vertex))
         (when *debugging*
            (format t "~%Unification failure on rule ~A and edges ~:A" 
               (rule-id rule) (mapcar #'edge-id child-fs-list))))))


(defun evaluate-unifications (rule child-fs-list &optional nu-orth child-edges)
   ;; modified for YADU
   ;; 
   ;; An additional optional argument is given. This is the
   ;; new orthography if the unification relates to a morphological
   ;; process. If it is present, it is inserted in the resulting fs.
   ;;  The actual process of unification 
   (let*
      ((current-tdfs (rule-full-fs rule))
       (rule-daughter-order (cdr (rule-order rule)))
       (rule-apply-order (rule-daughters-apply-order rule))
       (n 0)
       (new-orth-fs (if nu-orth (get-orth-tdfs nu-orth))))
       ;; shouldn't do this here because we may not need it
       ;; but otherwise we get a nested unification context error
     ;; - cache the values for a word, so it's not reconstructed 
     ;; only wasted if the morphology is wrong
      (with-unification-context (ignore)
         (dolist (rule-feat rule-apply-order)
            (cond
               ((eql (incf n) 1))
               ((x-restrict-and-compatible-p
                   (if (listp rule-feat)
                      (x-existing-dag-at-end-of 
                       (tdfs-indef current-tdfs) rule-feat)
                      (x-get-dag-value (tdfs-indef current-tdfs) rule-feat))
                   (edge-dag-restricted
                      (nth (position rule-feat rule-daughter-order) 
                           child-edges)))
                  (incf *check-paths-successes*))
               (t
                  (incf *check-paths-fails*)
                  (return-from evaluate-unifications nil)))
            (incf *parse-unifs*)
           (unless
               (setf current-tdfs
                 (yadu current-tdfs
                       (create-temp-parsing-tdfs
                        (nth 
                         (position rule-feat rule-daughter-order) 
                         child-fs-list)
                        rule-feat)))
               (incf *parse-fails*)
               (return-from evaluate-unifications nil)))
         ;; if (car (rule-order rule)) is NIL - tdfs-at-end-of
         ;; will return the entire structure
         (let ((result (tdfs-at-end-of (car (rule-order rule)) current-tdfs)))
           (when new-orth-fs
             (setf result
               (yadu result new-orth-fs))) 
           (when result
               ;; delete arcs just holding constituents' feature 
               ;; structures - before copying
               ;; otherwise their copies would be thrown away immediately
               ;; we have to check whether any of the deleted dags 
               ;; contain a cycle -
               ;; if so then the whole rule application should fail
             (let* ((real-dag (deref-dag (tdfs-indef result)))
                    (new (clone-dag real-dag))
                    (arcs-to-check nil))
               (flet ((member-with-cyclic-check (arc)
                        (when (member (dag-arc-attribute arc) 
                                      *deleted-daughter-features*)
                          (push arc arcs-to-check)
                          t)))
                 (setf (dag-arcs new)
                   (remove-if #'member-with-cyclic-check (dag-arcs new)))
                 (setf (dag-comp-arcs new)
                   (remove-if #'member-with-cyclic-check 
                              (dag-comp-arcs new)))
                 ;; take advantage of the fact that removed 
                 ;; arcs might share structure
                 ;; by checking them all at once
                 (if (cyclic-dag-p (make-dag :type *toptype* 
                                             :arcs arcs-to-check))
                     (progn (incf *parse-fails*) nil)
                   (progn
                     ;; (setf (dag-copy new) 'copy)
                     (setf (dag-forward real-dag) new)
                     (copy-tdfs-elements result))))))))))


(defun create-temp-parsing-tdfs (tdfs flist)
  (if (null flist) tdfs
    (let ((indef-dag (create-dag))
          (tail nil))
      (unify-list-path flist indef-dag (tdfs-indef tdfs))
      (when (tdfs-tail tdfs)
        (let ((path (create-path-from-feature-list (listify flist))))
          (for tail-element in (tdfs-tail tdfs)
               do
               (push (add-path-to-tail path tail-element) tail))))
      (make-tdfs :indef indef-dag :tail tail))))

(defun get-orth-tdfs (str)
  (or (cdr (assoc str *cached-orth-str-list* :test #'equal))
      (let ((new-orth-tdfs (make-orth-tdfs str)))
        (push (cons str new-orth-tdfs) *cached-orth-str-list*)
        new-orth-tdfs)))

;;; evaluate-unifications-with-fail-messages - temporarily removed

(defun find-spanning-edges (start-vertex end-vertex)
   ;;; Returns all edges between two vertices and checks for
   ;;; root conditions - used to see if a parse has been found. 
   (let ((start-symbols (if (listp *start-symbol*)
                                      *start-symbol*
                                    (list *start-symbol*)))
         (chart-index
            (aref *chart* end-vertex)))
      (if chart-index
        (for item in (chart-entry-configurations chart-index)
             append
             (if (eql (chart-configuration-begin item) start-vertex)
;;; root may be a list of (td)fs 
;;; with the interpretation that if any of them match
;;; the parse is OK
                 (if (null *start-symbol*)
                     (list (chart-configuration-edge item))
                   (if *substantive-roots-p*
                       (create-new-root-edges item start-symbols
                                              start-vertex end-vertex)
                     (filter-root-edges item start-symbols))))))))

(defun filter-root-edges (item start-symbols)
  (dolist (start-symbol start-symbols)
    (let ((root-spec (get-tdfs-given-id start-symbol)))
         (when root-spec
             (when (yadu root-spec
                         (edge-dag 
                          (chart-configuration-edge item)))
               (return (list (chart-configuration-edge item))))))))

(defun create-new-root-edges (item start-symbols start-vertex end-vertex)
  (for start-symbol in start-symbols        
       filter
       (let ((tdfs (get-tdfs-given-id 
                    start-symbol)))
         (if tdfs
            (let ((unif
                    (yadu tdfs
                          (edge-dag 
                            (chart-configuration-edge item)))))
               (if unif
                   (let ((new-edge
                          (make-edge :dag (copy-tdfs-elements unif)
                                     :id (next-edge)
                                     :category
                                     (indef-type-of-tdfs unif)
                                     :rule-number start-symbol
                                     :children 
                                     (list (chart-configuration-edge item))
                                     :lex-ids (edge-lex-ids
                                               (chart-configuration-edge item))
                                     :leaves
                                     (edge-leaves 
                                      (chart-configuration-edge item)))))
                     (add-to-chart start-vertex
                                   new-edge
                                   end-vertex)
                     new-edge)))))))


;;; TTY printout of chart
;;; chart edges are ordered on: right vertex, left vertex, edge id

(defun print-chart nil 
   (format t "~% > chart dump:~%")
   (dotimes (vertex (- *chart-limit* 1))
      (unless 
         (print-chart-entry (+ 1 vertex) (aref *chart* (+ 1 vertex)))
         (return nil)))
   (terpri))

(defun print-chart-entry (vertex item)
   (when item 
      (terpri)
      (dolist
         (configuration
            (sort (copy-list (chart-entry-configurations item))
               #'(lambda (span1 span2)
                   (cond
                      ((eql (chart-configuration-begin span1)
                          (chart-configuration-begin span2))
                         (< (edge-id (chart-configuration-edge span1))
                            (edge-id (chart-configuration-edge span2))))
                      (t
                        (< (chart-configuration-begin span1)
                           (chart-configuration-begin span2)))))))
         (print-chart-configuration configuration vertex))
      t))

(defun print-chart-configuration (span right-vertex)
   (let ((e (chart-configuration-edge span)))
      (format t "~A-~A [~A] ~A => ~A~A  [~{~A~^ ~}]~%"
         (chart-configuration-begin span)
         right-vertex
         (edge-id e)
         (edge-category e)
         (edge-leaves e)
         (if (chart-configuration-roots span) "*" "")
         (mapcar #'edge-id (edge-children e)))))


;;; Parsing sentences from file

(defun parse-sentences (&optional input-file parse-file result-file)
   (unless input-file 
      (setq input-file (ask-user-for-existing-pathname "Sentence file?")))
   (when
      (and input-file
         (or (probe-file input-file)
            (error "Input file ~A does not exist" input-file)))
      (with-open-file (istream input-file :direction :input)
         (let ((line (read-line istream nil 'eof)))
            (cond
               ((eq line 'eof))
               ((eql (count #\@ line) 11) ; must be 12 fields in tsdb input
                  (parse-tsdb-sentences1 istream line parse-file result-file))
               (t
                  (batch-parse-sentences istream line parse-file)))))))


(defparameter *do-something-with-parse* nil)

(defun batch-parse-sentences (istream raw-sentence parse-file)
  (let* ((output-file 
            (or parse-file (ask-user-for-new-pathname "Output file?")))
         (start-time (get-universal-time)))
     (unless output-file (return-from batch-parse-sentences nil))
     (with-open-file (ostream output-file :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
        (loop
           (when (eql raw-sentence 'eof) (return))
           (format ostream "~A~%" raw-sentence)
           (finish-output ostream)
           (let ((sentence (string-trim '(#\Space #\Tab) raw-sentence)))
              (unless (equal sentence "")
                 (let ((user-input 
                        (split-into-words 
                         (preprocess-sentence-string sentence))))
                    (cond
                       ((> (length user-input) *chart-limit*)
                          (format ostream 
                                  "~%can't parse this length of sentence") 
                          (finish-output ostream)
                          nil)
                       (t
                          (let ((*safe-not-to-copy-p* t)
                                (*parse-unifs* 0) (*parse-fails* 0))
                             (clear-chart)
                             #+powerpc(setq aa 0 bb 0 cc 0 dd 0 ee 0 ff 0 gg 0 hh 0 ii 0 jj 0)
                             (add-morphs-to-morphs user-input)
                             (add-words-to-chart)
                             (setf *parse-record*
                                (find-spanning-edges 0 (length user-input)))
                             (when (fboundp *do-something-with-parse*)
                                (funcall *do-something-with-parse*))
                             (let ((n (length *parse-record*)))
                                (format ostream "  ~R parse~:[s~;~] found~%" 
                                        n (= n 1)))
                             (finish-output ostream)))))))
           (setq raw-sentence (read-line istream nil 'eof)))
        (format ostream "Total elapsed time: ~A secs~%" 
                (- (get-universal-time) start-time)))))


;;; extracting a list of lexical entries used in a parse
;;; used for testing the generation lexical lookup algorithm

(defun retrieve-lex-from-parses nil
  (for edge in *parse-record*
       collect
       (edge-lex-ids edge)))

; (collect-parse-base (car *parse-record*))

(defun collect-parse-base (edge-rec)
  ;;; takes a top edge, returns a list of 
  ;;; lexical identifiers, unary-rule-list pairs
  (if (or (cdr (edge-lex-ids edge-rec))
          (not (get-lex-rule-entry (edge-rule-number edge-rec))))
      (for child in (edge-children edge-rec)
           append
           (collect-parse-base child))
    (list (cons (car (edge-lex-ids edge-rec))
          (nreverse (collect-unary-rule-names edge-rec))))))

(defun collect-unary-rule-names (edge-rec)
  (when (cdr (edge-children edge-rec))
    (error "~%Should be unary edge ~A" edge-rec))
  (if (edge-children edge-rec)
    (cons (edge-rule-number edge-rec)
              (collect-unary-rule-names (car (edge-children edge-rec))))
    (if (edge-morph-history edge-rec)
        (cons (edge-rule-number edge-rec)
              (collect-morph-history-rule-names 
               (edge-morph-history edge-rec))))))

(defun collect-morph-history-rule-names (edge-rec)
  (if (edge-morph-history edge-rec)
      (cons (edge-rule-number edge-rec)
            (collect-morph-history-rule-names 
             (edge-morph-history edge-rec)))))

        

(defun preprocess-sentence-string (str)
  ;;; to be defined by the user
    str)


      
      


