;;; Copyright (c) 2003--2004
;;;   John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen;
;;;   see `licence.txt' for conditions.

(in-package :mrs)

;;; interconversion between RMRS and MRS
;;; 1. MRS to RMRS 
;;; 2. RMRS to MRS (needs SEM-I)

;;; convert an MRS structure to an RMRS  

;;; settings for qa
;;; (defparameter *unknown-word-types* '(n_proper_le))
;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs)
;;;
;;; (defparameter lkb::*do-something-with-parse* 'mrs::batch-output-rmrs-only)

(defparameter *mrs-to-rmrs-conversion-warnings* nil)

(defun warn-rmrs-problem (str)
  #+:lkb  (push lkb::*parse-input* *mrs-to-rmrs-conversion-warnings*)
  (push str *mrs-to-rmrs-conversion-warnings*))

(defparameter *qa-count* 1)

#+:lkb
(defun batch-output-rmrs nil
  (let ((sentence lkb::*parse-input*)
        (ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (format ostream "~%<S id='~A'>" *qa-count*)
    (setf *qa-count* (+ 1 *qa-count*))
    (if sentence
        (format ostream
                "~%<string>~%~S~%</string>" sentence)
      (format ostream
              "~%<string></string>"))
    (format ostream
            "~%<tree></tree>")
    ;;; for rasp output compatibility
    (if *parse-record*
          (let* ((parse (car *parse-record*))
                 (mrs-struct (extract-mrs parse))
                 (rmrs-struct 
                    (mrs-to-rmrs mrs-struct)))
            (output-rmrs1 rmrs-struct 'xml ostream))
      ; (format ostream
					;        "~%<rmrs></rmrs>")
      )
    (format ostream "</S>~%")
    (finish-output ostream)))

#+:lkb
(defun batch-output-rmrs-only nil  
  (let ((ostream (if (and lkb::*ostream* 
                          (streamp lkb::*ostream*) 
                          (output-stream-p lkb::*ostream*)) 
                     lkb::*ostream*  t)))
    (if *parse-record*
          (let* ((parse (car *parse-record*))
                 (mrs-struct (extract-mrs parse))
                 (rmrs-struct 
                    (mrs-to-rmrs mrs-struct)))
            (output-rmrs1 rmrs-struct 'xml ostream))
;      (format ostream
					;              "~%<rmrs></rmrs>")
      )
    (finish-output ostream)))


;;; Full MRS to RMRS conversion

(defun mrs-to-rmrs (mrs)
  (if *rel-handel-path*
      (progn
        (initialize-rmrs-variables-plus)
        (let ((lzt (psoa-liszt mrs))
              (new-lzt nil)
              (new-args nil)
              (label-recs nil)
	      (ings nil)
	      )
          (dolist (rel lzt)
            (multiple-value-bind (ep rmrs-args new-label-recs)
                (parsonify-rel rel label-recs)
              (push ep new-lzt)
              (setf new-args (append new-args rmrs-args))
	      (unless *anchor-rmrs-p*
		(setf label-recs new-label-recs))))
	  (unless *anchor-rmrs-p*
	    (setf ings (construct-converted-in-groups label-recs)))
          (make-rmrs   :top-h (psoa-top-h mrs)
                       :h-cons (construct-converted-hcons 
				(psoa-h-cons mrs) ings)
                       :liszt (nreverse new-lzt)
                       :in-groups (unless *anchor-rmrs-p* ings)
                       :rmrs-args new-args
                       :origin :erg)))))



#|
bindings aren't set, since the assumption is that all variable
equalities are known.  So the code simply has to walk down the list
of rels in the lzt, converting them to simple eps plus rmrs-args
|#

(defparameter *rmrs-ignore-features* '("DIM"))

(defstruct ing-info
  label type new-label-type-pairs)

(defun parsonify-rel (rel label-recs)
  ;;; label-recs is a list of ing-info structures
  ;;; so that we can have a canonical notion of in-group
  (let* ((pred (rmrs-convert-pred (rel-pred rel)))
	 (rel-type (unless *anchor-rmrs-p* (determine-ing-rel-type pred)))
         (flist (rel-flist rel))
	 (main-arg (fvpair-value (car flist)))
         (converted-main-arg (if (var-p main-arg)
				 (rmrs-convert-variable main-arg)
			       (progn (warn-rmrs-problem 
				       (format nil "~A as main argument" main-arg))
				      main-arg)))
         (label (rel-handel rel))
	 (label-id (var-id label))
	 (label-match (unless *anchor-rmrs-p*
			(dolist (label-rec label-recs)
			  (when (eql label-id 
				     (var-id (ing-info-label label-rec)))
			    (return label-rec)))))
         (new-label (if label-match ;; conjunction
                        (create-new-rmrs-var 
			 "h" *rmrs-variable-generator* nil)))
	 (anchor (if *anchor-rmrs-p*
		     (create-new-rmrs-var 
			 "h" *rmrs-variable-generator* nil)))
         (rmrs-args
          (if (cdr flist)
              (loop for fvpair in (cdr flist)
                  nconc
                    (let ((feat (fvpair-feature fvpair))
                          (val (fvpair-value fvpair)))
                      (unless (or 
                               (and (var-p val) 
                                    (equal (var-type val) "u"))
			       ;;; remove any optional arguments
                               (member (string feat) *rmrs-ignore-features*
                                       :test #'equal))
                        (list (make-rmrs-arg 
                               :arg-type (string feat)
                               :label (if  *anchor-rmrs-p*
					  anchor 
					(or new-label label))
                               :val (if (var-p val)
					(rmrs-convert-variable val)
				      val))))))))
         (ep 
          (make-rel
           :handel (if *anchor-rmrs-p* label
		     (or new-label label))
	   :anchor anchor
           :parameter-strings (rel-parameter-strings rel)
           :extra (rel-extra rel)
           :pred pred 
           :flist (list converted-main-arg)
           :str (rel-str rel)
	   :cfrom (if (integerp (rel-cfrom rel)) 
		      (rel-cfrom rel)
		    (if (and (rel-lnk rel) 
			     (eql (car (rel-lnk rel)) :CHARACTERS))
			(cadr (rel-lnk rel))))
	   :cto (if (integerp (rel-cto rel)) 
		    (rel-cto rel)
		  (if (and (rel-lnk rel) 
			     (eql (car (rel-lnk rel)) :CHARACTERS))
			(caddr (rel-lnk rel)))))))
    (values ep rmrs-args 
	    (if (not *anchor-rmrs-p*)
	    (record-ing-info label-recs 
			     label new-label rel-type label-match)))))

(defun record-ing-info (label-recs label new-label rel-type
			      label-match)
  (if label-match
      (progn
	(push (cons new-label rel-type) 
	      (ing-info-new-label-type-pairs label-match))
	label-recs)
    (cons (make-ing-info :label label :type rel-type)
	  label-recs)))

(defun construct-converted-in-groups (label-recs)
  (let ((ings nil))
    (dolist (label-rec label-recs)
      (let ((matches (ing-info-new-label-type-pairs label-rec)))
	(when matches
          (when (ing-info-label label-rec)
            (let* ((var-pairs (cons (cons (ing-info-label label-rec)
                                          (ing-info-type label-rec))
                                    matches))
                   (sorted-pairs (sort var-pairs #'ing-rel-type-greater-p 
                                       :key #'cdr))
                   (top-rank-var (caar sorted-pairs)))
              (dolist (pair (cdr sorted-pairs))
                (push
                 (make-in-group :label-a top-rank-var
                                :label-b (car pair))
                 ings)))))))
     ings))


(defun construct-converted-hcons (hcons ings)
  (if (null ings)
      hcons
    (loop for hcons-el in hcons
	collect
	  (let* ((hcons1 (hcons-scarg hcons-el))
		 (hcons2 (hcons-outscpd hcons-el))
		 (hcons-rel (hcons-relation hcons-el))
		 (ing-match
		  (loop for ing-el in ings
		      thereis 
			(if (eq (in-group-label-b ing-el) hcons2)
			    (in-group-label-a ing-el)))))
	    (if ing-match
		(make-hcons :relation hcons-rel :scarg hcons1
			    :outscpd ing-match)
	      hcons-el)))))

  
(defun rmrs-convert-pred (pred)
  ;;; the pred should obey the format:
  ;;; _lemma_pos_sense_rel
  ;;; or
  ;;; _lemma_pos_rel
  ;;; if the senses are not distinguished
  ;;;
  ;;; If there is no leading underscore, the pred
  ;;; is not decomposed
  ;;; 
  ;;; For unknown word handling, underscores may be escaped
  (let* ((str (string-downcase (string pred))))
    ;;; parse the string - hacky ...
    (if (not (eql (elt str 0) #\_))
        str
      (let*
          ((uscore-pos2 (next-pred-splitter-position str 1))
           (uscore-pos3 
            (if uscore-pos2
		(next-pred-splitter-position str (+ 1 uscore-pos2))))
           (uscore-pos4
            (if uscore-pos3
		(next-pred-splitter-position str (+ 1 uscore-pos3))))
           (remainder (cond (uscore-pos4
                             (subseq str uscore-pos4))
                            (uscore-pos3
                             (subseq str uscore-pos3))
                            (t nil))))
        (if (not (equal remainder "_rel"))
              ;;; we're missing the _rel
              ;;; nasty so just output what we've got
	    (progn (warn-rmrs-problem str)
		   str)
          (make-realpred :lemma (subseq str 1 uscore-pos2)
                         :pos (subseq str (+ 1 uscore-pos2) uscore-pos3)
                         :sense (if uscore-pos4
                                   (subseq str (+ 1 uscore-pos3) uscore-pos4))))))))


(defun next-pred-splitter-position (str start)
  ;;; returns the position in the string of the
  ;;; first unescaped underscore
  (loop 
    (let ((pos (position #\_ str :start start)))
    ;;; start is 1 or more
      (unless pos (return))
      (when (not (eql (elt str (- pos 1)) #\\))
	  (return pos))
      (setf start (+ 1 pos)))))


;;; the conversion of extra values is grammar specific
;;; (and there's no guarantee that it can be done one-to-one)
;;;
;;; the table should be bidirectional - i.e. work for
;;; RMRS to MRS too


;;; *var-extra-conversion-table* - now defined in mrsglobals.lisp

;;; `compiled' table

(defstruct var-conversion-table 
  simple compleks)


;;; tables is defined as a structure with simple and compleks
;;; entries, where simple is defined as simplex on the input
;;; side (one fvp) and compleks is a conjunction on the input side
;;; (list of fvps)
;;; Output will be a list of fvps (possibly singleton) 
;;; in either case.


;;; function for compilation of table

(defun compile-var-extra-table (table rmrs-in-p)
  (let ((simple nil)
	(compleks nil))
    (dolist (entry table)
      (let* ((input (if rmrs-in-p 
			(cdr entry)
		      (car entry)))
	     (output (if rmrs-in-p 
			(car entry)
		       (cdr entry)))
	     (new-output (make-var-table-output-side output rmrs-in-p)))
	(if (eql (car input) 'AND)
	    (push
	     (cons 
	      (loop for fvp in (cdr input)
		  collect 
		  (make-fvp-input-side (car fvp) (cadr fvp) rmrs-in-p))
	      new-output)
	     compleks)
	  (push
	   (cons (make-fvp-input-side (car input) 
				      (cadr input)
				      rmrs-in-p)
	    new-output)
	   simple))))
    (make-var-conversion-table :simple (nreverse simple)
			       :compleks (nreverse compleks))))

(defun make-fvp-input-side (feature value rmrs-in-p)
  (let ((fstr (format nil "~A"  feature))
	(vstr (format nil "~A"  value)))
    (make-extrapair :feature (if rmrs-in-p (string-downcase fstr) (vsym fstr))
		    :value (if rmrs-in-p (string-downcase vstr) (vsym vstr)))))


(defun make-var-table-output-side (out rmrs-in-p)
  (loop for fvp in (if (eql (car out) 'AND) 
		       (cdr out)
		     (list out))
      collect
	(let ((fstr (format nil "~A"  (car fvp)))
	      (vstr (format nil "~A"  (cadr fvp))))
	  (make-extrapair :feature (if rmrs-in-p (vsym fstr) 
				     (string-downcase fstr))
			  :value (if rmrs-in-p (vsym vstr) 
				   (string-downcase vstr))))))

;;; end compilation of table


(defparameter *var-extra-mrs-compiled-table*
    nil)

;;; was
;;;    (compile-var-extra-table *var-extra-conversion-table* nil))
;;; but this has all ceased to work / become redundant

;;; conversion functions - mostly generic for MRS <-> RMRS
;;; calling function supplies the right table


(defun rmrs-convert-variable (var)
  (make-var :type (if (member (var-type var) '("x" "e" "h" "u" "l")
                              :test #'equal)
                      (var-type var)
                    "u")
            :id (var-id var)
	    :extra (if *var-extra-mrs-compiled-table*
		       (rmrs-convert-var-extra 
			(var-extra var) 
			*var-extra-mrs-compiled-table*)
		     (var-extra var))))


(defun rmrs-convert-var-extra (extras table)
  (let ((converted nil)
	(to-do-list nil))
    (dolist (extra extras)
      (let ((simple-match
	      (simple-var-extra-check
	       extra
	       (var-conversion-table-simple table))))
	(if simple-match
	  (setf converted (append converted simple-match))
	  (push extra to-do-list))))
    (multiple-value-bind (complex-match-results left-overs)
	(complex-var-extra-check to-do-list 
				 (var-conversion-table-compleks
				  table))
      (when complex-match-results
	(setf converted (append converted complex-match-results)))
      (dolist (remainder left-overs)
	(format t "~%~A unconverted" remainder)))
    converted))
	  
(defun simple-var-extra-check (extra table)
  (let ((feat (extrapair-feature extra))
	(val (extrapair-value extra)))
    (dolist (entry table)
      (when (and (equal feat (extrapair-feature (car entry)))
		 (equal val (extrapair-value (car entry))))
	(return (cdr entry))))))
    
(defun complex-var-extra-check (remainder table)
  (let ((converted nil))
    (dolist (entry table)
      (unless remainder (return))
      (let ((matching (match-var-complex-entry (car entry) 
					       remainder nil)))
	(when matching
	  (dolist (match matching)
	    (setf remainder (remove match remainder)))
	  (setf converted
	    (append (cdr entry) converted)))))
    converted))
   
(defun match-var-complex-entry (fvlist extras matches)
  ;;; go through fvlist until all matched
  ;;; returning items that have matched
  (if fvlist
      ;; got something left that needs matching
      (if extras
	  (let* ((to-match (car fvlist))
		 (feat (extrapair-feature to-match))
		 (val (extrapair-value to-match))
		 (match-in-extras 
		  (dolist (extra extras)
		    (when (and (equal feat (extrapair-feature extra))
			       (equal val (extrapair-value extra)))
		      (return extra)))))
	     (if match-in-extras
		 (match-var-complex-entry (cdr fvlist)
					  (remove match-in-extras extras)
					  (cons match-in-extras matches))
	       nil))
	nil) ; no match
    matches ; nothing left, so return matching extras
    ))

;;; **************************************************
;;; ING handling
;;; **************************************************

#|

It is convenient to have a notion of a canonical set of IN-Gs
This can be regarded in the same way as the set of qeqs - although
different groups of qeqs might semantically result in the same thing,
if we assume consistent composition principles, we'll get the same set
of qeqs for comparable strings.  The same is true of IN-Gs - although
semantically they (probably) just correspond to conjunction, we
can compare much more efficiently if we assume rules about how they are 
constructed.  This also allow us to play games with underspecification
and possibly change the interpretation of IN-Gs, but this is ignored for
now.

The assumption is that whenever an IN-G is created, it corresponds to
a situation where one label can be considered `higher'.
Higher could be defined in several ways (and I think it's really arbitrary), 
but let's assume that the modifiee is always `higher' 
(this makes sense because it may have several modifiers)
and that a semantic head is always higher otherwise.				This is a bit complicated here in the context of conversion from the ERG
because we don't know what the composition rules are, so we have to
try and simulate on the basis of which label comes from a more `major' relation.
Code here allows record of this for debugging.  Main problem is working
out the hierarchy on grammar preds.  However, in practice we can 
limit this to the cases which occur in the RASP-RMRS code for now.

Errors won't be devastating anyway ...
|#

(defun determine-ing-rel-type (pred)
  (if (realpred-p pred)
      (realpred-pos pred)
    pred))

;;; FIX to treat grammar preds
;;; probably by reference to type hierarchy

(defparameter *ing-ranking*
    '(("n" "v" "a" "r" "j" "p" "prpstn_m_rel" "poss_rel")
      ("v" "p" "r")))
      

(defun ing-rel-type-greater-p (type1 type2)
  (let* ((outrank-rec (assoc type1 *ing-ranking* :test #'equal))
	 (outrank-p
	  (and outrank-rec
	       (member type2 (cdr outrank-rec) :test #'equal))))
;;;    (format t "~%Outranks ~A ~A: ~A" type1 type2 outrank-p)
    outrank-p))


;;; RMRS to MRS conversion

;;; Currently this is for purposes of generation.  The conversion
;;; is done with respect to a SEM-I for some grammar, so as the MRS
;;; is constructed, it is checked for well-formedness.  
;;; Because it is likely that _all_ errors will be required,
;;; errors are accumulated by the various functions

(defun report-rmrs-conversion-problems (problems)
  (dolist (problem problems)
    (format t "~%~A" problem)))

;;; rmrs table - uses same global as MRS->RMRS conversion
;;; but rearranges it so same lookup code can be used for both

(defparameter *var-extra-rmrs-compiled-table* nil)
;;;    (compile-var-extra-table *var-extra-conversion-table* t))


;;;
;;; The current semi is read in via 
;;; (mt:read-semi "~/delph-in/erg/trunk/erg.smi")
;;; 

;;; (convert-rmrs-to-mrs *rmrs-debug*)

(defparameter *already-converted-rmrs-variables* nil)

(defun convert-rmrs-to-mrs (rmrs)
  (setf *already-converted-rmrs-variables* nil)
  (let ((top-h (rmrs-top-h rmrs))
	(index (make-var :type "i"
			 :id (funcall *variable-generator*)))
	;;; we need to hallucinate an index because otherwise the
	;;; transfer code (which is responsible for the null semantics
	;;; guys in generation) will fail.  Luckily it doesn't seem to matter
	;;; what the index is.
	(h-cons (rmrs-h-cons rmrs)))
    (multiple-value-bind
	(liszt problems) 
	 (convert-rmrs-liszt-to-mrs (rmrs-liszt rmrs)
				    (rmrs-rmrs-args rmrs))
      (if problems
	  (report-rmrs-conversion-problems problems)
	(make-psoa  :top-h top-h
		    :index index
		    :h-cons h-cons
		    :liszt liszt)))))
		
(defun convert-rmrs-liszt-to-mrs (eps rargs)
  (let* ((problems nil)
	 (new-eps
	  (loop for ep in eps
	      collect
		(multiple-value-bind (new-ep ep-problems)
		    (convert-rmrs-ep-to-mrs 
		     ep
		     (loop for rarg in rargs
			 when (eql-var-id (rmrs-arg-label rarg) 
					  (rel-anchor ep))
			 collect rarg))
		  (setf problems (append problems ep-problems))
		  new-ep))))
    (values new-eps problems)))
	    

(defun convert-rmrs-ep-to-mrs (ep rargs)
  (let* ((problems nil)
	 (rmrs-pred (rel-pred ep))
	 (mrs-pred (convert-rmrs-pred-to-mrs rmrs-pred))
	 (semi-pred (find-semi-entries mrs-pred)))
    (if semi-pred
	(let ((new-ep
	      (make-rel
	       :handel (rel-handel ep)
	       :parameter-strings (rel-parameter-strings ep)
               :extra (rel-extra ep)
	       :pred semi-pred
	       :flist (cons (convert-rmrs-main-arg (car (rel-flist ep)))
			    (loop for rarg in rargs
				collect
				  (deparsonify rarg)))
               :str (rel-str ep)
	       :cfrom (rel-cfrom ep)
	       :cto (rel-cto ep))))
	  (values new-ep problems))
      (values nil
	      (list (format nil "No entry found in SEM-I for ~A" 
			    rmrs-pred))))))


(defun convert-rmrs-main-arg (var)
  (make-fvpair :feature (vsym "ARG0")
	       :value (convert-rmrs-to-mrs-variable var)))

(defun deparsonify (rarg)
  (make-fvpair :feature (vsym (rmrs-arg-arg-type rarg))
	       :value 
	       (let ((val (rmrs-arg-val rarg)))
		 (if (var-p val)
		     (convert-rmrs-to-mrs-variable val)
		   val))))



(defun convert-rmrs-to-mrs-variable (var)
  (let ((done (assoc var *already-converted-rmrs-variables*)))
    (or (cdr done)
	(let ((new-var
		   (make-var :id (var-id var)
			     :type (var-type var)
			     :extra (if *var-extra-rmrs-compiled-table*
					(rmrs-convert-var-extra 
					 (var-extra var) 
					 *var-extra-rmrs-compiled-table*)
				      (var-extra var)))))
	  (push (cons var new-var) *already-converted-rmrs-variables*)
	  new-var))))

(defun convert-rmrs-pred-to-mrs (pred)
  (if (realpred-p pred)
      (concatenate 'string
	(convert-realpred-to-string 
	 (realpred-lemma pred)
	 (realpred-pos pred)
	 (realpred-sense pred))
	"_rel")
    pred))



;;; takes a string and tries to look it up in the SEMI

(defun find-semi-entries (pred)
;;; code adapted from test-semi-compliance
  (unless mt::*semis*     (error "Semis not initialised"))
  (let* ((semi (first mt::*semis*))
	 (pred-symbol (vsym (string-upcase pred))))
    (if
	(or
	 (member pred-symbol mt::*semi-fragment-relations* :test #'eq)
	 (member
	  pred-symbol
	  mt::*semi-punctuation-relations* :test #'eq)
	 (member pred-symbol mt::*semi-token-relations* :test #'eq)
	 (mt::lookup-predicate pred-symbol semi))
	pred-symbol
      (if (mt::lookup-predicate pred semi)
	  pred
	nil))))



  
;;; Generation

;;; this can be called from generate-rmrs-from-emacs
;;; see ACL_specific/emacs.lsp

#+:lkb
(defun generate-from-rmrs (rmrs)
  (let ((mrs (convert-rmrs-to-mrs rmrs)))
    (when mrs
      ;;; (mrs-quick-check-lex-retrieval mrs)
      ;;; FIX
      (let ((lkb::*bypass-equality-check* t))
	(lkb::generate-from-mrs mrs)
	)
      (lkb::show-gen-result))))

;;; Utility fn

(defun rmrs-for-sentence (input parse-number)
  (let ((lkb::*show-parse-p* nil))
    (lkb::do-parse-tty input)
    (unless lkb::*parse-record*
      (error "Parse failed"))
    (let ((selected-parse (nth (- parse-number 1) lkb::*parse-record*)))
      (unless selected-parse
	(error "Incorrect parse number"))
      (let ((mrs (mrs::extract-mrs selected-parse)))
	(unless mrs (error "~%Can't extract MRS"))
	(mrs::mrs-to-rmrs mrs)))))

;;; 
