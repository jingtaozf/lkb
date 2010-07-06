;;; Copyright (c) 1998-2001 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen
;;; see LICENSE for conditions

(in-package "MRS")

;;; Time conversion
;;; takes an MRS representing a clock time and converts it into
;;; a VIT-compatible ctime format
;;;
;;; Optional (and limited) cleverness in doing am/pm on the basis of
;;; morning/afternoon expressions

;;;
;;; numbered_hour_rel takes four (non-handel) arguments
;;; INST (an index)
;;; AM-PM (coindexed with am / pm rel if present)
;;; HOUR (oblig - integer - assumed to be using a 12 hour clock)
;;; MIN (coindexed with a minute_rel in expressions like two forty five)
;;;
;;; ctime_rel takes three (non-handel) arguments
;;; INST (an index)
;;; HOUR (oblig - integer or integer/integer when am/pm is ambiguous)
;;; MIN  (oblig - integer)
;;;
;;; HANDEL and INST values for ctime are always identical to
;;; those for nh_rel
;;;
;;; nh-rel, minute_rel, after_hour_rel, before_hour_rel, am_rel, pm_rel
;;; should all be removed and replaced by ctime_rel
;;;
;;; Issues
;;; a) determining the value for MIN in various circumstances
;;;    (should always be found - default is 0)
;;; b) determining the am/pm value (may not be possible)
;;;
;;; Case 1 - e.g. "at three"
;;;
;;; numbered_hour_rel alone present
;;; ctime HOUR = nh_rel HOUR / +12
;;; ctime MIN = 0
;;;
;;; Case 1a - e.g. "at three pm"
;;;
;;; as above, but with am/pm as well - am/pm rel
;;; coindexed to the AM/PM slot in numbered_hour_rel
;;; AM => ctime HOUR = nh_rel HOUR
;;; PM => ctime HOUR = nh_rel HOUR+12
;;;
;;; Case 3 - e.g. "at three forty five"
;;;
;;; minute_rel coindexed with MIN slot of numbered_hour_rel
;;; ctime MIN = minute_rel MINUTE
;;;
;;; Case 3a - e.g. "at three oclock"
;;;
;;; same as above
;;;
;;; Case 4 - e.g. "half past three"
;;;
;;; nhrel (h1 x y-pm number z-min)
;;; after_hour_rel (h2 arg1 x w)
;;; minute_rel (h3 w 30)
;;;
;;; ctime(h1 x number/+12 30)
;;;
;;; Case 4a - e.g. a quarter to three
;;;
;;; nhrel (h1 x y-pm number z-min)
;;; before_hour_rel (h2 arg1 x w)
;;; minute_rel (h3 w 15)
;;;
;;; ctime(h1 x number/+12 45)
;;;
;;; case 6 - e.g. a quarter past
;;; ??? Speculation
;;;
;;; after_hour_rel (h2 x v w)
;;; minute_rel (h3 w 15)
;;; 
;;; here there is no numbered_hour-rel so the
;;; ctime is based on the after/before_hour rel
;;; 
;;; ctime(h2 x UNKNOWN 15)



;;; AM/PM heuristics -
;;; preposition(x y) and am/pmdeterminer(x) and nhrel(y)
;;; or
;;; preposition(x y) and am/pmdeterminer(y) and nhrel(x)
;;; where am/pmdeterminer is morning_rel, etc

;;; UTILITY FN

(defun get-rel-feature-value (rel feature)
  ;;; assumes only occurs once
  (when (rel-p rel)
    (dolist (fvpair (rel-flist  rel))
      (when (eql (fvpair-feature fvpair) feature)
        (return (fvpair-value fvpair))))))

;;; GRAMMAR SPECIFIC GLOBALS AND UTILITIES

(defparameter *nhr-inst-feature* (vsym "INST"))
(defparameter *nhr-ampm-feature* (vsym "AM-PM"))
(defparameter *nhr-hour-feature* (vsym "HOUR"))
(defparameter *nhr-min-feature* (vsym "MIN"))
(defparameter *min-min-feature* (vsym "CONST_VALUE"))
(defparameter *min-index-feature* (vsym "ARG"))
(defparameter *ampm-index-feature* (vsym "INST")) 
(defparameter *inst-feature* (vsym "INST"))
(defparameter *rel-hour-feature* (vsym "HOUR-IND"))
(defparameter *rel-minute-feature* (vsym "MINUTE-IND"))
(defparameter *prep-prep-feature* (vsym "ARG3"))
(defparameter *prep-arg-feature* (vsym "ARG"))



(defun create-ctime-rel (handel inst hour minutes)
  (make-rel
   :pred (vsym "CTIME_REL")
   :handel handel
   :flist
   (list (make-fvpair :feature (vsym "INST") :value inst)
         (make-fvpair :feature (vsym "HOUR") :value hour)
         (make-fvpair :feature (vsym "MIN") :value minutes))))


(defun nhrel-p (pred)
  (eql pred (vsym "NUMBERED_HOUR_REL")))

(defun relrel-p (pred)
    (member pred `(,(vsym "_AFTER_HOUR_REL") ,(vsym "_BEFORE_HOUR_REL"))))

(defun minrel-p (pred)
  (eql pred (vsym "MINUTE_REL")))

(defun ampmrel-p (pred)
  (member pred `(,(vsym "_AM_REL"),(vsym "_PM_REL"))))

(defun pm-rel-p (pred)
  (eql pred (vsym "_PM_REL")))

(defun am-rel-p (pred)
  (eql pred (vsym "_AM_REL")))

(defun past-rel-p (pred)
  (eql pred (vsym "_AFTER_HOUR_REL")))

(defun to-rel-p (pred)
  (eql pred (vsym "_BEFORE_HOUR_REL")))

(defun daytime-p (pred)
  (member pred `(,(vsym "_MORNING_REL") ,(vsym "_AFTERNOON_REL") ,(vsym "_EVENING_REL"))))

(defun derive-am-pm-spec (pred)
  (cond
    ((eql pred (vsym "_MORNING_REL")) 'am)
    ((eql pred (vsym "_AFTERNOON_REL")) 'pm)
    ((eql pred (vsym "_EVENING_REL")) 'pm)
    (t 'pm)))

;;; Main function

(defun time-convert-mrs-struct (mrsstruct)
  (let* ((rels (psoa-liszt mrsstruct))
         (nhrels nil)
         (minrels nil)
         (ampmrels nil)
         (relrels nil)
         (others nil))
    (dolist (rel rels)
            (let ((pred (rel-pred rel)))
              (cond ((nhrel-p pred) (push rel nhrels))
                    ((relrel-p pred) (push rel relrels))
                    ((minrel-p pred) (push rel minrels))
                    ((ampmrel-p pred) (push rel ampmrels))
                    (t (push rel others)))))
    (if (or nhrels relrels) 
        (let ((new-liszt (construct-time-expressions 
                          nhrels minrels ampmrels relrels others)))
          (if new-liszt
                (setf (psoa-liszt mrsstruct) new-liszt)
            (unless *giving-demo-p*
              (format t 
             "~%Warning: error in time conversion, structure unchanged"))
	    ))
      (if (or minrels ampmrels)
            (struggle-on-error
"~%Error in time conversion: no numbered_hour or after/before_hour relations")))
    mrsstruct))


(defun construct-time-expressions (nhrels minrels ampmrels relrels others)
  (if nhrels
      (append
       (loop for nhrel in nhrels
            collect
            (construct-ctime nhrel minrels ampmrels relrels others))
       (reverse others))
    (if ampmrels
        (struggle-on-error 
                "~%am/pm specified but no hour")
      (append 
              (loop for relrel in relrels
                   collect
                   (ctime-case6 relrel minrels))
              (reverse others)))))

(defun construct-ctime (nhrel minrels ampmrels relrels others)
  (let* ((handel (rel-handel nhrel))
         (inst (get-rel-feature-value nhrel *nhr-inst-feature*))
         (am-pm (get-rel-feature-value nhrel *nhr-ampm-feature*))
         (base-hour (get-rel-feature-value nhrel *nhr-hour-feature*))
         (min (get-rel-feature-value nhrel *nhr-min-feature*))
         (direct-mins (get-coindexed-mins min minrels))
         (am-pm-value (or (get-coindexed-ampm-rels am-pm ampmrels)
                         (am-pm-heuristics inst others base-hour)))
                ; returns 'am 'pm nil 
	 (direct-rel-rels (get-coindexed-rel-rels inst relrels)))            
    (cond ((and (null direct-mins) (null direct-rel-rels)) 
           (ctime-case1 handel inst 
                        (create-ctime-hour base-hour am-pm-value)))
          (direct-mins
           (when direct-rel-rels
             (struggle-on-error 
              "~%Minutes specified directly and indirectly: ~A"
                      nhrel))
           (when (cdr direct-mins)
             (struggle-on-error 
              "~%Minutes specified more than one: ~A" nhrel))
           (ctime-case3 handel inst 
                        (create-ctime-hour base-hour am-pm-value)
                        (car direct-mins)))
          ((cdr direct-rel-rels)
                 (struggle-on-error "~%Past/to specified more than once: ~A"
                      nhrel))
          (t
           (let* ((past-or-to-rel (car direct-rel-rels))
                  (rel-minute-index 
                   (get-rel-feature-value past-or-to-rel 
                                          *rel-minute-feature*))
                  (indirect-minutes
                   (if rel-minute-index
                       (get-coindexed-mins rel-minute-index minrels))))
               (unless indirect-minutes
                       (struggle-on-error "~%No minute specification: ~A" nhrel))
               (when (cdr indirect-minutes)
                 (struggle-on-error 
                  "~%Minutes specified more than once: ~A" nhrel))
               (ctime-case4 handel inst base-hour am-pm-value
                            (rel-pred past-or-to-rel)
                            (or (car indirect-minutes) 0)))))))
;;;


(defun get-coindexed-mins (index minrels)
  (loop for minrel in minrels
      when (eql 
            (get-var-num (get-rel-feature-value minrel *min-index-feature*))
            (get-var-num index))
      collect (get-rel-feature-value minrel *min-min-feature*)))

(defun get-coindexed-ampm-rels (index ampmrels)
  (let ((ampmspecs
         (loop for ampmrel in ampmrels
              when (eql (get-var-num (get-rel-feature-value ampmrel *ampm-index-feature*))
                         (get-var-num index))
              collect (rel-pred ampmrel))))         
    (cond ((null ampmspecs) nil)
          ((cdr ampmspecs) 
           (struggle-on-error "~%Dual specification of AM and PM"))
          ((am-rel-p (car ampmspecs))
           'am)
          ((pm-rel-p (car ampmspecs))
           'pm)
          (t (struggle-on-error 
              "~%Incorrect AM/PM rel pred: ~A" (car ampmspecs))))))

(defun get-coindexed-rel-rels (index relrels)
  (loop for relrel in relrels
       when
       (eql (get-var-num (get-rel-feature-value relrel *rel-hour-feature*))
            (get-var-num index))
       collect relrel))


(defun create-ctime-hour (base-hour am-pm)
  (cond ((eql am-pm 'am) 
	 (if (eql base-hour 12) 
	     (+ 12 base-hour)
	   base-hour))
        ((eql am-pm 'pm) 
	 (if (eql base-hour 12) 
	     base-hour
	   (+ 12 base-hour)))
        ((null am-pm) 
	 ;(format nil "~A/~A" base-hour (+ 12 base-hour))
	 (if (< base-hour 8)
	     (format nil "~A" base-hour (+ 12 base-hour))
	     (format nil "~A" base-hour)))
        (t (struggle-on-error "~%Unexpected value for am/pm: ~A" am-pm)))) 



;;; cases as in comments at beginning of file

(defun ctime-case1 (handel inst hour)
  (create-ctime-rel handel inst hour 0))

(defun ctime-case3 (handel inst hour minutes)
  (create-ctime-rel handel inst hour minutes))

(defun ctime-case4 (handel inst base-hour am-pm-value
                           past-or-to-rel minutes)
    (cond 
     ((past-rel-p past-or-to-rel)
        (create-ctime-rel handel inst 
                          (create-ctime-hour base-hour am-pm-value) 
                          minutes))
     ((to-rel-p past-or-to-rel)
        (create-ctime-rel handel inst
                          (create-ctime-hour (- base-hour 1) 
                                             am-pm-value)
                          (- 60 minutes)))
     (t (struggle-on-error "~%Problem with after/to specification"))))

 
                           
(defun ctime-case6 (past-or-to-rel minrels)
  (let* ((handel (rel-handel past-or-to-rel))
         (inst (get-rel-feature-value past-or-to-rel *nhr-inst-feature*))
         (past-or-to (rel-pred past-or-to-rel))
         (rel-minute-index 
                   (get-rel-feature-value past-or-to-rel 
                                          *rel-minute-feature*))
         (direct-mins (get-coindexed-mins rel-minute-index minrels)))           
    (unless direct-mins
      (struggle-on-error "~%No minute specification: ~A" 
                         past-or-to-rel)
      (setf direct-mins '(0)))
    (when (cdr direct-mins)
      (struggle-on-error 
       "~%Minutes specified more than once: ~A" past-or-to-rel))
    (cond 
     ((past-rel-p past-or-to)
        (create-ctime-rel handel inst
                          nil
                          (car direct-mins)))
     ((to-rel-p past-or-to)
        (create-ctime-rel handel inst 
                          nil
                          (- 60 (car direct-mins))))
     (t (struggle-on-error "~%Problem with after/to specification")))))
                
;;;
;;; Heuristics for morning / afternoon etc
;;; only called if no explicit am/pm specification
            
;;; AM/PM heuristics -
;;; index = y and
;;; preposition(x y) and am/pmtime(x) 
;;; or
;;; index = x
;;; preposition(x y) and am/pmtime(y) and nhrel(x)
;;; where am/pmtime is morning_rel, etc
     
           
(defun am-pm-heuristics (index others base-hour)
  ;;; prepositions are distinguished by their features
  ;;; specified as globals
  (let ((prep-arg-vals nil)
        (prep-prep-vals nil)
        (daytimes nil))
    (dolist (rel others)
      (cond ((daytime-p (rel-pred rel))
             (push rel daytimes))
            ((eql (get-var-num (get-rel-feature-value rel *prep-arg-feature*))
                  (get-var-num index))
             (push (get-var-num (get-rel-feature-value rel *prep-prep-feature*))
                   prep-prep-vals))
            ((eql (get-var-num (get-rel-feature-value rel *prep-prep-feature*))
                  (get-var-num index))
             (push (get-var-num (get-rel-feature-value rel *prep-arg-feature*))
                   prep-arg-vals))
            (t nil)))
      (let ((ampmspecs nil))
        (dolist (daytime daytimes)
          (let ((daytime-index 
                 (get-var-num (get-rel-feature-value daytime *inst-feature*))))
            (if (or (member daytime-index prep-prep-vals)
                    (member daytime-index prep-arg-vals))
              (push (derive-am-pm-spec (rel-pred daytime))
                    ampmspecs))))
        (cond ((null ampmspecs) (horrible-unmotivated-hack base-hour))
              ((every #'(lambda (spec) (eql spec 'am)) ampmspecs)
               'am)
              ((every #'(lambda (spec) (eql spec 'pm)) ampmspecs)
               'pm)
              (t (horrible-unmotivated-hack base-hour))))))

(defun horrible-unmotivated-hack (base-hour)
        (if (integerp base-hour)
          (if (or (< base-hour 8)
                  (> base-hour 11))
              'pm 
              'am)))
#|
(defun horrible-unmotivated-hack (base-hour)
        (if (and (integerp base-hour)
		 (< base-hour 12))
            'am))
|#



;;;
;;; simple-minded number conversion; ad-hoc for VerbMobil  (26-oct-99  -  oe)
;;;

(defparameter *nc-times_rel* (vsym "TIMES_REL"))
(defparameter *nc-plus_rel* (vsym "PLUS_REL"))
(defparameter *nc-const_rel* (vsym "CONST_REL"))
(defparameter *nc-integer_rel* (vsym "INTEGER_REL"))
(defparameter *nc-term1* (vsym "TERM1"))
(defparameter *nc-term2* (vsym "TERM2"))
(defparameter *nc-factor1* (vsym "FACTOR1"))
(defparameter *nc-factor2* (vsym "FACTOR2"))
(defparameter *nc-const_value* (vsym "CONST_VALUE"))
(defparameter *nc-arg* (vsym "ARG"))

(defun times_rel-p (pred)
  (eq pred *nc-times_rel*))

(defun plus_rel-p (pred)
  (eq pred *nc-plus_rel*))

(defun const_rel-p (pred)
  (or (eq pred *nc-const_rel*) 
      (eq pred *nc-integer_rel*)))

(defun number-convert (mrs)
  (let ((liszt (psoa-liszt mrs))
        constants operators additions deletions)
    (loop
        for relation in liszt
        for pred = (rel-pred relation)
        when (plus_rel-p pred) do (push relation operators)
        when (times_rel-p pred) do (push relation operators)
        when (const_rel-p pred)	do (push relation constants))
    (loop
        for stable = t
        for i from 0
        do
          (loop
              for operator in operators
              for handel = (rel-handel operator)
              for arg = (get-rel-feature-value operator *nc-arg*)
              unless (member operator deletions :test #'eq) do
                (let* ((pred (rel-pred operator))
                       (key1 (cond 
                               ((plus_rel-p pred) *nc-term1*)
                               ((times_rel-p pred) *nc-factor1*)))
                       (key2 (cond
                               ((plus_rel-p pred) *nc-term2*)
                               ((times_rel-p pred) *nc-factor2*)))
                       (term1 (get-rel-feature-value operator key1))
                       (const1 (find-const-by-handle 
                                term1 additions constants))
                       (value1 (get-rel-feature-value const1 *nc-const_value*))
                       (term2 (get-rel-feature-value operator key2))
                       (const2 (find-const-by-handle 
                                term2 additions constants))
                       (value2 (get-rel-feature-value const2 *nc-const_value*))
                       (value (and value1 value2
                                   (compute-value operator value1 value2))))
                  (when value
                    (push (make-constant handel arg value)
			  additions)
                    (push operator deletions)
                    (push const1 deletions)
                    (push const2 deletions)
                    (setf stable nil))))
        until (or stable (>= i 42)))
    (if (or additions deletions)
      (let ((copy (copy-psoa mrs))
            (additions (loop
                           for relation in additions
                           unless (member relation deletions :test #'eq)
                           collect relation))
            (relations (loop
                           for relation in liszt
                           unless (member relation deletions :test #'eq)
                           collect relation)))
        (setf (psoa-liszt copy) (nconc additions relations))
        copy)
      mrs)))

(defun find-const-by-handle (handel new old)
  (or
   (loop
       for relation in new
       thereis (when (and (const_rel-p (rel-pred relation))
                          (eq (rel-handel relation) handel))
                 relation))
   (loop
       for relation in old
       thereis (when (and (const_rel-p (rel-pred relation))
                          (eq (rel-handel relation) handel))
                 relation))))

(defun compute-value (operator term1 term2)
  (let ((pred (rel-pred operator)))
    (cond
     ((plus_rel-p pred) (+ term1 term2))
     ((times_rel-p pred) (* term1 term2)))))

(defun make-constant (handel arg value)
  (make-rel
   :pred *nc-const_rel*
   :handel handel
   :flist (list  
           (make-fvpair :feature *nc-arg* :value arg)
           (make-fvpair :feature *nc-const_value* :value value))))





