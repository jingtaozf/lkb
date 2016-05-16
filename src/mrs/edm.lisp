;;; Copyright (c) 2009 -- 2011
;;;   Stephan Oepen, Rebecca Dridan;
;;;   see `LICENSE' for conditions.

(in-package :mrs)

(defun edm (mrs gmrs)
  (labels ((intersect (set1 set2 &key (test #'eql))
             ;;
             ;; much like intersection(), except well-defined on multi-sets.
             ;;
             (loop
                 with set2 = (append set2 (list (gensym "")))
                 for foo in set1
                 for bar = (member foo set2 :test test)
                 when bar 
                 do
                   (setf (first bar) (first (rest bar)))
                   (setf (rest bar) (rest (rest bar)))
                 and collect foo)))
    (let* ((test (and (psoa-p mrs) (edm-explode-mrs mrs)))
           (gold (edm-explode-mrs gmrs))
           (correct (intersect test gold :test #'equal))
           (tn 0) (ta 0) (tp 0)
           (gn 0) (ga 0) (gp 0)
           (cn 0) (ca 0) (cp 0))
      (loop
          for triple in test
          for type = (first triple)
          when (eq type :predicate) do (incf tn)
          when (or (eq type :root) (eq type :argument)) do (incf ta)
          when (eq type :property) do (incf tp))
      (loop
          for triple in gold
          for type = (first triple)
          when (eq type :predicate) do (incf gn)
          when (or (eq type :root) (eq type :argument)) do (incf ga)
          when (eq type :property) do (incf gp))
      (loop
          for triple in correct
          for type = (first triple)
          when (eq type :predicate) do (incf cn)
          when (or (eq type :root) (eq type :argument)) do (incf ca)
          when (eq type :property) do (incf cp))
      (pairlis '(:tn :ta :tp :gn :ga :gp :cn :ca :cp)
               (list tn ta tp gn ga gp cn ca cp)))))

(defun edm-explode-mrs (mrs
                        &key (cargp t) (eds (eds-convert-psoa mrs)))
  
  ;;
  ;; make sure the distinguished variable slot is available when it should be
  ;; _fix_me_
  ;; i wonder, though: why should this step be necessary?       (26-jan-12; oe)
  ;;
  (loop
      with key = (vsym "ARG0")
      for ed in (eds-relations eds)
      for raw = (ed-raw ed)
      for roles = (and (rel-p raw) (rel-flist raw))
      for arg0 = (loop
                     for role in roles
                     when (eq (fvpair-feature role) key)
                     return (fvpair-value role))
      do (setf (ed-variable ed) arg0))

  (let (result)
    (let* ((top (eds-top eds))
           (root (find top (eds-relations eds) :test #'equal :key #'ed-id)))
      (when root (push (list :root (ed-lnk root)) result)))
    (loop
        for ed in (eds-relations eds)
        for lnk = (ed-lnk ed)
        unless (or (not (valid-lnk-p lnk))
                   (and (null (ed-status ed)) 
                        (or (ed-bleached-p ed) (ed-vacuous-p ed))))
        do
          ;;
          ;; _fix_me_
          ;; this is getting somewhat baroque: we want a way of including
          ;; quantifiers in this list, jointly with the EP introducing the
          ;; variable bound by the quantifier.                 (13-aug-04; oe)
          ;;
          (when (and (ed-quantifier-p ed) (null *eds-quantifier-argument*))
            (let* ((target (loop
                               with id = (ed-id ed)
                               for ed in (eds-relations eds)
                               when (and (equal (ed-id ed) id)
                                         (not (ed-quantifier-p ed)))
                               return ed)))
              (when target
                (push
                 (list :argument lnk (vsym "BV") (ed-lnk target)) result))))
          (let ((predicate (format
                            nil "~a~@[(~s)~]"
                            (ed-predicate ed) (and cargp (ed-carg ed)))))
            (push (list :predicate lnk predicate) result))
          (loop
              for (role . value) in (ed-arguments ed)
              when (ed-p value)
              do (push (list :argument lnk role (ed-lnk value)) result))
          (when (ed-variable ed)
            (loop
                for extra in (var-extra (ed-variable ed))
                for feature = (vsym (extrapair-feature extra))
                for value = (vsym (extrapair-value extra))
                when value
                do (push (list :property lnk feature value) result))))
    (nreverse result)))
