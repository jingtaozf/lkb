(in-package :mt)

;;;
;;; Copyright (c) 2004 -- 2006 Stephan Oepen (oe@csli.stanford.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.
;;; 

(defun fragment-ep-p (ep)
  (member (mrs:rel-pred ep) *semi-fragment-relations* :test #'equal))

(defun punctuation-ep-p (ep)
  (member (mrs:rel-pred ep) *semi-punctuation-relations* :test #'equal))

(defun token-ep-p (ep)
  (member (mrs:rel-pred ep) *semi-token-relations* :test #'equal))

(defmacro add (n i)
  `(when (numberp ,i) (setf ,n (+ ,n ,i))))

;;;
;;; (LOGON) fragment detection; see comment in `mrsglobals.lisp'.
;;;
(defun fragmentp (mrs)
  (when (mrs::psoa-p mrs)
    (loop
        for ep in (mrs:psoa-liszt mrs)
        when (member (mrs:rel-pred ep) *semi-fragment-relations* :test #'equal)
        count 1 into n
        finally (return (unless (zerop n) n)))))

(defun generate-from-fragmented-mrs (mrs &key signal)
  (declare (ignore signal))

  (labels ((cross-product (sets &key (separator "||"))
             (if (null (rest sets))
               (loop
                   for foo in (first sets) collect foo)
               (loop
                   with rests = (cross-product (rest sets))
                   for foo in (first sets)
                   nconc (loop
                             for bar in rests
                             collect (format
                                      nil
                                      "~a ~a ~a"
                                      foo separator bar))))))
    (let* ((fragments (discriminate-fragments mrs))
           (fragments (loop
                          for fragment in fragments
                          unless (loop
                                     for ep in (mrs:psoa-liszt fragment)
                                     always (punctuation-ep-p ep))
                          collect fragment))
           (tftasks 0) (tetasks 0) (tstasks 0)
           (tunifications 0) (tcopies 0)
           (taedges 0) (tpedges 0)
           (tsubsumptions 0) 
           (tequivalent 0) (tproactive 0) (tretroactive 0)
           outputs statistics)
      (loop
          with lkb::*start-symbol* = lkb::*fragment-start-symbols*
          for fragment in fragments
          for surface = (when (and (mrs:psoa-liszt fragment)
                                   (null (rest (mrs:psoa-liszt fragment)))
                                   (token-ep-p 
                                    (first (mrs:psoa-liszt fragment))))
                          (loop
                              with carg = (mrs::vsym "CARG")
                              with ep = (first (mrs:psoa-liszt fragment))
                              for role in (mrs:rel-flist ep)
                              when (eq (mrs:fvpair-feature role) carg)
                              return (format
                                      nil
                                      "/~a/"
                                      (mrs:fvpair-value role))))
          do
            (multiple-value-bind (strings ftasks etasks stasks
                                  unifications copies aedges pedges)
                (if surface
                  (values (list (list surface)) 0 0 0 0 0 0 0)
                  (lkb::generate-from-mrs fragment :signal nil))
              (push (or strings (list "...")) outputs)
              (add tftasks ftasks) (add tetasks etasks) (add tstasks stasks)
              (add tunifications unifications) (add tcopies copies)
              (add taedges aedges) (add tpedges pedges)
              (add
               tsubsumptions (lkb::statistics-subsumptions lkb::*statistics*))
              (add tequivalent (lkb::statistics-equivalent lkb::*statistics*))
              (add tproactive (lkb::statistics-proactive lkb::*statistics*))
              (add
               tretroactive (lkb::statistics-retroactive lkb::*statistics*))
              ;;
              ;; _fix_me_
              ;; do accumulation of %generator-statistics%, maybe think of a
              ;; way of packaging up fragment combinations as a single edge.
              ;;                                               (18-jul-04; oe)
              (push lkb::%generator-statistics% statistics)))
      (setf lkb::*gen-record* nil)
      (setf lkb::%generator-statistics% nil)
      (setf (lkb::statistics-subsumptions lkb::*statistics*) tsubsumptions)
      (setf (lkb::statistics-equivalent lkb::*statistics*) tequivalent)
      (setf (lkb::statistics-proactive lkb::*statistics*) tproactive)
      (setf (lkb::statistics-retroactive lkb::*statistics*) tretroactive)
      (let* ((strings (cross-product (nreverse outputs)))
             #+:ppcre
             (strings (remove-if
                       #'(lambda (string) (ppcre:scan "^[.|/ ]*$" string))
                       strings)))
        (setf lkb::*gen-record*
          (loop
              for string in strings
              collect (lkb::make-edge :string string)))
        (values
         strings
         tftasks tetasks tstasks tunifications tcopies taedges tpedges)))))

(defun discriminate-fragments (mrs)
  (loop
      with mrss
      for fragments = (find-eps mrs (mrs:psoa-top-h mrs))
      then (find-eps 
            mrs (find-role-value top *semi-fragment-right*))
      for top = (loop for ep in fragments when (fragment-ep-p ep) return ep)
      while fragments
      when (and nil (rest fragments)) do
        (error
         "discriminate-fragments(): multiple matches (~{|~a|~^, ~}).~%"
         (loop
             for ep in fragments
             collect (mrs::ep-shorthand ep)))
        (return-from discriminate-fragments)
      when (and top (fragment-ep-p top)) do
        (let ((mrs (extract-connected-mrs 
                    mrs 
                    (find-role-value top *semi-fragment-left*))))
          (if mrs
            (push mrs mrss)
            (error
             "discriminate-fragments(): null left daughter (|~a|).~%"
             top)))
      else do
        (push 
         (mrs::make-psoa
          :top-h (mrs::make-var :id (incf %transfer-variable-id%) :type "h")
          :index (mrs::make-var :id (incf %transfer-variable-id%) :type "u")
          :liszt fragments)
         mrss)
         (setf fragments nil)
      finally (return (nreverse mrss))))

;;;
;;; _fix_me_
;;; it appears that in transfer outputs, there can be equivalent but non-eq()
;;; instances of variables, possibly distributed between RELS and HCONS.  in
;;; many contexts, we would not notice, specifically when reading back an MRS
;;; from the string representation (e.g. across processes).     (17-jul-04; oe)
;;;
(defun variable-equal (variable1 variable2)
  (when (and (mrs::var-p variable1) (mrs::var-p variable2))
    (or (eq variable1 variable2)
        (and (equal (mrs:var-type variable1) (mrs:var-type variable2))
             (eql (mrs:var-id variable1) (mrs:var-id variable2))))))

(defun find-role-value (ep role)
  (when (and (mrs::rel-p ep) (symbolp role))
    (loop
        for pair in (mrs:rel-flist ep)
        when (eq (mrs:fvpair-feature pair) role)
        return (mrs:fvpair-value pair))))

(defun find-eps (mrs variable &key role)
  (when (and (mrs::psoa-p mrs) (mrs::var-p variable))
    (loop
        for ep in (mrs:psoa-liszt mrs)
        for key = (if role
                    (find-role-value ep role)
                    (mrs:rel-handel ep))
        when (variable-equal key variable)
        collect ep)))

(defun extract-connected-mrs (mrs variable)
  (when (and (mrs::psoa-p mrs) (mrs::var-p variable))
    (let* ((agenda (list variable))
           variables eps hconss)
      (loop
          for variable = (pop agenda) 
          while variable do
            (push variable variables)
            (loop
                for ep in (mrs:psoa-liszt mrs)
                for label = (mrs:rel-handel ep)
                unless (or (fragment-ep-p ep) (member ep eps :test #'eq))
                do
                  (let ((match (or (variable-equal label variable)
                                   (loop
                                       for pair in (mrs:rel-flist ep)
                                       for value = (mrs:fvpair-value pair)
                                       thereis (variable-equal
                                                value variable)))))
                    (when match
                      (push ep eps)
                      (unless (or (member label variables)
                                  (member label agenda))
                        (push label agenda))
                      (loop
                          for pair in (mrs:rel-flist ep)
                          for value = (mrs:fvpair-value pair)
                          when (and (mrs::var-p value)
                                    (not (member value variables))
                                    (not (member value agenda)))
                          do (push value agenda)))))
            (loop
                for hcons in (mrs:psoa-h-cons mrs)
                for harg = (mrs:hcons-outscpd hcons)
                for larg = (mrs:hcons-scarg hcons)
                when (and (variable-equal harg variable)
                          (not (member larg variables))
                          (not (member larg agenda)))
                do (push larg agenda)
                when (and (variable-equal larg variable)
                          (not (member harg variables))
                          (not (member harg agenda)))
                do (push harg agenda)))
      
      (loop
          for hcons in (mrs:psoa-h-cons mrs)
          when (or (member (mrs:hcons-outscpd hcons) variables)
                   (member (mrs:hcons-scarg hcons) variables))
          do (pushnew hcons hconss :test #'eq))
      
      (mrs::make-psoa
       :top-h (mrs::make-var :id (incf %transfer-variable-id%) :type "h")
       :index (mrs::make-var :id (incf %transfer-variable-id%) :type "u")
       :liszt eps :h-cons hconss))))