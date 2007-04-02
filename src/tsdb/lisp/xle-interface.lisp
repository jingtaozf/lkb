(in-package :xle)

;;;
;;; [incr tsdb()] --- Competence and Performance Profiling Environment
;;; Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
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

(defparameter tsdb::*tsdb-maximal-number-of-fragments* 5)

(defun tsdb::current-grammar ()
  ;;
  ;; return a string identifying the grammar that is currently in use, ideally
  ;; including relevant grammar-internal parameters of variation and a version.
  ;;
  (or (tsdb::clients-grammar) "norgram (mo)"))

(defun tsdb::initialize-run (&key interactive 
                            exhaustive nanalyses
                            protocol custom)
  (declare (ignore interactive exhaustive nanalyses protocol custom))
  ;;
  ;; prepare processor for a (test) run; if necessary, load grammar, set global
  ;; parameters, et al.  parameters are:
  ;;
  ;;   .interactive. (bool) - usually false, true only when interactive 
  ;;                          processing is required (e.g. to enable additional
  ;;                          tracing);
  ;;   .exhaustive. (bool) - all-paths vs. best-first processing;
  ;;   .nanalyses. (int) - upper limit for solutions to be returned; relevant
  ;;                       in best-first mode only;
  ;;   .protocol. (int) - [incr tsdb()] protocol version (1: record complete
  ;;                      derivations; 2: record packed derivations);
  ;;   .custom. (string) - custom data supplied in the client definition (as a
  ;;                       :create field); may point to grammar or other
  ;;                       client-specific data.
  ;;
  ;; the return value is a [incr tsdb()] run structure (association list); see
  ;; the User Manual for details.  if initialize-run() needs to preserve some
  ;; aspect of the orginal environment that it wants finalize-run() to restore,
  ;; a field :context can be included in the run structure and will be passed
  ;; as the first parameter to finalize-run() upon completion.
  ;;
  #+:null
  (parse "Sov!" nil)
  nil)
  
(defun tsdb::finalize-run (context &key custom)
  (declare (ignore context custom))
  ;;
  ;; restore processor to original state after completion of a (test) run.  the
  ;; parameters are:
  ;;
  ;;   .context. (any) - :context field from initialize-run() return value;
  ;;   .custom. (string) - custom data supplied in the client definition (as a
  ;;                       :complete field); may point to grammar or other
  ;;                       client-specific data.
  ;;
  ;; the return value can be nil or a (partial) [incr tsdb()] run structure, in
  ;; which case it will be prepended to the initialize-run() return value.
  ;;
  )

(defun tsdb::parse-item (string 
                   &key id exhaustive nanalyses trace
                        edges derivations semantix-hook trees-hook
                        filter burst (nresults 0))
  (declare (ignore id exhaustive nanalyses edges derivations 
                   semantix-hook trees-hook burst)
           (special tsdb::*process-suppress-duplicates*))
  ;;
  ;; send string through processor (i.e. parser) and return processing results.
  ;; the parameters are:
  ;;
  ;;   .string. (string) - input sequence to parser;
  ;;   .id. (int) - item identifier (if part of a processing a test suite);
  ;;   .exhaustive. (bool) - all-paths vs. best-first processing;
  ;;   .nanalyses. (int) - upper limit for solutions to be returned; relevant
  ;;                       in best-first mode only;
  ;;   .trace. (bool) - tracing flag passed to parser verbatim;
  ;;   .edges. (int) - upper limit on chart size (in edges), 0 for unlimited;
  ;;   .nresults. (int) - upper limit on results to be recorded; 0 for all.
  ;;
  ;; the return value is a [incr tsdb()] item structure (association list); see
  ;; the User Manual for details.  pretty much all elements are optional, with
  ;; the exception of the :readings (number of analyses) field, probably.  the
  ;; item structure can contain a :results field, which should be a list of 
  ;; [incr tsdb()] result structures, including (among others) a :mrs field.
  ;; note that all values in [incr tsdb()] item and result structures have to
  ;; be of type integer or string.
  ;;
  (let ((start (get-internal-run-time)) stop
        (filterp (member :mrs filter))
        tgc tcpu utgc utcpu treal graph solutions)
    (nconc
     (handler-case
         (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (nfragments 0)
                readings mrss unknown invalid)
           (tsdb::time-a-funcall
            #'(lambda () (setf graph (parse string trace :ranking t)))
            #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
                (declare (ignore scons ssym sother ignore))
                (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr)))
           (setf readings 
             (tsdb::time-a-funcall
              #'(lambda () 
                  (loop
                      ;;
                      ;; _fix_me_
                      ;; find a better way of not having to keep all the memory
                      ;; around: probably allow de-allocation of the solutions
                      ;; here and then rebuild them in the loop below
                      ;; extracting results.                   (29-oct-03; oe)
                      ;;
                      for solution = (solution 
                                      (get-next-solution graph))
                      while (not (zerop solution)) 
                      do (push solution solutions)
                      count 1))
              #'(lambda (tgcu tgcs tu ts tr scons ssym sother
                         &rest ignore)
                  (declare (ignore scons ssym sother ignore))
                  (setf utgc (+ tgcu tgcs) utcpu (+ tu ts))
                  (incf treal tr))))
           (pairlis '(:treal :tcpu :tgc :readings
                      :results :nresults :fragments :comment :error)
                    (list treal (+ tcpu utcpu) (+ tgc utgc)
                          readings
                          (loop
                              with fragmentp = (fragment-analysis-p graph)
                              with n 
                              = (if fragmentp
                                  tsdb::*tsdb-maximal-number-of-fragments*
                                  (if (<= nresults 0)
                                    readings
                                    (min readings nresults)))
                              with nunwanted = n
                              for i from 0 
                              to (if fragmentp 1000 n)
                              for solution in (nreverse solutions)
                              for derivation =
                                (extract-c-structure graph solution)
                              for score = (extract-score graph solution)
                              for warnings = nil
                              for mrs =
                                (let* ((mrs (extract-mrs graph solution))
                                       ;;
                                       ;; _fix_me_
                                       ;; i hesitate activating this just now,
                                       ;; as we are finalizing `fjell'.
                                       ;;                       (7-dec-06; oe)
                                       #+:null
                                       (fishy (mt:test-integrity mrs))
                                       #-:null
                                       (fishy nil)
                                       ;;
                                       ;; _fix_me_
                                       ;; it seems clear we cannot tolerate
                                       ;; transfer ambiguity here, but still we
                                       ;; should probably check and throw an
                                       ;; error, should the impossible happen.
                                       ;;                       (28-sep-06; oe)
                                       ;;
                                       (edges (unless fishy
                                                (mt:transfer-mrs
                                                 mrs :task :semi :block nil)))
                                       (edge
                                        (loop
                                            for edge in edges
                                            for mtr = (mt::edge-rule edge)
                                            for block 
                                            = (and mtr (mt::mtr-block mtr))
                                            when (or (null mtr) (null block))
                                            return edge))
                                       (mrs (and edge (mt::edge-mrs edge))))
                                  (when mrs
                                    (let ((edges
                                           (mt:transfer-mrs
                                            mrs :task :filter :block nil)))
                                      (when edges
                                        (setf warnings
                                          (mt::edge-warn (first edges))))))
                                  (setf invalid (union invalid fishy))
                                  (when (and (mrs::psoa-p mrs)
                                             (null invalid))
                                    (unless (and filterp
                                                 (member 
                                                  mrs mrss 
                                                  :test 
                                                  #'tsdb::safe-mrs-equal-p))
                                      (when (mt:fragmentp mrs)
                                        (incf nfragments))
                                      (let ((errors
                                             (mt:test-semi-compliance mrs)))
                                        (cond
                                         ((null errors)
                                          (push mrs mrss)
                                          (with-output-to-string (stream)
                                            (mrs::output-mrs1 
                                             mrs 'mrs::simple stream)))
                                         (t
                                          (loop
                                              for error in errors
                                              do (pushnew
                                                  (mrs::rel-pred error)
                                                  unknown
                                                  :test #'equal))))))))
                              for flags
                              = (acons
                                 :ascore score
                                 (when warnings
                                   (acons :warnings (length warnings) nil)))
                              for result
                              = (pairlis '(:result-id :derivation :mrs :flags) 
                                         (list i derivation mrs flags))
                              when (and mrs (null warnings))
                              do (decf n) and collect result into results
                              else when mrs 
                              do (decf nunwanted) 
                              and collect result into unwanted
                              while (and solution (> n 0) (> nunwanted 0))
                              finally
                                #+:debug
                                (setf %mrss mrss)
                                (unless (zerop (solution graph))
                                  (free-graph-solution 
                                   (graph-address graph)))
                                (return
                                  (loop
                                      for i from 0
                                      for result in (nconc results unwanted)
                                      collect (acons :result-id i result))))
                          (length mrss) nfragments
                          (format
                           nil
                           "(:nresults . ~d) (:nfragments . ~d)"
                           (length mrss) nfragments)
                          (let* ((mrs::*mrs-raw-output-p* nil)
                                 (invalid
                                  (when invalid
                                    (format
                                     nil
                                     "invalid MRS elements: ~{|~(~s~)|~^, ~}"
                                     invalid)))
                                 (unknown
                                  (when unknown
                                    (format
                                     nil
                                     "invalid SEM-I predicates: ~
                                      ~{|~(~s~)|~^, ~}"
                                     unknown))))
                            (format nil "~@[~a; ~]~@[~a~]" invalid unknown)))))

       (storage-condition (condition)
         (declare (ignore condition))
         #+:allegro
         (excl:exit 0 :no-unwind t)
         #+:lispworks
         (lw:quit :ignore-errors-p t)
         #-(or :allegro :lispworks)
         (error 
          "no known mechanism to shutdown Lisp (see `xle-interface.lisp'"))

       (condition (condition)
         #+:debug (error condition)
         (let* ((error (tsdb::normalize-string 
                        (format nil "~a" condition))))
           (pairlis '(:readings :error)
                    (list -1  error)))))
     
     (let* ((stop (or (when (numberp stop) stop) (get-internal-run-time)))
            (total
             (round (* (- stop start) 1000) internal-time-units-per-second)))
       (acons :total total nil)))))
