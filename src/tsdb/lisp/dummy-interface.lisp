;;; Hey, emacs(1), this file is -*- Mode: Common-Lisp; Package: dummy; -*-

(in-package :tsdb)

(defpackage :dummy
  (:use :common-lisp :make))

(in-package :dummy)

(defparameter %dummy-mrs% "
[ LTOP: h1
  INDEX: e2 [ EVENT
               DIVISIBLE:  BOOL
               E.TENSE:  PRESENT*
               E.ASPECT:  NO_ASPECT*
               E.MOOD:  INDICATIVE* ]
  RELS: <
          [ prpstn_m_rel
            LBL: h1
            MARG: h3 ]
          [ _a_q_rel
            LBL: h4
            ARG0: x5 [ REF-IND
                        PNG.GEN:  REAL_GENDER
                        PNG.PN:  3SG
                        DIVISIBLE:  - ]
            RSTR: h6
            BODY: h7 ]
          [ _dog_n_rel
            LBL: h8
            ARG0: x5 ]
          [ _sleep_rel
            LBL: h9
            ARG0: e2
            ARG1: x5 ] >
  HCONS: <  h3 QEQ h9
            h6 QEQ h8 > ]")
    
(defun current-grammar ()
  ;;
  ;; return a string identifying the grammar that is currently in use, ideally
  ;; including relevant grammar-internal parameters of variation and a version.
  ;;
  "unknown")

(defun initialize-run (&key interactive 
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
  )
  
(defun finalize-run (context &key custom)
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

(defun parse-item (string 
                   &key id exhaustive nanalyses trace
                        edges derivations semantix-hook trees-hook
                        burst (nresults 0))
  (declare (ignore string id exhaustive nanalyses trace
                   edges derivations semantix-hook trees-hook burst))
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
  (multiple-value-bind (return condition)
      (ignore-errors
       (let (tgc tcpu treal conses symbols others)
         (tsdb::time-a-funcall
          #'(lambda () #+:null (parse string trace))
          #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
              (declare (ignore ignore))
              (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr
                    conses (* scons 8) symbols (* ssym 24) others sother)))
         (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (readings 1))
           (pairlis '(:others :symbols :conses :treal :tcpu :tgc
                      :readings :results)
                    (list others symbols conses treal tcpu tgc
                          readings
                          (loop
                              with nresults = (if (<= nresults 0)
                                                readings
                                                (min readings nresults))
                              for i from 1
                              for mrs = %dummy-mrs%
                              while (>= (decf nresults) 0) collect
                                (pairlis '(:result-id :mrs) (list i mrs))))))))
    (append
     (when condition
       (let* ((error (tsdb::normalize-string (format nil "~a" condition))))
         (pairlis '(:readings :error) (list -1 error))))
     return)))

(eval-when #+:ansi-eval-when (:load-toplevel :compile-toplevel :execute)
	   #-:ansi-eval-when (load eval compile)
  (import '(current-grammar initialize-run finalize-run parse-item)
           :tsdb))
