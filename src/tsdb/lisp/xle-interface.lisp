(in-package :xle)

(defun tsdb::current-grammar ()
  ;;
  ;; return a string identifying the grammar that is currently in use, ideally
  ;; including relevant grammar-internal parameters of variation and a version.
  ;;
  (or (tsdb::clients-grammar) "norgram (mar-04)"))

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
  (parse "sov!" nil)
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
                        burst (nresults 0))
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
  (multiple-value-bind (return condition)
      (#-:debug ignore-errors #+:debug progn
       (let ((filterp (member :mrs tsdb::*process-suppress-duplicates*))
             tgc tcpu utcpu treal graph solutions)
         (tsdb::time-a-funcall
          #'(lambda () (setf graph (parse string trace)))
          #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
              (declare (ignore scons ssym sother ignore))
              (setf tgc (+ tgcu tgcs) tcpu (+ tu ts) treal tr)))
         (let* ((*print-pretty* nil) (*print-level* nil) (*print-length* nil)
                (readings 
		 (tsdb::time-a-funcall
		  #'(lambda () 
		      (loop
                          ;;
                          ;; _fix_me_
                          ;; find a better way of not having to keep all the
                          ;; memory around: probably allow de-allocation of the
                          ;; solutions here and then rebuild them in the loop
                          ;; below extracting results.         (29-oct-03; oe)
                          ;;
			  for solution = (solution 
                                          (get-next-solution graph nil))
			  while (not (zerop solution)) 
			  do (push solution solutions)
			  count 1))
		  #'(lambda (tgcu tgcs tu ts tr scons ssym sother &rest ignore)
		      (declare (ignore scons ssym sother ignore))
		      (setf utcpu (- (+ tu ts) (+ tgcu tgcs)))
		      (incf treal tr)))))
           (pairlis '(:treal :total :tcpu :tgc :readings :results)
                    (list treal (+ tcpu utcpu) tcpu tgc
                          readings
                          (loop
                              with mrss
                              with nresults = (if (<= nresults 0)
                                                readings
                                                (min readings nresults))
                              for i from 0
                              for solution in (nreverse solutions)
			      for derivation =
				(extract-c-structure graph solution)
                              for mrs =
                                #+:mrs
                                (let ((mrs (extract-mrs graph solution)))
                                  (when (mrs::psoa-p mrs)
                                    (unless (and filterp
                                                 (member 
                                                  mrs mrss 
                                                  :test 
                                                  #'tsdb::safe-mrs-equal-p))
                                      (push mrs mrss)
                                      (with-output-to-string (stream)
                                        (mrs::output-mrs1 
                                         mrs 'mrs::simple stream)))))
                                #-:mrs
                                nil
                              while (and (>= (decf nresults) 0) mrs) collect
                                (pairlis '(:result-id :derivation :mrs) 
					 (list i derivation mrs))
			      finally 
				(unless (zerop (solution graph))
				  (free-graph-solution 
				   (graph-address graph)))))))))

    (append
     (when condition
       (let* ((error (tsdb::normalize-string (format nil "~a" condition))))
         (pairlis '(:readings :error) (list -1 error))))
     return)))
