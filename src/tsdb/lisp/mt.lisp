(in-package :tsdb)

(defun summarize (data &key file meter)
  (when meter (meter :value (get-field :start meter)))
  (loop
      with semi = (mt::make-semi)
      for item in (analyze data :thorough '(:mrs))
      for results = (get-field :results item)
      do
        (loop
            for result in results
            for mrs = (get-field :mrs result)
            when mrs do (mt::record-mrs mrs semi))
      finally 
        (let ((stream (create-output-stream file nil)))
          (mt::print-semi semi :stream stream)
          (when (stringp file) (close stream))))
  (when meter (meter :value (get-field :end meter))))
