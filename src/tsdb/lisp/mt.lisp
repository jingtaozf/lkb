(in-package :tsdb)

(defun summarize (profiles &key file meter)
  (when meter (meter :value (get-field :start meter)))
  (loop
      with semi = (mt::make-semi)
      for profile in (if (consp profiles) profiles (list profiles))
      for message = (format nil "summarizing `~a' ..." profile)
      do
        (when meter (status :text message))
        (loop
            for item in (analyze profile :thorough '(:mrs))
            for results = (get-field :results item)
            do
              (loop
                  for result in results
                  for mrs = (get-field :mrs result)
                  when mrs do (mt::record-mrs mrs semi)))
      finally 
        (when meter (status :text (format nil "~a done" message) :duration 5))
        (let ((stream (create-output-stream file nil)))
          (mt::print-semi semi :stream stream)
          (when (stringp file) (close stream))))
  (when meter (meter :value (get-field :end meter))))
