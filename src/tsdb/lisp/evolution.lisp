;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: TSDB -*-

(in-package :tsdb)

(defun summarize-evolution (profiles 
                            &key attributes division condition meter format)

  (loop
      with increment = (/ 1 (length profiles))
      with coveragep = (member :coverage attributes :test #'eq)
      with overgenerationp = (member :overgeneration attributes :test #'eq)
      with division = (unless (or (null division) (equal division ""))
                        (if (or (null condition) (equal condition ""))
                          division
                          (format nil "(~a) and (~a)" condition division)))
      initially (when meter (meter :value (get-field :start meter)))
      finally (when meter 
                (status :text (format nil "~a done" message) :duration 2)
                (meter :value (get-field :end meter)))
                
      for i from 0
      for profile in profiles
      for message = (when meter 
                      (format nil "summarizing profile `~a' ..." profile))
      for ameter = (when meter 
                     (status :text message)
                     (if division
                       (let ((increment (/ increment 2)))
                         (make-meter (* i increment) (* (+ i 1) increment)))
                       (make-meter (* i increment) (* (+ i 1) increment))))
      for dmeter = (when (and meter division)
                     (let ((increment (/ increment 2)))
                       (make-meter (* i increment) (* (+ i 1) increment))))
      for items = (analyze-aggregates profile :condition condition
                                      :meter ameter :message nil)
      for ditems = (when division
                     (analyze-aggregates profile :condition division
                                         :meter dmeter :format format))
      for wf = #'(lambda (foo) (not (= (get-field :i-wf foo) 1)))
      for if = #'(lambda (foo) (not (= (get-field :i-wf foo) 0)))
      for summary = (get-field
                     :total 
                     (summarize-competence-parameters items))
      for csummary = (when coveragep
                       (get-field 
                        :total
                        (if division
                          (summarize-competence-parameters-by-division   
                           items ditems :restrictor wf)  
                          (summarize-competence-parameters
                           items :restrictor wf))))
      for osummary = (when overgenerationp
                       (get-field 
                        :total
                        (if division
                          (summarize-competence-parameters-by-division   
                           items ditems :restrictor wf)  
                          (summarize-competence-parameters
                           items :restrictor if))))
      for words = (get-field :lambiguity summary)
      for analyses = (get-field :analyses summary)
      for crestricted = (get-field :restricted csummary)
      for cresults = (get-field :results csummary)
      for coverage = (when (and crestricted cresults)
                       (if (zerop crestricted)
                         0
                         (float (* 100 (/ cresults crestricted)))))
      for orestricted = (get-field :restricted osummary)
      for oresults = (get-field :results osummary)
      for overgeneration = (when (and orestricted oresults)
                             (if (zerop orestricted)
                               0
                               (float (* 100 (/ oresults orestricted)))))
      for performance = (get-field
                         :total
                         (summarize-performance-parameters 
                          items :format (profile-granularity profile)))
      for pedges = (get-field :pedges performance)
      for aedges = (get-field :aedges performance)
      for rpedges = (get-field :rpedges performance)
      for first = (get-field :first performance)
      for total = (get-field :total performance)
      for runs = (select '("run-id" "avms" "lexicon" "lrules" "rules"
                           "host" "start")
                         '(:integer :integer :integer :integer :integer
                           :string :date)
                         "run" nil
                         profile
                         :sort :run-id)
      for run = (first runs)
      for date = (parse-date (get-field :start run))
      for host = (get-field :host run)
      for types = (get-field :avms run)
      for lexicon = (get-field :lexicon run)
      for lrules = (get-field :lrules run)
      for rules = (+ (if (minus-one-p lrules) 0 lrules) (get-field :rules run))
      collect (pairlis '(:profile :coverage :overgeneration :words :analyses
                         ;;
                         ;; _fix_me_
                         ;; sort out uniform naming scheme one day ...
                         ;;                            (31-aug-02; oe@taipei)
                         :readings
                         :pedges :aedges :rpedges :first :total
                         :host :date :types :lexicon :rules)
                       (list profile coverage overgeneration words analyses
                             analyses
                             pedges aedges rpedges first total
                             host date types lexicon rules))))

(defun graph-evolution (summaries &key (attributes '(:coverage))
                                       file (format :tcl) 
                                       title xtitle 
                                       symbols labels colours logscale)

  (let* ((stream (if file
                   (create-output-stream file nil)
                   *tsdb-io*))
         (summaries (sort (copy-list summaries) #'< 
                          :key #'(lambda (foo) (get-field :date foo))))
         (xmin (get-field :date (first summaries)))
         (xmax (get-field :date (first (last summaries))))
         (units (loop
                    for summary in summaries
                    collect (get-field :date summary)))
         (start (decode-time xmin :long :pretty))
         (end (decode-time xmax :long :pretty))
         (width (- xmax xmin))
         (boundaries (loop
                         for summary in summaries
                         for values = (loop
                                          for attribute in attributes
                                          collect 
                                            (get-field attribute summary))
                         when values minimize (apply #'min values) into minimum
                         maximize (apply #'max values) into maximum
                         finally (return (cons minimum maximum))))
         (ymin (first boundaries))
         (ymax (rest boundaries))
         (height (- ymax ymin))
         (intervals '(1 2 5))
         (goal 10)
         (ydivision
          ;;
          ;; this somewhat awkwardly-looking loop() computes the number of
          ;; ticks (divisions) on the y axis; it aims for an interval size
          ;; (distance between two ticks) that is a member of .intervals. or
          ;; a 10 multiply of one of the members; the loop() chooses the y
          ;; axis layout that comes closest to .goal. ticks.
          ;;
          (loop
              with division = 1
              for i from 1 by 1
              for n = 1 then (if (zerop (mod i (length intervals))) (* n 10) n)
              for interval = (* (nth (mod i (length intervals)) intervals) n)
              for ticks = (round height division)
              for comparison = (round height interval)
              do
                (if (< (abs (- goal comparison)) (abs (- goal ticks)))
                  (setf division interval)
                  (return division))))
         (title (or title "Evolution"))
         (xtitle (or xtitle (format nil "time [~a -- ~a]" start end)))
         lsymbols llabels)
                     
    (case format
      (:tcl
       (format 
        stream
        "graph -font {Helvetica 10 bold} -plotbackground white \\~%  ~
         -width 15c -height 10c -rightmargin 10  \\~%  ~
         -title ~s~%"
        title)
       (format 
        stream "data x ~a~%" 
        (list2tcl units :format "~,4f"))
       (format stream "axis x -title {~a} -showticks no ~%" xtitle)
       (format
        stream
        "axis y -tickfont {Helvetica 9} -logscale ~:[no~;yes~]~%"
        logscale))
      (:latex
       ;;
       ;; enlarge plot area (vertically) slightly to guarantee room for the
       ;; copyright caption
       ;;
       (let* ((pad (* (- ymax ymin) 0.04))
              (ymin (- ymin pad))
              (height (- ymax ymin))
              (ytics 
               (loop 
                   for tic = (* (ceiling ymin ydivision) ydivision)
                   then (+ tic ydivision)
                   while (<= tic ymax)
                   collect tic))
              (caption 
               (format 
                nil "(generated by ~a at ~a)"
                *tsdb-name* (current-time :long :pretty))))

         (format
          stream
          "\\dimendef\\plotwidth=0~%\\dimendef\\plotheight=1~%~
           \\plotwidth=~amm~%\\plotheight=~amm~%~
           \\divide\\plotwidth by ~a~%\\divide\\plotheight by ~a~%~
           \\beginpicture~%  ~
           \\setplotsymbol({\\rule{.4pt}{.4pt}})~%  ~
           \\setlinear~%  ~
           \\setcoordinatesystem units <\\plotwidth,\\plotheight>~%  ~
           \\setplotarea x from ~,4f to ~,4f, y from ~,4f to ~,4f~%  ~
           \\axis bottom label {\\sf ~a} /~%  ~
           \\axis left ticks numbered~%        at ~{~a ~}/ /~%  ~
           \\plotheading {\\frame <1pt> {\\frame <5pt> {\\Large\\sf ~a}}}~%"
          *statistics-plot-width* *statistics-plot-height*
          (round width) (round height)
          0 width ymin ymax
          xtitle
          ytics
          title)
         (format
          stream
          "  \\put {\\tiny\\sf ~a} [r] at ~,4f ~,4f~%"
          caption width (+ ymin (/ pad 2))))))


    (loop
        for attribute in attributes
        for i from 0
        for label = (or (nth i labels) (find-attribute-label attribute))
        for symbol = (or (nth i symbols) 
                         (find-attribute-symbol attribute i :format format))
        for colour = (or (nth i colours) (find-attribute-colour attribute i))
        for values = (loop
                         for summary in summaries
                         collect (cons (get-field :date summary)
                                       (get-field attribute summary)))
        do
          (case format
            (:tcl
             (format stream "ids {")
             (loop 
                 for foo in summaries 
                 do (format stream "{~a} " (get-field :profile foo)))
             (format stream "}~%")
             (format stream "data xx~a {" i)
             (loop for foo in values do (format stream "~,4f " (first foo)))
             (format stream "}~%")
             (format stream "data yy~a {" i) 
             (loop for foo in values do (format stream "~,4f " (rest foo)))
             (format stream "}~%")
             (format
              stream
              "element ee~a -xdata xx~a -ydata yy~a -label {~a} \\~%  ~
               -symbol ~a -pixel 2 -linewidth 0 \\~%  ~
               -color ~a -fill defcolor -outline defcolor~%"
              i i i label symbol colour))
            (:latex
             (format stream "  \\multiput{~a} at~%" symbol)
             (loop 
                 for (x . y) in values do
                   (format stream "    ~12,4f ~12,4f~%" (- x xmin) y))
             (format stream "  /~%")))
          (push symbol lsymbols)
          (push label llabels))

    (case format
      (:tcl
       (if attributes
         (format 
          stream 
          "legend -font {Helvetica 9} -position plotarea -anchor nw \\~%  ~
           -relief ridge~%")
         (format stream "legend -hide yes~%")))
      (:latex
       (let* ((x (* (- xmax xmin) 0.05))
              (y (- ymax (* (- ymax ymin) 0.05))))
         (format
          stream
          "  \\put {\\frame <5pt> {\\lines [l] {~%")
         (loop
             for symbol in (nreverse lsymbols)
             and label in (nreverse llabels)
             do
               (format stream "   {~a --- {\\sf ~a}}\\cr~%" symbol label))
         (format stream "  }}} [lt] at ~,4f ~,4f~%" x y))
       (format
        stream
        "\\endpicture~%")))

    (force-output stream)
    (when file (close stream))))

(defun evolution (profiles &key (attributes '(:coverage)) 
                                division 
                                (condition *statistics-select-condition*)
                                (format :tcl) file meter)
  (let ((summaries (summarize-evolution 
                    profiles :attributes attributes
                    :division division :condition condition
                    :format format :meter meter)))
    (when summaries
      (graph-evolution summaries :attributes attributes
                       :format format :file file))))

