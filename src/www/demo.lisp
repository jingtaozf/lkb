;;; -*- Mode: Common-Lisp; Package: www; -*-

(in-package :lkb)

(defpackage :www
  (:use :net.aserve :net.html.generator :lkb :make :common-lisp))

(in-package :www)

(defparameter *www-port* 8000)

(defparameter *www-log* (format nil "www.log.~a" (tsdb::current-user)))

(defparameter *www-interrupt* nil)

(defparameter *www-lkb-css*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "lkb.css"))

(defparameter *www-lkb-js*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "lkb.js"))

(defparameter *www-introduction*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "introduction.html"))

(defparameter *www-maximal-number-of-edges* 20000)

(defparameter *www-maximal-number-of-results* 10)

(defvar %www-clients% 0)

(defvar %www-object-counter% 0)

(defvar %www-attic% (make-array 512))

(defun initialize (&key (port *www-port*) pattern)
  (let ((interrupt (format 
                    nil 
                    "/tmp/.aserve.~a.~a" 
                    (tsdb::current-user) port)))
    (unless (mp:process-p *www-interrupt*)
      (flet ((check-interrupt () 
               (loop
                 (when (probe-file interrupt)
                   (delete-file interrupt)
                   (excl:exit))
                 (sleep 5))))
        (setf *www-interrupt*
          (mp:process-run-function 
           '(:name "aserve interrupt handler") #'check-interrupt))))
    (setf %www-clients% 0)
    (setf %www-object-counter% 0)
    (setf %www-attic% (make-array 512))
    ;;
    ;; a first attempt at `session management': if we fail to grab the port we
    ;; need, attempt to shut down the competing process (assuming it is a web
    ;; server that unterstands our interrupt protocol), wait long enough for
    ;; the interrupt handler to take effect, and try again.
    ;;
    (unless (ignore-errors
             (start :port port :external-format (excl:crlf-base-ef :utf-8)))
      (with-open-file (foo interrupt :direction :output :if-exists :supersede))
      (sleep 10)
      (start :port port :external-format (excl:crlf-base-ef :utf-8))))
    
  (publish-file :path "/lkb.css" :file *www-lkb-css*)
  (publish-file :path "/lkb.js" :file *www-lkb-js*)

  (publish :path "/compare"
   :content-type "text/html"
   :function #'(lambda (request entity) (www-compare request entity)))

  (cond
   ((null pattern)
    (publish :path "/erg"
     :content-type "text/html"
     :function #'(lambda (request entity) (www-erg request entity)))
    (publish :path "/browse"
      :content-type "text/html"
      :function #'(lambda (request entity) (www-browse request entity))))
   (t
    (publish :path "/podium"
      :content-type "text/html"
      :function #'(lambda (request entity) 
                    (www-podium request entity :pattern pattern)))
    (publish :path "/itsdb"
      :content-type "text/html"
      :function #'(lambda (request entity) (www-itsdb request entity))))))

(defun www-erg (request entity)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (input (or (lookup-form-value "input" query)
                    "just try pressing return in this window!"))
         (exhaustivep 
          (let ((foo (lookup-form-value "exhaustivep" query)))
            (string-equal foo "all")))
         (output (lookup-form-value "output" query))
         (treep (or (null body)
                    (if (stringp output)
                      (equal output "tree")
                      (member "tree" output :test #'equal))))
         (mrsp (or (null body) 
                   (if (stringp output)
                     (equal output "mrs")
                     (member "mrs" output :test #'equal))))
         (nresults (lookup-form-value "nresults" query))
         (*www-maximal-number-of-results*
          (cond
           ((equal nresults "10") 10)
           ((equal nresults "50") 50)
           ((equal nresults "100") 100)
           ((equal nresults "500") 500)
           ((equal nresults "all") nil)
           (t *www-maximal-number-of-results*))))
    (with-http-response (request entity)
      (with-http-body (request entity
                       :external-format (excl:crlf-base-ef :utf-8))
        (format
         *html-stream*
         "<!DOCTYPE HTML PUBLIC ~
          \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%")
        (html (:html
               (:head
                ((:meta
                  :http-equiv "Content-Type" 
                  :content "text/html; charset=utf-8"))
                (:title "LinGO English Resource Grammar On-Line")
                :newline
                ((:link
                  :type "text/css" :rel "stylesheet"
                  :href "/lkb.css"))
                :newline
                ((:script
                  :src "/lkb.js" :language "javascript" 
                  :type "text/javascript")))
               :newline
               (:body
                (:center
                 (unless (eq method :post)
                   (www-output *www-introduction* :stream *html-stream*))
                 ((:form 
                   :action "/erg" :method "post"
                   :accept-charset "utf-8")
                  :newline
                  ((:input :type "reset" :value "Reset"))
                  "&nbsp;"
                  ((:input
                    :type "text" :name "input"
                    :value (or input "") :size "100"))
                  "&nbsp;"
                  ((:input :type "submit" :value "Analyze"))
                  :br :newline
                  ((:table :border 0 :cellspacing 0)
                   (:tr
                    ((:td :class "buttons") "results:")
                    ((:td :class "buttons")
                     ((:input
                       :type :radio :name "exhaustivep"
                       :value "all" :if* exhaustivep :checked '||)))
                    ((:td :class "buttons") "all")
                    ((:td :class "buttons")
                     ((:input
                       :type :radio :name "exhaustivep"
                       :value "first"
                       :if* (not exhaustivep) :checked '||)))
                    ((:td :class "buttons") "first" )
                    ((:td :class "buttons")
                     "&nbsp;&nbsp;|&nbsp;&nbsp;output:")
                    ((:td :class "buttons")
                     ((:input
                       :type "checkbox" :name "output" :value "tree"
                       :if* treep :checked '||)))
                    ((:td :class "buttons") "tree")
                    ((:td :class "buttons")
                     ((:input
                       :type "checkbox" :name "output" :value "mrs"
                       :if* mrsp :checked '||)))
                    ((:td :class "buttons") "mrs")
                    ((:td :class "buttons")
                     "&nbsp;&nbsp;|&nbsp;&nbsp;show&nbsp;")
                    ((:td :class "buttons")
                     ((:select :size 1 :name "nresults")
                      ((:option :value "10" :selected '||) "10")
                      ((:option :value "50") "50")
                      ((:option :value "100") "100")
                      ((:option :value "500") "500")
                      ((:option :value "all") "all")))
                    ((:td :class "buttons") "&nbsp;results")))))
                (:center
                 (if (and (eq method :post) input)
                   (www-parse
                    input
                    :exhaustivep exhaustivep :treep treep :mrsp mrsp
                    :request request :stream *html-stream*)
                   (www-version *html-stream*))))))))))

(defun www-parse (input &key exhaustivep treep mrsp request stream)
  (let* ((result 
          (www-analyze 
           input exhaustivep 
           *www-maximal-number-of-edges* *www-maximal-number-of-results*))
         (readings (tsdb::get-field :readings result))
         (nresults (or *www-maximal-number-of-results* readings))
         (time (tsdb::get-field :tcpu result))
         (time (and (numberp time) (/ time 1000)))
         (pedges (tsdb::get-field :pedges result))
         (results (tsdb::get-field :results result))
         (error (tsdb::get-field :error result))
         (error (unless (and (numberp readings) (> readings 0))
                  (or
                   (loop
                       with result = nil
                       with end = 0 with start = end 
                       with starts = nil with ends = nil
                       while end do
                         (setf start end)
                         (multiple-value-setq (start end starts ends)
                           (ppcre::scan
                            "Word `([^']*)' is not in lexicon."
                            error :start start))
                         (when (and starts ends)
                           (pushnew 
                            (subseq error (aref starts 0) (aref ends 0)) 
                            result
                            :test #'equal))
                       finally (return (nreverse result)))
                   (when (search "no lexicon entries for" error)
                     (loop
                         with result = nil
                         with end = 0 with start = end 
                         with starts = nil with ends = nil
                         while end do
                           (setf start end)
                           (multiple-value-setq (start end starts ends)
                             (ppcre::scan
                              "\"([^\"]*)\""
                              error :start start))
                           (when (and starts ends)
                             (pushnew 
                              (subseq error (aref starts 0) (aref ends 0)) 
                              result
                              :test #'equal))
                         finally (return (nreverse result))))
                   (multiple-value-bind (foo bar)
                       (ppcre::scan-to-strings 
                        "edge limit \\(([0-9]+)\\)" error)
                     (declare (ignore foo))
                     (when bar 
                       (ignore-errors 
                        (read-from-string (aref bar 0) nil nil))))
                   (multiple-value-bind (foo bar)
                       (ppcre::scan-to-strings 
                        "edge limit exhausted \\(([0-9]+)" error)
                     (declare (ignore foo))
                     (when bar 
                       (ignore-errors 
                        (read-from-string (aref bar 0) nil nil))))
                   error))))
    (when request (www-log request input readings time pedges error))
    (cond
     ((null error)
      (format
       stream 
       "<div id=summary>~
        [~d of ~d ~:[analyses~;analysis~]~
        ~@[; processing time: ~,2f seconds~]~
        ~@[; ~a edges~]]</div>~%~
        <br>~%"
       (if (numberp *www-maximal-number-of-results*)
         (min readings *www-maximal-number-of-results*)
         readings)
       readings (= readings 1)
       time pedges pedges)
      (loop
          with tsdb::*reconstruct-cache* = (make-hash-table :test #'eql)
          initially
            (format 
             stream 
             "<form action=\"/browse\" method=post
                    accept-charset=\"utf-8\" target=\"_blank\">~%~
              <input type=hidden name=results value=~a>~%  ~
              <div id=action>~%  ~
              <input type=submit name=action value=compare>~%  ~
              <select name=set size=1>~%    ~
              <option value=all selected>all analyses</option>~%    ~
              <option value=active>selection</option>~%    ~
              </select>~%  ~
              &nbsp;&nbsp;|&nbsp;&nbsp;~%  ~
              <input type=submit name=action value=avm disabled>&nbsp;~%  ~
              <input type=submit name=action value=scope disabled>&nbsp;~%  ~
              <input type=submit name=action value=dependencies ~
                     disabled>&nbsp;~%  ~
              <input type=submit name=action value=generate disabled>~%  ~
              </div>~%<table class=results>~%"
             (www-store-object nil results))
          finally (format stream "</table></form>~%")
          for i from 0
          for result in results
          for derivation = (tsdb::get-field :derivation result)
          for mrs = (mrs::read-mrs-from-string (tsdb::get-field :mrs result))
          for edge = (when (or treep (and mrsp (null mrs)))
                       (or (tsdb::get-field :edge result)
                           (and derivation (tsdb::reconstruct derivation))))
          while (< i nresults) 
          do
            (when edge (nconc result (acons :edge edge nil)))
            (format 
             stream 
             "<tr>~%<td class=resultsNavigation>~%  ~
              <table class=resultsNavigation>~%    ~
              <tr><td class=center>~%~
              <div class=resultsNavigation># ~a</div></td></tr>~%    ~
              <tr><td class=center>~
              <input type=checkbox name=selection value=\"~a\">~
              </td></tr>~%  ~
              </table></td>~%"
             (+ i 1) i)
          when (and treep edge) do
            (format stream "<td class=resultsTree>~%")
            (lkb::html-tree edge :stream stream :indentation 4)
            (format stream "</td>~%")
          when (and mrsp (or mrs edge)) do
            (format stream "<td class=resultsMrs>~%")
            (mrs::output-mrs1 
             (or mrs (mrs::extract-mrs edge))'mrs::html stream)
            (format stream "</td>~%")
          do (format stream "</tr>")))
     ((or (null error) (equal error ""))
      (format
       stream 
       "<div id=error>~
        <p>No analysis was found for your input.&nbsp;&nbsp;~
        Is it grammatical?</p>~%~
        </div>~%"))
     ((integerp error)
      (format
       stream 
       "<div id=error>~
        <p>The parser exhausted its search space limit ~
        (of ~d passive edge~p);<br>~
        try non-exaustive parsing or a shorter (or less ambiguous) ~
        sentence.</p>~%</div>~%"
       error error))
     ((consp error)
      (format
       stream 
       "<div id=error>~
        The following input tokens were not found in the lexicon: ~
        ~{&lsquo;~(~a~)&rsquo;~^ ~}.~%</div>~%"
       error))
     (t
      (format
       stream 
       "<div id=error>~
        The server encountered an (unexpected) error: ~
        &lsquo;~a&rsquo;.~%</div>~%"
       (string-right-trim '(#\. #\? #\!) error))))
    (www-version stream)))

(defun www-analyze (input exhaustivep nedges nresults)
  (let* ((item (pairlis '(:i-id :parse-id :i-input :edges)
                        (list 0 0 input nedges)))
         (client (tsdb::allocate-client item :task :parse :wait 5))
         (cpu (and client (pvm::client-cpu client)))
         (tid (and client (pvm::client-tid client)))
         (protocol (and client (pvm::client-protocol client)))
         (p-input (cond
                   ((and (pvm::cpu-p cpu) (pvm::cpu-preprocessor cpu))
                    (tsdb::call-hook (pvm::cpu-preprocessor cpu) input))
                   (tsdb::*tsdb-preprocessing-hook*
                    (tsdb::call-hook tsdb::*tsdb-preprocessing-hook* input))))
         (item (nconc item (acons :p-input p-input nil)))
         (nanalyses (if exhaustivep 0 1))
         (nresults (or nresults 0))
         (status (if tid 
                   (case protocol
                     (:raw
                      (tsdb::process_item tid item nanalyses nresults nil))
                     (:lisp
                      (tsdb::revaluate 
                       tid 
                       `(tsdb::process-item
                         (quote ,item)
                         :exhaustive ,exhaustivep
                         :nanalyses ,nanalyses
                         :nresults ,nresults
                         :verbose nil :interactive nil :burst t)
                       nil
                       :key :process-item
                       :verbose nil)))
                   :null)))
    (case status
      (:ok 
       (let ((status (tsdb::process-queue nil :client client)))
         (if (rest (assoc :pending status))
           (pairlis '(:readings :error)
                    (list -1 (format nil "PVM client exit (tid # ~a)" tid)))
           (rest (assoc :result status)))))
      (:error 
       (setf (pvm::client-status client) :error)
       (pairlis '(:readings :error)
                (list -1 (format nil "PVM internal error (tid # ~a)" tid))))
      (:null
       (pairlis '(:readings :error)
                (list -1 
                      (format 
                       nil 
                       "maximum number of active sessions exhausted")))))))

(defun www-browse (request entity &key results)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (action (lookup-form-value "action" query))
         (results (or results
                      (if query 
                        (lookup-form-value "results" query)
                        (request-query-value "results" request :post nil))))
         (results (typecase results
                    (string (ignore-errors (parse-integer results)))
                    (number results)))
         (set (lookup-form-value "set" query))
         (selection (lookup-form-value "selection" query)))
    (cond
     ((equal action "compare")
      (when (and selection (equal set "active"))
        (loop
            with all = (www-retrieve-object nil results)
            with active = nil
            for foo in (if (listp selection) selection (list selection))
            for i = (ignore-errors (parse-integer foo))
            for edge = (and i (nth i all))
            when results do (push edge active)
            finally (setf results (www-store-object nil active))))
      (www-compare request entity :results results)))))

(defun www-compare (request entity &key data results)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (index (if query 
                  (lookup-form-value "frame" query)
                  (request-query-value "frame" request :post nil)))
         (frame (when (stringp index) (ignore-errors (parse-integer index))))
         (frame (when (integerp frame) (www-retrieve-object nil frame)))
         (results (or results
                      (if query 
                        (lookup-form-value "results" query)
                        (request-query-value "results" request :post nil))))
         (results (typecase results
                    (string (ignore-errors (parse-integer results)))
                    (number results)))
         (data (or data
                   (if query 
                     (lookup-form-value "data" query)
                     (request-query-value "data" request :post nil))))
         (action (lookup-form-value "action" query))
         (mode (lookup-form-value "mode" query))
         (mode (and mode (intern (string-upcase mode) :keyword)))
         (display (lookup-form-value "display" query))
         (display (and display (intern (string-upcase display) :keyword)))
         classicp concisep orderedp fullp)

    ;;
    ;; there are quite a few different ways for this function to be called ...
    ;;
    (cond
     ;;
     ;; first-time entry for browsing a (Redwoods-type) profile: construct a
     ;; comparison frame, store it in the attic, and initialize everything.
     ;;
     ((and (null frame) data)
      (setf frame (tsdb::browse-trees data :runp nil))
      (setf index (www-store-object nil frame))
      (tsdb::browse-tree 
       data (first (lkb::compare-frame-ids frame)) frame :runp nil))
     ;;
     ;; interactive parse comparison from set of results: again, construct a 
     ;; new comparison frame, store it in the attic, and initialize everything.
     ;; go into `modern' discriminant mode, mostly for advertising purposes ...
     ;;
     ((and (null frame) (integerp results))
      (let* ((results (www-retrieve-object nil results))
             (tsdb::*reconstruct-cache* (make-hash-table :test #'eql))
             (edges (loop
                        for result in results
                        for derivation = (tsdb::get-field :derivation result)
                        for edge = (or (tsdb::get-field :edge result)
                                       (let ((edge 
                                              (tsdb::reconstruct derivation)))
                                         (nconc result (acons :edge edge nil))
                                         edge))
                        collect edge))
             (lkb::*tree-discriminants-mode* :modern)
             (lkb::*tree-display-threshold* 10))
        (when edges
          (setf frame (lkb::compare edges :runp nil))
          (setf index (www-store-object nil frame)))))
     ;;
     ;; call-back from comparison form: perform whatever action was requested
     ;; and update the comparison frame and our local variables accordingly.
     ;; 
     (frame
      (cond
       ;;
       ;; while browsing a profile, move to previous or following item: from 
       ;; the list of identifiers in the frame, find the appropriate position
       ;; and re-initialize the compare frame
       ;;
       ((member action '("previous" "next") :test #'string-equal)
        (let ((nextp (string-equal action "next"))
              (id (lkb::compare-frame-item frame)))
          (loop
              for ids on (lkb::compare-frame-ids frame)
              when (or (and nextp (eql id (first ids)) (second ids))
                       (and (null nextp) (eql id (second ids))))
              return (tsdb::browse-tree 
                      data (if nextp (second ids) (first ids)) frame 
                      :runp nil))))
       ((and mode (not (eq mode (lkb::compare-frame-mode frame))))
        (setf (lkb::compare-frame-mode frame) mode)
        (lkb::set-up-compare-frame frame (lkb::compare-frame-edges frame)))

      ((and display (not (eq display (lkb::compare-frame-display frame))))
       (setf (lkb::compare-frame-display frame) display)
       (lkb::update-trees frame))
      

      ((string-equal action "clear")
       (lkb::reset-discriminants frame))
      (t
       (loop
           with discriminants = (lkb::compare-frame-discriminants frame)
           with decisions = nil
           for i from 0 to (length (lkb::compare-frame-discriminants frame))
           for key = (format nil "~a" i)
           for value = (lookup-form-value key query)
           when (and value (not (equal value "?"))) do
             (let ((value (when (equal value "+") t)))
               (push (cons i value) decisions))
           finally
             (loop
                 for (i . value) in decisions
                 for discriminant = (nth i discriminants)
                 do
                   (setf (lkb::discriminant-toggle discriminant) value)
                   (setf (lkb::discriminant-state discriminant) value))
             (lkb::recompute-in-and-out frame)
             (lkb::update-trees frame t))))))

    (setf classicp (eq (lkb::compare-frame-mode frame) :classic))
    (setf concisep (eq (lkb::compare-frame-display frame) :concise))
    (setf orderedp (eq (lkb::compare-frame-display frame) :ordered))
    (setf fullp (eq (lkb::compare-frame-display frame) :full))

    #+:debug
    (setf lkb::%frame% frame)
    
    (with-http-response (request entity)
      (with-http-body (request entity
                       :external-format (excl:crlf-base-ef :utf-8))
        (format
         *html-stream*
         "<!DOCTYPE HTML PUBLIC ~
          \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%")
        (html (:html
               (:head
                ((:meta
                  :http-equiv "Content-Type" 
                  :content "text/html; charset=utf-8"))
                (:title "Redwoods Tree Comparison")
                :newline
                ((:link
                  :type "text/css" :rel "stylesheet"
                  :href "/lkb.css"))
                :newline
                ((:script
                  :src "/lkb.js" :language "javascript" 
                  :type "text/javascript")))
               :newline
               (:body
                (:center
                 ((:form 
                   :action "/compare" :method "post"
                   :accept-charset "utf-8")
                  ((:table :class "compareNavigation")
                   (:span
                    (:td
                     ((:input 
                       :type "button" :name "close" :value "close"
                       :onClick "window.close()")))
                    (:td "&nbsp;")
                    (:td
                     ((:input 
                       :type "button" :name "save" :value "save"
                       :disabled '||)))
                    (when data
                      (html
                       (:td "&nbsp;")
                       (:td
                        ((:input 
                          :type "submit" :name "action" :value "previous")))
                       (:td "&nbsp;")
                       (:td
                        ((:input 
                          :type "submit" :name "action" :value "next")))))
                    (:td "&nbsp;")
                    (:td
                     ((:input :type "submit" :name "action" :value "clear")))
                    (:td "&nbsp;&nbsp;|&nbsp;&nbsp;mode:")
                    (:td
                     ((:select 
                       :size 1 :name "mode"
                       :onChange "this.form.submit()")
                      ((:option 
                        :value "classic" 
                        :if* classicp :selected :if* classicp '||)
                       "classic")
                      ((:option 
                        :value "modern" 
                        :if* (not classicp) :selected :if* (not classicp) '||)
                       "modern")))
                    (:td "&nbsp;&nbsp;|&nbsp;&nbsp;display:")
                    (:td
                     ((:select 
                       :size 1 :name "display"
                       :onChange "this.form.submit()")
                      ((:option 
                        :value "concise" 
                        :if* concisep :selected :if* concisep '||)
                       "concise")
                      ((:option 
                        :value "ordered" 
                        :if* orderedp :selected :if* orderedp '||)
                       "ordered")
                      ((:option 
                        :value "full" 
                        :if* fullp :selected :if* fullp '||)
                       "full")))))
                  :newline
                  (when data
                    (html 
                     ((:input :type "hidden" :name "data" :value data))))
                  ((:input :type "hidden" :name "frame" :value index))
                  :newline
                  (when frame
                    (lkb::html-compare frame :stream *html-stream*))
                  (www-version *html-stream*))))))))))

(defun www-podium (request entity &key pattern)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body))))
    (declare (ignore query))

    (with-http-response (request entity)
      (with-http-body (request entity
                               :external-format (excl:crlf-base-ef :utf-8))
        (format
         *html-stream*
         "<!DOCTYPE HTML PUBLIC ~
          \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%")
        (html (:html
               (:head
                ((:meta
                  :http-equiv "Content-Type" 
                  :content "text/html; charset=utf-8"))
                (:title "Redwoods Treebanks")
                :newline
                ((:link
                  :type "text/css" :rel "stylesheet"
                  :href "/lkb.css"))
                :newline
                ((:script
                  :src "/lkb.js" :language "javascript" 
                  :type "text/javascript")))
               :newline
               (:body
                (:center
                 ((:form
                   :action "/itsdb" :method "post" :target "_blank"
                   :accept-charset "utf-8")
                  :newline
                  ((:table :border 0 :cellspacing 0)
                   (:tr
                    ((:td :class "buttons") "condition&nbsp;")
                    ((:td :class "buttons")
                     ((:input
                       :type "text" :name "condition"
                       :value "" :size "40")))
                    ((:td :class "buttons") "&nbsp;")
                    ((:td :class "buttons")
                     ((:input 
                       :type "submit" :name "action"  :value "summarize")))
                    ((:td :class "buttons") "&nbsp;")
                    ((:td :class "buttons")
                     ((:input 
                       :type "submit" :name "action" :value "browse")))
                    ((:td :class "buttons") "&nbsp;")
                    ((:td :class "buttons")
                     ((:input 
                       :type "submit" :name "action" :value "Errors" 
                       :disabled '||)))))
                  :br :newline
                  ((:div :class "profiles")
                   (tsdb::tsdb-do-list 
                    tsdb::*tsdb-home* :pattern pattern
                    :stream *html-stream* :format :html)))
                 (www-version *html-stream*)))))))))

(defun www-itsdb (request entity)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (action (lookup-form-value "action" query))
         (data (lookup-form-value "data" query))
         (condition (lookup-form-value "condition" query)))

    (cond
     ((equal action "browse")
      (www-compare request entity :data data))
     ((equal action "summarize")
      (with-http-response (request entity)
        (with-http-body (request entity
                                 :external-format (excl:crlf-base-ef :utf-8))
        (format
         *html-stream*
         "<!DOCTYPE HTML PUBLIC ~
          \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%")
        (html 
         (:html
          (:head
           ((:meta
             :http-equiv "Content-Type" 
             :content "text/html; charset=utf-8"))
           (:title "Redwoods Annotation Summary")
           :newline
           ((:link
             :type "text/css" :rel "stylesheet"
             :href "/lkb.css"))
           :newline
           ((:script
             :src "/lkb.js" :language "javascript" 
             :type "text/javascript")))
          :newline
          (:body
           (:center
            (tsdb::analyze-trees 
             data :file *html-stream* :condition condition :format :html)
            (www-version *html-stream*)))))))))))

(defun www-version (stream)
  (format
   stream 
   "<div id=version>[ERG: ~a &mdash; PET (stable): Aug 12 2003 (17:29:09) ~
    &mdash; LKB: ~a &mdash; ~a: ~a]</div>~%"
   (tsdb::current-grammar) 
   (subseq lkb::*cvs-version* 6 (- (length lkb::*cvs-version*) 2))
   tsdb::*tsdb-name* tsdb::*tsdb-version*))

(let ((lock (mp:make-process-lock)))
  (defun www-log (request input readings time edges error)
    (mp:with-process-lock (lock)
      (with-open-file (stream *www-log* :direction :output
                       :if-does-not-exist :create :if-exists :append)
        (let* ((socket (request-socket request))
               (address (socket:remote-host socket))
               (host (socket:ipaddr-to-hostname address)))
          (format
           stream
           "[~a] www-log(): [~a] `~a' --- ~a~@[ (~,2f)~]~@[ <~a>~]~
            ~@[ error: `~(~a~)'~].~%"
           (tsdb::current-time :long :pretty) 
           host input readings time edges error))))))

(let ((lock (mp:make-process-lock)))
  (defun www-store-object (id object &key globalp)
    (mp:with-process-lock (lock)
      (let ((n %www-object-counter%))
        (setf (aref %www-attic% n) (cons (if globalp -1 id) object))
        (incf %www-object-counter%)
        (when (>= %www-object-counter% (array-total-size %www-attic%))
          (setf %www-attic% 
            (adjust-array %www-attic% (* %www-object-counter% 2))))
        n)))

  (defun www-retrieve-object (id n)
    (when (and (numberp n) (< n (array-total-size %www-attic%)))
      (mp:with-process-lock (lock)
        (let ((bucket (aref %www-attic% n)))
          (when (or (equal (first bucket) -1) (equal (first bucket) id))
            (rest bucket)))))))

(defun lookup-form-value (name query)
  (loop
      with result = nil
      for (key . value) in query
      when (string-equal key name) do (push value result)
      finally (return (if (rest result) result (first result)))))

(defun www-output (file &key (stream t) values)
  (when (probe-file file)
    (loop
        with buffer = (make-array 4096
                                  :element-type 'character
                                  :adjustable t :fill-pointer 0)
        with in = (open file :direction :input)
        for c = (read-char in nil nil)
        while c do (vector-push-extend c buffer)
        finally
          (close in)
          (apply #'format stream buffer values))))
