;;; -*- Mode: Common-Lisp; Package: www; -*-

(in-package :lkb)

(defpackage :www
  (:use :net.aserve :net.html.generator :lkb :make :common-lisp))

(in-package :www)

(defparameter *www-port* 8000)

(defparameter *www-log* (format nil "/tmp/www.log.~a" (tsdb::current-user)))

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

(defparameter *www-maximal-number-of-edges* 5000)

(defparameter *www-maximal-number-of-results* 10)

(defvar %www-clients% 0)

(defvar %www-object-counter% 0)

(defvar %www-attic% (make-array 512))

(defun initialize (&key (port *www-port*))
  (setf %www-clients% 0)
  (setf %www-object-counter% 0)
  (setf %www-attic% (make-array 512))
  (start :port port :external-format (excl:crlf-base-ef :utf-8))
  (publish-file :path "/lkb.css" :file *www-lkb-css*)
  (publish-file :path "/lkb.js" :file *www-lkb-js*)
  (publish :path "/erg"
    :content-type "text/html"
    :function #'(lambda (request entity) (www-erg request entity)))
  (publish :path "/erg/browse"
    :content-type "text/html"
    :function #'(lambda (request entity) (www-browse request entity)))
  (publish :path "/erg/compare"
    :content-type "text/html"
    :function #'(lambda (request entity) (www-compare request entity))))

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
         (treep (if (stringp output)
                  (equal output "tree")
                  (member "tree" output :test #'equal)))
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
                    ((:td  :class "buttons") "tree")
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
                      ((:option :value "all") "all")))
                    ((:td :class "buttons") "&nbsp;results")))))
                (:center
                 (when (and (eq method :post) input)
                   (www-parse
                    input
                    :exhaustivep exhaustivep :treep treep :mrsp mrsp
                    :request request :stream *html-stream*))))))))))

(defparameter %www-parse-lock% (mp:make-process-lock))

(defparameter %www-visualize-lock% (mp:make-process-lock))

(defparameter %www-log-lock% (mp:make-process-lock))

(defun www-parse (input &key exhaustivep treep mrsp request stream)
  (let* ((result 
          (mp:with-process-lock (%www-parse-lock%)
            (tsdb::parse-item 
             input 
             :exhaustive exhaustivep 
             :edges *www-maximal-number-of-edges*)))
         (readings (tsdb::get-field :readings result))
         (time (tsdb::get-field :tcpu result))
         (time (and (numberp time) (/ time 1000)))
         (pedges (tsdb::get-field :pedges result))
         (results (tsdb::get-field :results result))
         (edges (loop 
                    for result in results
                    for derivation = (tsdb::get-field :derivation result)
                    for edge = (and derivation (tsdb::reconstruct derivation))
                    do (nconc result (acons :edge edge nil))
                    collect edge))
         (error (tsdb::get-field :error result))
         (error (unless (> readings 0)
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
                   (multiple-value-bind (foo bar)
                       (ppcre::scan-to-strings 
                        "edge limit \\(([0-9]+)\\)" error)
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
      (mp:with-process-lock (%www-visualize-lock%)
        (loop
            initially
              (format 
               stream 
               "<form action=\"/erg/browse\" method=post
                      accept-charset=\"utf-8\" target=\"_blank\">~%~
                <input type=hidden name=edges value=~a>~%  ~
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
               (www-store-object nil edges))
            finally (format stream "</table></form>~%")
            for i from 0 to (- (or *www-maximal-number-of-results* readings) 1)
            for result in results
            for edge = (tsdb::get-field :edge result)
            for mrs = (mrs::extract-mrs edge)
            do
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
               i i)
            when treep do
              (format stream "<td class=resultsTree>~%")
              (lkb::html-tree edge :stream stream :indentation 4)
              (format stream "</td>~%")
            when mrsp do
              (format stream "<td class=resultsMrs>~%")
              (mrs::output-mrs1 mrs 'mrs::html stream)
              (format stream "</td>~%")
            do (format stream "</tr>"))))
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
        The parser encountered an (unexpected) error: ~
        &lsquo;~a&rsquo;~%</div>~%"
       error)))
    (www-version stream)))

(defun www-browse (request entity &key edges)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (action (lookup-form-value "action" query))
         (edges (or edges
                    (if query 
                      (lookup-form-value "edges" query)
                      (request-query-value "edges" request :post nil))))
         (edges (typecase edges
                  (string (ignore-errors (parse-integer edges)))
                  (number edges)))
         (set (lookup-form-value "set" query))
         (selection (lookup-form-value "selection" query)))
    (cond
     ((equal action "compare")
      (when (and selection (equal set "active"))
        (loop
            with all = (www-retrieve-object nil edges)
            with active = nil
            for foo in (if (listp selection) selection (list selection))
            for i = (ignore-errors (parse-integer foo))
            for edge = (and i (nth i all))
            when edges do (push edge active)
            finally (setf edges (www-store-object nil active))))
      (www-compare request entity :edges edges)))))

(defun www-compare (request entity &key profile edges)
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
         (edges (or edges
                    (if query 
                      (lookup-form-value "edges" query)
                      (request-query-value "edges" request :post nil))))
         (edges (typecase edges
                  (string (ignore-errors (parse-integer edges)))
                  (number edges)))
         (profile (or profile
                      (if query 
                        (lookup-form-value "profile" query)
                        (request-query-value "profile" request :post nil))))
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
     ((and (null frame) profile)
      (setf frame (tsdb::browse-trees profile :runp nil))
      (setf index (www-store-object nil frame))
      (tsdb::browse-tree 
       profile (first (lkb::compare-frame-ids frame)) frame :runp nil))
     ;;
     ;; interactive parse comparison from set of edges: again, construct a new
     ;; comparison frame, store it in the attic, and initialize everything.
     ;;
     ((and (null frame) (integerp edges))
      (let ((edges (www-retrieve-object nil edges))
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
                      profile (if nextp (second ids) (first ids)) frame 
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
                   :action "/erg/compare" :method "post"
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
                    (when profile
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
                  (when profile
                    (html 
                     ((:input :type "hidden" :name "profile" :value profile))))
                  ((:input :type "hidden" :name "frame" :value index))
                  :newline
                  (when frame
                    (lkb::html-compare frame :stream *html-stream*))
                  (www-version *html-stream*))))))))))

(defun www-version (stream)
  (format
   stream 
   "<div id=version>[ERG: ~a &mdash; LKB: ~a &mdash; ~a: ~a]</div>~%"
   (tsdb::current-grammar) 
   (subseq lkb::*cvs-version* 6 (- (length lkb::*cvs-version*) 2))
   tsdb::*tsdb-name* tsdb::*tsdb-version*))

(defun www-log (request input readings time edges error)
  (mp:with-process-lock (%www-log-lock%)
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
         host input readings time edges error)))))

(defun www-store-object (id object &key globalp)
  (let ((n %www-object-counter%))
    (setf (aref %www-attic% n) (cons (if globalp -1 id) object))
    (incf %www-object-counter%)
    (when (>= %www-object-counter% (array-total-size %www-attic%))
      (setf %www-attic% (adjust-array %www-attic% (* %www-object-counter% 2))))
    n))

(defun www-retrieve-object (id n)
  (when (and (numberp n) (< n (array-total-size %www-attic%)))
    (let ((bucket (aref %www-attic% n)))
      (when (or (equal (first bucket) -1) (equal (first bucket) id))
        (rest bucket)))))

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
